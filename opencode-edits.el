;;; opencode-edits.el --- Processing the files the agent edits  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann
;; Copyright (C) 2026  Valentin Leon

;; Author: Scott Zimmermann <sczi@disroot.org>
;;         Valentin Leon <valentin@leon.click>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; When an edit, write or patch tool call of a live turn succeeds, each
;; file it changed goes through `opencode-file-edited-functions' with the
;; line ranges the edit left, read from the diff the tool reports; a
;; write reports none and counts as the whole file.  This happens before
;; the agent's next step, so formatting lands before its later commands
;; read the file.  By default, Lisp files with unbalanced parentheses are
;; fixed with parinfer, Node projects with a local Prettier configuration
;; have their files formatted by that Prettier, and other files are
;; reindented by Emacs where the edit touched them: the top-level forms in
;; Lisp, the regions given to `opencode-org-indent-function' in Org, the
;; edited lines elsewhere.  Indentation-sensitive modes are left alone.
;; Transcripts loaded from the server never run these hooks, so a reloaded
;; or forked session does not reformat files edited long ago.
;;
;; When the turn ends, `opencode-files-finished-editing-functions' run
;; with every file it edited, one after another; by default Flycheck
;; checks them.  Their reports reach the agent as one synthetic message,
;; which starts a turn to fix them.  An interrupted turn keeps its files
;; for the end of the next one.
;;
;; The server writes files behind Emacs, and the file notification may
;; not have reached auto-revert yet, so `find-file-noselect' would ask
;; whether to reread a stale buffer.  Processing never asks: it visits
;; files noninteractively, rereads only unmodified buffers, skips a file
;; with unsaved Emacs edits, and refuses to save over a file that changed
;; again.  Buffers opened only for processing are killed afterwards.
;; `opencode-session-roots-functions' reports the directories sessions
;; work in, for configuring such rereads elsewhere.

;;; Code:

(require 'opencode-blocks)
(require 'opencode-common)
(require 'opencode-diff-parser)
(require 'opencode-http)
(require 'opencode-session)

(defcustom opencode-file-edited-functions '(opencode-fix-parens opencode-format-edited-file)
  "Functions run after the agent edits a file during a live turn.
Each receives FILE, the absolute name of the edited file, and RANGES,
the (START-LINE . END-LINE) line ranges the edit left in it, or nil
when the whole file was written.  A deletion-only range has no END-LINE."
  :type 'hook
  :group 'opencode)

(defcustom opencode-files-finished-editing-functions '(opencode-flycheck-edited-files)
  "Functions run with the files a turn edited, once the turn ends.
Each receives the list of absolute file names, in the session buffer.
A function that finishes asynchronously returns `:opencode-async' and
calls `opencode-hook-finished' when done.  `opencode-report-diagnostic'
passes a report back; the reports of all functions reach the agent as
one synthetic message."
  :type 'hook
  :group 'opencode)

(defcustom opencode-org-indent-function nil
  "Function reindenting the regions an edit touched in an Org buffer, or nil.
Called with a list of (BEG . END) regions.  Org's own src-block
indentation round-trips blocks through an edit buffer and shifts string
literals, so Org files are left alone unless this names an indenter that
preserves them."
  :type '(choice (const :tag "Leave Org files alone" nil) function)
  :group 'opencode)

(defcustom opencode-prettier t
  "When non-nil, edits in Node projects with a local Prettier configuration use it.
The project's installed Prettier runs through npx with --no-install,
honoring .prettierignore; other files are indented by Emacs."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-indent-sensitive-modes
  '(python-base-mode haskell-mode haskell-ts-mode fsharp-mode fsharp-ts-mode
    yaml-mode yaml-ts-mode nim-mode nim-ts-mode makefile-mode)
  "Major modes whose files are never reindented after an edit."
  :type '(repeat symbol)
  :group 'opencode)

(defcustom opencode-session-roots-functions nil
  "Abnormal hook run with the directories of the live session buffers.
It runs whenever a session buffer opens or is killed."
  :type 'hook
  :group 'opencode)

;;;; Visiting edited files

(defvar-local opencode--edit-conflict-notified nil
  "Non-nil after reporting that this buffer's unsaved edits blocked processing.")

(defvar-local opencode--edit-owned-buffer nil
  "Non-nil when this buffer was opened only to process an agent edit.")

(defun opencode-visit-edited-file (file)
  "Return an unmodified buffer visiting FILE, or nil when it has unsaved edits.
A stale buffer is reread without asking."
  (when (and (stringp file) (file-regular-p file) (file-readable-p file))
    (setq file (expand-file-name file))
    (condition-case err
        (let* ((existing (find-buffer-visiting file))
               (buffer (or existing (find-file-noselect file t))))
          (with-current-buffer buffer
            (unless existing
              (setq opencode--edit-owned-buffer t))
            (if (buffer-modified-p)
                (progn
                  (unless opencode--edit-conflict-notified
                    (setq opencode--edit-conflict-notified t)
                    (message "OpenCode left %s alone: it has unsaved Emacs edits"
                             (abbreviate-file-name file)))
                  nil)
              (setq opencode--edit-conflict-notified nil)
              (unless (verify-visited-file-modtime buffer)
                (let ((inhibit-message t))
                  (revert-buffer t t t)))
              buffer)))
      (file-error
       (message "OpenCode left %s alone: %s" (abbreviate-file-name file) (error-message-string err))
       nil))))

(defun opencode-release-edited-file (buffer)
  "Kill BUFFER if it was opened only for processing and stays unmodified and hidden."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and opencode--edit-owned-buffer
                 (not (buffer-modified-p))
                 (not (get-buffer-window buffer t)))
        (setq opencode--edit-owned-buffer nil)
        (kill-buffer buffer)))))

(declare-function userlock--check-content-unchanged "userlock" (filename))

(defun opencode--refuse-interactive-save (file)
  "Signal that saving FILE would need to ask the user."
  (signal 'file-supersession (list "Refusing to ask while processing" file)))

(defun opencode-save-edited-file (file)
  "Save the current buffer, visiting FILE, without asking anything.
Return non-nil when saved.  A file that changed on disk again is not
overwritten: the buffer is reread instead."
  ;; userlock.el provides no feature.
  (unless (fboundp 'userlock--check-content-unchanged)
    (load "userlock" nil t))
  (condition-case err
      (progn
        (unless (or (verify-visited-file-modtime)
                    (ignore-errors (userlock--check-content-unchanged file)))
          (signal 'file-supersession (list "Refusing to overwrite changed file" file)))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (opencode--refuse-interactive-save file)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (opencode--refuse-interactive-save file)))
                  ((symbol-function 'userlock--ask-user-about-supersession-threat)
                   (lambda (filename)
                     (unless (ignore-errors (userlock--check-content-unchanged filename))
                       (signal 'file-supersession (list "Refusing to overwrite changed file" filename)))))
                  ((symbol-function 'ask-user-about-lock)
                   (lambda (filename _owner) (signal 'file-locked (list filename nil)))))
          (save-buffer))
        t)
    ((file-error user-error)
     (message "OpenCode did not save %s: %s" (abbreviate-file-name file) (error-message-string err))
     (let ((inhibit-message t))
       (ignore-errors (revert-buffer t t t)))
     nil)))

;;;; Parentheses

(declare-function parinfer-rust--set-default-state "parinfer-rust-mode")
(declare-function parinfer-rust--execute "parinfer-rust-mode")
(defvar parinfer-rust--mode)
(defvar flycheck-checkers)

(defun opencode-fix-parens (file &optional _ranges)
  "Balance the parentheses of edited Lisp FILE with parinfer, when installed."
  (when (and (string-match-p "\\.\\(?:el\\|lisp\\)\\'" file)
             (require 'parinfer-rust-mode nil t))
    (when-let* ((buffer (opencode-visit-edited-file file)))
      (unwind-protect
          (with-current-buffer buffer
            (condition-case nil
                (check-parens)
              (error
               (let ((flycheck-checkers (delq 'parinfer-rust
                                              (copy-sequence (bound-and-true-p flycheck-checkers)))))
                 (parinfer-rust--set-default-state)
                 (let ((parinfer-rust--mode "indent"))
                   (parinfer-rust--execute)))
               (opencode-save-edited-file file))))
        (opencode-release-edited-file buffer)))))

;;;; Indentation

(defun opencode--range-regions (ranges)
  "Return the (BEG . END) regions of the current buffer covering line RANGES.
A deletion-only range, with no end line, covers its start line."
  (save-excursion
    (save-restriction
      (widen)
      (mapcar (lambda (range)
                (let* ((start (max 1 (car range)))
                       (end (max start (or (cdr range) start))))
                  (goto-char (point-min))
                  (forward-line (1- start))
                  (cons (point)
                        (progn (forward-line (- end start)) (line-end-position)))))
              ranges))))

(defun opencode--lisp-form-region (region)
  "Grow REGION, a (BEG . END) pair, over the top-level forms it overlaps."
  (save-excursion
    (cons (let ((open (nth 9 (syntax-ppss (car region)))))
            (goto-char (if open (car open) (car region)))
            (line-beginning-position))
          (let ((open (nth 9 (syntax-ppss (cdr region)))))
            (goto-char (cdr region))
            (when open
              (goto-char (car open))
              (condition-case nil
                  (forward-sexp 1)
                (scan-error (goto-char (point-max)))))
            (line-end-position)))))

(defun opencode-indent-edited-file (file ranges)
  "Reindent what an edit touched in FILE.
RANGES are the edit's line ranges; nil, for a whole-file write, means the
whole file.  Org files go to `opencode-org-indent-function', Lisp files
reindent the top-level forms the ranges overlap, and others the lines."
  (when-let* ((buffer (opencode-visit-edited-file file)))
    (unwind-protect
        (with-current-buffer buffer
          (unless (derived-mode-p opencode-indent-sensitive-modes)
            (let ((inhibit-message t)
                  (regions (if ranges
                               (opencode--range-regions ranges)
                             (list (cons (point-min) (point-max))))))
              (if (derived-mode-p 'org-mode)
                  (when opencode-org-indent-function
                    (funcall opencode-org-indent-function regions))
                (when (derived-mode-p 'lisp-data-mode)
                  (setq regions (mapcar #'opencode--lisp-form-region regions)))
                (dolist (region (mapcar (lambda (region)
                                          (cons (copy-marker (car region))
                                                (copy-marker (cdr region) t)))
                                        regions))
                  (indent-region (car region) (cdr region))
                  (set-marker (car region) nil)
                  (set-marker (cdr region) nil))))
            (when (buffer-modified-p)
              (message "OpenCode indented %s" (abbreviate-file-name file))
              (opencode-save-edited-file file))))
      (opencode-release-edited-file buffer))))

;;;; Prettier

(defvar opencode--prettier-projects (make-hash-table :test #'equal)
  "Node package roots mapped to t when their local Prettier has a configuration.")

(defun opencode--prettier-project-root (file)
  "Return FILE's Node package root when its local Prettier has a configuration."
  (when-let* ((root (locate-dominating-file file "package.json")))
    (setq root (file-name-as-directory (file-truename root)))
    (pcase (gethash root opencode--prettier-projects 'unknown)
      ('unknown
       (let ((found (and (executable-find "npx")
                         (with-temp-buffer
                           (let ((default-directory root))
                             (and (zerop (process-file "npx" nil t nil "--no-install" "prettier"
                                                       "--find-config-path" file))
                                  (not (string-empty-p (string-trim (buffer-string))))))))))
         (puthash root found opencode--prettier-projects)
         (and found root)))
      ('nil nil)
      (_ root))))

(defun opencode--prettier-file-action (file root)
  "Return `format' when ROOT's Prettier formats FILE, `ignore' when it ignores it."
  (with-temp-buffer
    (let ((default-directory root))
      (when (zerop (process-file "npx" nil t nil "--no-install" "prettier" "--file-info" file))
        (let ((info (json-parse-string (buffer-string) :object-type 'plist
                                       :false-object nil :null-object nil)))
          (cond ((plist-get info :ignored) 'ignore)
                ((plist-get info :inferredParser) 'format)))))))

(defun opencode--prettier-format-file (file root)
  "Format FILE with ROOT's local Prettier."
  (when-let* ((buffer (opencode-visit-edited-file file)))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((input (save-restriction
                          (widen)
                          (buffer-substring-no-properties (point-min) (point-max))))
                 (formatted
                  (with-temp-buffer
                    (insert input)
                    (let ((default-directory root))
                      (unless (zerop (call-process-region (point-min) (point-max) "npx" t t nil
                                                          "--no-install" "prettier"
                                                          "--stdin-filepath" file))
                        (error "Prettier failed for %s: %s" file (string-trim (buffer-string)))))
                    (buffer-string))))
            (unless (string= input formatted)
              (let ((inhibit-read-only t))
                (save-excursion
                  (save-restriction
                    (widen)
                    (erase-buffer)
                    (insert formatted))))
              (opencode-save-edited-file file))))
      (opencode-release-edited-file buffer))))

(defun opencode-format-edited-file (file ranges)
  "Format FILE after an edit of line RANGES: with Prettier, else by indenting."
  (when (file-exists-p file)
    (let* ((root (and opencode-prettier (opencode--prettier-project-root file)))
           (action (and root (opencode--prettier-file-action file root))))
      (pcase action
        ('format (opencode--prettier-format-file file root))
        ('ignore nil)
        (_ (opencode-indent-edited-file file ranges))))))

;;;; Recording edits

(defvar-local opencode--files-edited-this-turn nil
  "Absolute names of the files the current turn edited, newest first.")

(defun opencode-tool-edited-files (tool input metadata)
  "Return (FILE . RANGES) for each file a finished TOOL call changed.
INPUT and METADATA are the call's; FILE is relative to the session."
  (pcase tool
    ((or "edit" "patch")
     (seq-keep (lambda (entry)
                 (let-alist entry
                   (unless (equal .status "deleted")
                     (cons .file (and (stringp .patch)
                                      (mapcan #'cdr (opencode-diff->source-line-ranges .patch)))))))
               (alist-get 'files metadata)))
    ("write"
     (when-let* ((path (alist-get 'path input)))
       (list (list path))))))

(defun opencode--edits-tool-finished (overlay)
  "Run the edit hooks for the files the finished tool call of OVERLAY changed."
  (let ((data (opencode-block-data overlay)))
    (when (and (equal (plist-get data :status) "completed")
               (eq opencode-session-status 'busy))
      (dolist (entry (opencode-tool-edited-files (plist-get data :tool) (plist-get data :input)
                                                 (plist-get data :metadata)))
        (when-let* ((file (opencode-resolve-file-reference (car entry))))
          (cl-pushnew file opencode--files-edited-this-turn :test #'equal)
          (dolist (function opencode-file-edited-functions)
            (condition-case err
                (funcall function file (cdr entry))
              (error (message "OpenCode: %s failed on %s: %s" function
                              (abbreviate-file-name file) (error-message-string err))))))))))

(add-hook 'opencode-blocks-finished-functions #'opencode--edits-tool-finished)

;;;; After the turn

(defvar opencode--finished-editing-current nil
  "Plist of the finished-editing run in progress, or nil.")

(defvar opencode--finished-editing-queue nil
  "Finished-editing runs waiting for the current one, oldest first.")

(defun opencode-report-diagnostic (report)
  "Pass REPORT, a string, back from the running finished-editing function."
  (unless opencode--finished-editing-current
    (error "`opencode-report-diagnostic' called outside a finished-editing function"))
  (when (and report (not (string-empty-p report)))
    (push report (plist-get opencode--finished-editing-current :reports))))

(defun opencode-hook-finished ()
  "Mark the running asynchronous finished-editing function as done."
  (let ((run opencode--finished-editing-current))
    (unless (and run (plist-get run :active))
      (error "`opencode-hook-finished' called without a running finished-editing function"))
    (plist-put run :active nil)
    (opencode--finished-editing-next)))

(defun opencode--finished-editing-start ()
  "Start the next queued finished-editing run unless one is in progress."
  (when (and (null opencode--finished-editing-current) opencode--finished-editing-queue)
    (setq opencode--finished-editing-current (pop opencode--finished-editing-queue))
    (opencode--finished-editing-next)))

(defun opencode--finished-editing-done (run)
  "Send the reports of finished RUN to its session, then start the next run."
  (let ((reports (reverse (plist-get run :reports)))
        (buffer (plist-get run :buffer)))
    (setq opencode--finished-editing-current nil)
    (when (and reports (buffer-live-p buffer))
      (with-current-buffer buffer
        (opencode-request
         'post (format "/api/session/%s/synthetic" opencode-session-id)
         :body `((text . ,(concat "Diagnostics were reported after editing files. "
                                  "Please fix any problems caused by your changes.\n\n"
                                  (string-join reports "\n\n")))
                 (description . "Diagnostics after editing")
                 (delivery . "queue")
                 (resume . t))))))
  (opencode--finished-editing-start))

(defun opencode--finished-editing-next ()
  "Run the next function of the current finished-editing run."
  (let* ((run opencode--finished-editing-current)
         (buffer (plist-get run :buffer)))
    (if (not (buffer-live-p buffer))
        (progn (setq opencode--finished-editing-current nil)
               (opencode--finished-editing-start))
      (with-current-buffer buffer
        (if-let* ((function (pop (plist-get run :functions))))
            (let ((result (condition-case err
                              (progn (plist-put run :active function)
                                     (funcall function (plist-get run :files)))
                            (error
                             (message "OpenCode: %s failed: %s" function (error-message-string err))
                             nil))))
              (unless (eq result :opencode-async)
                (when (and (eq opencode--finished-editing-current run)
                           (eq (plist-get run :active) function))
                  (plist-put run :active nil)
                  (opencode--finished-editing-next))))
          (opencode--finished-editing-done run))))))

(defun opencode--edits-turn-ended (outcome)
  "Run the finished-editing functions over the files a turn with OUTCOME edited.
An interrupted turn keeps them for the next turn."
  (when (and opencode--files-edited-this-turn (not (eq outcome 'interrupted)))
    (let ((files (reverse opencode--files-edited-this-turn)))
      (setq opencode--files-edited-this-turn nil)
      (when opencode-files-finished-editing-functions
        (setq opencode--finished-editing-queue
              (append opencode--finished-editing-queue
                      (list (list :buffer (current-buffer) :files files
                                  :functions (copy-sequence opencode-files-finished-editing-functions)
                                  :reports nil :active nil))))
        (opencode--finished-editing-start)))))

(add-hook 'opencode-turn-ended-functions #'opencode--edits-turn-ended)

;;;; Flycheck

(declare-function flycheck-buffer "flycheck")
(declare-function flycheck-mode "flycheck" (&optional arg))
(declare-function flycheck-running-p "flycheck")
(declare-function flycheck-stop "flycheck")
(declare-function flycheck-error-column "flycheck" (err))
(declare-function flycheck-error-filename "flycheck" (err))
(declare-function flycheck-error-format-message-and-id "flycheck" (err &optional include-snippet))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function flycheck-error-line "flycheck" (err))
(defvar flycheck-current-errors)
(defvar flycheck-mode)

(defun opencode--flycheck-report (file directory errors)
  "Return the errors and warnings of ERRORS in FILE, relative to DIRECTORY."
  (when-let* ((relevant (seq-filter (lambda (err) (memq (flycheck-error-level err) '(error warning)))
                                    errors)))
    (mapconcat (lambda (err)
                 (format "- %s:%d%s: %s: %s"
                         (file-relative-name (or (flycheck-error-filename err) file) directory)
                         (or (flycheck-error-line err) 1)
                         (if-let* ((column (flycheck-error-column err))) (format ":%d" column) "")
                         (flycheck-error-level err)
                         (flycheck-error-format-message-and-id err)))
               relevant "\n")))

(defun opencode--flycheck-file (file directory finish)
  "Check FILE with Flycheck, then call FINISH with its report or nil.
DIRECTORY shortens the file names in the report."
  (if-let* ((buffer (opencode-visit-edited-file file)))
      (with-current-buffer buffer
        (let ((was-enabled flycheck-mode)
              (done nil)
              after status)
          (cl-flet ((complete (report)
                      (unless done
                        (setq done t)
                        (remove-hook 'flycheck-after-syntax-check-hook after t)
                        (remove-hook 'flycheck-status-changed-functions status t)
                        (unless was-enabled (flycheck-mode -1))
                        ;; Killing the buffer now would break Flycheck's own cleanup.
                        (run-at-time 0 nil #'opencode-release-edited-file buffer)
                        (funcall finish report))))
            (setq after (lambda ()
                          (complete (opencode--flycheck-report file directory flycheck-current-errors)))
                  status (lambda (state)
                           (when (memq state '(no-checker errored interrupted suspicious))
                             (complete (unless (eq state 'no-checker)
                                         (format "- %s: warning: Flycheck ended with status `%s'"
                                                 (file-relative-name file directory) state))))))
            (unless flycheck-mode (flycheck-mode 1))
            (when (flycheck-running-p) (flycheck-stop))
            (add-hook 'flycheck-after-syntax-check-hook after nil t)
            (add-hook 'flycheck-status-changed-functions status nil t)
            (flycheck-buffer))))
    (funcall finish nil)))

(defun opencode-flycheck-edited-files (files)
  "Check FILES with Flycheck and report their errors and warnings."
  (when (and files (require 'flycheck nil t))
    (let ((directory default-directory)
          (remaining (length files)))
      (dolist (file files)
        (opencode--flycheck-file file directory
                                 (lambda (report)
                                   (when report (opencode-report-diagnostic report))
                                   (when (zerop (cl-decf remaining))
                                     (opencode-hook-finished)))))
      :opencode-async)))

;;;; Session roots

(defun opencode-session-roots ()
  "Return the directories of the live session buffers."
  (delete-dups
   (seq-keep (lambda (buffer)
               (with-current-buffer buffer
                 (and (derived-mode-p 'opencode-session-mode) default-directory)))
             (buffer-list))))

(defvar opencode--session-roots-timer nil
  "Timer reporting the session roots once buffer changes settle.")

(defun opencode--session-roots-changed ()
  "Report the session roots once the current command finishes."
  (when opencode-session-roots-functions
    (unless (timerp opencode--session-roots-timer)
      (setq opencode--session-roots-timer
            (run-at-time 0 nil (lambda ()
                                 (setq opencode--session-roots-timer nil)
                                 (run-hook-with-args 'opencode-session-roots-functions
                                                     (opencode-session-roots))))))))

(defun opencode--session-roots-setup ()
  "Report the session roots now and when this session buffer is killed."
  (add-hook 'kill-buffer-hook #'opencode--session-roots-changed nil t)
  (opencode--session-roots-changed))

(add-hook 'opencode-session-mode-hook #'opencode--session-roots-setup)

(provide 'opencode-edits)
;;; opencode-edits.el ends here
