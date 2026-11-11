;;; opencode-permission.el --- Permission requests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  nineluj
;; Copyright (C) 2026  Valentin Leon

;; Author: nineluj <code@nineluj.com>
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

;; A permission request names the tool call that asked, so it marks that
;; call's block in the attention face, with a readable action.  Expanding
;; it shows the operation, its context and the asking rules.
;; The transient holds just a one-line command or action and its controls.
;; A request whose block has not arrived yet prints a fallback line that
;; the block replaces.  The transient
;; accepts once (o), always (a, showing the patterns saved), rejects (n),
;; rejects with feedback and continues (f),
;; or hands the task off to another primary agent (h): the session
;; switches agent, a synthetic message tells the model, and the pending
;; call is accepted once, so the turn continues with its context intact.
;;
;; A shell command and its execution details are literal text, not
;; Markdown.

;;; Code:

(require 'opencode-blocks)
(require 'opencode-common)
(require 'opencode-events)
(require 'opencode-http)
(require 'opencode-requests)
(require 'opencode-session)
(require 'transient)

(defvar-local opencode-pending-permissions nil
  "Permission requests of this session awaiting an answer, oldest first.
Each is a plist (:id :action :resources :save :metadata :source :reasons
:title :prompt :marker).")

(defvar opencode--active-permission nil
  "Plist (:buffer :id) of the permission shown or suspended in the transient.")

;;;; Titles

(defun opencode--permission-path (path)
  "Return PATH for display: relative inside the session directory, else abbreviated.
A remote service's path is its host's, made relative as a string."
  (when (and path (not (string-empty-p path)) (not (opencode-service-files-p)))
    (setq path (let ((relative (opencode-relative-path-for-display path)))
                 (if (equal relative "") "." relative))))
  (when (and path (not (string-empty-p path)) (opencode-service-files-p))
    (let* ((expanded (expand-file-name path))
           (relative (file-relative-name expanded default-directory)))
      (if (string-prefix-p ".." relative)
          (abbreviate-file-name expanded)
        (if (string= relative "") "." relative)))))

(defun opencode-permission-command-summary (command)
  "Return COMMAND's first line, with an ellipsis when more lines follow."
  (let ((lines (split-string (string-trim command) "\n")))
    (if (cdr lines)
        (concat (string-trim-right (car lines)) " …")
      (car lines))))

(defun opencode--permission-tool-input (source)
  "Return the input of the tool call named by permission SOURCE, if rendered."
  (when-let* ((id (alist-get 'id source))
              (overlay (opencode-block id)))
    (plist-get (opencode-block-data overlay) :input)))

(defun opencode-permission-title (action resources metadata input)
  "Return the title of a permission request for ACTION on RESOURCES.
METADATA comes with the request; INPUT is the asking tool call's input."
  (let ((first (car resources)))
    (let-alist metadata
      (pcase action
        ("shell"
         (format "Run %s" (opencode-permission-command-summary
                           (or .command (alist-get 'command input) (string-join resources " ")))))
        ("edit" (format "Edit %s" (opencode--permission-path (or (alist-get 'path input) first ""))))
        ("read" (format "Read %s" (opencode--permission-path (or (alist-get 'path input) first ""))))
        ((or "glob" "grep") (format "%s %S" (capitalize action) (or (alist-get 'pattern input) first "")))
        ("webfetch" (format "Fetch %s" (or (alist-get 'url input) first "")))
        ("websearch" (format "Web search %S" (or (alist-get 'query input) first "")))
        ("subagent" (format "Run the %s subagent" (or (alist-get 'agent input) first "?")))
        ("external_directory"
         (format "Access the external directory %s"
                 (opencode--permission-path
                  (or .parentDir .path
                      (and first (if (string-match-p "\\*" first) (file-name-directory first) first))))))
        ("doom_loop" "Continue after repeated failures")
        (_ (or (opencode-call-tool-function opencode-tool-summary-functions action input)
               (format "Call %s%s" action
                       (if (and resources (not (equal resources '("*"))))
                           (format " on %s" (string-join resources ", "))
                         ""))))))))

(defun opencode--permission-prompt (permission input)
  "Return the one-line command or action of PERMISSION with tool INPUT."
  (opencode-permission-command-summary
   (or (alist-get 'command (plist-get permission :metadata))
       (alist-get 'command input)
       (and (equal (plist-get permission :action) "shell")
            (string-join (plist-get permission :resources) " "))
       (plist-get permission :title))))

;;;; Reasons

(defun opencode--reason-line (text needle)
  "Return the numbered line of TEXT holding NEEDLE, or nil."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (when (search-forward needle nil t)
        (format "line %d: %s" (line-number-at-pos)
                (truncate-string-to-width
                 (string-trim (buffer-substring (line-beginning-position) (line-end-position)))
                 120 nil nil "…"))))))

(defun opencode--reason-resource (resource rule &optional spelling)
  "Return the lines showing RESOURCE, which RULE asks for.
SPELLING is the other spelling of RESOURCE that RULE matched, such as
the command without its directory or the script a runner runs; it is
shown under RESOURCE.  When the longest literal part of RULE's wildcard
pattern lies past the first line of what RULE matched, the line holding
it is shown with its number, which locates the match inside a long
script."
  (let* ((matched (or spelling resource))
         (summary (opencode-permission-command-summary resource))
         (as (when spelling (opencode-permission-command-summary spelling)))
         (needle (car (seq-sort-by #'length #'>
                                   (split-string (or (alist-get 'resource rule) "") "[*?]" t))))
         (line (and needle (not (string-search needle (or as summary)))
                    (opencode--reason-line matched needle))))
    (concat "\n    " summary
            (when as (concat "\n      " (propertize (concat "as: " as) 'face 'shadow)))
            (when line (concat "\n      " (propertize line 'face 'shadow))))))

(defun opencode-permission-reasons-summary (reasons)
  "Return a one-line summary of the rules asking for REASONS, or nil."
  (when reasons
    (let* ((rules (seq-uniq (delq nil (mapcar (lambda (reason)
                                                (alist-get 'resource (alist-get 'rule reason)))
                                              reasons))))
           (default (seq-some (lambda (reason) (null (alist-get 'rule reason))) reasons))
           (named (when rules
                    (format "asked by %s %s" (if (cdr rules) "rules" "rule")
                            (mapconcat (lambda (rule) (format "\"%s\"" rule)) rules ", ")))))
      (cond ((and named default) (concat named "; no rule allows the rest"))
            (named)
            (t "asked: no rule allows it")))))

(defun opencode-permission-reasons-text (reasons &optional command)
  "Return the text explaining why a request with REASONS asks, or nil.
Resources are grouped by the rule asking for them; one contained in
another of its group, such as a command without its redirections, is
left out.  COMMAND, when it is the only resource, is already shown."
  (when reasons
    (let (groups)
      (dolist (reason reasons)
        (push reason (alist-get (alist-get 'rule reason) groups nil nil #'equal)))
      (concat
       "\n\nApproval required"
       (mapconcat
        (lambda (group)
          (let* ((rule (car group))
                 (members (reverse (cdr group)))
                 (resources (mapcar (lambda (reason) (alist-get 'resource reason)) members))
                 (shown (seq-remove (lambda (reason)
                                      (let ((resource (alist-get 'resource reason)))
                                        (seq-some (lambda (other)
                                                    (and (not (equal other resource))
                                                         (string-search resource other)))
                                                  resources)))
                                    members)))
            (concat "\n  "
                    (if rule
                        (format "%s rule \"%s\""
                                (alist-get 'action rule) (alist-get 'resource rule))
                      "no rule allows it")
                    (unless (and (= (length reasons) 1)
                                 (not (alist-get 'spelling (car reasons)))
                                 command
                                 (equal (string-trim command)
                                        (string-trim (alist-get 'resource (car reasons)))))
                      (concat ":"
                              (mapconcat (lambda (reason)
                                           (opencode--reason-resource (alist-get 'resource reason) rule
                                                                      (alist-get 'spelling reason)))
                                         shown ""))))))
        (reverse groups) "")))))

(defconst opencode--permission-host-actions
  '("shell" "read" "edit" "glob" "grep" "external_directory")
  "Permission actions acting on the files or processes of the service's host.
Their expansion names the directory and, for a remote service, the host;
other tools, such as an MCP server's, run elsewhere.")

(defun opencode--permission-display (permission tool input)
  "Return the expanded operation and context of PERMISSION for TOOL with INPUT."
  (let* ((metadata (plist-get permission :metadata))
         (command (or (alist-get 'command metadata) (alist-get 'command input)))
         (directory (or (alist-get 'cwd metadata) (alist-get 'workdir input)
                        (opencode-location) default-directory))
         (reasons (opencode-permission-reasons-text (plist-get permission :reasons) command)))
    (concat
     (if (and (equal tool "shell") command)
         (opencode-fontify-as (concat "$ " command) 'verbatim)
       (opencode-format-tool-call tool input))
     (opencode-fontify-as
      (concat (when (member (plist-get permission :action) opencode--permission-host-actions)
                (concat "\n\nDirectory  " (if (opencode-service-files-p)
                                              (abbreviate-file-name (expand-file-name directory)) directory)
                        (unless (opencode-service-files-p) (concat "\nHost       " (opencode-service-label)))))
              (when (eq (alist-get 'background input) t) "\nExecution  Background")
              (when-let* ((shell (alist-get 'shell metadata))) (concat "\nShell      " shell))
              (when-let* ((timeout (or (alist-get 'timeout metadata) (alist-get 'timeout input))))
                (format "\nTimeout    %s" (if (zerop timeout) "Unlimited" (format "%s ms" timeout))))
              (or reasons (concat "\n\nApproval required\n  " (plist-get permission :action)))
              (when-let* ((message (plist-get permission :message))) (concat "\n  " message)))
      'verbatim))))

;;;; Queue

(defun opencode--permission-find (id)
  "Return the queued permission ID of this buffer."
  (seq-find (lambda (permission) (equal (plist-get permission :id) id))
            opencode-pending-permissions))

(defun opencode--permission-block (permission)
  "Return the block of the tool call that asked PERMISSION, if rendered."
  (when-let* ((id (alist-get 'id (plist-get permission :source))))
    (opencode-block id)))

(defun opencode--permission-mark-block (permission)
  "Mark PERMISSION's tool block as waiting; return the block.
The expanded block holds the details; the transient stays compact."
  (when-let* ((overlay (opencode--permission-block permission)))
    (let* ((data (opencode-block-data overlay))
           (input (plist-get data :input)))
      (plist-put permission :title
                 (opencode-permission-title (plist-get permission :action) (plist-get permission :resources)
                                            (plist-get permission :metadata) input))
      (plist-put permission :prompt (opencode--permission-prompt permission input))
      (opencode-block-update overlay :waiting t :waiting-kind 'permission
                             :waiting-summary (plist-get permission :title) :waiting-description nil
                             :permission-display (opencode--permission-display permission (plist-get data :tool) input)))
    (overlay-put overlay 'help-echo
                 (format "Permission pending: %s.  %s responds."
                         (plist-get permission :title) (opencode-command-keys 'opencode-respond)))
    overlay))

(defun opencode--permission-clear-block (permission)
  "Clear the waiting mark of PERMISSION's tool block."
  (when-let* ((overlay (opencode--permission-block permission)))
    (when (eq (plist-get (opencode-block-data overlay) :waiting-kind) 'permission)
      (opencode-block-update overlay :waiting nil :waiting-kind nil :waiting-description nil
                             :waiting-summary nil :permission-display nil)
      (overlay-put overlay 'help-echo nil))))

(defun opencode--permission-fallback (permission)
  "Write PERMISSION's fallback line, for a request whose block is not shown."
  (when (get-buffer-process (current-buffer))
    (opencode-call-preserving-viewport
     (current-buffer)
     (lambda ()
       (opencode-before-prompt
         (opencode-open-block 'notice)
         (opencode-output (opencode-fontify-as
                           (propertize (concat "? " (plist-get permission :title))
                                       'font-lock-face 'opencode-attention)
                           'verbatim))
         (plist-put permission :marker (copy-marker (opencode-pmark)))
         (opencode-output "\n"))))))

(defun opencode--permission-remove-fallback (permission)
  "Delete PERMISSION's fallback line when it is still exactly as written."
  (let ((marker (plist-get permission :marker))
        (text (concat "? " (plist-get permission :title))))
    (when (and (markerp marker) (eq (marker-buffer marker) (current-buffer)))
      (let ((start (- marker (length text)))
            (end (min (point-max) (1+ marker))))
        (when (and (>= start (point-min))
                   (equal (buffer-substring-no-properties start marker) text))
          (let ((inhibit-read-only t))
            (delete-region start end))))
      (set-marker marker nil)
      (plist-put permission :marker nil))))

(defun opencode--permission-label (permission label face)
  "Append LABEL in FACE to PERMISSION's fallback line, if it has one."
  (when-let* ((marker (plist-get permission :marker))
              ((marker-buffer marker)))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char marker)
        (insert (propertize label 'face face 'font-lock-face face))))
    (set-marker marker nil)))

(defun opencode-permission-queue (request)
  "Queue permission REQUEST, an alist from the server, in this session buffer."
  (let-alist request
    (unless (opencode--permission-find .id)
      (let* ((input (opencode--permission-tool-input .source))
             (permission (list :id .id :action .action :resources .resources :save .save
                               :metadata .metadata :source .source :reasons .reasons :message .message
                               :title (opencode-permission-title .action .resources .metadata input)
                               :prompt nil)))
        (plist-put permission :prompt (opencode--permission-prompt permission input))
        (setq opencode-pending-permissions (append opencode-pending-permissions (list permission)))
        (unless (opencode--permission-mark-block permission)
          (opencode--permission-fallback permission))
        (force-mode-line-update)
        (opencode-request-state-changed)
        (if (opencode-buffer-active-p (current-buffer))
            (message "OpenCode permission pending; %s answers it"
                     (opencode-command-keys 'opencode-respond))
          (opencode-notify-session "OpenCode permission request" (plist-get permission :title)
                                   'permission .id)
          (opencode-alert-session))))))

(defun opencode--permission-attach (overlay)
  "Attach a queued permission to newly inserted tool block OVERLAY."
  (let ((id (plist-get (opencode-block-data overlay) :key)))
    (when-let* ((permission (seq-find (lambda (permission)
                                        (equal (alist-get 'id (plist-get permission :source)) id))
                                      opencode-pending-permissions)))
      (opencode--permission-remove-fallback permission)
      (plist-put permission :title
                 (opencode-permission-title (plist-get permission :action)
                                            (plist-get permission :resources)
                                            (plist-get permission :metadata)
                                            (plist-get (opencode-block-data overlay) :input)))
      (opencode--permission-mark-block permission))))

(defvar opencode--permission-transient-state)

(defun opencode-refresh-tool-blocks (&rest tools)
  "Redraw the blocks of calls to TOOLS in every session buffer.
For a package whose descriptions of these calls, through
`opencode-tool-summary-functions' and `opencode-tool-display-functions',
changed since they were drawn, such as once data it looked up arrived.
A pending permission of such a call gets its title, prompt and
expansion anew, and an open permission menu its heading."
  (let (menu)
    (dolist (service (opencode-services))
      (opencode-with-service service
        (dolist (buffer (hash-table-values (opencode-sv opencode-session-buffers)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (dolist (overlay (opencode-tool-blocks))
                (when (member (plist-get (opencode-block-data overlay) :tool) tools)
                  (if-let* ((permission (seq-find (lambda (permission)
                                                    (eq (opencode--permission-block permission) overlay))
                                                  opencode-pending-permissions)))
                      (progn (opencode--permission-mark-block permission)
                             (when (eq permission (plist-get opencode--permission-transient-state :permission))
                               (setq menu t)))
                    (opencode-refresh-block overlay)))))))))
    (when (and menu (bound-and-true-p transient--prefix)
               (eq (oref transient--prefix command) 'opencode-permission-transient)
               (fboundp 'transient--redisplay))
      (transient--redisplay))))

(defun opencode--permission-forget (permission &optional label face)
  "Drop PERMISSION from the queue, labelling its fallback LABEL in FACE."
  (opencode-dismiss-notifications 'permission (plist-get permission :id))
  (setq opencode-pending-permissions (delq permission opencode-pending-permissions))
  (opencode--permission-clear-block permission)
  (when label (opencode--permission-label permission label face))
  (when (and (equal (plist-get opencode--active-permission :id) (plist-get permission :id))
             (eq (plist-get opencode--active-permission :buffer) (current-buffer)))
    (opencode--permission-close-transient))
  (force-mode-line-update)
  (opencode-request-state-changed))

;;;; Replying

(defun opencode-permission-reply (permission decision &optional message)
  "Answer PERMISSION with DECISION, `once', `always' or `reject', and MESSAGE."
  (let ((buffer (current-buffer)))
    (opencode--permission-forget
     permission
     (pcase decision
       ('once " [accepted]") ('always " [accepted always]") (_ " [rejected]"))
     (if (eq decision 'reject) 'error 'success))
    ;; A plain rejection ends the agent's step: the turn is not handed back.
    (when (or message (not (eq decision 'reject)))
      (opencode-handed-back 'permission))
    (opencode-request
     'post (format "/api/session/%s/permission/%s/reply" opencode-session-id (plist-get permission :id))
     :body `((decision . ,(symbol-name decision)) ,@(when message `((message . ,message))))
     :else (lambda (error)
             (unless (eql (opencode-error-status error) 404)
               (with-current-buffer buffer
                 (setq opencode-pending-permissions
                       (cons permission opencode-pending-permissions))
                 (opencode--permission-mark-block permission)
                 (force-mode-line-update))
               (message "OpenCode permission reply failed (%s); %s retries"
                        (opencode-error-describe error)
                        (opencode-command-keys 'opencode-respond)))))))

;;;; Hand-off

(defun opencode--handoff-agents ()
  "Return the other primary agents, the build agent first."
  (let ((agents (seq-remove (lambda (agent) (equal (alist-get 'id agent) opencode-session-agent))
                            (opencode-primary-agents))))
    (append (seq-filter (lambda (agent) (equal (alist-get 'id agent) "build")) agents)
            (seq-remove (lambda (agent) (equal (alist-get 'id agent) "build")) agents))))

(defun opencode-permission-hand-off (permission agent)
  "Continue the turn with AGENT, then accept PERMISSION once."
  (let ((buffer (current-buffer))
        (session opencode-session-id))
    (opencode--permission-forget permission (format " [handed off to %s]" agent) 'success)
    (opencode-handed-back 'permission)
    (cl-flet ((fail (error)
                (with-current-buffer buffer
                  (setq opencode-pending-permissions (cons permission opencode-pending-permissions))
                  (opencode--permission-mark-block permission)
                  (message "Hand-off failed (%s); the permission is still pending"
                           (opencode-error-describe error)))))
      (opencode-switch-agent
       agent
       :else #'fail
       :then (lambda ()
               (opencode-request
                'post (format "/api/session/%s/synthetic" session)
                :body `((text . ,(format "The user approved handing off the current task to the %s agent.  Continue the task from the preceding research and complete the requested work." agent))
                        (description . ,(format "Handed off to %s" agent))
                        (delivery . "steer"))
                :else #'fail
                :then (lambda (_)
                        (opencode-request
                         'post (format "/api/session/%s/permission/%s/reply" session (plist-get permission :id))
                         :body '((decision . "once")))
                        (message "Handed off to %s; the current tool continues" agent))))))))

;;;; Transient

(defvar opencode--permission-transient-state nil
  "Plist (:buffer :permission) the permission transient acts on.")

(defun opencode--permission-transient-act (function &rest arguments)
  "Call FUNCTION with the transient's permission and ARGUMENTS in its buffer."
  (let* ((state opencode--permission-transient-state)
         (buffer (plist-get state :buffer)))
    (setq opencode--active-permission nil
          opencode--permission-transient-state nil)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (apply function (plist-get state :permission) arguments)))))

(transient-define-suffix opencode-permission-accept ()
  "Accept the request once."
  :transient nil
  (interactive)
  (opencode--permission-transient-act #'opencode-permission-reply 'once))

(transient-define-suffix opencode-permission-accept-always ()
  "Accept the request and save its patterns."
  :transient nil
  :description (lambda ()
                 (format "Accept always (%s)"
                         (string-join (or (plist-get (plist-get opencode--permission-transient-state :permission) :save)
                                          '("this request"))
                                      ", ")))
  (interactive)
  (opencode--permission-transient-act #'opencode-permission-reply 'always))

(transient-define-suffix opencode-permission-reject ()
  "Reject the request and stop the session's current execution."
  :transient nil
  (interactive)
  (opencode--permission-transient-act #'opencode-permission-reply 'reject))

(transient-define-suffix opencode-permission-reject-with-feedback (feedback)
  "Reject the request and let the assistant continue using FEEDBACK."
  :transient nil
  (interactive (list (read-string "Reject and continue with feedback: ")))
  (let ((message (string-trim feedback)))
    (when (string-empty-p message)
      (user-error "Feedback is empty; use Reject to stop instead"))
    (opencode--permission-transient-act #'opencode-permission-reply 'reject message)))

(transient-define-suffix opencode-permission-hand-off-suffix ()
  "Hand the task off to another primary agent."
  :transient nil
  (interactive)
  (let* ((buffer (plist-get opencode--permission-transient-state :buffer))
         (agents (with-current-buffer buffer
                   (opencode-catalog-wait 'agent)
                   (opencode--handoff-agents)))
         (names (mapcar (lambda (agent) (alist-get 'id agent)) agents)))
    (unless names (user-error "No other primary agent"))
    (let ((agent (completing-read (format-prompt "Hand off to" (car names)) names nil t nil nil (car names))))
      (opencode--permission-transient-act #'opencode-permission-hand-off agent))))

(defun opencode--permission-description ()
  "Return the one-line command or action heading the permission transient."
  (let ((permission (plist-get opencode--permission-transient-state :permission)))
    (or (plist-get permission :prompt) (plist-get permission :title) "Permission")))

(transient-define-prefix opencode-permission-transient ()
  "Answer an OpenCode permission request."
  [:description opencode--permission-description
                [("o" "Accept once" opencode-permission-accept)
                 ("n" "Reject and stop" opencode-permission-reject)
                 ("h" "Hand off to another agent" opencode-permission-hand-off-suffix)]
                [("a" opencode-permission-accept-always)
                 ("f" "Reject with feedback" opencode-permission-reject-with-feedback)]])

(opencode-install-hidden-navigation 'opencode-permission-transient opencode-transient-scroll-suffixes)

(defun opencode--permission-on-exit ()
  "Keep a dismissed request pending; only a suspended one stays active.
The exit of another transient whose suffix opened this one, such as
drover's menu, comes once this one is shown, and does not count."
  (unless (opencode-transient-displayed-p 'opencode-permission-transient)
    (remove-hook 'transient-exit-hook #'opencode--permission-on-exit)
    (if (opencode-transient-suspended-p 'opencode-permission-transient)
        (message "OpenCode permission suspended; %s resumes it" (opencode-command-keys 'opencode-respond))
      (when opencode--active-permission
        (setq opencode--active-permission nil)
        (message "OpenCode permission deferred; %s resumes it" (opencode-command-keys 'opencode-respond))))))

(defun opencode--permission-close-transient ()
  "Close the permission transient because its request was resolved elsewhere."
  (setq opencode--active-permission nil
        opencode--permission-transient-state nil)
  (cond
   ((opencode-transient-displayed-p 'opencode-permission-transient)
    (transient--emergency-exit))
   ((opencode-transient-suspended-p 'opencode-permission-transient)
    (pop transient--stack))))

(defun opencode-respond-permission (&optional permission)
  "Open the transient of PERMISSION, else the oldest pending one.
A transient suspended from this buffer is resumed instead.  Opened in
place (see `opencode-respond-session'), it always shows its menu."
  (interactive)
  (opencode--with-transient-menu opencode--answering-in-place
    (cond
     ((and (opencode-transient-suspended-p 'opencode-permission-transient)
           (eq (plist-get opencode--active-permission :buffer) (current-buffer))
           (or (null permission)
               (equal (plist-get permission :id) (plist-get opencode--active-permission :id))))
      (add-hook 'transient-exit-hook #'opencode--permission-on-exit)
      (transient-resume))
     ((bound-and-true-p transient--prefix)
      (user-error "Finish or suspend the active transient first"))
     (t
      (let ((permission (or permission (car opencode-pending-permissions)
                            (user-error "No pending OpenCode permission"))))
        (when (opencode-transient-suspended-p 'opencode-permission-transient)
          (pop transient--stack))
        (setq opencode--active-permission (list :buffer (current-buffer) :id (plist-get permission :id))
              opencode--permission-transient-state
              (list :buffer (current-buffer) :permission permission))
        (add-hook 'transient-exit-hook #'opencode--permission-on-exit)
        (call-interactively #'opencode-permission-transient))))))

(defun opencode--permission-at-point ()
  "Return the queued permission of the tool block at point, if any."
  (when-let* ((overlay (opencode-block-at (point)))
              ((eq (plist-get (opencode-block-data overlay) :waiting-kind) 'permission)))
    (let ((id (plist-get (opencode-block-data overlay) :key)))
      (seq-find (lambda (permission) (equal (alist-get 'id (plist-get permission :source)) id))
                opencode-pending-permissions))))

(defun opencode--permission-provider ()
  "Describe this buffer's pending permissions for `opencode-respond'."
  (list :kind 'permission
        :pending opencode-pending-permissions
        :at-point (when-let* ((permission (opencode--permission-at-point)))
                    (lambda () (opencode-respond-permission permission)))
        :respond #'opencode-respond-permission
        :sync (lambda (callback) (opencode-permission-sync nil callback))))

(add-to-list 'opencode-request-providers #'opencode--permission-provider)
(add-hook 'opencode-blocks-attach-functions #'opencode--permission-attach)

;;;; Events and synchronization

(defun opencode--permission-asked (event)
  "Queue the permission request of EVENT in its session's buffer.
A session without a buffer gets one in the background, so the request has
somewhere to wait."
  (let* ((request (alist-get 'data event))
         (session (alist-get 'sessionID request)))
    (if-let* ((buffer (gethash session (opencode-sv opencode-session-buffers)))
              ((buffer-live-p buffer)))
        (with-current-buffer buffer (opencode-permission-queue request))
      (opencode-session-open-by-id
       session :no-display t
       :callback (lambda (_info) (opencode-permission-queue request))))))

(defun opencode--permission-replied (event)
  "Forget the permission EVENT says was answered, by any client."
  (let ((data (alist-get 'data event)))
    (opencode-with-session-buffer (alist-get 'sessionID data)
      (when-let* ((permission (opencode--permission-find (alist-get 'requestID data))))
        (opencode--permission-forget
         permission
         (pcase (alist-get 'reply data)
           ("reject" " [rejected]") ("always" " [accepted always]") (_ " [accepted]"))
         (if (equal (alist-get 'reply data) "reject") 'error 'success))))))

(defun opencode-permission-sync (&optional _info callback)
  "Reconcile this buffer's permission queue with the server's pending requests.
A request the server no longer has is reported lost; one asked while no
stream listened is queued.  CALLBACK, when given, is called with no
argument in this buffer afterwards, even when the request failed."
  (if (not opencode-session-id)
      (when callback (funcall callback))
    (let ((known (copy-sequence opencode-pending-permissions)))
      (opencode-request
       'get (format "/api/session/%s/permission" opencode-session-id)
       :then (lambda (response)
               (let* ((requests (opencode-request-data response))
                      (ids (mapcar (lambda (request) (alist-get 'id request)) requests)))
                 (dolist (permission known)
                   (when (and (memq permission opencode-pending-permissions)
                              (not (member (plist-get permission :id) ids)))
                     (opencode--permission-forget permission " [lost]" 'error)
                     (opencode-output-notice
                      (format "The OpenCode service no longer has the permission request “%s”."
                              (plist-get permission :title))
                      'opencode-attention)))
                 (dolist (request requests)
                   (opencode-permission-queue request)))
               (when callback (funcall callback)))
       :else (lambda (error)
               (message "OpenCode permission sync failed: %s" (opencode-error-describe error))
               (when callback (funcall callback)))))))

(opencode-events-register "permission.asked" #'opencode--permission-asked)
(opencode-events-register "permission.replied" #'opencode--permission-replied)
(add-hook 'opencode-session-resync-functions #'opencode-permission-sync)
(add-hook 'opencode-session-replayed-hook #'opencode-permission-sync)

(provide 'opencode-permission)
;;; opencode-permission.el ends here
