;;; opencode-edits-test.el --- Tests of edit processing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(defvar opencode-test-edits nil
  "Calls of the recording edit hook, newest first.")

(defun opencode-test-record-edit (file ranges)
  "Record an edit of FILE with RANGES."
  (push (cons (file-name-nondirectory file) ranges) opencode-test-edits))

(defun opencode-test-indent-edited (text ranges)
  "Return TEXT after indenting it as an edited .el file with RANGES."
  (let ((file (make-temp-file "opencode-indent" nil ".el" text)))
    (unwind-protect
        (progn
          (opencode-indent-edited-file file ranges)
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-string)))
      (delete-file file))))

(ert-deftest opencode-test-indent-edited-forms ()
  "Only the top-level forms the ranges overlap are reindented."
  (let ((text "(when a\n(b))\n(when c\n(d)\n(e))\n"))
    (should (equal (opencode-test-indent-edited text '((4 . 4)))
                   "(when a\n(b))\n(when c\n  (d)\n  (e))\n"))
    (should (equal (opencode-test-indent-edited text '((2)))
                   "(when a\n  (b))\n(when c\n(d)\n(e))\n"))
    (should (equal (opencode-test-indent-edited text nil)
                   "(when a\n  (b))\n(when c\n  (d)\n  (e))\n"))))

(ert-deftest opencode-test-indent-leaves-org-to-its-indenter ()
  "Org files are only reindented by `opencode-org-indent-function'."
  (let ((file (make-temp-file "opencode-indent" nil ".org"
                              "#+begin_src elisp\n(when a\n(b))\n#+end_src\n"))
        (seen nil))
    (unwind-protect
        (progn
          (opencode-indent-edited-file file '((2 . 3)))
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "^(b))" (buffer-string))))
          (let ((opencode-org-indent-function (lambda (regions) (setq seen regions))))
            (opencode-indent-edited-file file '((2 . 3))))
          (should (= (length seen) 1)))
      (delete-file file))))

(ert-deftest opencode-test-tool-edited-files ()
  "Edited files and line ranges come from the tool's diffs, writes from its input."
  (let ((patch (concat "Index: C:\\w\\notes.txt\n===\n--- C:\\w\\notes.txt\n+++ C:\\w\\notes.txt\n"
                       "@@ -1,3 +1,4 @@\n alpha\n beta\n-gamma\n+gamma\n+bye\n")))
    (should (equal (opencode-tool-edited-files
                    "patch" nil `((files ((file . "notes.txt") (patch . ,patch) (status . "modified"))
                                         ((file . "gone.txt") (patch . "") (status . "deleted")))))
                   '(("notes.txt" (3 . 4)))))
    (should (equal (opencode-tool-edited-files "write" '((path . "new.txt") (content . "x")) nil)
                   '(("new.txt"))))
    (should-not (opencode-tool-edited-files "read" '((path . "notes.txt")) nil))))

(ert-deftest opencode-test-live-edits-run-hooks ()
  "A live turn's edits run the per-file hooks, then the finished hooks report back."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "edit-tools"))
           (buffer (opencode-test-open-empty id))
           (opencode-test-edits nil)
           (finished nil)
           (opencode-file-edited-functions (list #'opencode-test-record-edit))
           (opencode-files-finished-editing-functions
            (list (lambda (files)
                    (setq finished (mapcar #'file-name-nondirectory files))
                    (opencode-report-diagnostic "- notes.txt:4: warning: trailing word")))))
      (opencode-test-dispatch (opencode-test-events id))
      ;; Appending to the CRLF notes.txt adds line 4 alone: the patch keeps the file's line ends.
      (should (equal (reverse opencode-test-edits)
                     '(("hello.txt" (1 . 1)) ("hello.txt" (1 . 1)) ("notes.txt" (4 . 4)))))
      (should (equal finished '("hello.txt" "notes.txt")))
      (with-current-buffer buffer
        (should-not opencode--files-edited-this-turn))
      (let ((request (seq-find (lambda (request) (string-suffix-p "/synthetic" (plist-get request :path)))
                               opencode-test-requests)))
        (should request)
        (should (eq (alist-get 'resume (plist-get request :body)) t))
        (should (string-match-p "trailing word" (alist-get 'text (plist-get request :body))))))))

(ert-deftest opencode-test-replayed-edits-run-nothing ()
  "Opening a session never processes the files its history edited."
  (opencode-test-with-server
    (let ((opencode-test-edits nil)
          (opencode-file-edited-functions (list #'opencode-test-record-edit)))
      (with-current-buffer (opencode-test-open (opencode-test-scenario "edit-tools"))
        (should-not opencode-test-edits)
        (should-not opencode--files-edited-this-turn)))))

(ert-deftest opencode-test-finished-editing-queue ()
  "Runs wait for an asynchronous function; an interrupted turn keeps its files."
  (opencode-test-with-server
    (let* ((first (opencode-test-open (opencode-test-scenario "text")))
           (second (opencode-test-open (opencode-test-scenario "shell")))
           (calls nil)
           (opencode--finished-editing-current nil)
           (opencode--finished-editing-queue nil)
           (opencode-files-finished-editing-functions
            (list (lambda (files) (push (cons (current-buffer) files) calls) :opencode-async))))
      (with-current-buffer first
        (setq opencode--files-edited-this-turn '("/a"))
        (opencode--edits-turn-ended 'interrupted)
        (should (equal opencode--files-edited-this-turn '("/a")))
        (should-not calls)
        (opencode--edits-turn-ended 'succeeded))
      (with-current-buffer second
        (setq opencode--files-edited-this-turn '("/b"))
        (opencode--edits-turn-ended 'failed))
      ;; The second run waits for the first one's asynchronous function.
      (should (equal calls (list (cons first '("/a")))))
      (opencode-hook-finished)
      (should (equal (car calls) (cons second '("/b"))))
      (opencode-hook-finished)
      (should-not opencode--finished-editing-current))))

(ert-deftest opencode-test-visit-edited-file ()
  "Stale buffers are reread without asking; buffers with unsaved edits are skipped."
  (let ((file (make-temp-file "opencode-visit" nil ".txt" "one\n")))
    (unwind-protect
        (let ((buffer (find-file-noselect file)))
          (unwind-protect
              (progn
                (with-temp-file file (insert "two\n"))
                (with-current-buffer buffer (set-visited-file-modtime '(0 0)))
                (should (eq (opencode-visit-edited-file file) buffer))
                (with-current-buffer buffer
                  (should (equal (buffer-string) "two\n"))
                  (insert "mine")
                  (should-not (opencode-visit-edited-file file))))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))
          ;; A buffer opened for processing is killed afterwards.
          (let ((owned (opencode-visit-edited-file file)))
            (should (buffer-local-value 'opencode--edit-owned-buffer owned))
            (opencode-release-edited-file owned)
            (should-not (buffer-live-p owned))))
      (delete-file file))))

(ert-deftest opencode-test-save-refuses-changed-file ()
  "A file changed on disk again is reread, never overwritten."
  (let ((file (make-temp-file "opencode-save" nil ".txt" "one\n")))
    (unwind-protect
        (let ((buffer (find-file-noselect file)))
          (unwind-protect
              (with-current-buffer buffer
                (insert "zero\n")
                (with-temp-file file (insert "other\n"))
                (set-file-times file (time-add nil 10))
                (should-not (let ((inhibit-message t)) (opencode-save-edited-file file)))
                (should (equal (buffer-string) "other\n")))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "other\n"))))
      (delete-file file))))

(ert-deftest opencode-test-flycheck-report ()
  "Errors and warnings are listed relative to the session directory."
  (let ((errors (list (list :file "/w/a.el" :line 3 :column 5 :level 'error :message "boom")
                      (list :file "/w/a.el" :line 9 :level 'info :message "fyi")
                      (list :file nil :line nil :level 'warning :message "hmm"))))
    (cl-letf (((symbol-function 'flycheck-error-level) (lambda (err) (plist-get err :level)))
              ((symbol-function 'flycheck-error-filename) (lambda (err) (plist-get err :file)))
              ((symbol-function 'flycheck-error-line) (lambda (err) (plist-get err :line)))
              ((symbol-function 'flycheck-error-column) (lambda (err) (plist-get err :column)))
              ((symbol-function 'flycheck-error-format-message-and-id)
               (lambda (err &rest _) (plist-get err :message))))
      (should (equal (opencode--flycheck-report "/w/b.el" "/w/" errors)
                     "- a.el:3:5: error: boom\n- b.el:1: warning: hmm"))
      (should-not (opencode--flycheck-report "/w/b.el" "/w/" (list (cadr errors)))))))

(ert-deftest opencode-test-session-roots ()
  "Opening and killing session buffers reports their directories."
  (opencode-test-with-server
    (let ((reports nil)
          (pending nil)
          (opencode--session-roots-timer nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function) (push function pending) 'timer))
                ((symbol-function 'timerp) (lambda (object) (eq object 'timer))))
        (cl-flet ((run-pending ()
                    (while pending (funcall (pop pending)))))
          (let ((opencode-session-roots-functions (list (lambda (roots) (push roots reports)))))
            (let ((buffer (opencode-test-open (opencode-test-scenario "text"))))
              (run-pending)
              (should (equal (car reports) (list (opencode-normalize-directory opencode-test-workspace))))
              (kill-buffer buffer)
              (run-pending)
              (should-not (car reports)))))))))

(provide 'opencode-edits-test)
;;; opencode-edits-test.el ends here
