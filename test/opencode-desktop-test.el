;;; opencode-desktop-test.el --- Tests of desktop saving and restoring  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)
(require 'desktop)

(defvar opencode-test-service-callbacks nil
  "(CALLBACK . ERRBACK) pairs waiting for the service, newest first.")

(defmacro opencode-test-with-desktop (&rest body)
  "Run BODY with the recorded sessions served and the service held until answered."
  (declare (indent 0) (debug t))
  `(opencode-test-with-server
     (let ((opencode-test-service-callbacks nil)
           (placeholders nil))
       (cl-letf (((symbol-function 'opencode-service-ensure)
                  (lambda (callback &optional errback)
                    (push (cons callback errback) opencode-test-service-callbacks))))
         (unwind-protect (progn ,@body)
           (dolist (buffer placeholders)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (opencode--desktop-stop))
               (kill-buffer buffer))))))))

(defun opencode-test-service-ready ()
  "Answer every waiting service request as ready."
  (let ((waiting (reverse opencode-test-service-callbacks)))
    (setq opencode-test-service-callbacks nil)
    (dolist (pair waiting) (funcall (car pair) '(:url "http://test")))))

(defun opencode-test-desktop-entry (id &optional point)
  "Return a desktop entry of recorded session ID at POINT."
  (list :version 1 :session-id id :directory opencode-test-workspace :point (or point 1)))

(ert-deftest opencode-test-desktop-entry-of-session ()
  "A session buffer saves its ID, directory and point."
  (opencode-test-with-desktop
    (let ((id (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open id)
        (goto-char 5)
        (should (desktop-save-buffer-p nil (buffer-name) major-mode))
        (should (equal (funcall desktop-save-buffer nil)
                       (list :version 1 :session-id id :directory default-directory :point 5)))))))

(ert-deftest opencode-test-desktop-restore-in-place ()
  "Restoring returns a placeholder at once and fills that buffer with the session."
  (opencode-test-with-desktop
    (let* ((id (opencode-test-scenario "text"))
           (entry (opencode-test-desktop-entry id 20))
           (buffer (opencode-desktop-restore nil "*OpenCode: text*" entry)))
      (push buffer placeholders)
      (with-current-buffer buffer
        (should (derived-mode-p 'opencode-desktop-pending-mode))
        (should (equal (funcall desktop-save-buffer nil) entry))
        (should (string-match-p "Waiting for the OpenCode service" (buffer-string))))
      ;; Restoring the same entry again finds the placeholder.
      (should (eq (opencode-desktop-restore nil "other" entry) buffer))
      (opencode-test-service-ready)
      (with-current-buffer buffer
        (should (derived-mode-p 'opencode-session-mode))
        (should (eq (gethash id opencode-session-buffers) buffer))
        (should (equal (buffer-name) "*OpenCode: text*"))
        (should (string-match-p "hello fixture" (buffer-string)))
        (should-not buffer-read-only)
        (should (= (point) 20))
        (should-not opencode--desktop-data)
        (should-not opencode--desktop-attempt)
        (should (equal (plist-get (funcall desktop-save-buffer nil) :session-id) id)))
      (should (eq (opencode-desktop-restore nil "again" entry) buffer)))))

(ert-deftest opencode-test-desktop-deleted-session ()
  "A deleted session leaves a placeholder that is still saved and can retry."
  (opencode-test-with-desktop
    (let* ((entry (opencode-test-desktop-entry (opencode-test-scenario "delete")))
           (buffer (opencode-desktop-restore nil "*deleted*" entry)))
      (push buffer placeholders)
      (opencode-test-service-ready)
      (with-current-buffer buffer
        (should (derived-mode-p 'opencode-desktop-pending-mode))
        (should (string-match-p "no longer exists" (buffer-string)))
        (should (equal (funcall desktop-save-buffer nil) entry))
        (should-not opencode--desktop-attempt)
        (opencode-desktop-retry)
        (should opencode--desktop-attempt)
        (should (= (length opencode-test-service-callbacks) 1))))))

(ert-deftest opencode-test-desktop-service-unavailable ()
  "A service that cannot start is reported in the placeholder."
  (opencode-test-with-desktop
    (let ((buffer (opencode-desktop-restore nil "*offline*"
                                            (opencode-test-desktop-entry (opencode-test-scenario "text")))))
      (push buffer placeholders)
      (funcall (cdr (pop opencode-test-service-callbacks)) "OpenCode did not start")
      (with-current-buffer buffer
        (should (derived-mode-p 'opencode-desktop-pending-mode))
        (should (string-match-p "OpenCode did not start" (buffer-string)))))))

(ert-deftest opencode-test-desktop-deadline-after-service ()
  "Waiting for the service, as while it imports the earlier sessions, uses none of the restore deadline."
  (opencode-test-with-desktop
    (let* ((opencode-desktop-restore-timeout 0.2)
           (buffer (opencode-desktop-restore nil "*slow service*"
                                             (opencode-test-desktop-entry (opencode-test-scenario "text")))))
      (push buffer placeholders)
      (accept-process-output nil 0.5)
      (with-current-buffer buffer
        (should-not (string-match-p "Timed out" (buffer-string)))
        (should opencode--desktop-attempt)
        (should-not opencode--desktop-timer))
      (opencode-test-service-ready)
      (with-current-buffer buffer
        (should (derived-mode-p 'opencode-session-mode))
        (should-not opencode--desktop-timer)))))

(ert-deftest opencode-test-desktop-late-answers-ignored ()
  "Answers to an abandoned attempt do nothing."
  (opencode-test-with-desktop
    (let ((buffer (opencode-desktop-restore nil "*late*"
                                            (opencode-test-desktop-entry (opencode-test-scenario "text")))))
      (push buffer placeholders)
      (let ((old (pop opencode-test-service-callbacks)))
        (with-current-buffer buffer (opencode-desktop-retry))
        (setq opencode-test-requests nil)
        (funcall (car old) '(:url "http://test"))
        (should-not opencode-test-requests)
        (funcall (cdr old) "stale failure")
        (with-current-buffer buffer
          (should-not (string-match-p "stale failure" (buffer-string)))
          (should opencode--desktop-attempt))))))

(ert-deftest opencode-test-desktop-history-failure ()
  "A failed transcript load turns the buffer back into a placeholder."
  (opencode-test-with-desktop
    (let* ((id (opencode-test-scenario "text"))
           (opencode-test-routes (cons (list 'get "/message\\'" (lambda (_) '(:error 500 "Database busy")))
                                       opencode-test-routes))
           (buffer (opencode-desktop-restore nil "*history*" (opencode-test-desktop-entry id))))
      (push buffer placeholders)
      (opencode-test-service-ready)
      (with-current-buffer buffer
        (should (derived-mode-p 'opencode-desktop-pending-mode))
        (should-not (get-buffer-process buffer))
        (should-not (gethash id opencode-session-buffers))
        (should (string-match-p "Database busy" (buffer-string)))
        (should (equal (plist-get (funcall desktop-save-buffer nil) :session-id) id))))))

(ert-deftest opencode-test-desktop-superseded ()
  "A placeholder whose session was opened meanwhile gives way to that buffer."
  (opencode-test-with-desktop
    (let* ((id (opencode-test-scenario "text"))
           (buffer (opencode-desktop-restore nil "*superseded*" (opencode-test-desktop-entry id)))
           (open (opencode-test-open id)))
      (opencode-test-service-ready)
      (should-not (buffer-live-p buffer))
      (should (eq (gethash id opencode-session-buffers) open)))))

(ert-deftest opencode-test-desktop-entry-validated ()
  "Malformed entries are rejected before any buffer is made."
  (dolist (entry (list '(:version 2 :session-id "ses_a" :directory "/tmp/")
                       '(:version 1 :session-id "bad id" :directory "/tmp/")
                       '(:version 1 :session-id "ses_a" :directory "/ssh:host:/tmp/")))
    (should-error (opencode-desktop-restore nil "x" entry))))

(ert-deftest opencode-test-desktop-handlers ()
  "Desktop restores session buffers and placeholders through the package."
  (dolist (mode '(opencode-session-mode opencode-desktop-pending-mode))
    (should (eq (alist-get mode desktop-buffer-mode-handlers) #'opencode-desktop-restore))))

(ert-deftest opencode-test-desktop-file-round-trip ()
  "An entry read back from a desktop file restores through `desktop-create-buffer'."
  (opencode-test-with-desktop
    (let* ((id (opencode-test-scenario "text"))
           (entry (read (prin1-to-string (opencode-test-desktop-entry id 3))))
           (buffer (cl-progv '(desktop-buffer-ok-count desktop-buffer-fail-count desktop-first-buffer)
                       '(0 0 nil)
                     (desktop-create-buffer 208 nil "*from desktop*" 'opencode-session-mode nil
                                            3 '(nil nil) nil entry))))
      (push buffer placeholders)
      (should (eq (buffer-local-value 'major-mode buffer) 'opencode-desktop-pending-mode))
      (opencode-test-service-ready)
      (should (eq (buffer-local-value 'major-mode buffer) 'opencode-session-mode)))))

(provide 'opencode-desktop-test)
;;; opencode-desktop-test.el ends here
