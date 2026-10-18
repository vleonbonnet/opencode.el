;;; opencode-notify-test.el --- Tests of notifications  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-notification-kinds ()
  "Return the kinds of the notifications shown, oldest first."
  (reverse (mapcar (lambda (notification) (alist-get 'kind notification))
                   opencode-test-notifications)))

(ert-deftest opencode-test-notification-default-filters ()
  "Finished turns and retries stay out of *Messages*; only retries stay out of the OS."
  (should-not (opencode-notification-messages-default-p '((kind . finished))))
  (should-not (opencode-notification-messages-default-p '((kind . retry))))
  (should (opencode-notification-messages-default-p '((kind . permission))))
  (should (opencode-notification-messages-default-p '((kind . failed))))
  (should-not (opencode-notification-os-default-p '((kind . retry))))
  (should (opencode-notification-os-default-p '((kind . finished)))))

(ert-deftest opencode-test-notify-routes-and-logs ()
  "Every notification is logged; the filters gate *Messages* and the OS."
  (let ((shown nil)
        (echoed nil)
        (notification '((title . "OpenCode finished") (message . "Fix 50% bug")
                        (variant . "success") (kind . finished))))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args) (push (apply #'format format args) echoed))))
      (let ((opencode-notify-os-function (lambda (n) (push n shown)))
            (opencode-notification-messages-filter #'opencode-notification-messages-default-p)
            (opencode-notification-os-filter #'opencode-notification-os-default-p))
        (with-current-buffer (get-buffer-create opencode-notifications-buffer)
          (let ((inhibit-read-only t)) (erase-buffer)))
        (opencode-notify notification)
        (should (equal shown (list notification)))
        (should-not echoed)
        (with-current-buffer opencode-notifications-buffer
          (should (string-match-p "\\[success\\] OpenCode finished: Fix 50% bug\n\\'" (buffer-string))))
        ;; A failing backend is reported, never signaled.
        (let ((opencode-notify-os-function (lambda (_) (error "No bus")))
              (opencode-notification-messages-filter t))
          (opencode-notify notification)
          (should (equal (car echoed) "OpenCode: cannot show a notification: No bus"))
          (should (equal (cadr echoed) "OpenCode finished: Fix 50% bug")))
        (let ((opencode-notification-os-filter nil))
          (setq shown nil)
          (opencode-notify notification)
          (should-not shown))))))

(ert-deftest opencode-test-notify-turn-ended ()
  "A turn ending in an undisplayed session is notified and remembered once."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-dispatch (opencode-test-events id))
      (should (equal (opencode-test-notification-kinds) '(finished)))
      (let ((notification (car opencode-test-notifications)))
        (should (equal (alist-get 'session notification) id))
        (should (equal (alist-get 'message notification) "text")))
      (with-current-buffer buffer
        (opencode-notify-turn-ended 'failed))
      (should (equal (opencode-test-notification-kinds) '(finished failed)))
      (should (equal (mapcar (lambda (info) (alist-get 'id info)) opencode-alerted-sessions)
                     (list id))))))

(ert-deftest opencode-test-notify-skips-subagents ()
  "A subagent's turn ending is left to its parent."
  (opencode-test-with-server
    (let ((child (opencode-test-scenario "subagent-child")))
      (with-current-buffer (opencode-test-open-empty child)
        (should (alist-get 'parentID opencode-session-info))
        (opencode-notify-turn-ended 'succeeded))
      (should-not opencode-test-notifications)
      (should-not opencode-alerted-sessions))))

(ert-deftest opencode-test-notify-requests-and-retries ()
  "Permission requests and retries of undisplayed sessions are notified."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "permission"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")
      (should (equal (opencode-test-notification-kinds) '(permission)))
      (should (equal (alist-get 'message (car opencode-test-notifications))
                     "Shell command: rm junk.txt"))
      (opencode-test-dispatch
       `(((type . "session.retry.scheduled")
          (data (sessionID . ,id) (assistantMessageID . "msg_x") (attempt . 2) (at . 0)
                (error (type . "provider.overloaded") (message . "Overloaded"))))))
      (should (equal (car (last (opencode-test-notification-kinds))) 'retry))
      (should (equal (alist-get 'message (car opencode-test-notifications)) "Overloaded\n\nRetry #2"))
      (with-current-buffer buffer
        (should (equal (alist-get 'attempt opencode-session-retry) 2))))))

(provide 'opencode-notify-test)
;;; opencode-notify-test.el ends here
