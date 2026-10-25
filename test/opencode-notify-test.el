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

(ert-deftest opencode-test-notify-permission-dismissal-is-request-scoped ()
  "Answering each permission dismisses only its toast, even across sessions."
  (dolist (decision '(once always reject))
    (opencode-test-with-server
     (let ((first (opencode-test-open-empty (opencode-test-scenario "permission")))
           (second (opencode-test-open-empty (opencode-test-scenario "text"))))
       (dolist (buffer (list first second))
         (with-current-buffer buffer
           (dolist (id '("per_one" "per_two"))
             (opencode-permission-queue
              `((id . ,id) (action . "shell") (resources "echo hello") (save))))))
       (with-current-buffer first
         (opencode-permission-reply (car opencode-pending-permissions) decision)
         (should (= (length opencode--notification-dismissers) 1))
         (let ((dismissed (car opencode-test-dismissed-notifications)))
           (should (= (length opencode-test-dismissed-notifications) 1))
           (should (equal (alist-get 'request dismissed) "per_one"))
           (should (equal (alist-get 'session dismissed) opencode-session-id)))
         (opencode-test-dispatch
          `(((type . "permission.replied")
             (data (sessionID . ,opencode-session-id) (requestID . "per_two") (reply . "reject")))))
         (should-not opencode--notification-dismissers)
         (should (= (length opencode-test-dismissed-notifications) 2)))
       (with-current-buffer second
         (should (= (length opencode--notification-dismissers) 2)))))))

(ert-deftest opencode-test-notify-question-dismissal-is-request-scoped ()
  "Submitting or declining a question closes it; another client's answer does too."
  (dolist (cancel '(nil t))
    (opencode-test-with-server
     (let* ((id (opencode-test-scenario "question"))
            (buffer (opencode-test-open-empty id))
            (event (seq-find (lambda (event) (equal (alist-get 'type event) "form.created"))
                             (opencode-test-events id)))
            (form (copy-tree (alist-get 'form (alist-get 'data event)))))
       (with-current-buffer buffer
         (opencode-form-queue form)
         (let ((other (copy-tree form)))
           (setf (alist-get 'id other) "frm_other")
           (opencode-form-queue other))
         (opencode-form-send (car opencode-pending-forms) cancel)
         (should (= (length opencode-test-dismissed-notifications) 1))
         (should (equal (alist-get 'request (car opencode-test-dismissed-notifications))
                        (alist-get 'id form)))
         (should (= (length opencode--notification-dismissers) 1))
         (let ((resolved `((type . ,(if cancel "form.cancelled" "form.replied"))
                           (data (sessionID . ,id) (id . "frm_other")))))
           (opencode-test-dispatch (list resolved resolved)))
         (should-not opencode--notification-dismissers)
         (should (= (length opencode-test-dismissed-notifications) 2)))))))

(ert-deftest opencode-test-notify-prompt-and-kill-dismissal ()
  "A new prompt clears the outcome; killing a buffer clears only its remaining toasts."
  (opencode-test-with-server
   (let ((first (opencode-test-open-empty (opencode-test-scenario "text")))
         (second (opencode-test-open-empty (opencode-test-scenario "permission"))))
     (dolist (buffer (list first second))
       (with-current-buffer buffer
         (opencode-notify-turn-ended 'succeeded)))
     (with-current-buffer first
       (opencode-permission-queue '((id . "per_one") (action . "shell")
                                    (resources "echo hello") (save)))
       (opencode-session-send "Next prompt")
       (should (equal (mapcar (lambda (n) (alist-get 'kind n))
                              opencode-test-dismissed-notifications)
                      '(finished)))
       (should (= (length opencode--notification-dismissers) 1)))
     (kill-buffer first)
     (should (= (length opencode-test-dismissed-notifications) 2))
     (with-current-buffer second
       (should (= (length opencode--notification-dismissers) 1)))
     (kill-buffer second)
     (should (= (length opencode-test-dismissed-notifications) 3)))))

(ert-deftest opencode-test-notify-other-client-prompt-and-command ()
  "Admitted user prompts and slash commands clear outcomes, synthetic inbox items do not."
  (opencode-test-with-server
   (let ((id (opencode-test-scenario "text")))
     (with-current-buffer (opencode-test-open-empty id)
       (opencode-notify-turn-ended 'succeeded)
       (opencode-test-dispatch
        `(((type . "session.inbox.enqueued")
           (data (sessionID . ,id) (inboxID . "inb_synthetic") (item (type . "synthetic"))))))
       (should-not opencode-test-dismissed-notifications)
       (opencode-test-dispatch
        `(((type . "session.inbox.enqueued")
           (data (sessionID . ,id) (inboxID . "inb_user") (item (type . "user"))))))
       (should (= (length opencode-test-dismissed-notifications) 1))
       (opencode-notify-turn-ended 'succeeded)
       (opencode-session-command "review" "this patch")
       (should (= (length opencode-test-dismissed-notifications) 2))
       (should-not opencode--notification-dismissers)))))

(ert-deftest opencode-test-notify-replacement-and-failing-dismissal ()
  "Replacement retires its predecessor; one broken closer never blocks the others."
  (with-temp-buffer
    (let ((closed nil)
          (count 0)
          (opencode-notification-messages-filter nil)
          (opencode-notification-os-filter t))
      (let ((opencode-notify-os-function
             (lambda (_notification)
               (let ((number (cl-incf count)))
                 (lambda ()
                   (push number closed)
                   (when (= number 2) (error "Notification service unavailable")))))))
        (opencode-notify '((title . "Done") (message . "First") (kind . finished)))
        (opencode-notify '((title . "Done") (message . "Second") (kind . finished)))
        (should (equal closed '(1)))
        (opencode-notify '((title . "Question") (message . "One") (kind . question) (request . "one")))
        (opencode-notify '((title . "Question") (message . "Two") (kind . question) (request . "two")))
        (opencode-dismiss-notifications)
        (should (equal (sort closed #'<) '(1 2 3 4)))
        (should-not opencode--notification-dismissers)
        (opencode-dismiss-notifications)
        (should (= (length closed) 4))))))

(ert-deftest opencode-test-notify-dbus-dismisses-native-id ()
  "The native D-Bus backend's closer sends the ID returned by Notify."
  (skip-unless (and (featurep 'dbusbind) (require 'notifications nil t)))
  (let (closed)
    (cl-letf (((symbol-function 'notifications-notify) (lambda (&rest _) 1234))
              ((symbol-function 'notifications-close-notification) (lambda (id) (setq closed id))))
      (let ((dismiss (opencode--notify-dbus '((title . "Question") (message . "Which?")))))
        (should (functionp dismiss))
        (funcall dismiss)
        (should (eql closed 1234))))))

(ert-deftest opencode-test-notify-macos-groups-are-request-scoped ()
  "Terminal-notifier groups keep sessions and simultaneous requests independent."
  (let (commands)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/bin/terminal-notifier"))
              ((symbol-function 'start-process) (lambda (&rest args) (push args commands))))
      (let ((first (opencode--notify-macos '((session . "one") (kind . question) (request . "a")
                                             (title . "One asks") (message . "Which?"))))
            (second (opencode--notify-macos '((session . "two") (kind . question) (request . "a")
                                              (title . "Two asks") (message . "Which?"))))
            (third (opencode--notify-macos '((session . "one") (kind . question) (request . "b")
                                             (title . "One asks") (message . "Which?")))))
        (let ((groups (mapcar (lambda (args) (cadr (member "-group" args))) commands)))
          (should (= (length (delete-dups (copy-sequence groups))) 3)))
        (mapc #'funcall (list first second third))
        (should (equal (mapcar (lambda (args) (cadr (member "-remove" args))) (seq-take commands 3))
                       '("opencode:one:question:b" "opencode:two:question:a" "opencode:one:question:a")))))))

(provide 'opencode-notify-test)
;;; opencode-notify-test.el ends here
