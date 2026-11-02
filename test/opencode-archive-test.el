;;; opencode-archive-test.el --- Tests of sending to archived sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Input sent to an archived session, from its buffer or by
;; `opencode-session-send', restores the session from the archive first;
;; other sessions send as before, without asking the service anything.

;;; Code:

(require 'opencode-test-helpers)

(defvar opencode-test-restore-answer nil
  "How the fake service answers a restore: `hold' defers it, or (:error ...).
Nil answers at once with the restored session's info.")

(defmacro opencode-test-with-archived (&rest body)
  "Run BODY in the buffer of an archived session, the registry not holding it.
The session is the recorded text scenario's, opened with an empty
transcript; requests to archive or restore it answer as
`opencode-test-restore-answer' says."
  (declare (indent 0) (debug t))
  `(opencode-test-with-server
     (let* ((opencode-test-restore-answer opencode-test-restore-answer)
            (opencode-test-routes
             (cons (list 'patch "\\`/api/session/\\(ses_[A-Za-z0-9]+\\)\\'"
                         (lambda (request)
                           (let ((info `((id . ,(plist-get request :match)) (time (created . 1) (updated . 2)))))
                             (pcase opencode-test-restore-answer
                               ('hold (cons :defer (lambda () `((data . ,info)))))
                               ((and `(:error . ,_) answer) answer)
                               (_ `((data . ,info)))))))
                   opencode-test-routes)))
       (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
         (opencode--note-archived opencode-session-id 1000)
         (setq opencode-test-requests nil)
         ,@body))))

(defun opencode-test-archive-traffic ()
  "Return the restores and sends requested, oldest first, as (KIND BODY)."
  (nreverse
   (delq nil (mapcar (lambda (request)
                       (let ((path (plist-get request :path)))
                         (cond
                          ((eq (plist-get request :method) 'patch)
                           (list 'patch (plist-get request :body)))
                          ((string-match "/\\(prompt\\|command\\|shell\\)\\'" path)
                           (list (intern (match-string 1 path)) (plist-get request :body))))))
                     opencode-test-requests))))

(defun opencode-test-archive-kinds ()
  "Return the kinds of the restores and sends requested, oldest first."
  (mapcar #'car (opencode-test-archive-traffic)))

(defun opencode-test-archive-type (text)
  "Type TEXT at the end of the current buffer's input."
  (goto-char (point-max))
  (insert text))

(defun opencode-test-archive-input ()
  "Return the input typed at the current buffer's prompt."
  (buffer-substring-no-properties (opencode-pmark) (point-max)))

(ert-deftest opencode-test-archived-prompt-restores-first ()
  "A prompt sent to an archived session restores it, then goes once restored.
The input waits at the prompt meanwhile; sending it again asks nothing
more of the service."
  (let ((opencode-test-restore-answer 'hold))
    (opencode-test-with-archived
      (should (opencode-session-archived-p))
      (opencode-test-archive-type "wake up")
      (opencode-send-input)
      (should (equal (opencode-test-archive-traffic) '((patch ((archived . :false))))))
      (should (equal (opencode-test-archive-input) "wake up"))
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest arguments) (push (apply #'format-message format arguments) messages))))
          (opencode-send-input))
        (should (equal messages '("The session is being restored from the archive; the input is sent then"))))
      (should (equal (opencode-test-archive-kinds) '(patch)))
      (should (opencode-test-release))
      (should-not opencode-test-deferred)
      (pcase-let ((`((patch ,_) (prompt ,body)) (opencode-test-archive-traffic)))
        (should (equal (alist-get 'text body) "wake up"))
        (should (equal (alist-get 'delivery body) "steer")))
      (should (equal (opencode-test-archive-input) ""))
      (should-not (opencode-session-archived-p))
      ;; Restored, the session sends as any other.
      (opencode-test-archive-type "and again")
      (opencode-send-input)
      (should (equal (opencode-test-archive-kinds) '(patch prompt prompt))))))

(ert-deftest opencode-test-archived-command-and-shell-restore-first ()
  "A slash command or a shell command sent to an archived session restores it first."
  (opencode-test-with-archived
    (opencode-test-archive-type "/review HEAD")
    (opencode-send-input)
    (pcase-let ((`((patch ((archived . :false))) (command ,body)) (opencode-test-archive-traffic)))
      (should (equal (alist-get 'name body) "review"))
      (should (equal (alist-get 'text body) "HEAD")))
    (opencode--note-archived opencode-session-id 2000)
    (setq opencode-test-requests nil)
    (opencode-test-archive-type "!ls")
    (opencode-send-input)
    (should (equal (opencode-test-archive-traffic)
                   '((patch ((archived . :false))) (shell ((command . "ls"))))))))

(ert-deftest opencode-test-archived-queued-input-keeps-delivery ()
  "Input queued after the turn of an archived session is queued once restored."
  (let ((opencode-test-restore-answer 'hold))
    (opencode-test-with-archived
      (opencode-test-archive-type "after the turn")
      (opencode-queue-input)
      (should (equal (opencode-test-archive-kinds) '(patch)))
      (should (opencode-test-release))
      (pcase-let ((`(,_ (prompt ,body)) (opencode-test-archive-traffic)))
        (should (equal (alist-get 'delivery body) "queue"))))))

(ert-deftest opencode-test-archived-session-send-restores-once ()
  "Prompts sent by `opencode-session-send' to an archived session restore it once.
The registry tells it is archived, and learns the restore as soon as
the service answered; the prompts then go in the order they were sent."
  (let ((opencode-test-restore-answer 'hold))
    (opencode-test-with-archived
      (let* ((id opencode-session-id)
             (admitted nil))
        ;; The registry holds it and tells, whatever the buffer's info says.
        (opencode--note-archived id nil)
        (should-not (opencode-session-archived-p))
        (opencode-state--store id (opencode-state--make-entry :id id :title "Text" :archived 1.0))
        (should (opencode-session-archived-p id))
        (opencode-session-send "first" :show t :then (lambda (_) (push 'first admitted)))
        (opencode-session-send "second" :show t :then (lambda (_) (push 'second admitted)))
        (should (equal (opencode-test-archive-kinds) '(patch)))
        (should (opencode-test-release))
        (should-not (plist-get (opencode-state-get id) :archived))
        (should (equal (mapcar (lambda (request) (alist-get 'text (cadr request)))
                               (cdr (opencode-test-archive-traffic)))
                       '("first" "second")))
        (should (equal (nreverse admitted) '(first second)))
        ;; The event following the answer changes nothing more.
        (opencode-test-dispatch `(((type . "session.unarchived") (data (sessionID . ,id)))))
        (should-not (opencode-session-archived-p id))))))

(ert-deftest opencode-test-archived-sends-share-the-restore ()
  "Input sent at the prompt and prompts sent by ID while a restore runs share it."
  (let ((opencode-test-restore-answer 'hold))
    (opencode-test-with-archived
      (opencode-test-archive-type "from the prompt")
      (opencode-send-input)
      (opencode-session-send "from a list" :show t)
      (should (equal (opencode-test-archive-kinds) '(patch)))
      (should (opencode-test-release))
      (should (equal (mapcar (lambda (request) (alist-get 'text (cadr request)))
                             (cdr (opencode-test-archive-traffic)))
                     '("from the prompt" "from a list"))))))

(ert-deftest opencode-test-archived-restore-refused-keeps-input ()
  "A restore the service refuses sends nothing: the input stays, and one notice says why."
  (let ((opencode-test-restore-answer '(:error 409 "Session is busy")))
    (opencode-test-with-archived
      (opencode-test-archive-type "keep me")
      (opencode-send-input)
      (should (equal (opencode-test-archive-kinds) '(patch)))
      (should (equal (opencode-test-archive-input) "keep me"))
      (should (= (how-many "restoring the session from the archive failed" (point-min) (point-max)) 1))
      (should (string-search "Session is busy" (buffer-string)))
      (should (opencode-session-archived-p))
      ;; Nothing waits: sending again tries again.
      (opencode-send-input)
      (should (equal (opencode-test-archive-kinds) '(patch patch)))
      (should (equal (opencode-test-archive-input) "keep me")))))

(ert-deftest opencode-test-archived-session-send-refused-tells-caller ()
  "A refused restore calls the ELSE of `opencode-session-send' with its error, and sends nothing."
  (let ((opencode-test-restore-answer '(:error 409 "Session is busy")))
    (opencode-test-with-archived
      (let ((errors nil))
        (opencode-session-send "lost" :show t :else (lambda (error) (push error errors)))
        (should (equal (opencode-test-archive-kinds) '(patch)))
        (should (= (length errors) 1))
        (should (eq (opencode-error-method (car errors)) 'patch))
        (should (eql (opencode-error-status (car errors)) 409))
        (should (zerop (hash-table-count opencode--own-inputs))))
      ;; Without ELSE, the buffer says why.
      (opencode-session-send "lost too")
      (should (= (how-many "Not sent: restoring the session from the archive failed" (point-min) (point-max))
                 1))
      (should (zerop (hash-table-count (opencode--sent-input-table)))))))

(ert-deftest opencode-test-unarchived-session-sends-as-before ()
  "A session that is not archived sends at once, without restoring anything."
  (opencode-test-with-archived
    (opencode--note-archived opencode-session-id nil)
    (should-not (opencode-session-archived-p))
    (opencode-test-archive-type "plain")
    (opencode-send-input)
    (opencode-test-archive-type "!pwd")
    (opencode-send-input)
    (opencode-session-send "by id" :show t)
    (opencode-session-command "review" "HEAD")
    (should (equal (opencode-test-archive-kinds) '(prompt shell prompt command)))))

(ert-deftest opencode-test-archived-cancelled-check-restores-nothing ()
  "Input a send check keeps at the prompt restores nothing."
  (opencode-test-with-archived
    (let ((opencode-send-checks (list (lambda (_) 'cancel))))
      (opencode-test-archive-type "not now")
      (opencode-send-input)
      (should-not (opencode-test-archive-traffic))
      (should (equal (opencode-test-archive-input) "not now"))
      (should (opencode-session-archived-p)))))

(ert-deftest opencode-test-archive-events-reach-the-buffer ()
  "A session archived or restored elsewhere is known so in its buffer."
  (opencode-test-with-archived
    (let ((id opencode-session-id))
      (opencode-test-dispatch `(((type . "session.unarchived") (data (sessionID . ,id)))))
      (should-not (opencode-session-archived-p))
      (should-not (assq 'archived (alist-get 'time opencode-session-info)))
      (opencode-test-dispatch `(((type . "session.archived") (created . 5000) (data (sessionID . ,id)))))
      (should (opencode-session-archived-p))
      (should (equal (alist-get 'archived (alist-get 'time opencode-session-info)) 5000)))))

(ert-deftest opencode-test-archive-answer-updates-registry ()
  "The registry learns an archive or a restore from the service's answer, before its event."
  (opencode-test-with-archived
    (let ((id opencode-session-id))
      (opencode-state--store id (opencode-state--make-entry :id id :title "Text" :archived 1.0))
      (opencode-session-archive id t)
      (should-not (plist-get (opencode-state-get id) :archived))
      (should-not (opencode-session-archived-p id))
      (opencode-session-archive id)
      (should (plist-get (opencode-state-get id) :archived))
      (should (opencode-session-archived-p id)))))

(ert-deftest opencode-test-archived-input-keeps-attachments ()
  "Input waiting for its session to leave the archive keeps its attachments, and sends them."
  (let ((opencode-test-restore-answer 'hold))
    (opencode-test-with-archived
      (let* ((file (expand-file-name "README.md" opencode-test-workspace))
             (uri (opencode--file-uri file)))
        (opencode-test-archive-type "read ")
        (opencode--attach-file file)
        (opencode--insert-mention "explore")
        (opencode-send-input)
        (should (equal (opencode-test-archive-kinds) '(patch)))
        (should (= (length opencode--attachments) 2))
        (should (string-prefix-p "read `" (opencode-test-archive-input)))
        (should (opencode-test-release))
        (pcase-let ((`(,_ (prompt ,body)) (opencode-test-archive-traffic)))
          (should (equal (mapcar (lambda (file) (alist-get 'uri file)) (alist-get 'files body)) (list uri)))
          (should (equal (alist-get 'agents body) [((name . "explore"))])))
        (should-not opencode--attachments)
        (should-not (seq-some (lambda (overlay) (overlay-get overlay 'opencode-attachment))
                              (overlays-in (point-min) (point-max))))))))

(ert-deftest opencode-test-archived-session-send-keeps-files ()
  "Files and mentions given to `opencode-session-send' go with the prompt once restored."
  (let ((opencode-test-restore-answer 'hold)
        (files [((uri . "file:///tmp/a.txt") (name . "a.txt"))])
        (agents [((name . "explore"))]))
    (opencode-test-with-archived
      (opencode-session-send "with files" :show t :files files :agents agents)
      (should (opencode-test-release))
      (pcase-let ((`((patch ,_) (prompt ,body)) (opencode-test-archive-traffic)))
        (should (equal (alist-get 'files body) files))
        (should (equal (alist-get 'agents body) agents)))
      ;; And at once from a session not archived.
      (setq opencode-test-requests nil)
      (opencode-session-send "again" :show t :files files :agents agents)
      (pcase-let ((`((prompt ,body)) (opencode-test-archive-traffic)))
        (should (equal (alist-get 'files body) files))
        (should (equal (alist-get 'agents body) agents))))))

(provide 'opencode-archive-test)
;;; opencode-archive-test.el ends here
