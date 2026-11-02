;;; opencode-test-helpers.el --- Shared helpers of the opencode.el tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Fixtures in fixtures/v2 were recorded from an OpenCode 2 service
;; driving scripted sessions in a scratch repository; their paths read
;; @WORKSPACE@ (that repository), @TRIAL@ and @HOME@.  Tests never reach
;; a real service: `opencode-test-with-server' answers requests from
;; route handlers, synchronously, and records every request made.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'opencode)

(defvar opencode-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the test files.")

(defvar opencode-test-workspace
  (file-name-as-directory (make-temp-file "opencode-workspace-" t))
  "Scratch directory standing for the recorded @WORKSPACE@.")

(dolist (name '("README.md" "notes.txt" "hello.txt" "AGENTS.md"))
  (with-temp-file (expand-file-name name opencode-test-workspace)
    (insert name "\n")))

(defun opencode-test-lowercase-drive (directory)
  "Return DIRECTORY with a lowercase drive letter, as Emacs spells it on Windows."
  (concat (downcase (substring directory 0 1)) (substring directory 1)))

(defun opencode-test-capital-drive-p (directory)
  "Return non-nil when DIRECTORY starts with a capital drive letter.
The letter may be followed by a colon or, URI-encoded, by %3A."
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z]\\(?::/\\|%3A%2F\\)" directory)))

(defun opencode-test-substitute (text)
  "Replace the fixture placeholders in TEXT."
  (let ((workspace (directory-file-name opencode-test-workspace)))
    (replace-regexp-in-string
     "@\\(WORKSPACE\\|TRIAL\\|HOME\\)@"
     (lambda (match)
       (pcase match
         ("@WORKSPACE@" (string-replace "\\" "\\\\" workspace))
         ("@TRIAL@" (string-replace "\\" "\\\\" (directory-file-name temporary-file-directory)))
         (_ (string-replace "\\" "\\\\" (directory-file-name (expand-file-name "~"))))))
     text t t)))

(defun opencode-test-parse (text)
  "Parse JSON TEXT as `opencode-request' decodes responses."
  (json-parse-string (opencode-test-substitute text)
                     :object-type 'alist :array-type 'list :null-object nil :false-object nil))

(defun opencode-test-fixture (name)
  "Return the decoded JSON fixture NAME."
  (opencode-test-parse
   (with-temp-buffer
     (insert-file-contents (expand-file-name (concat "fixtures/v2/" name) opencode-test-directory))
     (buffer-string))))

(defun opencode-test-events (&optional session-id)
  "Return the recorded events, only those of SESSION-ID when given."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "fixtures/v2/events.jsonl" opencode-test-directory))
    (let (events)
      (dolist (line (split-string (buffer-string) "\n" t))
        (let* ((event (opencode-test-parse line))
               (data (alist-get 'data event))
               (owner (or (alist-get 'sessionID data)
                          (alist-get 'sessionID (alist-get 'form data)))))
          (when (or (null session-id) (equal owner session-id))
            (push event events))))
      (nreverse events))))

(defconst opencode-test-scenarios
  '("text" "read-tools" "edit-tools" "shell" "permission" "question" "subagent"
    "interrupt" "switch" "steer" "history" "shell-input" "reasoning")
  "Recorded scenarios whose sessions can be opened.")

(defun opencode-test-scenario (name)
  "Return the session ID recorded for scenario NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "fixtures/v2/scenarios.log" opencode-test-directory))
    (goto-char (point-min))
    (when (re-search-forward (format "\t%s\t\\(ses_[A-Za-z0-9]+\\)" (regexp-quote name)) nil t)
      (match-string 1))))

(defun opencode-test-first-reasoning (session-id)
  "Return the ID of the first recorded assistant message of SESSION-ID with reasoning."
  (alist-get 'id (seq-find (lambda (message)
                             (and (equal (alist-get 'type message) "assistant")
                                  (seq-some (lambda (item) (equal (alist-get 'type item) "reasoning"))
                                            (alist-get 'content message))))
                           (alist-get 'data (opencode-test-fixture (format "messages-%s.json" session-id))))))

(defun opencode-test-session-title (session-id)
  "Return the recorded title of SESSION-ID."
  (alist-get 'title (alist-get 'data (opencode-test-fixture (format "session-%s.json" session-id)))))

(defvar opencode-test-requests nil
  "Requests made under `opencode-test-with-server', newest first.
Each is a plist (:method :path :query :body).")

(defvar opencode-test-routes nil
  "Route handlers of `opencode-test-with-server'.
Each is (METHOD REGEXP FUNCTION); FUNCTION receives the request plist
and returns the decoded response, or (:error STATUS MESSAGE [TAG]).")

(defun opencode-test-requests-to (method suffix)
  "Return the bodies of the METHOD requests whose path ends in SUFFIX, oldest first."
  (nreverse (delq nil (mapcar (lambda (request)
                                (when (and (eq (plist-get request :method) method)
                                           (string-suffix-p suffix (plist-get request :path)))
                                  (or (plist-get request :body) t)))
                              opencode-test-requests))))

(defun opencode-test-cursor-with-order-p (request)
  "Return non-nil when REQUEST's query has both a cursor and an order.
The service refuses message pages asked for so."
  (let ((query (plist-get request :query)))
    (and (alist-get 'cursor query) (assq 'order query))))

(defun opencode-test-default-routes ()
  "Return routes serving the recorded sessions and catalogs."
  (list
   (list 'get "\\`/api/session/\\(ses_[A-Za-z0-9]+\\)/message\\'"
         (lambda (request)
           (let ((file (format "messages-%s.json" (plist-get request :match))))
             (cond
              ;; As the service does: a cursor carries its order.
              ((opencode-test-cursor-with-order-p request)
               '(:error 400 "Cursor cannot be combined with order"))
              ((or (plist-get request :cursor-seen)
                   (not (file-exists-p (expand-file-name (concat "fixtures/v2/" file)
                                                         opencode-test-directory))))
               '((data) (cursor)))
              (t (opencode-test-fixture file))))))
   (list 'get "\\`/api/session/\\(ses_[A-Za-z0-9]+\\)\\'"
         (lambda (request)
           (let* ((file (format "session-%s.json" (plist-get request :match)))
                  (fixture (and (file-exists-p (expand-file-name (concat "fixtures/v2/" file)
                                                                 opencode-test-directory))
                                (opencode-test-fixture file))))
             ;; A recorded error body, such as a deleted session's, was a 404.
             (if (and fixture (not (alist-get '_tag fixture)))
                 fixture
               `(:error 404 ,(format "Session not found: %s" (plist-get request :match)))))))
   (list 'get "\\`/api/session/active\\'" (lambda (_) '((data))))
   (list 'get "\\`/api/agent\\'" (lambda (_) (opencode-test-fixture "agent.json")))
   (list 'get "\\`/api/model\\'" (lambda (_) (opencode-test-fixture "model.json")))
   (list 'get "\\`/api/provider\\'" (lambda (_) (opencode-test-fixture "provider.json")))
   (list 'get "\\`/api/command\\'" (lambda (_) (opencode-test-fixture "command.json")))
   (list t "" (lambda (_) '((data))))))

(defvar opencode-test-deferred nil
  "Answers held back under `opencode-test-with-server', oldest first.
A route defers its answer by returning (:defer . FUNCTION): FUNCTION
returns the answer once `opencode-test-release' releases it.")

(defun opencode-test--request (method path &rest arguments)
  "Answer METHOD on PATH with ARGUMENTS from `opencode-test-routes'."
  (let* ((query (plist-get arguments :query))
         (request (list :method method :path path :query query :body (plist-get arguments :body)
                        :directory (plist-get arguments :directory)))
         (route (seq-find (lambda (route)
                            (and (or (eq (car route) t) (eq (car route) method))
                                 (string-match (cadr route) path)))
                          opencode-test-routes))
         (buffer (if (plist-member arguments :buffer) (plist-get arguments :buffer) (current-buffer))))
    (push request opencode-test-requests)
    (when route
      (string-match (cadr route) path)
      (let* ((request (append request (list :match (match-string 1 path)
                                             :cursor-seen (alist-get 'cursor query))))
             (response (funcall (caddr route) request))
             (then (or (plist-get arguments :then) #'ignore))
             (else (plist-get arguments :else)))
        (cl-labels ((deliver (function value)
                      (if (null buffer)
                          (funcall function value)
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer (funcall function value)))))
                    (answer (response)
                      (if (eq (car-safe response) :error)
                          (let ((error (opencode-error--make :status (nth 1 response) :message (nth 2 response)
                                                             :tag (nth 3 response)
                                                             :method method :path path)))
                            (if else (deliver else error) (error "%s" (opencode-error-describe error))))
                        (deliver then response))))
          (if (eq (car-safe response) :defer)
              (setq opencode-test-deferred
                    (append opencode-test-deferred
                            (list (lambda () (answer (funcall (cdr response)))))))
            (answer response)))))))

(defvar opencode-test-timers nil
  "Timers held by `opencode-test-with-held-timers', oldest first.")

(defmacro opencode-test-with-held-timers (&rest body)
  "Run BODY with the timers `run-at-time' makes held until `opencode-test-run-timers'.
They are real timer objects, which `timerp' recognizes, never scheduled."
  (declare (indent 0) (debug t))
  `(let ((opencode-test-timers nil))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (_time _repeat function &rest arguments)
                  (let ((timer (timer-create)))
                    (timer-set-function timer function arguments)
                    (setq opencode-test-timers (append opencode-test-timers (list timer)))
                    timer))))
       ,@body)))

(defun opencode-test-run-timers ()
  "Run the held timers, with those they make, oldest first; return how many ran."
  (let ((count 0))
    (while opencode-test-timers
      (let ((timer (pop opencode-test-timers)))
        (cl-incf count)
        (apply (timer--function timer) (timer--args timer))))
    count))

(defun opencode-test-release ()
  "Deliver the oldest answer held back, returning non-nil when there was one."
  (when-let* ((answer (pop opencode-test-deferred)))
    (funcall answer)
    t))

(defvar opencode-test-notifications nil
  "Notifications shown under `opencode-test-with-server', newest first.")

(defvar opencode-test-dismissed-notifications nil
  "Notifications dismissed under `opencode-test-with-server', newest first.")

(defmacro opencode-test-with-state (&rest body)
  "Run BODY with an empty session registry of its own, never bootstrapped."
  (declare (indent 0) (debug t))
  `(let ((opencode-state--sessions (make-hash-table :test #'equal))
         (opencode-state--stream 'none)
         (opencode-events--greeted nil)
         (opencode-state--ready nil)
         (opencode-state--bootstrapping nil)
         (opencode-state--generation 0)
         (opencode-state--held nil)
         (opencode-state--fetching (make-hash-table :test #'equal))
         (opencode-state--current (make-hash-table :test #'equal))
         (opencode-state--listing nil)
         (opencode-state--loaded nil)
         (opencode-state--list-timer nil)
         (opencode-state--list-delay 1)
         (opencode-state--batch nil)
         (opencode-state--waiters nil)
         (opencode-state--wanted nil)
         (opencode-state--retry-timer nil)
         (opencode-state--retry-delay 1)
         (opencode-state--viewing (make-hash-table :test #'equal))
         (opencode-state--contexts (make-hash-table :test #'equal))
         (opencode-state--compacting (make-hash-table :test #'equal))
         (opencode-state--context-queue nil)
         (opencode-state--context-timer nil)
         (opencode-state--caches (make-hash-table :test #'equal))
         (opencode-state--cache-timer nil)
         (opencode-state--cache-generation 0)
         (opencode-state--cache-listing nil)
         (opencode-state--cache-held nil)
         (opencode-state--cache-unsupported nil))
     (unwind-protect (progn ,@body)
       (dolist (timer (list opencode-state--retry-timer opencode-state--cache-timer
                            opencode-state--context-timer opencode-state--list-timer))
         (when (timerp timer)
           (cancel-timer timer))))))

(defmacro opencode-test-with-fresh-catalogs (&rest body)
  "Run BODY with no catalog cached, fetched or queued, and no mode line to update."
  (declare (indent 0) (debug t))
  `(let ((opencode--catalogs (make-hash-table :test #'equal))
         (opencode--catalog-stale (make-hash-table :test #'equal))
         (opencode--catalog-waiters (make-hash-table :test #'equal))
         (opencode--catalog-scheduled (make-hash-table :test #'equal))
         (opencode--catalog-queue nil)
         (opencode--catalog-timer nil)
         (opencode--catalog-failed (make-hash-table :test #'equal))
         (opencode--catalog-refetch (make-hash-table :test #'equal))
         (opencode--catalog-epoch 0)
         (opencode--catalog-generations (make-hash-table :test #'equal))
         (opencode--missing-locations (make-hash-table :test #'equal))
         (opencode--missing-reported (make-hash-table :test #'equal))
         (opencode--mode-line-directories nil)
         (opencode--mode-line-timer nil))
     (unwind-protect (progn ,@body)
       (dolist (timer (list opencode--catalog-timer opencode--mode-line-timer))
         (when (timerp timer)
           (cancel-timer timer))))))

(defmacro opencode-test-with-server (&rest body)
  "Run BODY with requests answered by `opencode-test-routes' and no display.
Notifications reach `opencode-test-notifications' instead of the OS.  The
session registry starts empty and follows events once a test dispatches
`server.connected'."
  (declare (indent 0) (debug t))
  `(opencode-test-with-state
     (opencode-test-with-fresh-catalogs
       (let ((opencode-test-requests nil)
             (opencode-test-deferred nil)
             (opencode-test-routes (opencode-test-default-routes))
             (opencode-session-buffers (make-hash-table :test #'equal))
             (opencode--projects nil)
             (opencode-last-session-buffer nil)
             (opencode-alerted-sessions nil)
             (opencode-test-notifications nil)
             (opencode-test-dismissed-notifications nil)
             (opencode-notification-messages-filter nil)
             (opencode-notification-os-filter t)
             (opencode-notify-os-function (lambda (notification)
                                            (push notification opencode-test-notifications)
                                            (lambda ()
                                              (push notification opencode-test-dismissed-notifications)))))
         (cl-letf (((symbol-function 'opencode-request) #'opencode-test--request)
                   ((symbol-function 'opencode-events-connected-p) (lambda () t))
                   ((symbol-function 'opencode-events-connect) #'ignore)
                   ((symbol-function 'display-buffer) (lambda (&rest _) (ert-fail "Unexpected display")))
                   ((symbol-function 'pop-to-buffer) (lambda (&rest _) (ert-fail "Unexpected pop-to-buffer")))
                   ((symbol-function 'switch-to-buffer) (lambda (&rest _) (ert-fail "Unexpected switch"))))
           (unwind-protect (progn ,@body)
             (maphash (lambda (_id buffer) (when (buffer-live-p buffer) (kill-buffer buffer)))
                      opencode-session-buffers)))))))

(defun opencode-test-open (session-id)
  "Open recorded SESSION-ID without display and return its buffer."
  (let ((info (opencode-test-fixture (format "session-%s.json" session-id))))
    (opencode-session-open (alist-get 'data info) :no-display t)))

(defun opencode-test-open-empty (session-id)
  "Open a buffer for SESSION-ID with an empty transcript, as a new session."
  (let ((info (alist-get 'data (opencode-test-fixture (format "session-%s.json" session-id))))
        (opencode-test-routes
         (cons (list 'get "/message\\'" (lambda (_) '((data) (cursor)))) opencode-test-routes)))
    (opencode-session-open info :no-display t)))

(defun opencode-test-dispatch (events)
  "Run the handlers of EVENTS in order."
  (dolist (event events)
    (opencode-events--handle event)))

(defun opencode-test-dispatch-until (events type)
  "Dispatch EVENTS up to and including the first of TYPE; return the rest."
  (let ((done nil))
    (while (and events (not done))
      (let ((event (pop events)))
        (opencode-events--handle event)
        (setq done (equal (alist-get 'type event) type))))
    events))

(defun opencode-test-inbox-events (session id text &optional metadata)
  "Return the events admitting, then delivering, prompt ID of SESSION with TEXT and METADATA."
  `(((type . "session.inbox.enqueued")
     (data (sessionID . ,session) (inboxID . ,id)
           (item (type . "user") (delivery . "steer")
                 (payload (text . ,text) ,@(when metadata `((metadata . ,metadata)))))))
    ((type . "session.inbox.delivered") (data (sessionID . ,session) (inboxID . ,id)))))

(defun opencode-test-answer (session message text)
  "Stream TEXT as the answer of assistant MESSAGE in SESSION."
  (let ((base `((sessionID . ,session) (assistantMessageID . ,message) (ordinal . 0))))
    (opencode-test-dispatch
     `(((type . "session.step.started") (data (sessionID . ,session) (assistantMessageID . ,message)))
       ((type . "session.text.started") (data ,@base))
       ((type . "session.text.delta") (data ,@base (delta . ,text)))
       ((type . "session.text.ended") (data ,@base (text . ,text)))))))

(defun opencode-test-turn (session event)
  "Dispatch the execution EVENT of SESSION, such as \"started\"."
  (opencode-test-dispatch `(((type . ,(concat "session.execution." event)) (data (sessionID . ,session))))))

(defun opencode-test-signature (kind)
  "Return a reasoning signature whose decoded header names block KIND, as Anthropic signs it."
  (base64-encode-string (concat (string #x12 #x05) "B" (string (length kind)) kind (string 0) (make-string 45 7)) t))

(defun opencode-test-text (buffer)
  "Return BUFFER's text without properties and trailing whitespace."
  (with-current-buffer buffer
    (string-trim-right (buffer-substring-no-properties (point-min) (point-max)))))

(defun opencode-test-find (text)
  "Return the position of the first TEXT in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (match-beginning 0)))

(provide 'opencode-test-helpers)
;;; opencode-test-helpers.el ends here
