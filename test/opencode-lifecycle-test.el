;;; opencode-lifecycle-test.el --- Tests of the service's lifecycle and request cleanup  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Whatever stops the service, a request starts it again and goes
;; through, and the event stream comes back as soon as the service does,
;; so that replies render; every request's buffer is killed once it is
;; over.  These tests talk to `opencode-test-with-service' over sockets,
;; with the CLI stood for by `opencode-test-with-cli'.

;;; Code:

(require 'opencode-test-server)

(defun opencode-test-agent-route ()
  "Return a route answering the agent catalog."
  (cons "/api/agent" (lambda (_) '(200 . ((data . []))))))

(defun opencode-test-password-of (request)
  "Return the password REQUEST authenticated with."
  (let ((header (cdr (assoc "authorization" (plist-get request :headers)))))
    (cadr (split-string (base64-decode-string (string-remove-prefix "Basic " header)) ":"))))

(defun opencode-test-count-connections ()
  "Count every `server.connected' event; return a function reading the count."
  (let ((count 0))
    (opencode-events-register "server.connected" (lambda (_) (cl-incf count)))
    (lambda () count)))

;;;; Request buffers

(ert-deftest opencode-test-request-buffers-killed ()
  "Every request's buffer is killed once it is over, and its callbacks run once.
That holds for answers, errors, unreadable answers, refused
connections, timeouts and connections deleted from outside; only the
few connections kept for later requests keep theirs."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (opencode-test-agent-route)
                (cons "/api/model" (lambda (_) '(200 . "{not json")))
                (cons "/api/session/active" (lambda (_) '(hang)))))
    (let ((before (length (opencode-test-request-buffers)))
          (outcomes nil))
      (cl-flet ((send (path &rest arguments)
                  (apply #'opencode-request 'get path :buffer nil
                         :then (lambda (_) (push (cons path 'then) outcomes))
                         :else (lambda (error) (push (list path 'else (opencode-error-message error)) outcomes))
                         arguments))
                (outcome (path) (seq-filter (lambda (entry) (equal (car entry) path)) outcomes)))
        (opencode-request-sync 'get "/api/agent" :timeout 10)
        (send "/api/agent")
        (send "/api/session/ses_missing")
        (send "/api/model")
        (send "/api/session/active" :timeout 1)
        (opencode-test-wait-for (lambda () (= (length outcomes) 4)) 15)
        (should (equal (outcome "/api/agent") '(("/api/agent" . then))))
        (should (equal (mapcar #'cadr (outcome "/api/session/ses_missing")) '(else)))
        (should (string-match-p "unreadable response" (nth 2 (car (outcome "/api/model")))))
        (should (equal (mapcar #'cadr (outcome "/api/session/active")) '(else)))
        ;; A request whose connection is deleted from outside.
        (send "/api/session/active" :timeout 30)
        (let ((connection (opencode-test-wait-for
                           (lambda ()
                             (and (opencode-test-service-held opencode-test-service)
                                  (= (length (opencode-test-service-held opencode-test-service)) 2)
                                  (seq-find (lambda (process)
                                              (and (process-live-p process)
                                                   (process-get process 'opencode-transport-exchange)))
                                            (process-list)))))))
          (delete-process connection))
        (opencode-test-wait-for (lambda () (= (length (outcome "/api/session/active")) 2)))
        (should (string-match-p "without an answer" (nth 2 (car (outcome "/api/session/active")))))
        ;; A refused connection is retried once on the registered service.
        (setq opencode-service-connection
              (plist-put (copy-sequence opencode-service-connection) :url "http://127.0.0.1:1"))
        (send "/api/agent")
        (opencode-test-wait-for (lambda () (= (length (outcome "/api/agent")) 2)))
        (should (equal (outcome "/api/agent") '(("/api/agent" . then) ("/api/agent" . then))))
        (accept-process-output nil 0.2)
        (should (= (length outcomes) 6))
        (opencode-test-wait-for (lambda () (= (length (opencode-test-request-buffers)) before)))
        (should (<= (opencode-transport-idle-count) opencode-transport-idle-connections))))))

(ert-deftest opencode-test-stream-buffers-killed ()
  "The event stream's buffers are killed however each stream ends."
  (opencode-test-with-service
    (let ((before (length (opencode-test-request-buffers)))
          (connections (opencode-test-count-connections)))
      (opencode-events-connect)
      (opencode-test-wait-for (lambda () (= (funcall connections) 1)))
      (setq opencode-events--backoff 0.1)
      ;; Closed by the service.
      (opencode-test-close-streams)
      (opencode-test-wait-for (lambda () (= (funcall connections) 2)))
      ;; Silent past the heartbeat timeout.
      (setq opencode-events--last-output 0
            opencode-events--backoff 0.1)
      (opencode-events--watch)
      (opencode-test-wait-for (lambda () (= (funcall connections) 3)))
      ;; The open stream's, for its connection and its frames.
      (opencode-test-wait-for (lambda () (= (length (opencode-test-request-buffers)) (+ 2 before))))
      (opencode-disconnect)
      (opencode-test-wait-for (lambda () (= (length (opencode-test-request-buffers)) before))))))

;;;; Event stream

(ert-deftest opencode-test-stream-killed-reconnects ()
  "A stream whose connection is deleted or whose buffer is killed from outside reopens."
  (opencode-test-with-service
    (let ((connections (opencode-test-count-connections)))
      (opencode-events-connect)
      (opencode-test-wait-for (lambda () (= (funcall connections) 1)))
      (setq opencode-events--backoff 0.1)
      (delete-process (opencode-transport-exchange-connection opencode-events--stream))
      (opencode-test-wait-for (lambda () (= (funcall connections) 2)))
      (setq opencode-events--backoff 0.1)
      ;; Killing its buffer deletes the connection too.
      (kill-buffer (process-buffer (opencode-transport-exchange-connection opencode-events--stream)))
      (opencode-test-wait-for (lambda () (= (funcall connections) 3)))
      (should (opencode-events-connected-p)))))

(ert-deftest opencode-test-stream-lost-revived ()
  "A wanted stream left closed with nothing reopening it is reopened.
The watchdog does it at its next check; reopening it, as a sent prompt
does, at once."
  (opencode-test-with-service
    (let ((connections (opencode-test-count-connections)))
      (opencode-events-connect)
      (opencode-test-wait-for (lambda () (= (funcall connections) 1)))
      (opencode-test-lose-streams)
      (setq opencode-events--backoff 0.1)
      (opencode-events--watch)
      (opencode-test-wait-for (lambda () (= (funcall connections) 2)))
      (opencode-test-lose-streams)
      (setq opencode-events--backoff 30)
      (opencode-events-reopen)
      (opencode-test-wait-for (lambda () (= (funcall connections) 3)) 5))))

(ert-deftest opencode-test-disconnect-stays-closed-whatever-asks ()
  "After a disconnect, neither the watchdog, a send nor a ready service reopens the stream."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service) (list (opencode-test-agent-route)))
    (let ((opencode-session-buffers (make-hash-table :test #'equal))
          (buffer (opencode-test-session-stand-in "ses_test"))
          (opencode-state--wanted t))
      (unwind-protect
          (progn
            (puthash "ses_test" buffer opencode-session-buffers)
            (opencode-events-connect)
            (opencode-test-wait-for #'opencode-events-connected-p)
            (opencode-disconnect)
            (opencode-events--watch)
            (opencode-events-reopen)
            (opencode-test-with-cli
                (lambda (_arguments exit)
                  (opencode-test-restart-service "second-password")
                  (funcall exit 0))
              (let ((restarted nil))
                (opencode-service-restart (lambda (_) (setq restarted t)))
                (opencode-test-wait-for (lambda () restarted))))
            (opencode-request-sync 'get "/api/agent" :timeout 10)
            (accept-process-output nil 0.5)
            (should-not (opencode-events-connected-p)))
        (kill-buffer buffer)))))

(defmacro opencode-test-with-clock (offset &rest body)
  "Run BODY with `float-time' running OFFSET seconds ahead of the real clock.
OFFSET is a variable BODY may change, as a machine waking from sleep
finds the clock moved on."
  (declare (indent 1) (debug t))
  (let ((real (make-symbol "real")))
    `(let ((,real (symbol-function 'float-time)))
       (cl-letf (((symbol-function 'float-time)
                  (lambda (&optional time) (if time (funcall ,real time) (+ (funcall ,real) ,offset)))))
         ,@body))))

(defmacro opencode-test-with-watched-stream (bindings &rest body)
  "Run BODY watching a stream that stands for an open one, at a clock BODY sets.
BINDINGS is a list (NOW DROPS): NOW is the variable holding what
`float-time' returns, and DROPS the variable counting the times the
watchdog dropped the stream and asked for a reconnection."
  (declare (indent 1) (debug t))
  (pcase-let ((`(,now ,drops) bindings))
    `(let ((,now 1000.0)
           (,drops 0)
           (opencode-events-heartbeat-timeout 45)
           (opencode-events--wanted t)
           (opencode-events--opening nil)
           (opencode-events--reconnect-timer nil)
           (opencode-events--last-output 1000.0)
           (opencode-events--last-watch 1000.0)
           (opencode-events--resumed 0))
       (cl-letf (((symbol-function 'float-time) (lambda (&optional _) ,now))
                 ((symbol-function 'opencode-events-connected-p) (lambda () t))
                 ((symbol-function 'opencode-events--drop) (lambda () (cl-incf ,drops)))
                 ((symbol-function 'opencode-events--schedule-reconnect) #'ignore))
         ,@body))))

(ert-deftest opencode-test-watchdog-drops-silent-stream ()
  "A stream silent past the heartbeat timeout while the watchdog runs on time is dropped."
  (opencode-test-with-watched-stream (now drops)
    (dolist (tick '(15 30 45))
      (setq now (+ 1000.0 tick))
      (opencode-events--watch))
    (should (= drops 0))
    (setq now 1060.0)
    (opencode-events--watch)
    (should (= drops 1))))

(ert-deftest opencode-test-watchdog-tolerates-sleep ()
  "A stream silent while the machine slept gets a heartbeat timeout from the wake to show it lives."
  (opencode-test-with-watched-stream (now drops)
    (setq now 1010.0 opencode-events--last-output 1005.0)
    (opencode-events--watch)
    ;; An hour asleep: the first check is late, and the timer catches up
    ;; with checks in a row.
    (setq now 4610.0)
    (dotimes (_ 3) (opencode-events--watch))
    (should (= drops 0))
    (should (= opencode-events--resumed 4610.0))
    ;; A heartbeat after the wake keeps it, as before the sleep.
    (setq now 4620.0 opencode-events--last-output 4618.0)
    (opencode-events--watch)
    (dolist (tick '(4635 4650))
      (setq now (float tick))
      (opencode-events--watch))
    (should (= drops 0))
    (setq now 4665.0)
    (opencode-events--watch)
    (should (= drops 1))))

(ert-deftest opencode-test-watchdog-dead-after-sleep ()
  "A stream that died during the sleep is dropped a heartbeat timeout after the wake."
  (opencode-test-with-watched-stream (now drops)
    (setq now 1100.0)
    (opencode-events--watch)
    (should (= drops 0))
    (dolist (tick '(1115 1130 1145))
      (setq now (float tick))
      (opencode-events--watch))
    (should (= drops 0))
    (setq now 1160.0)
    (opencode-events--watch)
    (should (= drops 1))))

(ert-deftest opencode-test-watchdog-clock-back ()
  "A clock set back neither drops the stream nor hides that it died."
  (opencode-test-with-watched-stream (now drops)
    (setq now 1010.0 opencode-events--last-output 1010.0)
    (opencode-events--watch)
    (setq now 500.0)
    (opencode-events--watch)
    (should (= drops 0))
    (dolist (tick '(515 530 545))
      (setq now (float tick))
      (opencode-events--watch))
    (should (= drops 0))
    (setq now 560.0)
    (opencode-events--watch)
    (should (= drops 1))))

(ert-deftest opencode-test-stream-survives-sleep ()
  "An open stream is kept through a sleep, and dropped once it stays silent after the wake."
  (opencode-test-with-service
    (let ((connections (opencode-test-count-connections))
          (offset 0))
      (opencode-events-connect)
      (opencode-test-wait-for (lambda () (= (funcall connections) 1)))
      (opencode-test-with-clock offset
        (setq offset 3600)
        (opencode-events--watch)
        (opencode-test-send-raw ": heartbeat\n\n")
        (opencode-test-wait-for (lambda () (> opencode-events--last-output (+ (float-time) -5))))
        (setq offset 3615)
        (opencode-events--watch)
        (accept-process-output nil 0.2)
        (should (= (funcall connections) 1))
        (should (opencode-events-connected-p))
        ;; Silent from then on, though Emacs reads it: it is dropped.
        (setq opencode-events--backoff 0.1)
        (dolist (seconds '(3630 3645 3660 3675))
          (setq offset seconds)
          (opencode-events--watch))
        (opencode-test-wait-for (lambda () (= (funcall connections) 2)))))))

;;;; Restarts

(ert-deftest opencode-test-restart-reopens-stream-at-once ()
  "After a restart, the stream reopens at once, not after its backoff.
Requests made during the restart wait for the restarted service, rather
than find the one going away or start another beside the CLI."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service) (list (opencode-test-agent-route)))
    (let ((connections (opencode-test-count-connections))
          (cli nil)
          (answer nil)
          (restarted nil))
      (opencode-events-connect)
      (opencode-test-wait-for (lambda () (= (funcall connections) 1)))
      (opencode-test-with-cli
          (lambda (arguments exit)
            (push arguments cli)
            (pcase arguments
              ('("service" "restart")
               ;; The old service goes away, and the new one registers later.
               (opencode-test-close-streams)
               (delete-file (opencode-test-service-registration opencode-test-service))
               (run-at-time 0.3 nil #'opencode-test-restart-service "second-password")
               (run-at-time 0.6 nil exit 0))
              (_ (opencode-test-service-register opencode-test-service)
                 (funcall exit 0))))
        ;; A reconnection after this backoff would come too late.
        (setq opencode-events--backoff 30)
        (opencode-service-restart (lambda (_) (setq restarted (float-time))))
        (opencode-request 'get "/api/agent" :buffer nil :then (lambda (_) (setq answer t)))
        (opencode-test-wait-for (lambda () (and answer restarted)) 10)
        (opencode-test-wait-for (lambda () (= (funcall connections) 2)) 3)
        (should (< (- (float-time) restarted) 3))
        (should (equal cli '(("service" "restart"))))
        (should (equal (mapcar #'opencode-test-password-of (opencode-test-requests-on "/api/agent"))
                       '("second-password")))
        (should (equal (plist-get opencode-service-connection :password) "second-password"))
        (should (equal (plist-get opencode-events--connection :password) "second-password"))))))

(ert-deftest opencode-test-restart-reopens-lost-stream ()
  "A restart reopens a stream that was closed with nothing reopening it."
  (opencode-test-with-service
    (let ((connections (opencode-test-count-connections))
          (restarted nil))
      (opencode-events-connect)
      (opencode-test-wait-for (lambda () (= (funcall connections) 1)))
      (opencode-test-lose-streams)
      (opencode-test-with-cli
          (lambda (_arguments exit)
            (opencode-test-restart-service "second-password")
            (run-at-time 0.1 nil exit 0))
        (opencode-service-restart (lambda (_) (setq restarted t)))
        (opencode-test-wait-for (lambda () restarted))
        (opencode-test-wait-for (lambda () (= (funcall connections) 2)) 3)))))

(ert-deftest opencode-test-restart-failure-serves-waiting-requests ()
  "A failed restart fails its caller alone; requests waiting for it find the service then."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service) (list (opencode-test-agent-route)))
    (let ((settled nil))
      (opencode-test-with-cli
          (lambda (_arguments exit) (run-at-time 0.2 nil exit 1 "service is locked"))
        (opencode-service-restart #'ignore (lambda (text) (push text settled)))
        (opencode-request 'get "/api/agent" :buffer nil :then (lambda (_) (push 'answer settled)))
        (opencode-test-wait-for (lambda () (= (length settled) 2)))
        (should (equal settled '(answer "opencode service restart failed: service is locked")))))))

;;;; Stops

(ert-deftest opencode-test-stop-then-request-starts-service ()
  "Requests made while the service stops start it once the stop ends.
The stream then reopens for the session registry's clients."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service) (list (opencode-test-agent-route)))
    (let ((connections (opencode-test-count-connections))
          (opencode-state--wanted t)
          (cli nil)
          (stopped nil)
          (answer nil))
      (opencode-events-connect)
      (opencode-test-wait-for (lambda () (= (funcall connections) 1)))
      (opencode-test-with-cli
          (lambda (arguments exit)
            (push (cons arguments stopped) cli)
            (pcase arguments
              ('("service" "stop")
               (delete-file (opencode-test-service-registration opencode-test-service))
               (run-at-time 0.3 nil exit 0))
              (_ (opencode-test-restart-service "second-password")
                 (funcall exit 0))))
        (opencode-service-stop (lambda () (setq stopped t)))
        (should-not (opencode-events-connected-p))
        (opencode-request 'get "/api/agent" :buffer nil :then (lambda (_) (setq answer t)))
        (opencode-test-wait-for (lambda () answer))
        ;; The start came once the stop had ended.
        (should (equal (mapcar #'car (reverse cli)) '(("service" "stop") ("service" "start"))))
        (should (cdr (car cli)))
        (opencode-test-wait-for (lambda () (= (funcall connections) 2)) 3)))))

;;;; Sending

(defconst opencode-test-lifecycle-session "ses_lifecycle"
  "ID of the session the sending tests open.")

(defun opencode-test-lifecycle-routes ()
  "Return the routes serving one empty session and what opening it asks."
  (let ((info `((id . ,opencode-test-lifecycle-session) (title . "Lifecycle") (agent . "build")
                (time (created . 1792531215759) (updated . 1792531215759))
                (location (directory . ,(directory-file-name opencode-test-workspace))))))
    (append
     (list (cons (concat "/api/session/" opencode-test-lifecycle-session)
                 (lambda (_) `(200 . ((data . ,info)))))
           (cons (concat "/api/session/" opencode-test-lifecycle-session "/message")
                 (lambda (_) '(200 . ((data . []) (cursor)))))
           (cons (concat "/api/session/" opencode-test-lifecycle-session "/prompt")
                 (lambda (request)
                   (cons 200 `((data (id . ,(alist-get 'id (json-parse-string
                                                             (plist-get request :body)
                                                             :object-type 'alist))))))))
           (cons "/api/session" (lambda (_) '(200 . ((data . [])))))
           (cons "/api/session/active" (lambda (_) '(200 . ((data . nil))))))
     (mapcar (lambda (path) (cons path (lambda (_) '(200 . ((data . []))))))
             (list "/api/agent" "/api/model" "/api/provider" "/api/command"
                   (concat "/api/session/" opencode-test-lifecycle-session "/permission")
                   (concat "/api/session/" opencode-test-lifecycle-session "/form"))))))

(defun opencode-test-reply (text)
  "Send a turn of the open session answering TEXT on the stream."
  (let* ((session opencode-test-lifecycle-session)
         (base `((sessionID . ,session) (assistantMessageID . "msg_lifecycle_reply") (ordinal . 0))))
    (dolist (event `(((type . "session.execution.started") (data (sessionID . ,session)))
                     ((type . "session.step.started")
                      (data (sessionID . ,session) (assistantMessageID . "msg_lifecycle_reply")))
                     ((type . "session.text.started") (data ,@base))
                     ((type . "session.text.delta") (data ,@base (delta . ,text)))
                     ((type . "session.text.ended") (data ,@base (text . ,text)))
                     ((type . "session.execution.succeeded") (data (sessionID . ,session)))))
      (opencode-test-send-event event))))

(defmacro opencode-test-with-lifecycle-session (buffer &rest body)
  "Run BODY with BUFFER bound to the open session's buffer, its stream connected."
  (declare (indent 1) (debug t))
  `(opencode-test-with-service
     (setf (opencode-test-service-routes opencode-test-service) (opencode-test-lifecycle-routes))
     (opencode-test-with-fresh-catalogs
       (let ((opencode-session-buffers (make-hash-table :test #'equal))
             (opencode-send-checks nil)
             (opencode-before-send-functions nil)
             (opencode-last-session-buffer nil)
             (opencode-notify-os-function #'ignore)
             (,buffer nil))
         (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) (ert-fail "Unexpected display"))))
           (unwind-protect
               (progn
                 (opencode-session-open-by-id opencode-test-lifecycle-session :no-display t
                                              :callback (lambda (_) (setq ,buffer (current-buffer))))
                 (opencode-test-wait-for (lambda () ,buffer))
                 (opencode-test-wait-for #'opencode-events-connected-p)
                 ,@body)
             (when (buffer-live-p ,buffer) (kill-buffer ,buffer))))))))

(defun opencode-test-type-prompt (buffer text)
  "Type TEXT at the input of BUFFER and send it as RET does."
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert text)
    (opencode-send-input)))

(defun opencode-test-buffer-shows-p (buffer text)
  "Return non-nil when BUFFER shows TEXT."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward text nil t))))

(ert-deftest opencode-test-reply-renders-after-restart ()
  "A prompt typed after a restart reaches the restarted service, and its reply renders.
The stream had ended before the restart without being reopened, as when
its connection died unnoticed."
  (opencode-test-with-lifecycle-session buffer
    (opencode-test-lose-streams)
    (opencode-test-with-cli
        (lambda (_arguments exit)
          (opencode-test-restart-service "second-password")
          (run-at-time 0.1 nil exit 0))
      (let ((restarted nil))
        (opencode-service-restart (lambda (_) (setq restarted t)))
        (opencode-test-wait-for (lambda () restarted))))
    (opencode-test-type-prompt buffer "Hello again")
    (opencode-test-wait-for (lambda () (opencode-test-requests-on
                                        (concat "/api/session/" opencode-test-lifecycle-session "/prompt"))))
    (should (equal (opencode-test-password-of
                    (car (opencode-test-requests-on
                          (concat "/api/session/" opencode-test-lifecycle-session "/prompt"))))
                   "second-password"))
    (opencode-test-wait-for (lambda () (opencode-test-service-streams opencode-test-service)) 3)
    (opencode-test-reply "The answer after the restart.")
    (opencode-test-wait-for (lambda () (opencode-test-buffer-shows-p buffer "The answer after the restart.")))))

(ert-deftest opencode-test-send-starts-stopped-service ()
  "A prompt sent while the service is gone starts it, goes through, and its reply renders.
The stream, waiting for its long backoff, reopens as soon as the service is back."
  (opencode-test-with-lifecycle-session buffer
    (let ((cli nil))
      ;; The stream retries late, as after failed attempts while the service was gone.
      (setq opencode-events--backoff 30)
      (opencode-test-close-streams)
      (opencode-test-wait-for (lambda () (timerp opencode-events--reconnect-timer)))
      ;; The service went away; the connection Emacs knows is refused now.
      (delete-file (opencode-test-service-registration opencode-test-service))
      (setq opencode-service-connection
            (plist-put (copy-sequence opencode-service-connection) :url "http://127.0.0.1:1"))
      (opencode-test-with-cli
          (lambda (arguments exit)
            (push arguments cli)
            (opencode-test-restart-service "second-password")
            (funcall exit 0))
        (with-current-buffer buffer
          (opencode-session-send "Are you there?" :show t))
        (opencode-test-wait-for (lambda () (opencode-test-requests-on
                                            (concat "/api/session/" opencode-test-lifecycle-session "/prompt"))))
        (should (equal cli '(("service" "start"))))
        (opencode-test-wait-for (lambda () (opencode-test-service-streams opencode-test-service)) 3)
        (opencode-test-reply "Back again.")
        (opencode-test-wait-for (lambda () (opencode-test-buffer-shows-p buffer "Back again.")))))))

(ert-deftest opencode-test-send-during-stop-reopens-stream ()
  "A prompt typed while the service stops starts it once stopped, and its reply renders."
  (opencode-test-with-lifecycle-session buffer
    (let ((cli nil)
          (stopped nil))
      (opencode-test-with-cli
          (lambda (arguments exit)
            (push (cons arguments stopped) cli)
            (if (equal arguments '("service" "stop"))
                (progn (opencode-test-close-streams)
                       (delete-file (opencode-test-service-registration opencode-test-service))
                       (run-at-time 0.3 nil exit 0))
              (opencode-test-restart-service "second-password")
              (funcall exit 0)))
        (opencode-service-stop (lambda () (setq stopped t)))
        (should-not (opencode-events-connected-p))
        (opencode-test-type-prompt buffer "Wake up")
        (opencode-test-wait-for (lambda () (opencode-test-requests-on
                                            (concat "/api/session/" opencode-test-lifecycle-session "/prompt"))))
        (should (equal (reverse cli) '((("service" "stop")) (("service" "start") . t))))
        (opencode-test-wait-for (lambda () (opencode-test-service-streams opencode-test-service)) 3)
        (opencode-test-reply "Awake.")
        (opencode-test-wait-for (lambda () (opencode-test-buffer-shows-p buffer "Awake.")))))))

(provide 'opencode-lifecycle-test)
;;; opencode-lifecycle-test.el ends here
