;;; opencode-test-server.el --- A fake OpenCode service for transport tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; `opencode-test-with-service' runs a local HTTP server standing for the
;; OpenCode service, registered in a temporary service.json, so requests
;; go over sockets as they do against the real service.  Routes answer
;; JSON, or a string as it is, or never when they answer `hang', or
;; close the connection without answering when they answer `close';
;; `/api/info' reports the registered process ID, and `/api/event' keeps
;; its connection open for `opencode-test-send-event'.  Connections stay
;; open between requests, as Node's server keeps them, unless the
;; service's KEEP-ALIVE is nil; answers are framed by Content-Length, or
;; in chunks of CHUNKED bytes when that is a number, and are written
;; SPLIT bytes at a time, a timer apart, when that is a number, so that
;; Emacs reads them in pieces.  The CLI is stood for by
;; `opencode-test-with-cli'.  The server listens on IPv4 only: Winsock's
;; IPv6 loopback is unreliable here.

;;; Code:

(require 'opencode-test-helpers)
(require 'url-util)

(cl-defstruct (opencode-test-service (:constructor opencode-test-service--make))
  server port password (pid 4242) registration
  (info-status 200) routes requests streams held
  (keep-alive t) chunked split)

(defvar opencode-test-service nil
  "The fake service of the running test.")

(defun opencode-test-service-url (service)
  "Return SERVICE's base URL."
  (format "http://127.0.0.1:%d" (opencode-test-service-port service)))

(defun opencode-test-service-register (service &optional password)
  "Write SERVICE's registration, with PASSWORD when given."
  (when password (setf (opencode-test-service-password service) password))
  (with-temp-file (opencode-test-service-registration service)
    (insert (json-serialize
             `((id . "svc_test") (version . "2.0.0-test")
               (url . ,(opencode-test-service-url service))
               (pid . ,(opencode-test-service-pid service))
               (password . ,(opencode-test-service-password service)))))))

(defun opencode-test--chunk (bytes)
  "Return BYTES as one chunk of a chunked body."
  (format "%x\r\n%s\r\n" (length bytes) bytes))

(defun opencode-test--write (connection bytes &optional close)
  "Write BYTES on CONNECTION, then close it when CLOSE.
With the service's SPLIT, they go a few at a time, a timer apart."
  (let ((split (and opencode-test-service (opencode-test-service-split opencode-test-service))))
    (if (not split)
        (progn
          (when (process-live-p connection)
            (process-send-string connection bytes))
          (when (and close (process-live-p connection))
            (delete-process connection)))
      (let ((queue (process-get connection 'output-queue)))
        (process-put connection 'output-queue
                     (append queue (list (cons bytes close))))
        (unless queue
          (opencode-test--write-next connection split))))))

(defun opencode-test--write-next (connection split)
  "Write the next SPLIT bytes queued for CONNECTION."
  (when-let* ((entry (car (process-get connection 'output-queue))))
    (let ((bytes (car entry)))
      (when (process-live-p connection)
        (process-send-string connection (substring bytes 0 (min split (length bytes)))))
      (if (> (length bytes) split)
          (setcar entry (substring bytes split))
        (process-put connection 'output-queue (cdr (process-get connection 'output-queue)))
        (when (and (cdr entry) (process-live-p connection))
          (delete-process connection)))
      (when (process-get connection 'output-queue)
        (run-at-time 0.001 nil #'opencode-test--write-next connection split)))))

(defun opencode-test--respond (connection status body &optional content-type close)
  "Answer CONNECTION with STATUS and BODY, keeping it open as the service does.
CLOSE, or a service without keep-alive, closes it after the answer."
  (let* ((payload (encode-coding-string (or body "") 'utf-8))
         (service opencode-test-service)
         (close (or close (not (and service (opencode-test-service-keep-alive service)))))
         (chunked (and service (opencode-test-service-chunked service))))
    (opencode-test--write
     connection
     (concat (format "HTTP/1.1 %d %s\r\n" status (if (< status 400) "OK" "Error"))
             (format "Content-Type: %s\r\n" (or content-type "application/json"))
             (if chunked
                 "Transfer-Encoding: chunked\r\n"
               (format "Content-Length: %d\r\n" (length payload)))
             (if close "Connection: close\r\n" "Connection: keep-alive\r\nKeep-Alive: timeout=5\r\n")
             "\r\n"
             (if chunked
                 (concat (mapconcat #'opencode-test--chunk (seq-partition payload chunked) "")
                         "0\r\n\r\n")
               payload))
     close)))

(defun opencode-test--authorized-p (service headers)
  "Return non-nil when HEADERS carry SERVICE's credentials."
  (equal (cdr (assoc "authorization" headers))
         (concat "Basic " (base64-encode-string
                           (concat "opencode:" (opencode-test-service-password service)) t))))

(defun opencode-test--handle (service connection request)
  "Answer REQUEST, a plist (:method :path :headers :body :connection), on CONNECTION."
  (push request (opencode-test-service-requests service))
  (let ((path (car (split-string (plist-get request :path) "?"))))
    (cond
     ((not (opencode-test--authorized-p service (plist-get request :headers)))
      (opencode-test--respond connection 401 "{\"message\":\"Unauthorized\"}"))
     ((equal path "/api/info")
      (let ((status (opencode-test-service-info-status service)))
        (if (functionp status) (setq status (funcall status)))
        (opencode-test--respond connection status
                                (json-serialize `((pid . ,(opencode-test-service-pid service))
                                                  (version . "2.0.0-test"))))))
     ((equal path "/api/event")
      (process-put connection 'chunked (opencode-test-service-chunked service))
      (opencode-test--write connection
                            (concat "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n"
                                    "Cache-Control: no-cache\r\n"
                                    (if (process-get connection 'chunked)
                                        "Transfer-Encoding: chunked\r\n"
                                      "")
                                    "\r\n"))
      (push connection (opencode-test-service-streams service))
      (opencode-test-send-event '((type . "server.connected") (data)) connection))
     (t
      (let ((route (seq-find (lambda (route) (equal (car route) path))
                             (opencode-test-service-routes service))))
        (if route
            (pcase-let ((`(,status . ,body) (funcall (cdr route) request)))
              (pcase status
                ('hang (push connection (opencode-test-service-held opencode-test-service)))
                ('close (delete-process connection))
                ;; Bytes written as they are, closing the connection.
                ('raw (opencode-test--write connection body t))
                (_ (opencode-test--respond connection status
                                           (if (stringp body) body (json-serialize body))))))
          (opencode-test--respond connection 404 "{\"message\":\"Not found\"}")))))))

(defun opencode-test--filter (service connection output)
  "Collect OUTPUT of CONNECTION and handle each of SERVICE's requests once complete.
A connection kept open carries one request after another."
  (let ((input (concat (or (process-get connection 'input) "") output))
        (done nil))
    (while (not done)
      (setq done t)
      (when-let* ((end (string-search "\r\n\r\n" input)))
        (let* ((lines (split-string (substring input 0 end) "\r\n"))
               (start (split-string (car lines) " "))
               (headers (mapcar (lambda (line)
                                  (when (string-match "\\`\\([^:]+\\): *\\(.*\\)\\'" line)
                                    (cons (downcase (match-string 1 line)) (match-string 2 line))))
                                (cdr lines)))
               (length (string-to-number (or (cdr (assoc "content-length" headers)) "0")))
               (body-start (+ end 4)))
          (when (>= (- (length input) body-start) length)
            (let ((body (substring input body-start (+ body-start length))))
              (setq input (substring input (+ body-start length))
                    done nil)
              (process-put connection 'input input)
              (opencode-test--handle service connection
                                     (list :method (car start) :path (cadr start) :headers headers
                                           :body (decode-coding-string body 'utf-8)
                                           :connection connection)))))))
    (process-put connection 'input input)))

(defun opencode-test--stream-write (stream text)
  "Write TEXT on STREAM, in a chunk of its own when the stream is chunked."
  (when (process-live-p stream)
    (let ((bytes (encode-coding-string text 'utf-8)))
      (opencode-test--write stream (if (process-get stream 'chunked)
                                       (opencode-test--chunk bytes)
                                     bytes)))))

(defun opencode-test-send-event (event &optional connection)
  "Send EVENT as one SSE frame on CONNECTION, else on every open stream."
  (let ((frame (concat "data: " (json-serialize event) "\n\n")))
    (dolist (stream (if connection (list connection)
                      (opencode-test-service-streams opencode-test-service)))
      (opencode-test--stream-write stream frame))))

(defun opencode-test-send-raw (text)
  "Send TEXT unchanged on every open stream."
  (dolist (stream (opencode-test-service-streams opencode-test-service))
    (opencode-test--stream-write stream text)))

(defun opencode-test-close-streams ()
  "Close every open event stream, as a restarting service would."
  (dolist (stream (opencode-test-service-streams opencode-test-service))
    (when (process-live-p stream) (delete-process stream)))
  (setf (opencode-test-service-streams opencode-test-service) nil))

(defun opencode-test-lose-streams ()
  "Close every open event stream without Emacs noticing.
That leaves the stream wanted, closed and with no reconnection pending,
which nothing should cause but which an unnoticed death of its curl
process once did."
  (cl-letf (((symbol-function 'opencode-events--closed) #'ignore))
    (opencode-test-close-streams)
    (opencode-test-wait-for (lambda () (not (opencode-events-connected-p))))
    ;; The end of the request is reported from a timer.
    (accept-process-output nil 0.2))
  (should opencode-events--wanted)
  (should-not (timerp opencode-events--reconnect-timer)))

(defun opencode-test-request-buffers ()
  "Return the live buffers of requests and streams.
Those of connections kept idle for later requests are left out: there
are a few at most."
  (seq-filter (lambda (buffer)
                (or (string-prefix-p " *opencode-events*" (buffer-name buffer))
                    (and (string-prefix-p " *opencode-transport*" (buffer-name buffer))
                         (not (when-let* ((connection (get-buffer-process buffer)))
                                (and (process-live-p connection)
                                     (null (process-get connection 'opencode-transport-exchange))))))))
              (buffer-list)))

(defun opencode-test-session-stand-in (id)
  "Return a new buffer standing for the buffer of session ID."
  (let ((buffer (generate-new-buffer " *opencode-test-session*")))
    (with-current-buffer buffer
      (setq-local opencode-session-id id))
    buffer))

(defun opencode-test-requests-on (path)
  "Return the requests the fake service received on PATH, oldest first."
  (reverse (seq-filter (lambda (request) (equal (car (split-string (plist-get request :path) "?")) path))
                       (opencode-test-service-requests opencode-test-service))))

(defun opencode-test-requests-in-flight-p ()
  "Return non-nil while a connection carries a request, the event stream's aside."
  (seq-some (lambda (connection)
              (when-let* ((exchange (process-get connection 'opencode-transport-exchange)))
                (and (process-live-p connection)
                     (not (eq exchange opencode-events--stream)))))
            (process-list)))

(defun opencode-test-restart-service (password)
  "Make the fake service a new process, registered with PASSWORD."
  (opencode-test-close-streams)
  (setf (opencode-test-service-pid opencode-test-service)
        (1+ (opencode-test-service-pid opencode-test-service)))
  (opencode-test-service-register opencode-test-service password))

(defmacro opencode-test-with-cli (handler &rest body)
  "Run BODY with HANDLER standing for the OpenCode CLI.
HANDLER is called with the CLI's arguments and a function of an exit
status and optional output that ends the stand-in, which counts as the
running CLI process until then."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'opencode-service--run-cli)
              (lambda (arguments on-exit)
                (let ((process (make-pipe-process :name "opencode-test-cli" :noquery t)))
                  (setq opencode-service--starter process)
                  (funcall ,handler arguments
                           (lambda (status &optional output)
                             (delete-process process)
                             (when (eq opencode-service--starter process)
                               (setq opencode-service--starter nil))
                             (funcall on-exit status (or output ""))))
                  process))))
     ,@body))

(defun opencode-test-wait-for (predicate &optional timeout)
  "Process input until PREDICATE returns non-nil, or fail after TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 10)))
        value)
    (while (and (not (setq value (funcall predicate))) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (or value (ert-fail (format "Timed out waiting for %S" predicate)))))

(defvar opencode-test--server nil
  "The listening process shared by every test.
On Windows, a server made after earlier ones were deleted stops
accepting after a few connections, so one server serves the whole run.")

(defun opencode-test--server ()
  "Return the shared listening process, creating it once."
  (unless (process-live-p opencode-test--server)
    (setq opencode-test--server
          (make-network-process :name "opencode-test-service" :server t
                                :host "127.0.0.1" :service t :family 'ipv4
                                :noquery t :coding 'binary
                                :filter (lambda (connection output)
                                          (opencode-test--filter opencode-test-service
                                                                 connection output)))))
  opencode-test--server)

(defun opencode-test-make-service (password)
  "Return another fake service, listening on a port of its own, with PASSWORD.
It stands for the service of another host, which the helpers above
serve while `opencode-test-service' is bound to it: as its own server's
filter binds it."
  (let* ((service (opencode-test-service--make :password password :pid 5151))
         (server (make-network-process
                  :name "opencode-test-other-service" :server t
                  :host "127.0.0.1" :service t :family 'ipv4
                  :noquery t :coding 'binary
                  :filter (lambda (connection output)
                            (process-put connection 'opencode-test-service service)
                            (let ((opencode-test-service service))
                              (opencode-test--filter service connection output))))))
    (setf (opencode-test-service-server service) server
          (opencode-test-service-port service) (process-contact server :service))
    service))

(defun opencode-test-close-service (service)
  "Stop SERVICE, made by `opencode-test-make-service': its server and connections."
  (dolist (connection (append (opencode-test-service-streams service)
                              (opencode-test-service-held service)))
    (when (process-live-p connection) (delete-process connection)))
  (dolist (process (process-list))
    ;; Connections it accepted are named after its server.
    (when (and (not (eq process (opencode-test-service-server service)))
               (eq (process-type process) 'network)
               (eq (process-get process 'opencode-test-service) service))
      (delete-process process)))
  (when (process-live-p (opencode-test-service-server service))
    (delete-process (opencode-test-service-server service))))

(defmacro opencode-test-with-service (&rest body)
  "Run BODY with `opencode-test-service' serving a fresh registration."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "opencode-service-" t))
          (server (opencode-test--server))
          (opencode-test-service
           (opencode-test-service--make :server server :port (process-contact server :service)
                                        :password "first-password"
                                        :registration (expand-file-name "service.json" directory)))
          (opencode-service-registration-file (opencode-test-service-registration opencode-test-service))
          (opencode-service-connection nil)
          (opencode-service--waiters nil)
          (opencode-service--starter nil)
          (opencode-service--lookup 0)
          (opencode-service--import-checked nil)
          (opencode-command "opencode-test-missing-cli")
          (opencode-events--handlers (copy-hash-table opencode-events--handlers))
          (opencode-events--stream nil)
          (opencode-transport--idle (make-hash-table :test #'equal))
          (opencode-events--wanted nil)
          (opencode-events--closed-for-stop nil)
          (opencode-events--opening nil)
          (opencode-events--connection nil)
          (opencode-events--backoff 1)
          (opencode-events--reconnect-timer nil)
          (opencode-events--watchdog nil)
          (opencode-events--last-output 0)
          (opencode-events--last-watch nil)
          (opencode-events--resumed 0)
          (opencode-events--queue nil)
          (opencode-events--dispatch-timer nil)
          ;; Each fake service is a new world: its first stream is the
          ;; first Emacs opens, whatever tests ran before.
          (opencode--stream-seen nil)
          (opencode--resync-epoch 0)
          (opencode--resync-queue nil)
          (opencode--resync-running nil)
          (opencode--resync-active nil))
     (opencode-test-service-register opencode-test-service)
     (unwind-protect (opencode-test-with-state ,@body)
       (opencode-disconnect)
       (dolist (connection (append (opencode-test-service-streams opencode-test-service)
                                   (opencode-test-service-held opencode-test-service)))
         (when (process-live-p connection) (delete-process connection)))
       ;; Requests still in flight, such as those a reconnection sends,
       ;; would otherwise report their outcome during a later test.
       (let ((deadline (+ (float-time) 5)))
         (while (and (< (float-time) deadline) (opencode-test-requests-in-flight-p))
           (accept-process-output nil 0.05)))
       (opencode-transport-close-idle)
       (delete-directory directory t))))

;;;; A session over sockets

;; The lifecycle and pending tests send prompts to a session the fake
;; service serves.

(defconst opencode-test-lifecycle-session "ses_lifecycle"
  "ID of the session the sending tests open.")

(defvar opencode-test-lifecycle-transcript nil
  "Messages of the session the sending tests open, oldest first, as the service holds them.")

(defun opencode-test-query (request)
  "Return the query of REQUEST to the fake service, an alist of symbols to strings."
  (when-let* ((query (cadr (split-string (plist-get request :path) "?"))))
    (mapcar (lambda (pair) (cons (intern (car pair)) (cadr pair)))
            (url-parse-query-string query))))

(defun opencode-test-lifecycle-routes ()
  "Return the routes serving one session and what opening it asks.
Its transcript is `opencode-test-lifecycle-transcript'."
  (let ((info `((id . ,opencode-test-lifecycle-session) (title . "Lifecycle") (agent . "build")
                (time (created . 1792531215759) (updated . 1792531215759))
                (location (directory . ,(directory-file-name opencode-test-workspace))))))
    (append
     (list (cons (concat "/api/session/" opencode-test-lifecycle-session)
                 (lambda (_) `(200 . ((data . ,info)))))
           (cons (concat "/api/session/" opencode-test-lifecycle-session "/message")
                 (lambda (request)
                   (let ((page (opencode-test-message-page opencode-test-lifecycle-transcript
                                                           (list :query (opencode-test-query request)))))
                     (cons 200 `((data . ,(vconcat (alist-get 'data page)))
                                 (cursor ,@(alist-get 'cursor page)))))))
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
  "Run a turn of the open session answering TEXT to the prompt sent last.
The service's transcript holds the turn before its events go out on the
stream, as the service appends a message before the event naming it, so
that the session buffer finds the turn whenever it catches up."
  (let* ((session opencode-test-lifecycle-session)
         (prompt (json-parse-string
                  (plist-get (car (last (opencode-test-requests-on
                                         (concat "/api/session/" session "/prompt"))))
                             :body)
                  :object-type 'alist))
         (input (alist-get 'id prompt))
         (base `((sessionID . ,session) (assistantMessageID . "msg_lifecycle_reply") (ordinal . 0))))
    (setq opencode-test-lifecycle-transcript
          (append opencode-test-lifecycle-transcript
                  `(((id . ,input) (type . "user") (text . ,(alist-get 'text prompt)) (time (created . 1)))
                    ((id . "msg_lifecycle_reply") (type . "assistant") (time (created . 2) (completed . 3))
                     (content . [((type . "text") (text . ,text))]))
                    ((id . "msg_lifecycle_idle") (type . "idle") (outcome . "succeeded") (time (created . 4))))))
    (dolist (event `(((type . "session.execution.started") (data (sessionID . ,session)))
                     ((type . "session.inbox.delivered") (data (sessionID . ,session) (inboxID . ,input)))
                     ((type . "session.step.started")
                      (data (sessionID . ,session) (assistantMessageID . "msg_lifecycle_reply") (started . 2)))
                     ((type . "session.text.started") (data ,@base))
                     ((type . "session.text.delta") (data ,@base (delta . ,text)))
                     ((type . "session.text.ended") (data ,@base (text . ,text)))
                     ((type . "session.step.ended")
                      (data (sessionID . ,session) (assistantMessageID . "msg_lifecycle_reply") (finish . "stop")))
                     ((id . "evt_lifecycle_idle") (type . "session.execution.succeeded")
                      (data (sessionID . ,session)))))
      (opencode-test-send-event event))))

(defmacro opencode-test-with-lifecycle-session (buffer &rest body)
  "Run BODY with BUFFER bound to the open session's buffer, its stream open on both ends."
  (declare (indent 1) (debug t))
  `(opencode-test-with-service
     (setf (opencode-test-service-routes opencode-test-service) (opencode-test-lifecycle-routes))
     (opencode-test-with-fresh-catalogs
       (let ((opencode-session-buffers (make-hash-table :test #'equal))
             (opencode-test-lifecycle-transcript nil)
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
                 ;; Events sent before the service holds the stream would be lost.
                 (opencode-test-wait-for (lambda ()
                                           (and (opencode-test-service-streams opencode-test-service)
                                                (opencode-events-connected-p)
                                                (eq opencode-events--greeted opencode-events--stream))))
                 ,@body)
             (when (buffer-live-p ,buffer) (kill-buffer ,buffer))))))))

(defun opencode-test-type-prompt (buffer text)
  "Type TEXT at the input of BUFFER and send it as RET does."
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert text)
    (opencode-send-input)))

(defun opencode-test-wait-caught-up (buffer epoch)
  "Wait until BUFFER caught up after a reconnection later than EPOCH."
  (opencode-test-wait-for
   (lambda ()
     (and (> opencode--resync-epoch epoch)
          (null opencode--resync-queue)
          (null opencode--resync-running)
          (with-current-buffer buffer
            (not (or opencode--holding opencode--loading opencode--resync-wanted)))
          (not (opencode-test-requests-in-flight-p))))))

(defun opencode-test-catch-up (buffer)
  "Have BUFFER catch up with the service, as when its stream reopens, and wait until it did."
  (let ((epoch opencode--resync-epoch))
    (opencode-test-send-event '((type . "server.connected") (data)))
    (opencode-test-wait-caught-up buffer epoch)))

(defun opencode-test-buffer-count (buffer text)
  "Return how many times BUFFER shows TEXT."
  (with-current-buffer buffer
    (how-many (regexp-quote text) (point-min) (point-max))))

(defun opencode-test-buffer-shows-p (buffer text)
  "Return non-nil when BUFFER shows TEXT."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward text nil t))))

(provide 'opencode-test-server)
;;; opencode-test-server.el ends here
