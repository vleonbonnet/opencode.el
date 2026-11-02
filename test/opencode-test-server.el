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
          (opencode-events--dispatch-timer nil))
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

(provide 'opencode-test-server)
;;; opencode-test-server.el ends here
