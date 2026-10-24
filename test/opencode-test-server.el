;;; opencode-test-server.el --- A fake OpenCode service for transport tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; `opencode-test-with-service' runs a local HTTP server standing for the
;; OpenCode service, registered in a temporary service.json, so requests
;; go through curl and plz as they do against the real service.  Routes
;; answer JSON, `/api/info' reports the registered process ID, and
;; `/api/event' keeps its connection open for `opencode-test-send-event'.
;; It listens on IPv4 only: Winsock's IPv6 loopback is unreliable here.

;;; Code:

(require 'opencode-test-helpers)

(cl-defstruct (opencode-test-service (:constructor opencode-test-service--make))
  server port password (pid 4242) registration
  (info-status 200) routes requests streams)

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

(defun opencode-test--respond (connection status body &optional content-type)
  "Answer CONNECTION with STATUS and BODY, then close it."
  (let ((payload (encode-coding-string (or body "") 'utf-8)))
    (process-send-string
     connection
     (concat (format "HTTP/1.1 %d %s\r\n" status (if (< status 400) "OK" "Error"))
             (format "Content-Type: %s\r\n" (or content-type "application/json"))
             (format "Content-Length: %d\r\n" (length payload))
             "Connection: close\r\n\r\n"
             payload))
    (delete-process connection)))

(defun opencode-test--authorized-p (service headers)
  "Return non-nil when HEADERS carry SERVICE's credentials."
  (equal (cdr (assoc "authorization" headers))
         (concat "Basic " (base64-encode-string
                           (concat "opencode:" (opencode-test-service-password service)) t))))

(defun opencode-test--handle (service connection request)
  "Answer REQUEST, a plist (:method :path :headers :body), on CONNECTION."
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
      (process-send-string connection
                           (concat "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n"
                                   "Cache-Control: no-cache\r\n\r\n"))
      (push connection (opencode-test-service-streams service))
      (opencode-test-send-event '((type . "server.connected") (data)) connection))
     (t
      (let ((route (seq-find (lambda (route) (equal (car route) path))
                             (opencode-test-service-routes service))))
        (if route
            (pcase-let ((`(,status . ,body) (funcall (cdr route) request)))
              (opencode-test--respond connection status (json-serialize body)))
          (opencode-test--respond connection 404 "{\"message\":\"Not found\"}")))))))

(defun opencode-test--filter (service connection output)
  "Collect OUTPUT of CONNECTION and handle SERVICE's request once complete."
  (let ((input (concat (or (process-get connection 'input) "") output)))
    (process-put connection 'input input)
    (when-let* ((end (string-search "\r\n\r\n" input)))
      (let* ((lines (split-string (substring input 0 end) "\r\n"))
             (start (split-string (car lines) " "))
             (headers (mapcar (lambda (line)
                                (when (string-match "\\`\\([^:]+\\): *\\(.*\\)\\'" line)
                                  (cons (downcase (match-string 1 line)) (match-string 2 line))))
                              (cdr lines)))
             (length (string-to-number (or (cdr (assoc "content-length" headers)) "0")))
             (body (substring input (+ end 4))))
        (when (and (>= (string-bytes body) length) (not (process-get connection 'handled)))
          (process-put connection 'handled t)
          (opencode-test--handle service connection
                                 (list :method (car start) :path (cadr start) :headers headers
                                       :body (decode-coding-string body 'utf-8))))))))

(defun opencode-test-send-event (event &optional connection)
  "Send EVENT as one SSE frame on CONNECTION, else on every open stream."
  (let ((frame (concat "data: " (json-serialize event) "\n\n")))
    (dolist (stream (if connection (list connection)
                      (opencode-test-service-streams opencode-test-service)))
      (when (process-live-p stream)
        (process-send-string stream (encode-coding-string frame 'utf-8))))))

(defun opencode-test-send-raw (text)
  "Send TEXT unchanged on every open stream."
  (dolist (stream (opencode-test-service-streams opencode-test-service))
    (when (process-live-p stream)
      (process-send-string stream (encode-coding-string text 'utf-8)))))

(defun opencode-test-close-streams ()
  "Close every open event stream, as a restarting service would."
  (dolist (stream (opencode-test-service-streams opencode-test-service))
    (when (process-live-p stream) (delete-process stream)))
  (setf (opencode-test-service-streams opencode-test-service) nil))

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
          (opencode-service--import-checked nil)
          (opencode-command "opencode-test-missing-cli")
          (opencode-events--handlers (copy-hash-table opencode-events--handlers))
          (opencode-events--process nil)
          (opencode-events--wanted nil)
          (opencode-events--closed-for-stop nil)
          (opencode-events--backoff 1)
          (opencode-events--reconnect-timer nil)
          (opencode-events--watchdog nil)
          (opencode-events--queue nil)
          (opencode-events--dispatch-timer nil))
     (opencode-test-service-register opencode-test-service)
     (unwind-protect (progn ,@body)
       (opencode-disconnect)
       (dolist (stream (opencode-test-service-streams opencode-test-service))
         (when (process-live-p stream) (delete-process stream)))
       (delete-directory directory t))))

(provide 'opencode-test-server)
;;; opencode-test-server.el ends here
