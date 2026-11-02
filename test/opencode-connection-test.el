;;; opencode-connection-test.el --- Tests of the HTTP client over sockets  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; `opencode-transport-request' reads answers however they are split,
;; keeps connections for later requests, sends a request again when a
;; kept connection was closed under it, and settles every request
;; exactly once, whatever ends it.  The parser is checked on answers cut
;; at every byte; the rest talks to `opencode-test-with-service'.  No
;; request starts a process, so none waits for one to start.

;;; Code:

(require 'opencode-test-server)

;;;; Reading answers in pieces

(cl-defun opencode-test-parse-pieces (pieces &key (method 'get) filter ended)
  "Parse the answer PIECES, inserted one after the other as reads would.
METHOD is the request's, FILTER a stream's filter, and ENDED non-nil
when the connection ends after the last piece.  Return a plist of the
outcome, the status, the headers, the body read and the bytes left."
  (let ((exchange (opencode-transport--make-exchange :method method :filter filter
                                                     :then #'ignore :else #'ignore))
        outcome)
    (opencode-transport--reset exchange)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((rest pieces))
        (while rest
          (goto-char (point-max))
          (insert (car rest))
          (setq outcome (opencode-transport--parse exchange (and ended (null (cdr rest))))
                rest (cdr rest))))
      (list :outcome outcome
            :status (opencode-transport-exchange-status exchange)
            :headers (opencode-transport-exchange-headers exchange)
            :body (and (eq outcome 'done)
                       (buffer-substring-no-properties
                        (point-min) (opencode-transport-exchange-body-end exchange)))
            :rest (and (eq outcome 'done)
                       (buffer-substring-no-properties
                        (opencode-transport-exchange-body-end exchange) (point-max)))))))

(defun opencode-test-every-cut (answer)
  "Return ANSWER as single bytes, and as two pieces cut at every byte."
  (cons (mapcar #'string answer)
        (cl-loop for cut from 1 below (length answer)
                 collect (list (substring answer 0 cut) (substring answer cut)))))

(defconst opencode-test-utf-8-body (encode-coding-string "{\"a\":\"été ☕\"}" 'utf-8)
  "A JSON body as bytes, with multibyte characters.")

(ert-deftest opencode-test-parse-every-cut ()
  "An answer reads the same however reads split it, whatever frames its body."
  (dolist (case
           `((,(concat (format "HTTP/1.1 200 OK\r\nContent-Length: %d\r\n" (length opencode-test-utf-8-body))
                       "Content-Type: application/json\r\n\r\n" opencode-test-utf-8-body)
              ,opencode-test-utf-8-body)
             ;; Chunks with an extension, one of them empty of the body's end, and a trailer.
             (,(concat "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
                       "4;name=value\r\n{\"a\"\r\n"
                       (opencode-test--chunk ":[1,2,3]}\r\n")
                       "0\r\nX-Trailer: yes\r\n\r\n")
              "{\"a\":[1,2,3]}\r\n")
             ;; An interim answer comes first.
             ("HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 201 Created\r\nContent-Length: 2\r\n\r\n{}" "{}")
             ("HTTP/1.1 204 No Content\r\nContent-Type: application/json\r\n\r\n" "")))
    (pcase-let ((`(,answer ,body) case))
      (dolist (pieces (opencode-test-every-cut answer))
        (let ((parsed (opencode-test-parse-pieces pieces)))
          (should (eq (plist-get parsed :outcome) 'done))
          (should (equal (plist-get parsed :body) body))
          (should (equal (plist-get parsed :rest) "")))))))

(ert-deftest opencode-test-parse-headers-and-framing ()
  "Headers are read without regard to case, folded lines joined; the end of the connection frames a bare body."
  (let ((parsed (opencode-test-parse-pieces '("HTTP/1.1 204 No Content\r\nX-Folded: a\r\n b\r\n\r\n"))))
    (should (equal (cdr (assoc "x-folded" (plist-get parsed :headers))) "a b")))
  (let ((answer "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nuntil the end"))
    (dolist (pieces (opencode-test-every-cut answer))
      (let ((parsed (opencode-test-parse-pieces pieces :ended t)))
        (should (eq (plist-get parsed :outcome) 'done))
        (should (equal (plist-get parsed :body) "until the end"))))
    ;; Until the connection ends, such a body may go on.
    (should-not (plist-get (opencode-test-parse-pieces (list answer)) :outcome)))
  ;; A HEAD answer has no body, whatever its Content-Length says.
  (should (equal (plist-get (opencode-test-parse-pieces '("HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n")
                                                        :method 'head)
                            :body)
                 "")))

(ert-deftest opencode-test-parse-stream-every-cut ()
  "A stream's filter receives the body, without its chunk framing, however reads split it."
  (let* ((frames "data: {\"a\":1}\r\n\r\ndata: {\"b\":\"☕\"}\n\n")
         (bytes (encode-coding-string frames 'utf-8))
         (answer (concat "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nContent-Type: text/event-stream\r\n\r\n"
                         (opencode-test--chunk (substring bytes 0 9))
                         (opencode-test--chunk (substring bytes 9)))))
    (dolist (pieces (opencode-test-every-cut answer))
      (let* ((received nil)
             (parsed (opencode-test-parse-pieces
                      pieces :filter (lambda (_exchange output) (push output received)))))
        (should-not (plist-get parsed :outcome))
        (should (equal (apply #'concat (nreverse received)) bytes))))))

(ert-deftest opencode-test-parse-malformed ()
  "What is not an HTTP answer is refused rather than misread."
  (dolist (answer '("HTTP/1.1 OK\r\n\r\n"
                    "SSH-2.0-OpenSSH_9.6\r\n\r\n"
                    "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nContent-Length: 3\r\n\r\n{}"
                    "HTTP/1.1 200 OK\r\nContent-Length: -1\r\n\r\n"
                    "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nzz\r\n"
                    "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n2\r\n{}XX"))
    (should (condition-case nil
                (progn (opencode-test-parse-pieces (list answer)) nil)
              (opencode-transport-protocol t)))))

;;;; Answers over the network

(defun opencode-test-get (path &rest arguments)
  "Request PATH of the fake service, waiting; return (THEN . VALUE) or (ELSE . FAILURE).
ARGUMENTS are further `opencode-transport-request' keywords.  Fail
when a callback runs twice."
  (let ((outcomes nil))
    (apply #'opencode-transport-request 'get (concat (opencode-test-service-url opencode-test-service) path)
           :headers `(("Authorization" . ,(opencode-service-authorization
                                           (list :password (opencode-test-service-password
                                                            opencode-test-service)))))
           :then (lambda (value) (push (cons 'then value) outcomes))
           :else (lambda (failure) (push (cons 'else failure) outcomes))
           arguments)
    (opencode-test-wait-for (lambda () outcomes) 20)
    (accept-process-output nil 0.05)
    (should (= (length outcomes) 1))
    (car outcomes)))

(defun opencode-test-service-connections ()
  "Return how many distinct connections carried requests to the fake service."
  (length (delete-dups (mapcar (lambda (request) (plist-get request :connection))
                               (opencode-test-service-requests opencode-test-service)))))

(defun opencode-test-connections-of (path)
  "Return the distinct server-side connections that carried requests on PATH."
  (delete-dups (mapcar (lambda (request) (plist-get request :connection))
                       (opencode-test-requests-on path))))

(ert-deftest opencode-test-answers-in-pieces ()
  "Answers written a few bytes at a time are read whole, with either framing, UTF-8 included."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/agent" (lambda (_) '(200 . ((data . [((id . "build") (name . "Café ☕"))])))))))
    (dolist (chunked '(nil 5))
      (setf (opencode-test-service-split opencode-test-service) 3
            (opencode-test-service-chunked opencode-test-service) chunked)
      (pcase-let ((`(,outcome . ,value)
                   (opencode-test-get "/api/agent"
                                      :as (lambda () (json-parse-buffer :object-type 'alist)))))
        (should (eq outcome 'then))
        (should (equal value '((data . [((id . "build") (name . "Café ☕"))]))))))))

(ert-deftest opencode-test-large-answers ()
  "Answers of megabytes are read whole, framed by length or in chunks."
  (opencode-test-with-service
    (let ((big (apply #'string (cl-loop for index below (* 6 1024 1024) collect (+ ?a (% index 26))))))
      (setf (opencode-test-service-routes opencode-test-service)
            (list (cons "/api/agent" (lambda (_) (cons 200 big)))))
      (dolist (chunked '(nil 65536))
        (setf (opencode-test-service-chunked opencode-test-service) chunked)
        (pcase-let ((`(,outcome . ,value) (opencode-test-get "/api/agent")))
          (should (eq outcome 'then))
          (should (= (length value) (length big)))
          (should (string= value big)))))))

(ert-deftest opencode-test-connections-kept ()
  "Requests one after the other share a connection, errors included; a few are kept at most."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))
                (cons "/api/session/active" (lambda (_) '(500 . ((message . "boom")))))))
    (dotimes (_ 3)
      (should (eq (car (opencode-test-get "/api/agent")) 'then))
      (let ((failure (cdr (opencode-test-get "/api/session/active"))))
        (should (eq (opencode-transport-failure-reason failure) 'http))
        (should (= (opencode-transport-failure-status failure) 500))
        (should (equal (opencode-transport-failure-body failure) "{\"message\":\"boom\"}"))))
    (should (= (length (delete-dups (append (opencode-test-connections-of "/api/agent")
                                            (opencode-test-connections-of "/api/session/active"))))
               1))
    (should (= (opencode-transport-idle-count) 1))
    ;; Without keep-alive, the kept connection serves one more request,
    ;; and every other one has a connection of its own.
    (setf (opencode-test-service-keep-alive opencode-test-service) nil)
    (dotimes (_ 3) (should (eq (car (opencode-test-get "/api/agent")) 'then)))
    (should (= (length (opencode-test-connections-of "/api/agent")) 3))
    (should (zerop (opencode-transport-idle-count)))))

(ert-deftest opencode-test-concurrent-requests ()
  "Requests in flight together each get their own answer, once, and few connections stay."
  (opencode-test-with-service
    (let ((answers (make-hash-table :test #'equal))
          (count 40))
      (setf (opencode-test-service-routes opencode-test-service)
            (cl-loop for index below count
                     collect (let ((id (format "ses_%02d" index)))
                               (cons (concat "/api/session/" id)
                                     (lambda (_) (cons 200 `((data (id . ,id)))))))))
      (dotimes (index count)
        (let ((id (format "ses_%02d" index)))
          (opencode-transport-request
           'get (format "%s/api/session/%s" (opencode-test-service-url opencode-test-service) id)
           :headers `(("Authorization" . ,(opencode-service-authorization (list :password "first-password"))))
           :as (lambda () (json-parse-buffer :object-type 'alist))
           :then (lambda (value) (push (alist-get 'id (alist-get 'data value)) (gethash id answers)))
           :else (lambda (failure) (push failure (gethash id answers))))))
      (opencode-test-wait-for (lambda () (= (hash-table-count answers) count)) 20)
      (accept-process-output nil 0.1)
      (maphash (lambda (id values) (should (equal values (list id)))) answers)
      (should (<= (opencode-transport-idle-count) opencode-transport-idle-connections))
      ;; Kept connections serve the next ones.
      (let ((before (opencode-test-service-connections)))
        (dotimes (_ 3) (should (eq (car (opencode-test-get "/api/session/ses_00")) 'then)))
        (should (= (opencode-test-service-connections) before))))))

(ert-deftest opencode-test-kept-connection-closed ()
  "A kept connection the service closed is not used again, and one closed under a request is replaced.
The request goes out once more on a new connection, as long as no answer had begun."
  (opencode-test-with-service
    (let ((answered 0))
      (setf (opencode-test-service-routes opencode-test-service)
            (list (cons "/api/agent" (lambda (_) (cl-incf answered) '(200 . ((data . [])))))
                  (cons "/api/command"
                        (lambda (_)
                          ;; The first one finds its kept connection closing.
                          (if (= (length (opencode-test-requests-on "/api/command")) 1)
                              '(close)
                            '(200 . ((data . ["compact"]))))))))
      (should (eq (car (opencode-test-get "/api/agent")) 'then))
      ;; The service closes the idle connection, as Node does after its keep-alive timeout.
      (delete-process (car (opencode-test-connections-of "/api/agent")))
      (opencode-test-wait-for (lambda () (zerop (opencode-transport-idle-count))))
      (should (eq (car (opencode-test-get "/api/agent")) 'then))
      (should (= (length (opencode-test-connections-of "/api/agent")) 2))
      ;; Closed as the request arrives on it.
      (pcase-let ((`(,outcome . ,value) (opencode-test-get "/api/command")))
        (should (eq outcome 'then))
        (should (equal value "{\"data\":[\"compact\"]}")))
      (let ((commands (opencode-test-requests-on "/api/command")))
        (should (= (length commands) 2))
        (should (eq (plist-get (car commands) :connection)
                    (car (last (opencode-test-connections-of "/api/agent"))))))
      (should (= answered 2)))))

(ert-deftest opencode-test-closed-without-answer ()
  "A new connection closed without an answer fails as closed; one closed during the answer, as truncated.
`opencode-request' sends the first once more after looking for the
service, on a new connection, and reports the second at once.  Its
first attempt went on a kept connection, and was sent again on a new
one when that closed."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))
                (cons "/api/command" (lambda (_) '(close)))
                (cons "/api/provider"
                      (lambda (_) '(raw . "HTTP/1.1 200 OK\r\nContent-Length: 100\r\n\r\n{\"data\":")))
                (cons "/api/model" (lambda (_) '(raw . "SSH-2.0-OpenSSH_9.6\r\n\r\n")))))
    (opencode-transport-close-idle)
    (let ((failure (cdr (opencode-test-get "/api/command"))))
      (should (eq (opencode-transport-failure-reason failure) 'closed)))
    (let ((failure (cdr (opencode-test-get "/api/provider"))))
      (should (eq (opencode-transport-failure-reason failure) 'truncated)))
    (let ((failure (cdr (opencode-test-get "/api/model"))))
      (should (eq (opencode-transport-failure-reason failure) 'protocol)))
    (let ((errors nil))
      (dolist (path '("/api/command" "/api/provider"))
        (opencode-request 'get path :buffer nil :else (lambda (error) (push (cons path error) errors))))
      (opencode-test-wait-for (lambda () (= (length errors) 2)))
      (accept-process-output nil 0.1)
      (should (= (length errors) 2))
      (should (= (length (opencode-test-requests-on "/api/command")) 4))
      (should (= (length (opencode-test-requests-on "/api/provider")) 2))
      (should (string-match-p "without an answer"
                              (opencode-error-message (cdr (assoc "/api/command" errors))))))
    ;; The connections that failed are not kept.
    (should (eq (car (opencode-test-get "/api/agent")) 'then))
    (should (<= 1 (opencode-transport-idle-count) opencode-transport-idle-connections))))

(ert-deftest opencode-test-request-timeouts ()
  "A request answered too late fails as a timeout, and its connection is closed, not kept.
So does a connection that does not open in time."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))
                (cons "/api/session/active" (lambda (_) '(hang)))))
    (let ((failure (cdr (opencode-test-get "/api/session/active" :timeout 0.3))))
      (should (eq (opencode-transport-failure-reason failure) 'timeout))
      (should (string-match-p "no complete answer within 0.3 s" (opencode-transport-failure-message failure))))
    (opencode-test-wait-for (lambda () (not (process-live-p (car (opencode-test-service-held opencode-test-service))))))
    (should (eq (car (opencode-test-get "/api/agent")) 'then))
    (opencode-transport-close-idle)
    ;; A connection whose opening is never reported.
    (cl-letf* ((make-network-process (symbol-function 'make-network-process))
               ((symbol-function 'make-network-process)
                (lambda (&rest arguments)
                  (let ((sentinel (plist-get arguments :sentinel)))
                    (apply make-network-process
                           (plist-put (copy-sequence arguments) :sentinel
                                      (lambda (process event)
                                        (unless (equal event "open\n")
                                          (funcall sentinel process event)))))))))
      (let ((failure (cdr (opencode-test-get "/api/agent" :connect-timeout 0.3))))
        (should (eq (opencode-transport-failure-reason failure) 'timeout))
        (should (string-match-p "no connection" (opencode-transport-failure-message failure)))))
    (should (zerop (opencode-transport-idle-count)))))

(ert-deftest opencode-test-request-refused ()
  "A port nothing listens on refuses the connection, after the request returned."
  (opencode-test-with-service
    (let* ((outcomes nil)
           (exchange (opencode-transport-request
                      'get "http://127.0.0.1:1/api/agent"
                      :then (lambda (value) (push (cons 'then value) outcomes))
                      :else (lambda (failure) (push (cons 'else failure) outcomes)))))
      (should-not outcomes)
      (should (opencode-transport-live-p exchange))
      (opencode-test-wait-for (lambda () outcomes))
      (should (eq (opencode-transport-failure-reason (cdar outcomes)) 'refused))
      (should-not (opencode-transport-live-p exchange)))
    ;; Nor is a URL Emacs cannot reach this way answered during the call.
    (let ((failure nil))
      (opencode-transport-request 'get "https://127.0.0.1:1/" :else (lambda (value) (setq failure value)))
      (should-not failure)
      (opencode-test-wait-for (lambda () failure))
      (should (eq (opencode-transport-failure-reason failure) 'protocol)))))

(ert-deftest opencode-test-request-cancelled ()
  "A cancelled request fails once as cancelled, its connection closed and its buffer killed.
Cancelling one settled already does nothing."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))
                (cons "/api/session/active" (lambda (_) '(hang)))))
    (let ((before (length (opencode-test-request-buffers)))
          (outcomes nil))
      (cl-flet ((request (path)
                  (opencode-transport-request
                   'get (concat (opencode-test-service-url opencode-test-service) path)
                   :headers `(("Authorization" . ,(opencode-service-authorization
                                                   (list :password "first-password"))))
                   :then (lambda (value) (push (list path 'then value) outcomes))
                   :else (lambda (failure)
                           (push (list path 'else (opencode-transport-failure-reason failure)) outcomes)))))
        ;; While it waits for its answer.
        (let ((exchange (request "/api/session/active")))
          (opencode-test-wait-for (lambda () (opencode-test-service-held opencode-test-service)))
          (opencode-transport-cancel exchange)
          (should (equal outcomes '(("/api/session/active" else cancelled))))
          (opencode-transport-cancel exchange)
          (opencode-test-wait-for (lambda () (not (process-live-p (car (opencode-test-service-held
                                                                         opencode-test-service)))))))
        ;; Before its connection opened.
        (setq outcomes nil)
        (opencode-transport-cancel (request "/api/agent"))
        (should (equal outcomes '(("/api/agent" else cancelled))))
        ;; Once it was answered.
        (setq outcomes nil)
        (let ((exchange (request "/api/agent")))
          (opencode-test-wait-for (lambda () outcomes))
          (opencode-transport-cancel exchange)
          (accept-process-output nil 0.1)
          (should (equal (mapcar #'cadr outcomes) '(then)))))
      (should (= (length (opencode-test-request-buffers)) before)))))

(ert-deftest opencode-test-service-moved ()
  "A service restarted on another port with another password is found, and requests follow it.
The connections kept to the one that went away are not used again."
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))))
    (let* ((old (make-network-process :name "opencode-test-old-service" :server t
                                      :host "127.0.0.1" :service t :family 'ipv4
                                      :noquery t :coding 'binary
                                      :filter (lambda (connection output)
                                                (opencode-test--filter opencode-test-service
                                                                       connection output))))
           (new-port (opencode-test-service-port opencode-test-service)))
      (unwind-protect
          (progn
            (setf (opencode-test-service-port opencode-test-service) (process-contact old :service))
            (opencode-test-service-register opencode-test-service)
            (should (opencode-request-sync 'get "/api/agent" :timeout 10))
            (should (>= (opencode-transport-idle-count) 1))
            ;; It goes away, its connections with it, and another one registers.
            (let ((connections (opencode-test-connections-of "/api/agent")))
              (delete-process old)
              (mapc #'delete-process connections))
            (setf (opencode-test-service-port opencode-test-service) new-port
                  (opencode-test-service-pid opencode-test-service) 4343)
            (opencode-test-service-register opencode-test-service "second-password")
            (should (opencode-request-sync 'get "/api/agent" :timeout 10))
            (should (equal (plist-get opencode-service-connection :url)
                           (opencode-test-service-url opencode-test-service)))
            (let ((last (car (last (opencode-test-requests-on "/api/agent")))))
              (should (equal (cdr (assoc "authorization" (plist-get last :headers)))
                             (opencode-service-authorization (list :password "second-password"))))))
        (when (process-live-p old) (delete-process old))))))

(ert-deftest opencode-test-requests-start-no-process ()
  "Requests start no process, so a process start taking a second holds none of them.
Starting a process could stop Emacs for seconds, so requests open sockets instead."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))))
    (opencode-request-sync 'get "/api/agent" :timeout 10)
    (let ((starts 0)
          (answers nil)
          (longest 0))
      (cl-letf* ((make-process (symbol-function 'make-process))
                 ((symbol-function 'make-process)
                  (lambda (&rest arguments)
                    (cl-incf starts)
                    (let ((end (+ (float-time) 1)))
                      (while (< (float-time) end)))
                    (apply make-process arguments))))
        (let ((start (float-time)))
          (dotimes (_ 5)
            (let ((call (float-time)))
              (opencode-request 'get "/api/agent" :buffer nil
                                :then (lambda (_) (push (- (float-time) start) answers)))
              (setq longest (max longest (- (float-time) call)))))
          (opencode-test-wait-for (lambda () (= (length answers) 5)))
          (should (zerop starts))
          (should (< longest 0.1))
          (should (< (apply #'max answers) 0.5))
          ;; The event stream too.
          (opencode-events-connect)
          (opencode-test-wait-for #'opencode-events-connected-p)
          (should (zerop starts)))))))

(ert-deftest opencode-test-chunked-event-stream ()
  "A stream in chunks delivers frames whatever the chunks and reads split, and reopens when it ends."
  (opencode-test-with-service
    (setf (opencode-test-service-chunked opencode-test-service) 7
          (opencode-test-service-split opencode-test-service) 5)
    (let ((seen nil))
      (opencode-events-register t (lambda (event) (push (alist-get 'type event) seen)))
      (opencode-events-connect)
      (opencode-test-wait-for (lambda () (member "server.connected" seen)))
      ;; One frame across chunks, two in one.
      (opencode-test-send-raw "data: {\"type\":\"te")
      (opencode-test-send-raw "st.one\"}\n\ndata: {\"type\":\"test.two\"}\n\ndata: {\"type\":\"test.three\"}\n\n")
      (opencode-test-wait-for (lambda () (equal (seq-take seen 3) '("test.three" "test.two" "test.one"))))
      (setq opencode-events--backoff 0.1)
      (opencode-test-close-streams)
      (opencode-test-wait-for (lambda () (= (seq-count (lambda (type) (equal type "server.connected")) seen) 2))))))

(provide 'opencode-connection-test)
;;; opencode-connection-test.el ends here
