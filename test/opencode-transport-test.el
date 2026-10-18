;;; opencode-transport-test.el --- Tests of the service, requests and event stream  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; These tests talk HTTP through curl to `opencode-test-with-service',
;; never to a real OpenCode service.

;;; Code:

(require 'opencode-test-server)

(defun opencode-test-last-request (path)
  "Return the latest request the fake service received on PATH."
  (seq-find (lambda (request) (string-prefix-p path (plist-get request :path)))
            (opencode-test-service-requests opencode-test-service)))

(ert-deftest opencode-test-request-through-registration ()
  "Requests find the service through its registration and carry its credentials."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/agent" (lambda (_) '(200 . ((data . [((id . "build"))])))))
                (cons "/api/session" (lambda (request)
                                       (cons 200 `((data . ((echo . ,(plist-get request :body))))))))))
    (let ((default-directory opencode-test-workspace))
      (should (equal (opencode-request-data (opencode-request-sync 'get "/api/agent" :timeout 10))
                     '(((id . "build")))))
      (should (equal (plist-get opencode-service-connection :url)
                     (opencode-test-service-url opencode-test-service)))
      (let ((request (opencode-test-last-request "/api/agent")))
        (should (equal (cdr (assoc "x-opencode-directory" (plist-get request :headers)))
                       (url-hexify-string (directory-file-name
                                           (opencode-normalize-directory opencode-test-workspace))))))
      ;; Bodies are UTF-8 JSON.
      (let ((response (opencode-request-sync 'post "/api/session" :body '((title . "Café ☕")) :timeout 10)))
        (should (equal (json-parse-string (alist-get 'echo (opencode-request-data response))
                                          :object-type 'alist)
                       '((title . "Café ☕"))))))))

(ert-deftest opencode-test-request-errors ()
  "Failed requests carry the status and the server's message."
  (opencode-test-with-service
    (let ((error nil))
      (opencode-request 'get "/api/missing" :buffer nil :else (lambda (value) (setq error value)))
      (opencode-test-wait-for (lambda () error))
      (should (eql (opencode-error-status error) 404))
      (should (equal (opencode-error-describe error) "GET /api/missing failed (HTTP 404): Not found")))))

(ert-deftest opencode-test-service-starting ()
  "A service answering 503 while it boots is waited for."
  (opencode-test-with-service
    (let ((answers 0)
          (ready nil))
      (setf (opencode-test-service-info-status opencode-test-service)
            (lambda () (if (< (cl-incf answers) 3) 503 200)))
      (opencode-service-ensure (lambda (connection) (setq ready connection)))
      (opencode-test-wait-for (lambda () ready))
      (should (>= answers 3)))))

(ert-deftest opencode-test-service-restarted-password ()
  "A 401 after the service restarted with a new password rereads the registration."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))))
    (opencode-request-sync 'get "/api/agent" :timeout 10)
    (opencode-test-service-register opencode-test-service "second-password")
    (opencode-request-sync 'get "/api/agent" :timeout 10)
    (should (equal (plist-get opencode-service-connection :password) "second-password"))))

(ert-deftest opencode-test-service-started-when-absent ()
  "Without a registered service, the CLI is asked to start one."
  (opencode-test-with-service
    (delete-file (opencode-test-service-registration opencode-test-service))
    (let ((cli nil)
          (ready nil))
      (cl-letf (((symbol-function 'opencode-service--run-cli)
                 (lambda (arguments on-exit)
                   (setq cli arguments)
                   (opencode-test-service-register opencode-test-service)
                   (funcall on-exit 0 ""))))
        (opencode-service-ensure (lambda (connection) (setq ready connection)))
        (opencode-test-wait-for (lambda () ready))
        (should (equal cli '("service" "start")))))))

(ert-deftest opencode-test-service-other-process ()
  "A registration whose process no longer answers starts a new service."
  (opencode-test-with-service
    (let ((cli nil)
          (failure nil))
      (setf (opencode-test-service-pid opencode-test-service) 1)
      (with-temp-file (opencode-test-service-registration opencode-test-service)
        (insert (json-serialize `((url . ,(opencode-test-service-url opencode-test-service))
                                  (pid . 99) (password . "first-password")))))
      (cl-letf (((symbol-function 'opencode-service--run-cli)
                 (lambda (arguments on-exit) (setq cli arguments) (funcall on-exit 1 "no luck"))))
        (opencode-service-ensure #'ignore (lambda (message) (setq failure message)))
        (opencode-test-wait-for (lambda () failure))
        (should (equal cli '("service" "start")))
        (should (equal failure "opencode service start failed: no luck"))))))

(ert-deftest opencode-test-service-restart-failure ()
  "A failed restart reaches the error callback."
  (opencode-test-with-service
    (let ((failure nil))
      (cl-letf (((symbol-function 'opencode-service--run-cli)
                 (lambda (arguments on-exit)
                   (should (equal arguments '("service" "restart")))
                   (funcall on-exit 1 "service is locked"))))
        (opencode-service-restart #'ignore (lambda (text) (setq failure text)))
        (should (equal failure "opencode service restart failed: service is locked"))))))

(ert-deftest opencode-test-running-sessions ()
  "Running sessions are counted from the service; an absent service counts as none known."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/session/active"
                      (lambda (_) '(200 . ((data . ((ses_a . ((status . "busy")))
                                                     (ses_b . ((status . "busy")))))))))))
    (should (= (opencode-service-running-sessions) 2))
    (delete-file (opencode-test-service-registration opencode-test-service))
    (should-not (opencode-service-running-sessions))))

(ert-deftest opencode-test-event-stream ()
  "Frames are decoded once complete, whatever the chunking; comments are skipped."
  (opencode-test-with-service
    (let ((seen nil)
          (big (make-string (* 2 1024 1024) ?x)))
      (opencode-events-register t (lambda (event) (push event seen)))
      (opencode-events-connect)
      (opencode-test-wait-for (lambda () (assoc-default 'type (car seen))))
      (should (equal (alist-get 'type (car seen)) "server.connected"))
      (opencode-test-send-raw ": heartbeat\n\n")
      (let ((frame (concat "data: " (json-serialize `((type . "test.big") (data (text . ,big))))
                           "\n\n")))
        ;; A frame split mid-way arrives as one event.
        (opencode-test-send-raw (substring frame 0 1000))
        (accept-process-output nil 0.2)
        (opencode-test-send-raw (substring frame 1000)))
      (opencode-test-wait-for (lambda () (equal (alist-get 'type (car seen)) "test.big")) 20)
      (should (= (length (alist-get 'text (alist-get 'data (car seen)))) (length big)))
      (should (= (length seen) 2)))))

(ert-deftest opencode-test-event-stream-reconnects ()
  "A dropped stream reopens and announces itself again."
  (opencode-test-with-service
    (let ((connected 0))
      (opencode-events-register "server.connected" (lambda (_) (cl-incf connected)))
      (opencode-events-connect)
      (opencode-test-wait-for (lambda () (= connected 1)))
      (setq opencode-events--backoff 0.1)
      (opencode-test-close-streams)
      (opencode-test-wait-for (lambda () (= connected 2)) 15)
      (should (opencode-events-connected-p)))))

(provide 'opencode-transport-test)
;;; opencode-transport-test.el ends here
