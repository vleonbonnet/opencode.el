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
                       (url-hexify-string (opencode-directory-name opencode-test-workspace)))))
      ;; Bodies are UTF-8 JSON.
      (let ((response (opencode-request-sync 'post "/api/session" :body '((title . "Café ☕")) :timeout 10)))
        (should (equal (json-parse-string (alist-get 'echo (opencode-request-data response))
                                          :object-type 'alist)
                       '((title . "Café ☕"))))))))

(ert-deftest opencode-test-directory-header-capital-drive ()
  "The directory header spells a Windows drive letter as a capital.
The service keys locations by the exact string, so `c:' and `C:' would be
two locations of one directory."
  (skip-unless (memq system-type '(windows-nt ms-dos)))
  (should (opencode-test-capital-drive-p
           (cdr (opencode-request-directory-header
                 (opencode-test-lowercase-drive opencode-test-workspace))))))

(ert-deftest opencode-test-network-share-uses-its-drive ()
  "A directory on a mapped network share is spelled with the drive it is mapped to.
Imported sessions name a share by its UNC path while `file-truename' of a
link to the drive gives the drive, and the service would key the two as
separate locations."
  (skip-unless (memq system-type '(windows-nt ms-dos)))
  (let ((opencode--network-drives nil))
    (cl-letf (((symbol-function 'w32-read-registry)
               (lambda (root key name)
                 (and (eq root 'HKCU) (equal key "Network\\Q") (equal name "RemotePath")
                      "\\\\SERVER\\Share")))
              ((symbol-function 'file-truename) #'identity))
      (should (equal (opencode-normalize-directory "//server/share/Notes/") "Q:/Notes/"))
      (should (equal (opencode-directory-name "//SERVER/Share/Notes") "Q:/Notes"))
      (should (equal (opencode-normalize-directory "//SERVER/Share") "Q:/"))
      ;; A share no drive is mapped to keeps its UNC name.
      (should (equal (opencode-normalize-directory "//other/share/x/") "//other/share/x/")))))

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

(defun opencode-test-import-route (&rest answers)
  "Return a route answering the import status with ANSWERS in turn, the last one after."
  (cons "/api/experimental/migration/v1"
        (lambda (_) (cons 200 (if (cdr answers) (pop answers) (car answers))))))

(defun opencode-test-import-requests ()
  "Return how many import status requests the fake service received."
  (seq-count (lambda (request) (equal (plist-get request :path) "/api/experimental/migration/v1"))
             (opencode-test-service-requests opencode-test-service)))

(ert-deftest opencode-test-service-waits-for-import ()
  "Requests wait while the service imports the earlier sessions, which look deleted until then."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (opencode-test-import-route
                 '((status . "running") (progress (label . "Clearing old events")))
                 '((status . "running")
                   (progress (label . "Migrating sessions") (numerator . 1) (denominator . 2)))
                 '((status . "completed")))
                (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))))
    (let ((opencode-service--import-interval 0.1)
          (messages nil)
          (answered nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest arguments)
                   (push (apply #'format-message format arguments) messages))))
        (opencode-request 'get "/api/agent" :buffer nil :then (lambda (_) (setq answered t)))
        (opencode-test-wait-for (lambda () answered)))
      (should (= (opencode-test-import-requests) 3))
      ;; The request went out only once the import had completed.
      (should (equal (plist-get (car (opencode-test-service-requests opencode-test-service)) :path)
                     "/api/agent"))
      (should (equal (plist-get (cadr (opencode-test-service-requests opencode-test-service)) :path)
                     "/api/experimental/migration/v1"))
      (should (equal (reverse messages)
                     '("OpenCode service: Clearing old events…"
                       "OpenCode service: Migrating sessions 1/2…"
                       "OpenCode service: earlier sessions imported")))
      ;; Reconnecting to the same service asks no more; a new service is asked again.
      (setq opencode-service-connection nil)
      (opencode-request-sync 'get "/api/agent" :timeout 10)
      (should (= (opencode-test-import-requests) 3))
      (setf (opencode-test-service-pid opencode-test-service) 4343)
      (opencode-test-service-register opencode-test-service)
      (setq opencode-service-connection nil)
      (opencode-request-sync 'get "/api/agent" :timeout 10)
      (should (= (opencode-test-import-requests) 4)))))

(ert-deftest opencode-test-service-import-failed ()
  "A failed import is reported once, and the service is used all the same."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (opencode-test-import-route '((status . "error") (error . "disk full")))
                (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))))
    (let ((warnings nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (type message &rest _) (push (cons type message) warnings))))
        (should (opencode-request-sync 'get "/api/agent" :timeout 10))
        (setq opencode-service-connection nil)
        (should (opencode-request-sync 'get "/api/agent" :timeout 10)))
      (should (equal warnings
                     '((opencode . "The OpenCode service could not import the earlier sessions: disk full"))))
      (should (= (opencode-test-import-requests) 1)))))

(ert-deftest opencode-test-service-import-stalled ()
  "An import that stops making progress is given up on, with a warning."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (opencode-test-import-route
                 '((status . "running")
                   (progress (label . "Migrating sessions") (numerator . 1) (denominator . 9))))
                (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))))
    (let ((opencode-service--import-interval 0.1)
          (opencode-service-import-stall-timeout 0.5)
          (warnings nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (type message &rest _) (push (cons type message) warnings))))
        (should (opencode-request-sync 'get "/api/agent" :timeout 10)))
      (should (>= (opencode-test-import-requests) 4))
      (should (equal (mapcar #'car warnings) '(opencode)))
      (should (string-match-p "made no progress for 0.5 s" (cdar warnings))))))

(ert-deftest opencode-test-service-import-unanswerable ()
  "A service that stops answering during its import fails the waiting requests."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/experimental/migration/v1" (lambda (_) '(500 . ((message . "boom")))))))
    (let ((failure nil))
      (opencode-service-ensure #'ignore (lambda (text) (setq failure text)))
      (opencode-test-wait-for (lambda () failure))
      (should (string-match-p "import of earlier sessions" failure))
      (should-not opencode-service-connection))))

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

(ert-deftest opencode-test-service-stop ()
  "Stopping closes the event stream first, so no reconnect starts a new service."
  (opencode-test-with-service
    (let ((cli nil)
          (wanted 'unknown)
          (stopped nil))
      (opencode-events-connect)
      (opencode-test-wait-for #'opencode-events-connected-p)
      (cl-letf (((symbol-function 'opencode-service--run-cli)
                 (lambda (arguments on-exit)
                   (setq cli arguments
                         wanted opencode-events--wanted)
                   (delete-file (opencode-test-service-registration opencode-test-service))
                   (funcall on-exit 0 ""))))
        (opencode-service-stop (lambda () (setq stopped t)))
        (should stopped)
        (should (equal cli '("service" "stop")))
        (should-not wanted)
        (should-not opencode-service-connection)
        (should-not (opencode-events-connected-p))
        (should-not opencode-events--reconnect-timer)
        ;; Nothing asks for a service once the stream is gone.
        (setq cli nil)
        (accept-process-output nil 1.5)
        (should-not cli)))))

(ert-deftest opencode-test-service-stop-then-start ()
  "A service answering again after a stop reopens the stream while session buffers are open.
Any request starts a service by itself, and open buffers must not go stale."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))))
    (let ((opencode-session-buffers (make-hash-table :test #'equal))
          (buffer (generate-new-buffer " *opencode-test-session*")))
      (unwind-protect
          (progn
            (opencode-events-connect)
            (opencode-test-wait-for #'opencode-events-connected-p)
            ;; The registration stays, as when something starts the service right away.
            (cl-letf (((symbol-function 'opencode-service--run-cli)
                       (lambda (_arguments on-exit) (funcall on-exit 0 ""))))
              (opencode-service-stop #'ignore))
            (should-not (opencode-events-connected-p))
            ;; Without a session buffer, a request leaves the stream closed.
            (opencode-request-sync 'get "/api/agent" :timeout 10)
            (accept-process-output nil 0.5)
            (should-not (opencode-events-connected-p))
            (puthash "ses_test" buffer opencode-session-buffers)
            (setq opencode-service-connection nil)
            (opencode-request-sync 'get "/api/agent" :timeout 10)
            (opencode-test-wait-for #'opencode-events-connected-p))
        (kill-buffer buffer)))))

(ert-deftest opencode-test-disconnect-stays-closed ()
  "A deliberate disconnect stays closed even with session buffers open."
  (opencode-test-with-service
    (setf (opencode-test-service-routes opencode-test-service)
          (list (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))))
    (let ((opencode-session-buffers (make-hash-table :test #'equal))
          (buffer (generate-new-buffer " *opencode-test-session*")))
      (unwind-protect
          (progn
            (puthash "ses_test" buffer opencode-session-buffers)
            (opencode-events-connect)
            (opencode-test-wait-for #'opencode-events-connected-p)
            (opencode-disconnect)
            (opencode-request-sync 'get "/api/agent" :timeout 10)
            (accept-process-output nil 0.5)
            (should-not (opencode-events-connected-p)))
        (kill-buffer buffer)))))

(ert-deftest opencode-test-service-stop-failure ()
  "A failed stop reaches the error callback."
  (opencode-test-with-service
    (let ((failure nil))
      (cl-letf (((symbol-function 'opencode-service--run-cli)
                 (lambda (arguments on-exit)
                   (should (equal arguments '("service" "stop")))
                   (funcall on-exit 1 "access denied"))))
        (opencode-service-stop #'ignore (lambda (text) (setq failure text)))
        (should (equal failure "opencode service stop failed: access denied"))))))

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
