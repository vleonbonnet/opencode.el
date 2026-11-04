;;; opencode-services-test.el --- Tests of several services at once  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Emacs talks to the local service and to a remote one at once: two
;; fake services over sockets, the local one registered as usual, the
;; other registered by name, reached directly or through a relay as
;; drover reaches another host's.  Each has its own event stream,
;; registry, catalogs and session buffers, its requests go to it, and
;; one failing leaves the other working.

;;; Code:

(require 'opencode-test-server)

(defconst opencode-test-remote-directory "C:/Users/val/proj"
  "The directory of the remote service's sessions, as that host spells it.")

(defvar opencode-test--remote-url nil
  "The URL of the fake remote service of the running test.")

(defun opencode-test-service-info (id title directory)
  "Return the info of session ID, TITLE, in DIRECTORY."
  `((id . ,id) (title . ,title) (location (directory . ,directory))
    (time (created . 1700000000000) (updated . 1700000000000))))

(defun opencode-test-service-routes-for (sessions agent &optional active)
  "Return routes serving SESSIONS, infos by ID, and an agent catalog of AGENT.
ACTIVE, a function of no argument, returns the IDs running a turn."
  (append
   (mapcan (lambda (info)
             (let ((id (alist-get 'id info)))
               (list (cons (format "/api/session/%s" id) (lambda (_) (cons 200 `((data . ,info)))))
                     (cons (format "/api/session/%s/inbox" id) (lambda (_) '(200 . ((data . [])))))
                     (cons (format "/api/session/%s/permission" id) (lambda (_) '(200 . ((data . [])))))
                     (cons (format "/api/session/%s/form" id) (lambda (_) '(200 . ((data . [])))))
                     (cons (format "/api/session/%s/message" id)
                           (lambda (_) '(200 . ((data . []) (cursor))))))))
           sessions)
   (list (cons "/api/session/active"
               (lambda (_)
                 (cons 200 `((data . ,(let ((table (make-hash-table :test #'equal)))
                                        (dolist (id (and active (funcall active)))
                                          (puthash id t table))
                                        (if (zerop (hash-table-count table)) (opencode-empty-object) table)))))))
         (cons "/api/session" (lambda (_) (cons 200 `((data . ,(vconcat sessions)) (cursor)))))
         (cons "/api/agent" (lambda (_) (cons 200 `((data . [((id . ,agent) (mode . "primary"))])))))
         (cons "/api/model" (lambda (_) '(200 . ((data . [])))))
         (cons "/api/provider" (lambda (_) '(200 . ((data . []))))))))

(defun opencode-test-relay-to (url password)
  "Return a relay carrying requests to the fake service at URL, with PASSWORD.
It stands for drover's: the relay adds the credentials, the requests
the relay carries having none."
  (lambda (exchange method path headers body)
    (let* ((stream (opencode-transport-exchange-filter exchange))
           (inner (opencode-transport-request
                   method (concat url path)
                   :headers (cons (cons "Authorization"
                                        (opencode-service-authorization (list :password password)))
                                  (seq-remove (lambda (header) (equal (downcase (car header)) "authorization"))
                                              headers))
                   :body body
                   :filter (and stream (lambda (_ bytes) (opencode-transport-relay-chunk exchange bytes)))
                   :then (lambda (value)
                           (if stream
                               (opencode-transport-relay-end exchange)
                             (opencode-transport-relay-answer exchange 200 value)))
                   :else (lambda (failure)
                           (if (eq (opencode-transport-failure-reason failure) 'http)
                               (opencode-transport-relay-answer exchange (opencode-transport-failure-status failure)
                                                                (opencode-transport-failure-body failure))
                             (opencode-transport-relay-fail exchange (opencode-transport-failure-reason failure)
                                                            (opencode-transport-failure-describe failure)))))))
      (lambda () (opencode-transport-cancel inner)))))

(defmacro opencode-test-with-remote (spec &rest body)
  "Run BODY with a remote service registered, as SPEC tells: (SERVICE FAKE &key relay).
SERVICE is bound to the registered service, named \"core\", FAKE to the
fake service standing for it; with RELAY non-nil, it is reached through
a relay.  The local fake service runs too (`opencode-test-with-service')."
  (declare (indent 1) (debug t))
  (let ((service (nth 0 spec))
        (fake (nth 1 spec))
        (relay (plist-get (nthcdr 2 spec) :relay)))
    `(opencode-test-with-service
       (let* ((,fake (opencode-test-make-service "remote-password"))
              (opencode-session-buffers (make-hash-table :test #'equal))
              (opencode-test--remote-url (opencode-test-service-url ,fake))
              (,service nil))
         (when-let* ((old (opencode-service-get "core")))
           (opencode--service-reset old))
         (when ,relay
           (opencode-transport-register-relay
            "core" (opencode-test-relay-to opencode-test--remote-url "remote-password")))
         (setq ,service
               (opencode-service-register
                "core" :label "Core"
                :connect (let ((url (if ,relay "relay://core" opencode-test--remote-url)))
                           (lambda (callback _errback)
                             (funcall callback (list :url url :password (unless ,relay "remote-password")
                                                     :id "svc_core"))))))
         (unwind-protect (progn ,@body)
           (opencode-service-unregister ,service)
           (dolist (buffer (buffer-list))
             (when (and (buffer-live-p buffer) (buffer-local-value 'opencode-session-id buffer))
               (kill-buffer buffer)))
           (opencode-test-close-service ,fake)
           (opencode-transport-unregister-relay "core")
           (opencode--service-reset ,service)
           (remhash "core" opencode--services))))))

(defun opencode-test-remote-send (fake event)
  "Send EVENT on the streams of FAKE, the remote fake service."
  (let ((opencode-test-service fake))
    (opencode-test-send-event event)))

(defun opencode-test-open-local-and-remote (service fake)
  "Open session ses_both on the local service and on SERVICE, FAKE serving it.
Return the two buffers, local first."
  (setf (opencode-test-service-routes opencode-test-service)
        (opencode-test-service-routes-for (list (opencode-test-service-info "ses_both" "Here" opencode-test-workspace))
                                      "local-agent")
        (opencode-test-service-routes fake)
        (opencode-test-service-routes-for (list (opencode-test-service-info "ses_both" "There" opencode-test-remote-directory))
                                      "core-agent"))
  (let (local remote)
    (opencode-session-open-by-id "ses_both" :no-display t :callback (lambda (_) (setq local (current-buffer))))
    (opencode-with-service service
      (opencode-session-open-by-id "ses_both" :no-display t :callback (lambda (_) (setq remote (current-buffer)))))
    (opencode-test-wait-for (lambda () (and local remote)) 15)
    (list local remote)))

(defun opencode-test-both-streams (service)
  "Wait until the local stream and SERVICE's are open."
  (opencode-test-wait-for (lambda ()
                            (and (opencode-events-connected-p)
                                 (opencode-with-service service (opencode-events-connected-p))))
                          15))

(defun opencode-test-remote-requests (fake path)
  "Return the requests FAKE received on PATH, oldest first."
  (let ((opencode-test-service fake))
    (opencode-test-requests-on path)))

(defun opencode-test-check-two-services (relay)
  "Check two services at once, the remote one through a relay when RELAY."
  (opencode-test-with-remote (service fake :relay relay)
    (pcase-let ((`(,local ,remote) (opencode-test-open-local-and-remote service fake)))
      (opencode-test-both-streams service)
      ;; Buffers keyed per service: one session ID, two buffers.
      (should-not (eq local remote))
      (should (equal (buffer-name local) "*OpenCode: Here*"))
      (should (equal (buffer-name remote) "*OpenCode@Core: There*"))
      (should (eq (gethash "ses_both" opencode-session-buffers) local))
      (should (eq (opencode-with-service service (gethash "ses_both" (opencode-sv opencode-session-buffers)))
                  remote))
      (with-current-buffer remote
        (should (eq (opencode-current-service) service))
        (should (equal opencode-session-location (concat opencode-test-remote-directory "/")))
        (should (equal default-directory (expand-file-name "~/")))
        (should (equal (opencode-location) (concat opencode-test-remote-directory "/"))))
      (with-current-buffer local
        (should (eq (opencode-current-service) opencode--local-service)))
      ;; Requests routed: each to its service, the remote one scoped to
      ;; its directory as that host spells it.
      (should (opencode-test-requests-on "/api/session/ses_both/message"))
      (let ((requests (opencode-test-remote-requests fake "/api/session/ses_both/message")))
        (should requests)
        (should (equal (url-unhex-string (cdr (assoc "x-opencode-directory" (plist-get (car requests) :headers))))
                       opencode-test-remote-directory))
        (should (equal (cdr (assoc "authorization" (plist-get (car requests) :headers)))
                       (opencode-service-authorization '(:password "remote-password")))))
      ;; Two event streams: each one's events reach its own buffers.
      (opencode-test-remote-send fake '((type . "session.renamed")
                                        (data (sessionID . "ses_both") (title . "There renamed"))))
      (opencode-test-wait-for (lambda () (equal (buffer-name remote) "*OpenCode@Core: There renamed*")))
      (should (equal (buffer-name local) "*OpenCode: Here*"))
      (opencode-test-send-event '((type . "session.renamed")
                                  (data (sessionID . "ses_both") (title . "Here renamed"))))
      (opencode-test-wait-for (lambda () (equal (buffer-name local) "*OpenCode: Here renamed*")))
      (should (equal (buffer-name remote) "*OpenCode@Core: There renamed*"))
      ;; Registries per service, the remote one holding its buffer's session.
      (opencode-test-wait-for (lambda () (opencode-with-service service (opencode-state-get "ses_both"))))
      (opencode-test-wait-for (lambda () (opencode-state-get "ses_both")))
      (should (equal (plist-get (opencode-with-service service (opencode-state-get "ses_both")) :directory)
                     (concat opencode-test-remote-directory "/")))
      (should (equal (plist-get (opencode-state-get "ses_both") :directory)
                     (opencode-normalize-directory opencode-test-workspace)))
      ;; Catalogs per service.
      (opencode-catalog-wait 'agent opencode-test-workspace)
      (with-current-buffer remote
        (opencode-catalog-wait 'agent))
      (should (equal (alist-get 'id (car (opencode-catalog 'agent opencode-test-workspace))) "local-agent"))
      (with-current-buffer remote
        (should (equal (alist-get 'id (car (opencode-catalog 'agent))) "core-agent"))
        (should (equal (opencode-default-agent) "core-agent"))))))

(ert-deftest opencode-test-two-services ()
  "The local service and a remote one at once, each with its own state."
  (opencode-test-check-two-services nil))

(ert-deftest opencode-test-two-services-relayed ()
  "The same, the remote service reached through a relay that holds its password."
  (opencode-test-check-two-services t))

(ert-deftest opencode-test-one-service-failing ()
  "A remote service going away leaves the local one working, and comes back alone."
  (opencode-test-with-remote (service fake)
    (pcase-let ((`(,local ,remote) (opencode-test-open-local-and-remote service fake)))
      (opencode-test-both-streams service)
      (let ((connections 0))
        (opencode-events-register "server.connected"
                                  (lambda (_) (when (opencode-service-remote-p) (cl-incf connections))))
        ;; The remote service's streams end: it reconnects, the local one stays.
        (let ((local-stream opencode-events--stream))
          (let ((opencode-test-service fake))
            (opencode-test-close-streams))
          (opencode-test-wait-for (lambda () (opencode-with-service service (not (opencode-events-connected-p)))))
          (should (eq opencode-events--stream local-stream))
          (should (opencode-events-connected-p))
          (opencode-test-wait-for (lambda () (opencode-with-service service (opencode-events-connected-p))) 15)
          (opencode-test-wait-for (lambda () (> connections 0)))
          (should (eq opencode-events--stream local-stream)))
        ;; Gone for good: its requests fail, the local service's succeed.
        (opencode-test-close-service fake)
        (let ((outcome nil))
          (with-current-buffer remote
            (opencode-request 'get "/api/agent" :then (lambda (_) (setq outcome 'then))
                              :else (lambda (_) (setq outcome 'else))))
          (opencode-test-wait-for (lambda () outcome) 15)
          (should (eq outcome 'else)))
        (opencode-test-send-event '((type . "session.renamed")
                                    (data (sessionID . "ses_both") (title . "Still here"))))
        (opencode-test-wait-for (lambda () (equal (buffer-name local) "*OpenCode: Still here*")))
        (with-current-buffer local
          (should (opencode-request-sync 'get "/api/agent" :timeout 5)))))))

(ert-deftest opencode-test-remote-registry-follows-buffers ()
  "A remote service's registry holds its buffers' sessions and those around them only."
  (opencode-test-with-remote (service fake)
    (let ((running nil))
      (setf (opencode-test-service-routes opencode-test-service)
            (opencode-test-service-routes-for nil "local-agent")
            (opencode-test-service-routes fake)
            (opencode-test-service-routes-for
             (list (opencode-test-service-info "ses_open" "Open" opencode-test-remote-directory)
                   (opencode-test-service-info "ses_other" "Other" opencode-test-remote-directory)
                   (opencode-test-service-info "ses_busy" "Busy" opencode-test-remote-directory))
             "core-agent" (lambda () running)))
      (setq running '("ses_busy"))
      (opencode-with-service service
        (opencode-session-open-by-id "ses_open" :no-display t))
      (opencode-test-wait-for (lambda () (opencode-with-service service (opencode-state-ready-p))) 15)
      (opencode-test-wait-for (lambda () (opencode-with-service service (opencode-state-loaded-p))) 15)
      (opencode-with-service service
        (should (opencode-state-get "ses_open"))
        (should (opencode-state-get "ses_busy"))
        (should-not (opencode-state-get "ses_other")))
      ;; Not listed: the remote service's sessions come from its host.
      (should-not (opencode-test-remote-requests fake "/api/session"))
      ;; An event of an unrelated session is not followed.
      (opencode-test-remote-send fake '((type . "session.renamed")
                                        (data (sessionID . "ses_other") (title . "Renamed"))))
      ;; A subagent created under the open session is.
      (opencode-test-remote-send fake `((type . "session.created")
                                        (data (sessionID . "ses_child") (parentID . "ses_open")
                                              (title . "Child")
                                              (location (directory . ,opencode-test-remote-directory)))))
      (opencode-test-wait-for (lambda () (opencode-with-service service (opencode-state-get "ses_child"))))
      (accept-process-output nil 0.2)
      (opencode-with-service service
        (should-not (opencode-state-get "ses_other")))
      ;; The local registry knows none of them.
      (should-not (opencode-state-get "ses_open"))
      (should-not (opencode-state-get "ses_child")))))

(ert-deftest opencode-test-service-bound-whatever-the-buffer ()
  "Timers and handlers of a service run for it, whatever buffer is current."
  (opencode-test-with-remote (service fake)
    (pcase-let ((`(,local ,remote) (opencode-test-open-local-and-remote service fake)))
      (opencode-test-both-streams service)
      ;; The local service's events dispatched from a timer while the
      ;; remote buffer is current go to the local buffer.
      (with-current-buffer remote
        (opencode-test-send-event '((type . "session.renamed")
                                    (data (sessionID . "ses_both") (title . "Local only"))))
        (opencode-test-wait-for (lambda () (equal (buffer-name local) "*OpenCode: Local only*"))))
      (should (equal (buffer-name remote) "*OpenCode@Core: There*"))
      ;; And the remote service's while the local buffer is current.
      (with-current-buffer local
        (opencode-test-remote-send fake '((type . "session.renamed")
                                          (data (sessionID . "ses_both") (title . "Remote only"))))
        (opencode-test-wait-for (lambda () (equal (buffer-name remote) "*OpenCode@Core: Remote only*"))))
      (should (equal (buffer-name local) "*OpenCode: Local only*"))
      ;; The mode line of each buffer reads its own service.
      (with-current-buffer remote
        (should (equal (opencode-with-service nil (opencode-location)) default-directory))
        (should (equal (opencode-location) (concat opencode-test-remote-directory "/")))))))

(ert-deftest opencode-test-unregister-and-register-again ()
  "Unregistering a service closes its stream and marks its buffers; registering it again resumes."
  (opencode-test-with-remote (service fake)
    (pcase-let ((`(,_local ,remote) (opencode-test-open-local-and-remote service fake)))
      (opencode-test-both-streams service)
      (opencode-service-unregister service)
      (should (opencode-svc-closed service))
      (should-not (opencode-with-service service (opencode-events-connected-p)))
      (should (opencode-events-connected-p))
      (with-current-buffer remote
        (should (string-match-p "closed" (format "%s" header-line-format)))
        (let ((failure nil))
          (opencode-request 'get "/api/agent" :else (lambda (error) (setq failure error)))
          (opencode-test-wait-for (lambda () failure))
          (should (string-match-p "closed" (opencode-error-message failure)))))
      (should (eq (opencode-service-register "core" :label "Core"
                                             :connect (lambda (callback _)
                                                        (funcall callback (list :url opencode-test--remote-url
                                                                                :password "remote-password"))))
                  service))
      (should-not (opencode-svc-closed service))
      (with-current-buffer remote
        (opencode-events-reopen))
      (opencode-test-wait-for (lambda () (opencode-with-service service (opencode-events-connected-p))) 15)
      (opencode-test-wait-for (lambda () (not (local-variable-p 'header-line-format remote)))))))

(ert-deftest opencode-test-remote-files-not-here ()
  "A remote service's files are its host's: nothing resolves, links nor processes them here."
  (opencode-test-with-remote (service fake)
    (pcase-let ((`(,local ,remote) (opencode-test-open-local-and-remote service fake)))
      (with-current-buffer remote
        (should-not (opencode-service-files-p))
        (should-not (opencode-resolve-file-reference "README.md"))
        (should (equal (opencode-relative-path-for-display (concat opencode-test-remote-directory "/src/a.el"))
                       "src/a.el"))
        (should (equal (opencode-normalize-directory "c:\\Users\\val\\proj") "C:/Users/val/proj/"))
        (should (equal (opencode-directory-name "C:/Users/val/proj/") "C:/Users/val/proj"))
        (should (equal (opencode-directory-name "C:/") "C:/")))
      (with-current-buffer local
        (should (opencode-service-files-p))
        (should (opencode-resolve-file-reference "README.md")))
      (should-not (member (with-current-buffer remote default-directory)
                          (delete (with-current-buffer local default-directory) (opencode-session-roots)))))))

(provide 'opencode-services-test)
;;; opencode-services-test.el ends here
