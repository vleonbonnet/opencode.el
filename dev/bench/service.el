;;; service.el --- Fake OpenCode service for the reconnection benchmark  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Run in a batch Emacs of its own by run.sh, so that the measured Emacs
;; does only a client's work.  It serves the fake service of
;; test/opencode-test-server.el with sessions spread over many
;; locations, each with the recorded catalogs, the model catalog
;; enlarged to a realistic size, and registers it in
;; $BENCH_ROOT/service.json.  Nothing here reaches a real service.

;;; Code:

(require 'opencode-test-server)

(defvar bench-root (file-name-as-directory (file-truename (getenv "BENCH_ROOT")))
  "Scratch directory of the benchmark.")

(defvar bench-directories (string-to-number (or (getenv "BENCH_DIRS") "53"))
  "Number of session locations.")

(defvar bench-sessions (string-to-number (or (getenv "BENCH_SESSIONS") "120"))
  "Number of sessions.")

(defvar bench-model-copies (string-to-number (or (getenv "BENCH_MODEL_COPIES") "5"))
  "How many times the recorded model catalog is repeated, for a realistic size.")

(defun bench-directory (index)
  "Return the truename of location INDEX, created when missing."
  (let ((directory (expand-file-name (format "project-%02d/" index) bench-root)))
    (make-directory directory t)
    (file-truename directory)))

(defun bench-fixture-text (name)
  "Return fixture NAME as JSON text, its placeholders replaced."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat "fixtures/v2/" name) opencode-test-directory))
    (opencode-test-substitute (buffer-string))))

(defun bench-models ()
  "Return the recorded model catalog repeated `bench-model-copies' times, as JSON."
  (let ((models (alist-get 'data (json-parse-string (bench-fixture-text "model.json")
                                                    :object-type 'alist :array-type 'array))))
    (json-serialize
     `((data . ,(apply #'vconcat
                       (cl-loop for copy below bench-model-copies
                                collect (mapcar (lambda (model)
                                                  (if (zerop copy)
                                                      model
                                                    (cons (cons 'id (format "%s-%d" (alist-get 'id model) copy))
                                                          (assq-delete-all 'id (copy-alist model)))))
                                                models))))))))

(defun bench-session-info (index)
  "Return the info of session INDEX."
  (let ((models '(((id . "gpt-6-sol") (providerID . "openai") (variant . "low"))
                  ((id . "gpt-6-luna") (providerID . "openai"))
                  ((id . "gpt-5.5") (providerID . "openai")))))
    `((id . ,(format "ses_ea4d%04xffeBench%010d" index index))
      (projectID . ,(format "project%02d" (% index bench-directories)))
      (agent . ,(if (zerop (% index 3)) "plan" "build"))
      (model . ,(nth (% index 3) models))
      (cost . 0)
      (tokens (input . 0) (output . 0) (reasoning . 0) (cache (read . 0) (write . 0)))
      (time (created . ,(- 1792531211669 (* index 60000))) (updated . ,(- 1792531211669 (* index 60000))))
      (title . ,(format "Bench session %d, a title of a usual length" index))
      (location (directory . ,(directory-file-name (bench-directory (% index bench-directories))))))))

(defun bench-messages (directory)
  "Return the recorded switch transcript, located in DIRECTORY, as JSON.
It has no cursor, so that it is one page."
  (let* ((opencode-test-workspace directory)
         (messages (json-parse-string (bench-fixture-text "messages-ses_ea4d0d207ffeGQzpFYDlTSKWAG.json")
                                      :object-type 'alist :array-type 'array)))
    (json-serialize `((data . ,(alist-get 'data messages)) (cursor . ,(make-hash-table))))))

(defun bench-routes (infos)
  "Return the routes serving session INFOS and the catalogs."
  (let ((agents (bench-fixture-text "agent.json"))
        (models (bench-models))
        (providers (bench-fixture-text "provider.json"))
        (commands (bench-fixture-text "command.json"))
        (routes nil))
    (dolist (info infos)
      (let ((id (alist-get 'id info))
            (messages (bench-messages (alist-get 'directory (alist-get 'location info)))))
        (push (cons (format "/api/session/%s" id) (lambda (_) (cons 200 `((data . ,info))))) routes)
        (push (cons (format "/api/session/%s/message" id) (lambda (_) (cons 200 messages))) routes)
        (dolist (kind '("permission" "form"))
          (push (cons (format "/api/session/%s/%s" id kind) (lambda (_) (cons 200 "{\"data\":[]}"))) routes))))
    (append
     (list (cons "/api/session" (lambda (_) (cons 200 `((data . ,(vconcat infos)) (cursor . ,(make-hash-table))))))
           (cons "/api/session/active" (lambda (_) (cons 200 `((data . ,(make-hash-table))))))
           (cons "/api/agent" (lambda (_) (cons 200 agents)))
           (cons "/api/model" (lambda (_) (cons 200 models)))
           (cons "/api/provider" (lambda (_) (cons 200 providers)))
           (cons "/api/command" (lambda (_) (cons 200 commands))))
     routes)))

(let* ((infos (cl-loop for index below bench-sessions collect (bench-session-info index)))
       ;; A backlog for the many requests a reconnection sends at once:
       ;; the default of 5 drops connections, which curl retries late.
       (server (make-network-process :name "opencode-bench-service" :server 256
                                     :host "127.0.0.1" :service t :family 'ipv4
                                     :noquery t :coding 'binary
                                     :filter (lambda (connection output)
                                               (opencode-test--filter opencode-test-service
                                                                      connection output))))
       (service (opencode-test-service--make
                 :server server :port (process-contact server :service)
                 :password "bench-password" :pid (emacs-pid)
                 :registration (expand-file-name "service.json" bench-root))))
  (setq opencode-test-service service)
  (setf (opencode-test-service-routes service) (bench-routes infos))
  (opencode-test-service-register service)
  (message "bench service on port %d, pid %d" (opencode-test-service-port service) (emacs-pid))
  ;; Heartbeats keep the client's watchdog from dropping a quiet stream.
  (run-at-time 10 10 (lambda () (opencode-test-send-event '((type . "server.heartbeat") (data))))))

(when (getenv "BENCH_LOG")
  (advice-add 'opencode-test--handle :before
              (lambda (_service _connection request)
                (message "%.3f %s %s %s" (float-time) (plist-get request :method) (plist-get request :path)
                         (cdr (assoc "x-opencode-directory" (plist-get request :headers)))))))

(while t
  (accept-process-output nil 1))

;;; service.el ends here
