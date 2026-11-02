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
;;
;; The $BENCH_OPEN newest sessions, those the client opens, have long
;; transcripts: the first four $BENCH_LONG_TURNS turns of six parts
;; each (a prompt, reasoning, a file read, a shell command with its
;; output, an answer in Markdown and the turn's end), the next eight
;; $BENCH_MEDIUM_TURNS, the others $BENCH_SHORT_TURNS; the rest have the
;; recorded switch transcript.  Transcripts are paged as the service
;; pages them.  POST /api/bench/activity stands for the work agents do
;; while the client is disconnected: a turn is added to half of the open
;; sessions, the long ones included, and a turn starts, or ends, in one
;; more, which /api/session/active then reports running; the second
;; time, a long session's last turn is also reverted and replaced.
;; Three more sessions live in directories that do not exist, for which
;; the catalogs answer 404 LocationNotFoundError.

;;; Code:

(require 'opencode-test-server)
(require 'url-util)

(defvar bench-root (file-name-as-directory (file-truename (getenv "BENCH_ROOT")))
  "Scratch directory of the benchmark.")

(defvar bench-directories (string-to-number (or (getenv "BENCH_DIRS") "53"))
  "Number of session locations.")

(defvar bench-sessions (string-to-number (or (getenv "BENCH_SESSIONS") "120"))
  "Number of sessions.")

(defvar bench-model-copies (string-to-number (or (getenv "BENCH_MODEL_COPIES") "5"))
  "How many times the recorded model catalog is repeated, for a realistic size.")

(defvar bench-open (string-to-number (or (getenv "BENCH_OPEN") "26"))
  "Number of sessions with long transcripts, which the client opens.")

(defvar bench-turns
  (list (string-to-number (or (getenv "BENCH_LONG_TURNS") "400"))
        (string-to-number (or (getenv "BENCH_MEDIUM_TURNS") "60"))
        (string-to-number (or (getenv "BENCH_SHORT_TURNS") "8")))
  "Turns of the long, medium and short transcripts.")

(defvar bench-gone 3
  "Number of sessions living in directories that do not exist.")

(defun bench-directory (index)
  "Return the truename of location INDEX, created when missing."
  (let ((directory (expand-file-name (format "project-%02d/" index) bench-root)))
    (make-directory directory t)
    (file-truename directory)))

(defun bench-gone-directory (index)
  "Return the directory of gone session INDEX, which does not exist."
  (expand-file-name (format "gone-%d" index) bench-root))

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

(defun bench-session-id (index)
  "Return the ID of session INDEX."
  (format "ses_ea4d%04xffeBench%010d" index index))

(defun bench-session-info (index &optional directory)
  "Return the info of session INDEX, in DIRECTORY when given."
  (let ((models '(((id . "gpt-6-sol") (providerID . "openai") (variant . "low"))
                  ((id . "gpt-6-luna") (providerID . "openai"))
                  ((id . "gpt-5.5") (providerID . "openai")))))
    `((id . ,(bench-session-id index))
      (projectID . ,(format "project%02d" (% index bench-directories)))
      (agent . ,(if (zerop (% index 3)) "plan" "build"))
      (model . ,(nth (% index 3) models))
      (cost . 0)
      (tokens (input . 0) (output . 0) (reasoning . 0) (cache (read . 0) (write . 0)))
      (time (created . ,(- 1792531211669 (* index 60000))) (updated . ,(- 1792531211669 (* index 60000))))
      (title . ,(format "Bench session %d, a title of a usual length" index))
      (location (directory . ,(directory-file-name
                               (or directory (bench-directory (% index bench-directories)))))))))

;;;; Transcripts

(defvar bench-transcripts (make-hash-table :test #'equal)
  "Session IDs mapped to their transcripts, oldest first.")

(defvar bench-infos (make-hash-table :test #'equal)
  "Session IDs mapped to their infos.")

(defvar bench-active nil
  "IDs of the sessions running a turn.")

(defun bench-time ()
  "Return the time now, in epoch milliseconds."
  (truncate (* 1000 (float-time))))

(defun bench-turn (session turn &optional running)
  "Return the messages of TURN of SESSION, oldest first.
RUNNING leaves its step running, without its answer nor its end."
  (let* ((key (format "%s%05d" (substring session -6) turn))
         (time (bench-time))
         (table (when (zerop (% turn 7))
                  "\n\n| File | Lines |\n|------|-------|\n| a.el | 12 |\n| b.el | 340 |\n"))
         (step `((id . ,(format "msg_%sa" key)) (type . "assistant") (agent . "build")
                 (model (id . "gpt-6-sol") (providerID . "openai"))
                 (time (created . ,time) ,@(unless running `((completed . ,time))))
                 (tokens (input . 1200) (output . 300) (reasoning . 100) (cache (read . 5000) (write . 0)))
                 (content . ,(vconcat
                              `(((type . "reasoning")
                                 (text . ,(format "**Planning** step %d: read the module, run the checks, then answer. %s"
                                                  turn (make-string 200 ?r))))
                                ((type . "tool") (id . ,(format "call_%sr" key)) (name . "read")
                                 (state (status . "completed") (input (path . "README.md"))
                                        (content . [((type . "text") (text . ,(make-string 1500 ?l)))])))
                                ((type . "tool") (id . ,(format "call_%ss" key)) (name . "shell")
                                 (state (status . ,(if running "running" "completed"))
                                        (input (command . "make check"))
                                        ,@(unless running
                                            `((content . [((type . "text") (text . ,(make-string 600 ?o)))]))))))
                              (unless running
                                `(((type . "text")
                                   (text . ,(format "Turn %d checked:\n\n- `README.md` read\n- see https://example.com/turn/%d\n\n```elisp\n(message \"%d\")\n```%s"
                                                    turn turn turn (or table "")))))))))))
    (append
     (list `((id . ,(format "msg_%su" key)) (type . "user") (text . ,(format "Check part %d of the module" turn))
             (time (created . ,time)))
           step)
     (unless running
       (list `((id . ,(format "msg_%si" key)) (type . "idle") (outcome . "succeeded") (time (created . ,time))))))))

(defun bench-long-turns (index)
  "Return how many turns the transcript of open session INDEX starts with."
  (cond ((< index 4) (nth 0 bench-turns))
        ((< index 12) (nth 1 bench-turns))
        (t (nth 2 bench-turns))))

(defun bench-recorded (directory)
  "Return the recorded switch transcript, located in DIRECTORY, oldest first."
  (let ((opencode-test-workspace directory))
    (append (alist-get 'data (json-parse-string (bench-fixture-text "messages-ses_ea4d0d207ffeGQzpFYDlTSKWAG.json")
                                                :object-type 'alist :array-type 'array))
            nil)))

(defun bench-query (path)
  "Return the query of request PATH as an alist of symbols to strings."
  (when-let* ((start (string-search "?" path)))
    (mapcar (lambda (pair) (cons (intern (car pair)) (cadr pair)))
            (url-parse-query-string (substring path (1+ start))))))

(defun bench-page (messages path)
  "Return the page of MESSAGES, oldest first, that request PATH asks for, as the service does."
  (let* ((query (bench-query path))
         (cursor (alist-get 'cursor query))
         (decoded (and cursor (json-parse-string (base64-decode-string cursor t) :object-type 'alist)))
         (order (if decoded (alist-get 'order decoded) (or (alist-get 'order query) "desc")))
         (limit (and (alist-get 'limit query) (string-to-number (alist-get 'limit query))))
         (ordered (if (equal order "asc") messages (reverse messages)))
         (rest (if decoded
                   (cdr (seq-drop-while (lambda (message) (not (equal (alist-get 'id message) (alist-get 'id decoded))))
                                        ordered))
                 ordered))
         (page (if limit (seq-take rest limit) rest)))
    (cl-flet ((cursor (message direction)
                (base64url-encode-string
                 (json-serialize `((id . ,(alist-get 'id message)) (order . ,order) (direction . ,direction))) t)))
      `((data . ,(vconcat page))
        (cursor . ,(if page
                       `((previous . ,(cursor (car page) "previous")) (next . ,(cursor (car (last page)) "next")))
                     (make-hash-table)))))))

(defun bench-touch (id)
  "Mark session ID updated now."
  (setf (alist-get 'updated (alist-get 'time (gethash id bench-infos))) (bench-time)))

(defvar bench-activity-count 0
  "Count of the activity requests served.")

(defvar bench-busy-index 5
  "Index of the open session whose turn starts and ends with every other activity.")

(defvar bench-revert-index 1
  "Index of the open session, a long one, reverted at the second activity.")

(defun bench-activity ()
  "Add a turn to half of the open sessions, and start or end one in another.
The second time, a long session's last turn is reverted, and a turn
takes its place: what a buffer showed last is gone."
  (cl-incf bench-activity-count)
  (when (= bench-activity-count 2)
    (let ((id (bench-session-id bench-revert-index)))
      (puthash id (append (butlast (gethash id bench-transcripts) 3) (bench-turn id 300000))
               bench-transcripts)
      (bench-touch id)))
  (dotimes (index bench-open)
    (when (and (zerop (% index 2)) (/= index bench-busy-index))
      (let* ((id (bench-session-id index))
             (messages (gethash id bench-transcripts)))
        (puthash id (append messages (bench-turn id (+ 100000 (* 10 bench-activity-count) index))) bench-transcripts)
        (bench-touch id))))
  (let* ((id (bench-session-id bench-busy-index))
         (turn (+ 200000 bench-activity-count))
         (messages (gethash id bench-transcripts)))
    (if (member id bench-active)
        ;; The running step ends, and its turn with it.
        (progn
          (puthash id (append (butlast messages 2) (bench-turn id (1- turn))) bench-transcripts)
          (setq bench-active (delete id bench-active)))
      (puthash id (append messages (bench-turn id turn t)) bench-transcripts)
      (push id bench-active))
    (bench-touch id))
  '((data . t)))

(defun bench-message (service connection request)
  "Answer REQUEST for one message on CONNECTION, or return nil when it is no such request.
SERVICE answers it as the service does: 404 MessageNotFoundError for a
message the session does not have."
  (let ((path (car (split-string (plist-get request :path) "?"))))
    (when (and (string-match "\\`/api/session/\\(ses_[A-Za-z0-9]+\\)/message/\\(msg_[A-Za-z0-9_]+\\)\\'" path)
               (opencode-test--authorized-p service (plist-get request :headers)))
      (push request (opencode-test-service-requests service))
      (let* ((id (match-string 2 path))
             (message (seq-find (lambda (message) (equal (alist-get 'id message) id))
                                (gethash (match-string 1 path) bench-transcripts))))
        (if message
            (opencode-test--respond connection 200 (json-serialize `((data . ,message))))
          (opencode-test--respond connection 404
                                  (json-serialize `((_tag . "MessageNotFoundError")
                                                    (message . ,(format "Message not found: %s" id)))))))
      t)))

(advice-add 'opencode-test--handle :before-until #'bench-message)

;;;; Routes

(defun bench-located-route (gone function)
  "Return a route answering 404 for the directories GONE, else calling FUNCTION."
  (lambda (request)
    (let ((directory (url-unhex-string (or (cdr (assoc "x-opencode-directory" (plist-get request :headers))) ""))))
      (if (member (directory-file-name directory) gone)
          (cons 404 `((_tag . "LocationNotFoundError") (location (directory . ,directory))
                      (message . ,(format "Location not found: %s" directory))))
        (funcall function request)))))

(defun bench-routes (infos gone)
  "Return the routes serving session INFOS and the catalogs, which locations GONE lack."
  (let ((agents (bench-fixture-text "agent.json"))
        (models (bench-models))
        (providers (bench-fixture-text "provider.json"))
        (commands (bench-fixture-text "command.json"))
        (routes nil))
    (dolist (info infos)
      (let ((id (alist-get 'id info)))
        (puthash id info bench-infos)
        (push (cons (format "/api/session/%s" id) (lambda (_) (cons 200 `((data . ,(gethash id bench-infos))))))
              routes)
        (push (cons (format "/api/session/%s/message" id)
                    (lambda (request) (cons 200 (bench-page (gethash id bench-transcripts) (plist-get request :path)))))
              routes)
        (dolist (kind '("permission" "form"))
          (push (cons (format "/api/session/%s/%s" id kind) (lambda (_) (cons 200 "{\"data\":[]}"))) routes))))
    (append
     (list (cons "/api/session" (lambda (_) (cons 200 `((data . ,(vconcat infos)) (cursor . ,(make-hash-table))))))
           (cons "/api/session/active"
                 (lambda (_)
                   (cons 200 `((data . ,(let ((table (make-hash-table :test #'equal)))
                                          (dolist (id bench-active) (puthash id t table))
                                          table))))))
           (cons "/api/bench/activity" (lambda (_) (cons 200 (bench-activity))))
           (cons "/api/agent" (bench-located-route gone (lambda (_) (cons 200 agents))))
           (cons "/api/model" (bench-located-route gone (lambda (_) (cons 200 models))))
           (cons "/api/provider" (bench-located-route gone (lambda (_) (cons 200 providers))))
           (cons "/api/command" (bench-located-route gone (lambda (_) (cons 200 commands)))))
     routes)))

(let* ((infos (cl-loop for index below bench-sessions collect (bench-session-info index)))
       (gone (cl-loop for index below bench-gone collect (bench-gone-directory index)))
       (gone-infos (cl-loop for index below bench-gone
                            collect (bench-session-info (+ bench-sessions index) (nth index gone))))
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
  (dolist (info (append infos gone-infos))
    (let* ((id (alist-get 'id info))
           (index (string-to-number (substring id -10))))
      (puthash id
               (if (< index bench-open)
                   (cl-loop for turn below (bench-long-turns index) append (bench-turn id turn))
                 (bench-recorded (alist-get 'directory (alist-get 'location info))))
               bench-transcripts)))
  (setq opencode-test-service service)
  (setf (opencode-test-service-routes service) (bench-routes (append infos gone-infos) gone))
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
