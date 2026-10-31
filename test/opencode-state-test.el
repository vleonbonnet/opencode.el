;;; opencode-state-test.el --- Tests of the session registry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The registry is bootstrapped against `opencode-test-state-server', a
;; plist standing for what the service knows, and driven by synthetic
;; events shaped like those of fixtures/v2/events.jsonl.  Times are
;; epoch milliseconds counted from `opencode-test-state-epoch'.

;;; Code:

(require 'opencode-test-helpers)

(defvar opencode-test-state-epoch (- (truncate (* 1000 (float-time))) 3600000)
  "Epoch milliseconds the times of these tests count from, an hour ago.
Request times are read from IDs whose clock wraps every 795 days, so
the tests' times stay close to the present.")

(defun opencode-test-state-ms (offset)
  "Return the epoch milliseconds OFFSET after `opencode-test-state-epoch'."
  (+ opencode-test-state-epoch offset))

(defun opencode-test-state-time (offset)
  "Return the registry time, in float seconds, OFFSET milliseconds after the epoch."
  (opencode-time-seconds (opencode-test-state-ms offset)))

(defun opencode-test-state-id (prefix offset)
  "Return an ascending ID with PREFIX created OFFSET milliseconds after the epoch."
  (format "%s%012x%s" prefix (logand (* 4096 (opencode-test-state-ms offset)) #xFFFFFFFFFFFF)
          "AbCdEfGhIjKlMn"))

(cl-defun opencode-test-state-info (id &key title (created 0) (updated created) idle viewed archived
                                       outcome parent)
  "Return the service's info of session ID; times are offsets in milliseconds."
  `((id . ,id) (projectID . "prj_test") (title . ,(or title id))
    ,@(when parent `((parentID . ,parent)))
    (agent . "build") (model (id . "gpt-test") (providerID . "openai") (variant . "low"))
    (cost . 0) (tokens (input . 0) (output . 0) (reasoning . 0) (cache (read . 0) (write . 0)))
    ,@(when outcome `((outcome . ,outcome)))
    (time (created . ,(opencode-test-state-ms created)) (updated . ,(opencode-test-state-ms updated))
          ,@(when idle `((idle . ,(opencode-test-state-ms idle))))
          ,@(when viewed `((viewed . ,(opencode-test-state-ms viewed))))
          ,@(when archived `((archived . ,(opencode-test-state-ms archived)))))
    (location (directory . ,(directory-file-name opencode-test-workspace)))))

(defvar opencode-test-state-server nil
  "What the service knows: a plist of :sessions (infos), :active (IDs),
:permissions, :forms and :messages (alists of session IDs to lists, the
messages newest first), and :on-list, a function called when the
sessions are listed.")

(defun opencode-test-state--messages (request)
  "Answer the message page REQUEST asks for, as the service pages them."
  (let* ((query (plist-get request :query))
         (type (alist-get 'type query))
         (cursor (alist-get 'cursor query))
         (limit (alist-get 'limit query))
         (limit (if (stringp limit) (string-to-number limit) limit))
         (messages (seq-filter (lambda (message) (or (null type) (equal (alist-get 'type message) type)))
                               (alist-get (plist-get request :match)
                                          (plist-get opencode-test-state-server :messages)
                                          nil nil #'equal)))
         (messages (if (equal (alist-get 'order query) "asc") (reverse messages) messages))
         (start (if cursor (string-to-number cursor) 0))
         (end (min (length messages) (+ start limit))))
    (should-not (opencode-test-cursor-with-order-p request))
    `((data . ,(seq-subseq messages (min start end) end))
      (cursor (next . ,(and (< end (length messages)) (number-to-string end)))))))

(defun opencode-test-state-routes ()
  "Return the routes serving `opencode-test-state-server'."
  (cl-flet ((info (id)
              (seq-find (lambda (info) (equal (alist-get 'id info) id))
                        (plist-get opencode-test-state-server :sessions))))
    (list
     (list 'get "\\`/api/session\\'"
           (lambda (request)
             (let* ((query (plist-get request :query))
                    (limit (string-to-number (format "%s" (alist-get 'limit query)))))
               (when-let* ((hook (plist-get opencode-test-state-server :on-list)))
                 (funcall hook))
               (if-let* ((failure (plist-get opencode-test-state-server :list-error)))
                   failure
                 `((data . ,(seq-take
                             (sort (seq-remove (lambda (info)
                                                 (and (equal (alist-get 'archived query) "false")
                                                      (alist-get 'archived (alist-get 'time info))))
                                               (plist-get opencode-test-state-server :sessions))
                                   :key (lambda (info) (alist-get 'updated (alist-get 'time info)))
                                   :reverse t)
                             limit))
                   (cursor))))))
     (list 'get "\\`/api/session/active\\'"
           (lambda (_)
             `((data . ,(mapcar (lambda (id) (cons (intern id) '((type . "running"))))
                                (plist-get opencode-test-state-server :active))))))
     (list 'get "\\`/api/session/\\(ses_[A-Za-z0-9_]+\\)/permission\\'"
           (lambda (request)
             `((data . ,(alist-get (plist-get request :match) (plist-get opencode-test-state-server :permissions)
                                   nil nil #'equal)))))
     (list 'get "\\`/api/session/\\(ses_[A-Za-z0-9_]+\\)/form\\'"
           (lambda (request)
             `((data . ,(alist-get (plist-get request :match) (plist-get opencode-test-state-server :forms)
                                   nil nil #'equal)))))
     (list 'get "\\`/api/session/\\(ses_[A-Za-z0-9_]+\\)/message\\'"
           (lambda (request)
             (if (assoc (plist-get request :match) (plist-get opencode-test-state-server :messages))
                 (opencode-test-state--messages request)
               '((data) (cursor)))))
     (list 'get "\\`/api/session/\\(ses_[A-Za-z0-9_]+\\)\\'"
           (lambda (request)
             (if-let* ((info (info (plist-get request :match))))
                 `((data . ,info))
               `(:error 404 ,(format "Session not found: %s" (plist-get request :match))))))
     (list 'post "/interrupt\\'"
           (lambda (_) `((interrupted . ,(plist-get opencode-test-state-server :interrupted))))))))

(defmacro opencode-test-with-state-server (server &rest body)
  "Run BODY with the service knowing SERVER, a plist like `opencode-test-state-server'."
  (declare (indent 1) (debug t))
  `(opencode-test-with-server
     (let ((opencode-test-state-server ,server))
       (let ((opencode-test-routes (append (opencode-test-state-routes) opencode-test-routes)))
         ,@body))))

(defun opencode-test-state-connect ()
  "Dispatch `server.connected', as every (re)opening of the stream does."
  (opencode-test-dispatch '(((type . "server.connected") (data)))))

(defun opencode-test-state-event (type session offset &rest data)
  "Return an event of TYPE about SESSION created OFFSET milliseconds after the epoch.
DATA are the members of its data besides `sessionID'."
  `((id . ,(opencode-test-state-id "evt_" offset)) (type . ,type)
    (created . ,(opencode-test-state-ms offset))
    (location (directory . ,(directory-file-name opencode-test-workspace)))
    (data (sessionID . ,session) ,@data)))

(defun opencode-test-state-permission (session offset &rest members)
  "Return a permission request of SESSION created at OFFSET, with MEMBERS."
  `((id . ,(opencode-test-state-id "per_" offset)) (sessionID . ,session) ,@members))

(defun opencode-test-state-form (session offset title)
  "Return a form of SESSION created at OFFSET, titled TITLE."
  `((id . ,(opencode-test-state-id "frm_" offset)) (sessionID . ,session) (title . ,title)
    (metadata (kind . "question")) (fields)))

(defun opencode-test-state-requests (&optional method)
  "Return the paths of the requests made, oldest first, only those of METHOD when given."
  (seq-keep (lambda (request)
              (and (or (null method) (eq (plist-get request :method) method))
                   (plist-get request :path)))
            (reverse opencode-test-requests)))

(defvar opencode-test-state-changes nil
  "Status changes reported, newest first, as (ID OLD NEW).")

(defvar opencode-test-state-updates nil
  "Sessions whose entry changed, newest first.")

(defmacro opencode-test-state-recording (&rest body)
  "Run BODY recording the registry's hooks in `opencode-test-state-changes' and `-updates'."
  (declare (indent 0) (debug t))
  `(let ((opencode-test-state-changes nil)
         (opencode-test-state-updates nil)
         (opencode-state-change-functions
          (cons (lambda (id old new) (push (list id old new) opencode-test-state-changes))
                opencode-state-change-functions))
         (opencode-state-updated-functions
          (cons (lambda (id) (push id opencode-test-state-updates))
                opencode-state-updated-functions)))
     ,@body))

(defun opencode-test-state-statuses ()
  "Return the IDs and statuses of the unarchived registry entries, sorted by ID."
  (sort (mapcar (lambda (entry) (list (plist-get entry :id) (opencode-state-status entry)))
                (opencode-state-sessions))
        :key #'car))

;;;; Bootstrap

(ert-deftest opencode-test-identifier-time ()
  "Ascending IDs give their creation time back; others give nothing."
  (let ((now (opencode-test-state-time 60000)))
    (should (equal (opencode-identifier-time (opencode-test-state-id "per_" 25) now)
                   (opencode-test-state-time 25)))
    ;; Recorded: the fixture's permission was asked by the event created then.
    (should (equal (opencode-identifier-time "per_15b2e7905001GsIj4YqbOQcPR0" (opencode-time-seconds 1792531200000))
                   (opencode-time-seconds 1792531134725)))
    (should-not (opencode-identifier-time "per_custom"))
    (should-not (opencode-identifier-time nil))
    (let ((id (opencode-generate-id "frm_")))
      (should (< (abs (- (opencode-identifier-time id) (float-time))) 1)))))

(ert-deftest opencode-test-state-bootstrap ()
  "The registry loads the newest unarchived sessions, the running ones and their requests."
  (let ((user (opencode-test-state-permission "ses_b" 20 '(action . "shell") '(resources "make test\nmake lint")))
        (asked (opencode-test-state-permission "ses_b" 25 '(action . "edit") '(resources "x.el")
                                               '(message . "Edit x.el?")))
        (form (opencode-test-state-form "ses_c" 30 "Questions")))
    (opencode-test-with-state-server
        (list :sessions (list (opencode-test-state-info "ses_a" :created 1 :updated 50 :idle 50 :outcome "succeeded")
                              (opencode-test-state-info "ses_b" :created 2 :updated 45 :idle 5 :viewed 5
                                                        :outcome "succeeded")
                              (opencode-test-state-info "ses_c" :created 0 :updated 1 :parent "ses_b")
                              (opencode-test-state-info "ses_d" :created 3 :updated 60 :archived 60)
                              (opencode-test-state-info "ses_e" :created 4 :updated 40 :idle 40 :viewed 41
                                                        :outcome "failed"))
              :active '("ses_b" "ses_c")
              :permissions `(("ses_b" ,asked ,user))
              :forms `(("ses_c" ,form))
              :messages `(("ses_b"
                           ((type . "assistant") (time (created . ,(opencode-test-state-ms 35)))
                            (retry (attempt . 2) (at . ,(opencode-test-state-ms 40))
                                   (error (type . "provider.overloaded") (message . "Overloaded"))))
                           ((type . "user") (time (created . ,(opencode-test-state-ms 15))))
                           ((type . "agent-switched") (time (created . ,(opencode-test-state-ms 12))))
                           ((type . "idle") (outcome . "succeeded") (time (created . ,(opencode-test-state-ms 5))))
                           ((type . "user") (time (created . ,(opencode-test-state-ms 3)))))
                          ("ses_e"
                           ((type . "idle") (outcome . "failed") (time (created . ,(opencode-test-state-ms 40)))
                            (error (type . "provider") (message . "Provider exploded\nat line 3"))))))
      (let ((opencode-state-recent-limit 3)
            (ready 0))
        (opencode-state-ensure (lambda () (cl-incf ready)))
        (should-not (opencode-state-ready-p))
        (should (= ready 0))
        (should-not opencode-test-requests)
        (opencode-test-state-connect)
        (should (opencode-state-ready-p))
        (should (= ready 1))
        (opencode-state-ensure (lambda () (cl-incf ready)))
        (should (= ready 2))
        (let ((list (car (last opencode-test-requests))))
          (should (equal (plist-get list :path) "/api/session"))
          (should (equal (plist-get list :query) '((archived . "false") (order . "desc") (limit . 3)))))
        ;; The archived session is left out; the running one older than the page is fetched.
        (should (equal (opencode-test-state-statuses)
                       '(("ses_a" done) ("ses_b" waiting) ("ses_c" waiting) ("ses_e" idle))))
        (should (equal (mapcar (lambda (entry) (plist-get entry :id)) (opencode-state-sessions t))
                       '("ses_a" "ses_b" "ses_e" "ses_c")))
        (should (member "/api/session/ses_c" (opencode-test-state-requests)))
        (dolist (id '("ses_a" "ses_e"))
          (should-not (member (format "/api/session/%s/permission" id) (opencode-test-state-requests))))
        (let ((a (opencode-state-get "ses_a")))
          (should (equal (plist-get a :directory) (opencode-normalize-directory opencode-test-workspace)))
          (should (equal (plist-get a :project-id) "prj_test"))
          (should (equal (plist-get a :model) '((id . "gpt-test") (providerID . "openai") (variant . "low"))))
          (should (equal (plist-get a :agent) "build"))
          (should (eq (plist-get a :outcome) 'succeeded))
          (should (equal (plist-get a :idle) (opencode-test-state-time 50)))
          (should (equal (plist-get a :created) (opencode-test-state-time 1)))
          (should-not (plist-get a :running))
          (should (opencode-state-unread-p "ses_a"))
          (should (equal (opencode-state-since "ses_a") (opencode-test-state-time 50))))
        (let ((b (opencode-state-get "ses_b")))
          (should (plist-get b :running))
          ;; Oldest first, timed from their IDs.
          (should (equal (plist-get b :requests)
                         `((:kind permission :id ,(alist-get 'id user) :summary "shell: make test …"
                                  :time ,(opencode-test-state-time 20))
                           (:kind permission :id ,(alist-get 'id asked) :summary "Edit x.el?"
                                  :time ,(opencode-test-state-time 25)))))
          (should (equal (opencode-state-since b) (opencode-test-state-time 20)))
          (should (equal (plist-get b :started) (opencode-test-state-time 15)))
          (should (equal (plist-get b :retry)
                         `(:attempt 2 :at ,(opencode-test-state-time 40) :message "Overloaded"))))
        (let ((c (opencode-state-get "ses_c")))
          (should (equal (plist-get c :parent-id) "ses_b"))
          (should (equal (plist-get c :requests)
                         `((:kind question :id ,(alist-get 'id form) :summary "Questions"
                                  :time ,(opencode-test-state-time 30)))))
          (should-not (plist-get c :started)))
        (let ((e (opencode-state-get "ses_e")))
          (should (eq (plist-get e :outcome) 'failed))
          (should (equal (plist-get e :error) "Provider exploded …"))
          (should-not (opencode-state-unread-p e)))))))

(ert-deftest opencode-test-state-bootstrap-pages-turn-start ()
  "A running turn's start is found past the first page of its transcript."
  (opencode-test-with-state-server
      (list :sessions (list (opencode-test-state-info "ses_long" :updated 1 :idle 1 :outcome "succeeded"))
            :active '("ses_long")
            :messages `(("ses_long"
                         ,@(mapcar (lambda (offset)
                                     `((type . "assistant") (time (created . ,(opencode-test-state-ms offset)))))
                                   (number-sequence 200 100 -1))
                         ((type . "user") (time (created . ,(opencode-test-state-ms 90))))
                         ((type . "idle") (time (created . ,(opencode-test-state-ms 1)))))))
    (opencode-test-state-connect)
    (should (equal (plist-get (opencode-state-get "ses_long") :started) (opencode-test-state-time 90)))
    (should-not (plist-get (opencode-state-get "ses_long") :retry))
    (should (eq (opencode-state-status "ses_long") 'working))
    (should (= (seq-count (lambda (path) (equal path "/api/session/ses_long/message"))
                          (opencode-test-state-requests))
               3))))

(ert-deftest opencode-test-state-follows-only-after-connecting ()
  "Events before `server.connected', or of another stream, leave the registry alone."
  (opencode-test-with-state-server (list :sessions (list (opencode-test-state-info "ses_a")))
    (opencode-test-dispatch (list (opencode-test-state-event "session.execution.started" "ses_a" 10)))
    (should-not (opencode-state-get "ses_a"))
    (should-not opencode-test-requests)
    (opencode-test-state-connect)
    (should (eq (opencode-state-status "ses_a") 'idle))
    (let ((opencode-events--process 'another-stream))
      (should-not (opencode-state-ready-p))
      (opencode-test-dispatch (list (opencode-test-state-event "session.execution.started" "ses_a" 10))))
    (should (eq (opencode-state-status "ses_a") 'idle))
    (opencode-test-dispatch (list (opencode-test-state-event "session.execution.started" "ses_a" 10)))
    (should (eq (opencode-state-status "ses_a") 'working))))

(ert-deftest opencode-test-state-bootstrap-retries ()
  "A failed bootstrap is reported and retried, by `opencode-state-ensure' or later."
  (opencode-test-with-state-server
      (list :sessions (list (opencode-test-state-info "ses_a"))
            :list-error '(:error 500 "busy"))
    (let ((ready nil)
          (messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest arguments) (push (apply #'format format arguments) messages))))
        (opencode-state-ensure (lambda () (setq ready t)))
        (opencode-test-state-connect))
      (should-not (opencode-state-ready-p))
      (should-not ready)
      (should (string-match-p "cannot load the state of the sessions.*busy" (car messages)))
      (should (timerp opencode-state--retry-timer))
      (plist-put opencode-test-state-server :list-error nil)
      (opencode-state-ensure)
      (should (opencode-state-ready-p))
      (should ready)
      (should-not opencode-state--retry-timer)
      (should (opencode-state-get "ses_a")))))

;;;; Statuses

(defun opencode-test-state-running-session ()
  "Bootstrap a registry holding the idle, seen session ses_s."
  (setq opencode-test-state-server
        (list :sessions (list (opencode-test-state-info "ses_s" :updated 5 :idle 5 :viewed 5
                                                        :outcome "succeeded"))))
  (opencode-test-state-connect)
  (should (eq (opencode-state-status "ses_s") 'idle))
  (setq opencode-test-requests nil))

(ert-deftest opencode-test-state-transitions ()
  "Every event moves the status as the service's projection does, and the hooks follow."
  (opencode-test-with-state-server nil
    (opencode-test-state-running-session)
    (opencode-test-state-recording
      (let ((permission (opencode-test-state-permission "ses_s" 110 '(action . "shell") '(resources "ls")))
            (first-form (opencode-test-state-form "ses_s" 130 "Pick one"))
            (second-form (opencode-test-state-form "ses_s" 150 "Pick again")))
        (cl-flet ((step (event status &optional check)
                    (opencode-test-dispatch (list event))
                    (should (eq (opencode-state-status "ses_s") status))
                    (when check (funcall check (opencode-state-get "ses_s")))))
          (step (opencode-test-state-event "session.execution.started" "ses_s" 100) 'working
                (lambda (entry)
                  (should (equal (opencode-state-since entry) (opencode-test-state-time 100)))))
          (step (opencode-test-state-event "session.retry.scheduled" "ses_s" 102 '(assistantMessageID . "msg_x")
                                           '(attempt . 1) `(at . ,(opencode-test-state-ms 105))
                                           '(error (type . "provider.overloaded") (message . "Overloaded")))
                'retrying
                (lambda (entry)
                  (should (equal (plist-get entry :retry)
                                 `(:attempt 1 :at ,(opencode-test-state-time 105) :message "Overloaded")))
                  (should (equal (opencode-state-since entry) (opencode-test-state-time 100)))))
          (step (opencode-test-state-event "session.step.started" "ses_s" 106 '(assistantMessageID . "msg_x"))
                'working
                (lambda (entry)
                  (should-not (plist-get entry :retry))
                  (should (equal (plist-get entry :updated) (opencode-test-state-time 106)))))
          (step `((type . "permission.asked") (created . ,(opencode-test-state-ms 110)) (data . ,permission))
                'waiting
                (lambda (entry)
                  (should (equal (plist-get entry :requests)
                                 `((:kind permission :id ,(alist-get 'id permission) :summary "shell: ls"
                                          :time ,(opencode-test-state-time 110)))))
                  (should (equal (opencode-state-since entry) (opencode-test-state-time 110)))))
          (step (opencode-test-state-event "permission.replied" "ses_s" 120
                                           `(requestID . ,(alist-get 'id permission)) '(reply . "once"))
                'working)
          (step `((type . "form.created") (created . ,(opencode-test-state-ms 130)) (data (form . ,first-form)))
                'waiting
                (lambda (entry) (should (equal (plist-get (car (plist-get entry :requests)) :summary) "Pick one"))))
          (step (opencode-test-state-event "form.replied" "ses_s" 140 `(id . ,(alist-get 'id first-form))) 'working)
          (step `((type . "form.created") (created . ,(opencode-test-state-ms 150)) (data (form . ,second-form)))
                'waiting)
          (step (opencode-test-state-event "form.cancelled" "ses_s" 160 `(id . ,(alist-get 'id second-form)))
                'working)
          (step (opencode-test-state-event "session.execution.succeeded" "ses_s" 200) 'done
                (lambda (entry)
                  (should (equal (plist-get entry :idle) (opencode-test-state-time 200)))
                  (should (eq (plist-get entry :outcome) 'succeeded))
                  (should-not (plist-get entry :started))
                  (should (opencode-state-unread-p entry))
                  (should (equal (opencode-state-since entry) (opencode-test-state-time 200)))))
          (step (opencode-test-state-event "session.viewed" "ses_s" 210 `(idle . ,(opencode-test-state-ms 200)))
                'idle
                (lambda (entry)
                  (should (equal (plist-get entry :viewed) (opencode-test-state-time 200)))
                  (should-not (opencode-state-since entry))))
          (step (opencode-test-state-event "session.execution.started" "ses_s" 250) 'working)
          (step (opencode-test-state-event "session.execution.failed" "ses_s" 300
                                           '(error (type . "provider") (message . "Boom")))
                'failed
                (lambda (entry)
                  (should (equal (plist-get entry :error) "Boom"))
                  (should (eq (plist-get entry :outcome) 'failed))))
          (step (opencode-test-state-event "session.execution.started" "ses_s" 350) 'working)
          (step (opencode-test-state-event "session.execution.interrupted" "ses_s" 400 '(reason . "user")) 'done
                (lambda (entry)
                  (should (eq (plist-get entry :outcome) 'interrupted))
                  (should-not (plist-get entry :error))
                  (should (equal (plist-get entry :idle) (opencode-test-state-time 400)))))
          (step (opencode-test-state-event "session.execution.started" "ses_s" 450) 'working)
          ;; A restarted service resumes the execution.
          (step (opencode-test-state-event "session.execution.interrupted" "ses_s" 500 '(reason . "shutdown"))
                'working
                (lambda (entry)
                  (should (equal (plist-get entry :idle) (opencode-test-state-time 400)))
                  (should (eq (plist-get entry :outcome) 'interrupted))
                  (should (equal (plist-get entry :started) (opencode-test-state-time 450))))))
        (should (equal (reverse opencode-test-state-changes)
                       '(("ses_s" idle working) ("ses_s" working retrying) ("ses_s" retrying working)
                         ("ses_s" working waiting) ("ses_s" waiting working)
                         ("ses_s" working waiting) ("ses_s" waiting working)
                         ("ses_s" working waiting) ("ses_s" waiting working)
                         ("ses_s" working done) ("ses_s" done idle)
                         ("ses_s" idle working) ("ses_s" working failed)
                         ("ses_s" failed working) ("ses_s" working done)
                         ("ses_s" done working))))))))

(ert-deftest opencode-test-state-idle-watermark ()
  "Every end of execution advances the idle time, even within one millisecond."
  (opencode-test-with-state-server nil
    (opencode-test-state-running-session)
    (opencode-test-dispatch
     (list (opencode-test-state-event "session.execution.started" "ses_s" 100)
           (opencode-test-state-event "session.execution.succeeded" "ses_s" 200)
           (opencode-test-state-event "session.execution.started" "ses_s" 200)
           ;; A clock behind, or the same millisecond: one past the last idle time.
           (opencode-test-state-event "session.execution.interrupted" "ses_s" 150 '(reason . "user"))))
    (should (equal (plist-get (opencode-state-get "ses_s") :idle) (opencode-test-state-time 201)))
    (opencode-test-dispatch
     (list (opencode-test-state-event "session.execution.started" "ses_s" 201)
           `((type . "permission.asked") (created . ,(opencode-test-state-ms 201))
             (data . ,(opencode-test-state-permission "ses_s" 201 '(action . "shell") '(resources "ls"))))
           (opencode-test-state-event "session.execution.succeeded" "ses_s" 201)))
    (let ((entry (opencode-state-get "ses_s")))
      (should (equal (plist-get entry :idle) (opencode-test-state-time 202)))
      ;; An interrupted turn's permission goes unanswered: it ends with its execution.
      (should-not (plist-get entry :requests))
      (should (eq (opencode-state-status entry) 'done)))))

;;;; Session lifecycle

(ert-deftest opencode-test-state-lifecycle ()
  "Creation, renames, moves, selections, archiving and deletion update the entries."
  (opencode-test-with-state-server nil
    (opencode-test-state-running-session)
    (opencode-test-state-recording
      (let ((elsewhere (make-temp-file "opencode-moved-" t)))
        (unwind-protect
            (progn
              (opencode-test-dispatch
               (list `((type . "session.created") (created . ,(opencode-test-state-ms 300))
                       (location (directory . ,(directory-file-name opencode-test-workspace)))
                       (data (sessionID . "ses_new") (projectID . "prj_test") (parentID . "ses_s")
                             (location (directory . ,(directory-file-name opencode-test-workspace)))
                             (title . "fresh") (agent . "plan")))))
              (should-not opencode-test-requests)
              (should (equal (car opencode-test-state-changes) '("ses_new" nil idle)))
              (let ((entry (opencode-state-get "ses_new")))
                (should (equal (plist-get entry :title) "fresh"))
                (should (equal (plist-get entry :parent-id) "ses_s"))
                (should (equal (plist-get entry :agent) "plan"))
                (should (equal (plist-get entry :directory) (opencode-normalize-directory opencode-test-workspace)))
                (should (equal (plist-get entry :created) (opencode-test-state-time 300)))
                (should (equal (plist-get entry :updated) (opencode-test-state-time 300))))
              (opencode-test-dispatch
               (list (opencode-test-state-event "session.renamed" "ses_new" 310 '(title . "renamed"))
                     (opencode-test-state-event "session.agent.selected" "ses_new" 320 '(agent . "build"))
                     (opencode-test-state-event "session.model.selected" "ses_new" 330
                                                '(model (id . "other") (providerID . "anthropic")))
                     (opencode-test-state-event "session.moved" "ses_new" 340
                                                `(location (directory . ,elsewhere)) '(projectID . "prj_other"))))
              (let ((entry (opencode-state-get "ses_new")))
                (should (equal (plist-get entry :title) "renamed"))
                (should (equal (plist-get entry :agent) "build"))
                (should (equal (plist-get entry :model) '((id . "other") (providerID . "anthropic"))))
                (should (equal (plist-get entry :directory) (opencode-normalize-directory elsewhere)))
                (should (equal (plist-get entry :project-id) "prj_other"))
                (should (equal (plist-get entry :updated) (opencode-test-state-time 340))))
              ;; Archiving hides the session from the default listing.
              (opencode-test-dispatch (list (opencode-test-state-event "session.archived" "ses_new" 350)))
              (should (equal (plist-get (opencode-state-get "ses_new") :archived) (opencode-test-state-time 350)))
              (should-not (member "ses_new" (mapcar (lambda (entry) (plist-get entry :id)) (opencode-state-sessions))))
              (should (member "ses_new" (mapcar (lambda (entry) (plist-get entry :id)) (opencode-state-sessions t))))
              (opencode-test-dispatch (list (opencode-test-state-event "session.unarchived" "ses_new" 360)))
              (should-not (plist-get (opencode-state-get "ses_new") :archived))
              (should (member "ses_new" (mapcar (lambda (entry) (plist-get entry :id)) (opencode-state-sessions))))
              (setq opencode-test-state-changes nil)
              (opencode-test-dispatch (list (opencode-test-state-event "session.deleted" "ses_new" 370)))
              (should-not (opencode-state-get "ses_new"))
              (should (equal opencode-test-state-changes '(("ses_new" idle nil))))
              (should (equal (car opencode-test-state-updates) "ses_new"))
              ;; Deleting an unknown session fetches nothing.
              (opencode-test-dispatch (list (opencode-test-state-event "session.deleted" "ses_gone" 380)))
              (should-not opencode-test-requests))
          (delete-directory elsewhere t))))))

(ert-deftest opencode-test-state-hooks-only-on-change ()
  "Hooks run when an entry changes, the status hook only when the status does."
  (opencode-test-with-state-server nil
    (opencode-test-state-running-session)
    (opencode-test-state-recording
      (let ((asked `((type . "permission.asked") (created . ,(opencode-test-state-ms 110))
                     (data . ,(opencode-test-state-permission "ses_s" 110 '(action . "shell") '(resources "ls"))))))
        (opencode-test-dispatch (list (opencode-test-state-event "session.execution.started" "ses_s" 100) asked))
        (should (= (length opencode-test-state-updates) 2))
        (should (= (length opencode-test-state-changes) 2))
        ;; The same request again, an older watermark, a renaming to the same title: nothing changes.
        (opencode-test-dispatch
         (list asked
               (opencode-test-state-event "session.viewed" "ses_s" 120 `(idle . ,(opencode-test-state-ms 1)))
               (opencode-test-state-event "permission.replied" "ses_s" 120 '(requestID . "per_unknown"))))
        (should (= (length opencode-test-state-updates) 2))
        ;; A step changes the entry, not the status.
        (opencode-test-dispatch (list (opencode-test-state-event "session.step.started" "ses_s" 130)))
        (should (= (length opencode-test-state-updates) 3))
        (should (= (length opencode-test-state-changes) 2))
        ;; A failing hook function stops neither the others nor the registry.
        (let ((opencode-state-updated-functions (cons (lambda (_) (error "Broken")) opencode-state-updated-functions))
              (debug-on-error nil))
          (opencode-test-dispatch (list (opencode-test-state-event "session.renamed" "ses_s" 140 '(title . "x"))))
          (should (= (length opencode-test-state-updates) 4))
          (should (equal (plist-get (opencode-state-get "ses_s") :title) "x")))))))

;;;; Unknown sessions

(ert-deftest opencode-test-state-unknown-session ()
  "An event of an unknown session fetches it, with what it waits on when running, then applies."
  (let ((permission (opencode-test-state-permission "ses_old" 90 '(action . "read") '(resources "a.txt"))))
    (opencode-test-with-state-server nil
      (opencode-test-state-running-session)
      (setq opencode-test-state-server
            (list :sessions (list (opencode-test-state-info "ses_old" :updated 1 :idle 1 :viewed 1)
                                  (opencode-test-state-info "ses_s" :updated 5 :idle 5 :viewed 5))
                  :active '("ses_old")
                  :permissions `(("ses_old" ,permission))))
      (setq opencode-test-requests nil)
      (opencode-test-state-recording
        (opencode-test-dispatch (list (opencode-test-state-event "session.step.started" "ses_old" 100)))
        (should (equal (seq-take (opencode-test-state-requests) 4)
                       '("/api/session/ses_old" "/api/session/active"
                         "/api/session/ses_old/permission" "/api/session/ses_old/form")))
        (let ((entry (opencode-state-get "ses_old")))
          (should (eq (opencode-state-status entry) 'waiting))
          (should (equal (plist-get entry :updated) (opencode-test-state-time 100)))
          (should (equal (plist-get (car (plist-get entry :requests)) :time) (opencode-test-state-time 90))))
        (should (equal (car (last opencode-test-state-changes)) '("ses_old" nil waiting)))
        ;; A session the service does not have is dropped quietly.
        (setq opencode-test-requests nil)
        (let ((messages nil))
          (cl-letf (((symbol-function 'message) (lambda (&rest arguments) (push arguments messages))))
            (opencode-test-dispatch (list (opencode-test-state-event "session.renamed" "ses_ghost" 110
                                                                     '(title . "boo")))))
          (should-not messages))
        (should (equal (opencode-test-state-requests) '("/api/session/ses_ghost")))
        (should-not (opencode-state-get "ses_ghost"))))))

(ert-deftest opencode-test-state-unknown-session-replay ()
  "Events of a session being fetched wait for it, and one the fetch reflects counts once."
  (opencode-test-with-state-server nil
    (opencode-test-state-running-session)
    (plist-put opencode-test-state-server :sessions
               (cons (opencode-test-state-info "ses_far" :updated 1 :idle 600 :outcome "succeeded")
                     (plist-get opencode-test-state-server :sessions)))
    (let ((routes opencode-test-routes))
      ;; Another event arrives while the session is fetched.
      (let ((opencode-test-routes
             (cons (list 'get "\\`/api/session/ses_far\\'"
                         (lambda (_)
                           (opencode-test-dispatch
                            (list (opencode-test-state-event "session.renamed" "ses_far" 610 '(title . "later"))))
                           (should (gethash "ses_far" opencode-state--fetching))
                           (should-not (opencode-state-get "ses_far"))
                           `((data . ,(car (plist-get opencode-test-state-server :sessions))))))
                   routes)))
        (opencode-test-dispatch (list (opencode-test-state-event "session.execution.succeeded" "ses_far" 600))))
      (let ((entry (opencode-state-get "ses_far")))
        (should (equal (plist-get entry :idle) (opencode-test-state-time 600)))
        (should (equal (plist-get entry :title) "later"))
        (should (eq (opencode-state-status entry) 'done))
        (should-not (gethash "ses_far" opencode-state--fetching))
        ;; Live, the next end of execution counts again.
        (opencode-test-dispatch (list (opencode-test-state-event "session.execution.started" "ses_far" 600)
                                      (opencode-test-state-event "session.execution.succeeded" "ses_far" 600)))
        (should (equal (plist-get (opencode-state-get "ses_far") :idle) (opencode-test-state-time 601)))))))

(ert-deftest opencode-test-state-fork ()
  "A fork, announced by `session.forked' alone, joins the registry."
  (opencode-test-with-state-server nil
    (opencode-test-state-running-session)
    (plist-put opencode-test-state-server :sessions
               (cons (opencode-test-state-info "ses_fork" :title "s (fork #1)" :created 400)
                     (plist-get opencode-test-state-server :sessions)))
    (opencode-test-dispatch
     (list (opencode-test-state-event "session.forked" "ses_fork" 400 '(parentID . "ses_s")
                                      '(boundary (type . "before") (messageID . "msg_x")))))
    (should (equal (car (opencode-test-state-requests)) "/api/session/ses_fork"))
    (should (equal (plist-get (opencode-state-get "ses_fork") :title) "s (fork #1)"))
    (should (eq (opencode-state-status "ses_fork") 'idle))))

;;;; Reconnection

(ert-deftest opencode-test-state-rebootstrap ()
  "Every `server.connected' rebuilds the registry, reporting what changed while away."
  (opencode-test-with-state-server
      (list :sessions (list (opencode-test-state-info "ses_a" :updated 10 :idle 10 :viewed 10)
                            (opencode-test-state-info "ses_b" :updated 20))
            :active '("ses_b")
            :permissions '(("ses_b" ((id . "per_custom") (sessionID . "ses_b")
                                     (action . "shell") (resources "ls")))))
    (opencode-test-state-connect)
    (should (equal (opencode-test-state-statuses) '(("ses_a" idle) ("ses_b" waiting))))
    (let ((asked (plist-get (car (plist-get (opencode-state-get "ses_b") :requests)) :time)))
      (should (< (abs (- asked (float-time))) 5))
      (setq opencode-test-state-server
            (list :sessions (list (opencode-test-state-info "ses_b" :updated 20)
                                  (opencode-test-state-info "ses_f" :updated 30 :idle 30 :outcome "failed"))
                  :active '("ses_b")
                  :permissions (plist-get opencode-test-state-server :permissions)))
      (opencode-test-state-recording
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _) (+ asked 1000))))
          (opencode-test-state-connect))
        (should (opencode-state-ready-p))
        (should (equal (opencode-test-state-statuses) '(("ses_b" waiting) ("ses_f" failed))))
        (should (equal (sort (copy-sequence opencode-test-state-changes) :key #'car)
                       '(("ses_a" idle nil) ("ses_f" nil failed))))
        ;; A request still pending keeps the time it was first seen.
        (should (equal (plist-get (car (plist-get (opencode-state-get "ses_b") :requests)) :time) asked))
        (should-not (member "ses_b" opencode-test-state-updates))))))

(ert-deftest opencode-test-state-events-during-bootstrap ()
  "Events received while bootstrapping are applied over its result, never twice."
  (opencode-test-with-state-server
      (list :sessions (list (opencode-test-state-info "ses_s" :updated 5 :idle 200 :outcome "succeeded"))
            :on-list (lambda ()
                       (should opencode-state--bootstrapping)
                       ;; The list already has this end of execution; the renaming comes after it.
                       (opencode-test-dispatch
                        (list (opencode-test-state-event "session.execution.succeeded" "ses_s" 200)
                              (opencode-test-state-event "session.renamed" "ses_s" 210 '(title . "renamed"))))
                       (should-not (opencode-state-get "ses_s"))))
    (opencode-test-state-connect)
    (let ((entry (opencode-state-get "ses_s")))
      (should (equal (plist-get entry :idle) (opencode-test-state-time 200)))
      (should (equal (plist-get entry :title) "renamed"))
      (should (eq (opencode-state-status entry) 'done)))
    (should-not opencode-state--held)))

;;;; Actions

(ert-deftest opencode-test-state-view ()
  "Viewing posts the idle watermark of an unread session, once, and nothing otherwise."
  (opencode-test-with-state-server
      (list :sessions (list (opencode-test-state-info "ses_u" :updated 10 :idle 10 :outcome "succeeded")
                            (opencode-test-state-info "ses_r" :updated 9 :idle 9 :viewed 9)))
    (opencode-test-state-connect)
    (setq opencode-test-requests nil)
    (opencode-session-view "ses_r")
    (should-not opencode-test-requests)
    (opencode-session-view "ses_unknown")
    (should-not opencode-test-requests)
    (let ((opencode-test-routes
           (cons (list 'post "/view\\'"
                       (lambda (_)
                         ;; Selected again before the service answers: no second request.
                         (opencode-session-view "ses_u")
                         nil))
                 opencode-test-routes)))
      (opencode-session-view "ses_u"))
    (should (equal (mapcar (lambda (request) (list (plist-get request :method) (plist-get request :path)
                                                   (plist-get request :body)))
                           opencode-test-requests)
                   `((post "/api/session/ses_u/view" ((idle . ,(opencode-test-state-ms 10)))))))
    (should-not (gethash "ses_u" opencode-state--viewing))
    (setq opencode-test-requests nil)
    (opencode-session-view "ses_u" (opencode-test-state-time 8))
    (should (equal (plist-get (car opencode-test-requests) :body) `((idle . ,(opencode-test-state-ms 8)))))
    (opencode-test-dispatch (list (opencode-test-state-event "session.viewed" "ses_u" 20
                                                             `(idle . ,(opencode-test-state-ms 10)))))
    (setq opencode-test-requests nil)
    (opencode-session-view "ses_u")
    (should-not opencode-test-requests)))

(ert-deftest opencode-test-state-actions-need-no-buffer ()
  "Archiving, restoring and interrupting go by session ID, scoped to its directory."
  (opencode-test-with-state-server
      (list :sessions (list (opencode-test-state-info "ses_a" :updated 10)))
    (opencode-test-state-connect)
    (setq opencode-test-requests nil)
    (with-temp-buffer
      (setq default-directory temporary-file-directory)
      (let ((directories nil))
        (cl-letf (((symbol-function 'opencode-request)
                   (let ((original (symbol-function 'opencode-request)))
                     (lambda (method path &rest arguments)
                       (push (plist-get arguments :directory) directories)
                       (should (plist-member arguments :buffer))
                       (should-not (plist-get arguments :buffer))
                       (apply original method path arguments)))))
          (opencode-session-archive "ses_a")
          (opencode-session-archive "ses_a" t)
          (let ((messages nil))
            (cl-letf (((symbol-function 'message) (lambda (format &rest arguments)
                                                    (push (apply #'format format arguments) messages))))
              (opencode-session-interrupt "ses_a"))
            (should (equal messages '("Nothing to interrupt")))))
        (should (equal directories (make-list 3 (opencode-normalize-directory opencode-test-workspace))))))
    (should (equal (mapcar (lambda (request) (list (plist-get request :method) (plist-get request :path)
                                                   (plist-get request :body)))
                           (reverse opencode-test-requests))
                   '((patch "/api/session/ses_a" ((archived . t)))
                     (patch "/api/session/ses_a" ((archived . :false)))
                     (post "/api/session/ses_a/interrupt" nil))))))

(ert-deftest opencode-test-state-last-reply ()
  "The last reply is the newest assistant text, cut to the length asked."
  (opencode-test-with-state-server
      (list :messages `(("ses_a"
                         ((type . "assistant") (content ((type . "tool") (id . "call_1") (name . "read"))))
                         ((type . "user") (text . "hi"))
                         ,@(make-list 25 '((type . "assistant") (content ((type . "reasoning") (text . "hmm"))))))
                        ("ses_b"
                         ((type . "assistant") (content ((type . "text") (text . "  Done: all tests pass.  "))
                                                        ((type . "text") (text . "Bye."))))
                         ((type . "assistant") (content ((type . "text") (text . "Older")))))
                        ("ses_c"
                         ,@(make-list 30 '((type . "assistant") (content ((type . "tool") (id . "c") (name . "x")))))
                         ((type . "assistant") (content ((type . "text") (text . "Found past a page")))))))
    (let ((results nil))
      (dolist (id '("ses_a" "ses_b" "ses_c" "ses_none"))
        (opencode-session-last-reply id (lambda (text) (push (cons id text) results))))
      (opencode-session-last-reply "ses_b" (lambda (text) (push (cons 'short text) results)) 8)
      (should (equal (reverse results)
                     '(("ses_a") ("ses_b" . "Done: all tests pass.  \nBye.") ("ses_c" . "Found past a page")
                       ("ses_none") (short . "Done: a…"))))
      (let ((pages (seq-filter (lambda (request) (equal (plist-get request :path) "/api/session/ses_c/message"))
                               (reverse opencode-test-requests))))
        (should (= (length pages) 2))
        (should (equal (alist-get 'type (plist-get (cadr pages) :query)) "assistant"))))))

(ert-deftest opencode-test-respond-session-open-buffer ()
  "Responding to a session with a buffer shows it and answers there."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty id))
           (shown nil)
           (answered nil))
      (cl-letf (((symbol-function 'opencode--display)
                 (lambda (buffer no-display other-window)
                   (should-not no-display)
                   (setq shown (list buffer other-window))
                   buffer))
                ((symbol-function 'opencode-respond) (lambda () (setq answered (current-buffer)))))
        (opencode-respond-session id t))
      (should (equal shown (list buffer t)))
      (should (eq answered buffer)))))

(ert-deftest opencode-test-respond-session-opens-buffer ()
  "Responding to a session without a buffer opens it, loads its requests, then answers."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (permission (opencode-test-state-permission id 10 '(action . "shell") '(resources "ls")))
           (opencode-test-routes
            (cons (list 'get "/permission\\'" (lambda (_) `((data ,permission)))) opencode-test-routes))
           (shown nil)
           (answered nil))
      (cl-letf (((symbol-function 'opencode--display)
                 (lambda (buffer no-display other-window)
                   (should-not no-display)
                   (push (list buffer other-window) shown)
                   buffer))
                ((symbol-function 'opencode-respond)
                 (lambda () (setq answered (list (current-buffer)
                                                 (mapcar (lambda (permission) (plist-get permission :id))
                                                         opencode-pending-permissions)
                                                 (copy-sequence shown))))))
        (opencode-respond-session id t)
        (let ((buffer (gethash id opencode-session-buffers)))
          (should (buffer-live-p buffer))
          ;; Shown once loaded, then answered with the session's request queued.
          (should (equal (seq-take answered 2) (list buffer (list (alist-get 'id permission)))))
          (should (member (list buffer t) (nth 2 answered)))))
      ;; A request already answered elsewhere is only reported.
      (kill-buffer (gethash id opencode-session-buffers))
      (let ((messages nil)
            (opencode-test-routes (cdr opencode-test-routes)))
        (cl-letf (((symbol-function 'opencode--display) (lambda (buffer &rest _) buffer))
                  ((symbol-function 'message)
                   (lambda (format &rest arguments) (push (apply #'format format arguments) messages))))
          (opencode-respond-session id))
        (should (member "No pending OpenCode request" messages))))))

;;;; Viewing marks the server

(defun opencode-test-state-viewed ()
  "Return the bodies of the view requests made, oldest first."
  (seq-keep (lambda (request)
              (and (string-suffix-p "/view" (plist-get request :path)) (plist-get request :body)))
            (reverse opencode-test-requests)))

(ert-deftest opencode-test-state-view-on-selection ()
  "Showing an unread session's buffer in the selected window of a focused frame marks it viewed."
  (let* ((id (opencode-test-scenario "text"))
         (info (alist-get 'data (opencode-test-fixture (format "session-%s.json" id))))
         (idle (alist-get 'idle (alist-get 'time info))))
    (opencode-test-with-state-server (list :sessions (list info))
      (opencode-test-state-connect)
      (should (eq (opencode-state-status id) 'done))
      (let ((buffer (opencode-test-open-empty id))
            (window-buffer (window-buffer (selected-window)))
            (focused t))
        (unwind-protect
            (cl-letf (((symbol-function 'opencode-buffer-active-p)
                       (lambda (candidate)
                         (and focused (eq candidate (window-buffer (selected-window)))))))
              (setq opencode-test-requests nil)
              (setq focused nil)
              (set-window-buffer (selected-window) buffer)
              (with-current-buffer buffer (opencode--selection-changed))
              (should-not (opencode-test-state-viewed))
              (setq focused t)
              (with-current-buffer buffer (opencode--selection-changed))
              (should (equal (opencode-test-state-viewed) `(((idle . ,idle)))))
              (should (eq opencode-last-session-buffer buffer))
              ;; Once the service reports it, nothing more is sent.
              (opencode-test-dispatch (list `((type . "session.viewed") (created . ,(1+ idle))
                                              (data (sessionID . ,id) (idle . ,idle)))))
              (should (eq (opencode-state-status id) 'idle))
              (opencode--view-selected-session (selected-frame))
              (should (= (length (opencode-test-state-viewed)) 1)))
          (set-window-buffer (selected-window) window-buffer))))))

(ert-deftest opencode-test-state-view-on-turn-end ()
  "A turn ending while its buffer is active is marked viewed; elsewhere it stays unread."
  (let* ((id (opencode-test-scenario "text"))
         (info (alist-get 'data (opencode-test-fixture (format "session-%s.json" id))))
         (idle (alist-get 'idle (alist-get 'time info))))
    (opencode-test-with-state-server (list :sessions (list info))
      (opencode-test-state-connect)
      (let ((buffer (opencode-test-open-empty id))
            (active nil))
        (cl-letf (((symbol-function 'opencode-buffer-active-p) (lambda (candidate) (and active (eq candidate buffer)))))
          (dolist (shown '(nil t))
            (setq active shown
                  opencode-test-requests nil)
            (opencode-test-dispatch
             (list `((type . "session.execution.started") (created . ,(+ idle (if shown 300 100)))
                     (data (sessionID . ,id)))
                   `((type . "session.execution.succeeded") (created . ,(+ idle (if shown 400 200)))
                     (data (sessionID . ,id)))))
            (if shown
                (should (equal (opencode-test-state-viewed) `(((idle . ,(+ idle 400))))))
              (should-not (opencode-test-state-viewed))
              (should (eq (opencode-state-status id) 'done)))))))))

(provide 'opencode-state-test)
;;; opencode-state-test.el ends here
