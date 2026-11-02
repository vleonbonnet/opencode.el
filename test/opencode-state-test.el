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
messages newest first), :caches (an alist of session IDs to prompt-cache
expiries), :list-error and :cache-error (the failures of the session and
expiry listings), :on-list and :on-cache, functions called when the
sessions or the expiries are listed, and :defer-pages, non-nil to hold
back the answers to session pages asked by cursor until released.")

(defun opencode-test-state--session-key (info)
  "Return the key the service orders session INFO by: its update time, then its ID."
  (cons (alist-get 'updated (alist-get 'time info)) (alist-get 'id info)))

(defun opencode-test-state--key< (a b)
  "Return non-nil when session key A sorts before B in ascending order."
  (or (< (car a) (car b)) (and (= (car a) (car b)) (string< (cdr a) (cdr b)))))

(defun opencode-test-state--list (query)
  "Answer the session listing QUERY as the service pages it.
Sessions are ordered by update time, then ID; a cursor anchors on a
session's key and carries the first page's filters and order, which a
request with a cursor does not send again."
  (let* ((cursor (and (alist-get 'cursor query) (car (read-from-string (alist-get 'cursor query)))))
         (filter (if cursor (plist-get cursor :filter)
                   (list :archived (alist-get 'archived query) :order (or (alist-get 'order query) "desc"))))
         (anchor (plist-get cursor :anchor))
         (direction (or (plist-get cursor :direction) 'next))
         (limit (string-to-number (format "%s" (alist-get 'limit query))))
         (descending (equal (plist-get filter :order) "desc"))
         ;; The way the rows are read: from the anchor, toward older or newer.
         (forward (if (eq direction 'previous) (not descending) descending))
         (sessions (seq-filter (lambda (info)
                                 (let ((archived (alist-get 'archived (alist-get 'time info))))
                                   (pcase (plist-get filter :archived)
                                     ("false" (not archived))
                                     ("true" archived)
                                     (_ t))))
                               (plist-get opencode-test-state-server :sessions)))
         (rows (sort (seq-filter (lambda (info)
                                   (or (null anchor)
                                       (let ((key (opencode-test-state--session-key info)))
                                         (if forward
                                             (opencode-test-state--key< key anchor)
                                           (opencode-test-state--key< anchor key)))))
                                 sessions)
                     :key #'opencode-test-state--session-key :lessp #'opencode-test-state--key<
                     :reverse forward))
         (data (seq-take rows limit))
         (data (if (eq direction 'previous) (reverse data) data)))
    (when cursor
      (should-not (assq 'archived query))
      (should-not (assq 'order query)))
    (cl-flet ((cursor (info direction)
                (prin1-to-string (list :filter filter :direction direction
                                       :anchor (opencode-test-state--session-key info)))))
      `((data . ,data)
        (cursor ,@(when data
                    `((previous . ,(cursor (car data) 'previous))
                      (next . ,(cursor (car (last data)) 'next)))))))))

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
             (let ((query (plist-get request :query)))
               (when-let* ((hook (plist-get opencode-test-state-server :on-list)))
                 (funcall hook))
               (cond
                ((plist-get opencode-test-state-server :list-error))
                ((and (alist-get 'cursor query) (plist-get opencode-test-state-server :defer-pages))
                 ;; Answered as the service stands when released.
                 (cons :defer (lambda () (opencode-test-state--list query))))
                (t (opencode-test-state--list query))))))
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
           (lambda (_) `((interrupted . ,(plist-get opencode-test-state-server :interrupted)))))
     (list 'get "\\`/api/experimental/session/cache\\'"
           (lambda (_)
             (when-let* ((hook (plist-get opencode-test-state-server :on-cache)))
               (funcall hook))
             (or (plist-get opencode-test-state-server :cache-error)
                 `((data . ,(mapcar (lambda (pair) (cons (intern (car pair)) (cdr pair)))
                                    (plist-get opencode-test-state-server :caches))))))))))

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
  "The registry loads the newest unarchived sessions, the running ones and their requests.
The message of a failure is fetched only while the failure is unseen."
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
                                                        :outcome "failed")
                              (opencode-test-state-info "ses_f" :created 5 :updated 30 :idle 30 :viewed 20
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
                            (error (type . "provider") (message . "Provider exploded\nat line 3"))))
                          ("ses_f"
                           ((type . "idle") (outcome . "failed") (time (created . ,(opencode-test-state-ms 30)))
                            (error (type . "provider") (message . "Rate limited"))))))
      (let ((opencode-state-recent-limit 4)
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
          (should (equal (plist-get list :query) '((archived . "false") (order . "desc") (limit . 4)))))
        ;; The archived session is left out; the running one older than the page is fetched.
        (should (equal (opencode-test-state-statuses)
                       '(("ses_a" done) ("ses_b" waiting) ("ses_c" waiting) ("ses_e" idle) ("ses_f" failed))))
        (should (equal (mapcar (lambda (entry) (plist-get entry :id)) (opencode-state-sessions t))
                       '("ses_a" "ses_b" "ses_e" "ses_f" "ses_c")))
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
          (should-not (plist-get e :error))
          (should-not (opencode-state-unread-p e)))
        (should-not (member "/api/session/ses_e/message" (opencode-test-state-requests)))
        (should (equal (plist-get (opencode-state-get "ses_f") :error) "Rate limited"))))))

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

(ert-deftest opencode-test-state-adopts-earlier-connection ()
  "A registry loaded after the stream connected bootstraps on `opencode-state-ensure'."
  (opencode-test-with-state-server (list :sessions (list (opencode-test-state-info "ses_a")))
    (let ((opencode-events--process 'test-stream))
      (opencode-test-state-connect)
      (should (opencode-state-ready-p))
      ;; Forget the connection, as a registry loaded after it never saw it.
      (setq opencode-state--stream 'none
            opencode-state--ready nil)
      (clrhash opencode-state--sessions)
      (setq opencode-test-requests nil)
      (let ((ready nil))
        (opencode-state-ensure (lambda () (setq ready t)))
        (should ready)
        (should (opencode-state-ready-p))
        (should (eq (opencode-state-status "ses_a") 'idle))
        (should (member "/api/session" (opencode-test-state-requests)))))))

(ert-deftest opencode-test-state-waits-for-greeting ()
  "An open stream not yet greeted by `server.connected' is not adopted early."
  (opencode-test-with-state-server (list :sessions (list (opencode-test-state-info "ses_a")))
    (let ((opencode-events--process 'test-stream)
          (ready nil))
      (opencode-state-ensure (lambda () (setq ready t)))
      (should-not ready)
      (should-not opencode-test-requests)
      (opencode-test-state-connect)
      (should ready))))

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

;;;; Listing every session

(defun opencode-test-state-numbered (count &optional from)
  "Return the infos of COUNT idle sessions, ses_01 the newest, numbered from FROM or 1.
Session N was updated at 1000 - 10N."
  (mapcar (lambda (number)
            (opencode-test-state-info (format "ses_%02d" number) :created number
                                      :updated (- 1000 (* 10 number))))
          (number-sequence (or from 1) (+ (or from 1) count -1))))

(defun opencode-test-state-ids (&optional archived)
  "Return the IDs of the registry's entries, sorted; ARCHIVED as `opencode-state-sessions' takes it."
  (sort (mapcar (lambda (entry) (plist-get entry :id)) (opencode-state-sessions archived)) #'string<))

(defun opencode-test-state-lists ()
  "Return the queries of the session listings asked for, oldest first."
  (seq-keep (lambda (request)
              (when (equal (plist-get request :path) "/api/session")
                (plist-get request :query)))
            (reverse opencode-test-requests)))

(defun opencode-test-state-session-requests ()
  "Return the paths of the requests about one session, oldest first."
  (seq-filter (lambda (path) (string-match-p "\\`/api/session/ses_" path))
              (opencode-test-state-requests)))

(ert-deftest opencode-test-state-lists-every-session ()
  "Every unarchived session is loaded, a page at a time and newest first, by the cursor.
Loading them asks nothing about any session, and an unarchived one
the first page did not hold is neither running nor waiting."
  (let ((opencode-state--page-size 3))
    (opencode-test-with-state-server
        (list :sessions (cons (opencode-test-state-info "ses_arch" :updated 2000 :archived 2000)
                              (opencode-test-state-numbered 8)))
      (should-not (opencode-state-loaded-p))
      (opencode-test-state-connect)
      (should (opencode-state-loaded-p))
      (should (equal (opencode-test-state-ids t) (mapcar (lambda (n) (format "ses_%02d" n)) (number-sequence 1 8))))
      (let ((lists (opencode-test-state-lists)))
        (should (equal (car lists) '((archived . "false") (order . "desc") (limit . 3))))
        ;; Two pages after the first, then the sessions updated since the first.
        (should (= (length lists) 4))
        (dolist (query (cdr lists))
          (should (equal (mapcar #'car query) '(cursor limit)))))
      (should-not (opencode-test-state-session-requests))
      (should (equal (opencode-state-status "ses_08") 'idle)))))

(ert-deftest opencode-test-state-pages-arrive ()
  "The registry is ready with the first page; each later page lands with one notification."
  (let ((opencode-state--page-size 3))
    (opencode-test-with-state-server
        (list :sessions (opencode-test-state-numbered 8) :defer-pages t)
      (opencode-test-state-recording
        (let ((ready nil))
          (opencode-state-ensure (lambda () (setq ready (opencode-test-state-ids))))
          (opencode-test-state-connect)
          ;; The newest first, at once.
          (should (equal ready '("ses_01" "ses_02" "ses_03")))
          (should (opencode-state-ready-p))
          (should-not (opencode-state-loaded-p))
          ;; One notification per page, statuses one by one.
          (should (equal opencode-test-state-updates '(nil)))
          (should (equal (sort (mapcar #'car opencode-test-state-changes) #'string<) ready))
          (setq opencode-test-state-updates nil)
          (should (opencode-test-release))
          (should (equal (opencode-test-state-ids) (mapcar (lambda (n) (format "ses_%02d" n)) (number-sequence 1 6))))
          (should (equal opencode-test-state-updates '(nil)))
          (should (opencode-test-release))
          (should (= (length (opencode-state-sessions)) 8))
          (should (equal opencode-test-state-updates '(nil nil)))
          (should-not (opencode-state-loaded-p))
          ;; The sessions updated since the first page come last; none was.
          (should (opencode-test-release))
          (should (opencode-state-loaded-p))
          (should (equal opencode-test-state-updates '(nil nil)))
          (should-not opencode-test-deferred))))))

(ert-deftest opencode-test-state-pages-meanwhile ()
  "Sessions updated, renamed or deleted while the pages arrive end as the service has them."
  (let ((opencode-state--page-size 3))
    (opencode-test-with-state-server
        (list :sessions (opencode-test-state-numbered 9) :defer-pages t)
      (opencode-test-state-connect)
      (should (equal (opencode-test-state-ids) '("ses_01" "ses_02" "ses_03")))
      (cl-flet ((info (id) (seq-find (lambda (info) (equal (alist-get 'id info) id))
                                     (plist-get opencode-test-state-server :sessions))))
        ;; ses_07 is updated without an event the registry follows, which
        ;; moves it ahead of the first page, out of the pages to come.
        (setf (alist-get 'updated (alist-get 'time (info "ses_07"))) (opencode-test-state-ms 2000))
        (setf (alist-get 'title (info "ses_07")) "moved ahead")
        ;; ses_05 is renamed; the page will tell the name it had.
        (setq opencode-test-requests nil)
        (opencode-test-dispatch (list (opencode-test-state-event "session.renamed" "ses_05" 995 '(title . "renamed"))))
        (should (equal (opencode-test-state-session-requests) '("/api/session/ses_05")))
        (should (equal (plist-get (opencode-state-get "ses_05") :title) "renamed"))
        ;; ses_04 is deleted, after the page holding it was read.
        (opencode-test-dispatch (list (opencode-test-state-event "session.deleted" "ses_04" 996)))
        (should-not (opencode-state-get "ses_04"))
        (while (opencode-test-release))
        (should (opencode-state-loaded-p))
        (should (equal (opencode-test-state-ids)
                       '("ses_01" "ses_02" "ses_03" "ses_05" "ses_06" "ses_07" "ses_08" "ses_09")))
        (should (equal (plist-get (opencode-state-get "ses_05") :title) "renamed"))
        (should (equal (plist-get (opencode-state-get "ses_07") :title) "moved ahead"))
        (should (equal (plist-get (opencode-state-get "ses_07") :updated) (opencode-test-state-time 2000)))))))

(ert-deftest opencode-test-state-limit-option ()
  "A number in `opencode-state-recent-limit' still caps the sessions listed."
  (let ((opencode-state--page-size 3)
        (opencode-state-recent-limit 4))
    (opencode-test-with-state-server
        (list :sessions (opencode-test-state-numbered 8) :active '("ses_07"))
      (opencode-test-state-connect)
      (should (opencode-state-loaded-p))
      (should (equal (mapcar (lambda (query) (alist-get 'limit query)) (opencode-test-state-lists))
                     '(3 1 3)))
      ;; Running sessions join whatever the cap.
      (should (equal (opencode-test-state-ids) '("ses_01" "ses_02" "ses_03" "ses_04" "ses_07"))))
    ;; None but the running ones: the service refuses an empty page.
    (let ((opencode-state-recent-limit 0))
      (opencode-test-with-state-server
          (list :sessions (opencode-test-state-numbered 3) :active '("ses_02"))
        (opencode-test-state-connect)
        (should (opencode-state-loaded-p))
        (should-not (opencode-test-state-lists))
        (should (equal (opencode-test-state-ids) '("ses_02")))))))

(ert-deftest opencode-test-state-listing-asks-little ()
  "Loading sessions fetches the message of unseen failures only, and no context size."
  (let ((opencode-state--page-size 4))
    (opencode-test-with-state-server
        (list :sessions (append (opencode-test-state-numbered 6)
                                (list (opencode-test-state-info "ses_seen" :updated 300 :idle 300 :viewed 300
                                                                :outcome "failed")
                                      (opencode-test-state-info "ses_unseen" :updated 200 :idle 200 :viewed 100
                                                                :outcome "failed")))
              :messages `(("ses_unseen"
                           ((type . "idle") (outcome . "failed") (time (created . ,(opencode-test-state-ms 200)))
                            (error (type . "provider") (message . "Quota exceeded"))))))
      (opencode-test-state-connect)
      (should (opencode-state-loaded-p))
      (should (= (length (opencode-state-sessions)) 8))
      (should (equal (opencode-test-state-session-requests) '("/api/session/ses_unseen/message")))
      (should (eq (opencode-state-status "ses_unseen") 'failed))
      (should (equal (plist-get (opencode-state-get "ses_unseen") :error) "Quota exceeded"))
      (should-not (plist-get (opencode-state-get "ses_seen") :error)))))

(ert-deftest opencode-test-state-page-retried ()
  "A page that fails is asked again later, the registry staying ready meanwhile."
  (let ((opencode-state--page-size 3)
        (failures 1)
        (messages nil))
    (opencode-test-with-state-server
        (list :sessions (opencode-test-state-numbered 5))
      (setq opencode-test-routes
            (cons (list 'get "\\`/api/session\\'"
                        (lambda (request)
                          (if (and (alist-get 'cursor (plist-get request :query)) (> failures 0))
                              (progn (cl-decf failures) '(:error 503 "busy"))
                            (opencode-test-state--list (plist-get request :query)))))
                  opencode-test-routes))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest arguments) (push (apply #'format format arguments) messages))))
        (opencode-test-state-connect))
      (should (opencode-state-ready-p))
      (should-not (opencode-state-loaded-p))
      (should (string-match-p "cannot list the sessions, retrying.*busy" (car messages)))
      (should (timerp opencode-state--list-timer))
      (funcall (timer--function opencode-state--list-timer))
      (should (opencode-state-loaded-p))
      (should (= (length (opencode-state-sessions)) 5)))))

(defun opencode-test-state-open (id)
  "Open the buffer of session ID, as the service knows it, without display."
  (opencode-session-open
   (seq-find (lambda (info) (equal (alist-get 'id info) id)) (plist-get opencode-test-state-server :sessions))
   :no-display t))

(ert-deftest opencode-test-state-keeps-open-sessions ()
  "A session whose buffer is open is in the registry, archived, older or opened later."
  (let ((opencode-state--page-size 3))
    (opencode-test-with-state-server
        (list :sessions (append (list (opencode-test-state-info "ses_arch" :updated 2000 :archived 1500)
                                      (opencode-test-state-info "ses_later" :updated 50 :archived 60))
                                (opencode-test-state-numbered 5)))
      (opencode-test-state-open "ses_arch")
      (opencode-test-state-connect)
      (should (opencode-state-loaded-p))
      (should (opencode-state-get "ses_arch"))
      (should (plist-get (opencode-state-get "ses_arch") :archived))
      ;; Archived, it is listed with the others while its buffer is open.
      (should (member "ses_arch" (opencode-test-state-ids)))
      (should-not (opencode-state-get "ses_later"))
      (setq opencode-test-requests nil)
      (opencode-test-state-open "ses_later")
      (should (equal (seq-take (opencode-test-state-session-requests) 1) '("/api/session/ses_later")))
      (should (opencode-state-get "ses_later"))
      ;; A buffer of a session the registry holds asks nothing more of it.
      (setq opencode-test-requests nil)
      (opencode-test-state-open "ses_02")
      (should-not (member "/api/session/ses_02" (opencode-test-state-requests)))
      ;; Closed, its session is let go at the next rebuild.
      (kill-buffer (gethash "ses_later" opencode-session-buffers))
      (opencode-test-state-connect)
      (should (opencode-state-get "ses_arch"))
      (should-not (opencode-state-get "ses_later")))))

(ert-deftest opencode-test-state-rebuild-keeps-entries ()
  "A rebuild keeps the previous entries until the listing ends, then drops those gone."
  (let ((opencode-state--page-size 3))
    (opencode-test-with-state-server
        (list :sessions (opencode-test-state-numbered 7))
      (opencode-test-state-open "ses_06")
      (opencode-test-state-connect)
      (should (= (length (opencode-state-sessions)) 7))
      ;; While away, ses_05 and ses_06 were archived, and ses_07 renamed.
      (cl-flet ((info (id) (seq-find (lambda (info) (equal (alist-get 'id info) id))
                                     (plist-get opencode-test-state-server :sessions))))
        (dolist (id '("ses_05" "ses_06"))
          (setf (alist-get 'archived (alist-get 'time (info id))) (opencode-test-state-ms 900)))
        (setf (alist-get 'title (info "ses_07")) "renamed away"))
      (plist-put opencode-test-state-server :defer-pages t)
      (opencode-test-state-recording
        (setq opencode-test-requests nil)
        (opencode-test-state-connect)
        (should (opencode-state-ready-p))
        ;; The entries not listed yet stay, but events no longer apply to them.
        (should (equal (opencode-test-state-ids)
                       (mapcar (lambda (n) (format "ses_%02d" n)) (number-sequence 1 7))))
        (should-not opencode-test-state-changes)
        (opencode-test-dispatch (list (opencode-test-state-event "session.agent.selected" "ses_07" 999
                                                                 '(agent . "plan"))))
        (should (member "/api/session/ses_07" (opencode-test-state-requests)))
        (should (equal (plist-get (opencode-state-get "ses_07") :title) "renamed away"))
        (should (equal (plist-get (opencode-state-get "ses_07") :agent) "plan"))
        (while (opencode-test-release))
        (should (opencode-state-loaded-p))
        ;; ses_06 has a buffer: fetched archived, and kept.
        (should (equal (opencode-test-state-ids) '("ses_01" "ses_02" "ses_03" "ses_04" "ses_06" "ses_07")))
        (should (plist-get (opencode-state-get "ses_06") :archived))
        (should-not (opencode-state-get "ses_05"))
        (should (equal opencode-test-state-changes '(("ses_05" idle nil))))))))
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

;;;; Prompt-cache expiry

(defvar opencode-test-state-now (float-time)
  "Float seconds the expiries of these tests count from.")

(defun opencode-test-state-expiry (seconds ttl tokens &optional prompt-tokens)
  "Return a service expiry SECONDS after `opencode-test-state-now', of TTL seconds, covering TOKENS.
PROMPT-TOKENS, when given, is the last request's whole prompt, which
only newer builds of the fork tell."
  `((expires . ,(round (* 1000 (+ opencode-test-state-now seconds)))) (ttl . ,(* 1000 ttl))
    (tokens . ,tokens) ,@(when prompt-tokens `((promptTokens . ,prompt-tokens)))))

(defun opencode-test-state-cache (seconds ttl tokens &optional prompt-tokens)
  "Return the registry's form of `opencode-test-state-expiry' with SECONDS, TTL and TOKENS.
PROMPT-TOKENS is as for `opencode-test-state-expiry'."
  (list :expires (/ (round (* 1000 (+ opencode-test-state-now seconds))) 1000.0)
        :ttl (float ttl) :tokens tokens :prompt-tokens prompt-tokens))

(defun opencode-test-state-cache-event (session &optional expiry)
  "Return a `session.cache.updated' event of SESSION carrying EXPIRY."
  `((id . ,(opencode-generate-id "evt_")) (type . "session.cache.updated")
    (created . ,(round (* 1000 (float-time))))
    (data (sessionID . ,session) ,@(when expiry `((expiry . ,expiry))))))

(defun opencode-test-state-cache-listings ()
  "Return how many times the expiries were listed."
  (seq-count (lambda (path) (equal path "/api/experimental/session/cache"))
             (opencode-test-state-requests)))

(defun opencode-test-state-cache-timer-at ()
  "Return when the expiry timer fires, in float seconds, or nil without one."
  (and (timerp opencode-state--cache-timer)
       (memq opencode-state--cache-timer timer-list)
       (float-time (timer--time opencode-state--cache-timer))))

(defmacro opencode-test-state-at (seconds &rest body)
  "Run BODY with the clock SECONDS after `opencode-test-state-now'."
  (declare (indent 1) (debug t))
  `(let ((now (+ opencode-test-state-now ,seconds))
         (original (symbol-function 'float-time)))
     (cl-letf (((symbol-function 'float-time)
                (lambda (&optional time) (if time (funcall original time) now))))
       ,@body)))

(ert-deftest opencode-test-state-cache-bootstrap ()
  "Connecting lists the expiries, after the sessions, for every session with a live one."
  (let ((opencode-test-state-now (float-time)))
    (opencode-test-with-state-server
        (list :sessions (list (opencode-test-state-info "ses_a" :updated 10))
              :caches `(("ses_a" . ,(opencode-test-state-expiry 300 300 120000))
                        ("ses_old" . ,(opencode-test-state-expiry 3000 3600 8000))
                        ;; Expired by the time it arrives: never known.
                        ("ses_gone" . ,(opencode-test-state-expiry -5 300 100))))
      (opencode-test-state-recording
        (opencode-test-state-at 0
          (should-not (opencode-state-cache "ses_a"))
          (opencode-test-state-connect)
          (should (equal (car (opencode-test-state-requests)) "/api/session"))
          (let ((listing (seq-find (lambda (request)
                                     (equal (plist-get request :path) "/api/experimental/session/cache"))
                                   opencode-test-requests)))
            (should (eq (plist-get listing :method) 'get))
            (should-not (plist-get listing :query)))
          (should (= (opencode-test-state-cache-listings) 1))
          (should (equal (opencode-state-cache "ses_a") (opencode-test-state-cache 300 300 120000)))
          ;; A session the registry does not hold has its expiry too.
          (should-not (opencode-state-get "ses_old"))
          (should (equal (opencode-state-cache "ses_old") (opencode-test-state-cache 3000 3600 8000)))
          (should-not (opencode-state-cache "ses_gone"))
          (should-not (opencode-state-cache "ses_none"))
          (should (< (abs (- (opencode-state-cache-remaining "ses_a") 300)) 0.01))
          (should (opencode-state-cache-supported-p))
          ;; The page of one session reports it; the expiries, several, all at once.
          (should (equal opencode-test-state-updates '(nil "ses_a")))
          ;; The status hook is about statuses only.
          (should-not (assoc "ses_old" opencode-test-state-changes)))
        ;; One timer, set past the earliest expiry.
        (should (< (abs (- (opencode-test-state-cache-timer-at)
                           (+ (plist-get (opencode-test-state-cache 300 300 0) :expires)
                              opencode-state--cache-margin)))
                   0.5))
        ;; Once it passed, it is not returned even before the timer forgets it.
        (opencode-test-state-at 301
          (should-not (opencode-state-cache "ses_a"))
          (should-not (opencode-state-cache-remaining "ses_a")))))))

(ert-deftest opencode-test-state-cache-events ()
  "Each `session.cache.updated' sets or clears its session's expiry and rearms the timer."
  (let ((opencode-test-state-now (float-time)))
    (opencode-test-with-state-server
        (list :sessions (list (opencode-test-state-info "ses_a" :updated 10))
              :caches `(("ses_a" . ,(opencode-test-state-expiry 300 300 1000))))
      (opencode-test-state-connect)
      (opencode-test-state-recording
        ;; A provider response restarts the window and covers more tokens.
        (opencode-test-dispatch (list (opencode-test-state-cache-event
                                       "ses_a" (opencode-test-state-expiry 600 300 2000))))
        (should (equal (opencode-state-cache "ses_a") (opencode-test-state-cache 600 300 2000)))
        (should (equal opencode-test-state-updates '("ses_a")))
        (should (< (abs (- (opencode-test-state-cache-timer-at)
                           (+ opencode-test-state-now 600 opencode-state--cache-margin)))
                   0.5))
        ;; The same expiry again changes nothing.
        (opencode-test-dispatch (list (opencode-test-state-cache-event
                                       "ses_a" (opencode-test-state-expiry 600 300 2000))))
        (should (equal opencode-test-state-updates '("ses_a")))
        ;; A session first heard of by its expiry is not fetched for it.
        (setq opencode-test-requests nil)
        (opencode-test-dispatch (list (opencode-test-state-cache-event
                                       "ses_b" (opencode-test-state-expiry 120 3600 500))))
        (should-not opencode-test-requests)
        (should-not (opencode-state-get "ses_b"))
        (should (equal (opencode-state-cache "ses_b") (opencode-test-state-cache 120 3600 500)))
        (should (< (abs (- (opencode-test-state-cache-timer-at)
                           (+ opencode-test-state-now 120 opencode-state--cache-margin)))
                   0.5))
        ;; Without an expiry, nothing is cached any more.
        (opencode-test-dispatch (list (opencode-test-state-cache-event "ses_b")))
        (should-not (opencode-state-cache "ses_b"))
        (should (equal opencode-test-state-updates '("ses_b" "ses_b" "ses_a")))
        (should (< (abs (- (opencode-test-state-cache-timer-at)
                           (+ opencode-test-state-now 600 opencode-state--cache-margin)))
                   0.5))
        ;; A deleted session's expiry goes with it.
        (opencode-test-dispatch (list (opencode-test-state-event "session.deleted" "ses_a" 20)))
        (should-not (opencode-state-cache "ses_a"))
        (should-not (hash-table-keys opencode-state--caches))
        ;; Nothing live, no timer.
        (should-not opencode-state--cache-timer)
        ;; Events of another stream are left alone.
        (let ((opencode-events--process 'another-stream))
          (opencode-test-dispatch (list (opencode-test-state-cache-event
                                         "ses_c" (opencode-test-state-expiry 60 300 10)))))
        (should-not (opencode-state-cache "ses_c"))
        (should-not opencode-state--cache-timer)))))

(ert-deftest opencode-test-state-cache-prompt-tokens ()
  "An expiry tells the last request's whole prompt, of which its tokens are cached."
  (let ((opencode-test-state-now (float-time)))
    (opencode-test-with-state-server
        (list :sessions (list (opencode-test-state-info "ses_a" :updated 10))
              :caches `(("ses_a" . ,(opencode-test-state-expiry 300 3600 120000 130000))
                        ("ses_b" . ,(opencode-test-state-expiry 300 300 900))))
      (opencode-test-state-connect)
      (should (equal (opencode-state-cache "ses_a") (opencode-test-state-cache 300 3600 120000 130000)))
      (should (= (plist-get (opencode-state-cache "ses_a") :prompt-tokens) 130000))
      ;; A build of the service that does not tell it.
      (should (plist-member (opencode-state-cache "ses_b") :prompt-tokens))
      (should-not (plist-get (opencode-state-cache "ses_b") :prompt-tokens))
      (opencode-test-state-recording
        ;; A new turn sent more, and cached a larger share of it.
        (opencode-test-dispatch (list (opencode-test-state-cache-event
                                       "ses_a" (opencode-test-state-expiry 600 3600 140000 141000))))
        (should (= (plist-get (opencode-state-cache "ses_a") :prompt-tokens) 141000))
        (should (= (plist-get (opencode-state-cache "ses_a") :tokens) 140000))
        (should (equal opencode-test-state-updates '("ses_a")))
        ;; The prompt alone changing is a change.
        (opencode-test-dispatch (list (opencode-test-state-cache-event
                                       "ses_a" (opencode-test-state-expiry 600 3600 140000 150000))))
        (should (= (plist-get (opencode-state-cache "ses_a") :prompt-tokens) 150000))
        (should (equal opencode-test-state-updates '("ses_a" "ses_a")))))))

(ert-deftest opencode-test-state-cache-expires ()
  "The earliest expiry passing forgets it and lists the expiries again, which may hold another."
  (let ((opencode-test-state-now (float-time)))
    (opencode-test-with-state-server
        (list :caches `(("ses_a" . ,(opencode-test-state-expiry 300 300 120000))
                        ("ses_b" . ,(opencode-test-state-expiry 900 3600 4000))))
      (opencode-test-state-connect)
      (should (= (opencode-test-state-cache-listings) 1))
      (let ((timer opencode-state--cache-timer))
        (should (eq (timer--function timer) #'opencode-state--cache-expired))
        ;; Only the conversation's entry lapsed: the system prompt's, cached for an hour, remains.
        (plist-put opencode-test-state-server :caches
                   `(("ses_a" . ,(opencode-test-state-expiry 3000 3600 9000))
                     ("ses_b" . ,(opencode-test-state-expiry 900 3600 4000))))
        (opencode-test-state-recording
          (plist-put opencode-test-state-server :on-cache
                     ;; Forgotten before the service is asked.
                     (lambda () (should-not (gethash "ses_a" opencode-state--caches))))
          (opencode-test-state-at (+ 300 opencode-state--cache-margin)
            (timer-event-handler timer))
          (plist-put opencode-test-state-server :on-cache nil)
          (should (= (opencode-test-state-cache-listings) 2))
          (should (equal (opencode-state-cache "ses_a") (opencode-test-state-cache 3000 3600 9000)))
          (should (equal (opencode-state-cache "ses_b") (opencode-test-state-cache 900 3600 4000)))
          (should (equal opencode-test-state-updates '("ses_a" "ses_a")))
          (should (< (abs (- (opencode-test-state-cache-timer-at)
                             (+ opencode-test-state-now 900 opencode-state--cache-margin)))
                     0.5))
          ;; Listed again when nothing is live any more: no timer.
          (plist-put opencode-test-state-server :caches nil)
          (opencode-test-state-at (+ 900 opencode-state--cache-margin)
            (timer-event-handler opencode-state--cache-timer))
          (should (= (opencode-test-state-cache-listings) 3))
          (should-not (opencode-state-cache "ses_b"))
          (should-not (hash-table-keys opencode-state--caches))
          (should-not opencode-state--cache-timer))))))

(ert-deftest opencode-test-state-cache-expires-offline ()
  "An expiry passing while the stream is down is forgotten without asking the service."
  (let ((opencode-test-state-now (float-time)))
    (opencode-test-with-state-server
        (list :caches `(("ses_a" . ,(opencode-test-state-expiry 300 300 100))
                        ("ses_b" . ,(opencode-test-state-expiry 900 300 100))))
      (opencode-test-state-connect)
      (cl-letf (((symbol-function 'opencode-events-connected-p) #'ignore))
        (opencode-test-state-at (+ 300 opencode-state--cache-margin)
          (timer-event-handler opencode-state--cache-timer)))
      (should (= (opencode-test-state-cache-listings) 1))
      (should-not (gethash "ses_a" opencode-state--caches))
      (should (gethash "ses_b" opencode-state--caches))
      (should (< (abs (- (opencode-test-state-cache-timer-at)
                         (+ opencode-test-state-now 900 opencode-state--cache-margin)))
                 0.5)))))

(ert-deftest opencode-test-state-cache-events-while-listing ()
  "Expiry events received while listing are applied over the answer."
  (let ((opencode-test-state-now (float-time)))
    (opencode-test-with-state-server
        (list :caches `(("ses_a" . ,(opencode-test-state-expiry 300 300 100))
                        ("ses_b" . ,(opencode-test-state-expiry 300 300 100)))
              :on-cache (lambda ()
                          (opencode-test-dispatch
                           (list (opencode-test-state-cache-event
                                  "ses_a" (opencode-test-state-expiry 600 300 200))
                                 (opencode-test-state-cache-event "ses_b")))
                          (should opencode-state--cache-listing)
                          (should-not (opencode-state-cache "ses_a"))))
      (opencode-test-state-connect)
      (should-not opencode-state--cache-listing)
      (should-not opencode-state--cache-held)
      (should (equal (opencode-state-cache "ses_a") (opencode-test-state-cache 600 300 200)))
      (should-not (opencode-state-cache "ses_b")))))

(ert-deftest opencode-test-state-cache-superseded ()
  "A listing superseded by another, such as a reconnection's, is ignored when it answers."
  (let ((opencode-test-state-now (float-time))
        (answers nil))
    (opencode-test-with-state-server nil
      (cl-letf (((symbol-function 'opencode-request)
                 (let ((original (symbol-function 'opencode-request)))
                   (lambda (method path &rest arguments)
                     ;; Answered below, out of order.
                     (if (equal path "/api/experimental/session/cache")
                         (push (plist-get arguments :then) answers)
                       (apply original method path arguments))))))
        (opencode-test-state-connect)
        (opencode-test-state-connect))
      (should (= (length answers) 2))
      (should opencode-state--cache-listing)
      ;; The newer answers first, then the older one arrives late.
      (funcall (car answers) `((data (ses_a . ,(opencode-test-state-expiry 300 300 1)))))
      (funcall (cadr answers) `((data (ses_a . ,(opencode-test-state-expiry 60 300 9)))))
      (should (equal (opencode-state-cache "ses_a") (opencode-test-state-cache 300 300 1)))
      (should-not opencode-state--cache-listing))))

(ert-deftest opencode-test-state-cache-unsupported ()
  "A service without the route is asked once per connection, quietly."
  (let ((opencode-test-state-now (float-time)))
    (opencode-test-with-state-server
        (list :sessions (list (opencode-test-state-info "ses_a" :updated 10))
              :caches `(("ses_a" . ,(opencode-test-state-expiry 300 300 100))))
      (opencode-test-state-connect)
      (should (opencode-state-cache "ses_a"))
      ;; The service restarts as a build without the route: what it cached is gone with it.
      (plist-put opencode-test-state-server :cache-error '(:error 404 "Not Found"))
      (let ((messages nil))
        (opencode-test-state-recording
          (cl-letf (((symbol-function 'message) (lambda (&rest arguments) (push arguments messages))))
            (opencode-test-state-connect))
          (should-not messages)
          (should (opencode-state-ready-p))
          (should-not (opencode-state-cache-supported-p))
          (should-not (opencode-state-cache "ses_a"))
          (should (member "ses_a" opencode-test-state-updates))
          (should-not opencode-state--cache-timer)
          ;; Events are still applied, and their expiry does not ask the service.
          (opencode-test-dispatch (list (opencode-test-state-cache-event
                                         "ses_a" (opencode-test-state-expiry 60 300 5))))
          (should (opencode-state-cache "ses_a"))
          (opencode-test-state-at (+ 60 opencode-state--cache-margin)
            (timer-event-handler opencode-state--cache-timer))
          (should-not (opencode-state-cache "ses_a"))
          (should (= (opencode-test-state-cache-listings) 2))
          (should-not messages)))
      ;; The next connection, maybe to the fork, asks again.
      (plist-put opencode-test-state-server :cache-error nil)
      (opencode-test-state-connect)
      (should (= (opencode-test-state-cache-listings) 3))
      (should (opencode-state-cache-supported-p))
      (should (opencode-state-cache "ses_a")))))

(ert-deftest opencode-test-state-cache-failure ()
  "A failed listing is reported and keeps the expiries known, without retrying at once."
  (let ((opencode-test-state-now (float-time)))
    (opencode-test-with-state-server
        (list :caches `(("ses_a" . ,(opencode-test-state-expiry 300 300 100))
                        ("ses_b" . ,(opencode-test-state-expiry 900 300 100))))
      (opencode-test-state-connect)
      (plist-put opencode-test-state-server :cache-error '(:error 500 "busy"))
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest arguments) (push (apply #'format format arguments) messages))))
          (opencode-test-state-at (+ 300 opencode-state--cache-margin)
            (timer-event-handler opencode-state--cache-timer)))
        (should (equal (length messages) 1))
        (should (string-match-p "cannot list the prompt-cache expiries.*busy" (car messages))))
      (should (= (opencode-test-state-cache-listings) 2))
      (should (opencode-state-cache-supported-p))
      (should-not (gethash "ses_a" opencode-state--caches))
      (should (opencode-state-cache "ses_b"))
      ;; The next expiry to pass lists again.
      (should (< (abs (- (opencode-test-state-cache-timer-at)
                         (+ opencode-test-state-now 900 opencode-state--cache-margin)))
                 0.5)))))

;;;; Context size

(defun opencode-test-state-usage (input &optional output cached)
  "Return a token usage of INPUT, OUTPUT and CACHED read tokens, as the service reports it."
  `((input . ,input) (output . ,(or output 0)) (reasoning . 0)
    (cache (read . ,(or cached 0)) (write . 0))))

(defun opencode-test-state-assistant (offset &optional tokens)
  "Return an assistant message created at OFFSET, reporting usage TOKENS when given."
  `((id . ,(opencode-test-state-id "msg_" offset)) (type . "assistant")
    (time (created . ,(opencode-test-state-ms offset)))
    (agent . "build") (model (id . "gpt-test") (providerID . "openai")) (content)
    ,@(when tokens `((tokens . ,tokens)))))

(defun opencode-test-state-step-ended (session offset tokens &optional type)
  "Return the end of a step of SESSION at OFFSET reporting TOKENS.
TYPE is the event type, `session.step.ended' by default."
  (opencode-test-state-event (or type "session.step.ended") session offset
                             `(assistantMessageID . ,(opencode-test-state-id "msg_" offset))
                             '(finish . "stop") '(cost . 0) `(tokens . ,tokens)))

(defun opencode-test-state-fetch-contexts ()
  "Run the timer starting the queued context-size fetches, as the command loop would."
  (while (timerp opencode-state--context-timer)
    (let ((timer opencode-state--context-timer))
      (cancel-timer timer)
      (apply (timer--function timer) (timer--args timer)))))

(defun opencode-test-state-message-queries (&optional type)
  "Return the message requests made, oldest first, as (SESSION-ID . QUERY).
Only those for messages of TYPE when given: \"assistant\" ones start
every context-size fetch."
  (seq-keep (lambda (request)
              (let ((path (plist-get request :path))
                    (query (plist-get request :query)))
                (when (and (string-match "\\`/api/session/\\([^/]+\\)/message\\'" path)
                           (or (null type) (equal (alist-get 'type query) type)))
                  (cons (match-string 1 path) query))))
            (reverse opencode-test-requests)))

(defvar opencode-test-state-held nil
  "Answers of the message requests held by `opencode-test-state-holding-messages', newest first.")

(defvar opencode-test-state-held-types '("assistant")
  "Message types whose requests `opencode-test-state-holding-messages' holds.
A context-size fetch asks for assistant messages first, so holding
those holds the fetch; the compaction it asks for next is answered at
once unless \"compaction\" is held too.")

(defmacro opencode-test-state-holding-messages (&rest body)
  "Run BODY with message requests held until `opencode-test-state-answer' answers them.
Only requests for the types of `opencode-test-state-held-types' are
held.  A held request is recorded in `opencode-test-requests' when
answered, from what the server knows then.  Other requests are answered
at once."
  (declare (indent 0) (debug t))
  `(let ((answer (symbol-function 'opencode-request))
         (opencode-test-state-held nil))
     (cl-letf (((symbol-function 'opencode-request)
                (lambda (method path &rest arguments)
                  (if (and (string-suffix-p "/message" path)
                           (member (alist-get 'type (plist-get arguments :query))
                                   opencode-test-state-held-types))
                      (push (lambda () (apply answer method path arguments))
                            opencode-test-state-held)
                    (apply answer method path arguments)))))
       ,@body)))

(defun opencode-test-state-answer ()
  "Answer the oldest message request held."
  (let ((oldest (car (last opencode-test-state-held))))
    (setq opencode-test-state-held (butlast opencode-test-state-held))
    (funcall oldest)))

(ert-deftest opencode-test-state-context-fetched-when-asked ()
  "A loaded session's context size is fetched once asked for, from its newest usage."
  (opencode-test-with-state-server
      (list :sessions (list (opencode-test-state-info "ses_a" :updated 30)
                            (opencode-test-state-info "ses_b" :updated 20)
                            (opencode-test-state-info "ses_c" :updated 10))
            :messages `(("ses_a"
                         ;; A step still running reports no usage yet.
                         ,(opencode-test-state-assistant 30)
                         ((type . "user") (time (created . ,(opencode-test-state-ms 25))))
                         ,(opencode-test-state-assistant 20 (opencode-test-state-usage 200 50 1000))
                         ,(opencode-test-state-assistant 10 (opencode-test-state-usage 100)))
                        ("ses_b" ,(opencode-test-state-assistant 20 (opencode-test-state-usage 5000)))
                        ("ses_c" ((type . "user") (time (created . ,(opencode-test-state-ms 5)))))))
    (opencode-test-state-connect)
    ;; The bootstrap costs no request per session.
    (should-not (opencode-test-state-message-queries))
    (should-not (plist-get (opencode-state-get "ses_a") :context))
    (opencode-test-state-recording
      (dolist (id '("ses_a" "ses_a" "ses_b" "ses_c" "ses_unknown"))
        (should-not (opencode-state-context id)))
      ;; Asked from redisplay, the requests wait for a timer.
      (should-not (opencode-test-state-message-queries))
      (should (equal opencode-state--context-queue '("ses_a" "ses_b" "ses_c")))
      (opencode-test-state-fetch-contexts)
      ;; Then the newest compaction, which none of them had.
      (should (equal (opencode-test-state-message-queries)
                     `(("ses_a" (limit . 1) (type . "assistant") (order . "desc"))
                       ("ses_a" (limit . 1) (type . "assistant") (cursor . "1"))
                       ("ses_a" (limit . 1) (type . "compaction") (order . "desc"))
                       ("ses_b" (limit . 1) (type . "assistant") (order . "desc"))
                       ("ses_b" (limit . 1) (type . "compaction") (order . "desc"))
                       ("ses_c" (limit . 1) (type . "assistant") (order . "desc"))
                       ("ses_c" (limit . 1) (type . "compaction") (order . "desc")))))
      (should (equal (opencode-state-context "ses_a") 1250))
      (should (equal (plist-get (opencode-state-get "ses_a") :context) 1250))
      (should (equal (opencode-state-context "ses_b") 5000))
      (should-not (opencode-state-context "ses_c"))
      ;; A session without usage changes nothing.
      (should (equal (sort opencode-test-state-updates) '("ses_a" "ses_b")))
      (should-not opencode-test-state-changes)
      (setq opencode-test-requests nil)
      (dolist (id '("ses_a" "ses_b" "ses_c"))
        (opencode-state-context id))
      (opencode-test-state-fetch-contexts)
      (should-not opencode-test-requests))))

(ert-deftest opencode-test-state-context-fetches-capped ()
  "Context sizes are fetched a few sessions at a time, in the order asked."
  (let ((ids '("ses_1" "ses_2" "ses_3" "ses_4" "ses_5")))
    (opencode-test-with-state-server
        (list :sessions (seq-map-indexed (lambda (id index) (opencode-test-state-info id :updated index)) ids)
              :messages (seq-map-indexed (lambda (id index)
                                           (list id (opencode-test-state-assistant
                                                     index (opencode-test-state-usage (* 1000 (1+ index))))))
                                         ids))
      (opencode-test-state-connect)
      (opencode-test-state-holding-messages
        (let ((opencode-state-context-requests 2))
          (mapc #'opencode-state-context ids)
          (opencode-test-state-fetch-contexts)
          (should (= (length opencode-test-state-held) 2))
          (opencode-test-state-answer)
          (should (equal (opencode-state-context "ses_1") 1000))
          (opencode-test-state-fetch-contexts)
          ;; An answer frees a slot for the next session.
          (should (= (length opencode-test-state-held) 2))
          (while opencode-test-state-held
            (should (<= (length opencode-test-state-held) 2))
            (opencode-test-state-answer)
            (opencode-test-state-fetch-contexts))
          (should (equal (mapcar #'car (opencode-test-state-message-queries "assistant")) ids))
          (should (equal (mapcar #'opencode-state-context ids) '(1000 2000 3000 4000 5000))))))))

(ert-deftest opencode-test-state-context-events ()
  "Step ends keep the context size current without requests; a revert has it fetched again."
  (opencode-test-with-state-server nil
    (opencode-test-state-running-session)
    (plist-put opencode-test-state-server :messages
               `(("ses_s" ,(opencode-test-state-assistant 50 (opencode-test-state-usage 300)))))
    (opencode-test-state-recording
      (opencode-test-dispatch
       (list `((type . "session.created") (created . ,(opencode-test-state-ms 100))
               (data (sessionID . "ses_new") (projectID . "prj_test") (title . "fresh")
                     (location (directory . ,(directory-file-name opencode-test-workspace)))))))
      ;; A new session has no usage, and nothing to fetch.
      (should-not (opencode-state-context "ses_new"))
      (opencode-test-state-fetch-contexts)
      (setq opencode-test-state-updates nil)
      (cl-flet ((step (event context)
                  (opencode-test-dispatch (list event))
                  (should (equal (opencode-state-context (alist-get 'sessionID (alist-get 'data event)))
                                 context))))
        (step (opencode-test-state-step-ended "ses_new" 110 (opencode-test-state-usage 6000 40 2000)) 8040)
        (should (equal opencode-test-state-updates '("ses_new")))
        ;; A step reporting no usage leaves it.
        (step (opencode-test-state-step-ended "ses_new" 120 (opencode-test-state-usage 0)) 8040)
        (step (opencode-test-state-step-ended "ses_new" 130 nil "session.step.failed") 8040)
        ;; A step failing after the provider answered reports its usage.
        (step (opencode-test-state-step-ended "ses_new" 140 (opencode-test-state-usage 9000) "session.step.failed")
              9000)
        (should (equal (plist-get (opencode-state-get "ses_new") :updated) (opencode-test-state-time 140)))
        ;; A loaded session's size known from an event needs no fetch.
        (step (opencode-test-state-step-ended "ses_s" 150 (opencode-test-state-usage 700)) 700))
      (opencode-test-state-fetch-contexts)
      (should-not (opencode-test-state-message-queries))
      ;; A revert drops messages, maybe the newest usage.
      (setq opencode-test-state-updates nil)
      (opencode-test-dispatch
       (list (opencode-test-state-event "session.revert.committed" "ses_s" 160
                                        `(to . ,(opencode-test-state-id "msg_" 140)))))
      (should (equal opencode-test-state-updates '("ses_s")))
      (should-not (opencode-state-context "ses_s"))
      (opencode-test-state-fetch-contexts)
      (should (equal (mapcar #'car (opencode-test-state-message-queries "assistant")) '("ses_s")))
      (should (equal (opencode-state-context "ses_s") 300))
      ;; The size is no status.
      (should (equal opencode-test-state-changes '(("ses_new" nil idle)))))))

(ert-deftest opencode-test-state-context-superseded ()
  "An event settling the context size during its fetch wins over the fetch's answer."
  (opencode-test-with-state-server nil
    (opencode-test-state-running-session)
    (plist-put opencode-test-state-server :messages
               `(("ses_s" ,(opencode-test-state-assistant 50 (opencode-test-state-usage 300)))))
    (opencode-test-state-holding-messages
      (opencode-state-context "ses_s")
      (opencode-test-state-fetch-contexts)
      (should (= (length opencode-test-state-held) 1))
      (opencode-test-dispatch (list (opencode-test-state-step-ended "ses_s" 100 (opencode-test-state-usage 700))))
      (opencode-test-state-answer)
      (should (equal (opencode-state-context "ses_s") 700))
      (opencode-test-state-fetch-contexts)
      (should-not opencode-test-state-held)
      ;; A revert during the fetch fetches again: the answer may predate it.
      (plist-put opencode-test-state-server :messages
                 `(("ses_s" ,(opencode-test-state-assistant 50 (opencode-test-state-usage 100)))))
      (opencode-test-dispatch (list (opencode-test-state-event "session.revert.committed" "ses_s" 110
                                                               '(to . "msg_x"))))
      (opencode-state-context "ses_s")
      (opencode-test-state-fetch-contexts)
      (opencode-test-dispatch (list (opencode-test-state-event "session.revert.committed" "ses_s" 120
                                                               '(to . "msg_y"))))
      (opencode-test-state-fetch-contexts)
      (should (= (length opencode-test-state-held) 2))
      (plist-put opencode-test-state-server :messages
                 `(("ses_s" ,(opencode-test-state-assistant 50 (opencode-test-state-usage 40)))))
      (opencode-test-state-answer)
      (should-not (opencode-state-context "ses_s"))
      (opencode-test-state-answer)
      (should (equal (opencode-state-context "ses_s") 40)))))

(ert-deftest opencode-test-state-context-reconnect ()
  "A reconnection keeps the sizes of sessions not updated since, and fetches the rest when asked."
  (opencode-test-with-state-server
      (list :sessions (list (opencode-test-state-info "ses_a" :updated 10)
                            (opencode-test-state-info "ses_b" :updated 10)
                            (opencode-test-state-info "ses_c" :updated 10)
                            (opencode-test-state-info "ses_d" :updated 10))
            :messages `(("ses_a" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 100)))
                        ("ses_b" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 200)))
                        ("ses_d" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 400)))))
    (opencode-test-state-connect)
    (let ((failing (list 'get "\\`/api/session/ses_c/message\\'" (lambda (_) '(:error 500 "busy")))))
      (let ((opencode-test-routes (cons failing opencode-test-routes)))
        (mapc #'opencode-state-context '("ses_a" "ses_b" "ses_c"))
        (opencode-test-state-fetch-contexts))
      (should (equal (mapcar #'opencode-state-context '("ses_a" "ses_b")) '(100 200)))
      ;; A failure is not retried on every ask.
      (setq opencode-test-requests nil)
      (should-not (opencode-state-context "ses_c"))
      (opencode-test-state-fetch-contexts)
      (should-not opencode-test-requests))
    (opencode-test-state-holding-messages
      ;; Asked while the stream is down, the fetch waits for the reconnection.
      (cl-letf (((symbol-function 'opencode-events-connected-p) #'ignore))
        (opencode-state-context "ses_d")
        (opencode-test-state-fetch-contexts))
      (should-not opencode-test-state-held)
      ;; A step ran in ses_b while the stream was down.
      (setq opencode-test-state-server
            (plist-put opencode-test-state-server :sessions
                       (list (opencode-test-state-info "ses_a" :updated 10)
                             (opencode-test-state-info "ses_b" :updated 50)
                             (opencode-test-state-info "ses_c" :updated 10)
                             (opencode-test-state-info "ses_d" :updated 10))))
      (plist-put opencode-test-state-server :messages
                 `(("ses_a" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 100)))
                   ("ses_b" ,(opencode-test-state-assistant 50 (opencode-test-state-usage 250)))
                   ("ses_c" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 300)))
                   ("ses_d" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 400)))))
      (setq opencode-test-requests nil)
      (opencode-test-state-recording
        (opencode-test-state-connect)
        (should (equal (opencode-state-context "ses_a") 100))
        (should-not (member "ses_a" opencode-test-state-updates))
        (should (member "ses_b" opencode-test-state-updates))
        (should-not (plist-get (opencode-state-get "ses_b") :context))
        (mapc #'opencode-state-context '("ses_a" "ses_b" "ses_c"))
        (opencode-test-state-fetch-contexts)
        (while opencode-test-state-held
          (opencode-test-state-answer)
          (opencode-test-state-fetch-contexts))
        ;; The fetch asked for while down comes first.
        (should (equal (mapcar #'car (opencode-test-state-message-queries "assistant"))
                       '("ses_d" "ses_b" "ses_c")))
        (should (equal (mapcar #'opencode-state-context '("ses_a" "ses_b" "ses_c" "ses_d"))
                       '(100 250 300 400))))
      ;; A fetch in flight when the stream reconnects is made again.
      (plist-put opencode-test-state-server :messages
                 `(("ses_a" ,(opencode-test-state-assistant 60 (opencode-test-state-usage 600)))))
      (opencode-test-dispatch (list (opencode-test-state-event "session.revert.committed" "ses_a" 60
                                                               '(to . "msg_x"))))
      (opencode-state-context "ses_a")
      (opencode-test-state-fetch-contexts)
      (should (= (length opencode-test-state-held) 1))
      (setq opencode-test-requests nil)
      (opencode-test-state-connect)
      (opencode-test-state-fetch-contexts)
      (should (= (length opencode-test-state-held) 2))
      (opencode-test-state-answer)
      (should-not (opencode-state-context "ses_a"))
      (opencode-test-state-answer)
      (should (equal (opencode-state-context "ses_a") 600))
      ;; A deleted session is forgotten.
      (opencode-test-dispatch (list (opencode-test-state-event "session.deleted" "ses_a" 70)))
      (should-not (gethash "ses_a" opencode-state--contexts)))))

(ert-deftest opencode-test-state-context-matches-buffers ()
  "The registry's context size of every recorded session is its buffer's.
Fetched, it is what replaying the transcript finds; followed live, what
the buffer streaming the events finds."
  (dolist (scenario opencode-test-scenarios)
    (let* ((id (opencode-test-scenario scenario))
           (info (alist-get 'data (opencode-test-fixture (format "session-%s.json" id))))
           (server (list :sessions (list info)
                         :messages (list (cons id (reverse (alist-get 'data (opencode-test-fixture
                                                                             (format "messages-%s.json" id)))))))))
      (opencode-test-with-state-server (copy-sequence server)
        (opencode-test-state-connect)
        (let ((replayed (buffer-local-value 'opencode-session-tokens (opencode-test-open id))))
          ;; Only a shell command ran in one; the other was compacted
          ;; last, by a service that did not estimate what that left.
          (should (eq (null replayed) (and (member scenario '("shell-input" "history")) t)))
          (opencode-state-context id)
          (opencode-test-state-fetch-contexts)
          (should (equal (list scenario (opencode-state-context id)) (list scenario replayed)))))
      (opencode-test-with-state-server (copy-sequence server)
        (opencode-test-state-connect)
        (let ((live (opencode-test-open-empty id))
              (events (opencode-test-events id)))
          (opencode-test-dispatch events)
          (setq opencode-test-requests nil)
          (opencode-state-context id)
          (opencode-test-state-fetch-contexts)
          (should (equal (list scenario (opencode-state-context id))
                         (list scenario (buffer-local-value 'opencode-session-tokens live))))
          ;; Known from the events, without a fetch, unless no step
          ;; reported usage: the compaction that followed the revert
          ;; of the history settled the size.
          (should (eq (and (opencode-test-state-message-queries) t)
                      (equal scenario "shell-input"))))))))

;;;; Context size after a compaction

(defun opencode-test-state-compaction (offset status &optional context)
  "Return a compaction message of STATUS created at OFFSET, estimating CONTEXT.
Its usage is the compaction request's, which read the 120k tokens of
conversation it replaced, as the service records it."
  `((id . ,(opencode-test-state-id "msg_" offset)) (type . "compaction")
    (time (created . ,(opencode-test-state-ms offset)))
    (status . ,status) (reason . "manual")
    ,@(when (equal status "completed")
        `((model (id . "gpt-test") (providerID . "openai")) (summary . "## Objective\n- go on")
          ,@(when context `((context . ,context)))))
    ,@(when (equal status "failed")
        '((error (type . "compaction.failed") (message . "Compaction produced no summary"))))
    (cost . 0.4) (tokens . ,(opencode-test-state-usage 2000 900 118000))))

(defun opencode-test-state-compaction-ended (session offset &optional context)
  "Return the end of a compaction of SESSION at OFFSET, estimating CONTEXT."
  (opencode-test-state-event "session.compaction.ended" session offset
                             '(reason . "manual") '(model (id . "gpt-test") (providerID . "openai"))
                             '(text . "## Objective\n- go on")
                             `(context . ,context) '(cost . 0.4)
                             `(tokens . ,(opencode-test-state-usage 2000 900 118000))))

(ert-deftest opencode-test-state-context-compaction-events ()
  "A compaction's end settles the size to the service's estimate, or to unknown without one."
  (opencode-test-with-state-server nil
    (opencode-test-state-running-session)
    (opencode-test-dispatch (list (opencode-test-state-step-ended "ses_s" 100 (opencode-test-state-usage 120000))))
    (should (equal (opencode-state-context "ses_s") 120000))
    (should-not (opencode-state-context-estimated-p "ses_s"))
    (opencode-test-state-recording
      (opencode-test-dispatch
       (list (opencode-test-state-event "session.compaction.started" "ses_s" 110 '(reason . "manual"))))
      ;; Running, a compaction changes nothing.
      (should (equal (opencode-state-context "ses_s") 120000))
      (should-not opencode-test-state-updates)
      (opencode-test-dispatch (list (opencode-test-state-compaction-ended "ses_s" 120 4200)))
      (should (equal opencode-test-state-updates '("ses_s")))
      ;; Not its request's usage, which read the conversation replaced.
      (should (equal (opencode-state-context "ses_s") 4200))
      (should (opencode-state-context-estimated-p "ses_s"))
      (should (plist-get (opencode-state-get "ses_s") :context-estimated))
      ;; The service does not mark the session updated.
      (should (equal (plist-get (opencode-state-get "ses_s") :updated) (opencode-test-state-time 100)))
      (should-not opencode-test-state-changes))
    ;; The next step measures it.
    (opencode-test-dispatch (list (opencode-test-state-step-ended "ses_s" 130 (opencode-test-state-usage 4900))))
    (should (equal (opencode-state-context "ses_s") 4900))
    (should-not (opencode-state-context-estimated-p "ses_s"))
    ;; Without an estimate, as from a native compaction or an older
    ;; service, the size is unknown until the next step.
    (opencode-test-dispatch (list (opencode-test-state-compaction-ended "ses_s" 140)))
    (should-not (opencode-state-context "ses_s"))
    (should-not (opencode-state-context-estimated-p "ses_s"))
    ;; A failed compaction left the conversation as it was.
    (opencode-test-dispatch
     (list (opencode-test-state-step-ended "ses_s" 150 (opencode-test-state-usage 6000))
           (opencode-test-state-event "session.compaction.failed" "ses_s" 160 '(reason . "manual")
                                      '(error (type . "compaction.failed") (message . "no summary")))))
    (should (equal (opencode-state-context "ses_s") 6000))
    ;; All settled by events: nothing was fetched.
    (opencode-test-state-fetch-contexts)
    (should-not (opencode-test-state-message-queries))))

(ert-deftest opencode-test-state-context-fetched-after-compaction ()
  "A loaded session's size is that of a completed compaction newer than its newest usage."
  (opencode-test-with-state-server
      (list :sessions (mapcar (lambda (id) (opencode-test-state-info id :updated 40))
                              '("ses_a" "ses_b" "ses_c" "ses_d" "ses_e" "ses_f"))
            :messages
            `(("ses_a" ,(opencode-test-state-compaction 20 "completed" 4000)
               ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
              ;; A step after the compaction measured the size.
              ("ses_b" ,(opencode-test-state-assistant 30 (opencode-test-state-usage 5000))
               ,(opencode-test-state-compaction 20 "completed" 4000)
               ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
              ;; A compaction the service did not estimate.
              ("ses_c" ,(opencode-test-state-compaction 20 "completed")
               ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
              ;; A failed compaction changed nothing since the completed one.
              ("ses_d" ,(opencode-test-state-compaction 30 "failed")
               ,(opencode-test-state-compaction 20 "completed" 3000)
               ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
              ;; The step after the compaction runs, without usage yet.
              ("ses_e" ,(opencode-test-state-assistant 30)
               ,(opencode-test-state-compaction 20 "completed" 3500)
               ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
              ;; Compactions older than the newest usage are not paged through.
              ("ses_f" ,(opencode-test-state-assistant 30 (opencode-test-state-usage 700))
               ,(opencode-test-state-compaction 20 "failed")
               ,(opencode-test-state-compaction 10 "completed" 3000))))
    (opencode-test-state-connect)
    (let ((ids '("ses_a" "ses_b" "ses_c" "ses_d" "ses_e" "ses_f")))
      (mapc #'opencode-state-context ids)
      (opencode-test-state-fetch-contexts)
      (should (equal (mapcar (lambda (id)
                               (list id (opencode-state-context id) (opencode-state-context-estimated-p id)))
                             ids)
                     '(("ses_a" 4000 t) ("ses_b" 5000 nil) ("ses_c" nil nil) ("ses_d" 3000 t)
                       ("ses_e" 3500 t) ("ses_f" 700 nil))))
      (should (equal (seq-keep (lambda (query) (and (member (car query) '("ses_d" "ses_f")) query))
                               (opencode-test-state-message-queries "compaction"))
                     '(("ses_d" (limit . 1) (type . "compaction") (order . "desc"))
                       ("ses_d" (limit . 1) (type . "compaction") (cursor . "1"))
                       ("ses_f" (limit . 1) (type . "compaction") (order . "desc"))))))))

(ert-deftest opencode-test-state-context-compaction-supersedes-fetch ()
  "A compaction ending during a fetch wins over its answer, at either of its requests."
  (dolist (held '("assistant" "compaction"))
    (opencode-test-with-state-server nil
      (opencode-test-state-running-session)
      (plist-put opencode-test-state-server :messages
                 `(("ses_s" ,(opencode-test-state-assistant 50 (opencode-test-state-usage 120000)))))
      (let ((opencode-test-state-held-types (list held)))
        (opencode-test-state-holding-messages
          (opencode-state-context "ses_s")
          (opencode-test-state-fetch-contexts)
          (should (= (length opencode-test-state-held) 1))
          (opencode-test-dispatch (list (opencode-test-state-compaction-ended "ses_s" 100 4200)))
          (opencode-test-state-answer)
          (should (equal (list held (opencode-state-context "ses_s")) (list held 4200)))
          (should (opencode-state-context-estimated-p "ses_s"))
          (opencode-test-state-fetch-contexts)
          (should-not opencode-test-state-held))))))

(ert-deftest opencode-test-state-context-compaction-reconnect ()
  "A compaction that may have ended while the stream was down has the size fetched again.
The service does not mark the session updated when it ends."
  (opencode-test-with-state-server
      (list :sessions (list (opencode-test-state-info "ses_a" :updated 10)
                            (opencode-test-state-info "ses_b" :updated 10))
            :messages `(("ses_a" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
                        ("ses_b" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 90000)))))
    (opencode-test-state-connect)
    (mapc #'opencode-state-context '("ses_a" "ses_b"))
    (opencode-test-state-fetch-contexts)
    (should (equal (mapcar #'opencode-state-context '("ses_a" "ses_b")) '(120000 90000)))
    (opencode-test-dispatch
     (list (opencode-test-state-event "session.compaction.started" "ses_a" 20 '(reason . "auto"))))
    (cl-flet ((reconnect (messages)
                (plist-put opencode-test-state-server :messages
                           `(("ses_a" ,@messages ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
                             ("ses_b" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 90000)))))
                (setq opencode-test-requests nil)
                (opencode-test-state-connect)
                (mapc #'opencode-state-context '("ses_a" "ses_b"))
                (opencode-test-state-fetch-contexts)
                (mapcar #'car (opencode-test-state-message-queries "assistant"))))
      ;; Still running when the stream comes back.
      (should (equal (reconnect (list (opencode-test-state-compaction 20 "running"))) '("ses_a")))
      (should (equal (mapcar #'opencode-state-context '("ses_a" "ses_b")) '(120000 90000)))
      ;; It ended while the stream was down again.
      (should (equal (reconnect (list (opencode-test-state-compaction 20 "completed" 4000))) '("ses_a")))
      (should (equal (opencode-state-context "ses_a") 4000))
      (should (opencode-state-context-estimated-p "ses_a"))
      ;; That fetch found none running: a reconnection keeps the estimate,
      ;; as any size, while nothing changed.
      (should-not (reconnect (list (opencode-test-state-compaction 20 "completed" 4000))))
      (should (equal (opencode-state-context "ses_a") 4000))
      (should (opencode-state-context-estimated-p "ses_a"))
      ;; A compaction starting while a fetch runs outlives that fetch's answer.
      (opencode-test-dispatch (list (opencode-test-state-event "session.revert.committed" "ses_b" 30
                                                               '(to . "msg_x"))))
      (let ((opencode-test-state-held-types '("compaction")))
        (opencode-test-state-holding-messages
          (opencode-state-context "ses_b")
          (opencode-test-state-fetch-contexts)
          (opencode-test-dispatch
           (list (opencode-test-state-event "session.compaction.started" "ses_b" 40 '(reason . "manual"))))
          (opencode-test-state-answer)
          (should (equal (opencode-state-context "ses_b") 90000))))
      (should (equal (reconnect (list (opencode-test-state-compaction 20 "completed" 4000))) '("ses_b"))))))

(ert-deftest opencode-test-state-context-compaction-matches-buffers ()
  "After a compaction, the registry's size is the one the session buffer shows.
Fetched, it is what replaying the transcript finds; followed live, what
the buffer receiving the compaction's end finds."
  (pcase-dolist (`(,messages ,events)
                 `(;; The service's estimate.
                   ((,(opencode-test-state-compaction 20 "completed" 4000)
                     ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
                    (,(opencode-test-state-compaction-ended "ses_x" 20 4000)))
                   ;; None: unknown until the next step.
                   ((,(opencode-test-state-compaction 20 "completed")
                     ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
                    (,(opencode-test-state-compaction-ended "ses_x" 20)))
                   ;; Measured by the next step.
                   ((,(opencode-test-state-assistant 30 (opencode-test-state-usage 5000))
                     ,(opencode-test-state-compaction 20 "completed" 4000)
                     ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
                    (,(opencode-test-state-compaction-ended "ses_x" 20 4000)
                     ,(opencode-test-state-step-ended "ses_x" 30 (opencode-test-state-usage 5000))))
                   ;; A failed compaction changes nothing.
                   ((,(opencode-test-state-compaction 20 "failed")
                     ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
                    (,(opencode-test-state-event "session.compaction.failed" "ses_x" 20 '(reason . "manual")
                                                 '(error (type . "compaction.failed") (message . "no summary")))))))
    (let ((server (list :sessions (list (opencode-test-state-info "ses_x" :updated 40))
                        :messages (list (cons "ses_x" messages)))))
      (opencode-test-with-state-server (copy-sequence server)
        (opencode-test-state-connect)
        (let ((buffer (opencode-session-open (opencode-test-state-info "ses_x" :updated 40) :no-display t)))
          (opencode-state-context "ses_x")
          (opencode-test-state-fetch-contexts)
          (should (equal (list (opencode-state-context "ses_x") (opencode-state-context-estimated-p "ses_x"))
                         (list (buffer-local-value 'opencode-session-tokens buffer)
                               (buffer-local-value 'opencode-session-tokens-estimated buffer))))))
      ;; Live, from an empty transcript.
      (opencode-test-with-state-server (list :sessions (plist-get server :sessions) :messages '(("ses_x")))
        (opencode-test-state-connect)
        (let ((buffer (opencode-session-open (opencode-test-state-info "ses_x" :updated 40) :no-display t)))
          (plist-put opencode-test-state-server :messages (plist-get server :messages))
          ;; The buffer and the registry knew the measured size first.
          (opencode-test-dispatch (list (opencode-test-state-step-ended "ses_x" 10 (opencode-test-state-usage 120000))))
          (opencode-test-dispatch events)
          (setq opencode-test-requests nil)
          (should (equal (list (opencode-state-context "ses_x") (opencode-state-context-estimated-p "ses_x"))
                         (list (buffer-local-value 'opencode-session-tokens buffer)
                               (buffer-local-value 'opencode-session-tokens-estimated buffer))))
          (opencode-test-state-fetch-contexts)
          (should-not (opencode-test-state-message-queries)))))))

;;;; Compactions and the listing

(defun opencode-test-state-sizes (ids)
  "Return the context size of each of IDS, and whether it is an estimate."
  (mapcar (lambda (id) (list id (opencode-state-context id) (and (opencode-state-context-estimated-p id) t)))
          ids))

(ert-deftest opencode-test-state-context-compaction-rebuild-pages ()
  "A rebuild forgets at once the size of a session seen compacting, whatever page holds it.
Other sizes wait for their session's page, which keeps them with their
estimate mark; a size fetched since the rebuild began stays when the
page arrives, even of a session whose compaction that fetch found
running."
  (let ((opencode-state--page-size 3)
        (ids '("ses_05" "ses_06" "ses_07")))
    (opencode-test-with-state-server
        (list :sessions (opencode-test-state-numbered 7)
              :messages `(("ses_05" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
                          ("ses_06" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 90000)))
                          ("ses_07" ,(opencode-test-state-compaction 20 "completed" 4000)
                           ,(opencode-test-state-assistant 10 (opencode-test-state-usage 80000)))))
      (opencode-test-state-connect)
      (mapc #'opencode-state-context ids)
      (opencode-test-state-fetch-contexts)
      (should (equal (opencode-test-state-sizes ids)
                     '(("ses_05" 120000 nil) ("ses_06" 90000 nil) ("ses_07" 4000 t))))
      (opencode-test-dispatch
       (list (opencode-test-state-event "session.compaction.started" "ses_05" 20 '(reason . "auto"))
             (opencode-test-state-event "session.compaction.started" "ses_06" 20 '(reason . "auto"))))
      ;; The compaction of ses_05 ended while the stream was down; that of
      ;; ses_06 still runs.
      (plist-put opencode-test-state-server :messages
                 `(("ses_05" ,(opencode-test-state-compaction 20 "completed")
                    ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))
                   ("ses_06" ,(opencode-test-state-compaction 30 "running")
                    ,(opencode-test-state-assistant 10 (opencode-test-state-usage 90000)))
                   ,(assoc "ses_07" (plist-get opencode-test-state-server :messages))))
      (plist-put opencode-test-state-server :defer-pages t)
      (setq opencode-test-requests nil)
      (opencode-test-state-connect)
      (should (opencode-state-ready-p))
      ;; Their pages are still to come.
      (should-not (opencode-state--current-p "ses_05"))
      (should-not (opencode-state--current-p "ses_07"))
      ;; Seen compacting: no size, rather than the one the compaction may
      ;; have replaced; the others keep theirs meanwhile.
      (should (equal (opencode-test-state-sizes ids)
                     '(("ses_05" nil nil) ("ses_06" nil nil) ("ses_07" 4000 t))))
      (opencode-test-state-fetch-contexts)
      (should (equal (mapcar #'car (opencode-test-state-message-queries "assistant")) '("ses_05" "ses_06")))
      (should (equal (opencode-test-state-sizes ids)
                     '(("ses_05" nil nil) ("ses_06" 90000 nil) ("ses_07" 4000 t))))
      (should-not (gethash "ses_05" opencode-state--compacting))
      (should (gethash "ses_06" opencode-state--compacting))
      (while (opencode-test-release))
      (should (opencode-state-loaded-p))
      (setq opencode-test-requests nil)
      (should (equal (opencode-test-state-sizes ids)
                     '(("ses_05" nil nil) ("ses_06" 90000 nil) ("ses_07" 4000 t))))
      (opencode-test-state-fetch-contexts)
      (should-not (opencode-test-state-message-queries)))))

(ert-deftest opencode-test-state-context-compaction-during-listing ()
  "A compaction ending in a session whose page is still to come settles its size for good.
The event fetches the session; its page, arriving later, leaves it."
  (let ((opencode-state--page-size 3))
    (opencode-test-with-state-server
        (list :sessions (opencode-test-state-numbered 6) :defer-pages t
              :messages `(("ses_05" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 120000)))))
      (opencode-test-state-connect)
      (setq opencode-test-requests nil)
      (opencode-test-dispatch
       (list (opencode-test-state-event "session.compaction.started" "ses_05" 990 '(reason . "manual"))
             (opencode-test-state-compaction-ended "ses_05" 995 4200)))
      (should (equal (opencode-test-state-session-requests) '("/api/session/ses_05")))
      (should (opencode-state--current-p "ses_05"))
      (should-not (gethash "ses_05" opencode-state--compacting))
      (should (equal (opencode-test-state-sizes '("ses_05")) '(("ses_05" 4200 t))))
      (while (opencode-test-release))
      (should (opencode-state-loaded-p))
      (should (equal (opencode-test-state-sizes '("ses_05")) '(("ses_05" 4200 t))))
      (opencode-test-state-fetch-contexts)
      (should-not (opencode-test-state-message-queries)))))

(ert-deftest opencode-test-state-context-compaction-dropped ()
  "A session dropped at the end of the listing leaves no compaction mark; one kept by its buffer keeps it."
  (opencode-test-with-state-server
      (list :sessions (opencode-test-state-numbered 4)
            :messages `(("ses_03" ,(opencode-test-state-assistant 10 (opencode-test-state-usage 50000)))))
    (opencode-test-state-open "ses_03")
    (opencode-test-state-connect)
    (opencode-state-context "ses_03")
    (opencode-test-state-fetch-contexts)
    (should (equal (opencode-state-context "ses_03") 50000))
    (opencode-test-dispatch
     (list (opencode-test-state-event "session.compaction.started" "ses_03" 20 '(reason . "auto"))
           (opencode-test-state-event "session.compaction.started" "ses_04" 20 '(reason . "auto"))))
    ;; Both are archived while the stream is down.
    (dolist (info (plist-get opencode-test-state-server :sessions))
      (when (member (alist-get 'id info) '("ses_03" "ses_04"))
        (setf (alist-get 'archived (alist-get 'time info)) (opencode-test-state-ms 950))))
    (opencode-test-state-connect)
    (should (opencode-state-loaded-p))
    (should-not (opencode-state-get "ses_04"))
    (should-not (gethash "ses_04" opencode-state--compacting))
    ;; Kept by its buffer, its compaction may still run.
    (should (plist-get (opencode-state-get "ses_03") :archived))
    (should (gethash "ses_03" opencode-state--compacting))
    (should-not (plist-get (opencode-state-get "ses_03") :context))))

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

(ert-deftest opencode-test-archive-tells-its-outcome ()
  "Archiving and restoring call back once the service answered, as it answered.
Until then neither is called, so that a caller acts on the outcome only."
  (opencode-test-with-server
    (let* ((outcomes nil)
           (then (lambda (info) (push (list 'then (alist-get 'id info)) outcomes)))
           (else (lambda (error) (push (list 'else (opencode-error-status error)) outcomes)))
           (opencode-test-routes
            (append (list (list 'patch "\\`/api/session/ses_refused\\'"
                                (lambda (_) '(:error 409 "Session is busy")))
                          (list 'patch "\\`/api/session/\\(ses_[a-z]+\\)\\'"
                                (lambda (request)
                                  (cons :defer (lambda () `((id . ,(plist-get request :match))))))))
                    opencode-test-routes)))
      (opencode-session-archive "ses_held" nil then else)
      (opencode-session-archive "ses_held" t then else)
      (should-not outcomes)
      (should (opencode-test-release))
      (should (equal outcomes '((then "ses_held"))))
      (should (opencode-test-release))
      (should (equal outcomes '((then "ses_held") (then "ses_held"))))
      (setq outcomes nil)
      (opencode-session-archive "ses_refused" nil then else)
      (should (equal outcomes '((else 409))))
      ;; Without callbacks, a refusal is reported, as before.
      (should-error (opencode-session-archive "ses_refused")))))

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

(ert-deftest opencode-test-state-last-turn ()
  "The last turn is the newest prompt, the reply and error since, and the files changed."
  (opencode-test-with-state-server
      (list :messages `(("ses_a"
                         ((type . "assistant") (time (created . 50))
                          (content ((type . "tool") (id . "call_1") (name . "edit")))
                          (error (type . "provider") (message . "Quota exceeded")))
                         ((type . "assistant") (time (created . 40))
                          (content ((type . "text") (text . "  All done.  "))))
                         ((type . "user") (time (created . 30)) (text . " Fix the build \n"))
                         ((type . "assistant") (time (created . 20))
                          (content ((type . "text") (text . "Earlier reply")))))
                        ("ses_b"
                         ((type . "user") (time (created . 30)) (text . "Next step"))
                         ((type . "assistant") (time (created . 20))
                          (content ((type . "text") (text . "A former turn's reply")))))))
    (let ((opencode-test-routes
           (cons (list 'get "\\`/api/session/\\(ses_[A-Za-z0-9_]+\\)/diff\\'"
                       (lambda (request)
                         (if (equal (plist-get request :match) "ses_a")
                             '((data ((file . "a.el") (patch . "") (additions . 10) (deletions . 2)
                                      (status . "modified"))
                                     ((file . "b/c.el") (patch . "") (additions . 3) (deletions . 0)
                                      (status . "added"))))
                           '(:error 500 "Snapshots unavailable"))))
                 opencode-test-routes))
          (results nil))
      (dolist (id '("ses_a" "ses_b"))
        (opencode-session-last-turn id (lambda (turn) (push (cons id turn) results))))
      (should (equal (alist-get "ses_a" results nil nil #'equal)
                     '(:prompt "Fix the build" :reply "All done." :error "Quota exceeded"
                       :changes (("a.el" 10 2) ("b/c.el" 3 0)))))
      ;; A reply older than the prompt is a former turn's; the files
      ;; changed, unavailable, are unknown.
      (should (equal (alist-get "ses_b" results nil nil #'equal)
                     '(:prompt "Next step" :reply nil :error nil :changes nil)))
      ;; Without context: only the counts are read.
      (let ((diff (seq-find (lambda (request) (equal (plist-get request :path) "/api/session/ses_a/diff"))
                            opencode-test-requests)))
        (should (equal (plist-get diff :query) '((context . "0")))))
      (let ((prompt (seq-find (lambda (request)
                                (and (equal (plist-get request :path) "/api/session/ses_a/message")
                                     (equal (alist-get 'type (plist-get request :query)) "user")))
                              opencode-test-requests)))
        (should (equal (alist-get 'limit (plist-get prompt :query)) 1))))))

(ert-deftest opencode-test-state-last-turn-failure ()
  "A turn none of whose requests succeed is a failure, told once."
  (opencode-test-with-state-server nil
    (let ((opencode-test-routes
           (cons (list 'get "\\`/api/session/ses_x/" (lambda (_) '(:error 404 "Session not found: ses_x")))
                 opencode-test-routes))
          (results nil))
      (opencode-session-last-turn "ses_x" (lambda (turn) (push turn results)))
      (should (= (length results) 1))
      (should (eq (car (car results)) :failure))
      (should (string-match-p "Session not found" (cadr (car results)))))))

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
