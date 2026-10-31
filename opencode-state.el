;;; opencode-state.el --- Live state of every session  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A registry of every known session's live state, independent of
;; session buffers: whether an execution runs, the retry it waits for,
;; the permission requests and questions it waits on, how its last turn
;; ended and whether anyone saw that.  Session lists and dashboards read
;; it instead of asking the service.
;;
;; The service is authoritative.  Whenever the event stream (re)opens,
;; whoever opened it, the registry is rebuilt from REST: the newest
;; unarchived sessions, the running ones, and what those wait on.
;; Events received meanwhile are held and applied over the result; after
;; that, events keep it current, and an event of a session it does not
;; know fetches that session first.
;;
;; Unread is the service's, shared by every client: a session is unread
;; when its last idle transition is newer than the watermark
;; `opencode-session-view' advances.  The registry projects both times
;; as the service does, so the watermark it posts is one the service
;; accepts.  An entry fetched after an event may already reflect it, so
;; replaying such an event never advances the idle time twice.

;;; Code:

(require 'opencode-common)
(require 'opencode-events)
(require 'opencode-http)

(defcustom opencode-state-recent-limit 100
  "Number of the newest unarchived sessions loaded when the stream connects.
Running sessions are loaded whatever their age, and any other session
joins the registry when an event concerns it."
  :type 'natnum
  :group 'opencode)

(defcustom opencode-state-change-functions nil
  "Abnormal hook run when the derived status of a session changes.
Each function receives the session ID, its old status and its new one,
as `opencode-state-status' returns them.  The old status is nil for a
session new to the registry, the new one nil for a session removed."
  :type 'hook
  :group 'opencode)

(defcustom opencode-state-updated-functions nil
  "Abnormal hook run with a session ID whenever its registry entry changes.
Creation and removal count as changes."
  :type 'hook
  :group 'opencode)

(defconst opencode-state--keys
  '(:id :title :directory :project-id :parent-id :agent :model :running :retry :requests
        :outcome :error :idle :viewed :archived :created :updated :started)
  "Keys of a registry entry, in order.")

(defvar opencode-state--sessions (make-hash-table :test #'equal)
  "Session IDs mapped to their registry entries.")

(defvar opencode-state--stream 'none
  "Event stream process whose `server.connected' the registry last handled.
Events are applied only while they come from this stream.")

(defvar opencode-state--ready nil
  "Non-nil once the registry was bootstrapped on `opencode-state--stream'.")

(defvar opencode-state--bootstrapping nil
  "Non-nil while a bootstrap runs.")

(defvar opencode-state--generation 0
  "Count of bootstraps; answers to a superseded one are ignored.")

(defvar opencode-state--held nil
  "Events received while bootstrapping, newest first.")

(defvar opencode-state--fetching (make-hash-table :test #'equal)
  "IDs of unknown sessions being fetched, mapped to their events, newest first.")

(defvar opencode-state--waiters nil
  "Functions waiting for the registry to be ready, newest first.")

(defvar opencode-state--retry-timer nil
  "Timer retrying a failed bootstrap.")

(defvar opencode-state--retry-delay 1
  "Seconds before the next retry of a failed bootstrap.")

(defvar opencode-state--viewing (make-hash-table :test #'equal)
  "Session IDs mapped to the idle watermark, in milliseconds, of their view in flight.")

;;;; Entries

(defun opencode-state--make-entry (&rest values)
  "Return an entry with VALUES, a plist; keys missing from it are nil."
  (mapcan (lambda (key) (list key (plist-get values key))) opencode-state--keys))

(defun opencode-state--ms (seconds)
  "Return float SECONDS as integer epoch milliseconds, the service's unit."
  (and seconds (round (* seconds 1000))))

(defun opencode-state--location-directory (location)
  "Return the normalized directory of LOCATION, a location alist, or nil."
  (when-let* ((directory (alist-get 'directory location)))
    (opencode-normalize-directory directory)))

(defun opencode-state--one-line (text)
  "Return the first line of TEXT trimmed, with an ellipsis when more follow.
Return nil when TEXT is not a string or blank."
  (when (stringp text)
    (let ((lines (split-string (string-trim text) "\n")))
      (unless (string-empty-p (car lines))
        (concat (string-trim (car lines)) (if (cdr lines) " …" ""))))))

(defun opencode-state--error-message (error)
  "Return the one-line message of session ERROR, an alist with `type' and `message'."
  (or (opencode-state--one-line (alist-get 'message error))
      (alist-get 'type error)))

(defun opencode-state--info-entry (info &optional running)
  "Return an entry made from session INFO, running when RUNNING is non-nil.
The live parts INFO lacks, the running execution's start, retry and
requests and the last failure's message, are left empty."
  (let-alist info
    (opencode-state--make-entry
     :id .id :title .title :directory (opencode-state--location-directory .location)
     :project-id .projectID :parent-id .parentID :agent .agent :model .model
     :running (and running t)
     :outcome (and .outcome (intern .outcome))
     :idle (opencode-time-seconds .time.idle)
     :viewed (opencode-time-seconds .time.viewed)
     :archived (opencode-time-seconds .time.archived)
     :created (opencode-time-seconds .time.created)
     :updated (opencode-time-seconds .time.updated))))

(defun opencode-state--permission-summary (request)
  "Return the one-line summary of permission REQUEST, a service alist."
  (let-alist request
    (or (opencode-state--one-line .message)
        (if .resources
            (format "%s: %s" .action (or (opencode-state--one-line (car .resources)) ""))
          .action))))

(defun opencode-state--request (kind request time)
  "Return the entry request of KIND for service REQUEST, asked at TIME.
KIND is `permission', whose REQUEST is a permission request, or
`question', whose REQUEST is a form."
  (list :kind kind :id (alist-get 'id request)
        :summary (if (eq kind 'permission)
                     (opencode-state--permission-summary request)
                   (alist-get 'title request))
        :time time))

(defun opencode-state--find-request (requests id)
  "Return the request of REQUESTS whose ID is ID."
  (seq-find (lambda (request) (equal (plist-get request :id) id)) requests))

;;;; Queries

(defun opencode-state-ready-p ()
  "Return non-nil once the registry is bootstrapped on the current event stream."
  (and opencode-state--ready
       (eq opencode-state--stream opencode-events--process)
       (opencode-events-connected-p)))

(defun opencode-state-get (id)
  "Return the registry entry of session ID, or nil.
An entry is a plist with the keys :id, :title, :directory (normalized),
:project-id, :parent-id, :agent, :model (a model reference), :running,
:retry (a plist :attempt :at :message, or nil), :requests (plists :kind
:id :summary :time, oldest first), :outcome (`succeeded', `failed' or
`interrupted'), :error (the last failure's message while the outcome is
`failed'), :idle, :viewed, :archived, :created, :updated and :started.
Times are float seconds since the epoch."
  (gethash id opencode-state--sessions))

(defun opencode-state-sessions (&optional archived)
  "Return every registry entry, most recently updated first.
Archived sessions are included only when ARCHIVED is non-nil."
  (let ((entries nil))
    (maphash (lambda (_id entry)
               (when (or archived (not (plist-get entry :archived)))
                 (push entry entries)))
             opencode-state--sessions)
    (sort entries :key (lambda (entry) (or (plist-get entry :updated) 0)) :reverse t)))

(defun opencode-state--entry (entry-or-id)
  "Return ENTRY-OR-ID's entry: itself, or the entry of that session ID."
  (if (stringp entry-or-id) (opencode-state-get entry-or-id) entry-or-id))

(defun opencode-state-unread-p (entry-or-id)
  "Return non-nil when the last turn of ENTRY-OR-ID ended unseen.
That is when no execution runs and its idle time is newer than the time
up to which it was viewed, as the service decides for every client."
  (when-let* ((entry (opencode-state--entry entry-or-id))
              ((not (plist-get entry :running)))
              (idle (opencode-state--ms (plist-get entry :idle))))
    (let ((viewed (opencode-state--ms (plist-get entry :viewed))))
      (or (null viewed) (< viewed idle)))))

(defun opencode-state-status (entry-or-id)
  "Return the derived status of ENTRY-OR-ID, an entry or a session ID.
`waiting' while a request awaits the user; else while an execution runs,
`retrying' when it waits for a retry, `working' otherwise; else when
unread, `failed' when the last turn failed, `done' otherwise; else
`idle'.  Return nil for an unknown session."
  (when-let* ((entry (opencode-state--entry entry-or-id)))
    (cond ((plist-get entry :requests) 'waiting)
          ((plist-get entry :running) (if (plist-get entry :retry) 'retrying 'working))
          ((opencode-state-unread-p entry)
           (if (eq (plist-get entry :outcome) 'failed) 'failed 'done))
          (t 'idle))))

(defun opencode-state-since (entry-or-id)
  "Return the time the current status of ENTRY-OR-ID began, or nil.
The oldest request's time while waiting, the execution's start while
running, the idle time when failed or done, and nil when idle or not
known."
  (when-let* ((entry (opencode-state--entry entry-or-id)))
    (pcase (opencode-state-status entry)
      ('waiting (plist-get (car (plist-get entry :requests)) :time))
      ((or 'working 'retrying) (plist-get entry :started))
      ((or 'failed 'done) (plist-get entry :idle)))))

;;;; Changes

(defun opencode-state--run-hook (hook &rest arguments)
  "Run HOOK with ARGUMENTS; an error of one function does not stop the others."
  (apply #'run-hook-wrapped hook
         (lambda (function &rest arguments)
           (with-demoted-errors "OpenCode session state hook: %S"
             (apply function arguments))
           nil)
         arguments))

(defun opencode-state--store (id entry)
  "Make ENTRY the entry of session ID, nil removing it, and run the hooks on a change."
  (let ((old (gethash id opencode-state--sessions)))
    (unless (equal old entry)
      (if entry
          (puthash id entry opencode-state--sessions)
        (remhash id opencode-state--sessions)
        (remhash id opencode-state--viewing))
      (let ((before (and old (opencode-state-status old)))
            (after (and entry (opencode-state-status entry))))
        (unless (eq before after)
          (opencode-state--run-hook 'opencode-state-change-functions id before after)))
      (opencode-state--run-hook 'opencode-state-updated-functions id))))

(defun opencode-state--apply (entry event replay)
  "Return ENTRY, a fresh copy of a registry entry, updated by EVENT.
REPLAY means ENTRY was fetched after EVENT may have happened: an end of
execution whose idle time ENTRY already has is then not counted again."
  (let* ((data (alist-get 'data event))
         (milliseconds (or (alist-get 'created event) (opencode-state--ms (float-time))))
         (created (opencode-time-seconds milliseconds))
         (requests (plist-get entry :requests)))
    (cl-flet ((assign (&rest pairs)
                (while pairs
                  (setq entry (plist-put entry (pop pairs) (pop pairs)))))
              (add (request)
                (unless (opencode-state--find-request requests (plist-get request :id))
                  (setq entry (plist-put entry :requests (append requests (list request))))))
              (drop (id)
                (setq entry (plist-put entry :requests
                                       (seq-remove (lambda (request) (equal (plist-get request :id) id))
                                                   requests)))))
      (pcase (alist-get 'type event)
        ("session.renamed" (assign :title (alist-get 'title data) :updated created))
        ("session.moved"
         (assign :directory (opencode-state--location-directory (alist-get 'location data))
                 :updated created)
         (when-let* ((project (alist-get 'projectID data)))
           (assign :project-id project)))
        ("session.agent.selected" (assign :agent (alist-get 'agent data) :updated created))
        ("session.model.selected" (assign :model (alist-get 'model data) :updated created))
        ("session.archived" (assign :archived created :updated created))
        ("session.unarchived" (assign :archived nil :updated created))
        ("session.viewed"
         ;; The service keeps the highest watermark.
         (let ((idle (alist-get 'idle data))
               (viewed (opencode-state--ms (plist-get entry :viewed))))
           (when (and (numberp idle) (or (null viewed) (> idle viewed)))
             (assign :viewed (opencode-time-seconds idle)))))
        ((or "session.step.ended" "session.inbox.enqueued") (assign :updated created))
        ("session.step.started" (assign :retry nil :updated created))
        ("session.execution.started"
         (assign :running t :started created :retry nil :requests nil))
        ("session.retry.scheduled"
         (assign :retry (list :attempt (alist-get 'attempt data)
                              :at (opencode-time-seconds (alist-get 'at data))
                              :message (opencode-state--error-message (alist-get 'error data)))))
        ;; A restarted service resumes the execution: neither idle nor outcome change.
        ((and "session.execution.interrupted" (guard (equal (alist-get 'reason data) "shutdown")))
         (assign :retry nil))
        ((and (or "session.execution.succeeded" "session.execution.failed"
                  "session.execution.interrupted")
              type)
         (let ((idle (opencode-state--ms (plist-get entry :idle))))
           (assign :running nil :started nil :retry nil :requests nil)
           (unless (and replay idle (>= idle milliseconds))
             ;; Unread compares strictly, so the service advances every idle time.
             (assign :idle (opencode-time-seconds (if idle (max milliseconds (1+ idle)) milliseconds))
                     :outcome (pcase type
                                ("session.execution.succeeded" 'succeeded)
                                ("session.execution.failed" 'failed)
                                (_ 'interrupted))
                     :error (and (equal type "session.execution.failed")
                                 (opencode-state--error-message (alist-get 'error data)))))))
        ("permission.asked" (add (opencode-state--request 'permission data created)))
        ("permission.replied" (drop (alist-get 'requestID data)))
        ("form.created" (add (opencode-state--request 'question (alist-get 'form data) created)))
        ((or "form.replied" "form.cancelled") (drop (alist-get 'id data)))))
    entry))

;;;; Fetching

(defun opencode-state--active-ids (response)
  "Return the IDs of the running sessions in RESPONSE of `/api/session/active'."
  (mapcar (lambda (pair) (format "%s" (car pair))) (opencode-request-data response)))

(defun opencode-state--fetch-turn (entry then)
  "Call THEN with the start and the retry of running ENTRY's execution.
The start is not recorded anywhere; it is taken as the first message
after the transcript's last idle marker, other than an agent, model or
location switch, which happen between turns too.  The retry is that
of the newest assistant message, which the service clears once the
step restarts.  Both are nil when unknown."
  (let ((id (plist-get entry :id))
        (started nil)
        (retry nil)
        (assistant-seen nil))
    (cl-labels ((page (cursor)
                  (opencode-request
                   'get (format "/api/session/%s/message" id)
                   :directory (plist-get entry :directory) :buffer nil
                   :query (if cursor `((limit . 50) (cursor . ,cursor)) '((limit . 50) (order . "desc")))
                   :else (lambda (_) (funcall then started retry))
                   :then (lambda (response)
                           (let ((messages (opencode-request-data response))
                                 (done nil))
                             (while (and messages (not done))
                               (let* ((message (pop messages))
                                      (type (alist-get 'type message)))
                                 (cond
                                  ((equal type "idle") (setq done t))
                                  ((member type '("agent-switched" "model-switched" "location-switched")))
                                  (t (setq started (or (opencode-time-seconds
                                                        (alist-get 'created (alist-get 'time message)))
                                                       started))))
                                 (when (and (equal type "assistant") (not assistant-seen))
                                   (setq assistant-seen t
                                         retry (when-let* ((data (alist-get 'retry message)))
                                                 (list :attempt (alist-get 'attempt data)
                                                       :at (opencode-time-seconds (alist-get 'at data))
                                                       :message (opencode-state--error-message
                                                                 (alist-get 'error data))))))))
                             (let ((next (alist-get 'next (alist-get 'cursor response))))
                               (if (or done (null next) (null (opencode-request-data response)))
                                   (funcall then started retry)
                                 (page next))))))))
      (page nil))))

(defun opencode-state--fetch-error (entry then)
  "Call THEN with the message of failed ENTRY's last failure, or nil."
  (opencode-request
   'get (format "/api/session/%s/message" (plist-get entry :id))
   :directory (plist-get entry :directory) :buffer nil
   :query '((limit . 1) (order . "desc") (type . "idle"))
   :else (lambda (_) (funcall then nil))
   :then (lambda (response)
           (funcall then (when-let* ((error (alist-get 'error (car (opencode-request-data response)))))
                           (opencode-state--error-message error))))))

(defun opencode-state--fetch-requests (entry then else)
  "Call THEN with the pending permission requests and forms of ENTRY, oldest first.
ELSE receives the `opencode-error' of a failed request.  A session that
no longer exists has none.  Times of requests the registry knows are
kept; others come from their IDs, which hold their creation time."
  (let* ((id (plist-get entry :id))
         (known (plist-get (opencode-state-get id) :requests))
         (pending 2)
         (requests nil)
         (failed nil))
    (cl-flet ((collect (kind)
                (lambda (response)
                  (dolist (request (opencode-request-data response))
                    (let ((request-id (alist-get 'id request)))
                      (push (opencode-state--request
                             kind request
                             (or (plist-get (opencode-state--find-request known request-id) :time)
                                 (opencode-identifier-time request-id)
                                 (float-time)))
                            requests)))
                  (when (and (zerop (cl-decf pending)) (not failed))
                    (funcall then (sort requests :key (lambda (request) (plist-get request :time)))))))
              (fail (error)
                (if (eql (opencode-error-status error) 404)
                    (when (and (zerop (cl-decf pending)) (not failed))
                      (funcall then (sort requests :key (lambda (request) (plist-get request :time)))))
                  (unless failed
                    (setq failed t)
                    (funcall else error)))))
      (dolist (kind '(permission question))
        (opencode-request 'get (format "/api/session/%s/%s" id (if (eq kind 'permission) "permission" "form"))
                          :directory (plist-get entry :directory) :buffer nil
                          :then (collect kind) :else #'fail)))))

(defun opencode-state--complete (entries then else)
  "Fill in the live parts of ENTRIES from the service, then call THEN with them.
Running sessions get their pending requests, start and retry, failed
ones the message of their failure, reused when the registry knows that
failure already.  ELSE receives the `opencode-error' of a failed request
for pending requests; the rest is best effort."
  (let ((pending 1)
        (failed nil))
    (cl-labels ((done ()
                  (when (and (zerop (cl-decf pending)) (not failed))
                    (funcall then entries)))
                (fail (error)
                  (unless failed
                    (setq failed t)
                    (funcall else error))))
      (dolist (entry entries)
        (let ((known (opencode-state-get (plist-get entry :id))))
          (when (plist-get entry :running)
            (cl-incf pending 2)
            (opencode-state--fetch-requests
             entry (lambda (requests) (plist-put entry :requests requests) (done)) #'fail)
            (opencode-state--fetch-turn
             entry (lambda (started retry)
                     (plist-put entry :started started)
                     (plist-put entry :retry retry)
                     (done))))
          (when (eq (plist-get entry :outcome) 'failed)
            (if (and (eq (plist-get known :outcome) 'failed)
                     (plist-get known :error)
                     (equal (plist-get known :idle) (plist-get entry :idle)))
                (plist-put entry :error (plist-get known :error))
              (cl-incf pending)
              (opencode-state--fetch-error
               entry (lambda (message) (plist-put entry :error message) (done)))))))
      (done))))

(defun opencode-state--load (id then else)
  "Call THEN with the entry of session ID built from the service.
ELSE receives the `opencode-error' of a failed request."
  (opencode-request
   'get (concat "/api/session/" id) :directory nil :buffer nil :else else
   :then (lambda (response)
           (let ((info (opencode-request-data response)))
             (opencode-request
              'get "/api/session/active" :directory nil :buffer nil :else else
              :then (lambda (response)
                      (opencode-state--complete
                       (list (opencode-state--info-entry
                              info (member id (opencode-state--active-ids response))))
                       (lambda (entries) (funcall then (car entries)))
                       else)))))))

(defun opencode-state--fetch (id event)
  "Fetch unknown session ID, then apply EVENT and those that follow it."
  (puthash id (list event) opencode-state--fetching)
  (let ((generation opencode-state--generation))
    (cl-flet ((current-p ()
                (and (= generation opencode-state--generation)
                     (gethash id opencode-state--fetching))))
      (opencode-state--load
       id
       (lambda (entry)
         (when (current-p)
           (let ((events (reverse (gethash id opencode-state--fetching))))
             (remhash id opencode-state--fetching)
             (opencode-state--store id entry)
             (dolist (event events)
               (opencode-state--dispatch id event t)))))
       (lambda (error)
         (when (current-p)
           (remhash id opencode-state--fetching)
           ;; A deleted session has nothing to follow.
           (unless (eql (opencode-error-status error) 404)
             (message "OpenCode: cannot follow session %s: %s" id (opencode-error-describe error)))))))))

;;;; Events

(defun opencode-state--event-session (event)
  "Return the ID of the session EVENT concerns, or nil."
  (let ((data (alist-get 'data event)))
    (or (alist-get 'sessionID data)
        (alist-get 'sessionID (alist-get 'form data)))))

(defun opencode-state--created-entry (event)
  "Return the entry of the session `session.created' EVENT announces."
  (let ((data (alist-get 'data event))
        (created (opencode-time-seconds (alist-get 'created event))))
    (opencode-state--make-entry
     :id (alist-get 'sessionID data) :title (alist-get 'title data)
     :directory (opencode-state--location-directory (alist-get 'location data))
     :project-id (alist-get 'projectID data) :parent-id (alist-get 'parentID data)
     :agent (alist-get 'agent data) :model (alist-get 'model data)
     :created created :updated created)))

(defun opencode-state--dispatch (id event replay)
  "Apply EVENT of session ID, fetching the session first when unknown.
REPLAY is passed to `opencode-state--apply'."
  (pcase (alist-get 'type event)
    ("session.created"
     (cond
      ((gethash id opencode-state--fetching)
       (push event (gethash id opencode-state--fetching)))
      ;; A known session's entry is at least as recent as its creation.
      ((not (opencode-state-get id))
       (opencode-state--store id (opencode-state--created-entry event)))))
    ("session.deleted"
     (remhash id opencode-state--fetching)
     (opencode-state--store id nil))
    (_
     (cond
      ((gethash id opencode-state--fetching)
       (push event (gethash id opencode-state--fetching)))
      ((opencode-state-get id)
       (opencode-state--store id (opencode-state--apply (copy-sequence (opencode-state-get id))
                                                        event replay)))
      (t (opencode-state--fetch id event))))))

(defun opencode-state--handle (event)
  "Apply EVENT to the registry when it follows the stream that delivered it.
Events received while bootstrapping wait for the bootstrap; before the
registry was ever bootstrapped on this stream there is nothing to apply
them to, and the next bootstrap covers them."
  (when (eq opencode-state--stream opencode-events--process)
    (when-let* ((id (opencode-state--event-session event))
                ;; Forms of no session belong to no entry.
                ((not (equal id "global"))))
      (cond
       (opencode-state--bootstrapping (push event opencode-state--held))
       (opencode-state--ready (opencode-state--dispatch id event nil))))))

;;;; Bootstrap

(defun opencode-state--install (entries)
  "Replace the registry with ENTRIES, running the hooks of every change."
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (puthash (plist-get entry :id) t ids))
    (dolist (id (hash-table-keys opencode-state--sessions))
      (unless (gethash id ids)
        (opencode-state--store id nil)))
    (dolist (entry entries)
      (opencode-state--store (plist-get entry :id) entry))))

(defun opencode-state--bootstrapped (entries)
  "Install bootstrapped ENTRIES, apply the events held meanwhile, call the waiters."
  (setq opencode-state--bootstrapping nil
        opencode-state--ready t
        opencode-state--retry-delay 1)
  (opencode-state--install entries)
  (let ((held (reverse opencode-state--held)))
    (setq opencode-state--held nil)
    (dolist (event held)
      (opencode-state--dispatch (opencode-state--event-session event) event t)))
  (let ((waiters (reverse opencode-state--waiters)))
    (setq opencode-state--waiters nil)
    (dolist (waiter waiters)
      (with-demoted-errors "OpenCode session state waiter: %S"
        (funcall waiter)))))

(defun opencode-state--cancel-retry ()
  "Cancel the pending retry of a failed bootstrap."
  (when (timerp opencode-state--retry-timer)
    (cancel-timer opencode-state--retry-timer))
  (setq opencode-state--retry-timer nil))

(defun opencode-state--bootstrap-failed (error)
  "Report the bootstrap's ERROR and retry later while the stream stays open."
  (setq opencode-state--bootstrapping nil
        opencode-state--held nil)
  (message "OpenCode: cannot load the state of the sessions: %s" (opencode-error-describe error))
  (opencode-state--cancel-retry)
  (let ((stream opencode-state--stream)
        (timer nil))
    (setq timer (run-at-time
                 opencode-state--retry-delay nil
                 (lambda ()
                   (when (eq opencode-state--retry-timer timer)
                     (setq opencode-state--retry-timer nil)
                     (when (and (eq stream opencode-state--stream)
                                (eq stream opencode-events--process)
                                (opencode-events-connected-p)
                                (not opencode-state--bootstrapping)
                                (not opencode-state--ready))
                       (opencode-state--bootstrap)))))
          opencode-state--retry-timer timer
          opencode-state--retry-delay (min 60 (* 2 opencode-state--retry-delay)))))

(defun opencode-state--bootstrap ()
  "Rebuild the registry from the service.
The newest unarchived sessions and the running ones are fetched, with
what the running ones wait on.  Events received before the requests
are covered by their answers; those received meanwhile are held."
  (let ((generation (cl-incf opencode-state--generation)))
    (setq opencode-state--bootstrapping t
          opencode-state--ready nil
          opencode-state--held nil)
    (clrhash opencode-state--fetching)
    (opencode-state--cancel-retry)
    (cl-labels ((current-p () (= generation opencode-state--generation))
                (fail (error) (when (current-p) (opencode-state--bootstrap-failed error)))
                (finish (infos active)
                  (opencode-state--complete
                   (mapcar (lambda (info)
                             (opencode-state--info-entry info (member (alist-get 'id info) active)))
                           infos)
                   (lambda (entries) (when (current-p) (opencode-state--bootstrapped entries)))
                   #'fail))
                (add-missing (infos active missing)
                  ;; Running sessions older than the page are fetched one by one.
                  (if (null missing)
                      (finish infos active)
                    (opencode-request
                     'get (concat "/api/session/" (car missing)) :directory nil :buffer nil
                     :then (lambda (response)
                             (when (current-p)
                               (add-missing (append infos (list (opencode-request-data response)))
                                            active (cdr missing))))
                     :else (lambda (error)
                             (when (current-p)
                               (if (eql (opencode-error-status error) 404)
                                   (add-missing infos active (cdr missing))
                                 (fail error))))))))
      (opencode-request
       'get "/api/session" :directory nil :buffer nil :else #'fail
       :query `((archived . "false") (order . "desc") (limit . ,opencode-state-recent-limit))
       :then (lambda (response)
               (when (current-p)
                 (let ((infos (opencode-request-data response)))
                   ;; Fetched after the list, a turn that ended in between shows as ended.
                   (opencode-request
                    'get "/api/session/active" :directory nil :buffer nil :else #'fail
                    :then (lambda (response)
                            (when (current-p)
                              (let ((active (opencode-state--active-ids response)))
                                (add-missing infos active
                                             (seq-remove (lambda (id)
                                                           (seq-find (lambda (info)
                                                                       (equal (alist-get 'id info) id))
                                                                     infos))
                                                         active)))))))))))))

(defun opencode-state--connected (_event)
  "Bootstrap the registry on the stream that just (re)connected."
  (setq opencode-state--stream opencode-events--process)
  (opencode-state--bootstrap))

(defun opencode-state-ensure (&optional callback)
  "Open the event stream and bootstrap the registry, then call CALLBACK.
CALLBACK, a function of no argument, runs at once when the registry is
ready on the current stream, else once it is.  The registry bootstraps
on every connection of the stream, whoever opens it; this also retries
a bootstrap that failed, and adopts a stream whose connection the
registry missed, having been loaded after it."
  (opencode-events-connect)
  (if (opencode-state-ready-p)
      (when callback (funcall callback))
    (when callback (push callback opencode-state--waiters))
    (when (opencode-events-connected-p)
      (cond
       ((eq opencode-state--stream opencode-events--process)
        (unless opencode-state--bootstrapping
          (opencode-state--bootstrap)))
       ((and opencode-events--process
             (eq opencode-events--greeted opencode-events--process))
        (opencode-state--connected nil))))))

(opencode-events-register "server.connected" #'opencode-state--connected)
;; A fork announces its new session by `session.forked' alone; being
;; unknown, the session is fetched.
(dolist (type '("session.created" "session.forked" "session.renamed" "session.moved" "session.deleted"
                "session.archived" "session.unarchived" "session.viewed"
                "session.execution.started" "session.execution.succeeded"
                "session.execution.failed" "session.execution.interrupted"
                "session.retry.scheduled" "session.step.started" "session.step.ended"
                "session.inbox.enqueued" "session.agent.selected" "session.model.selected"
                "permission.asked" "permission.replied"
                "form.created" "form.replied" "form.cancelled"))
  (opencode-events-register type #'opencode-state--handle))

;;;; The read marker

(defun opencode-state--directory (id)
  "Return the directory scoping requests about session ID, or nil when unknown."
  (or (plist-get (opencode-state-get id) :directory)
      (when-let* ((buffer (gethash id opencode-session-buffers))
                  ((buffer-live-p buffer)))
        (buffer-local-value 'default-directory buffer))))

(defun opencode-session-view (id &optional idle)
  "Mark session ID viewed up to IDLE, its last idle time by default.
IDLE is in float seconds.  Every client then sees the session read.
Nothing is sent when the session is not unread, nor twice for one
watermark while the first request is in flight."
  (when-let* ((entry (opencode-state-get id))
              ((opencode-state-unread-p entry))
              (milliseconds (opencode-state--ms (or idle (plist-get entry :idle))))
              ((not (eql (gethash id opencode-state--viewing) milliseconds))))
    (puthash id milliseconds opencode-state--viewing)
    (cl-flet ((settled ()
                (when (eql (gethash id opencode-state--viewing) milliseconds)
                  (remhash id opencode-state--viewing))))
      (opencode-request
       'post (format "/api/session/%s/view" id)
       :directory (opencode-state--directory id) :buffer nil
       :body `((idle . ,milliseconds))
       :then (lambda (_) (settled))
       :else (lambda (error)
               (settled)
               (message "OpenCode: cannot mark the session viewed: %s"
                        (opencode-error-describe error)))))))

(provide 'opencode-state)
;;; opencode-state.el ends here
