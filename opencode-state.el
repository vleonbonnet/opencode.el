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
;; ended and whether anyone saw that.  Session pickers, the send check
;; and other packages' lists and dashboards, such as drover's, read it
;; instead of asking the service.
;;
;; The service is authoritative.  Whenever the event stream (re)opens,
;; whoever opened it, the registry is rebuilt from REST: the first page
;; of the newest unarchived sessions, the running ones, and what those
;; wait on.  Events received meanwhile are held and applied over the
;; result, and the registry is then ready; after that, events keep it
;; current, and an event of a session it does not hold current fetches
;; that session first.  The other unarchived sessions are listed after,
;; a page at a time, newest first, each page parsed as it comes and
;; reported to clients at once.  Until the listing ends, a rebuild keeps
;; the entries of the previous stream not listed yet; it then drops the
;; ones the service no longer lists, except sessions whose buffer is
;; open, which are fetched, archived or not.  A session buffer opening
;; fetches its session too when the registry does not hold it.
;;
;; Unread is the service's, shared by every client: a session is unread
;; when its last idle transition is newer than the watermark
;; `opencode-session-view' advances.  The registry projects both times
;; as the service does, so the watermark it posts is one the service
;; accepts.  An entry fetched after an event may already reflect it, so
;; replaying such an event never advances the idle time twice.
;;
;; With my OpenCode fork, the registry also knows when each session's
;; provider prompt cache runs out (`opencode-state-cache'): listed when
;; the stream connects, updated by `session.cache.updated' after every
;; provider response, and listed again once the earliest known expiry
;; passes, since nothing reports an entry running out.  One timer serves
;; every session.
;;
;; Each entry also carries the session's context size, as the session
;; buffer's mode line shows it: the token counts of its newest assistant
;; message reporting usage, or, when a compaction completed since, the
;; service's estimate of the next request, which only a step measures
;; (`opencode-state-context-estimated-p').  Every step's end and every
;; compaction's end report it, but neither a session's info nor its
;; listing does, so a session the registry loaded rather than saw
;; created has it fetched, newest assistant message and compaction
;; first, only when a client asks (`opencode-state-context'), a few
;; sessions at a time.  A reconnection keeps the sizes of the sessions
;; the service has not updated since and that were not seen compacting.

;;; Code:

(require 'opencode-common)
(require 'opencode-events)
(require 'opencode-http)

(defcustom opencode-state-recent-limit nil
  "Number of the newest unarchived sessions loaded when the stream connects, or nil.
Nil loads them all, a page at a time.  Running sessions, sessions whose
buffer is open and any other session an event concerns join the
registry whatever this number."
  :type '(choice (const :tag "All" nil) natnum)
  :group 'opencode)

(defvar opencode-state--page-size 250
  "Number of sessions asked for per page of the listing.
Each page is parsed and installed at once, on the main thread.")

(defcustom opencode-state-context-requests 4
  "Number of sessions whose context size is fetched at once, at most.
See `opencode-state-context'."
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
Creation and removal count as changes, and so do the arrival of a
context size fetched for `opencode-state-context' and a change of the
session's prompt-cache expiry (`opencode-state-cache'), even for a
session the registry holds no entry of.  When many sessions change at
once, such as when a page of the session listing arrives, it runs once
with nil instead: clients then read the registry again."
  :type 'hook
  :group 'opencode)

(defconst opencode-state--keys
  '(:id :title :directory :project-id :parent-id :agent :model :context :context-estimated
        :running :retry :requests :outcome :error :idle :viewed :archived :created :updated :started)
  "Keys of a registry entry, in order.")

(defvar opencode-state--sessions (make-hash-table :test #'equal)
  "Session IDs mapped to their registry entries.")

(defvar opencode-state--children (make-hash-table :test #'equal)
  "Session IDs mapped to the IDs of their subagents the registry holds.
Kept as entries are stored, so that a session's subagents are found
without going through every entry, at every redisplay of a mode line.")

(defvar opencode-state--revision 0
  "Count of the changes of registry entries.
Values derived from several entries, such as what a session's
subagents do, are kept until it changes.")

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
  "IDs of sessions being fetched, mapped to (fetching . EVENTS).
EVENTS are those received meanwhile, newest first, applied once the
session's entry is in.")

(defvar opencode-state--current (make-hash-table :test #'equal)
  "IDs of the sessions whose entry is current on this stream, mapped to t.
A session deleted on this stream maps to `deleted'.  Entries of the
previous stream are not current, and events do not apply to them: their
session is fetched first.")

(defvar opencode-state--listing nil
  "Non-nil while the sessions after the first page are listed.")

(defvar opencode-state--loaded nil
  "Non-nil once every unarchived session was listed on this stream.")

(defvar opencode-state--list-timer nil
  "Timer retrying the page of the listing that failed.")

(defvar opencode-state--list-delay 1
  "Seconds before the next retry of a page of the listing that failed.")

(defvar opencode-state--batch nil
  "While changes are batched, a list whose cdr holds the IDs changed, newest first.")

(defvar opencode-state--waiters nil
  "Functions waiting for the registry to be ready, newest first.")

(defvar opencode-state--wanted nil
  "Non-nil once a client asked for the registry, which follows the event stream.")

(defvar opencode-state--retry-timer nil
  "Timer retrying a failed bootstrap.")

(defvar opencode-state--retry-delay 1
  "Seconds before the next retry of a failed bootstrap.")

(defvar opencode-state--viewing (make-hash-table :test #'equal)
  "Session IDs mapped to the idle watermark, in milliseconds, of their view in flight.")

(defvar opencode-state--caches (make-hash-table :test #'equal)
  "Session IDs mapped to their live prompt-cache expiry, as `opencode-state-cache' returns it.")

(defvar opencode-state--contexts (make-hash-table :test #'equal)
  "Session IDs mapped to how their context size stands.
`known' once events or a fetch settled it on this stream, `carried'
while the size settled on the previous stream waits for a fresh entry
to tell whether it is still current, `failed' when its fetch failed,
`queued' while waiting to be fetched, and a list unique to the fetch
while one runs.  The size of a session absent from it is not known;
one is fetched when a client asks for it.")

(defvar opencode-state--compacting (make-hash-table :test #'equal)
  "IDs of the sessions a compaction may run in, mapped to a unique mark.
A session joins when its compaction starts, and leaves when one ends or
fails, when a fetch of its size begun since finds none running, or when
its entry is dropped.  The service does not mark a session updated when
a compaction ends, so a reconnection fetches their size again rather
than keep it.")

(defvar opencode-state--context-queue nil
  "IDs of the sessions whose context size waits to be fetched, oldest first.")

(defvar opencode-state--context-timer nil
  "Timer starting the context-size fetches queued.")

(defvar opencode-state--cache-timer nil
  "Timer listing the expiries again once the earliest known one passes.")

(defvar opencode-state--cache-generation 0
  "Count of expiry listings; answers to a superseded one are ignored.")

(defvar opencode-state--cache-listing nil
  "Non-nil while an expiry listing is in flight.")

(defvar opencode-state--cache-held nil
  "Expiry events received while listing, newest first.")

(defvar opencode-state--cache-unsupported nil
  "Non-nil when the service cannot list expiries, until the stream reconnects.")

(defconst opencode-state--cache-margin 1
  "Seconds past an expiry before listing again.
The service drops an entry once its expiry is reached, an expiry it
rounds to the millisecond.")

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
  "Return non-nil once the registry is bootstrapped on the current event stream.
It then holds the newest sessions and the running ones, and follows
events; the older sessions may still be listed (see
`opencode-state-loaded-p')."
  (and opencode-state--ready
       (eq opencode-state--stream opencode-events--process)
       (opencode-events-connected-p)))

(defun opencode-state-loaded-p ()
  "Return non-nil once the registry holds every unarchived session of the stream.
That is once the listing that follows a bootstrap ended."
  (and opencode-state--loaded (opencode-state-ready-p)))

(defun opencode-state--retained-p (id)
  "Return non-nil when session ID has a buffer, which keeps it in the registry."
  (buffer-live-p (gethash id opencode-session-buffers)))

(defun opencode-state-get (id)
  "Return the registry entry of session ID, or nil.
An entry is a plist with the keys :id, :title, :directory (normalized),
:project-id, :parent-id, :agent, :model (a model reference), :context (the
context size in tokens, or nil; see `opencode-state-context'),
:context-estimated (non-nil when :context is the service's estimate; see
`opencode-state-context-estimated-p'), :running,
:retry (a plist :attempt :at :message, or nil), :requests (plists :kind
:id :summary :time, oldest first), :outcome (`succeeded', `failed' or
`interrupted'), :error (the last failure's message while it is unseen,
the status being `failed'; nil for a failure seen before the registry
loaded the session), :idle, :viewed, :archived, :created, :updated and
:started.  Times are float seconds since the epoch.  Read the context
size with `opencode-state-context', which fetches it when not known.
The registry holds every unarchived session, every session whose buffer
is open, and every other one an event concerned; lookups are constant
time."
  (gethash id opencode-state--sessions))

(defun opencode-state-sessions (&optional archived)
  "Return every registry entry, most recently updated first.
Archived sessions are included only when ARCHIVED is non-nil, or when
their buffer is open."
  (let ((entries nil))
    (maphash (lambda (id entry)
               (when (or archived (not (plist-get entry :archived)) (opencode-state--retained-p id))
                 (push entry entries)))
             opencode-state--sessions)
    (sort entries :key (lambda (entry) (or (plist-get entry :updated) 0)) :reverse t)))

(defun opencode-state-descendants (id)
  "Return the registry entries of the subagent sessions under session ID.
Children of children are included, at any depth, parents before their
children."
  (let ((children (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (descendants nil))
    (maphash (lambda (child entry)
               (when-let* ((parent (plist-get entry :parent-id)))
                 (push child (gethash parent children))))
             opencode-state--sessions)
    (let ((queue (gethash id children)))
      (puthash id t seen)
      (while queue
        (let ((child (pop queue)))
          (unless (gethash child seen)
            (puthash child t seen)
            (push (gethash child opencode-state--sessions) descendants)
            (setq queue (append queue (gethash child children)))))))
    (nreverse descendants)))

(defun opencode-state-subagent-statuses (id)
  "Return the statuses of the subagents of session ID in the middle of a turn.
Subagents at any depth count, those the registry holds; the value lists
each of `waiting', `working' and `retrying' at most once, nil when no
subagent is in the middle of a turn.  It costs the subagents of ID, not
every entry."
  (let ((seen (make-hash-table :test #'equal))
        (queue (gethash id opencode-state--children))
        (statuses nil))
    (puthash id t seen)
    (while queue
      (let ((child (pop queue)))
        (unless (gethash child seen)
          (puthash child t seen)
          (let ((status (opencode-state-status child)))
            (when (memq status '(waiting working retrying))
              (cl-pushnew status statuses)))
          (setq queue (append (gethash child opencode-state--children) queue)))))
    statuses))

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

(defun opencode-state-cache (id)
  "Return the live prompt-cache expiry of session ID, or nil.
It describes the provider's cache entry that a next turn extending the
session's last request would reuse, as a plist: :expires, when it runs
out, in float seconds since the epoch; :ttl, its retention window in
seconds, which every provider response reusing it starts over;
:tokens, the prompt tokens it covers; and :prompt-tokens, all the
prompt tokens of the session's last request, uncached input and cache
reads and writes, of which :tokens is the share cached, or nil from a
service that does not tell it.  Nil when no entry is live, the
service cannot tell (see `opencode-state-cache-supported-p'), or the
expiry is not known yet.  Nothing reports an entry running out, so
callers count down to :expires themselves; the registry lists the
expiries again once one passes, as a longer-lived entry of a shorter
prefix may then remain, and runs `opencode-state-updated-functions'
with every session whose expiry changed."
  (when-let* ((cache (gethash id opencode-state--caches))
              ((> (plist-get cache :expires) (float-time))))
    cache))

(defun opencode-state-cache-remaining (id)
  "Return the seconds left before session ID's prompt cache expires, or nil.
See `opencode-state-cache'."
  (when-let* ((cache (opencode-state-cache id)))
    (- (plist-get cache :expires) (float-time))))

(defun opencode-state-cache-supported-p ()
  "Return nil when the service cannot list prompt-cache expiries.
Only my OpenCode fork can.  Until the service first answers, and again
after the stream reconnects, it is taken to be able to."
  (not opencode-state--cache-unsupported))

;;;; Changes

(defun opencode-state--run-hook (hook &rest arguments)
  "Run HOOK with ARGUMENTS; an error of one function does not stop the others."
  (apply #'run-hook-wrapped hook
         (lambda (function &rest arguments)
           (with-demoted-errors "OpenCode session state hook: %S"
             (apply function arguments))
           nil)
         arguments))

(defun opencode-state--updated (id)
  "Report a change of session ID, or note it for the batch of changes being made."
  (if opencode-state--batch
      (push id (cdr opencode-state--batch))
    (opencode-state--run-hook 'opencode-state-updated-functions id)))

(defun opencode-state--batched (function)
  "Call FUNCTION, reporting the sessions it changes once rather than one by one.
`opencode-state-updated-functions' then runs once: with the session ID
when only one changed, else with nil.  A batch within a batch joins it."
  (if opencode-state--batch
      (funcall function)
    (let ((batch (list 'batch)))
      (unwind-protect
          (let ((opencode-state--batch batch))
            (funcall function))
        (let ((ids (delete-dups (cdr batch))))
          (when ids
            (opencode-state--run-hook 'opencode-state-updated-functions (and (null (cdr ids)) (car ids)))))))))

(defun opencode-state--store (id entry)
  "Make ENTRY the entry of session ID, nil removing it, and run the hooks on a change."
  (let ((old (gethash id opencode-state--sessions)))
    (unless (equal old entry)
      (cl-incf opencode-state--revision)
      (let ((old-parent (plist-get old :parent-id))
            (parent (plist-get entry :parent-id)))
        (unless (and old entry (equal old-parent parent))
          (when old-parent
            (let ((siblings (delete id (gethash old-parent opencode-state--children))))
              (if siblings
                  (puthash old-parent siblings opencode-state--children)
                (remhash old-parent opencode-state--children))))
          (when (and entry parent)
            (push id (gethash parent opencode-state--children)))))
      (if entry
          (puthash id entry opencode-state--sessions)
        (remhash id opencode-state--sessions)
        (remhash id opencode-state--viewing)
        (remhash id opencode-state--contexts)
        (remhash id opencode-state--compacting))
      (let ((before (and old (opencode-state-status old)))
            (after (and entry (opencode-state-status entry))))
        (unless (eq before after)
          (opencode-state--run-hook 'opencode-state-change-functions id before after)))
      (opencode-state--updated id))))

(defun opencode-state--put (entry)
  "Make ENTRY, built from the service or an event on this stream, its session's current entry.
A stale entry's context size is kept while the service has not updated
the session since."
  (let ((id (plist-get entry :id)))
    (puthash id t opencode-state--current)
    (opencode-state--store id (opencode-state--context-carry entry))))

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
        ((or "session.step.ended" "session.step.failed")
         (assign :updated created)
         ;; A step failing after the provider answered reports that usage too.
         (when-let* ((size (opencode-context-size (alist-get 'tokens data))))
           (assign :context size :context-estimated nil)))
        ;; Its usage is the compaction request's, which read the
        ;; conversation it replaced.  Without the service's estimate, from
        ;; a native compaction or an older service, the size is unknown
        ;; until the next step.  The service does not mark the session
        ;; updated.
        ("session.compaction.ended"
         (let ((context (alist-get 'context data)))
           (assign :context (and (numberp context) context)
                   :context-estimated (numberp context))))
        ("session.inbox.enqueued" (assign :updated created))
        ;; The messages after the boundary are gone, and with them maybe
        ;; the newest usage; the size is fetched again when asked for.
        ("session.revert.committed" (assign :context nil :context-estimated nil :updated created))
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
Running sessions get their pending requests, start and retry, sessions
whose failure is unseen the message of that failure, reused when the
registry knows that failure already.  A failure seen already is shown
by no status, so its message is not fetched: loading every session
would otherwise ask for one per failed session.  ELSE receives the
`opencode-error' of a failed request for pending requests; the rest is
best effort."
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
            (cond
             ((and (eq (plist-get known :outcome) 'failed)
                   (plist-get known :error)
                   (equal (plist-get known :idle) (plist-get entry :idle)))
              (plist-put entry :error (plist-get known :error)))
             ((opencode-state-unread-p entry)
              (cl-incf pending)
              (opencode-state--fetch-error
               entry (lambda (message) (plist-put entry :error message) (done))))))))
      (done))))

(defun opencode-state--load (ids then)
  "Build the entries of sessions IDS from the service, then call THEN with them.
THEN receives an alist mapping each ID to its entry, to nil when the
service has no such session, or to the `opencode-error' of a failure.
The infos come first, then the running sessions, once for all of them,
so that a turn ending in between shows as ended."
  (let ((pending (length ids))
        (infos nil)
        (results nil))
    (cl-labels ((failed (error)
                  (funcall then (append results
                                        (mapcar (lambda (info) (cons (alist-get 'id info) error)) infos))))
                (fetched ()
                  (when (zerop (cl-decf pending))
                    (if (null infos)
                        (funcall then results)
                      (opencode-request
                       'get "/api/session/active" :directory nil :buffer nil :else #'failed
                       :then (lambda (response)
                               (let ((active (opencode-state--active-ids response)))
                                 (opencode-state--complete
                                  (mapcar (lambda (info)
                                            (opencode-state--info-entry info (member (alist-get 'id info) active)))
                                          (nreverse infos))
                                  (lambda (entries)
                                    (funcall then (append results
                                                          (mapcar (lambda (entry) (cons (plist-get entry :id) entry))
                                                                  entries))))
                                  #'failed))))))))
      (if (null ids)
          (funcall then nil)
        (dolist (id ids)
          (opencode-request
           'get (concat "/api/session/" id) :directory nil :buffer nil
           :then (lambda (response)
                   (push (opencode-request-data response) infos)
                   (fetched))
           :else (lambda (error)
                   (push (cons id (unless (eql (opencode-error-status error) 404) error)) results)
                   (fetched))))))))

(defun opencode-state--fetch (ids &optional event)
  "Fetch sessions IDS, which the registry does not hold current, and follow them.
EVENT, of the one session of IDS that called for it, and the events of
each session received meanwhile are applied once its entry is in.  A
session the service does not have is dropped quietly."
  (dolist (id ids)
    (puthash id (cons 'fetching (and event (list event))) opencode-state--fetching))
  (let ((generation opencode-state--generation)
        (fetching (mapcar (lambda (id) (cons id (gethash id opencode-state--fetching))) ids)))
    (opencode-state--load
     ids
     (lambda (results)
       (when (= generation opencode-state--generation)
         (opencode-state--batched
          (lambda ()
            (pcase-dolist (`(,id . ,result) results)
              ;; Unless deleted meanwhile.
              (when (eq (gethash id opencode-state--fetching) (alist-get id fetching nil nil #'equal))
                (let ((events (reverse (cdr (gethash id opencode-state--fetching)))))
                  (remhash id opencode-state--fetching)
                  (cond
                   ((opencode-error-p result)
                    (message "OpenCode: cannot follow session %s: %s" id (opencode-error-describe result)))
                   ((null result)
                    (puthash id 'deleted opencode-state--current)
                    (opencode-state--store id nil))
                   (t
                    (opencode-state--put result)
                    (dolist (event events)
                      (opencode-state--dispatch id event t))))))))))))))

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

(defun opencode-state--current-p (id)
  "Return non-nil when the registry's entry of session ID is current on this stream."
  (eq (gethash id opencode-state--current) t))

(defun opencode-state--dispatch (id event replay)
  "Apply EVENT of session ID, fetching the session first unless its entry is current.
REPLAY is passed to `opencode-state--apply'."
  (pcase (alist-get 'type event)
    ("session.created"
     (cond
      ((gethash id opencode-state--fetching)
       (push event (cdr (gethash id opencode-state--fetching))))
      ;; A current entry is at least as recent as its creation.
      ((opencode-state--current-p id))
      ((opencode-state-get id) (opencode-state--fetch (list id) event))
      (t
       ;; A new session has no usage yet.
       (puthash id 'known opencode-state--contexts)
       (opencode-state--put (opencode-state--created-entry event)))))
    ("session.deleted"
     (remhash id opencode-state--fetching)
     (puthash id 'deleted opencode-state--current)
     (opencode-state--cache-set id nil)
     (opencode-state--cache-schedule)
     (opencode-state--store id nil))
    (_
     (cond
      ((gethash id opencode-state--fetching)
       (push event (cdr (gethash id opencode-state--fetching))))
      ((opencode-state--current-p id)
       (let ((entry (opencode-state--apply (copy-sequence (opencode-state-get id)) event replay)))
         ;; Before the hooks run, which may ask for the context size.
         (opencode-state--context-follow id event)
         (opencode-state--store id entry)))
      (t (opencode-state--fetch (list id) event))))))

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

(defun opencode-state--page-limit (count)
  "Return how many sessions to ask for after COUNT listed, or 0 once enough are."
  (if opencode-state-recent-limit
      (max 0 (min opencode-state--page-size (- opencode-state-recent-limit count)))
    opencode-state--page-size))

(defun opencode-state--bootstrapped (entries listing)
  "Install bootstrapped ENTRIES, apply the events held meanwhile, call the waiters.
Then list the other sessions from LISTING, a plist (:next :previous
:count) of the first page's cursors and size."
  (setq opencode-state--bootstrapping nil
        opencode-state--ready t
        opencode-state--retry-delay 1)
  (opencode-state--batched
   (lambda ()
     (mapc #'opencode-state--put entries)
     (let ((held (reverse opencode-state--held)))
       (setq opencode-state--held nil)
       (dolist (event held)
         (opencode-state--dispatch (opencode-state--event-session event) event t)))))
  (opencode-state--context-schedule)
  (let ((generation opencode-state--generation)
        (waiters (reverse opencode-state--waiters)))
    (setq opencode-state--waiters nil
          opencode-state--listing t)
    (dolist (waiter waiters)
      (with-demoted-errors "OpenCode session state waiter: %S"
        (funcall waiter)))
    (opencode-state--list generation listing)))

(defun opencode-state--cancel-retry ()
  "Cancel the pending retries of a failed bootstrap or page of the listing."
  (dolist (timer (list opencode-state--retry-timer opencode-state--list-timer))
    (when (timerp timer)
      (cancel-timer timer)))
  (setq opencode-state--retry-timer nil
        opencode-state--list-timer nil))

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
The first page of the newest unarchived sessions and the running ones
are fetched, with what the running ones wait on; the registry is then
ready, and lists the other sessions.  Events received before the
requests are covered by their answers; those received meanwhile are
held.  The entries of the previous stream stay until the listing ends,
none of them current."
  (let ((generation (cl-incf opencode-state--generation))
        (limit (opencode-state--page-limit 0)))
    (setq opencode-state--bootstrapping t
          opencode-state--ready nil
          opencode-state--held nil
          opencode-state--listing nil
          opencode-state--loaded nil
          opencode-state--list-delay 1)
    (clrhash opencode-state--fetching)
    (clrhash opencode-state--current)
    (opencode-state--cancel-retry)
    (opencode-state--context-reset)
    (cl-labels ((current-p () (= generation opencode-state--generation))
                (fail (error) (when (current-p) (opencode-state--bootstrap-failed error)))
                (finish (infos active listing)
                  (opencode-state--complete
                   (mapcar (lambda (info)
                             (opencode-state--info-entry info (member (alist-get 'id info) active)))
                           infos)
                   (lambda (entries) (when (current-p) (opencode-state--bootstrapped entries listing)))
                   #'fail))
                (add-missing (infos active missing listing)
                  ;; Running sessions older than the page are fetched one by one.
                  (if (null missing)
                      (finish infos active listing)
                    (opencode-request
                     'get (concat "/api/session/" (car missing)) :directory nil :buffer nil
                     :then (lambda (response)
                             (when (current-p)
                               (add-missing (append infos (list (opencode-request-data response)))
                                            active (cdr missing) listing)))
                     :else (lambda (error)
                             (when (current-p)
                               (if (eql (opencode-error-status error) 404)
                                   (add-missing infos active (cdr missing) listing)
                                 (fail error)))))))
                (listed (infos cursor)
                  (let ((listing (list :next (and (= (length infos) limit) (alist-get 'next cursor))
                                       :previous (alist-get 'previous cursor)
                                       :count (length infos))))
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
                                                          active)
                                              listing))))))))
      ;; The service refuses an empty page.
      (if (zerop limit)
          (listed nil nil)
        (opencode-request
         'get "/api/session" :directory nil :buffer nil :else #'fail
         :query `((archived . "false") (order . "desc") (limit . ,limit))
         :then (lambda (response)
                 (when (current-p)
                   (listed (opencode-request-data response) (alist-get 'cursor response)))))))))

;;;; Listing

;; After the first page, the other unarchived sessions are listed a page
;; at a time, newest first, by the cursor each page answers with, which
;; anchors the next page on the last session's update time.  A session
;; updated meanwhile moves ahead of the first page, where the pages
;; after never find it: it is listed last, from the first page's cursor
;; going the other way.  The sessions of a page are neither running, the
;; running ones were fetched with the first page, nor waiting on anything
;; then; one an event concerned since is fetched by the event and left
;; alone, its entry being newer.

(defun opencode-state--install (infos then fail)
  "Install the entries of session INFOS the registry holds no current entry of.
THEN is called with no argument once they are in, FAIL with the
`opencode-error' of a failed request."
  (let ((generation opencode-state--generation))
    (cl-flet ((fresh-p (id)
                (not (or (gethash id opencode-state--current)
                         (gethash id opencode-state--fetching)))))
      (opencode-state--complete
       (seq-keep (lambda (info) (when (fresh-p (alist-get 'id info)) (opencode-state--info-entry info)))
                 infos)
       (lambda (entries)
         (when (= generation opencode-state--generation)
           ;; An event may have settled one meanwhile.
           (opencode-state--batched
            (lambda ()
              (dolist (entry entries)
                (when (fresh-p (plist-get entry :id))
                  (opencode-state--put entry)))))
           (funcall then)))
       fail))))

(defun opencode-state--list (generation listing)
  "List the sessions after the first page of bootstrap GENERATION.
LISTING is a plist: :next, the cursor of the next page, or nil once
none follows; :previous, the cursor of the sessions updated since the
first page; :count, the number of sessions listed so far."
  (cl-labels ((current-p () (= generation opencode-state--generation))
              (page (query then)
                (opencode-request
                 'get "/api/session" :directory nil :buffer nil :query query
                 :then (lambda (response)
                         (when (current-p)
                           (let ((infos (opencode-request-data response)))
                             (opencode-state--install
                              infos
                              (lambda ()
                                (setq opencode-state--list-delay 1)
                                (funcall then infos (alist-get 'cursor response)))
                              (lambda (error) (retry error query then))))))
                 :else (lambda (error) (when (current-p) (retry error query then)))))
              (retry (error query then)
                (when (current-p)
                  (message "OpenCode: cannot list the sessions, retrying: %s" (opencode-error-describe error))
                  (opencode-state--cancel-retry)
                  (let ((timer nil))
                    (setq timer (run-at-time
                                 opencode-state--list-delay nil
                                 (lambda ()
                                   (when (eq opencode-state--list-timer timer)
                                     (setq opencode-state--list-timer nil)
                                     (when (and (current-p) (opencode-state-ready-p))
                                       (page query then)))))
                          opencode-state--list-timer timer
                          opencode-state--list-delay (min 60 (* 2 opencode-state--list-delay))))))
              (newer (cursor)
                (if (null cursor)
                    (opencode-state--listed generation)
                  (page `((cursor . ,cursor) (limit . ,opencode-state--page-size))
                        (lambda (infos cursor)
                          (newer (and (= (length infos) opencode-state--page-size)
                                      (alist-get 'previous cursor)))))))
              (older (cursor count paged)
                (let ((limit (opencode-state--page-limit count)))
                  (cond
                   ((and cursor (> limit 0))
                    ;; A cursor carries the first page's filters and order.
                    (page `((cursor . ,cursor) (limit . ,limit))
                          (lambda (infos cursor)
                            (older (and (= (length infos) limit) (alist-get 'next cursor))
                                   (+ count (length infos)) t))))
                   ;; With the first page alone, nothing could move ahead of it unseen.
                   (paged (newer (plist-get listing :previous)))
                   (t (opencode-state--listed generation))))))
    (when (current-p)
      (older (plist-get listing :next) (plist-get listing :count) nil))))

(defun opencode-state--listed (generation)
  "End the listing of bootstrap GENERATION.
Entries still not current are of sessions the service no longer lists
unarchived: they are dropped, except those whose buffer is open, which
are fetched, as are the sessions of open buffers the registry lacks."
  (when (= generation opencode-state--generation)
    (let ((stale nil)
          (wanted nil))
      (maphash (lambda (id _entry)
                 (unless (or (gethash id opencode-state--current) (gethash id opencode-state--fetching))
                   (if (opencode-state--retained-p id) (push id wanted) (push id stale))))
               opencode-state--sessions)
      (maphash (lambda (id buffer)
                 (when (and (buffer-live-p buffer)
                            (not (gethash id opencode-state--sessions))
                            (not (gethash id opencode-state--current)))
                   (push id wanted)))
               opencode-session-buffers)
      (setq opencode-state--listing nil
            opencode-state--loaded t)
      (opencode-state--batched
       (lambda ()
         (dolist (id stale)
           (opencode-state--store id nil))))
      (when-let* ((ids (seq-remove (lambda (id) (gethash id opencode-state--fetching)) wanted)))
        (opencode-state--fetch ids)))))

(defun opencode-state--buffer-changed (id)
  "Have the registry hold session ID once its buffer opened.
Until the listing that follows a bootstrap ends, its end does.  For
`opencode-session-buffer-functions'."
  (when (and (opencode-state--retained-p id)
             (opencode-state-loaded-p)
             (not (gethash id opencode-state--current))
             (not (gethash id opencode-state--fetching)))
    (opencode-state--fetch (list id))))

(add-hook 'opencode-session-buffer-functions #'opencode-state--buffer-changed)

(defun opencode-state--connected (_event)
  "Bootstrap the registry and list the expiries on the stream that just (re)connected.
A service restarted meanwhile, maybe another build, may list expiries
the previous one could not."
  (setq opencode-state--stream opencode-events--process
        opencode-state--cache-unsupported nil
        opencode-state--cache-held nil)
  (opencode-state--bootstrap)
  (opencode-state--cache-list))

(defun opencode-state-ensure (&optional callback)
  "Open the event stream and bootstrap the registry, then call CALLBACK.
CALLBACK, a function of no argument, runs at once when the registry is
ready on the current stream, else once it is.  The registry bootstraps
on every connection of the stream, whoever opens it; this also retries
a bootstrap that failed, and adopts a stream whose connection the
registry missed, having been loaded after it."
  (setq opencode-state--wanted t)
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

(defun opencode-state--wants-events ()
  "Return non-nil once the registry has clients, which it keeps current from the stream."
  opencode-state--wanted)

(add-hook 'opencode-events-wanted-functions #'opencode-state--wants-events)
;; A fork announces its new session by `session.forked' alone; being
;; unknown, the session is fetched.
(dolist (type '("session.created" "session.forked" "session.renamed" "session.moved" "session.deleted"
                "session.archived" "session.unarchived" "session.viewed"
                "session.execution.started" "session.execution.succeeded"
                "session.execution.failed" "session.execution.interrupted"
                "session.retry.scheduled" "session.step.started" "session.step.ended"
                "session.step.failed" "session.revert.committed" "session.inbox.enqueued"
                "session.compaction.started" "session.compaction.ended" "session.compaction.failed"
                "session.agent.selected" "session.model.selected"
                "permission.asked" "permission.replied"
                "form.created" "form.replied" "form.cancelled"))
  (opencode-events-register type #'opencode-state--handle))

;;;; Context size

(defun opencode-state-context (id)
  "Return the context size of session ID in tokens, or nil.
That is what the session's next request sends again, as its buffer's
mode line shows it: the token counts of its newest assistant message
reporting usage, which `opencode-context-display' shows against its
model's context size.  Every step's end updates it.  Once a compaction
completed after that message, it is the service's estimate of the next
request until a step measures it, which
`opencode-state-context-estimated-p' tells; nil when the service gave
none, as for a native compaction.

Nil when no step reported usage yet, the registry holds no entry of ID,
or the size is not known yet.  Neither a session's info nor its listing
has it, so the first call for a session loaded rather than seen created
fetches it in the background, at most `opencode-state-context-requests'
sessions at a time; `opencode-state-updated-functions' run once it
arrives, as on every change.  A failed fetch leaves it nil until the
stream reconnects.  This never waits, and is safe at redisplay."
  (when-let* ((entry (opencode-state-get id)))
    (unless (gethash id opencode-state--contexts)
      (opencode-state--context-enqueue id))
    (plist-get entry :context)))

(defun opencode-state-context-estimated-p (id)
  "Return non-nil when the context size of session ID is an estimate.
That is the size `opencode-state-context' returns, the service's
estimate of the next request after a compaction, which the next step
replaces with the measured size.  Like the size, it is known only once
`opencode-state-context' was asked for it."
  (plist-get (opencode-state-get id) :context-estimated))

(defun opencode-state--context-pending-p (id)
  "Return non-nil when the context size of session ID is queued or being fetched."
  (let ((state (gethash id opencode-state--contexts)))
    (or (eq state 'queued) (consp state))))

(defun opencode-state--context-enqueue (id)
  "Queue the fetch of session ID's context size, superseding one in flight."
  (puthash id 'queued opencode-state--contexts)
  (unless (member id opencode-state--context-queue)
    (setq opencode-state--context-queue (nconc opencode-state--context-queue (list id))))
  (opencode-state--context-schedule))

(defun opencode-state--context-schedule ()
  "Start the queued context-size fetches from a timer.
Clients ask from redisplay, and a timer also lets the asks of one
redisplay queue up before any starts."
  (when (and opencode-state--context-queue (not (timerp opencode-state--context-timer)))
    (setq opencode-state--context-timer
          (run-at-time 0 nil #'opencode-state--context-pump))))

(defun opencode-state--context-running ()
  "Return the number of context-size fetches in flight."
  (let ((count 0))
    (maphash (lambda (_id state) (when (consp state) (cl-incf count)))
             opencode-state--contexts)
    count))

(defun opencode-state--context-pump ()
  "Start the queued context-size fetches, up to `opencode-state-context-requests'.
While the registry is not ready they wait: its bootstrap queues them
again and starts them."
  (setq opencode-state--context-timer nil)
  (when (opencode-state-ready-p)
    (while (and opencode-state--context-queue
                (< (opencode-state--context-running) (max 1 opencode-state-context-requests)))
      (let ((id (pop opencode-state--context-queue)))
        (when (and (eq (gethash id opencode-state--contexts) 'queued)
                   (opencode-state-get id))
          (opencode-state--context-fetch id))))))

(defun opencode-state--context-fetch (id)
  "Fetch the context size of session ID into its entry.
Assistant messages are fetched one at a time from the newest until one
reports usage, usually the first: only a step still running, or one
that failed before the provider answered, reports none.  Compactions
are fetched the same way until a completed one, or one older than that
message: a completed compaction newer than it holds the size, the
service's estimate, as the buffer replaying the transcript finds.  An
event settling the size meanwhile supersedes the fetch, since the
answer may predate it."
  (let ((token (list 'fetching))
        (directory (plist-get (opencode-state-get id) :directory))
        (compacting (gethash id opencode-state--compacting)))
    (puthash id token opencode-state--contexts)
    (cl-labels ((current-p () (eq (gethash id opencode-state--contexts) token))
                (settle (state &optional size estimated)
                  (when (current-p)
                    (puthash id state opencode-state--contexts)
                    (when-let* (((eq state 'known))
                                (entry (opencode-state-get id)))
                      (opencode-state--store id (plist-put (plist-put (copy-sequence entry) :context size)
                                                           :context-estimated estimated))))
                  (opencode-state--context-schedule))
                (created (message) (alist-get 'created (alist-get 'time message)))
                (newest (type accept found &optional cursor)
                  ;; Calls FOUND with the newest message of TYPE that ACCEPT takes, or nil.
                  (opencode-request
                   'get (format "/api/session/%s/message" id)
                   :directory directory :buffer nil
                   ;; A cursor carries its order, and the service refuses both.
                   :query `((limit . 1) (type . ,type)
                            ,@(if cursor `((cursor . ,cursor)) '((order . "desc"))))
                   :else (lambda (_) (settle 'failed))
                   :then (lambda (response)
                           (let ((messages (opencode-request-data response))
                                 (next (alist-get 'next (alist-get 'cursor response))))
                             (if-let* ((message (seq-find accept messages)))
                                 (funcall found message)
                               (if (and messages next (current-p))
                                   (newest type accept found next)
                                 (funcall found nil)))))))
                (compared (assistant)
                  ;; Messages are created as they are appended, so creation
                  ;; times order them as the transcript does.
                  (let ((after (created assistant))
                        (running nil))
                    (newest "compaction"
                            (lambda (message)
                              ;; One running is the newest, the first page.
                              (when (equal (alist-get 'status message) "running")
                                (setq running t))
                              (or (equal (alist-get 'status message) "completed")
                                  (and after (numberp (created message)) (<= (created message) after))))
                            (lambda (compaction)
                              (when (and compacting (not running) (current-p)
                                         (eq (gethash id opencode-state--compacting) compacting))
                                (remhash id opencode-state--compacting))
                              (if (and compaction
                                       (equal (alist-get 'status compaction) "completed")
                                       (or (null after) (> (or (created compaction) 0) after)))
                                  (let ((context (alist-get 'context compaction)))
                                    (settle 'known (and (numberp context) context) (numberp context)))
                                (settle 'known (opencode-context-size (alist-get 'tokens assistant)))))))))
      (newest "assistant" (lambda (message) (opencode-context-size (alist-get 'tokens message)))
              (lambda (assistant)
                (if (current-p)
                    (compared assistant)
                  ;; Superseded: its slot is free for the fetches queued.
                  (opencode-state--context-schedule)))))))

(defun opencode-state--context-follow (id event)
  "Note how EVENT of session ID leaves its context size known."
  (pcase (alist-get 'type event)
    ((or "session.step.ended" "session.step.failed")
     (when (opencode-context-size (alist-get 'tokens (alist-get 'data event)))
       ;; Newer than the answer of a fetch in flight, which is dropped.
       (puthash id 'known opencode-state--contexts)))
    ;; Unique, so that a fetch which began before cannot remove it.
    ("session.compaction.started" (puthash id (list 'started) opencode-state--compacting))
    ("session.compaction.failed" (remhash id opencode-state--compacting))
    ("session.compaction.ended"
     (remhash id opencode-state--compacting)
     (puthash id 'known opencode-state--contexts))
    ("session.revert.committed"
     (if (opencode-state--context-pending-p id)
         ;; The answer in flight may predate the revert.
         (opencode-state--context-enqueue id)
       (remhash id opencode-state--contexts)))))

(defun opencode-state--context-reset ()
  "Ready the context sizes for a rebuild of the registry.
A known size becomes `carried': it stays current only while the
service has not updated its session since, which
`opencode-state--context-carry' decides once a fresh entry replaces
the session's.  The end of a compaction does not update a session, so
the size of one seen compacting is forgotten at once, and fetched again
when asked for, until a fetch begun since finds none running.  Fetches
in flight answer for the stream that dropped: they are queued again,
ahead of those a client asked for meanwhile.  A failed fetch is tried
again when next asked for."
  (let ((flying nil)
        (failed nil)
        (carried nil)
        (compacting nil))
    (maphash (lambda (id state)
               (cond ((consp state) (push id flying))
                     ((eq state 'failed) (push id failed))
                     ((not (memq state '(known carried))))
                     ((gethash id opencode-state--compacting) (push id compacting))
                     (t (push id carried))))
             opencode-state--contexts)
    (dolist (id (append failed compacting))
      (remhash id opencode-state--contexts))
    (dolist (id carried)
      (puthash id 'carried opencode-state--contexts))
    (dolist (id flying)
      (puthash id 'queued opencode-state--contexts))
    (opencode-state--batched
     (lambda ()
       (dolist (id compacting)
         (when-let* ((entry (opencode-state-get id)))
           (opencode-state--store id (plist-put (plist-put (copy-sequence entry) :context nil)
                                                :context-estimated nil))))))
    (setq opencode-state--context-queue
          (seq-filter (lambda (id) (eq (gethash id opencode-state--contexts) 'queued))
                      (delete-dups (append (nreverse flying) opencode-state--context-queue nil))))))

(defun opencode-state--context-carry (entry)
  "Return ENTRY, about to replace its session's entry, with the context size still current.
A size known on this stream is current, and so is its estimate mark.
One `carried' from the previous stream stays current while the service
has not updated the session since, the end of a step updating it, and
no compaction was seen to start without ending; otherwise it is
forgotten, and fetched again when asked for."
  (let* ((id (plist-get entry :id))
         (old (opencode-state-get id))
         (state (gethash id opencode-state--contexts)))
    (cl-flet ((keep ()
                (setq entry (plist-put (plist-put entry :context (plist-get old :context))
                                       :context-estimated (plist-get old :context-estimated)))))
      (when old
        (pcase state
          ('known (keep))
          ('carried
           (let ((before (opencode-state--ms (plist-get old :updated)))
                 (after (opencode-state--ms (plist-get entry :updated))))
             (if (and before after (<= after before)
                      (not (gethash id opencode-state--compacting)))
                 (progn (puthash id 'known opencode-state--contexts)
                        (keep))
               (remhash id opencode-state--contexts)))))))
    entry))

;;;; Prompt-cache expiry

(defun opencode-state--cache-expiry (expiry)
  "Return the registry's form of the service's EXPIRY, or nil when absent or past."
  (let-alist expiry
    (when-let* ((expires (opencode-time-seconds .expires))
                ((> expires (float-time))))
      (list :expires expires :ttl (and (numberp .ttl) (/ .ttl 1000.0)) :tokens .tokens
            :prompt-tokens .promptTokens))))

(defun opencode-state--cache-set (id cache)
  "Make CACHE the prompt-cache expiry of session ID, nil removing it.
The update hook runs when it changed."
  (unless (equal cache (gethash id opencode-state--caches))
    (if cache
        (puthash id cache opencode-state--caches)
      (remhash id opencode-state--caches))
    (opencode-state--updated id)))

(defun opencode-state--cache-install (data)
  "Replace the known expiries with DATA, the service's listing."
  (let ((caches (make-hash-table :test #'equal)))
    (dolist (pair data)
      (when-let* ((cache (opencode-state--cache-expiry (cdr pair))))
        (puthash (format "%s" (car pair)) cache caches)))
    (opencode-state--batched
     (lambda ()
       (dolist (id (hash-table-keys opencode-state--caches))
         (unless (gethash id caches)
           (opencode-state--cache-set id nil)))
       (maphash #'opencode-state--cache-set caches)))))

(defun opencode-state--cache-prune ()
  "Forget the expiries that passed."
  (let ((now (float-time)))
    (dolist (id (hash-table-keys opencode-state--caches))
      (when (<= (plist-get (gethash id opencode-state--caches) :expires) now)
        (opencode-state--cache-set id nil)))))

(defun opencode-state--cache-schedule ()
  "Arm the one timer listing the expiries again once the earliest known passes.
No timer runs while none is known."
  (when (timerp opencode-state--cache-timer)
    (cancel-timer opencode-state--cache-timer))
  (setq opencode-state--cache-timer nil)
  (let ((earliest nil))
    (maphash (lambda (_id cache)
               (let ((expires (plist-get cache :expires)))
                 (when (or (null earliest) (< expires earliest))
                   (setq earliest expires))))
             opencode-state--caches)
    (when earliest
      ;; Absolute, as the service's expiries are.
      (setq opencode-state--cache-timer
            (run-at-time (time-convert (+ earliest opencode-state--cache-margin) 'list)
                         nil #'opencode-state--cache-expired)))))

(defun opencode-state--cache-listable-p ()
  "Return non-nil when the service can be asked for the expiries now.
While the stream is down, its reconnection lists them."
  (and (not opencode-state--cache-unsupported)
       (eq opencode-state--stream opencode-events--process)
       (opencode-events-connected-p)))

(defun opencode-state--cache-expired ()
  "Forget the expiries that passed, then list them again.
Nothing reports an entry running out, and once it has, a longer-lived
entry of a shorter prefix, such as a system prompt cached for an hour
under a conversation cached for five minutes, may still serve the
session; a fork may also have refreshed the entry of its parent, which
hears nothing of it."
  (setq opencode-state--cache-timer nil)
  (opencode-state--cache-prune)
  (if (opencode-state--cache-listable-p)
      (opencode-state--cache-list)
    (opencode-state--cache-schedule)))

(defun opencode-state--cache-listed (data)
  "Install listed DATA, apply the events held meanwhile, and rearm the timer.
DATA is the service's listing, or `failed' to keep the expiries known."
  (setq opencode-state--cache-listing nil)
  (unless (eq data 'failed)
    (opencode-state--cache-install data))
  (let ((held (reverse opencode-state--cache-held)))
    (setq opencode-state--cache-held nil)
    (mapc #'opencode-state--cache-apply held))
  (opencode-state--cache-prune)
  (opencode-state--cache-schedule))

(defun opencode-state--cache-list ()
  "List every session's prompt-cache expiry, replacing the known ones.
A listing in flight is superseded.  Expiry events received meanwhile
are held and applied over the answer, which may predate them.  A
service without the route, which only my fork has, answers 404: it has
no expiries, and is not asked again until the stream reconnects.  A
failure keeps the known expiries, and the next one to pass lists again."
  (let ((generation (cl-incf opencode-state--cache-generation)))
    (setq opencode-state--cache-listing t)
    (opencode-request
     'get "/api/experimental/session/cache" :directory nil :buffer nil
     :then (lambda (response)
             (when (= generation opencode-state--cache-generation)
               (opencode-state--cache-listed (opencode-request-data response))))
     :else (lambda (error)
             (when (= generation opencode-state--cache-generation)
               (if (eql (opencode-error-status error) 404)
                   (progn
                     (setq opencode-state--cache-unsupported t)
                     (opencode-state--cache-listed nil))
                 (message "OpenCode: cannot list the prompt-cache expiries: %s"
                          (opencode-error-describe error))
                 (opencode-state--cache-listed 'failed)))))))

(defun opencode-state--cache-apply (event)
  "Make the expiry `session.cache.updated' EVENT carries its session's."
  (let ((data (alist-get 'data event)))
    (when-let* ((id (alist-get 'sessionID data)))
      (opencode-state--cache-set id (opencode-state--cache-expiry (alist-get 'expiry data))))))

(defun opencode-state--cache-event (event)
  "Apply `session.cache.updated' EVENT when it follows the registry's stream.
While a listing is in flight, the event waits for its answer."
  (when (eq opencode-state--stream opencode-events--process)
    (if opencode-state--cache-listing
        (push event opencode-state--cache-held)
      (opencode-state--cache-apply event)
      (opencode-state--cache-schedule))))

(opencode-events-register "session.cache.updated" #'opencode-state--cache-event)

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
