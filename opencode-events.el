;;; opencode-events.el --- The OpenCode event stream  -*- lexical-binding: t; -*-

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

;; One `GET /api/event' stream carries every location's events as SSE
;; `data:' frames.  The first frame is `server.connected', a comment
;; heartbeat follows every 15 seconds, and nothing is replayed after a
;; reconnect, so `server.connected' is the cue for every module to
;; resynchronize from REST.
;;
;; The stream is a request of `opencode-transport-request' whose body
;; never ends.  The server fails a subscriber that falls 4096 frames
;; behind, so the stream's filter only splits frames and queues them,
;; in a buffer of the stream's own; a zero-delay timer
;; dispatches them in time-bounded batches.  The scan resumes where the
;; previous chunk ended, so a multi-megabyte frame costs linear time.
;;
;; A wanted stream comes back however it ends: after a backoff when it
;; drops, at once when a service is ready for Emacs again or a prompt is
;; sent, and from the heartbeat watchdog when it was left closed with
;; nothing reopening it.  The watchdog drops a stream that stays silent,
;; counting only the time Emacs could have read it, so that a machine
;; waking from sleep keeps its healthy stream.  `opencode-disconnect'
;; alone keeps it closed; after `opencode-service-stop', it reopens only
;; for the clients of `opencode-events-wanted-functions', once something
;; starts a service.
;;
;; Modules register handlers per event type with `opencode-events-register'.

;;; Code:

(require 'opencode-common)
(require 'opencode-http)
(require 'opencode-service)
(require 'opencode-transport)

(defcustom opencode-events-dispatch-budget 0.05
  "Seconds spent dispatching queued events before yielding to redisplay."
  :type 'number
  :group 'opencode)

(defcustom opencode-events-heartbeat-timeout 45
  "Seconds without any stream output after which the stream is reopened."
  :type 'number
  :group 'opencode)

(defvar opencode-events-log nil
  "When non-nil, log every event type in *opencode-events*.")

(defvar opencode-events--handlers (make-hash-table :test #'equal)
  "Event types mapped to the list of their handler functions.")

(opencode-defvar-service opencode-events--stream nil
  "Exchange of the open event stream, an `opencode-transport-exchange', or nil.
It also names the stream: a stream reopened is another exchange.")

(opencode-defvar-service opencode-events--wanted nil
  "Non-nil while Emacs wants the stream open; nil after a disconnect.")

(opencode-defvar-service opencode-events--closed-for-stop nil
  "Non-nil when `opencode-service-stop' closed the stream, until it reopens.")

(opencode-defvar-service opencode-events--opening nil
  "Non-nil while the stream waits for a ready service to open on.
It is the handle of that wait, for `opencode-service-cancel'.")

(opencode-defvar-service opencode-events--connection nil
  "Connection of the service the stream was last opened on.")

(defvar opencode-events-wanted-functions nil
  "Functions, called without argument, telling whether something needs the stream.
After `opencode-service-stop', a service answering again reopens the
stream only when one of them returns non-nil.")

(opencode-defvar-service opencode-events--backoff 1
  "Seconds before the next reconnection attempt.")

(opencode-defvar-service opencode-events--reconnect-timer nil
  "Timer of the pending reconnection attempt.")

(defconst opencode-events--watch-interval 15
  "Seconds between two checks of the heartbeat watchdog.")

(opencode-defvar-service opencode-events--watchdog nil
  "Timer checking that the stream still delivers heartbeats.")

(opencode-defvar-service opencode-events--last-output 0
  "Time the stream last produced output.")

(opencode-defvar-service opencode-events--last-watch nil
  "Time of the watchdog's previous check, or nil before its first one.")

(opencode-defvar-service opencode-events--resumed 0
  "Time the watchdog last resumed after it could not run for a while.
Silence before it is no evidence that the stream died.")

(opencode-defvar-service opencode-events--queue nil
  "Queued events, a `tconc'-style cons (FIRST . LAST) of a list.")

(opencode-defvar-service opencode-events--dispatch-timer nil
  "Timer dispatching queued events.")

(defvar opencode-events-connected-hook nil
  "Hook run, after a stream (re)opens, on its `server.connected' event.")

(opencode-defvar-service opencode-events--greeted nil
  "Stream whose `server.connected' event was handled, or nil.")

(defvar-local opencode-events--scanned 1
  "Position up to which the stream buffer holds no frame terminator.")

(defvar-local opencode-events--buffer-service nil
  "The remote service whose event stream this stream buffer reads, or nil.")

(defun opencode-events-register (type function)
  "Call FUNCTION with every event of TYPE, a string or t for all events.
FUNCTION receives the event alist with members `type', `data', `id',
`created', and `location' when the event belongs to a directory."
  (let ((functions (gethash type opencode-events--handlers)))
    (unless (memq function functions)
      (puthash type (append functions (list function)) opencode-events--handlers))))

(defun opencode-events-unregister (type function)
  "Stop calling FUNCTION for events of TYPE."
  (puthash type (delq function (gethash type opencode-events--handlers))
           opencode-events--handlers))

(defun opencode-events-connected-p ()
  "Return non-nil while the event stream is open."
  (opencode-transport-live-p (opencode-sv opencode-events--stream)))

(defun opencode-event-directory (event)
  "Return the directory EVENT belongs to, normalized, or nil."
  (when-let* ((directory (alist-get 'directory (alist-get 'location event))))
    (opencode-normalize-directory directory)))

;;;; Parsing

(defun opencode-events--enqueue (event)
  "Queue decoded EVENT for dispatch."
  (let ((cell (list event)))
    (if (opencode-sv opencode-events--queue)
        (setcdr (cdr (opencode-sv opencode-events--queue)) cell)
      (setf (opencode-sv opencode-events--queue) (cons cell nil)))
    (setcdr (opencode-sv opencode-events--queue) cell))
  (unless (timerp (opencode-sv opencode-events--dispatch-timer))
    (setf (opencode-sv opencode-events--dispatch-timer)
          (apply #'run-at-time 0 nil #'opencode-events--dispatch (opencode--timer-arguments)))))

(defun opencode-events--frame (start end)
  "Decode the SSE frame between START and END in the stream buffer."
  (let ((lines (split-string
                (decode-coding-string (buffer-substring-no-properties start end) 'utf-8)
                "\r?\n"))
        data)
    (dolist (line lines)
      (cond
       ((string-prefix-p "data:" line)
        (push (string-remove-prefix " " (substring line 5)) data))))
    (when data
      (condition-case err
          (opencode-events--enqueue
           (json-parse-string (string-join (nreverse data) "\n")
                              :object-type 'alist :array-type 'list
                              :null-object nil :false-object nil))
        (error (message "OpenCode: undecodable event: %s" (error-message-string err)))))))

(defun opencode-events--filter (buffer output)
  "Append OUTPUT, bytes of the stream's body, to BUFFER and queue every complete frame.
The frames are for the service whose stream BUFFER reads, whatever the
caller bound: a stream opened by an earlier version of this package,
before services, binds none, and is the local service's."
  (opencode-with-service (and (buffer-live-p buffer)
                              (buffer-local-value 'opencode-events--buffer-service buffer))
    (opencode-events--filter-1 buffer output)))

(defun opencode-events--filter-1 (buffer output)
  "Read OUTPUT into BUFFER for the current service, as `opencode-events--filter'."
  (setf (opencode-sv opencode-events--last-output) (float-time))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (insert output)
        (let ((start (point-min))
              (consumed nil))
          (goto-char (max start (- opencode-events--scanned 3)))
          (while (re-search-forward "\r?\n\r?\n" nil t)
            (opencode-events--frame start (match-beginning 0))
            (setq start (point) consumed t))
          (when consumed
            (delete-region (point-min) start))
          (setq opencode-events--scanned (point-max)))))))

;;;; Dispatch

(defun opencode-events--handle (event)
  "Run the handlers of EVENT."
  (let ((type (alist-get 'type event)))
    (when opencode-events-log
      (with-current-buffer (get-buffer-create "*opencode-events*")
        (save-excursion
          (goto-char (point-max))
          (insert (format-time-string "%T ")
                  (if-let* ((label (opencode-service-label))) (concat "@" label " ") "")
                  (or type "?") "\n"))))
    (when (equal type "server.connected")
      (setf (opencode-sv opencode-events--backoff) 1
            (opencode-sv opencode-events--greeted) (opencode-sv opencode-events--stream)))
    (dolist (handler (append (gethash type opencode-events--handlers)
                             (gethash t opencode-events--handlers)))
      (condition-case err
          (funcall handler event)
        (error
         (message "OpenCode: %s handler %s failed: %s"
                  type handler (error-message-string err)))))
    (when (equal type "server.connected")
      (run-hooks 'opencode-events-connected-hook))))

(defun opencode-events--dispatch (&optional service)
  "Dispatch queued events of SERVICE until its queue empties or the budget is spent.
SERVICE is nil for the local service.  Handlers run for it."
  (opencode-with-service service
    (opencode-events--dispatch-1)))

(defun opencode-events--dispatch-1 ()
  "Dispatch the current service's queued events, as `opencode-events--dispatch'."
  (setf (opencode-sv opencode-events--dispatch-timer) nil)
  (let ((deadline (+ (float-time) opencode-events-dispatch-budget)))
    (while (and (opencode-sv opencode-events--queue) (< (float-time) deadline))
      (let ((event (car (car (opencode-sv opencode-events--queue)))))
        (if (cdr (car (opencode-sv opencode-events--queue)))
            (setcar (opencode-sv opencode-events--queue) (cdr (car (opencode-sv opencode-events--queue))))
          (setf (opencode-sv opencode-events--queue) nil))
        (opencode-events--handle event)))
    (when (opencode-sv opencode-events--queue)
      (setf (opencode-sv opencode-events--dispatch-timer)
            (apply #'run-at-time 0 nil #'opencode-events--dispatch (opencode--timer-arguments))))))

(defun opencode-events-dispatch-now ()
  "Dispatch every queued event of the current service at once."
  (while (opencode-sv opencode-events--queue)
    (opencode-events--dispatch-1)))

;;;; Lifecycle

(defun opencode-events--wants-p ()
  "Return non-nil when something needs the event stream.
That is a client of `opencode-events-wanted-functions'."
  (run-hook-with-args-until-success 'opencode-events-wanted-functions))

(defun opencode-events--schedule-reconnect ()
  "Reopen the stream after the current backoff, while it is still wanted."
  (when (and (opencode-sv opencode-events--wanted) (not (timerp (opencode-sv opencode-events--reconnect-timer))))
    (setf (opencode-sv opencode-events--reconnect-timer)
          (let ((service (opencode--remote-service)))
            (run-at-time (opencode-sv opencode-events--backoff) nil
                         (lambda ()
                           (opencode-with-service service
                             (setf (opencode-sv opencode-events--reconnect-timer) nil)
                             (opencode-events--open))))))
    (setf (opencode-sv opencode-events--backoff) (min 30 (* 2 (opencode-sv opencode-events--backoff))))))

(defun opencode-events--service-of (stream)
  "Return the service whose event stream STREAM is, the local one by default."
  (or (seq-find (lambda (service)
                  (opencode-with-service service
                    (eq stream (opencode-sv opencode-events--stream))))
                (cdr (opencode-services)))
      opencode--local-service))

(defun opencode-events--closed (stream)
  "Handle the end of STREAM, of whichever service it is.
A stream opened by an earlier version of this package, before services,
tells it ended without binding its service."
  (opencode-with-service (opencode-events--service-of stream)
    (opencode-events--closed-1 stream)))

(defun opencode-events--closed-1 (stream)
  "Handle the end of the current service's STREAM, as `opencode-events--closed'."
  (when (eq stream (opencode-sv opencode-events--stream))
    (setf (opencode-sv opencode-events--stream) nil)
    (opencode-events--schedule-reconnect)))

(defun opencode-events--drop ()
  "Close the stream, without reopening it."
  (when-let* ((stream (opencode-sv opencode-events--stream)))
    (setf (opencode-sv opencode-events--stream) nil)
    (opencode-transport-cancel stream)))

(defun opencode-events--note-resumption (now)
  "Note when the watchdog, checking at NOW, could not run for a while.
Emacs reads no output while the machine sleeps or while it is busy, so
the silence of such a gap says nothing of the stream: on a late check,
or when the clock went back, the silence counts from NOW, which gives
the stream a whole heartbeat timeout to show it is alive.  A late timer
then fires several times in a row to catch up, and only the first one
is late."
  (let ((gap (and (opencode-sv opencode-events--last-watch) (- now (opencode-sv opencode-events--last-watch)))))
    (when (and gap (or (< gap 0) (> gap (* 2 opencode-events--watch-interval))))
      (setf (opencode-sv opencode-events--resumed) now
            (opencode-sv opencode-events--last-output) (min (opencode-sv opencode-events--last-output) now)))
    (setf (opencode-sv opencode-events--last-watch) now)))

(defun opencode-events--watch (&optional service)
  "Reopen a stream that stopped producing heartbeats, or one left closed.
A stream counts as silent only for the time Emacs could have read its
output, see `opencode-events--note-resumption'.  Nothing should leave a
wanted stream closed with no reconnection scheduled or waiting for a
service, but if anything does, this reopens it rather than leave
session buffers stale.  SERVICE, nil for the local one, owns the stream."
  (opencode-with-service service
    (opencode-events--watch-1)))

(defun opencode-events--watch-1 ()
  "Watch the current service's stream, as `opencode-events--watch'."
  (let ((now (float-time)))
    (opencode-events--note-resumption now)
    (cond
     ((opencode-events-connected-p)
      (when (> (- now (max (opencode-sv opencode-events--last-output) (opencode-sv opencode-events--resumed)))
               opencode-events-heartbeat-timeout)
        (opencode-events--drop)
        (opencode-events--schedule-reconnect)))
     ((and (opencode-sv opencode-events--wanted)
           (not (opencode-sv opencode-events--opening))
           (not (timerp (opencode-sv opencode-events--reconnect-timer))))
      (opencode-events--schedule-reconnect)))))

(defun opencode-events--start (connection)
  "Open the stream on CONNECTION's service, unless it is open or unwanted."
  (when (and (opencode-sv opencode-events--wanted) (not (opencode-events-connected-p)))
    (when (timerp (opencode-sv opencode-events--reconnect-timer))
      (cancel-timer (opencode-sv opencode-events--reconnect-timer))
      (setf (opencode-sv opencode-events--reconnect-timer) nil))
    (setf (opencode-sv opencode-events--last-output) (float-time))
    (let ((buffer (generate-new-buffer " *opencode-events*" t))
          (service (opencode--remote-service))
          stream)
      (with-current-buffer buffer
        (set-buffer-multibyte nil)
        (setq opencode-events--buffer-service service))
      (cl-flet ((closed ()
                  (when (buffer-live-p buffer) (kill-buffer buffer))
                  (opencode-with-service service
                    (opencode-events--closed stream))))
        (condition-case err
            (setf stream
                  (opencode-transport-request
                   'get (concat (plist-get connection :url) "/api/event")
                   :headers `(("Authorization" . ,(opencode-service-authorization connection))
                              ("Accept" . "text/event-stream"))
                   :filter (lambda (_stream output)
                             (opencode-with-service service
                               (opencode-events--filter buffer output)))
                   :then (lambda (_) (closed))
                   :else (lambda (failure)
                           (when (eql 401 (opencode-transport-failure-status failure))
                             (opencode-with-service service
                               (setf (opencode-sv opencode-service-connection) nil)))
                           (closed)))
                  (opencode-sv opencode-events--stream) stream
                  (opencode-sv opencode-events--connection) connection)
          (error
           (kill-buffer buffer)
           (message "OpenCode: cannot open the event stream: %s" (error-message-string err))
           (opencode-events--schedule-reconnect)))))))

(defun opencode-events--open ()
  "Open the event stream on a ready service, starting one if needed."
  (when (and (opencode-sv opencode-events--wanted)
             (not (opencode-events-connected-p))
             (not (opencode-sv opencode-events--opening)))
    (setf (opencode-sv opencode-events--opening) t)
    (let* ((service (opencode--remote-service))
           (handle (opencode-service-ensure
                    (lambda (connection)
                      (opencode-with-service service
                        (setf (opencode-sv opencode-events--opening) nil)
                        (opencode-events--start connection)))
                    (lambda (message)
                      (opencode-with-service service
                        (setf (opencode-sv opencode-events--opening) nil)
                        (message "OpenCode: %s" message)
                        (opencode-events--schedule-reconnect))))))
      ;; Unless it settled already.
      (when (opencode-sv opencode-events--opening)
        (setf (opencode-sv opencode-events--opening) handle)))))

(defun opencode-events--want ()
  "Note that Emacs wants the stream, and watch its heartbeats."
  (setf (opencode-sv opencode-events--wanted) t
        (opencode-sv opencode-events--closed-for-stop) nil)
  (unless (timerp (opencode-sv opencode-events--watchdog))
    (setf (opencode-sv opencode-events--last-watch) (float-time)
          (opencode-sv opencode-events--watchdog) (apply #'run-at-time opencode-events--watch-interval
                                                               opencode-events--watch-interval
                                                               #'opencode-events--watch
                                                               (opencode--timer-arguments)))))

(defun opencode-events-connect (&optional callback)
  "Open the event stream, then call CALLBACK with the service connection."
  (opencode-events--want)
  (opencode-events--open)
  (when callback
    (opencode-service-ensure callback (lambda (message) (opencode-report-error message)))))

(defun opencode-events-reopen ()
  "Reopen the event stream at once when Emacs wants it and it is closed.
A reconnection waiting for its backoff is made now.  Actions whose
outcome only the stream reports, such as sending a prompt, call this,
so that it shows even when the stream was lost.  A stream closed by
`opencode-disconnect' or `opencode-service-stop' stays closed."
  (when (and (opencode-sv opencode-events--wanted) (not (opencode-events-connected-p)))
    (opencode-events--open)))

(defun opencode-disconnect ()
  "Close the event stream of the current service.  The service keeps running.
That is the local service but in a remote service's session buffers,
whose stream reopens as one of them asks for the service."
  (interactive)
  (setf (opencode-sv opencode-events--wanted) nil
        (opencode-sv opencode-events--closed-for-stop) nil)
  (dolist (timer (list (opencode-sv opencode-events--reconnect-timer) (opencode-sv opencode-events--watchdog)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (opencode-sv opencode-events--reconnect-timer) nil
        (opencode-sv opencode-events--watchdog) nil
        (opencode-sv opencode-events--last-watch) nil
        (opencode-sv opencode-events--connection) nil)
  ;; Its wait for a service would otherwise start one again after a stop.
  (when (consp (opencode-sv opencode-events--opening))
    (opencode-service-cancel (opencode-sv opencode-events--opening)))
  (setf (opencode-sv opencode-events--opening) nil)
  (opencode-events--drop)
  (setf (opencode-sv opencode-service-connection) nil))

(defun opencode-events--service-registered (_service)
  "Open the stream of the current service, registered, when something needs it.
For `opencode-service-registered-functions': session buffers of a
service registered again follow it anew."
  (when (opencode-events--wants-p)
    (opencode-events-connect)))

(add-hook 'opencode-service-registered-functions #'opencode-events--service-registered)

(defvar opencode-service-unregistered-functions nil
  "Abnormal hook run with a remote service as it is unregistered.
It runs for that service, which modules close their part of.")

(defun opencode-service-unregister (service)
  "Close remote SERVICE, a service or its name.
Its event stream closes, requests waiting for it fail, and its session
buffers stay, saying they no longer follow their sessions, until it is
registered again (see `opencode-service-register')."
  (let ((service (if (stringp service) (opencode-service-get service) service)))
    (when (and service (opencode-svc-remote service) (not (opencode-svc-closed service)))
      (setf (opencode-svc-closed service) t)
      (opencode-with-service service
        (opencode-disconnect)
        (let ((waiters (nreverse (opencode-sv opencode-service--waiters))))
          (setf (opencode-sv opencode-service--waiters) nil)
          (cl-incf (opencode-sv opencode-service--lookup))
          (dolist (waiter waiters)
            (when (cdr waiter)
              (with-demoted-errors "OpenCode: %S"
                (funcall (cdr waiter) (format "The OpenCode service of %s was closed"
                                              (opencode-service-label service)))))))
        (opencode-events--run-hook 'opencode-service-unregistered-functions service)))))

(defun opencode-events--run-hook (hook &rest arguments)
  "Run HOOK with ARGUMENTS, an error of one function not stopping the others."
  (apply #'run-hook-wrapped hook
         (lambda (function &rest arguments)
           (with-demoted-errors "OpenCode hook: %S"
             (apply function arguments))
           nil)
         arguments))

(defun opencode-service-stop (&optional callback errback)
  "Stop the shared service, then call CALLBACK with no argument.
ERRBACK, when given, receives an error message instead on failure.
The event stream closes first, so its reconnect cannot start another
service.  Any request starts one again, waiting for the stop to end,
and the stream then reopens while session buffers are open or the
session registry has clients; otherwise the next session opened
reconnects.  Every client loses the service, and running turns resume
once one starts, though their current step starts over, so
interactively this asks first while any runs.  It is the local
service, whatever the current buffer: Emacs never stops a remote one."
  (interactive)
  (when (process-live-p opencode-service--starter)
    (user-error "An OpenCode service start, restart or stop is already running"))
  (when-let* (((called-interactively-p 'any))
              (running (opencode-service-running-sessions))
              ((> running 0)))
    (unless (yes-or-no-p (format "%d OpenCode session%s running a turn; stop the service anyway? "
                                 running (if (= running 1) " is" "s are")))
      (user-error "Stop cancelled")))
  (opencode-with-service nil
    (opencode-service--stop-local callback errback)))

(defun opencode-service--stop-local (callback errback)
  "Stop the local service, then call CALLBACK, else ERRBACK, as `opencode-service-stop'."
  (opencode-disconnect)
  (setf (opencode-sv opencode-events--closed-for-stop) t)
  (opencode-service--stop
   (lambda (status output)
     (cond
      ((not (zerop status))
       (let ((text (format "opencode service stop failed: %s" output)))
         (if errback (funcall errback text) (message "OpenCode: %s" text))))
      (t
       (message "OpenCode service stopped")
       (when callback (funcall callback)))))))

(defun opencode-events--service-ready (connection)
  "Open the wanted stream on CONNECTION's service, which is ready, if closed.
A service is ready after Emacs started, restarted or found it again,
such as after a request was refused: the stream then reopens at once
rather than after its backoff, and one still open on another service
process, which is gone, moves to this one.  After
`opencode-service-stop', it reopens only for clients of
`opencode-events-wanted-functions'."
  (cond
   ((opencode-sv opencode-events--closed-for-stop)
    (when (and (not (opencode-sv opencode-events--wanted)) (opencode-events--wants-p))
      (opencode-events--want)
      (opencode-events--start connection)))
   ((not (opencode-sv opencode-events--wanted)))
   ((opencode-events-connected-p)
    (unless (opencode-service-same-p (opencode-sv opencode-events--connection) connection)
      (opencode-events--drop)
      (opencode-events--start connection)))
   ;; A stream waiting for a service is settled with this one.
   ((not (opencode-sv opencode-events--opening))
    (opencode-events--start connection))))

(add-hook 'opencode-service-ready-functions #'opencode-events--service-ready)

(provide 'opencode-events)
;;; opencode-events.el ends here
