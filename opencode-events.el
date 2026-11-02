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

(defvar opencode-events--stream nil
  "Exchange of the open event stream, an `opencode-transport-exchange', or nil.
It also names the stream: a stream reopened is another exchange.")

(defvar opencode-events--wanted nil
  "Non-nil while Emacs wants the stream open; nil after a disconnect.")

(defvar opencode-events--closed-for-stop nil
  "Non-nil when `opencode-service-stop' closed the stream, until it reopens.")

(defvar opencode-events--opening nil
  "Non-nil while the stream waits for a ready service to open on.
It is the handle of that wait, for `opencode-service-cancel'.")

(defvar opencode-events--connection nil
  "Connection of the service the stream was last opened on.")

(defvar opencode-events-wanted-functions nil
  "Functions, called without argument, telling whether something needs the stream.
After `opencode-service-stop', a service answering again reopens the
stream only when one of them returns non-nil.")

(defvar opencode-events--backoff 1
  "Seconds before the next reconnection attempt.")

(defvar opencode-events--reconnect-timer nil
  "Timer of the pending reconnection attempt.")

(defconst opencode-events--watch-interval 15
  "Seconds between two checks of the heartbeat watchdog.")

(defvar opencode-events--watchdog nil
  "Timer checking that the stream still delivers heartbeats.")

(defvar opencode-events--last-output 0
  "Time the stream last produced output.")

(defvar opencode-events--last-watch nil
  "Time of the watchdog's previous check, or nil before its first one.")

(defvar opencode-events--resumed 0
  "Time the watchdog last resumed after it could not run for a while.
Silence before it is no evidence that the stream died.")

(defvar opencode-events--queue nil
  "Queued events, a `tconc'-style cons (FIRST . LAST) of a list.")

(defvar opencode-events--dispatch-timer nil
  "Timer dispatching queued events.")

(defvar opencode-events-connected-hook nil
  "Hook run, after a stream (re)opens, on its `server.connected' event.")

(defvar opencode-events--greeted nil
  "Stream whose `server.connected' event was handled, or nil.")

(defvar-local opencode-events--scanned 1
  "Position up to which the stream buffer holds no frame terminator.")

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
  (opencode-transport-live-p opencode-events--stream))

(defun opencode-event-directory (event)
  "Return the directory EVENT belongs to, normalized, or nil."
  (when-let* ((directory (alist-get 'directory (alist-get 'location event))))
    (opencode-normalize-directory directory)))

;;;; Parsing

(defun opencode-events--enqueue (event)
  "Queue decoded EVENT for dispatch."
  (let ((cell (list event)))
    (if opencode-events--queue
        (setcdr (cdr opencode-events--queue) cell)
      (setq opencode-events--queue (cons cell nil)))
    (setcdr opencode-events--queue cell))
  (unless (timerp opencode-events--dispatch-timer)
    (setq opencode-events--dispatch-timer
          (run-at-time 0 nil #'opencode-events--dispatch))))

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
  "Append OUTPUT, bytes of the stream's body, to BUFFER and queue every complete frame."
  (setq opencode-events--last-output (float-time))
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
          (insert (format-time-string "%T ") (or type "?") "\n"))))
    (when (equal type "server.connected")
      (setq opencode-events--backoff 1
            opencode-events--greeted opencode-events--stream))
    (dolist (handler (append (gethash type opencode-events--handlers)
                             (gethash t opencode-events--handlers)))
      (condition-case err
          (funcall handler event)
        (error
         (message "OpenCode: %s handler %s failed: %s"
                  type handler (error-message-string err)))))
    (when (equal type "server.connected")
      (run-hooks 'opencode-events-connected-hook))))

(defun opencode-events--dispatch ()
  "Dispatch queued events until the queue empties or the budget is spent."
  (setq opencode-events--dispatch-timer nil)
  (let ((deadline (+ (float-time) opencode-events-dispatch-budget)))
    (while (and opencode-events--queue (< (float-time) deadline))
      (let ((event (car (car opencode-events--queue))))
        (if (cdr (car opencode-events--queue))
            (setcar opencode-events--queue (cdr (car opencode-events--queue)))
          (setq opencode-events--queue nil))
        (opencode-events--handle event)))
    (when opencode-events--queue
      (setq opencode-events--dispatch-timer
            (run-at-time 0 nil #'opencode-events--dispatch)))))

(defun opencode-events-dispatch-now ()
  "Dispatch every queued event at once."
  (while opencode-events--queue
    (opencode-events--dispatch)))

;;;; Lifecycle

(defun opencode-events--wants-p ()
  "Return non-nil when something needs the event stream.
That is a client of `opencode-events-wanted-functions'."
  (run-hook-with-args-until-success 'opencode-events-wanted-functions))

(defun opencode-events--schedule-reconnect ()
  "Reopen the stream after the current backoff, while it is still wanted."
  (when (and opencode-events--wanted (not (timerp opencode-events--reconnect-timer)))
    (setq opencode-events--reconnect-timer
          (run-at-time opencode-events--backoff nil
                       (lambda ()
                         (setq opencode-events--reconnect-timer nil)
                         (opencode-events--open))))
    (setq opencode-events--backoff (min 30 (* 2 opencode-events--backoff)))))

(defun opencode-events--closed (stream)
  "Handle the end of STREAM."
  (when (eq stream opencode-events--stream)
    (setq opencode-events--stream nil)
    (opencode-events--schedule-reconnect)))

(defun opencode-events--drop ()
  "Close the stream, without reopening it."
  (when-let* ((stream opencode-events--stream))
    (setq opencode-events--stream nil)
    (opencode-transport-cancel stream)))

(defun opencode-events--note-resumption (now)
  "Note when the watchdog, checking at NOW, could not run for a while.
Emacs reads no output while the machine sleeps or while it is busy, so
the silence of such a gap says nothing of the stream: on a late check,
or when the clock went back, the silence counts from NOW, which gives
the stream a whole heartbeat timeout to show it is alive.  A late timer
then fires several times in a row to catch up, and only the first one
is late."
  (let ((gap (and opencode-events--last-watch (- now opencode-events--last-watch))))
    (when (and gap (or (< gap 0) (> gap (* 2 opencode-events--watch-interval))))
      (setq opencode-events--resumed now
            opencode-events--last-output (min opencode-events--last-output now)))
    (setq opencode-events--last-watch now)))

(defun opencode-events--watch ()
  "Reopen a stream that stopped producing heartbeats, or one left closed.
A stream counts as silent only for the time Emacs could have read its
output, see `opencode-events--note-resumption'.  Nothing should leave a
wanted stream closed with no reconnection scheduled or waiting for a
service, but if anything does, this reopens it rather than leave
session buffers stale."
  (let ((now (float-time)))
    (opencode-events--note-resumption now)
    (cond
     ((opencode-events-connected-p)
      (when (> (- now (max opencode-events--last-output opencode-events--resumed))
               opencode-events-heartbeat-timeout)
        (opencode-events--drop)
        (opencode-events--schedule-reconnect)))
     ((and opencode-events--wanted
           (not opencode-events--opening)
           (not (timerp opencode-events--reconnect-timer)))
      (opencode-events--schedule-reconnect)))))

(defun opencode-events--start (connection)
  "Open the stream on CONNECTION's service, unless it is open or unwanted."
  (when (and opencode-events--wanted (not (opencode-events-connected-p)))
    (when (timerp opencode-events--reconnect-timer)
      (cancel-timer opencode-events--reconnect-timer)
      (setq opencode-events--reconnect-timer nil))
    (setq opencode-events--last-output (float-time))
    (let ((buffer (generate-new-buffer " *opencode-events*" t))
          stream)
      (with-current-buffer buffer
        (set-buffer-multibyte nil))
      (cl-flet ((closed ()
                  (when (buffer-live-p buffer) (kill-buffer buffer))
                  (opencode-events--closed stream)))
        (condition-case err
            (setq stream
                  (opencode-transport-request
                   'get (concat (plist-get connection :url) "/api/event")
                   :headers `(("Authorization" . ,(opencode-service-authorization connection))
                              ("Accept" . "text/event-stream"))
                   :filter (lambda (_stream output) (opencode-events--filter buffer output))
                   :then (lambda (_) (closed))
                   :else (lambda (failure)
                           (when (eql 401 (opencode-transport-failure-status failure))
                             (setq opencode-service-connection nil))
                           (closed)))
                  opencode-events--stream stream
                  opencode-events--connection connection)
          (error
           (kill-buffer buffer)
           (message "OpenCode: cannot open the event stream: %s" (error-message-string err))
           (opencode-events--schedule-reconnect)))))))

(defun opencode-events--open ()
  "Open the event stream on a ready service, starting one if needed."
  (when (and opencode-events--wanted
             (not (opencode-events-connected-p))
             (not opencode-events--opening))
    (setq opencode-events--opening t)
    (let ((handle (opencode-service-ensure
                   (lambda (connection)
                     (setq opencode-events--opening nil)
                     (opencode-events--start connection))
                   (lambda (message)
                     (setq opencode-events--opening nil)
                     (message "OpenCode: %s" message)
                     (opencode-events--schedule-reconnect)))))
      ;; Unless it settled already.
      (when opencode-events--opening
        (setq opencode-events--opening handle)))))

(defun opencode-events--want ()
  "Note that Emacs wants the stream, and watch its heartbeats."
  (setq opencode-events--wanted t
        opencode-events--closed-for-stop nil)
  (unless (timerp opencode-events--watchdog)
    (setq opencode-events--last-watch (float-time)
          opencode-events--watchdog (run-at-time opencode-events--watch-interval
                                                 opencode-events--watch-interval
                                                 #'opencode-events--watch))))

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
  (when (and opencode-events--wanted (not (opencode-events-connected-p)))
    (opencode-events--open)))

(defun opencode-disconnect ()
  "Close the event stream.  The shared service keeps running."
  (interactive)
  (setq opencode-events--wanted nil
        opencode-events--closed-for-stop nil)
  (dolist (timer (list opencode-events--reconnect-timer opencode-events--watchdog))
    (when (timerp timer) (cancel-timer timer)))
  (setq opencode-events--reconnect-timer nil
        opencode-events--watchdog nil
        opencode-events--last-watch nil
        opencode-events--connection nil)
  ;; Its wait for a service would otherwise start one again after a stop.
  (when (consp opencode-events--opening)
    (opencode-service-cancel opencode-events--opening))
  (setq opencode-events--opening nil)
  (opencode-events--drop)
  (setq opencode-service-connection nil))

(defun opencode-service-stop (&optional callback errback)
  "Stop the shared service, then call CALLBACK with no argument.
ERRBACK, when given, receives an error message instead on failure.
The event stream closes first, so its reconnect cannot start another
service.  Any request starts one again, waiting for the stop to end,
and the stream then reopens while session buffers are open or the
session registry has clients; otherwise the next session opened
reconnects.  Every client loses the service, and running turns resume
once one starts, though their current step starts over, so
interactively this asks first while any runs."
  (interactive)
  (when (process-live-p opencode-service--starter)
    (user-error "An OpenCode service start, restart or stop is already running"))
  (when-let* (((called-interactively-p 'any))
              (running (opencode-service-running-sessions))
              ((> running 0)))
    (unless (yes-or-no-p (format "%d OpenCode session%s running a turn; stop the service anyway? "
                                 running (if (= running 1) " is" "s are")))
      (user-error "Stop cancelled")))
  (opencode-disconnect)
  (setq opencode-events--closed-for-stop t)
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
   (opencode-events--closed-for-stop
    (when (and (not opencode-events--wanted) (opencode-events--wants-p))
      (opencode-events--want)
      (opencode-events--start connection)))
   ((not opencode-events--wanted))
   ((opencode-events-connected-p)
    (unless (opencode-service-same-p opencode-events--connection connection)
      (opencode-events--drop)
      (opencode-events--start connection)))
   ;; A stream waiting for a service is settled with this one.
   ((not opencode-events--opening)
    (opencode-events--start connection))))

(add-hook 'opencode-service-ready-functions #'opencode-events--service-ready)

(provide 'opencode-events)
;;; opencode-events.el ends here
