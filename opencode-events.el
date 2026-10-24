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
;; The server fails a subscriber that falls 4096 frames behind, so the
;; process filter only splits frames and queues them; a zero-delay timer
;; dispatches them in time-bounded batches.  The scan resumes where the
;; previous chunk ended, so a multi-megabyte frame costs linear time.
;;
;; Modules register handlers per event type with `opencode-events-register'.

;;; Code:

(require 'opencode-common)
(require 'opencode-http)
(require 'opencode-service)
(require 'plz)

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

(defvar opencode-events--process nil
  "Curl process of the event stream.")

(defvar opencode-events--wanted nil
  "Non-nil while Emacs wants the stream open; nil after a disconnect.")

(defvar opencode-events--closed-for-stop nil
  "Non-nil when `opencode-service-stop' closed the stream, until it reopens.")

(defvar opencode-events--backoff 1
  "Seconds before the next reconnection attempt.")

(defvar opencode-events--reconnect-timer nil
  "Timer of the pending reconnection attempt.")

(defvar opencode-events--watchdog nil
  "Timer checking that the stream still delivers heartbeats.")

(defvar opencode-events--last-output 0
  "Time the stream last produced output.")

(defvar opencode-events--queue nil
  "Queued events, a `tconc'-style cons (FIRST . LAST) of a list.")

(defvar opencode-events--dispatch-timer nil
  "Timer dispatching queued events.")

(defvar opencode-events-connected-hook nil
  "Hook run, after a stream (re)opens, on its `server.connected' event.")

(defvar-local opencode-events--body-start nil
  "Position after the HTTP headers in the stream buffer.")

(defvar-local opencode-events--scanned nil
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
  (process-live-p opencode-events--process))

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

(defun opencode-events--filter (process output)
  "Append OUTPUT of stream PROCESS and queue every complete frame."
  (setq opencode-events--last-output (float-time))
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (point-max))
        (insert output)
        (unless opencode-events--body-start
          (goto-char (point-min))
          ;; curl prints every response's headers; the stream follows the last block.
          (when (re-search-forward "\r?\n\r?\n" nil t)
            (setq opencode-events--body-start (point)
                  opencode-events--scanned (point))))
        (when opencode-events--body-start
          (let ((start opencode-events--body-start)
                (consumed nil))
            (goto-char (max start (- opencode-events--scanned 3)))
            (while (re-search-forward "\r?\n\r?\n" nil t)
              (opencode-events--frame start (match-beginning 0))
              (setq start (point) consumed t))
            (when consumed
              (delete-region opencode-events--body-start start))
            (setq opencode-events--scanned (point-max))))))))

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
      (setq opencode-events--backoff 1))
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

(defun opencode-events--schedule-reconnect ()
  "Reopen the stream after the current backoff, while it is still wanted."
  (when (and opencode-events--wanted (not (timerp opencode-events--reconnect-timer)))
    (setq opencode-events--reconnect-timer
          (run-at-time opencode-events--backoff nil
                       (lambda ()
                         (setq opencode-events--reconnect-timer nil)
                         (opencode-events--open))))
    (setq opencode-events--backoff (min 30 (* 2 opencode-events--backoff)))))

(defun opencode-events--closed (process)
  "Handle the end of stream PROCESS."
  (when (eq process opencode-events--process)
    (setq opencode-events--process nil)
    (opencode-events--schedule-reconnect)))

(defun opencode-events--watch ()
  "Reopen a stream that stopped producing heartbeats."
  (when (and (opencode-events-connected-p)
             (> (- (float-time) opencode-events--last-output)
                opencode-events-heartbeat-timeout))
    (let ((process opencode-events--process))
      (setq opencode-events--process nil)
      (delete-process process)
      (opencode-events--schedule-reconnect))))

(defun opencode-events--open ()
  "Open the event stream on a ready service."
  (when (and opencode-events--wanted (not (opencode-events-connected-p)))
    (opencode-service-ensure
     (lambda (connection)
       (when (and opencode-events--wanted (not (opencode-events-connected-p)))
         (setq opencode-events--last-output (float-time))
         (let (process
               ;; curl buffers piped output, and decompression holds frames back.
               (plz-curl-default-args '("--silent" "--no-buffer" "--location")))
           (setq process
                 (plz 'get (concat (plist-get connection :url) "/api/event")
                   :headers `(("Authorization" . ,(opencode-service-authorization connection))
                              ("Accept" . "text/event-stream"))
                   :as 'string :noquery t
                   :filter #'opencode-events--filter
                   :then (lambda (_) (opencode-events--closed process))
                   :else (lambda (error)
                           (when (and (plz-error-response error)
                                      (eql 401 (plz-response-status (plz-error-response error))))
                             (setq opencode-service-connection nil))
                           (opencode-events--closed process))))
           (setq opencode-events--process process))))
     (lambda (message)
       (message "OpenCode: %s" message)
       (opencode-events--schedule-reconnect)))))

(defun opencode-events-connect (&optional callback)
  "Open the event stream, then call CALLBACK with the service connection."
  (setq opencode-events--wanted t
        opencode-events--closed-for-stop nil)
  (unless (timerp opencode-events--watchdog)
    (setq opencode-events--watchdog (run-at-time 15 15 #'opencode-events--watch)))
  (opencode-events--open)
  (when callback
    (opencode-service-ensure callback (lambda (message) (opencode-report-error message)))))

(defun opencode-disconnect ()
  "Close the event stream.  The shared service keeps running."
  (interactive)
  (setq opencode-events--wanted nil
        opencode-events--closed-for-stop nil)
  (dolist (timer (list opencode-events--reconnect-timer opencode-events--watchdog))
    (when (timerp timer) (cancel-timer timer)))
  (setq opencode-events--reconnect-timer nil
        opencode-events--watchdog nil)
  (when (process-live-p opencode-events--process)
    (let ((process opencode-events--process))
      (setq opencode-events--process nil)
      (delete-process process)))
  (setq opencode-service-connection nil))

(defun opencode-service-stop (&optional callback errback)
  "Stop the shared service, then call CALLBACK with no argument.
ERRBACK, when given, receives an error message instead on failure.
The event stream closes first, so its reconnect cannot start another
service; reopening a session starts one again, and the stream reopens
once any service answers Emacs while session buffers are open.  Every
client loses the
service, and running turns resume once one starts, though their current
step starts over, so interactively this asks first while any runs."
  (interactive)
  (when (process-live-p opencode-service--starter)
    (user-error "An OpenCode service start or restart is already running"))
  (when-let* (((called-interactively-p 'any))
              (running (opencode-service-running-sessions))
              ((> running 0)))
    (unless (yes-or-no-p (format "%d OpenCode session%s running a turn; stop the service anyway? "
                                 running (if (= running 1) " is" "s are")))
      (user-error "Stop cancelled")))
  (opencode-disconnect)
  (setq opencode-events--closed-for-stop t)
  (opencode-service--run-cli
   '("service" "stop")
   (lambda (status output)
     (cond
      ((not (zerop status))
       (let ((text (format "opencode service stop failed: %s" output)))
         (if errback (funcall errback text) (message "OpenCode: %s" text))))
      (t
       (message "OpenCode service stopped")
       (when callback (funcall callback)))))))

(defun opencode-events--reopen-after-stop (_connection)
  "Reopen the stream `opencode-service-stop' closed, now that a service answers.
Any request starts a service again; without this, open session buffers
would stay stale.  With none open, the next session opened reconnects."
  (when (and opencode-events--closed-for-stop
             (not opencode-events--wanted)
             (seq-some #'buffer-live-p (hash-table-values opencode-session-buffers)))
    (opencode-events-connect)))

(add-hook 'opencode-service-ready-functions #'opencode-events--reopen-after-stop)

(provide 'opencode-events)
;;; opencode-events.el ends here
