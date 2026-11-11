;;; opencode-jobs.el --- Work that outlives an agent turn  -*- lexical-binding: t; -*-

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

;; A completed tool call may leave a shell, subagent or image job running.
;; List those jobs when the stream connects and when events announce work
;; starting or ending, never on a periodic poll.  Shell events also tell
;; the state immediately.  Events received during a listing are applied
;; over its answer, and a launch meanwhile asks for a newer listing.
;; Session entries and transcript blocks share this service-local state.

;;; Code:

(require 'opencode-common)
(require 'opencode-events)
(require 'opencode-http)

(opencode-defvar-service opencode-jobs--jobs (make-hash-table :test #'equal)
  "Job IDs mapped to the jobs the service last reported.")

(opencode-defvar-service opencode-jobs--sessions (make-hash-table :test #'equal)
  "Session IDs mapped to their running jobs, oldest first.")

(opencode-defvar-service opencode-jobs--ready nil
  "Non-nil once a job listing answered on this stream.")

(opencode-defvar-service opencode-jobs--generation 0
  "Count of listings; answers of superseded listings are ignored.")

(opencode-defvar-service opencode-jobs--listing nil
  "Non-nil while a job listing is in flight.")

(opencode-defvar-service opencode-jobs--held nil
  "Job events received during the listing, newest first.")

(opencode-defvar-service opencode-jobs--again nil
  "Non-nil when a launch during the listing needs a newer answer.")

(opencode-defvar-service opencode-jobs--timer nil
  "Timer coalescing changes that call for a job listing, or retrying one.")

(opencode-defvar-service opencode-jobs--delay 1
  "Seconds before retrying a failed job listing.")

(opencode-defvar-service opencode-jobs--unsupported nil
  "Non-nil when this service cannot list jobs, until it reconnects.")

(defvar opencode-jobs-changed-functions nil
  "Functions called with the session IDs whose jobs changed.
Nil means the first listing settled, so every client reconciles.")

(defun opencode-job-id (tool metadata)
  "Return the background job ID named by TOOL's result METADATA, or nil."
  (pcase tool
    ("shell" (and (equal (alist-get 'status metadata) "running")
                  (alist-get 'shellID metadata)))
    ("subagent" (and (equal (alist-get 'status metadata) "running")
                     (alist-get 'sessionID metadata)))
    ("experimental_image_generate" (alist-get 'jobID metadata))))

(defun opencode-job (id)
  "Return job ID as last reported by the service, or nil when not listed."
  (gethash id (opencode-sv opencode-jobs--jobs)))

(defun opencode-jobs (session)
  "Return the running jobs owned by SESSION, oldest first.
Each is a service job alist; its time is in epoch milliseconds."
  (gethash session (opencode-sv opencode-jobs--sessions)))

(defun opencode-jobs--install (jobs &optional ready)
  "Install JOBS, a table, and report changes; READY settles the first listing."
  (let ((old (opencode-sv opencode-jobs--jobs))
        (sessions (make-hash-table :test #'equal))
        (changed nil)
        (first (and ready (not (opencode-sv opencode-jobs--ready)))))
    (maphash (lambda (id job)
               (unless (equal job (gethash id jobs))
                 (cl-pushnew (alist-get 'sessionID job) changed :test #'equal))) old)
    (maphash (lambda (id job)
               (unless (equal job (gethash id old))
                 (cl-pushnew (alist-get 'sessionID job) changed :test #'equal))
               (when (equal (alist-get 'status job) "running")
                 (push job (gethash (alist-get 'sessionID job) sessions)))) jobs)
    (maphash (lambda (id entries)
               (puthash id (sort entries :key (lambda (job) (or (alist-get 'started (alist-get 'time job)) 0)))
                        sessions)) sessions)
    (setf (opencode-sv opencode-jobs--jobs) jobs
          (opencode-sv opencode-jobs--sessions) sessions)
    (when ready (setf (opencode-sv opencode-jobs--ready) t))
    (when (or first changed)
      (run-hook-wrapped 'opencode-jobs-changed-functions
                        (lambda (function ids)
                          (with-demoted-errors "OpenCode jobs hook: %S" (funcall function ids))
                          nil)
                        (unless first (delq nil changed))))))

(defun opencode-jobs--completion (event)
  "Return the job-completion metadata of EVENT, or nil."
  (let* ((data (alist-get 'data event))
         (metadata (pcase (alist-get 'type event)
                     ("session.synthetic" (alist-get 'metadata data))
                     ("session.inbox.enqueued"
                      (alist-get 'metadata (alist-get 'payload (alist-get 'item data)))))))
    (and (member (alist-get 'state metadata) '("completed" "error" "cancelled")) metadata)))

(defun opencode-jobs--launch (event)
  "Return the running job announced by tool-success EVENT, or nil."
  (when (equal (alist-get 'type event) "session.tool.success")
    (let-alist (alist-get 'data event)
      (let* ((kind (cond ((and (equal .metadata.status "running") .metadata.shellID) "shell")
                         ((and (equal .metadata.status "running") .metadata.sessionID) "subagent")
                         ((and (stringp .metadata.jobID) (string-prefix-p "image_" .metadata.jobID)) "image")))
             (id (pcase kind ("shell" .metadata.shellID) ("subagent" .metadata.sessionID) (_ .metadata.jobID))))
        (when (and kind id .sessionID)
          `((id . ,id) (kind . ,kind) (sessionID . ,.sessionID) (status . "running")
            (time (started . ,(alist-get 'created event)))))))))

(defun opencode-jobs--apply (jobs event)
  "Apply job EVENT to JOBS, a table, without notifying clients."
  (let* ((data (alist-get 'data event))
         (type (alist-get 'type event))
         (metadata (opencode-jobs--completion event))
         (id (or (alist-get 'jobID metadata) (alist-get 'shellID metadata)
                 (alist-get 'childID metadata) (alist-get 'id data)))
         (job (gethash id jobs)))
    (cond
     ((opencode-jobs--launch event)
      (let* ((launch (opencode-jobs--launch event))
             (id (alist-get 'id launch))
             (old (gethash id jobs)))
        ;; Shell and image IDs are never reused; a subagent can be continued.
        (when (or (null old) (and (equal (alist-get 'kind launch) "subagent")
                                  (not (equal (alist-get 'status old) "running"))))
          (puthash id launch jobs))))
     ((equal type "shell.created")
      (let-alist (alist-get 'info data)
        (when (and .metadata.sessionID (equal .status "running"))
          (puthash .id `((id . ,.id) (kind . "shell") (sessionID . ,.metadata.sessionID)
                         (shellID . ,.id) (command . ,.command) (status . "running") (time . ,.time)) jobs))))
     ((equal type "session.deleted")
      (maphash (lambda (key job)
                 (when (equal (alist-get 'sessionID job) (alist-get 'sessionID data))
                   (remhash key jobs))) jobs))
     ((and job (or metadata (equal type "shell.exited")
                   (and (equal type "shell.deleted") (equal (alist-get 'status job) "running"))))
      (let* ((state (or (alist-get 'state metadata)
                        (if (or (equal type "shell.deleted") (equal (alist-get 'status data) "killed"))
                            "cancelled" "completed")))
             (result (or metadata
                         `((state . ,state) (exit . ,(alist-get 'exit data))
                           ,@(when (equal (alist-get 'status data) "timeout") '((timeout . t))))))
             (next (copy-alist job)))
        (setf (alist-get 'status next) state
              (alist-get 'metadata next) result)
        (puthash id next jobs))))))

(defun opencode-jobs--schedule (&optional delay)
  "List jobs after DELAY seconds, zero by default, coalescing callers."
  (unless (or (opencode-sv opencode-jobs--unsupported) (timerp (opencode-sv opencode-jobs--timer)))
    (setf (opencode-sv opencode-jobs--timer)
          (apply #'run-at-time (or delay 0) nil #'opencode-jobs--list (opencode--timer-arguments)))))

(defun opencode-jobs--list (&optional service)
  "List the background jobs of SERVICE, the local one when nil."
  (opencode-with-service service
    (when (timerp (opencode-sv opencode-jobs--timer))
      (cancel-timer (opencode-sv opencode-jobs--timer)))
    (setf (opencode-sv opencode-jobs--timer) nil)
    (cond
     ((opencode-sv opencode-jobs--listing) (setf (opencode-sv opencode-jobs--again) t))
     ((and (not (opencode-sv opencode-jobs--unsupported)) (opencode-events-connected-p))
      (let ((generation (cl-incf (opencode-sv opencode-jobs--generation))))
        (setf (opencode-sv opencode-jobs--listing) t
              (opencode-sv opencode-jobs--held) nil
              (opencode-sv opencode-jobs--again) nil)
        (opencode-request
         'get "/api/experimental/job" :directory nil :buffer nil
         :then (lambda (response)
                 (when (= generation (opencode-sv opencode-jobs--generation))
                   (let ((jobs (make-hash-table :test #'equal)))
                     (dolist (job (opencode-request-data response))
                       (puthash (alist-get 'id job) job jobs))
                     (dolist (event (reverse (opencode-sv opencode-jobs--held)))
                       (opencode-jobs--apply jobs event))
                     (setf (opencode-sv opencode-jobs--listing) nil
                           (opencode-sv opencode-jobs--held) nil
                           (opencode-sv opencode-jobs--delay) 1)
                     (opencode-jobs--install jobs t)
                     (when (opencode-sv opencode-jobs--again) (opencode-jobs--schedule)))))
         :else (lambda (error)
                 (when (= generation (opencode-sv opencode-jobs--generation))
                   (setf (opencode-sv opencode-jobs--listing) nil
                         (opencode-sv opencode-jobs--held) nil)
                   (if (eql (opencode-error-status error) 404)
                       (progn
                         (setf (opencode-sv opencode-jobs--unsupported) t)
                         (opencode-jobs--install (opencode-sv opencode-jobs--jobs) t))
                     (message "OpenCode: cannot list background jobs: %s" (opencode-error-describe error))
                     (opencode-jobs--schedule (opencode-sv opencode-jobs--delay))
                     (setf (opencode-sv opencode-jobs--delay)
                           (min 60 (* 2 (opencode-sv opencode-jobs--delay)))))))))))))

(defun opencode-jobs--event (event)
  "Follow jobs starting or ending in EVENT and reconcile with the service."
  (let* ((type (alist-get 'type event))
         (launch (opencode-jobs--launch event))
         (completion (opencode-jobs--completion event)))
    (when (or launch completion
              (member type '("shell.created" "shell.exited" "shell.deleted" "session.deleted"
                             "session.execution.succeeded" "session.execution.failed" "session.execution.interrupted")))
      (when (opencode-sv opencode-jobs--listing)
        (push event (opencode-sv opencode-jobs--held))
        (setf (opencode-sv opencode-jobs--again) t))
      (let ((jobs (copy-hash-table (opencode-sv opencode-jobs--jobs))))
        (opencode-jobs--apply jobs event)
        (opencode-jobs--install jobs))
      (opencode-jobs--schedule))))

(defun opencode-jobs--closed (_service)
  "Forget requests and timers of the current service, which was unregistered."
  (when (timerp (opencode-sv opencode-jobs--timer)) (cancel-timer (opencode-sv opencode-jobs--timer)))
  (cl-incf (opencode-sv opencode-jobs--generation))
  (setf (opencode-sv opencode-jobs--timer) nil
        (opencode-sv opencode-jobs--listing) nil
        (opencode-sv opencode-jobs--held) nil
        (opencode-sv opencode-jobs--ready) nil))

(defun opencode-jobs--connected (_event)
  "Reconcile background jobs on the stream that just connected."
  (opencode-jobs--closed nil)
  (setf (opencode-sv opencode-jobs--unsupported) nil)
  (opencode-jobs--list (opencode--remote-service)))

(opencode-events-register "server.connected" #'opencode-jobs--connected)
(add-hook 'opencode-service-unregistered-functions #'opencode-jobs--closed)
(dolist (type '("shell.created" "shell.exited" "shell.deleted" "session.tool.success" "session.deleted"
                "session.execution.succeeded" "session.execution.failed" "session.execution.interrupted"
                "session.synthetic" "session.inbox.enqueued"))
  (opencode-events-register type #'opencode-jobs--event))

(provide 'opencode-jobs)
;;; opencode-jobs.el ends here
