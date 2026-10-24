;;; opencode-service.el --- Find and start the shared OpenCode service  -*- lexical-binding: t; -*-

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

;; OpenCode 2 runs one detached background service per user, shared by
;; the TUI, the desktop app and this client.  The service registers its
;; URL, process ID, version and password in a JSON file under the XDG
;; state directory; a client reads it, probes `/api/info' and, when no
;; service answers, asks the CLI to start one.  Emacs never owns the
;; service: disconnecting or exiting Emacs leaves it running, and a new
;; binary is picked up through `opencode service restart'.
;;
;; On its first start the service imports OpenCode 1's session history
;; in the background while it already answers, and until the import
;; reaches a session, that session looks deleted.  So the service is
;; ready only once its import status says completed: requests wait for
;; it meanwhile, and an import that fails or stalls is reported and then
;; left behind.

;;; Code:

(require 'opencode-common)
(require 'plz)

(defcustom opencode-command "opencode"
  "Executable of the OpenCode CLI."
  :type 'string
  :group 'opencode)

(defcustom opencode-service-registration-file nil
  "File in which the OpenCode service registers its URL and password.
When nil, use the default of the release channels:
`$XDG_STATE_HOME/opencode/service.json', or
`~/.local/state/opencode/service.json' without XDG_STATE_HOME, on
every platform."
  :type '(choice (const :tag "Default location" nil) file)
  :group 'opencode)

(defcustom opencode-service-start-timeout 120
  "Seconds to wait for a starting service to answer."
  :type 'number
  :group 'opencode)

(defcustom opencode-service-import-stall-timeout 300
  "Seconds the service's import of earlier sessions may make no progress.
Requests wait while the service imports OpenCode 1's sessions; after
this long without progress they stop waiting, with a warning."
  :type 'number
  :group 'opencode)

(defcustom opencode-service-stale-functions nil
  "Abnormal hook run when the service runs another version than the CLI.
Each function receives the service's version and the installed one."
  :type 'hook
  :group 'opencode)

(defvar opencode-service-ready-functions nil
  "Abnormal hook run with the connection each time a service is ready for Emacs.")

(defvar opencode-service-connection nil
  "Plist (:url :password :pid :version :id) of the service Emacs uses.")

(defvar opencode-service--waiters nil
  "Callbacks (SUCCESS . FAILURE) waiting for the running `opencode-service-ensure'.")

(defvar opencode-service--starter nil
  "Process running `opencode service start' or `restart', if any.")

(defvar opencode-service--installed-version nil
  "Cons (MTIME . VERSION) of the last `opencode --version' result.")

(defvar opencode-service--import-checked nil
  "Cons (ID . PID) of the service whose import Emacs no longer waits for.")

(defvar opencode-service--import-interval 1
  "Seconds between two import status requests.")

(defun opencode-service-registration-file ()
  "Return the service registration file name."
  (or opencode-service-registration-file
      (expand-file-name "opencode/service.json"
                        (or (getenv "XDG_STATE_HOME") "~/.local/state"))))

(defun opencode-service-read-registration ()
  "Return the registration as a plist (:url :password :pid :version :id), or nil."
  (let ((file (opencode-service-registration-file)))
    (when (file-readable-p file)
      (condition-case nil
          (let ((json (with-temp-buffer
                        (insert-file-contents file)
                        (json-parse-buffer :object-type 'alist :null-object nil
                                           :false-object nil))))
            (let-alist json
              (when (and (stringp .url) (stringp .password))
                (list :url (string-remove-suffix "/" .url) :password .password
                      :pid .pid :version .version :id .id))))
        (error nil)))))

(defun opencode-service-authorization (connection)
  "Return the Basic authorization header value for CONNECTION."
  (concat "Basic " (base64-encode-string
                    (encode-coding-string
                     (concat "opencode:" (plist-get connection :password)) 'utf-8)
                    t)))

(defun opencode-service--probe (connection callback)
  "Probe CONNECTION's `/api/info' and call CALLBACK with the outcome.
The outcome is `ready' when the registered process answers, `starting'
while it boots, and nil when nothing usable answers."
  (condition-case nil
      (plz 'get (concat (plist-get connection :url) "/api/info")
        :headers `(("Authorization" . ,(opencode-service-authorization connection)))
        :as (lambda () (json-parse-buffer :object-type 'alist :null-object nil
                                          :false-object nil))
        :connect-timeout 2 :timeout 5 :noquery t
        :then (lambda (info)
                (funcall callback
                         (if (or (null (plist-get connection :pid))
                                 (equal (alist-get 'pid info) (plist-get connection :pid)))
                             'ready
                           nil)))
        :else (lambda (error)
                (funcall callback
                         (when-let* ((response (plz-error-response error)))
                           (and (eql (plz-response-status response) 503) 'starting)))))
    (error (funcall callback nil))))

(defun opencode-service--finish (connection error)
  "Settle every waiter with CONNECTION, or with ERROR when CONNECTION is nil."
  (let ((waiters (nreverse opencode-service--waiters)))
    (setq opencode-service--waiters nil)
    (when connection
      (setq opencode-service-connection connection))
    (dolist (waiter waiters)
      (condition-case err
          (if connection
              (funcall (car waiter) connection)
            (when (cdr waiter) (funcall (cdr waiter) error)))
        (error (message "OpenCode: %s" (error-message-string err)))))
    (when connection
      (run-hook-with-args 'opencode-service-ready-functions connection)
      (opencode-service-check-version connection))))

(defun opencode-service--import-key (connection)
  "Return the cons (ID . PID) naming CONNECTION's service process."
  (cons (plist-get connection :id) (plist-get connection :pid)))

(defun opencode-service--import-progress (status)
  "Return the text of the progress reported in import STATUS."
  (let-alist (alist-get 'progress status)
    (if (not .label)
        "waiting to import the earlier sessions"
      (concat .label
              (cond ((and .numerator .denominator) (format " %d/%d" .numerator .denominator))
                    (.numerator (format " %d" .numerator)))))))

(defun opencode-service--import-done (connection)
  "Stop waiting for the import of CONNECTION's service and settle the waiters."
  (setq opencode-service--import-checked (opencode-service--import-key connection))
  (opencode-service--finish connection nil))

(defun opencode-service--plz-error-text (error)
  "Return a short description of plz ERROR."
  (let ((response (plz-error-response error)))
    (cond (response (format "HTTP %d" (plz-response-status response)))
          ((plz-error-curl-error error)
           (format "curl error %d" (car (plz-error-curl-error error))))
          (t (or (plz-error-message error) "no answer")))))

(defun opencode-service--await-import (connection &optional progress deadline)
  "Settle the waiters with CONNECTION once its service imported the earlier sessions.
PROGRESS is the progress text last reported, and DEADLINE the time
after which the import counts as stalled unless it progresses."
  (if (equal opencode-service--import-checked (opencode-service--import-key connection))
      (opencode-service--finish connection nil)
    (let ((deadline (or deadline (+ (float-time) opencode-service-import-stall-timeout))))
      (condition-case err
          (plz 'get (concat (plist-get connection :url) "/api/experimental/migration/v1")
            :headers `(("Authorization" . ,(opencode-service-authorization connection)))
            :as (lambda () (json-parse-buffer :object-type 'alist :null-object nil
                                              :false-object nil))
            :connect-timeout 2 :timeout 5 :noquery t
            :then
            (lambda (status)
              (pcase (alist-get 'status status)
                ("completed"
                 (when progress (message "OpenCode service: earlier sessions imported"))
                 (opencode-service--import-done connection))
                ("error"
                 (display-warning
                  'opencode (format "The OpenCode service could not import the earlier sessions: %s"
                                    (alist-get 'error status)))
                 (opencode-service--import-done connection))
                (_
                 (let ((current (opencode-service--import-progress status)))
                   (unless (equal current progress)
                     (message "OpenCode service: %s…" current)
                     (setq deadline (+ (float-time) opencode-service-import-stall-timeout)))
                   (if (> (float-time) deadline)
                       (progn
                         (display-warning
                          'opencode
                          (format "The OpenCode service's import of earlier sessions made no progress for %s s; sessions it has yet to import look deleted"
                                  opencode-service-import-stall-timeout))
                         (opencode-service--import-done connection))
                     (run-at-time opencode-service--import-interval nil
                                  #'opencode-service--await-import connection current deadline))))))
            :else
            (lambda (error)
              (if (eql 404 (when-let* ((response (plz-error-response error)))
                             (plz-response-status response)))
                  ;; A service without the import route has nothing to import.
                  (opencode-service--import-done connection)
                (opencode-service--finish
                 nil (format "Cannot read the OpenCode service's import of earlier sessions (%s)"
                             (opencode-service--plz-error-text error))))))
        (error (opencode-service--finish
                nil (format "Cannot read the OpenCode service's import of earlier sessions (%s)"
                            (error-message-string err))))))))

(defun opencode-service--process-environment ()
  "Return the environment for the CLI, without credentials of an older server."
  (seq-remove (lambda (entry)
                (string-match-p "\\`OPENCODE_\\(?:SERVER_\\)?PASSWORD=" entry))
              process-environment))

(defun opencode-service--run-cli (arguments on-exit)
  "Run the CLI with ARGUMENTS and call ON-EXIT with its exit status and output.
Output goes to a hidden buffer that is killed afterwards."
  (let* ((buffer (generate-new-buffer " *opencode-service*"))
         (process-environment (opencode-service--process-environment))
         (default-directory (expand-file-name "~/")))
    (setq opencode-service--starter
          (make-process
           :name "opencode-service" :buffer buffer :noquery t
           :connection-type 'pipe
           :command (cons opencode-command arguments)
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (let ((output (with-current-buffer (process-buffer process)
                               (string-trim (buffer-string)))))
                 (kill-buffer (process-buffer process))
                 (when (eq opencode-service--starter process)
                   (setq opencode-service--starter nil))
                 (funcall on-exit (process-exit-status process) output))))))))

(defun opencode-service--await (deadline)
  "Poll the registration until its service is ready or DEADLINE passes."
  (let ((connection (opencode-service-read-registration)))
    (if (not connection)
        (if (> (float-time) deadline)
            (opencode-service--finish nil "The OpenCode service did not register itself")
          (run-at-time 0.25 nil #'opencode-service--await deadline))
      (opencode-service--probe
       connection
       (lambda (outcome)
         (cond
          ((eq outcome 'ready) (opencode-service--await-import connection))
          ((> (float-time) deadline)
           (opencode-service--finish nil "The OpenCode service did not become ready"))
          (t (run-at-time 0.25 nil #'opencode-service--await deadline))))))))

(defun opencode-service--start ()
  "Ask the CLI to start the service, then wait for it to answer."
  (let ((deadline (+ (float-time) opencode-service-start-timeout)))
    (condition-case err
        (opencode-service--run-cli
         '("service" "start")
         (lambda (status output)
           (if (zerop status)
               (opencode-service--await deadline)
             (opencode-service--finish
              nil (format "opencode service start failed: %s" output)))))
      (error (opencode-service--finish
              nil (format "Cannot run %s: %s" opencode-command (error-message-string err)))))))

(defun opencode-service-ensure (callback &optional errback)
  "Call CALLBACK with the connection of a ready service, starting one if needed.
ERRBACK, when given, receives an error message instead on failure."
  (let ((first (null opencode-service--waiters)))
    (push (cons callback errback) opencode-service--waiters)
    (when first
      (let ((connection (opencode-service-read-registration)))
        (if (not connection)
            (opencode-service--start)
          (opencode-service--probe
           connection
           (lambda (outcome)
             (pcase outcome
               ('ready (opencode-service--await-import connection))
               ('starting
                (opencode-service--await (+ (float-time) opencode-service-start-timeout)))
               (_ (opencode-service--start))))))))))

(defun opencode-service-installed-version ()
  "Return the version of the installed CLI, or nil when unknown.
The result is cached per executable modification time."
  (when-let* ((executable (executable-find opencode-command))
              (mtime (file-attribute-modification-time (file-attributes executable))))
    (if (equal (car opencode-service--installed-version) mtime)
        (cdr opencode-service--installed-version)
      (let ((version
             (with-temp-buffer
               (when (eql 0 (ignore-errors
                              (let ((process-environment (opencode-service--process-environment)))
                                (call-process executable nil t nil "--version"))))
                 (goto-char (point-max))
                 (when (re-search-backward "v?\\([0-9][^ \t\n]*\\)" nil t)
                   (match-string 1))))))
        (setq opencode-service--installed-version (cons mtime version))
        version))))

(defun opencode-service-stale-p (&optional connection)
  "Return non-nil when CONNECTION's service runs another version than the CLI."
  (let ((service (plist-get (or connection opencode-service-connection) :version))
        (installed (opencode-service-installed-version)))
    (and service installed (not (equal service installed)))))

(defun opencode-service-check-version (connection)
  "Run `opencode-service-stale-functions' when CONNECTION's service is stale."
  (when (opencode-service-stale-p connection)
    (run-hook-with-args 'opencode-service-stale-functions
                        (plist-get connection :version)
                        (opencode-service-installed-version))))

(defun opencode-service-running-sessions ()
  "Return the number of sessions running a turn, or nil when the service is unreachable."
  (when-let* ((connection (or opencode-service-connection (opencode-service-read-registration))))
    (condition-case nil
        (let ((response (plz 'get (concat (plist-get connection :url) "/api/session/active")
                          :headers `(("Authorization" . ,(opencode-service-authorization connection)))
                          :as (lambda () (json-parse-buffer :object-type 'alist :null-object nil
                                                            :false-object nil))
                          :connect-timeout 2 :timeout 5 :noquery t)))
          (length (alist-get 'data response)))
      (error nil))))

(defun opencode-service-restart (&optional callback errback)
  "Restart the shared service, then call CALLBACK with the new connection.
ERRBACK, when given, receives an error message instead on failure.
Every client reconnects by itself; running turns resume after the
restart, though their current step starts over, so interactively this
asks first while any runs."
  (interactive)
  (when (process-live-p opencode-service--starter)
    (user-error "An OpenCode service start or restart is already running"))
  (when-let* (((called-interactively-p 'any))
              (running (opencode-service-running-sessions))
              ((> running 0)))
    (unless (yes-or-no-p (format "%d OpenCode session%s running a turn; restart the service anyway? "
                                 running (if (= running 1) " is" "s are")))
      (user-error "Restart cancelled")))
  (setq opencode-service-connection nil)
  (cl-flet ((fail (text)
              (if errback (funcall errback text) (message "OpenCode: %s" text))))
    (opencode-service--run-cli
     '("service" "restart")
     (lambda (status output)
       (if (zerop status)
           (opencode-service-ensure
            (lambda (connection)
              (message "OpenCode service restarted (%s)" (plist-get connection :version))
              (when callback (funcall callback connection)))
            #'fail)
         (fail (format "opencode service restart failed: %s" output)))))))

(provide 'opencode-service)
;;; opencode-service.el ends here
