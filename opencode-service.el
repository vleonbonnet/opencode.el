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
;;
;; While Emacs restarts or stops the service, requests wait for it to
;; finish rather than find the service going away or start another one
;; beside the CLI's, and then look for a service again, starting one.
;;
;; Every request goes through `opencode-transport-request', over a
;; socket Emacs opens itself: no request starts a process.

;;; Code:

(require 'opencode-common)
(require 'opencode-transport)

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

(opencode-defvar-service opencode-service-connection nil
  "Plist (:url :password :pid :version :id) of the service Emacs uses.")

(opencode-defvar-service opencode-service--waiters nil
  "Callbacks (SUCCESS . FAILURE) waiting for the running `opencode-service-ensure'.")

(opencode-defvar-service opencode-service--lookup 0
  "Count of the lookups for a ready service; only the latest settles the waiters.
A restart or a stop counts as a new one, so that a lookup in flight,
which may have found the service going away, settles nothing.")

(defvar opencode-service--starter nil
  "Process running `opencode service start', `restart' or `stop', if any.
While it runs, the waiters wait for it rather than look for a service.")

(defvar opencode-service--installed-version nil
  "Cons ((EXECUTABLE . MTIME) . VERSION) of the last `opencode --version' result.")

(opencode-defvar-service opencode-service--import-checked nil
  "Cons (ID . PID) of the service whose import Emacs no longer waits for.")

(defvar opencode-service--import-interval 1
  "Seconds between two import status requests.")

;;;; Remote services

(defvar opencode-service-registered-functions nil
  "Abnormal hook run with a remote service as it is registered, for it.")

(cl-defun opencode-service-register (name &key label connect files)
  "Register the remote OpenCode service NAME, a string, and return it.
CONNECT is a function of two arguments, CALLBACK and ERRBACK, telling
how to reach the service: it calls CALLBACK, now or later, with a
connection plist (:url URL :password PASSWORD :id ID :version VERSION),
URL being an http URL or a relay's, relay://NAME (see
`opencode-transport-register-relay'), or ERRBACK with a line telling
why it cannot.  It is asked again whenever the service cannot be
reached or refuses the password.  LABEL names the service in buffer
names, mode lines and messages, NAME by default.  FILES non-nil says
the files its sessions name are this host's, which they are not by
default: Emacs then neither visits, processes nor links them.

Emacs never starts, restarts nor stops a remote service.  Its session
buffers follow it through an event stream of its own, open while any
is; its registry holds their sessions and those around them, not every
session of the service.  Registering NAME again updates it, and opens
it again once unregistered (see `opencode-service-unregister'), its
buffers following it anew."
  (unless (and (stringp name) (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9._-]*\\'" name)
               (not (equal name "local")))
    (error "Invalid name of an OpenCode service: %S" name))
  (unless (functionp connect)
    (error "OpenCode service %s needs a function telling how to reach it" name))
  (let ((service (or (gethash name opencode--services)
                     (puthash name (opencode-svc--make :name name :remote t) opencode--services))))
    (setf (opencode-svc-label service) label
          (opencode-svc-connect service) connect
          (opencode-svc-files service) files
          (opencode-svc-closed service) nil)
    ;; Asked anew: its provider may know another way now.
    (opencode-with-service service
      (setf (opencode-sv opencode-service-connection) nil)
      (run-hook-with-args 'opencode-service-registered-functions service))
    service))

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

(defun opencode-service-same-p (connection other)
  "Return non-nil when CONNECTION and OTHER reach one service process."
  (and connection other
       (equal (plist-get connection :url) (plist-get other :url))
       (equal (plist-get connection :pid) (plist-get other :pid))
       (equal (plist-get connection :id) (plist-get other :id))))

;;;; Finding a ready service

(defun opencode-service--probe (connection callback)
  "Probe CONNECTION's `/api/info' and call CALLBACK with the outcome.
The outcome is `ready' when the registered process answers, `starting'
while it boots, and nil when nothing usable answers.  CALLBACK runs for
the current service."
  (let ((service (opencode--remote-service)))
    (condition-case nil
        (opencode-transport-request
         'get (concat (plist-get connection :url) "/api/info")
         :headers `(("Authorization" . ,(opencode-service-authorization connection)))
         :as (lambda () (json-parse-buffer :object-type 'alist :null-object nil
                                           :false-object nil))
         :connect-timeout 2 :timeout (if service 15 5)
         :then (lambda (info)
                 (opencode-with-service service
                   (funcall callback
                            (if (or (null (plist-get connection :pid))
                                    (equal (alist-get 'pid info) (plist-get connection :pid)))
                                'ready
                              nil))))
         :else (lambda (failure)
                 (opencode-with-service service
                   (funcall callback
                            (and (eql (opencode-transport-failure-status failure) 503) 'starting)))))
      (error (funcall callback nil)))))

(defun opencode-service--finish (lookup connection error)
  "Settle every waiter with CONNECTION, or with ERROR when CONNECTION is nil.
Nothing happens once LOOKUP is no longer the latest lookup."
  (when (eql lookup (opencode-sv opencode-service--lookup))
    (let ((waiters (nreverse (opencode-sv opencode-service--waiters))))
      (setf (opencode-sv opencode-service--waiters) nil)
      (when connection
        (setf (opencode-sv opencode-service-connection) connection))
      (dolist (waiter waiters)
        (condition-case err
            (if connection
                (funcall (car waiter) connection)
              (when (cdr waiter) (funcall (cdr waiter) error)))
          (error (message "OpenCode: %s" (error-message-string err)))))
      (when connection
        (run-hook-with-args 'opencode-service-ready-functions connection)
        (unless (opencode--remote-service)
          (opencode-service-check-version connection))))))

(defun opencode-service--withdraw (waiter lookup error)
  "Fail WAITER alone with ERROR, then look for a service for the others.
Nothing happens once LOOKUP is no longer the latest lookup."
  (when (eql lookup (opencode-sv opencode-service--lookup))
    (setf (opencode-sv opencode-service--waiters) (delq waiter (opencode-sv opencode-service--waiters)))
    (condition-case err
        (when (cdr waiter) (funcall (cdr waiter) error))
      (error (message "OpenCode: %s" (error-message-string err))))
    (when (opencode-sv opencode-service--waiters)
      (opencode-service--find))))

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

(defun opencode-service--import-done (lookup connection)
  "Stop waiting for the import of CONNECTION's service and settle LOOKUP's waiters."
  (setf (opencode-sv opencode-service--import-checked) (opencode-service--import-key connection))
  (opencode-service--finish lookup connection nil))

(defun opencode-service--failure-text (failure)
  "Return a short description of request FAILURE."
  (if-let* ((status (opencode-transport-failure-status failure)))
      (format "HTTP %d" status)
    (opencode-transport-failure-describe failure)))

(defun opencode-service--await-import (lookup connection &optional progress deadline service)
  "Settle LOOKUP's waiters with CONNECTION once its service imported old sessions.
PROGRESS is the progress text last reported, and DEADLINE the time
after which the import counts as stalled unless it progresses.
SERVICE, nil for the local one, is the service whose waiters these are."
  (opencode-with-service service
    (opencode-service--await-import-1 lookup connection progress deadline)))

(defun opencode-service--await-import-1 (lookup connection progress deadline)
  "Do the work of `opencode-service--await-import' for the current service.
LOOKUP, CONNECTION, PROGRESS and DEADLINE are as there."
  (cond
   ((not (eql lookup (opencode-sv opencode-service--lookup))))
   ((equal (opencode-sv opencode-service--import-checked) (opencode-service--import-key connection))
    (opencode-service--finish lookup connection nil))
   (t
    (let ((deadline (or deadline (+ (float-time) opencode-service-import-stall-timeout))))
      (condition-case err
          (opencode-transport-request
           'get (concat (plist-get connection :url) "/api/experimental/migration/v1")
           :headers `(("Authorization" . ,(opencode-service-authorization connection)))
           :as (lambda () (json-parse-buffer :object-type 'alist :null-object nil
                                             :false-object nil))
           :connect-timeout 2 :timeout 5
           :then
           (lambda (status)
             (pcase (alist-get 'status status)
               ("completed"
                (when progress (message "OpenCode service: earlier sessions imported"))
                (opencode-service--import-done lookup connection))
               ("error"
                (display-warning
                 'opencode (format "The OpenCode service could not import the earlier sessions: %s"
                                   (alist-get 'error status)))
                (opencode-service--import-done lookup connection))
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
                        (opencode-service--import-done lookup connection))
                    (apply #'run-at-time opencode-service--import-interval nil
                                 #'opencode-service--await-import
                                 lookup connection current deadline
                                 (opencode--timer-arguments)))))))
           :else
           (lambda (failure)
             (if (eql 404 (opencode-transport-failure-status failure))
                 ;; A service without the import route has nothing to import.
                 (opencode-service--import-done lookup connection)
               (opencode-service--finish
                lookup nil (format "Cannot read the OpenCode service's import of earlier sessions (%s)"
                                   (opencode-service--failure-text failure))))))
        (error (opencode-service--finish
                lookup nil (format "Cannot read the OpenCode service's import of earlier sessions (%s)"
                                   (error-message-string err)))))))))

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
                 ;; The CLI changes the local service only.
                 (opencode-with-service nil
                   (funcall on-exit (process-exit-status process) output)))))))))

(defun opencode-service--await (lookup deadline)
  "Poll the registration until its service is ready or DEADLINE passes.
The service then settles LOOKUP's waiters.  Only the local service has
a registration."
  (opencode-with-service nil
    (opencode-service--await-1 lookup deadline)))

(defun opencode-service--await-1 (lookup deadline)
  "Do the work of `opencode-service--await' for LOOKUP and DEADLINE."
  (when (eql lookup (opencode-sv opencode-service--lookup))
    (let ((connection (opencode-service-read-registration)))
      (if (not connection)
          (if (> (float-time) deadline)
              (opencode-service--finish lookup nil "The OpenCode service did not register itself")
            (run-at-time 0.25 nil #'opencode-service--await lookup deadline))
        (opencode-service--probe
         connection
         (lambda (outcome)
           (cond
            ((eq outcome 'ready) (opencode-service--await-import lookup connection))
            ((> (float-time) deadline)
             (opencode-service--finish lookup nil "The OpenCode service did not become ready"))
            (t (run-at-time 0.25 nil #'opencode-service--await lookup deadline)))))))))

(defun opencode-service--start (lookup)
  "Ask the CLI to start the service, then settle LOOKUP's waiters once it answers."
  (let ((deadline (+ (float-time) opencode-service-start-timeout)))
    (condition-case err
        (opencode-service--run-cli
         '("service" "start")
         (lambda (status output)
           (if (zerop status)
               (opencode-service--await lookup deadline)
             (opencode-service--finish
              lookup nil (format "opencode service start failed: %s" output)))))
      (error (opencode-service--finish
              lookup nil (format "Cannot run %s: %s" opencode-command (error-message-string err)))))))

(defun opencode-service--find-remote (service lookup)
  "Ask remote SERVICE's provider for its connection, then settle LOOKUP's waiters.
The waiters get the connection once its service answers; a remote
service is never started nor waited for to import anything."
  (let ((label (opencode-service-label service)))
    (cl-flet ((fail (text)
                ;; Never before `opencode-service-ensure' returns.
                (run-at-time 0 nil (lambda ()
                                     (opencode-with-service service
                                       (opencode-service--finish lookup nil text))))))
      (condition-case err
          (if (opencode-svc-closed service)
              (fail (format "The OpenCode service of %s is closed" label))
            (funcall (opencode-svc-connect service)
                   (lambda (connection)
                     (opencode-with-service service
                       (cond
                        ((not (eql lookup (opencode-sv opencode-service--lookup))))
                        ((not (and (consp connection) (stringp (plist-get connection :url))))
                         (fail (format "No OpenCode service of %s to connect to" label)))
                        (t
                         (opencode-service--probe
                          connection
                          (lambda (outcome)
                            (if (eq outcome 'ready)
                                (opencode-service--finish lookup connection nil)
                              (fail (format "The OpenCode service of %s does not answer" label)))))))))
                   (lambda (text)
                     (fail (format "Cannot reach the OpenCode service of %s: %s" label text)))))
        (error (fail (format "Cannot reach the OpenCode service of %s: %s"
                             label (error-message-string err))))))))

(defun opencode-service--find ()
  "Look for a ready service for the waiters, starting one when none answers.
A remote service is asked of its provider (see `opencode-service-register')."
  (let ((lookup (cl-incf (opencode-sv opencode-service--lookup)))
        (connection (and (not (opencode--remote-service)) (opencode-service-read-registration))))
    (if-let* ((service (opencode--remote-service)))
        (opencode-service--find-remote service lookup)
      (opencode-service--find-local lookup connection))))

(defun opencode-service--find-local (lookup connection)
  "Settle LOOKUP's waiters with the local service, CONNECTION registered or nil."
  (if (not connection)
      (opencode-service--start lookup)
    (opencode-service--probe
     connection
     (lambda (outcome)
       (when (eql lookup (opencode-sv opencode-service--lookup))
         (pcase outcome
           ('ready (opencode-service--await-import lookup connection))
           ('starting
            (opencode-service--await lookup (+ (float-time) opencode-service-start-timeout)))
           (_ (opencode-service--start lookup))))))))

(defun opencode-service-ensure (callback &optional errback)
  "Call CALLBACK with the connection of a ready service, starting one if needed.
ERRBACK, when given, receives an error message instead on failure.
While Emacs starts, restarts or stops the service, CALLBACK waits for
that to end.  Return a handle for `opencode-service-cancel'."
  (let ((first (null (opencode-sv opencode-service--waiters)))
        (waiter (cons callback errback)))
    (push waiter (opencode-sv opencode-service--waiters))
    ;; The CLI changing the service settles the waiters once it exits.
    (when (and first (or (opencode--remote-service) (not (process-live-p opencode-service--starter))))
      (opencode-service--find))
    waiter))

(defun opencode-service-cancel (handle)
  "Stop waiting for a service for the call of `opencode-service-ensure' HANDLE.
Neither of its functions runs then, and when nothing else waits, a
service stopped meanwhile is not started again for it."
  (setf (opencode-sv opencode-service--waiters) (delq handle (opencode-sv opencode-service--waiters))))

(defconst opencode-service--version-line
  (rx bos (* blank)
      (? (any alnum) (* (any alnum "_.-")) (+ blank))
      (? "v")
      (group (+ digit) "." (* (not blank)))
      (* blank) eos)
  "Regexp matching a line of `opencode --version' that is the version.
Such a line is the version, after an optional program name and \"v\";
group 1 is the version, as the service registers it.")

(defun opencode-service--parse-version (output)
  "Return the version that OUTPUT of `opencode --version' prints, or nil.
OpenCode 2 prints \"opencode v2.1.4\", the name being that of the build,
and OpenCode 1 \"2.1.4\"; the version may carry prerelease and build
suffixes, and is in bold on a terminal.  The first line that is a
version wins, so that lines such as warnings before or after it are
skipped."
  (seq-some (lambda (line)
              (when (string-match opencode-service--version-line line)
                (match-string 1 line)))
            (split-string (replace-regexp-in-string "\e\\[[0-9;]*m" "" output)
                          "[\r\n]+" t)))

(defun opencode-service-installed-version ()
  "Return the version of the installed CLI, or nil when unknown.
A build without a version, which calls itself \"local\", is unknown.
The result is cached per executable and modification time.  The CLI
answers in tens of milliseconds, which this waits for."
  (when-let* ((executable (executable-find opencode-command))
              (mtime (file-attribute-modification-time (file-attributes executable)))
              (key (cons executable mtime)))
    (if (equal (car opencode-service--installed-version) key)
        (cdr opencode-service--installed-version)
      (let ((version
             (with-temp-buffer
               ;; Only standard output holds the version; warnings go to standard error.
               (when (eql 0 (ignore-errors
                              (let ((process-environment (opencode-service--process-environment))
                                    (default-directory (expand-file-name "~/")))
                                (call-process executable nil '(t nil) nil "--version"))))
                 (opencode-service--parse-version (buffer-string))))))
        (setq opencode-service--installed-version (cons key version))
        version))))

(defun opencode-service-stale-p (&optional connection)
  "Return non-nil when CONNECTION's service runs another version than the CLI."
  (let ((service (plist-get (or connection (opencode-sv opencode-service-connection)) :version))
        (installed (opencode-service-installed-version)))
    (and service installed (not (equal service installed)))))

(defun opencode-service-check-version (connection)
  "Run `opencode-service-stale-functions' when CONNECTION's service is stale."
  (when (opencode-service-stale-p connection)
    (run-hook-with-args 'opencode-service-stale-functions
                        (plist-get connection :version)
                        (opencode-service-installed-version))))

(defun opencode-service-running-sessions ()
  "Return the number of sessions running a turn, or nil when unreachable.
That is the local service, which Emacs may restart or stop."
  (when-let* ((connection (or opencode-service-connection (opencode-service-read-registration))))
    (condition-case nil
        (let ((response (opencode-transport-request-sync
                         'get (concat (plist-get connection :url) "/api/session/active")
                         :headers `(("Authorization" . ,(opencode-service-authorization connection)))
                         :as (lambda () (json-parse-buffer :object-type 'alist :null-object nil
                                                           :false-object nil))
                         :connect-timeout 2 :timeout 5)))
          (length (alist-get 'data response)))
      (error nil))))

(defun opencode-service-restart (&optional callback errback)
  "Restart the shared service, then call CALLBACK with the new connection.
ERRBACK, when given, receives an error message instead on failure.
Requests made meanwhile wait for the restarted service.  Every client
reconnects by itself; running turns resume after the restart, though
their current step starts over, so interactively this asks first while
any runs.  It is the local service, whatever the current buffer: Emacs
never restarts a remote one."
  (interactive)
  (when (process-live-p opencode-service--starter)
    (user-error "An OpenCode service start, restart or stop is already running"))
  (when-let* (((called-interactively-p 'any))
              (running (opencode-service-running-sessions))
              ((> running 0)))
    (unless (yes-or-no-p (format "%d OpenCode session%s running a turn; restart the service anyway? "
                                 running (if (= running 1) " is" "s are")))
      (user-error "Restart cancelled")))
  (opencode-with-service nil
    (opencode-service--restart callback errback)))

(defun opencode-service--restart (callback errback)
  "Restart the local service, then call CALLBACK, else ERRBACK.
As `opencode-service-restart' does."
  (setf (opencode-sv opencode-service-connection) nil)
  (let ((lookup (cl-incf (opencode-sv opencode-service--lookup)))
        (waiter (cons (lambda (connection)
                        (message "OpenCode service restarted (%s)" (plist-get connection :version))
                        (when callback (funcall callback connection)))
                      (lambda (text)
                        (if errback (funcall errback text) (message "OpenCode: %s" text))))))
    ;; The waiters, and requests made until the restarted service is
    ;; ready, would otherwise find the service going away, or start
    ;; another one beside the CLI.
    (push waiter (opencode-sv opencode-service--waiters))
    (condition-case err
        (opencode-service--run-cli
         '("service" "restart")
         (lambda (status output)
           (if (zerop status)
               (opencode-service--await lookup (+ (float-time) opencode-service-start-timeout))
             (opencode-service--withdraw
              waiter lookup (format "opencode service restart failed: %s" output)))))
      (error (opencode-service--withdraw
              waiter lookup (format "Cannot run %s: %s" opencode-command (error-message-string err)))))))

(defun opencode-service--stop (on-exit)
  "Run `opencode service stop', then call ON-EXIT with its exit status and output.
Requests made meanwhile, and those waiting already, wait for the stop
to end, then look for a service again, which starts one.  The service
is the local one."
  (opencode-with-service nil
    (opencode-service--stop-1 on-exit)))

(defun opencode-service--stop-1 (on-exit)
  "Stop the local service, then call ON-EXIT, as `opencode-service--stop'."
  (setf (opencode-sv opencode-service-connection) nil)
  ;; A lookup in flight may have found the service being stopped.
  (cl-incf (opencode-sv opencode-service--lookup))
  (cl-flet ((resume ()
              (when (and (opencode-sv opencode-service--waiters)
                         (not (process-live-p opencode-service--starter)))
                (opencode-service--find))))
    (condition-case err
        (opencode-service--run-cli
         '("service" "stop")
         (lambda (status output)
           (unwind-protect (funcall on-exit status output)
             (resume))))
      (error (resume)
             (signal (car err) (cdr err))))))

(provide 'opencode-service)
;;; opencode-service.el ends here
