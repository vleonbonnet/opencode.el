;;; opencode-http.el --- Requests to the OpenCode service  -*- lexical-binding: t; -*-

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

;; `opencode-request' is the one way this package talks HTTP to the
;; service.  It authenticates, scopes location routes to a directory
;; through the `x-opencode-directory' header, encodes and decodes JSON
;; with the native parser, and runs its callbacks in the buffer that
;; issued the request.  A 401 after a service restart rereads the
;; registration and retries once; so does a refused connection, or one
;; closed without an answer, after ensuring a service runs again.
;; Requests travel over `opencode-transport-request'.
;;
;; Decoded JSON uses alists with symbol keys, lists for arrays, and nil
;; for both null and false.  Request bodies are encoded by
;; `json-serialize', so arrays must be vectors and false must be `:false'.

;;; Code:

(require 'opencode-common)
(require 'opencode-service)
(require 'opencode-transport)
(require 'url-util)

(defcustom opencode-request-timeout 60
  "Default seconds allowed for one request."
  :type 'number
  :group 'opencode)

(defcustom opencode-request-log nil
  "When non-nil, log every request and response in *opencode-requests*."
  :type 'boolean
  :group 'opencode)

(cl-defstruct (opencode-error (:constructor opencode-error--make))
  "A failed OpenCode request.
DATA is the error object the server answered, decoded, or nil: its
fields beside the tag and the message, such as a provider error's
reason, tell callers more of the failure."
  status tag message method path data)

(defun opencode-error-describe (error)
  "Return a one-line description of request ERROR."
  (if (opencode-error-p error)
      (format "%s %s failed%s: %s"
              (upcase (symbol-name (opencode-error-method error)))
              (opencode-error-path error)
              (if-let* ((status (opencode-error-status error)))
                  (format " (HTTP %s)" status) "")
              (or (opencode-error-message error) "unknown error"))
    (format "%s" error)))

(defun opencode-report-error (text)
  "Report TEXT like a `user-error' raised by a command, outside the command loop.
Asynchronous callbacks run from process sentinels, where a signaled
error would only print \"error in process sentinel\".  Without a frame
to show it in, in batch or on a daemon's initial frame, the default
`command-error-function' would print TEXT and exit Emacs, so TEXT then
goes to `message'."
  (if (and command-error-function
           (not noninteractive)
           (not (frame-initial-p (selected-frame))))
      ;; The context argument must be a string.
      (funcall command-error-function (list 'user-error text) "" nil)
    (message "%s" text)))

(defun opencode-request--log (format-string &rest arguments)
  "Append FORMAT-STRING with ARGUMENTS to the request log when enabled."
  (when opencode-request-log
    (with-current-buffer (get-buffer-create "*opencode-requests*")
      (save-excursion
        (goto-char (point-max))
        (insert (format-time-string "%T ") (apply #'format format-string arguments) "\n")))))

(defun opencode-request--url (connection path query)
  "Return CONNECTION's URL for PATH with QUERY, an alist of KEY and VALUE."
  (concat (plist-get connection :url) path
          (when query
            (concat (if (string-search "?" path) "&" "?")
                    (mapconcat (lambda (pair)
                                 (concat (url-hexify-string (format "%s" (car pair)))
                                         "="
                                         (url-hexify-string (format "%s" (cdr pair)))))
                               (seq-filter #'cdr query)
                               "&")))))

(defun opencode-request-directory-header (directory)
  "Return the directory header for DIRECTORY, URI-encoded as the server expects."
  (cons "x-opencode-directory"
        (url-hexify-string (opencode-directory-name directory))))

(defun opencode-request--parse ()
  "Parse the response body in the current buffer, nil when empty."
  (unless (= (point) (point-max))
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defun opencode-request--error (method path failure)
  "Return an `opencode-error' for METHOD on PATH from transport FAILURE."
  (let* ((body (opencode-transport-failure-body failure))
         (json (and (stringp body) (not (string-empty-p body))
                    (ignore-errors
                      (json-parse-string body :object-type 'alist :array-type 'list
                                         :null-object nil :false-object nil)))))
    (opencode-error--make
     :status (opencode-transport-failure-status failure) :method method :path path
     :tag (and (consp json) (alist-get '_tag json))
     :data (and (consp json) (consp (car json)) json)
     :message (or (and (consp json) (alist-get 'message json))
                  (and (stringp body) (not (string-empty-p body)) (string-trim body))
                  (opencode-transport-failure-message failure)))))

(defun opencode-request--retryable (failure connection)
  "Return a retry reason for transport FAILURE seen on CONNECTION, or nil.
`password' when the registration changed after a 401, `refused' when no
connection could be made or it closed without an answer.  A remote
service's provider is asked again after a 401: it may know a new
password."
  (pcase (opencode-transport-failure-reason failure)
    ('http
     (when (eql (opencode-transport-failure-status failure) 401)
       (let ((registered (if (opencode--remote-service)
                             '(:password :ask)
                           (opencode-service-read-registration))))
         (and registered
              (not (equal (plist-get registered :password)
                          (plist-get connection :password)))
              'password))))
    ((or 'refused 'closed) 'refused)))

(cl-defun opencode-request (method path &key query body (directory (opencode-location))
                                   (then #'ignore) else timeout (buffer (current-buffer))
                                   retried)
  "Send METHOD on PATH and call THEN with the decoded response.
The request goes to the current service (see `opencode-current-service'),
and THEN and ELSE run for it.  QUERY is an alist of query parameters;
BODY is encoded as JSON.  DIRECTORY scopes location routes, by default
the current buffer's location (see `opencode-location'); nil sends no
directory.  THEN and ELSE run in BUFFER when it is live, and are skipped
once it is killed; BUFFER nil runs them in whatever buffer is current.
ELSE receives an `opencode-error'; without ELSE, failures are reported
with `opencode-report-error'.  TIMEOUT defaults to
`opencode-request-timeout'.  RETRIED is internal."
  (let ((origin buffer)
        (service (opencode--remote-service)))
    (cl-flet ((deliver (function value)
                (opencode-with-service service
                  (if (null origin)
                      (funcall function value)
                    (when (buffer-live-p origin)
                      (with-current-buffer origin
                        (funcall function value)))))))
      (if (not (opencode-sv opencode-service-connection))
          (opencode-service-ensure
           (lambda (_connection)
             (opencode-with-service service
               (opencode-request method path :query query :body body :directory directory
                                 :then then :else else :timeout timeout :buffer buffer
                                 :retried retried)))
           (lambda (message)
             (let ((error (opencode-error--make :method method :path path :message message)))
               (if else (deliver else error) (opencode-report-error (opencode-error-describe error))))))
        (let* ((connection (opencode-sv opencode-service-connection))
               (url (opencode-request--url connection path query))
               (payload (and body (json-serialize body :null-object :null :false-object :false))))
          (opencode-request--log "%s %s%s" method url (if payload (concat " " payload) ""))
          (opencode-transport-request
           method url
           :headers (delq nil
                          (list (cons "Authorization" (opencode-service-authorization connection))
                                (cons "Accept" "application/json")
                                (and payload (cons "Content-Type" "application/json"))
                                (and directory (opencode-request-directory-header directory))))
           :body (and payload (encode-coding-string payload 'utf-8))
           :as #'opencode-request--parse
           :timeout (or timeout opencode-request-timeout)
           ;; A kept connection already had its chance to fail.
           :fresh retried
           :then (lambda (result)
                   (opencode-request--log "OK %s %s" method path)
                   (deliver then result))
           :else (lambda (failure)
                   (opencode-with-service service
                     (let ((reason (and (not retried)
                                        (opencode-request--retryable failure connection))))
                       (if reason
                           (progn
                             (setf (opencode-sv opencode-service-connection) nil)
                             (opencode-request method path :query query :body body
                                               :directory directory :then then :else else
                                               :timeout timeout :buffer buffer :retried t))
                         (let ((error (opencode-request--error method path failure)))
                           (opencode-request--log "ERROR %s" (opencode-error-describe error))
                           (if else
                               (deliver else error)
                             (opencode-report-error (opencode-error-describe error))))))))))))))

(defun opencode-request-sync (method path &rest arguments)
  "Send METHOD on PATH synchronously and return the decoded response.
ARGUMENTS are `opencode-request' keywords except THEN and BUFFER.  On
failure, return what ELSE returns for the `opencode-error', or signal
`user-error' without ELSE.  Use only for requests a command must wait
for, with a short :timeout."
  (let* ((else (plist-get arguments :else))
         (arguments (cl-loop for (key value) on arguments by #'cddr
                             unless (eq key :else) append (list key value)))
         (result :pending)
         (failure nil)
         (timeout (or (plist-get arguments :timeout) opencode-request-timeout))
         (deadline (+ (float-time) timeout 5)))
    (apply #'opencode-request method path
           :then (lambda (value) (setq result value))
           :else (lambda (error) (setq failure error result nil))
           :buffer nil
           arguments)
    (while (and (eq result :pending) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (when (and (eq result :pending) (not failure))
      (setq failure (opencode-error--make :method method :path path
                                          :message (format "no answer after %ss" timeout))))
    (cond
     ((and failure else) (funcall else failure))
     (failure (user-error "%s" (opencode-error-describe failure)))
     (t result))))

(defun opencode-wait (fetch)
  "Return what FETCH delivers, waiting for it.
FETCH is called with two functions: one taking the result, the other
the `opencode-error' of a failure.  Commands that prompt with fetched
data wait here, so their questions come from the command and never from
a network callback.  The wait lasts at most `opencode-request-timeout'
seconds, which \\[keyboard-quit] cuts short; a failure signals a
`user-error'."
  (let ((result 'opencode--pending)
        (failure nil)
        (deadline (+ (float-time) opencode-request-timeout)))
    (funcall fetch (lambda (value) (setq result value)) (lambda (error) (setq failure error)))
    (while (and (eq result 'opencode--pending) (not failure) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (cond
     (failure (user-error "%s" (opencode-error-describe failure)))
     ((eq result 'opencode--pending)
      (user-error "OpenCode did not answer within %s seconds" opencode-request-timeout))
     (t result))))

(defun opencode-request-data (response)
  "Return the `data' member of RESPONSE."
  (alist-get 'data response))

(provide 'opencode-http)
;;; opencode-http.el ends here
