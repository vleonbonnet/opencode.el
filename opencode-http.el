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
;; registration and retries once; so does a refused connection, after
;; ensuring a service runs again.
;;
;; Decoded JSON uses alists with symbol keys, lists for arrays, and nil
;; for both null and false.  Request bodies are encoded by
;; `json-serialize', so arrays must be vectors and false must be `:false'.

;;; Code:

(require 'opencode-common)
(require 'opencode-service)
(require 'plz)
(require 'url-util)

(defcustom opencode-request-timeout 60
  "Default seconds allowed for one request."
  :type 'number
  :group 'opencode)

(defvar opencode-request-log nil
  "When non-nil, log every request and response in *opencode-requests*.")

(cl-defstruct (opencode-error (:constructor opencode-error--make))
  "A failed OpenCode request."
  status tag message method path)

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
error would only print \"error in process sentinel\"."
  (if command-error-function
      (funcall command-error-function (list 'user-error text) nil nil)
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
        (url-hexify-string (directory-file-name (opencode-normalize-directory directory)))))

(defun opencode-request--parse ()
  "Parse the response body in the current buffer, nil when empty."
  (unless (= (point) (point-max))
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defun opencode-request--error (method path plz-error)
  "Return an `opencode-error' for METHOD on PATH from PLZ-ERROR."
  (let* ((response (plz-error-response plz-error))
         (status (and response (plz-response-status response)))
         (body (and response (plz-response-body response)))
         (json (and (stringp body) (not (string-empty-p body))
                    (ignore-errors
                      (json-parse-string body :object-type 'alist :array-type 'list
                                         :null-object nil :false-object nil)))))
    (opencode-error--make
     :status status :method method :path path
     :tag (and (consp json) (alist-get '_tag json))
     :message (or (and (consp json) (alist-get 'message json))
                  (and (stringp body) (not (string-empty-p body)) (string-trim body))
                  (plz-error-message plz-error)
                  (when-let* ((curl (plz-error-curl-error plz-error)))
                    (format "curl error %s: %s" (car curl) (cdr curl)))))))

(defun opencode-request--retryable (plz-error connection)
  "Return a retry reason for PLZ-ERROR seen on CONNECTION, or nil.
`password' when the registration changed after a 401, `refused' when the
connection was refused."
  (let ((response (plz-error-response plz-error))
        (curl (plz-error-curl-error plz-error)))
    (cond
     ((and response (eql (plz-response-status response) 401))
      (let ((registered (opencode-service-read-registration)))
        (and registered
             (not (equal (plist-get registered :password)
                         (plist-get connection :password)))
             'password)))
     ((and curl (memql (car curl) '(7 52 56)))
      'refused))))

(cl-defun opencode-request (method path &key query body (directory default-directory)
                                   (then #'ignore) else timeout (buffer (current-buffer))
                                   retried)
  "Send METHOD on PATH and call THEN with the decoded response.
QUERY is an alist of query parameters; BODY is encoded as JSON.
DIRECTORY scopes location routes; nil sends no directory.  THEN and
ELSE run in BUFFER when it is live, and are skipped once it is killed;
BUFFER nil runs them in whatever buffer is current.  ELSE receives an
`opencode-error'; without ELSE, failures are reported with
`opencode-report-error'.  TIMEOUT defaults to `opencode-request-timeout'.
RETRIED is internal."
  (let ((origin buffer))
    (cl-flet ((deliver (function value)
                (if (null origin)
                    (funcall function value)
                  (when (buffer-live-p origin)
                    (with-current-buffer origin
                      (funcall function value))))))
      (if (not opencode-service-connection)
          (opencode-service-ensure
           (lambda (_connection)
             (opencode-request method path :query query :body body :directory directory
                               :then then :else else :timeout timeout :buffer buffer
                               :retried retried))
           (lambda (message)
             (let ((error (opencode-error--make :method method :path path :message message)))
               (if else (deliver else error) (opencode-report-error (opencode-error-describe error))))))
        (let* ((connection opencode-service-connection)
               (url (opencode-request--url connection path query))
               (payload (and body (json-serialize body :null-object :null :false-object :false))))
          (opencode-request--log "%s %s%s" method url (if payload (concat " " payload) ""))
          (plz method url
            :headers (delq nil
                           (list (cons "Authorization" (opencode-service-authorization connection))
                                 (cons "Accept" "application/json")
                                 (and payload (cons "Content-Type" "application/json"))
                                 (and directory (opencode-request-directory-header directory))))
            :body (and payload (encode-coding-string payload 'utf-8))
            :body-type 'binary
            :as #'opencode-request--parse
            :timeout (or timeout opencode-request-timeout)
            :noquery t
            :then (lambda (result)
                    (opencode-request--log "OK %s %s" method path)
                    (deliver then result))
            :else (lambda (plz-error)
                    (let ((reason (and (not retried)
                                       (opencode-request--retryable plz-error connection))))
                      (if reason
                          (progn
                            (setq opencode-service-connection nil)
                            (opencode-request method path :query query :body body
                                              :directory directory :then then :else else
                                              :timeout timeout :buffer buffer :retried t))
                        (let ((error (opencode-request--error method path plz-error)))
                          (opencode-request--log "ERROR %s" (opencode-error-describe error))
                          (if else
                              (deliver else error)
                            (opencode-report-error (opencode-error-describe error)))))))))))))

(defun opencode-request-sync (method path &rest arguments)
  "Send METHOD on PATH synchronously and return the decoded response.
ARGUMENTS are `opencode-request' keywords except THEN, ELSE and BUFFER.
Signal `user-error' on failure.  Use only for requests a command must
wait for, with a short :timeout."
  (let ((result :pending)
        (failure nil)
        (deadline (+ (float-time) (or (plist-get arguments :timeout) opencode-request-timeout) 5)))
    (apply #'opencode-request method path
           :then (lambda (value) (setq result value))
           :else (lambda (error) (setq failure error result nil))
           :buffer nil
           arguments)
    (while (and (eq result :pending) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (cond
     (failure (user-error "%s" (opencode-error-describe failure)))
     ((eq result :pending) (user-error "OpenCode request %s timed out" path))
     (t result))))

(defun opencode-request-data (response)
  "Return the `data' member of RESPONSE."
  (alist-get 'data response))

(provide 'opencode-http)
;;; opencode-http.el ends here
