;;; opencode-desktop.el --- Saving session buffers with the desktop  -*- lexical-binding: t; -*-

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

;; The desktop saves each session buffer's session ID, directory and
;; point; the server keeps its agent, model and variant.  Restoring
;; returns a placeholder buffer at once, under the saved name, and fills
;; it in place once the service answers, without displaying anything or
;; sending a prompt.  A placeholder is itself saved with the desktop, so
;; a restore that failed, because the service was unavailable or the
;; session was deleted, keeps the session in the next desktop; `g' in it
;; tries again.  Late answers to an abandoned attempt are ignored.

;;; Code:

(require 'opencode-common)
(require 'opencode-events)
(require 'opencode-http)
(require 'opencode-service)
(require 'opencode-session)

(defvar desktop-buffer-mode-handlers)
(defvar desktop-save-buffer)

(defcustom opencode-desktop-restore-timeout 60
  "Seconds a session buffer may take to restore before it reports failure.
They count from the moment the service is ready."
  :type 'natnum
  :group 'opencode)

(defvar-local opencode--desktop-data nil
  "Saved desktop entry of this buffer while it is being restored.")

(defvar-local opencode--desktop-attempt nil
  "Identity of this buffer's restore attempt in progress, or nil.")

(defvar-local opencode--desktop-timer nil
  "Deadline timer of this buffer's restore attempt.")

;; They survive the change from the placeholder's mode to the session's.
(dolist (variable '(opencode--desktop-data opencode--desktop-attempt opencode--desktop-timer))
  (put variable 'permanent-local t))

(defun opencode-desktop-save (_directory)
  "Return the desktop entry of this session buffer."
  (or opencode--desktop-data
      (list :version 1 :session-id opencode-session-id
            :directory default-directory :point (point))))

(defun opencode--desktop-track ()
  "Save this buffer with the desktop."
  (setq-local desktop-save-buffer #'opencode-desktop-save)
  (add-hook 'kill-buffer-hook #'opencode--desktop-stop nil t))

(add-hook 'opencode-session-mode-hook #'opencode--desktop-track)

(defvar-keymap opencode-desktop-pending-mode-map
  :parent special-mode-map
  "g" #'opencode-desktop-retry)

(define-derived-mode opencode-desktop-pending-mode special-mode "OpenCode restore"
  "A saved session buffer waiting to be restored; \\[opencode-desktop-retry] retries."
  (opencode--desktop-track))

(defun opencode--desktop-notice (text)
  "Show TEXT in this placeholder."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert text "\n\n" (plist-get opencode--desktop-data :directory)
            (substitute-command-keys "\nPress \\<opencode-desktop-pending-mode-map>\\[opencode-desktop-retry] to retry.\n"))
    (goto-char (point-min))))

(defun opencode--desktop-stop ()
  "Abandon this buffer's restore attempt, so its late answers do nothing."
  (when (timerp opencode--desktop-timer)
    (cancel-timer opencode--desktop-timer))
  (setq opencode--desktop-timer nil
        opencode--desktop-attempt nil))

(defun opencode--desktop-fail (text)
  "Turn this buffer back into a placeholder reporting TEXT."
  (opencode--desktop-stop)
  (when (derived-mode-p 'opencode-session-mode)
    (opencode--session-cleanup))
  (unless (derived-mode-p 'opencode-desktop-pending-mode)
    (opencode-desktop-pending-mode))
  (opencode--desktop-notice text)
  (message "Desktop: %s: %s" (buffer-name) text))

(defmacro opencode--desktop-current (attempt arguments &rest body)
  "Return a function of ARGUMENTS running BODY in this buffer.
BODY runs only while ATTEMPT is the buffer's restore attempt in progress."
  (declare (indent 2))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (current-buffer)))
       (lambda ,arguments
         (when (buffer-live-p ,buffer)
           (with-current-buffer ,buffer
             (when (and ,attempt (eq ,attempt opencode--desktop-attempt))
               ,@body)))))))

(defun opencode-desktop-retry ()
  "Restore this saved session buffer in place, without displaying it."
  (interactive)
  (unless opencode--desktop-data (user-error "No saved session in this buffer"))
  (opencode--desktop-stop)
  (let ((attempt (setq opencode--desktop-attempt (make-symbol "attempt"))))
    (opencode--desktop-notice "Waiting for the OpenCode service…")
    (opencode-events-connect)
    ;; The service's start and import have deadlines of their own.
    (opencode-service-ensure
     (opencode--desktop-current attempt (_connection)
       (setq opencode--desktop-timer
             (run-at-time opencode-desktop-restore-timeout nil
                          (opencode--desktop-current attempt ()
                            (opencode--desktop-fail "Timed out restoring the session"))))
       (opencode--desktop-notice "Restoring the OpenCode session…")
       (opencode--desktop-fetch attempt))
     (opencode--desktop-current attempt (text) (opencode--desktop-fail text)))))

(defun opencode--desktop-fetch (attempt)
  "Fetch this buffer's saved session for ATTEMPT and fill the buffer with it."
  (let ((id (plist-get opencode--desktop-data :session-id))
        (directory (plist-get opencode--desktop-data :directory)))
    (if (not (file-directory-p directory))
        (opencode--desktop-fail "The session's directory is unavailable")
      (opencode-request
       'get (concat "/api/session/" id)
       :directory directory
       :else (opencode--desktop-current attempt (error)
               (opencode--desktop-fail
                (if (eql (opencode-error-status error) 404)
                    "This session no longer exists"
                  (opencode-error-describe error))))
       :then (opencode--desktop-current attempt (response)
               (let ((info (opencode-request-data response))
                     (open (gethash id opencode-session-buffers)))
                 (cond
                  ((not (equal (alist-get 'id info) id))
                   (opencode--desktop-fail "The service returned another session"))
                  ((and (buffer-live-p open) (not (eq open (current-buffer))))
                   (opencode--desktop-superseded open))
                  (t
                   (opencode-session-open
                    info :buffer (current-buffer) :no-display t
                    :callback (opencode--desktop-current attempt (_info) (opencode--desktop-ready))
                    :else (opencode--desktop-current attempt (error)
                            (opencode--desktop-fail (opencode-error-describe error))))))))))))

(defun opencode--desktop-superseded (buffer)
  "Give up restoring this placeholder: its session is open in BUFFER meanwhile.
The placeholder is killed unless displayed, and is never saved again."
  (opencode--desktop-stop)
  (setq opencode--desktop-data nil)
  (setq-local desktop-save-buffer nil)
  (if (get-buffer-window (current-buffer) t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "This session is open in %s.\n" (buffer-name buffer))))
    (kill-buffer (current-buffer))))

(defun opencode--desktop-ready ()
  "Finish restoring this buffer: return to the saved point."
  (let ((point (plist-get opencode--desktop-data :point)))
    (opencode--desktop-stop)
    (setq opencode--desktop-data nil)
    (when (integerp point)
      (goto-char (min (point-max) (max (point-min) point))))))

(defun opencode-desktop-restore (_file name data)
  "Return the buffer NAME of desktop entry DATA, restoring it in the background.
A buffer of the session already open is returned instead."
  ;; Restored buffers need every module, not only this one.
  (require 'opencode)
  (let ((id (plist-get data :session-id))
        (directory (plist-get data :directory)))
    (unless (and (eql (plist-get data :version) 1)
                 (stringp id) (string-match-p "\\`ses_[[:alnum:]_-]+\\'" id)
                 (stringp directory) (not (file-remote-p directory)))
      (error "Invalid OpenCode desktop entry"))
    (or (let ((buffer (gethash id opencode-session-buffers)))
          (and (buffer-live-p buffer) buffer))
        (seq-find (lambda (buffer)
                    (equal id (plist-get (buffer-local-value 'opencode--desktop-data buffer) :session-id)))
                  (buffer-list))
        (let ((buffer (generate-new-buffer name)))
          (with-current-buffer buffer
            (opencode-desktop-pending-mode)
            (setq default-directory (file-name-as-directory directory)
                  opencode--desktop-data (copy-tree data))
            (opencode-desktop-retry))
          buffer))))

(provide 'opencode-desktop)
;;; opencode-desktop.el ends here
