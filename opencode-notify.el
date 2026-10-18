;;; opencode-notify.el --- Notifications of session events  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann
;; Copyright (C) 2026  Valentin Leon

;; Author: Scott Zimmermann <sczi@disroot.org>
;;         Valentin Leon <valentin@leon.click>
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

;; A session that is not displayed reports what needs the user: its turn
;; ended, or it waits on a permission or a question.  Retries are
;; reported too.  Every notification is logged to the hidden buffer
;; ` *OpenCode Notifications*'; two filters decide whether it is also
;; echoed in *Messages* and shown by the operating system.  By default a
;; finished turn and a retry stay out of *Messages*, and a retry also
;; stays out of OS notifications.
;;
;; The OS backend tries D-Bus first, falling through when no session bus
;; answers: an Emacs built with D-Bus support may run where no bus does,
;; as on macOS.  Then come terminal-notifier or osascript on macOS and
;; tray notifications on Windows.  `opencode-notify-os-alert' hands them
;; to the alert package instead.

;;; Code:

(require 'opencode-common)

(defvar opencode-session-info)

(defcustom opencode-notification-messages-filter #'opencode-notification-messages-default-p
  "Which notifications are echoed in *Messages*.
nil suppresses all, t allows all, and a function is called with the
notification alist and returns non-nil to allow it."
  :type '(choice (const :tag "Suppress all" nil)
                 (const :tag "Allow all" t)
                 (function :tag "Predicate"))
  :group 'opencode)

(defcustom opencode-notification-os-filter #'opencode-notification-os-default-p
  "Which notifications are shown by the operating system.
Takes the same values as `opencode-notification-messages-filter'."
  :type '(choice (const :tag "Suppress all" nil)
                 (const :tag "Allow all" t)
                 (function :tag "Predicate"))
  :group 'opencode)

(defcustom opencode-notify-os-function #'opencode-notify-os-native
  "Function showing a notification alist outside Emacs."
  :type '(choice (function-item opencode-notify-os-native)
                 (function-item opencode-notify-os-alert)
                 function)
  :group 'opencode)

(defconst opencode-notifications-buffer " *OpenCode Notifications*"
  "Name of the hidden buffer logging every notification.")

(defun opencode-notification-messages-default-p (notification)
  "Return non-nil unless NOTIFICATION reports a finished turn or a retry."
  (not (memq (alist-get 'kind notification) '(finished retry))))

(defun opencode-notification-os-default-p (notification)
  "Return non-nil unless NOTIFICATION reports a retry."
  (not (eq (alist-get 'kind notification) 'retry)))

(defun opencode--notification-allowed-p (filter notification)
  "Return non-nil when FILTER allows NOTIFICATION."
  (if (functionp filter) (funcall filter notification) filter))

(defun opencode--log-notification (notification)
  "Append NOTIFICATION to the hidden notifications buffer."
  (let-alist notification
    (with-current-buffer (get-buffer-create opencode-notifications-buffer)
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "[%Y-%m-%d %H:%M:%S] ")
                (format "[%s] %s: %s\n" .variant .title .message))))))

(defun opencode-notify (notification)
  "Log NOTIFICATION and show it where the filters allow.
NOTIFICATION is an alist: `title', `message', `variant' (info, success,
warning or error), `kind' (a symbol such as `finished', `failed',
`interrupted', `retry', `permission' or `question') and `session' (its
ID)."
  (opencode--log-notification notification)
  (let-alist notification
    (when (opencode--notification-allowed-p opencode-notification-messages-filter notification)
      (message "%s: %s" .title .message))
    (when (opencode--notification-allowed-p opencode-notification-os-filter notification)
      (condition-case err
          (funcall opencode-notify-os-function notification)
        (error (message "OpenCode: cannot show a notification: %s" (error-message-string err)))))))

;;;; Session notifications

(defun opencode-alert-session ()
  "Remember this session as needing a visit, newest first."
  (setq opencode-alerted-sessions
        (cons opencode-session-info
              (seq-remove (lambda (info) (equal (alist-get 'id info) opencode-session-id))
                          opencode-alerted-sessions))))

(defun opencode-notify-session (title text kind)
  "Report TEXT under TITLE for this session's request of KIND.
Shown in the echo area when this buffer is displayed, else notified."
  (if (opencode-buffer-active-p (current-buffer))
      (message "%s: %s" title text)
    (opencode-notify `((title . ,title) (message . ,text) (variant . "info")
                       (kind . ,kind) (session . ,opencode-session-id)))))

(defun opencode-notify-turn-ended (outcome)
  "Notify that this session's turn ended with OUTCOME.
Nothing is reported while the buffer is displayed, nor for subagents,
whose parent reports for them."
  (unless (or (opencode-buffer-active-p (current-buffer))
              (alist-get 'parentID opencode-session-info))
    (opencode-notify
     `((title . ,(pcase outcome
                   ('succeeded "OpenCode finished")
                   ('failed "OpenCode failed")
                   (_ "OpenCode interrupted")))
       (message . ,(or (alist-get 'title opencode-session-info) opencode-session-id))
       (variant . ,(pcase outcome ('succeeded "success") ('failed "error") (_ "warning")))
       (kind . ,(pcase outcome ('succeeded 'finished) ('failed 'failed) (_ 'interrupted)))
       (session . ,opencode-session-id)))
    (opencode-alert-session)))

(defun opencode-notify-retry (data)
  "Notify the retry `session.retry.scheduled' DATA announces."
  (opencode-notify
   `((title . ,(concat "OpenCode: " (or (alist-get 'title opencode-session-info) opencode-session-id)))
     (message . ,(format "%s\n\nRetry #%d"
                         (or (alist-get 'message (alist-get 'error data)) "Request failed")
                         (or (alist-get 'attempt data) 1)))
     (variant . "warning")
     (kind . retry)
     (session . ,opencode-session-id))))

;;;; Operating system backends

(declare-function notifications-notify "notifications")
(declare-function w32-notification-notify "w32fns.c")
(declare-function w32-notification-close "w32fns.c")
(declare-function alert "alert")

(defun opencode-notify-os-native (notification)
  "Show NOTIFICATION with the operating system's notifications."
  (cond
   ((opencode--notify-dbus notification))
   ((eq system-type 'darwin) (opencode--notify-macos notification))
   ((fboundp 'w32-notification-notify) (opencode--notify-w32 notification))))

(defun opencode--notify-dbus (notification)
  "Show NOTIFICATION over D-Bus; return nil when no session bus answers."
  (when (and (featurep 'dbusbind) (require 'notifications nil t))
    (let-alist notification
      (condition-case nil
          (progn
            (notifications-notify :title .title
                                  :body .message
                                  :urgency (if (member .variant '("error" "warning")) 'critical 'normal)
                                  :timeout 5000
                                  :app-icon 'none)
            t)
        (dbus-error nil)))))

(defun opencode--notify-macos (notification)
  "Show NOTIFICATION with terminal-notifier, else osascript."
  (let-alist notification
    (let ((title (concat (pcase .variant ("error" "❌") ("warning" "⚠️") (_ "ℹ️")) " " .title)))
      (if (executable-find "terminal-notifier")
          (start-process "opencode-notification" nil "terminal-notifier"
                         "-title" title
                         "-message" .message
                         "-group" "opencode-toast"
                         "-activate" "org.gnu.Emacs"
                         "-sound" (if (member .variant '("error" "warning")) "Basso" "default"))
        (start-process "opencode-notification" nil "osascript" "-e"
                       (format "display notification %S with title %S" .message title))))))

(defvar opencode--w32-notification nil
  "(ID . FRAME) of the tray notification shown, or nil.")

(defun opencode--w32-notification-close (&optional id)
  "Remove the tray notification shown, only when it is ID if given."
  (pcase opencode--w32-notification
    (`(,shown . ,frame)
     (when (or (null id) (eql id shown))
       (setq opencode--w32-notification nil)
       ;; It can only be removed from the frame that showed it.
       (when (frame-live-p frame)
         (with-selected-frame frame
           (w32-notification-close shown)))))))

(defun opencode--notify-w32 (notification)
  "Show NOTIFICATION as a tray notification for ten seconds.
A frame shows one at a time, so it replaces the previous one."
  (let-alist notification
    (opencode--w32-notification-close)
    (when-let* ((id (w32-notification-notify
                     :title .title
                     :body .message
                     :level (pcase .variant ("error" 'error) ("warning" 'warning) (_ 'info))
                     :tip "OpenCode")))
      (setq opencode--w32-notification (cons id (selected-frame)))
      (run-at-time 10 nil #'opencode--w32-notification-close id))))

(defun opencode-notify-os-alert (notification)
  "Show NOTIFICATION with the alert package."
  (require 'alert)
  (let-alist notification
    (alert .message
           :title .title
           :severity (pcase .variant ("error" 'high) ("warning" 'moderate) (_ 'normal))
           :category 'opencode)))

(provide 'opencode-notify)
;;; opencode-notify.el ends here
