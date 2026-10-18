;;; opencode-requests.el --- Requests waiting on the user  -*- lexical-binding: t; -*-

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

;; Permission requests and questions never take focus.  Arrival marks the
;; tool block that asked, in the attention face and with a question mark,
;; and \\[opencode-respond] answers the request at point, a suspended one,
;; or the oldest one.  Inside either transient, C-v, M-v and the mouse
;; wheel scroll the transcript, C-z suspends it so normal navigation
;; works, and C-g defers it; these keys are hidden from the popup.  Each
;; request keeps its own progress, so deferring or suspending one never
;; blocks another.
;;
;; A subagent asks in its own session, whose buffer opens in the
;; background, so its parent looks busy while it actually waits.  The
;; parent's block of a waiting subagent is marked the same way and its
;; mode line shows the pending state, through every level of nesting;
;; \\[opencode-respond] there switches to the child and answers it.

;;; Code:

(require 'opencode-blocks)
(require 'opencode-common)
(require 'opencode-notify)
(require 'opencode-session)
(require 'transient)

(defvar opencode-request-providers nil
  "Functions describing the requests a session buffer waits on.
Each returns, in the current buffer, nil or a plist with :pending (non-nil
when a request awaits an answer), :at-point (a function answering the
request at point, or nil) and :respond (a function answering the oldest
request), tried in order.")

(defvar-local opencode--pending-children nil
  "Child session IDs, oldest first, whose requests await the user.")

(defun opencode-transient-displayed-p (prefix)
  "Return non-nil when transient PREFIX is on screen."
  (and (bound-and-true-p transient--prefix)
       (eq (oref transient--prefix command) prefix)))

(defun opencode-transient-suspended-p (prefix)
  "Return non-nil when transient PREFIX tops the stack of suspended transients."
  (eq (caar (bound-and-true-p transient--stack)) prefix))

(defun opencode-install-hidden-navigation (prefix suffixes)
  "Keep the navigation SUFFIXES of transient PREFIX active but unlisted."
  (unless (ignore-errors (transient-get-suffix prefix (caar suffixes)))
    (transient-append-suffix prefix '(-1)
      `[:hide (lambda () t) ,@suffixes])))

(defun opencode--scroll-transient-session (command &optional arg)
  "Run scroll COMMAND with ARG in the window the transient was called from."
  (when (window-live-p transient--original-window)
    (with-selected-window transient--original-window
      (funcall command arg))))

(transient-define-suffix opencode-transient-scroll-down ()
  "Scroll the transcript down."
  :transient t
  (interactive)
  (opencode--scroll-transient-session #'scroll-up-command))

(transient-define-suffix opencode-transient-scroll-up ()
  "Scroll the transcript up."
  :transient t
  (interactive)
  (opencode--scroll-transient-session #'scroll-down-command))

(transient-define-suffix opencode-transient-wheel-down ()
  "Scroll the transcript down by a mouse-wheel step."
  :transient t
  (interactive)
  (opencode--scroll-transient-session #'scroll-up-command 3))

(transient-define-suffix opencode-transient-wheel-up ()
  "Scroll the transcript up by a mouse-wheel step."
  :transient t
  (interactive)
  (opencode--scroll-transient-session #'scroll-down-command 3))

(defconst opencode-transient-scroll-suffixes
  '(("C-v" "Scroll transcript down" opencode-transient-scroll-down)
    ("M-v" "Scroll transcript up" opencode-transient-scroll-up)
    ("<wheel-down>" "Scroll transcript down" opencode-transient-wheel-down)
    ("<wheel-up>" "Scroll transcript up" opencode-transient-wheel-up))
  "Transcript scrolling keys of the request transients.")

(defun opencode-pending-request-p ()
  "Return non-nil when this session buffer awaits an answer from the user."
  (or opencode--pending-children
      (seq-some (lambda (provider) (plist-get (funcall provider) :pending))
                opencode-request-providers)))

(defun opencode-respond ()
  "Answer the request at point, a suspended one, or the oldest pending one.
On the block of a waiting subagent, or with only subagents waiting,
switch to the subagent's session and answer there."
  (interactive)
  (or (seq-some (lambda (provider)
                  (when-let* ((at-point (plist-get (funcall provider) :at-point)))
                    (funcall at-point)
                    t))
                opencode-request-providers)
      (when-let* ((child (opencode--child-at-point)))
        (opencode--respond-child child)
        t)
      (seq-some (lambda (provider)
                  (let ((state (funcall provider)))
                    (when (plist-get state :pending)
                      (funcall (plist-get state :respond))
                      t)))
                opencode-request-providers)
      (when opencode--pending-children
        (opencode--respond-child (car opencode--pending-children))
        t)
      (user-error "No pending OpenCode request")))

;;;; Subagent requests in their parents

(defvar opencode--sync-sessions nil
  "Session IDs whose pending state must reach their parents.")

(defvar opencode--sync-timer nil
  "Timer flushing `opencode--sync-sessions'.")

(defun opencode-request-state-changed (&optional session-id)
  "Propagate the pending state of SESSION-ID, this buffer's by default, to its parents.
Changes are gathered into one timer; each sync recomputes the state."
  (when-let* ((session-id (or session-id opencode-session-id)))
    (cl-pushnew session-id opencode--sync-sessions :test #'equal)
    (unless (timerp opencode--sync-timer)
      (setq opencode--sync-timer (run-at-time 0 nil #'opencode--flush-sync)))))

(defun opencode--flush-sync ()
  "Propagate every changed session's pending state to its parents."
  (setq opencode--sync-timer nil)
  (let ((sessions (reverse opencode--sync-sessions)))
    (setq opencode--sync-sessions nil)
    (dolist (session-id sessions)
      (with-demoted-errors "OpenCode subagent request sync: %S"
        (opencode--sync-parents session-id)))))

(defun opencode--child-pending-p (session-id)
  "Return non-nil when the open buffer of SESSION-ID awaits an answer."
  (when-let* ((buffer (gethash session-id opencode-session-buffers)))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer (opencode-pending-request-p))
         t)))

(defun opencode--subagent-blocks (session-id)
  "Return this buffer's subagent blocks running child SESSION-ID."
  (seq-filter (lambda (overlay)
                (let ((data (opencode-block-data overlay)))
                  (and (equal (plist-get data :tool) "subagent")
                       (equal (plist-get data :session-id) session-id))))
              (opencode-tool-blocks)))

(defun opencode--set-child-waiting (overlay pending)
  "Mark subagent block OVERLAY as PENDING; a finished subagent never waits.
A request of the subagent call itself keeps precedence."
  (let* ((data (opencode-block-data overlay))
         (kind (plist-get data :waiting-kind))
         (pending (and pending (equal (plist-get data :status) "running"))))
    (cond
     ((and pending (null kind))
      (opencode-block-update overlay :waiting t :waiting-kind 'child))
     ((and (not pending) (eq kind 'child))
      (opencode-block-update overlay :waiting nil :waiting-kind nil)))))

(defun opencode--update-pending-child (session-id)
  "Record whether child SESSION-ID awaits the user in this buffer."
  (let ((pending (seq-some (lambda (overlay) (plist-get (opencode-block-data overlay) :waiting))
                           (opencode--subagent-blocks session-id))))
    (if pending
        (unless (member session-id opencode--pending-children)
          (setq opencode--pending-children (append opencode--pending-children (list session-id))))
      (setq opencode--pending-children (remove session-id opencode--pending-children)))
    (force-mode-line-update)
    (opencode-request-state-changed)))

(defun opencode--sync-parents (session-id)
  "Mark every subagent block running SESSION-ID with its pending state."
  (let ((pending (opencode--child-pending-p session-id)))
    (maphash (lambda (id buffer)
               (when (and (not (equal id session-id)) (buffer-live-p buffer))
                 (with-current-buffer buffer
                   (when-let* ((overlays (opencode--subagent-blocks session-id)))
                     (dolist (overlay overlays)
                       (opencode--set-child-waiting overlay pending))
                     (opencode--update-pending-child session-id)))))
             opencode-session-buffers)))

(defun opencode--attach-child (overlay)
  "Show whether the subagent run by OVERLAY's block awaits the user."
  (let ((data (opencode-block-data overlay)))
    (when-let* (((equal (plist-get data :tool) "subagent"))
                (session-id (plist-get data :session-id)))
      (opencode--set-child-waiting overlay (opencode--child-pending-p session-id))
      (opencode--update-pending-child session-id))))

(defun opencode--child-at-point ()
  "Return the child session ID of the waiting subagent block at point."
  (when-let* ((overlay (opencode-block-at (point)))
              (data (opencode-block-data overlay))
              ((eq (plist-get data :waiting-kind) 'child)))
    (plist-get data :session-id)))

(defun opencode--respond-child (session-id)
  "Show child SESSION-ID in the selected window and answer its oldest request."
  (let ((buffer (gethash session-id opencode-session-buffers)))
    (unless (buffer-live-p buffer)
      (user-error "The subagent session is no longer open"))
    (pop-to-buffer-same-window buffer)
    (opencode-respond)))

(defun opencode--open-subagent (&optional overlay)
  "Open the child session of subagent block OVERLAY, or the block at point."
  (when-let* ((overlay (or overlay (opencode-block-at (point))))
              (data (opencode-block-data overlay))
              ((equal (plist-get data :tool) "subagent")))
    (if-let* ((session-id (plist-get data :session-id)))
        (opencode-session-open-by-id session-id)
      (user-error "The subagent session is not available yet"))
    t))

(defun opencode--forget-child ()
  "Clear this killed session's pending state from its parents."
  (opencode-request-state-changed))

(add-hook 'opencode-blocks-attach-functions #'opencode--attach-child)
(add-hook 'opencode-open-link-functions #'opencode--open-subagent)
(add-hook 'opencode-session-mode-hook
          (lambda () (add-hook 'kill-buffer-hook #'opencode--forget-child nil t)))

(defun opencode--refresh-subagent (overlay)
  "Recheck subagent OVERLAY once its call finished or reported its session."
  (opencode--attach-child overlay))

(add-hook 'opencode-blocks-finished-functions #'opencode--refresh-subagent)

(keymap-set opencode-session-mode-map "C-c C-q" #'opencode-respond)

(provide 'opencode-requests)
;;; opencode-requests.el ends here
