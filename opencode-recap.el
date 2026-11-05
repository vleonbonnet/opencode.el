;;; opencode-recap.el --- Where a session stands, above its input  -*- lexical-binding: t; -*-

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

;; Once a turn ended, a session buffer shows, just above its input, one
;; dimmed line on where the session stands, such as "Done: added FTS
;; search, tests pass", and on a click a recap of the turn in a few
;; lines, as Claude Code shows its recap above its prompt.  opencode.el
;; writes no summary: `opencode-session-summary-function' tells it,
;; such as drover's, which a cheap model writes, and calls
;; `opencode-session-summary-update' once it changed; nil shows none.
;; The line shows only while the session is idle at its prompt, and
;; goes as a turn starts or a prompt is sent.
;;
;; Like the pending area (see opencode-inbox.el), it is an overlay
;; string where the points following output start, after the pending
;; prompts, never text: it never holds point, is never part of the
;; transcript nor of the input, and line motion passes over it.

;;; Code:

(require 'opencode-common)
(require 'opencode-inbox)
(require 'opencode-look)
(require 'opencode-render)

(defvar opencode--holding)
(defvar opencode-session-status)

(defcustom opencode-session-summary-function nil
  "Function telling where the session of the current buffer stands, or nil.
It is called with no argument in a session buffer whose session is
idle at its prompt, and returns nil when there is nothing to tell, else
a string, one line such as \"Done: added FTS search, tests pass\", or
\(LINE . RECAP), RECAP recapping the last turn in a few lines, or nil.
The buffer shows LINE dimmed just above its input until a turn starts
or a prompt is sent, RECAP below it on a click.

It is called as the session settles at its prompt and when
`opencode-session-summary-update' asks, not at every redisplay: what
writes the summary calls that function once it changed.  It must
neither wait nor change the buffer.  Nil shows nothing."
  :type '(choice (const :tag "None" nil) function)
  :group 'opencode)

(defcustom opencode-session-summary-recap nil
  "Non-nil shows the recap below the line on where a session stands.
Otherwise the recap is folded, an ellipsis after the line telling there
is one, and a click on the line, or `opencode-toggle-session-summary-recap',
shows it."
  :type 'boolean
  :group 'opencode)

(defconst opencode-recap--lines 8
  "Lines of a recap shown, at most.")

(defvar-local opencode--recap-overlay nil
  "Overlay showing where the session stands as its string, or nil.")

(defvar-local opencode--recap-settled nil
  "Non-nil when the session was last seen idle at its prompt.
Only a change of it asks `opencode-session-summary-function' anew.")

(defvar-local opencode--recap-content nil
  "What the buffer shows of where its session stands, (LINE . RECAP), or nil.")

(defvar-local opencode--recap-unfolded nil
  "Non-nil when the recap of `opencode--recap-content' shows.")

(defvar-keymap opencode-session-summary-map
  :doc "Keys of the line on where a session stands, which a click unfolds."
  "<mouse-1>" #'opencode-session-summary-click
  "<mouse-2>" #'opencode-session-summary-click)

(defun opencode-recap--settled-p ()
  "Return non-nil when this buffer may show where its session stands.
That is while a function tells it and the session is idle at its
prompt: not while a turn runs, nor once a prompt was sent."
  (and opencode-session-summary-function
       (eq opencode-session-status 'idle)
       (get-buffer-process (current-buffer))
       (opencode-prompt-shown-p)))

(defun opencode-recap--ask ()
  "Return what `opencode-session-summary-function' tells, as (LINE . RECAP), or nil.
LINE is the first line of what it returned, RECAP the rest of a recap
of `opencode-recap--lines' lines at most; a failure tells nothing."
  (let* ((value (condition-case err
                    (funcall opencode-session-summary-function)
                  (error (message "OpenCode: the session's summary failed: %s" (error-message-string err))
                         nil)))
         (line (if (consp value) (car value) value))
         (line (and (stringp line) (string-trim (car (split-string line "[\n\r]")))))
         (recap (and (consp value) (stringp (cdr value))
                     (split-string (cdr value) "[\n\r]+" t "[ \t]+"))))
    (when (and line (not (string-empty-p line)))
      (cons line (and recap (string-join (seq-take recap opencode-recap--lines) "\n"))))))

(defun opencode-recap--string ()
  "Return the lines showing `opencode--recap-content', a blank line after them."
  (opencode-session-summary-text (car opencode--recap-content) (cdr opencode--recap-content)
                                 opencode--recap-unfolded opencode-session-summary-map))

(defun opencode-recap--place ()
  "Keep the line on where the session stands just above the input.
That is where points following output start, after the pending area,
a blank line before it, whatever ends the transcript.  An overlay that
a rewrite of the transcript deleted is made again."
  (let ((overlay opencode--recap-overlay))
    (if (not (and (overlayp overlay) (overlay-buffer overlay)))
        (opencode-recap--render)
      (save-restriction
        (widen)
        (let* ((at (opencode--follow-start))
               ;; The pending area ends with a blank line of its own.
               (lead (if (opencode-inbox-area-p) "" (opencode-area-lead at))))
          (unless (= (overlay-start overlay) at)
            (move-overlay overlay at at))
          (unless (equal lead (overlay-get overlay 'opencode-recap-lead))
            (overlay-put overlay 'opencode-recap-lead lead)
            (overlay-put overlay 'before-string
                         (concat lead (overlay-get overlay 'opencode-recap-body)))))))))

(defun opencode-recap--render ()
  "Show `opencode--recap-content' above the input, or nothing without it."
  (if (null opencode--recap-content)
      (when (overlayp opencode--recap-overlay)
        (delete-overlay opencode--recap-overlay)
        (setq opencode--recap-overlay nil))
    (unless (and (overlayp opencode--recap-overlay) (overlay-buffer opencode--recap-overlay))
      (let ((at (save-restriction (widen) (opencode--follow-start))))
        (setq opencode--recap-overlay (make-overlay at at nil t t))
        (overlay-put opencode--recap-overlay 'opencode-recap t)
        ;; Before-strings show by increasing priority: after the pending area's.
        (overlay-put opencode--recap-overlay 'priority 1)))
    (overlay-put opencode--recap-overlay 'opencode-recap-body (opencode-recap--string))
    (overlay-put opencode--recap-overlay 'opencode-recap-lead nil)
    (opencode-recap--place)))

(defun opencode-recap--show (content)
  "Show CONTENT, (LINE . RECAP) or nil, as where the session stands.
A recap new shows folded, unless `opencode-session-summary-recap'."
  (if (equal content opencode--recap-content)
      (when content (opencode-recap--place))
    (setq opencode--recap-content content
          opencode--recap-unfolded opencode-session-summary-recap)
    (opencode-recap--render)))

(defun opencode-recap--anchor (&rest _)
  "Show where the session stands while it is idle at its prompt, in place.
The session settling there asks `opencode-session-summary-function',
anything else hides the line at once; a buffer withholding its events
shows it only once they are applied.  Output and the idle prompt coming
and going change where it goes, so this runs after every change and
before redisplay; it costs a comparison when nothing changed."
  (when (get-buffer-process (current-buffer))
    (let ((settled (opencode-recap--settled-p)))
      (cond
       ((eq settled opencode--recap-settled)
        (when opencode--recap-content
          (opencode-recap--place)))
       ((not settled)
        (setq opencode--recap-settled nil)
        (opencode-recap--show nil))
       ((not opencode--holding)
        (setq opencode--recap-settled t)
        (opencode-recap--show (opencode-recap--ask)))))))

(defun opencode-recap--anchor-window (_window)
  "Keep the line in place before redisplay, as `pre-redisplay-functions' run it."
  (opencode-recap--anchor))

(defun opencode-recap-setup ()
  "Show in this session buffer where its session stands, above its input."
  (add-hook 'after-change-functions #'opencode-recap--anchor nil t)
  (add-hook 'pre-redisplay-functions #'opencode-recap--anchor-window nil t)
  (opencode-session-summary-update))

(defun opencode-session-summary-update (&optional buffer)
  "Show anew where the session of session buffer BUFFER stands.
BUFFER defaults to the current buffer.  `opencode-session-summary-function'
is asked again: what writes the summary calls this once it changed.  A
buffer withholding its session's events, as it loads or catches up,
shows it once they are applied."
  (with-current-buffer (or buffer (current-buffer))
    (when (and opencode-session-id (get-buffer-process (current-buffer)) (not opencode--holding))
      (let ((settled (opencode-recap--settled-p)))
        (setq opencode--recap-settled settled)
        (opencode-recap--show (and settled (opencode-recap--ask)))))))

(defun opencode-session-summary-string ()
  "Return the text this buffer shows on where its session stands, or nil."
  (when (and (overlayp opencode--recap-overlay) (overlay-buffer opencode--recap-overlay))
    (substring-no-properties (overlay-get opencode--recap-overlay 'before-string))))

(defun opencode-toggle-session-summary-recap ()
  "Show or hide the recap below the line on where this session stands.
The session buffer shows the line just above its input once a turn
ended, as `opencode-session-summary-function' tells it."
  (interactive)
  (unless (cdr opencode--recap-content)
    (user-error "No recap of the last turn to show"))
  (setq opencode--recap-unfolded (not opencode--recap-unfolded))
  (opencode-recap--render))

(defun opencode-session-summary-click (event)
  "Show or hide the recap of the line clicked, as `opencode-toggle-session-summary-recap'.
EVENT is the click."
  (interactive "e")
  (let ((window (posn-window (event-end event))))
    (when (windowp window)
      (with-current-buffer (window-buffer window)
        (opencode-toggle-session-summary-recap)))))

(provide 'opencode-recap)
;;; opencode-recap.el ends here
