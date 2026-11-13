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

;; Once a turn ended, a session buffer shows, just above its input, a
;; few dimmed sentences on where the session stands: what it set out to
;; do, what is done, what comes next, so that its last turn, just above,
;; reads in its context.  opencode.el writes no summary:
;; `opencode-session-summary-function' tells it, such as drover's,
;; which a cheap model writes, and calls `opencode-session-summary-update'
;; once it changed; nil shows none.  The summary shows while the session
;; is idle at its prompt, and goes as a turn starts or a prompt is sent.
;; It shows too while the session waits for its user to answer a
;; permission or a question, as the user comes back to it, at the end of
;; the transcript, until the request is answered.
;;
;; It is text, written with the idle prompt and going with it (see
;; `opencode-idle-start'): read-only as the transcript, it can be read,
;; moved through, copied and quoted as the transcript can, but it is no
;; part of the conversation, which the buffer catches up on and reloads
;; without it.  Text needs nothing written while a turn streams, as the
;; pending area would: the summary is gone by then.  A session waiting
;; on its user has no idle prompt, and its turn may still write, such as
;; a call running beside the one asking: the summary ends the transcript
;; then (see `opencode-write-end-text'), gives way to whatever is written
;; after it, and shows again once that stopped for
;; `opencode-recap--return-delay' seconds.

;;; Code:

(require 'opencode-common)
(require 'opencode-inbox)
(require 'opencode-look)
(require 'opencode-render)

(defvar opencode--holding)
(defvar opencode-session-status)

(declare-function opencode-pending-request-p "opencode-requests")

(defcustom opencode-session-summary-function nil
  "Function telling where the session of the current buffer stands, or nil.
It is called with no argument in a session buffer whose session is
idle at its prompt, or waits for its user to answer a permission or a
question, and returns nil when there is nothing to tell, else a string,
a few sentences such as \"Adding FTS search to the notes app: the index
and the query parser are done, tests pass; next, ranking.\", or (LINE .
RECAP), a line and a few more under it.  The buffer shows it dimmed, as
text just above its input, until a turn starts or a prompt is sent;
while the session waits, until the request is answered.

It is called as the idle prompt is written, and when
`opencode-session-summary-update' asks, not at every redisplay: what
writes the summary calls that function once it changed.  It must
neither wait nor change the buffer.  Nil shows nothing."
  :type '(choice (const :tag "None" nil) function)
  :group 'opencode)

(defconst opencode-recap--lines 8
  "Lines of a summary shown, at most.")

(defconst opencode-recap--limit 2000
  "Characters of a summary shown, at most.")

(defvar opencode-recap--return-delay 1
  "Seconds without output after which a waiting session's summary shows again.
It gives way to whatever its turn writes meanwhile.")

(defvar-local opencode--recap-content nil
  "The summary the buffer showed last, a string, or nil.")

(defvar-local opencode--recap-return nil
  "Timer showing again the summary that gave way to output, or nil.
Its last argument is the process mark when it was armed: output moving
it since puts the summary off again.")

(defvar opencode--recap-inhibited nil
  "Non-nil while the idle prompt is written to send a prompt at once.
The summary would go with it as soon as written.")

(defun opencode-recap--asking-p ()
  "Return non-nil when this buffer may ask where its session stands.
That is while a function tells it and the buffer follows its session,
its events applied."
  (and opencode-session-summary-function
       (not opencode--holding)
       (get-buffer-process (current-buffer))))

(defun opencode-recap--settled-p ()
  "Return non-nil when this buffer may show where its session stands at its prompt.
That is while the session is idle, not while a turn runs nor as a
prompt is sent."
  (and (opencode-recap--asking-p)
       (not opencode--recap-inhibited)
       (eq opencode-session-status 'idle)))

(defun opencode-recap--waiting-p ()
  "Return non-nil when this buffer's session waits for its user.
Its turn runs, but goes on only once a permission or a question is
answered, its own or a subagent's."
  (and (opencode-recap--asking-p)
       (eq opencode-session-status 'busy)
       (fboundp 'opencode-pending-request-p)
       (opencode-pending-request-p)))

(defun opencode-recap--ask ()
  "Return what `opencode-session-summary-function' tells, a string, or nil.
A line and a recap under it are joined; the summary keeps its first
`opencode-recap--lines' lines and `opencode-recap--limit' characters,
blank lines dropped.  A failure tells nothing."
  (let* ((value (condition-case err
                    (funcall opencode-session-summary-function)
                  (error (message "OpenCode: the session's summary failed: %s" (error-message-string err))
                         nil)))
         (text (cond ((stringp value) value)
                     ((and (consp value) (stringp (car value)))
                      (concat (car value) (and (stringp (cdr value)) (concat "\n" (cdr value))))))))
    (when text
      (let ((lines (seq-take (split-string text "[\n\r]+" t "[ \t]+") opencode-recap--lines)))
        (when lines
          (let ((summary (string-join lines "\n")))
            (if (> (length summary) opencode-recap--limit)
                (concat (substring summary 0 (1- opencode-recap--limit)) "…")
              summary)))))))

(defun opencode-recap--text (summary)
  "Return the text showing SUMMARY before the idle prompt, a blank line after it.
Its lines start after the icon's width, as its wrapped lines do."
  (let* ((icon (if (string-empty-p opencode-session-summary-icon)
                   ""
                 (concat (propertize opencode-session-summary-icon
                                     'font-lock-face 'opencode-session-summary-icon)
                         " ")))
         (indent (make-string (string-width icon) ?\s))
         (text (concat icon
                       (mapconcat (lambda (line) (propertize line 'font-lock-face 'opencode-session-summary))
                                  (split-string summary "\n")
                                  (concat "\n" indent))
                       "\n\n")))
    (add-text-properties 0 (length text) `(wrap-prefix ,indent opencode-idle-text t) text)
    (opencode-fontify-as text 'verbatim)))

(defun opencode-recap--idle-text ()
  "Return the text showing where the session stands, as the idle prompt is written.
For `opencode--idle-text-function'."
  (setq opencode--recap-content (and (opencode-recap--settled-p) (opencode-recap--ask)))
  (and opencode--recap-content (opencode-recap--text opencode--recap-content)))

(defun opencode-recap--start ()
  "Return where the summary shown starts, or nil when none shows.
Just before the idle prompt, else ending the transcript of a session
waiting for its user."
  (or (when-let* ((start (opencode-idle-start))
                  ((< start (marker-position opencode--prompt-start))))
        start)
      (opencode-end-text-start)))

(defun opencode-recap--shown ()
  "Return the summary the buffer shows above its input, or nil."
  (and (opencode-recap--start) opencode--recap-content))

(defun opencode-recap-remove ()
  "Delete the summary shown above the input, leaving the prompt and the input.
As a prompt is sent: the summary at the idle prompt described the turn
before it.  That of a session waiting for its user shows again after
the prompt, the session waiting still."
  (if (opencode-end-text-start)
      (opencode-lift-end-text t)
    (when-let* (((opencode-recap--shown))
                (start (opencode-idle-start)))
      (let ((inhibit-read-only t))
        (opencode-without-undo
          (delete-region start (marker-position opencode--prompt-start))))
      (setq opencode--recap-content nil))))

(defun opencode-recap--waiting-text (summary)
  "Return the text showing SUMMARY at the end of the transcript, after a blank line."
  (let ((lead (opencode-area-lead (opencode-pmark))))
    (concat (propertize lead 'opencode-idle-text t) (opencode-recap--text summary))))

(defun opencode-recap--return-later ()
  "Show the summary again once output stops, for `opencode--end-text-lifted-function'.
It gave way to output, an event or a prompt sent, while its session
waits for its user."
  (when (timerp opencode--recap-return)
    (cancel-timer opencode--recap-return))
  (setq opencode--recap-return
        (run-at-time opencode-recap--return-delay nil #'opencode-recap--return
                     (current-buffer) (opencode-pmark))))

(defun opencode-recap--return (buffer pmark)
  "Show again where the session of BUFFER stands, unless output came since.
PMARK is where the process mark was when the summary gave way: output
moving it since puts the summary off again, until output stops."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq opencode--recap-return nil)
      (when (get-buffer-process buffer)
        (if (/= pmark (opencode-pmark))
            (opencode-recap--return-later)
          (with-demoted-errors "OpenCode summary: %S"
            (opencode-session-summary-update)))))))

(defun opencode-recap-setup ()
  "Show in this session buffer where its session stands, above its input.
Earlier versions showed it as an overlay kept in place by hooks: those
go, and so does the overlay."
  (remove-hook 'after-change-functions 'opencode-recap--anchor t)
  (remove-hook 'pre-redisplay-functions 'opencode-recap--anchor-window t)
  (save-restriction
    (widen)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'opencode-recap)
        (delete-overlay overlay))))
  (opencode-session-summary-update))

(defun opencode-session-summary-update (&optional buffer)
  "Show anew where the session of session buffer BUFFER stands.
BUFFER defaults to the current buffer.  `opencode-session-summary-function'
is asked again: what writes the summary calls this once it changed.
The idle prompt is written again with it, the input and the points in
it kept; while the session waits for its user, the text ending the
transcript.  A buffer withholding its session's events, as it loads or
catches up, shows it once they are applied."
  (with-current-buffer (or buffer (current-buffer))
    (when (and opencode-session-id (get-buffer-process (current-buffer)) (not opencode--holding))
      (if (opencode-prompt-shown-p)
          (let ((summary (and (opencode-recap--settled-p) (opencode-recap--ask))))
            (unless (equal summary (opencode-recap--shown))
              (opencode-without-undo
                (opencode-keeping-followers
                  (let ((inhibit-read-only t))
                    (opencode-lift-prompt)
                    (opencode-open-block 'user)
                    (opencode-show-prompt))))))
        (let ((summary (and (opencode-recap--waiting-p) (opencode-recap--ask))))
          (unless (equal summary (opencode-recap--shown))
            (opencode-without-undo
              (opencode-keeping-followers
                (if summary
                    (progn
                      (opencode-lift-end-text)
                      (opencode-write-end-text (opencode-recap--waiting-text summary)))
                  (opencode-lift-end-text))))
            (setq opencode--recap-content summary)))))))

(defun opencode-session-summary-string ()
  "Return the text this buffer shows on where its session stands, or nil."
  (when-let* (((opencode-recap--shown))
              (start (opencode-recap--start)))
    (string-trim-left
     (buffer-substring-no-properties start (if (opencode-prompt-shown-p)
                                               (marker-position opencode--prompt-start)
                                             (opencode-pmark)))
     "\n+")))

;; Earlier versions kept their overlay in place with these hooks, which
;; buffers made then still run until `opencode-recap-setup' removes them.
(defun opencode-recap--anchor (&rest _)
  "Leave `after-change-functions', where an earlier version put this."
  (remove-hook 'after-change-functions 'opencode-recap--anchor t))

(defun opencode-recap--anchor-window (_window)
  "Leave `pre-redisplay-functions', where an earlier version put this."
  (remove-hook 'pre-redisplay-functions 'opencode-recap--anchor-window t))

(setq opencode--idle-text-function #'opencode-recap--idle-text
      opencode--end-text-lifted-function #'opencode-recap--return-later)

(provide 'opencode-recap)
;;; opencode-recap.el ends here
