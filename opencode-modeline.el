;;; opencode-modeline.el --- Session mode line  -*- lexical-binding: t; -*-

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

;; Session buffers get their own `mode-line-format', derived from the
;; default one at each redisplay so later changes to the default (moody,
;; minions, a global status) reach them too.  The status icon comes first
;; and is the only status shown, because it always stays visible: the
;; icon of the session's status as every display of sessions shows it
;; (see opencode-look.el), animated while a turn runs, followed by the
;; icons of subagents in the middle of a turn, waiting on an answer or
;; working, and of the archive.  The session title follows, inside a
;; moody tab when moody is in use, without the `*OpenCode: ...*' wrapper
;; the buffer name keeps for buffer completion.  Then come the agent (in
;; its configured color, else a stable theme face), the model, the
;; variant (in `opencode-setting-chosen' when selected,
;; `opencode-setting-inherited' when inherited), the provider as an icon
;; and the context use as a filling pie.  The mode name,
;; `mode-line-process' and the modified and remote flags, always `**-'
;; in a Comint buffer, are left out.
;;
;; Session windows are often 76 to 120 columns wide, so the title and
;; details are fitted to what the rest of the mode line leaves, in pixels:
;; icons are wider than a column and moody's slants are images.  The title
;; shrinks first, down to `opencode-mode-line-title-min-width' columns;
;; then details are dropped one step at a time, the model's "Claude "
;; prefix, the provider, the variant, the model and last the context.
;; The mode line is computed at every redisplay of its window, so each
;; window keeps its last fit, and fits again only when what it shows,
;; its width, the font or the rest of the mode line changed.
;;
;; The agent, model, variant, provider and context are drawn by the
;; displays of opencode-display.el, which other packages' session lists
;; share.  A model can cost more per token once a request exceeds a
;; context size; crossing it is reported once, as a step measures it:
;; the estimate a compaction leaves is shown, with a "~", but never
;; reported, and the next step past the size reports it again.

;;; Code:

(require 'opencode-blocks)
(require 'opencode-common)
(require 'opencode-display)
(require 'opencode-look)
(require 'opencode-requests)
(require 'opencode-session)

(defcustom opencode-mode-line t
  "When non-nil, session buffers show the session mode line."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-mode-line-title-min-width 24
  "Columns the session title keeps before session details are dropped."
  :type 'natnum
  :group 'opencode)

(defvar-local opencode--long-context-notified nil
  "Non-nil after reporting that this session's context passed a price tier.")

(defun opencode--long-context-check (&rest _)
  "Report once that the session's context passed its price tier."
  (let ((tier (opencode-long-context-tier)))
    (when (and (eq (car tier) 'over) (not opencode--long-context-notified))
      (message "OpenCode: %s exceeds %dk tokens of context; requests cost %s"
               (or (alist-get 'title opencode-session-info) opencode-session-id)
               (/ (nth 1 tier) 1000) (nth 2 tier)))
    (setq opencode--long-context-notified (eq (car tier) 'over))))

(defun opencode--long-context-replayed ()
  "Take a reopened session's tier as known, so an old crossing is not reported.
An estimate is no crossing: the next step measures the size."
  (setq opencode--long-context-notified (and (not opencode-session-tokens-estimated)
                                             (eq (car (opencode-long-context-tier)) 'over))))

(defun opencode--long-context-compacted (&rest _)
  "Have the next step past a price tier report it.
Compacting replaced the context that crossed it."
  (setq opencode--long-context-notified nil))

(add-hook 'opencode-step-ended-functions #'opencode--long-context-check)
(add-hook 'opencode-compaction-ended-functions #'opencode--long-context-compacted)
(add-hook 'opencode-session-replayed-hook #'opencode--long-context-replayed)

;;;; Segments

;;;; Status

;; The status icon is that of the session's status, as every display of
;; sessions shows it (see opencode-look.el), followed by the icons of
;; subagents in the middle of a turn and of the archive.  A working or
;; retrying session's icon is animated: every frame takes the room of
;; the widest, so that the fitted title and details stay as they are and
;; only the icon changes.  One timer changes frames for every mode line
;; that animates, by updating the mode lines of the buffers it shows,
;; and only while a window of a visible frame shows them: a mode line
;; drawn with an animated icon asks for it again, and nothing ticks once
;; none is shown.

(defvar opencode--mode-line-flushes)

(defvar-local opencode--mode-line-registry nil
  "(KEY . PART) of what the registry tells this session's mode line.
See `opencode--mode-line-registry-part'.")

(defun opencode--mode-line-registry-part ()
  "Return what the registry tells of this session for its mode line.
The value is (STATUS SUBAGENTS ARCHIVED): STATUS, `failed' or `done' when
its last turn ended unseen, else nil; SUBAGENTS, the kinds of
`opencode-subagent-icons' its subagents are in, `waiting' first; and
ARCHIVED, non-nil when it is archived.  It is made again only when the
registry changed."
  (let ((key (cons opencode-session-id (opencode-sv opencode-state--revision))))
    (if (equal key (car opencode--mode-line-registry))
        (cdr opencode--mode-line-registry)
      (let* ((entry (and opencode-session-id (opencode-state-get opencode-session-id)))
             (status (and entry (opencode-state-status entry)))
             (subagents (and opencode-session-id (opencode-state-subagent-statuses opencode-session-id)))
             (part (list (and (memq status '(failed done)) status)
                         (delq nil (list (and (memq 'waiting subagents) 'waiting)
                                         (and (or (memq 'working subagents) (memq 'retrying subagents))
                                              'working)))
                         (and entry (plist-get entry :archived) t))))
        (setq opencode--mode-line-registry (cons key part))
        part))))

(defun opencode--mode-line-own-request-p ()
  "Return non-nil when this session itself waits on a request of the user."
  (seq-some (lambda (provider) (plist-get (funcall provider) :pending)) opencode-request-providers))

(defun opencode--mode-line-help (kind)
  "Return the tooltip of the status icon of this session, of status KIND."
  (pcase kind
    ('waiting "Waiting for your answer")
    ('retrying (if-let* ((retry opencode-session-retry))
                   (format "Retry #%s: %s" (alist-get 'attempt retry)
                           (alist-get 'message (alist-get 'error retry)))
                 "Retrying"))
    ('working "Busy")
    (_ (opencode-session-status-label kind))))

(defun opencode--mode-line-pad (pixels)
  "Return a space PIXELS wide, or \"\" when PIXELS is not positive."
  (if (> pixels 0)
      (propertize " " 'display `(space :width (,pixels)))
    ""))

(defun opencode--mode-line-measure (string)
  "Return the width in pixels of STRING drawn inside the mode line.
A box of the `mode-line' face draws its edges at the ends of the whole
mode line only, not around STRING within it.  Measured alone, STRING
would count them, yet not for every glyph: a character a font draws
gets them, a stretch space or a character no font has does not."
  (let ((copy (copy-sequence string)))
    (add-face-text-property 0 (length copy) 'mode-line t copy)
    (add-face-text-property 0 (length copy) '(:box nil) nil copy)
    (opencode-mode-line-pixel-width copy)))

(defvar-local opencode--mode-line-status nil
  "(KEY . STATUS) of the status last made, see `opencode--mode-line-status-make'.")

(defun opencode--mode-line-status-make (kind subagents archived)
  "Return the status of this session's mode line, as a plist.
KIND is its status, SUBAGENTS and ARCHIVED as `opencode--mode-line-registry-part'
returns them.  :still is the status as a still string, its icon padded
to the cell of every frame; :frames, the strings of the frames of an
animated icon, each padded to that cell, or nil.  Each is followed by
the icons of SUBAGENTS and ARCHIVED, and escaped for the mode line."
  (let* ((help (opencode--mode-line-help kind))
         (icon (propertize (opencode-session-status-glyph kind) 'help-echo help))
         (frames (mapcar (lambda (frame) (propertize frame 'help-echo help))
                         (opencode-session-status-frames kind)))
         (widths (mapcar #'opencode--mode-line-measure (cons icon frames)))
         (cell (apply #'max widths))
         (tail (concat (mapconcat (lambda (kind)
                                    (concat " " (propertize (opencode-subagent-glyph kind)
                                                            'help-echo (if (eq kind 'waiting)
                                                                           "Subagents wait for your answer"
                                                                         "Subagents work"))))
                                  subagents "")
                       (and archived
                            (concat " " (propertize (opencode-archived-glyph) 'help-echo "Archived"))))))
    (cl-flet ((padded (string width)
                (opencode-mode-line-escape
                 (concat string (opencode--mode-line-pad (- cell width)) tail))))
      (list :still (padded icon (car widths))
            :frames (cl-mapcar #'padded frames (cdr widths))))))

(defun opencode--mode-line-status ()
  "Return the status of this session's mode line; see `opencode--mode-line-status-make'.
It is made again only when what it shows changed, as the mode line is
redrawn on every streamed chunk, and on every frame of an animation."
  (pcase-let* ((`(,status ,subagents ,archived) (opencode--mode-line-registry-part))
               ;; Its buffer knows a subagent's request before the registry.
               (subagents (if (and opencode--pending-children (not (memq 'waiting subagents)))
                              (cons 'waiting subagents)
                            subagents))
               (kind (cond ((opencode--mode-line-own-request-p) 'waiting)
                           ((eq opencode-session-status 'busy)
                            (if opencode-session-retry 'retrying 'working))
                           (status)
                           (t 'idle)))
               (key (list kind subagents archived opencode-session-retry
                          (window-font-width nil 'mode-line) opencode--mode-line-flushes
                          opencode-session-status-icons opencode-session-status-animations
                          opencode-session-status-animation-interval opencode-subagent-icons
                          opencode-archived-icon)))
    (if (equal key (car opencode--mode-line-status))
        (cdr opencode--mode-line-status)
      (let ((made (opencode--mode-line-status-make kind subagents archived)))
        ;; A copy, so a value later changed in place does not match it.
        (setq opencode--mode-line-status (cons (copy-tree key) made))
        made))))

(defun opencode-mode-line-status ()
  "Return the status icon of this session as its mode line shows it now.
That is the icon of its status, see `opencode-session-status-icons', or
the frame of its animation the clock tells, followed by the icons of
its subagents in the middle of a turn and of the archive."
  (let* ((status (opencode--mode-line-status))
         (frames (plist-get status :frames)))
    (if frames
        (nth (opencode-session-status-frame-index (length frames)) frames)
      (plist-get status :still))))

(defvar-local opencode--mode-line-animating nil
  "Non-nil while this buffer's mode line was last drawn with an animated icon.")

(defvar opencode--mode-line-animated nil
  "Session buffers whose mode line was drawn with an animated icon.")

(defvar opencode--mode-line-animation-timer nil
  "Timer showing the next frame of the animated icons of mode lines.")

(defvar opencode--mode-line-animation-pace nil
  "Seconds between the ticks of `opencode--mode-line-animation-timer'.")

(defun opencode--mode-line-animate ()
  "Have the mode line of this buffer show the next frame of its icon."
  (unless (memq (current-buffer) opencode--mode-line-animated)
    (push (current-buffer) opencode--mode-line-animated))
  (unless (timerp opencode--mode-line-animation-timer)
    (setq opencode--mode-line-animation-pace opencode-session-status-animation-interval
          opencode--mode-line-animation-timer
          (run-at-time (opencode-session-status-frame-delay)
                       opencode-session-status-animation-interval
                       #'opencode--mode-line-tick))))

(defun opencode--mode-line-stop-animation ()
  "Stop the timer of the animated icons of mode lines."
  (when (timerp opencode--mode-line-animation-timer)
    (cancel-timer opencode--mode-line-animation-timer))
  (setq opencode--mode-line-animation-timer nil
        opencode--mode-line-animated nil))

(defun opencode--mode-line-tick ()
  "Show the next frame in the mode lines that animate an icon and show.
Only the mode lines of the buffers shown in a window of a visible frame
are updated, with `force-mode-line-update' in each, never every mode
line.  A buffer no longer shown, or no longer animated, is dropped: its
mode line asks again once drawn animated.  The timer stops once none
is left.  Animations turned off or set to another pace stop it too, and
update the mode lines it animated, which then show still icons or start
it anew at that pace."
  (condition-case err
      (if (not (eql opencode--mode-line-animation-pace opencode-session-status-animation-interval))
          (let ((buffers opencode--mode-line-animated))
            (opencode--mode-line-stop-animation)
            (dolist (buffer buffers)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (force-mode-line-update)))))
        (let ((kept nil))
          (dolist (buffer opencode--mode-line-animated)
            (when (and (buffer-live-p buffer)
                       (buffer-local-value 'opencode--mode-line-animating buffer)
                       (get-buffer-window buffer 'visible))
              (push buffer kept)
              (with-current-buffer buffer
                (force-mode-line-update))))
          (if kept
              (setq opencode--mode-line-animated kept)
            (opencode--mode-line-stop-animation))))
    (error
     (opencode--mode-line-stop-animation)
     (message "OpenCode: mode-line animation stopped: %s" (error-message-string err)))))

(opencode-defvar-service opencode--mode-line-registry-timer nil
  "Timer comparing the mode lines shown with what the registry tells.")

(defun opencode--mode-line-registry-changed (&rest _)
  "Update the mode lines whose part from the registry changed, after this tick.
For `opencode-state-updated-functions': a turn ending unseen, a session
viewed, a subagent starting or ending a turn, or a session archived
show in the mode line, without the session buffer itself changing."
  (unless (timerp (opencode-sv opencode--mode-line-registry-timer))
    (setf (opencode-sv opencode--mode-line-registry-timer)
          (run-at-time 0 nil #'opencode--mode-line-registry-check))))

(defun opencode--mode-line-registry-check ()
  "Update the mode lines shown whose part from the registry changed."
  (setf (opencode-sv opencode--mode-line-registry-timer) nil)
  (maphash (lambda (_id buffer)
             (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
               (with-current-buffer buffer
                 (let ((drawn (cdr opencode--mode-line-registry)))
                   (unless (equal drawn (opencode--mode-line-registry-part))
                     (force-mode-line-update))))))
           (opencode-sv opencode-session-buffers)))

(add-hook 'opencode-state-updated-functions #'opencode--mode-line-registry-changed)

(defun opencode-mode-line-title (&optional name)
  "Return the session title in buffer NAME, without the OpenCode wrapper.
NAME defaults to the current buffer's name; a uniquifying <N> suffix stays."
  (let ((name (or name (buffer-name))))
    (if (string-match "\\`\\*OpenCode: \\(.*\\)\\*\\(<[0-9]+>\\)?\\'" name)
        (concat (match-string 1 name) (match-string 2 name))
      name)))

(defun opencode--mode-line-agent (id)
  "Return agent ID as `opencode-agent-display' shows it in this session.
Until the agent catalog arrives, ID goes by itself, in the first face."
  (or (opencode-agent-display id) (opencode--agent-string id nil)))

(defun opencode--mode-line-compute-segments ()
  "Return the session details as an alist of (SEGMENT . STRING-OR-NIL).
Until the catalogs arrive, the agent, model and provider go by their IDs."
  (when opencode-session-agent
    (let* ((model opencode-session-model)
           (id (alist-get 'id model))
           (provider-id (alist-get 'providerID model)))
      `((agent . ,(opencode--mode-line-agent opencode-session-agent))
        (model . ,(or (opencode-model-name model) id))
        (short-model . ,(or (opencode-model-display model) (and id (opencode--model-short-name id))))
        (variant . ,(opencode-variant-display model))
        (provider . ,(and provider-id
                          (or (opencode-provider-display provider-id)
                              (opencode--provider-string provider-id provider-id))))
        (context . ,(opencode-context-display opencode-session-tokens model nil
                                              opencode-session-tokens-estimated))))))

(defvar-local opencode--mode-line-segments nil
  "(KEY . SEGMENTS) of the session details last computed.")

(defun opencode-mode-line-segments ()
  "Return the session details as an alist of (SEGMENT . STRING-OR-NIL).
SEGMENT is one of `agent', `model', `short-model', `variant', `provider'
and `context'; `short-model' drops a leading \"Claude \".  They are
computed again only when the session or its location's catalogs
changed, as the mode line is redrawn on every streamed chunk, and once
after each reconnection, so that the displays ask again for the
catalogs it left stale (see `opencode-catalog-ensure')."
  (let ((key (list opencode-session-agent opencode-session-model opencode-session-tokens
                   opencode-session-tokens-estimated default-directory
                   (opencode-catalog-location-generation) (opencode-sv opencode--catalog-epoch)
                   opencode-provider-icons opencode-agent-faces opencode-default-variant-function
                   opencode-long-context-tiers opencode-long-context-near)))
    (if (equal key (car opencode--mode-line-segments))
        (cdr opencode--mode-line-segments)
      (let ((segments (opencode--mode-line-compute-segments)))
        ;; A copy, so a value later changed in place does not match it.
        (setq opencode--mode-line-segments (cons (copy-tree key) segments))
        segments))))

;;;; Fitting

(defconst opencode-mode-line-levels
  '((agent model variant provider context)
    (agent short-model variant provider context)
    (agent short-model variant context)
    (agent short-model context)
    (agent context)
    (agent))
  "Session detail segments shown, from the widest mode line to the narrowest.")

(defconst opencode-mode-line-element '(:eval (opencode-mode-line))
  "Mode-line element rendering the session status, title and details.")

(defvar opencode--mode-line-width-cache (make-hash-table :test #'equal)
  "Pixel widths of rendered mode-line strings, keyed by text and properties.")

(defvar opencode--mode-line-flushes 0
  "Count of `opencode-mode-line-flush' calls, which outdate every fitted mode line.")

(defun opencode-mode-line-flush ()
  "Forget the measured widths of mode-line text, after fonts changed."
  (clrhash opencode--mode-line-width-cache)
  (cl-incf opencode--mode-line-flushes)
  (opencode--update-mode-lines))

(defun opencode-mode-line-escape (string)
  "Return STRING with each % doubled, keeping its text properties."
  (let ((start 0) parts pos)
    (while (setq pos (string-search "%" string start))
      (push (substring string start (1+ pos)) parts)
      (push (substring string pos (1+ pos)) parts)
      (setq start (1+ pos)))
    (apply #'concat (nreverse (cons (substring string start) parts)))))

(defun opencode-mode-line-pixel-width (string)
  "Return the pixel width of rendered mode-line STRING, memoized.
Stretch spaces aligned to a position only fill leftover room, so they
count as one column."
  (let ((key (cons string (object-intervals string))))
    (or (gethash key opencode--mode-line-width-cache)
        (let ((copy (copy-sequence string))
              (pos 0))
          (while (< pos (length copy))
            (let ((next (next-single-property-change pos 'display copy (length copy)))
                  (display (get-text-property pos 'display copy)))
              (when (and (eq (car-safe display) 'space)
                         (plist-member (cdr display) :align-to))
                (remove-list-of-text-properties pos next '(display) copy))
              (setq pos next)))
          (when (> (hash-table-count opencode--mode-line-width-cache) 512)
            (clrhash opencode--mode-line-width-cache))
          (puthash key (string-pixel-width copy) opencode--mode-line-width-cache)))))

(defun opencode-mode-line-fit (title-width candidates available char-width min-width)
  "Choose session details and a title width fitting AVAILABLE pixels.
CANDIDATES are (DETAILS . PIXELS) from the most detailed to the least.
Return (DETAILS . COLUMNS) for the first candidate leaving the title its
full TITLE-WIDTH columns, or at least MIN-WIDTH of them.  Past the last
candidate the title keeps whatever remains, never below one column.
CHAR-WIDTH is the pixel width of one title column."
  (let (result)
    (while (and candidates (not result))
      (let* ((candidate (pop candidates))
             (columns (floor (- available (cdr candidate)) char-width)))
        (when (or (null candidates) (>= columns (min title-width min-width)))
          (setq result (cons (car candidate) (max 1 (min columns title-width)))))))
    result))

(defun opencode-mode-line-details (segments level)
  "Join the SEGMENTS alist entries named by LEVEL into one string.
The model and its variant form one group; groups are two spaces apart."
  (let ((model (string-join
                (delq nil (mapcar (lambda (key)
                                    (and (memq key level) (alist-get key segments)))
                                  '(model short-model variant)))
                " ")))
    (string-join
     (delq nil (list (and (memq 'agent level) (alist-get 'agent segments))
                     (and (not (string-empty-p model)) model)
                     (and (memq 'provider level) (alist-get 'provider segments))
                     (and (memq 'context level) (alist-get 'context segments))))
     "  ")))

(declare-function moody-tab "moody")

(defun opencode-mode-line-tab (string)
  "Return a mode-line construct showing STRING in a moody tab when available."
  (if (and (fboundp 'moody-tab)
           (memq 'moody-mode-line-buffer-identification (default-value 'mode-line-format)))
      (moody-tab string nil 'down)
    (list " " string " ")))

(defun opencode--mode-line-title-string (title)
  "Return session TITLE propertized like the buffer identification."
  (propertize title
              'face 'mode-line-buffer-id
              'help-echo (concat (buffer-name) "\nmouse-1: Previous buffer\nmouse-3: Next buffer")
              'mouse-face 'mode-line-highlight
              'local-map mode-line-buffer-identification-keymap))

(defvar-local opencode--mode-line-candidates nil
  "(KEY . CANDIDATES) of the details last measured, as `opencode-mode-line-fit' takes them.")

(defun opencode--mode-line-candidates (segments)
  "Return the details of SEGMENTS at every level with their pixel widths.
They are measured again only when SEGMENTS or the mode-line font changed."
  (let ((key (list segments (window-font-width nil 'mode-line) opencode--mode-line-flushes)))
    (if (equal key (car opencode--mode-line-candidates))
        (cdr opencode--mode-line-candidates)
      (let ((candidates
             (delete-dups
              (mapcar (lambda (level)
                        (let ((details (concat "  " (opencode-mode-line-details segments level) " ")))
                          (cons details (opencode-mode-line-pixel-width details))))
                      opencode-mode-line-levels))))
        (setq opencode--mode-line-candidates (cons key candidates))
        candidates))))

(defun opencode--mode-line-moody-p ()
  "Return non-nil when session titles are drawn in moody tabs."
  (and (fboundp 'moody-tab)
       (memq 'moody-mode-line-buffer-identification (default-value 'mode-line-format))
       t))

(defvar-local opencode--mode-line-fitted nil
  "Weak table of the windows showing this buffer to (KEY . FITTED).
FITTED is what `opencode--mode-line-fit-window' last returned there.")

(defun opencode--mode-line-fit-window (status segments)
  "Return (TITLE . DETAILS) fitted to the selected window, both escaped.
TITLE is the session title, cut to fit beside STATUS, the escaped status
icon as a still string, after a space; DETAILS those of SEGMENTS that
fit.  They are fitted again only when what they depend on changed: the
session's status, details and title, the window's width, the font, and
the rest of the mode line, which is formatted every time, since anything
may change it.  An animated icon's frames take the room of its still
string, so that a frame changing fits nothing again."
  (let* ((rest (format-mode-line (remove opencode-mode-line-element (opencode-mode-line-format)) t))
         (char-width (window-font-width nil 'mode-line))
         (key (list status segments (buffer-name) rest char-width (window-pixel-width)
                    (window-right-divider-width) (window-scroll-bar-width) (window-mode-line-height)
                    (opencode--mode-line-moody-p) opencode-mode-line-title-min-width
                    opencode--mode-line-flushes))
         (table (or opencode--mode-line-fitted
                    (setq opencode--mode-line-fitted (make-hash-table :test #'eq :weakness 'key))))
         (cached (gethash (selected-window) table)))
    ;; Faces and images in the rest change its width, not only its text.
    (if (and cached (equal-including-properties key (car cached)))
        (cdr cached)
      (let* ((title (opencode-mode-line-title))
             (tab (format-mode-line (opencode-mode-line-tab (concat status " ")) t))
             (available (- (window-pixel-width)
                           (window-right-divider-width)
                           (window-scroll-bar-width)
                           (opencode-mode-line-pixel-width rest)
                           (opencode-mode-line-pixel-width tab)
                           char-width))
             (fit (opencode-mode-line-fit (string-width title) (opencode--mode-line-candidates segments)
                                          available char-width opencode-mode-line-title-min-width))
             (fitted (cons (opencode-mode-line-escape
                            (concat " "
                                    (opencode--mode-line-title-string
                                     (truncate-string-to-width title (cdr fit) nil nil "…"))))
                           (opencode-mode-line-escape (car fit)))))
        (puthash (selected-window) (cons key fitted) table)
        fitted))))

(defun opencode-mode-line ()
  "Return the session status, title and details fitted to the selected window.
The tab is made at every call, as moody draws the selected window's
differently, and the status icon, which shows the frame of its
animation the clock tells (see `opencode-session-status-animations')."
  (let* ((status (opencode--mode-line-status))
         (frames (plist-get status :frames))
         (fitted (opencode--mode-line-fit-window (plist-get status :still)
                                                 (opencode-mode-line-segments))))
    (setq opencode--mode-line-animating (and frames t))
    (when frames
      (opencode--mode-line-animate))
    (list (opencode-mode-line-tab
           (concat (if frames
                       (nth (opencode-session-status-frame-index (length frames)) frames)
                     (plist-get status :still))
                   (car fitted)))
          (cdr fitted))))

;;;; Format

(defvar opencode--mode-line-format-warned nil
  "Non-nil once the default mode line lacked the expected anchors.")

(defvar opencode--mode-line-format nil
  "(DEFAULT MODES . FORMAT): the adapted format last made, and copies of its sources.")

(defun opencode-mode-line-format ()
  "Return the default mode-line format adapted to session buffers.
Everything up to the buffer identification is kept but the modified and
remote flags; the identification and the slots before `mode-line-modes'
give way to `opencode-mode-line-element', and `mode-line-modes' loses
the major mode name and `mode-line-process'.  When the default lacks
those anchors it is used unchanged.  It is called twice per redisplay
of a session mode line, so it is made again only when the default
format or `mode-line-modes' changed."
  (let ((default (default-value 'mode-line-format))
        (cache opencode--mode-line-format))
    (if (and cache
             (equal-including-properties default (car cache))
             (equal-including-properties mode-line-modes (cadr cache)))
        (cddr cache)
      (let ((format (opencode--mode-line-adapt default)))
        ;; Copies, so that a format later changed in place does not match.
        (setq opencode--mode-line-format (cons (copy-tree default) (cons (copy-tree mode-line-modes) format)))
        format))))

(defun opencode--mode-line-adapt (default)
  "Return DEFAULT, the default mode-line format, adapted as `opencode-mode-line-format' says."
  (let* ((id (seq-position default nil
                           (lambda (element _)
                             (memq element '(moody-mode-line-buffer-identification
                                             mode-line-buffer-identification)))))
         (modes (memq 'mode-line-modes default)))
    (if (not (and id modes))
        (progn
          (unless opencode--mode-line-format-warned
            (setq opencode--mode-line-format-warned t)
            ;; Called at redisplay, where a warning buffer cannot be shown.
            (delay-warning 'opencode "The default mode line has no buffer identification or `mode-line-modes'; the session mode line is off"))
          default)
      (append (seq-remove (lambda (element)
                            (memq element '(mode-line-modified mode-line-remote)))
                          (seq-take default id))
              (list opencode-mode-line-element
                    (seq-remove (lambda (element)
                                  (or (equal element '("" mode-line-process))
                                      (and (eq (car-safe element) :propertize)
                                           (equal (cadr element) '("" mode-name)))))
                                mode-line-modes))
              (cdr modes)))))

(defun opencode-mode-line-setup ()
  "Use the session mode line in this buffer when `opencode-mode-line' is non-nil."
  (when opencode-mode-line
    (setq-local mode-line-format '((:eval (opencode-mode-line-format))))))

(add-hook 'opencode-session-mode-hook #'opencode-mode-line-setup)

(provide 'opencode-modeline)
;;; opencode-modeline.el ends here
