;;; opencode-look.el --- How sessions look, wherever they show  -*- lexical-binding: t; -*-

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

;; One look for sessions, wherever they show: the icons, faces, names
;; and animations of their statuses, the icons telling that subagents
;; are at work or that a session is archived, the faces telling where a
;; session is in Emacs, the faces of agents, the icons of providers,
;; the faces of a setting chosen or inherited, of the context a session
;; fills and of its prompt cache, and how ages and token counts read.
;; The session mode line and the session pickers draw with these, and
;; so do other packages' lists of sessions, such as drover's, which keep
;; no look of their own: an icon or a face customized here changes
;; everywhere.
;;
;; Working and retrying sessions are animated where icons may move: the
;; frames of `opencode-session-status-animations' replace their icon in
;; turn, every `opencode-session-status-animation-interval' seconds,
;; slower while Emacs has no focus
;; (`opencode-session-status-animation-unfocused-interval').
;; The frame shown comes from the clock, so that every animated icon,
;; in any buffer or package, shows the same frame at the same time.

;;; Code:

(require 'opencode-common)

;;;; Statuses

(defconst opencode-session-statuses '(waiting failed done retrying working idle)
  "Session statuses, as `opencode-state-status' names them, most urgent first.
The first three need the user: a request waits for an answer, or a turn
ended, failed or not, while nobody looked at the session.")

(defconst opencode-session-status-labels
  '((waiting . "Needs you") (failed . "Failed") (done . "Done")
    (retrying . "Retrying") (working . "Working") (idle . "Idle"))
  "Display name of each session status.")

(defface opencode-session-status-waiting
  '((t :inherit opencode-attention :weight bold))
  "Status of a session waiting on a permission request or a question.
It inherits `opencode-attention', the face of anything waiting on you."
  :group 'opencode-faces)

(defface opencode-session-status-failed
  '((t :inherit error))
  "Status of a session whose turn failed unseen."
  :group 'opencode-faces)

(defface opencode-session-status-done
  '((t :inherit success))
  "Status of a session whose turn ended unseen."
  :group 'opencode-faces)

(defface opencode-session-status-retrying
  '((t :inherit opencode-session-status-working :slant italic))
  "Status of a session whose turn waits to retry a failed request."
  :group 'opencode-faces)

(defface opencode-session-status-working
  '((t :inherit font-lock-keyword-face))
  "Status of a session running a turn."
  :group 'opencode-faces)

(defface opencode-session-status-idle
  '((t :inherit shadow))
  "Status of an idle session whose last turn was seen."
  :group 'opencode-faces)

(defcustom opencode-session-status-icons
  '((waiting . "\U000F02D7")            ; nf-md-help_circle
    (failed . "\uf057")                 ; nf-fa-times_circle
    (done . "\U000F05E0")               ; nf-md-check_circle
    (retrying . "\uf021")               ; nf-fa-refresh
    (working . "\U000F051F")            ; nf-md-timer_sand
    (idle . "\uf10c"))                  ; nf-fa-circle_o
  "Icon of each session status, as `opencode-state-status' names them.
Each entry is (STATUS . ICON), ICON a string shown in the face of
STATUS (see `opencode-session-status-face').  The session mode line,
the session pickers and other packages' lists of sessions, such as
drover's, show them; where icons may move, working and retrying
sessions show the frames of `opencode-session-status-animations'
instead.  The default icons come from Nerd Fonts, which must be
installed for them to show.  An entry of the former form, (STATUS ICON
FACE), still gives its icon."
  :type '(alist :key-type (symbol :tag "Status") :value-type (string :tag "Icon"))
  :group 'opencode)

(defcustom opencode-session-status-animations
  ;; nf-md-moon_new, waxing_crescent, first_quarter, waxing_gibbous,
  ;; full, waning_gibbous, last_quarter, waning_crescent
  '((working "\U000F0F64" "\U000F0F67" "\U000F0F61" "\U000F0F68"
             "\U000F0F62" "\U000F0F66" "\U000F0F63" "\U000F0F65")
    (retrying "\U000F144A" "\U000F1441" "\U000F1444" "\U000F1447")) ; nf-md-clock_time_twelve, three, six, nine
  "Frames of the status icons that animate, shown in turn.
Each entry is (STATUS FRAME...), each FRAME a string.  STATUS is
`working' or `retrying': where icons may move, such as the session mode
line and drover's lists, sessions of that status show the frames in
place of their icon of `opencode-session-status-icons', the same frame
everywhere at a time.  A status without frames stays still.  Nerd Font
icons by default: the phases of the moon, and a clock whose hand turns.
Each frame differs little from the one before, so the cycle reads as
motion; a lone frame much darker or lighter than the others, such as a
full hourglass among outlined ones, reads as a blink.
`opencode-session-status-animation-interval' sets the pace, or stops
every animation."
  :type '(alist :key-type (choice (const working) (const retrying))
                :value-type (repeat string))
  :group 'opencode)

(defcustom opencode-session-status-animation-interval 0.15
  "Seconds between two frames of the animated status icons.
Working sessions and those waiting to retry show the frames of
`opencode-session-status-animations' in turn, wherever icons may move.
A value of nil shows still icons, those of
`opencode-session-status-icons'.

Each frame costs a redisplay of what shows an animated icon: the window
of a session whose mode line animates, which Emacs lays out again
whole to redraw its mode line, the window of a list, laid out again
whole unless selected.  The default turns the eight phases of the moon
in 1.2 seconds, quick enough to read as busy rather than as a slow
blink; a slower pace costs fewer redisplays.  Nothing ticks while no
animated icon shows, and while Emacs has no focus, icons move at the
pace of `opencode-session-status-animation-unfocused-interval'."
  :type '(choice (const :tag "Still icons" nil) (number :tag "Seconds"))
  :group 'opencode)

(defun opencode-face-string (string face)
  "Return STRING shown in FACE, whether or not font lock runs where it goes.
Font lock replaces `face' properties with its own in buffers where it
runs, and keeps `font-lock-face'."
  (propertize string 'face face 'font-lock-face face))

(defun opencode-session-status-face (status)
  "Return the face of STATUS, a session status; an unknown one is idle's."
  (pcase status
    ('waiting 'opencode-session-status-waiting)
    ('failed 'opencode-session-status-failed)
    ('done 'opencode-session-status-done)
    ('retrying 'opencode-session-status-retrying)
    ('working 'opencode-session-status-working)
    (_ 'opencode-session-status-idle)))

(defun opencode-session-status-label (status)
  "Return the display name of STATUS, a session status."
  (or (alist-get status opencode-session-status-labels)
      (capitalize (format "%s" status))))

(defun opencode--session-status-icon-text (status)
  "Return the icon `opencode-session-status-icons' gives STATUS, or nil."
  (let ((icon (alist-get status opencode-session-status-icons)))
    ;; The former form, (STATUS ICON FACE), keeps working.
    (if (consp icon) (car icon) icon)))

(defun opencode-session-status-glyph (status)
  "Return the icon of STATUS, a session status, in its face.
A status without an icon in `opencode-session-status-icons' is \"-\"."
  (opencode-face-string (or (opencode--session-status-icon-text status) "-")
                        (opencode-session-status-face status)))

(defun opencode-session-status-icon (status)
  "Return the icon of STATUS in its face, its tooltip naming STATUS.
See `opencode-session-status-glyph'."
  (propertize (opencode-session-status-glyph status)
              'help-echo (opencode-session-status-label status)))

(defun opencode-session-status-frames (status)
  "Return the frames animating the icon of STATUS, in its face, or nil.
They are those of `opencode-session-status-animations', empty ones left
out.  Nil when STATUS does not animate, or when
`opencode-session-status-animation-interval' is nil."
  (when-let* ((opencode-session-status-animation-interval)
              (frames (seq-remove (lambda (frame) (or (not (stringp frame)) (string-empty-p frame)))
                                  (alist-get status opencode-session-status-animations))))
    (let ((face (opencode-session-status-face status)))
      (mapcar (lambda (frame) (opencode-face-string frame face)) frames))))

(defun opencode-session-status-frame-index (count &optional now)
  "Return which of COUNT frames of an animation shows at NOW, a float time.
NOW defaults to the current time.  Every animated icon, wherever it
shows, shows the frame of the same instant, so that icons keep pace
with one another and a redraw keeps the pace."
  (mod (floor (or now (float-time)) opencode-session-status-animation-interval) count))

(defun opencode-session-status-frame-delay (&optional now)
  "Return the seconds from NOW to the middle of the next frame of animations.
NOW defaults to the current time.  A timer showing frames ticks in the
middle of each, so that running a little late or early it never shows a
frame twice nor skips one."
  (let ((interval opencode-session-status-animation-interval)
        (now (or now (float-time))))
    (- (* (+ (floor now interval) 1.5) interval) now)))

(defcustom opencode-session-status-animation-unfocused-interval 1.35
  "Seconds between two frames of the animated status icons while Emacs has no focus.
A mode line or a list may show while the user works in another
application, on another screen or beside it: its icons keep moving
then, at this slower pace, never quicker than
`opencode-session-status-animation-interval'.  The frame shown is the
one of the time, as while Emacs has focus, the same in every list and
mode line, so that a tick spans a whole number of frames: this many
seconds rounded up to the next number of frames that shows each
animation of `opencode-session-status-animations' its next frame, one
more than a multiple of the frames of each.  The default is nine frames
of 0.15 s: the eight phases of the moon and the four hours of the clock
step forward once a tick, where seven frames, a second, showed them
turning backwards.  When no such number of frames comes within three
times this many seconds, the tick spans the least number of frames,
from this many seconds up, showing their next frame to the animations
of the most frames together.

Once Emacs has focus again, the icons show the frame of the time at
once, and move at the pace of `opencode-session-status-animation-interval'.
A value of nil keeps them still while Emacs has no focus.

Emacs has focus while any of its frames has: a mode line or a list in a
frame other than the one the user works in moves at the full pace."
  :type '(choice (const :tag "Still while unfocused" nil) (number :tag "Seconds"))
  :group 'opencode)

(defun opencode-session-status-forward-frames (target counts)
  "Return how many frames a tick spans to show the next frame of animations.
TARGET is the frames it should span at least, a number; COUNTS are the
numbers of frames of the animations.  The frame shown is the clock's:
spanning one more than a multiple of an animation's frames shows its
next frame, one less its frame before, as if it turned backwards.  It
is the least whole number, at least TARGET, that is one more than a
multiple of every count.  When that is past three times TARGET, it is
the least from TARGET up to there showing their next frame to the
animations of the most frames together: the more frames an animation
has, the more it reads as turning one way."
  (let* ((least (max 1 (ceiling (- target 1e-9))))
         (period (apply #'cl-lcm 1 counts))
         (forward (1+ (* period (ceiling (1- least) period))))
         (most (max least (floor (+ (* 3 target) 1e-9)))))
    (if (<= forward most)
        forward
      (let ((best least)
            (stepping -1))
        (cl-loop for frames from least to most
                 for weight = (apply #'+ (seq-filter (lambda (count) (= (mod frames count) 1)) counts))
                 when (> weight stepping)
                 do (setq best frames stepping weight))
        best))))

(defun opencode-session-status-frame-pace (focused)
  "Return the seconds between two frames of animated icons, or nil for none.
FOCUSED tells whether Emacs has focus (see
`opencode-session-status-focused-p').  The pace is
`opencode-session-status-animation-interval' while it has, else
`opencode-session-status-animation-unfocused-interval' rounded up to a
whole number of those intervals showing each animation its next frame,
as `opencode-session-status-forward-frames' counts them: each tick then
falls in the middle of a frame of the time
\(`opencode-session-status-frame-delay'), as it does at the full pace,
and every icon steps forward."
  (when-let* ((interval opencode-session-status-animation-interval))
    (if focused
        interval
      (let ((slow opencode-session-status-animation-unfocused-interval))
        (and (numberp slow) (> slow 0)
             (* interval
                (opencode-session-status-forward-frames
                 (/ slow interval)
                 (seq-filter (lambda (count) (> count 1))
                             (mapcar (lambda (entry) (length (opencode-session-status-frames (car entry))))
                                     opencode-session-status-animations)))))))))

(defun opencode-session-status-focused-p ()
  "Return non-nil unless Emacs is known to have lost focus.
It has focus while a frame has, or may have, as `frame-focus-state'
tells; the initial frame of a daemon or of a batch Emacs does not count,
and without another frame, Emacs is taken to have focus."
  (let ((frames (seq-remove #'frame-initial-p (frame-list))))
    (or (null frames)
        (and (seq-some #'frame-focus-state frames) t))))

;;;; Subagents and the archive

(defcustom opencode-subagent-icons
  '((waiting . "\U000F169F")            ; nf-md-robot_confused
    (working . "\U000F06A9"))           ; nf-md-robot
  "Icons telling that subagents of a session are in the middle of a turn.
They show beside the status of the session: `waiting' when subagents,
at any depth, wait for you, in the face of the waiting status, and
`working' when they work or wait to retry, in the face of the working
status.  They stay still and tell no number.  Nerd Font icons by
default."
  :type '(alist :key-type (choice (const waiting) (const working)) :value-type string)
  :group 'opencode)

(defcustom opencode-archived-icon "\U000F003C" ; nf-md-archive
  "Icon telling that a session is archived, after its status.
It shows in the face `opencode-session-archived'.  A Nerd Font icon by
default."
  :type 'string
  :group 'opencode)

(defface opencode-session-archived '((t :inherit shadow))
  "Face of the icon telling that a session is archived."
  :group 'opencode-faces)

(defun opencode-subagent-glyph (kind)
  "Return the icon of subagents of KIND, `waiting' or `working', in its face.
See `opencode-subagent-icons'; a kind without an icon is \"?\"."
  (opencode-face-string (or (alist-get kind opencode-subagent-icons) "?")
                        (opencode-session-status-face (if (eq kind 'waiting) 'waiting 'working))))

(defun opencode-archived-glyph ()
  "Return `opencode-archived-icon' in the face `opencode-session-archived'."
  (opencode-face-string opencode-archived-icon 'opencode-session-archived))

;;;; Where a session is

;; A session's title tells where it is in Emacs: in no buffer, in a
;; buffer no window shows, in a window, or in the window the user works
;; in.  The faces inherit sibling faces of font lock, which themes tell
;; apart from one another and from the default foreground, and which no
;; status takes but the working one, an icon rather than a title:
;; `success', `warning' and `error' would read as a status.

(defface opencode-session-open '((t :inherit font-lock-keyword-face))
  "Face of the titles of sessions open in a buffer no window shows.
It goes over the other faces of a title, such as the dimming of idle
sessions in a list, which keep the attributes it does not set.  See
also `opencode-session-shown' and `opencode-session-active'."
  :group 'opencode-faces)

(defface opencode-session-shown '((t :inherit font-lock-type-face))
  "Face of the titles of sessions shown in a window of a visible frame.
As `opencode-session-open', it goes over the other faces of a title."
  :group 'opencode-faces)

(defface opencode-session-active '((t :inherit font-lock-function-name-face))
  "Face of the title of the session of the window the user works in.
That is the selected window, or, while the minibuffer or a list of
sessions is selected, the window selected before.  It inherits the face
`which-function-mode' names the definition point is in with: where the
user is.  As `opencode-session-open', it goes over the other faces of a
title."
  :group 'opencode-faces)

(defun opencode-session-place (id)
  "Return where session ID is in Emacs, which the face of its title tells.
That is `active' when its buffer shows in the window the user works in,
the selected window or, while the minibuffer is selected, the one
selected before; `shown' when a window of a visible frame shows it;
`open' when it has a buffer no such window shows; nil without a buffer.
See `opencode-session-place-face'."
  (when-let* ((buffer (gethash id (opencode-sv opencode-session-buffers)))
              ((buffer-live-p buffer)))
    (cond ((eq (window-buffer (or (minibuffer-selected-window) (selected-window))) buffer) 'active)
          ((get-buffer-window buffer 'visible) 'shown)
          (t 'open))))

(defun opencode-session-place-face (place)
  "Return the face of the title of a session at PLACE, or nil.
PLACE is `active', `shown' or `open', as `opencode-session-place'
returns it; nil, a session without a buffer, has no face."
  (pcase place
    ('active 'opencode-session-active)
    ('shown 'opencode-session-shown)
    ('open 'opencode-session-open)))

;;;; What sessions run

(define-obsolete-variable-alias 'opencode-mode-line-agent-faces 'opencode-agent-faces "2.1.0")

(defcustom opencode-agent-faces
  '(font-lock-keyword-face font-lock-type-face font-lock-constant-face font-lock-string-face)
  "Faces agents that configure no color are shown in, bold.
An OpenCode agent takes one by its place among the primary agents of
its location's catalog, in the session mode line and wherever
`opencode-agent-display' shows it; other packages' lists, such as
drover's, give the agents of other front ends one by their name."
  :type '(repeat face)
  :group 'opencode)

(defcustom opencode-provider-icons
  '(("\\`github-copilot\\'" . "\uf4b8")  ; nf-oct-copilot
    ("\\`anthropic\\'" . "\uec82")       ; nf-cod-claude
    ("\\`openai\\'" . "\uec81"))         ; nf-cod-openai
  "Alist of (PROVIDER-ID-REGEXP . ICON) for the provider of a session.
Providers without a match show `opencode-provider-default-icon' followed
by their name."
  :type '(alist :key-type regexp :value-type string)
  :group 'opencode)

(defconst opencode-provider-default-icon "\U000F048D" ; nf-md-server_network
  "Icon of a provider `opencode-provider-icons' gives none, before its name.")

(defface opencode-setting-chosen '((t :inherit success))
  "Face of a setting chosen rather than inherited, such as a selected variant.
The mode line and lists show in it the variant, or effort, a session
runs when one is selected; drover's prompt the settings picked for the
next session."
  :group 'opencode-faces)

(defface opencode-setting-inherited '((t :inherit shadow))
  "Face of a setting inherited rather than chosen, such as a default variant.
The mode line and lists show in it the variant, or effort, a model runs
by default; drover's prompt what a new session would inherit."
  :group 'opencode-faces)

;;;; Context and prompt cache

(defface opencode-context-bar
  '((((class color) (min-colors 88) (background light)) :background "#ebe3f2")
    (((class color) (min-colors 88) (background dark)) :background "#382d45")
    (((class color)) :background "magenta")
    (t :underline t))
  "Face of the part of a model's context a session's context fills, in bars.
Only a background, subtle beside a prompt cache's bar, which takes the
colour of `opencode-cache-bar', so that the two bars of a line are told
apart."
  :group 'opencode-faces)

(defface opencode-context-near '((t :inherit opencode-attention))
  "Face of a context size nearing a long-context price tier.
Bars draw it in inverse video: its foreground as their background."
  :group 'opencode-faces)

(defface opencode-context-over '((t :inherit error))
  "Face of a context size past a long-context price tier, whose requests cost more.
Bars draw it in inverse video: its foreground as their background."
  :group 'opencode-faces)

(defun opencode-context-face (tier)
  "Return the face of a context size near or past TIER, or nil.
TIER is (STATE SIZE LABEL), as `opencode-long-context-tier' returns it,
STATE `near' or `over'."
  (pcase (car tier)
    ('over 'opencode-context-over)
    ('near 'opencode-context-near)))

(defface opencode-cache-bar '((t :inherit success))
  "Face of the part of a prompt cache's lifetime still left.
Its foreground draws the bar: drover's short bars fill with it, in
inverse video, and the line of a session in two lines is underlined in
it, so that the bar shows over the highlight of the current line and
the region, which change only the background.  It inherits `success':
the cache is still there to save the next turn's cost."
  :group 'opencode-faces)

(defface opencode-cache-trough '((t :inherit shadow :underline t))
  "Face of the part of a prompt cache's lifetime already gone, in short bars.
It shows how long the bar would be, without drawing attention."
  :group 'opencode-faces)

;;;; Pending prompts

(defcustom opencode-pending-prompt-icon "\U000F0150" ; nf-md-clock_outline
  "Icon of a prompt the service took but has not delivered to its session yet.
Session buffers show such prompts between the transcript and the input,
each headed by this icon and a label in `opencode-pending-label'.  The
default icon comes from Nerd Fonts; an empty string shows the label
alone."
  :type 'string
  :group 'opencode)

(defface opencode-pending-label
  '((t :inherit opencode-session-status-working :slant italic))
  "Label heading a pending prompt, which tells when it will be delivered.
It inherits the working status: a pending prompt waits on the turn."
  :group 'opencode-faces)

(defface opencode-pending-prompt '((t :inherit shadow))
  "Text of a pending prompt, dimmed: it is not part of the conversation yet."
  :group 'opencode-faces)

(defface opencode-pending-attachment
  '((t :inherit (font-lock-constant-face opencode-pending-prompt)))
  "Labels of the files, subagents and skills a pending prompt attaches."
  :group 'opencode-faces)

(defface opencode-pending-margin
  '((t :inherit opencode-session-status-working :height reset))
  "Margin of pending prompts, in the colour of the turn they wait on."
  :group 'opencode-faces)

;;;; Where a session stands

(defcustom opencode-session-summary-icon "※"
  "Icon heading the summary of where a session stands, above its input.
Claude Code heads its recap with it; an empty string shows the summary
alone.  See `opencode-session-summary-function'."
  :type 'string
  :group 'opencode)

(defface opencode-session-summary-icon '((t :inherit shadow))
  "Icon heading the summary of where a session stands, dimmed."
  :group 'opencode-faces)

(defface opencode-session-summary '((t :inherit shadow :slant italic))
  "Summary of where a session stands, above its input.
Dimmed and slanted: it is no part of the conversation, which another
wrote it from."
  :group 'opencode-faces)

(defface opencode-session-summary-recap '((t :inherit shadow))
  "Recap of the last turn, below the line on where a session stands."
  :group 'opencode-faces)

(defun opencode-session-summary-text (line &optional recap unfolded keymap)
  "Return the lines showing LINE on where a session stands, a blank line after them.
RECAP, a few lines recapping the last turn, or nil, shows below LINE
when UNFOLDED, else an ellipsis after it tells there is one, and a
click on the lines runs what KEYMAP binds, which shows or hides it.
Other packages' buffers of sessions show it above their input, as an
overlay string; session buffers write their summary as text instead
\(see `opencode-session-summary-function')."
  (let* ((icon (if (string-empty-p opencode-session-summary-icon)
                   ""
                 (concat (propertize opencode-session-summary-icon 'face 'opencode-session-summary-icon)
                         " ")))
         (indent (make-string (string-width icon) ?\s))
         (unfolded (and recap unfolded))
         (string
          (concat icon (propertize line 'face 'opencode-session-summary)
                  (and recap (not unfolded) (propertize " …" 'face 'opencode-session-summary-recap))
                  "\n"
                  (and unfolded
                       (mapconcat (lambda (recap-line)
                                    (concat indent
                                            (propertize recap-line 'face 'opencode-session-summary-recap)
                                            "\n"))
                                  (split-string recap "\n")))
                  "\n")))
    (put-text-property 0 (length string) 'wrap-prefix indent string)
    (when (and recap keymap)
      (add-text-properties 0 (1- (length string))
                           `(keymap ,keymap
                             mouse-face highlight
                             pointer hand
                             help-echo ,(if unfolded
                                            "mouse-1: hide the recap of the last turn"
                                          (concat recap "\n\nmouse-1: show this recap below")))
                           string))
    string))

;;;; Numbers

(defun opencode-format-duration (seconds)
  "Return SECONDS as \"42s\", \"5m\", \"3h\" or \"2d\", rounded down.
A negative duration counts as none."
  (let ((seconds (max 0 (floor seconds))))
    (cond ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm" (/ seconds 60)))
          ((< seconds 86400) (format "%dh" (/ seconds 3600)))
          (t (format "%dd" (/ seconds 86400))))))

(defun opencode-format-age (time &optional now)
  "Return the age of TIME, a float time, as \"42s\", \"5m\", \"3h\" or \"2d\".
NOW defaults to the current time.  Return \"\" for a nil TIME."
  (if (null time)
      ""
    (opencode-format-duration (- (or now (float-time)) time))))

(defun opencode-format-tokens (tokens)
  "Return a count of TOKENS as \"950\", \"8.4k\", \"182k\" or \"1.2M\".
Counts of thousands and millions keep one decimal below ten, none
from ten on; a decimal of zero is dropped."
  (cl-flet ((scaled (value unit)
              (if (< value 9.95)
                  (concat (string-remove-suffix ".0" (format "%.1f" value)) unit)
                (format "%d%s" (round value) unit))))
    (cond ((< tokens 1000) (format "%d" tokens))
          ((< tokens 999500) (scaled (/ tokens 1000.0) "k"))
          (t (scaled (/ tokens 1000000.0) "M")))))

(defun opencode-long-context-description (tier &optional _tokens estimated)
  "Return the line describing TIER, from `opencode-long-context-tier'.
Such as \"Past 272k tokens: requests cost 2×\", or \"Near 272k tokens,
past which requests cost 2×\".  ESTIMATED non-nil means the context
size TIER was found for is an estimate, as after a compaction."
  (pcase tier
    (`(over ,size ,label)
     (format "%s %s tokens: requests cost %s"
             (if estimated "Estimated past" "Past") (opencode-format-tokens size) label))
    (`(near ,size ,label)
     (format "%s %s tokens, past which requests cost %s"
             (if estimated "Estimated near" "Near") (opencode-format-tokens size) label))))

(defun opencode-context-description (tokens &optional limit tier estimated)
  "Return what a context of TOKENS fills, for tooltips and details.
Such as \"182k of 200k tokens (91%)\", LIMIT being the model's context
size, then, on lines of their own, whether the size is an estimate and
the long-context price TIER it nears or passes, as
`opencode-long-context-tier' returns it.  ESTIMATED non-nil means TOKENS
is the service's estimate after a compaction, which the next step
measures: it begins with \"~\"."
  (concat (and estimated "~")
          (opencode-format-tokens tokens)
          (and limit (concat " of " (opencode-format-tokens limit)))
          " tokens"
          (and limit (> limit 0) (format " (%d%%)" (round (* 100.0 tokens) limit)))
          (and estimated "\nEstimated by the service after compaction; the next step measures it")
          (and tier (concat "\n" (opencode-long-context-description tier tokens estimated)))))

(provide 'opencode-look)
;;; opencode-look.el ends here
