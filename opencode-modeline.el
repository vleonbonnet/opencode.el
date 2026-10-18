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
;; and is the only status shown, because it always stays visible: idle,
;; busy, or a question mark when the session or one of its subagents
;; waits on an answer.  The session title follows, inside a moody tab when
;; moody is in use, without the `*OpenCode: ...*' wrapper the buffer name
;; keeps for buffer completion.  Then come the agent (in its configured
;; color, else a stable theme face), the model, the variant (green when
;; selected, dim when inherited), the provider as an icon and the context
;; use as a filling pie.  The mode name, `mode-line-process' and the
;; modified and remote flags, always `**-' in a Comint buffer, are left
;; out.
;;
;; Session windows are often 76 to 120 columns wide, so the title and
;; details are fitted to what the rest of the mode line leaves, in pixels:
;; icons are wider than a column and moody's slants are images.  The title
;; shrinks first, down to `opencode-mode-line-title-min-width' columns;
;; then details are dropped one step at a time, the model's "Claude "
;; prefix, the provider, the variant, the model and last the context.
;;
;; A model can cost more per token once a request exceeds a context size,
;; as listed in its catalog pricing or in `opencode-long-context-tiers'.
;; The context turns to the attention face near that size and to the error
;; face past it, and crossing it is reported once.

;;; Code:

(require 'opencode-blocks)
(require 'opencode-common)
(require 'opencode-requests)
(require 'opencode-session)

(defcustom opencode-mode-line t
  "When non-nil, session buffers show the session mode line."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-provider-icons
  '(("\\`github-copilot\\'" . "")  ; nf-oct-copilot
    ("\\`anthropic\\'" . "")       ; nf-cod-claude
    ("\\`openai\\'" . ""))         ; nf-cod-openai
  "Alist of (PROVIDER-ID-REGEXP . ICON) for the mode-line provider segment.
Providers without a match show a generic icon followed by their name."
  :type '(alist :key-type regexp :value-type string)
  :group 'opencode)

(defcustom opencode-mode-line-agent-faces
  '(font-lock-keyword-face font-lock-type-face font-lock-constant-face font-lock-string-face)
  "Faces cycled over primary agents that configure no color."
  :type '(repeat face)
  :group 'opencode)

(defcustom opencode-mode-line-title-min-width 24
  "Columns the session title keeps before session details are dropped."
  :type 'natnum
  :group 'opencode)

(defcustom opencode-default-variant-function nil
  "Function naming the variant a model runs with when none is selected.
Called with the provider and model catalog entries; returns a variant ID,
or nil when the provider decides.  nil shows a dim \"default\"."
  :type '(choice (const :tag "Provider default" nil) function)
  :group 'opencode)

(defcustom opencode-long-context-tiers nil
  "Context sizes past which models cost more, beyond their catalog pricing.
Each entry is (PROVIDER-REGEXP MODEL-REGEXP SIZE LABEL): a model whose
provider and model IDs match costs what LABEL says, such as \"2×\",
once a request exceeds SIZE tokens."
  :type '(repeat (list (regexp :tag "Provider ID")
                       (regexp :tag "Model ID")
                       (natnum :tag "Tokens")
                       (string :tag "Label")))
  :group 'opencode)

(defcustom opencode-long-context-near 0.9
  "Fraction of a long-context tier's size from which the mode line warns."
  :type 'number
  :group 'opencode)

;;;; Long-context pricing

(defun opencode-long-context-threshold (&optional model info)
  "Return (SIZE . LABEL) of the higher price tier of MODEL, or nil.
MODEL is a model reference, by default the session's; INFO its catalog
entry, by default the cached one."
  (let* ((model (or model opencode-session-model))
         (info (or info (opencode-model-info model))))
    (or (when-let* ((entry (seq-find (lambda (entry)
                                       (and (string-match-p (nth 0 entry) (or (alist-get 'providerID model) ""))
                                            (string-match-p (nth 1 entry) (or (alist-get 'id model) ""))))
                                     opencode-long-context-tiers)))
          (cons (nth 2 entry) (nth 3 entry)))
        (let* ((costs (alist-get 'cost info))
               (base (seq-find (lambda (cost) (null (alist-get 'tier cost))) costs))
               (tier (car (sort (seq-filter (lambda (cost) (alist-get 'tier cost)) costs)
                                :key (lambda (cost) (alist-get 'size (alist-get 'tier cost)))))))
          (when tier
            (cons (alist-get 'size (alist-get 'tier tier))
                  (let ((from (alist-get 'input base))
                        (to (alist-get 'input tier)))
                    (if (and (numberp from) (numberp to) (> from 0))
                        (format "%g×" (/ (float to) from))
                      "a higher rate"))))))))

(defun opencode-long-context-tier (&optional model info)
  "Return (STATE SIZE LABEL) when the session's context nears a price tier.
STATE is `near' or `over'; MODEL and INFO are as for
`opencode-long-context-threshold'."
  (when-let* (((numberp opencode-session-tokens))
              (threshold (opencode-long-context-threshold model info)))
    (cond ((> opencode-session-tokens (car threshold))
           (list 'over (car threshold) (cdr threshold)))
          ((>= opencode-session-tokens (* opencode-long-context-near (car threshold)))
           (list 'near (car threshold) (cdr threshold))))))

(defun opencode-long-context-description (tier)
  "Return the tooltip line describing TIER, from `opencode-long-context-tier'."
  (pcase-let ((`(,state ,size ,label) tier))
    (if (eq state 'over)
        (format "Context over %dk: requests cost %s" (/ size 1000) label)
      (format "Context %dk/%dk: near the %s tier"
              (/ opencode-session-tokens 1000) (/ size 1000) label))))

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
  "Take a reopened session's tier as known, so an old crossing is not reported."
  (setq opencode--long-context-notified (eq (car (opencode-long-context-tier)) 'over)))

(add-hook 'opencode-step-ended-functions #'opencode--long-context-check)
(add-hook 'opencode-session-replayed-hook #'opencode--long-context-replayed)

;;;; Segments

(defun opencode-mode-line-status ()
  "Return the status icon of this session."
  (cond ((opencode-pending-request-p)
         (propertize (opencode-pending-prompt-icon) 'help-echo "Waiting for your answer"))
        ((eq opencode-session-status 'busy)
         (propertize "\U000F051F" ; nf-md-timer_sand
                     'face 'opencode-tool-running-margin
                     'help-echo (if-let* ((retry opencode-session-retry))
                                    (format "Retry #%s: %s" (alist-get 'attempt retry)
                                            (alist-get 'message (alist-get 'error retry)))
                                  "Busy")))
        (t (propertize "\U000F05E0" 'face 'success 'help-echo "Idle")))) ; nf-md-check_circle

(defun opencode-mode-line-title (&optional name)
  "Return the session title in buffer NAME, without the OpenCode wrapper.
NAME defaults to the current buffer's name; a uniquifying <N> suffix stays."
  (let ((name (or name (buffer-name))))
    (if (string-match "\\`\\*OpenCode: \\(.*\\)\\*\\(<[0-9]+>\\)?\\'" name)
        (concat (match-string 1 name) (match-string 2 name))
      name)))

(defun opencode--mode-line-agent (id agents)
  "Return agent ID styled with its color among AGENTS, else a stable face."
  (let* ((info (seq-find (lambda (agent) (equal (alist-get 'id agent) id)) agents))
         (name (or (alist-get 'name info) id))
         (color (alist-get 'color info))
         (primary (seq-keep (lambda (agent)
                              (and (equal (alist-get 'mode agent) "primary")
                                   (not (alist-get 'hidden agent))
                                   (alist-get 'id agent)))
                            agents))
         (index (or (seq-position primary id) 0)))
    (propertize name
                'face (list 'bold
                            (cond ((and (stringp color) (color-defined-p color))
                                   (list :foreground color))
                                  (opencode-mode-line-agent-faces
                                   (nth (mod index (length opencode-mode-line-agent-faces))
                                        opencode-mode-line-agent-faces))))
                'help-echo (format "Agent: %s" name))))

(defun opencode--mode-line-variant (provider model variant)
  "Return VARIANT for display, or MODEL's default under PROVIDER."
  (cond
   ((and variant (not (equal variant "default")))
    (propertize variant 'face 'success 'help-echo (format "Variant: %s (selected)" variant)))
   ((alist-get 'variants model)
    (let ((default (and opencode-default-variant-function
                        (funcall opencode-default-variant-function provider model))))
      (propertize (or default "default")
                  'face 'shadow
                  'help-echo (if default
                                 (format "Variant: %s (default)" default)
                               "Variant: provider default"))))))

(defun opencode--mode-line-provider (provider id)
  "Return the icon of PROVIDER with ID, or a generic icon and its name."
  (let ((name (or (alist-get 'name provider) id)))
    (propertize (or (cdr (seq-find (lambda (entry) (string-match-p (car entry) id))
                                   opencode-provider-icons))
                    (concat "\U000F048D " name)) ; nf-md-server_network
                'help-echo (format "Provider: %s (%s)" name id))))

(defun opencode--mode-line-context (model info)
  "Return the context use of MODEL, with catalog entry INFO, or nil when unknown."
  (let ((limit (alist-get 'context (alist-get 'limit info))))
    (when (and (numberp limit) (> limit 0) (numberp opencode-session-tokens))
      (let* ((used (/ (* 100.0 opencode-session-tokens) limit))
             (slice (min 7 (max 0 (1- (ceiling used 12.5)))))
             (tier (opencode-long-context-tier model info)))
        (propertize (concat (string (+ #xF0A9E slice)) ; nf-md-circle_slice_1..8
                            (format " %.0f%%" used)
                            (when (eq (car tier) 'over) (concat " " (nth 2 tier))))
                    'face (pcase (car tier) ('over 'error) ('near 'opencode-attention))
                    'help-echo (concat (format "Context: %dk/%dk tokens"
                                               (/ opencode-session-tokens 1000) (/ limit 1000))
                                       (when tier
                                         (concat "\n" (opencode-long-context-description tier)))))))))

(defun opencode--mode-line-compute-segments ()
  "Return the session details as an alist of (SEGMENT . STRING-OR-NIL)."
  (when opencode-session-agent
    (let* ((agents (opencode-catalog-ensure 'agent))
           (models (opencode-catalog-ensure 'model))
           (providers (opencode-catalog-ensure 'provider))
           (model opencode-session-model)
           (provider-id (alist-get 'providerID model))
           (info (seq-find (lambda (entry)
                             (and (equal (alist-get 'id entry) (alist-get 'id model))
                                  (equal (alist-get 'providerID entry) provider-id)))
                           models))
           (provider (seq-find (lambda (entry) (equal (alist-get 'id entry) provider-id))
                               providers))
           (name (or (alist-get 'name info) (alist-get 'id model))))
      `((agent . ,(opencode--mode-line-agent opencode-session-agent agents))
        (model . ,name)
        (short-model . ,(and name (replace-regexp-in-string "\\`Claude " "" name)))
        (variant . ,(opencode--mode-line-variant provider info (alist-get 'variant model)))
        (provider . ,(and provider-id (opencode--mode-line-provider provider provider-id)))
        (context . ,(opencode--mode-line-context model info))))))

(defvar-local opencode--mode-line-segments nil
  "(KEY . SEGMENTS) of the session details last computed.")

(defun opencode-mode-line-segments ()
  "Return the session details as an alist of (SEGMENT . STRING-OR-NIL).
SEGMENT is one of `agent', `model', `short-model', `variant', `provider'
and `context'; `short-model' drops a leading \"Claude \".  They are
computed again only when the session or a catalog changed, as the mode
line is redrawn on every streamed chunk."
  (let ((key (list opencode-session-agent opencode-session-model opencode-session-tokens
                   opencode-catalog-generation default-directory opencode-provider-icons
                   opencode-mode-line-agent-faces opencode-default-variant-function
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

(defun opencode-mode-line ()
  "Return the session status, title and details fitted to the selected window."
  (let* ((status (opencode-mode-line-status))
         (title (opencode-mode-line-title))
         (segments (opencode-mode-line-segments))
         (char-width (window-font-width nil 'mode-line))
         (rest (format-mode-line (remove opencode-mode-line-element (opencode-mode-line-format)) t))
         (tab (format-mode-line (opencode-mode-line-tab (concat status " ")) t))
         (available (- (window-pixel-width)
                       (window-right-divider-width)
                       (window-scroll-bar-width)
                       (opencode-mode-line-pixel-width rest)
                       (opencode-mode-line-pixel-width tab)
                       char-width))
         (candidates
          (delete-dups
           (mapcar (lambda (level)
                     (let ((details (concat "  " (opencode-mode-line-details segments level) " ")))
                       (cons details (opencode-mode-line-pixel-width details))))
                   opencode-mode-line-levels)))
         (fit (opencode-mode-line-fit (string-width title) candidates available char-width
                                      opencode-mode-line-title-min-width)))
    (list (opencode-mode-line-tab
           (opencode-mode-line-escape
            (concat status " "
                    (opencode--mode-line-title-string
                     (truncate-string-to-width title (cdr fit) nil nil "…")))))
          (opencode-mode-line-escape (car fit)))))

;;;; Format

(defvar opencode--mode-line-format-warned nil
  "Non-nil once the default mode line lacked the expected anchors.")

(defun opencode-mode-line-format ()
  "Return the default mode-line format adapted to session buffers.
Everything up to the buffer identification is kept but the modified and
remote flags; the identification and the slots before `mode-line-modes'
give way to `opencode-mode-line-element', and `mode-line-modes' loses
the major mode name and `mode-line-process'.  When the default lacks
those anchors it is used unchanged."
  (let* ((default (default-value 'mode-line-format))
         (id (seq-position default nil
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
