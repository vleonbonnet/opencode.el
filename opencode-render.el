;;; opencode-render.el --- Transcript output primitives  -*- lexical-binding: t; -*-

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

;; A session buffer is a Comint buffer whose process is a pipe that
;; never runs anything: input goes over HTTP, and the transcript is
;; written at the process mark as Comint output, so prompt navigation,
;; input history and output commands work as in any REPL.
;;
;; The transcript is Markdown fontified in place by markdown-ts.
;; Font-lock owns `face', `display' and `invisible' there, so output that
;; must look a certain way whatever its Markdown reading carries an
;; `opencode-fontify' property instead, reapplied after every
;; fontification: `verbatim' text drops the Markdown faces and hidden
;; markup (block summaries, rendered tables), `hidden' text displays as
;; nothing (Org link brackets), and `link' text gets the link face on
;; top.  markdown-ts also turns links it parses into buttons, overwriting
;; any action already there, so buttons made with `opencode-make-button'
;; keep their properties in `opencode-button', reasserted by that pass.

;;; Code:

(require 'comint)
(require 'markdown-ts-mode)
(require 'opencode-common)
(require 'treesit)

(defcustom opencode-ensure-grammar-function #'opencode-require-grammar
  "Function making a tree-sitter grammar available, called with its language.
Session buffers need the `markdown' and `markdown-inline' grammars; the
default signals an error when one is missing.  A function that installs
or builds grammars lets the first session on a fresh machine set them up."
  :type 'function
  :group 'opencode)

(defcustom opencode-render-table-function #'opencode-align-markdown-table
  "Function rendering the completed Markdown table between BEG and END.
It replaces the region in the session buffer and must keep the Comint
output properties, as `opencode-output-properties' returns them, on
whatever it inserts.  The default aligns the table's pipes with
markdown-ts and keeps its markup."
  :type 'function
  :group 'opencode)

(defcustom opencode-rerender-tables-function nil
  "Function reflowing rendered tables after a window resize, or nil."
  :type '(choice (const :tag "Never reflow" nil) function)
  :group 'opencode)

(defcustom opencode-remap-headings nil
  "When non-nil, show Markdown headings of session buffers in `opencode-heading'."
  :type 'boolean
  :group 'opencode)

(defface opencode-heading
  '((t :inherit bold))
  "Markdown headings of session buffers, when `opencode-remap-headings' is set."
  :group 'opencode-faces)

(defvar-local opencode-last-response-start nil
  "Marker at the start of the latest assistant text block.")

(defvar-local opencode--table-scan-marker nil
  "Marker at the Markdown of the current text block not yet scanned for tables.")

(defvar-local opencode--last-block-kind nil
  "Kind of the last block written: `text', `reasoning', `tool', `notice' or `user'.")

;;;; The end of the transcript

(defun opencode-pmark ()
  "Return the process mark position of this session buffer."
  (marker-position (process-mark (get-buffer-process (current-buffer)))))

(defvar-local opencode--prompt-start nil
  "Marker at the start of the last input prompt written.
Comint's `comint-last-prompt' cannot tell: it covers whatever line of
output came last, prompt or not.")

(defun opencode-prompt-shown-p ()
  "Return non-nil when the transcript ends with an input prompt."
  (and (markerp opencode--prompt-start)
       (marker-buffer opencode--prompt-start)
       (= (+ (marker-position opencode--prompt-start) 2) (opencode-pmark))))

;;;; Points following output

;; Comint keeps the points at the process mark after the output it
;; writes: the buffer's point through a marker that advances, window
;; points through `window-point-insertion-type' and
;; `comint-postoutput-scroll-to-bottom'.  Output written in other ways,
;; such as an expansion at the end, a prompt written before typed input
;; or a table aligned in place, keeps them after it with
;; `opencode-keeping-followers'.

(defvar opencode--keeping-followers nil
  "Session buffers whose following points an outer call is keeping.
That call saw them before anything changed, so inner calls leave them.")

(defun opencode--follow-start ()
  "Return where the positions following output start in this buffer.
They are at the end of the transcript or in the input: from the start
of the idle prompt ending the transcript, which shows as nothing, else
from the process mark.  Without a process, only the end follows."
  (save-restriction
    (widen)
    (cond ((not (get-buffer-process (current-buffer))) (point-max))
          ((opencode-prompt-shown-p) (marker-position opencode--prompt-start))
          (t (opencode-pmark)))))

(defun opencode-follows-output-p (position)
  "Return non-nil when POSITION is at the end of the transcript or in the input.
A point there stays after the output written at the end, at its place
in the input."
  (>= position (opencode--follow-start)))

(defun opencode--followers ()
  "Return this buffer's point and window points that follow output.
The value lists (WHO . DISTANCE): WHO is t for the buffer's point, else
a window showing the buffer, and DISTANCE counts from the end of the
buffer, so changing the transcript before it leaves DISTANCE as it is."
  (save-restriction
    (widen)
    (let ((start (opencode--follow-start))
          (followers nil))
      (when (>= (point) start)
        (push (cons t (- (point-max) (point))) followers))
      (dolist (window (get-buffer-window-list nil nil t))
        (when (>= (window-point window) start)
          (push (cons window (- (point-max) (window-point window))) followers)))
      followers)))

(defun opencode--restore-followers (followers)
  "Put FOLLOWERS, as `opencode--followers' returned them, back at their distance."
  (save-restriction
    (widen)
    (pcase-dolist (`(,who . ,distance) followers)
      (let ((position (max (point-min) (- (point-max) distance))))
        (cond ((eq who t) (goto-char position))
              ((and (window-live-p who) (eq (window-buffer who) (current-buffer)))
               (set-window-point who position)))))))

(defun opencode--call-keeping-followers (function)
  "Call FUNCTION, keeping this buffer's points that follow output after it.
See `opencode-keeping-followers'."
  (let ((buffer (current-buffer)))
    (if (memq buffer opencode--keeping-followers)
        (funcall function)
      (let ((followers (opencode--followers))
            (opencode--keeping-followers (cons buffer opencode--keeping-followers)))
        (unwind-protect (funcall function)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (opencode--restore-followers followers))))))))

(defmacro opencode-keeping-followers (&rest body)
  "Run BODY, keeping the points that follow output after what it writes.
The buffer's point and the window points at the end of the transcript
or in the input keep their distance from the end of the buffer, so BODY
may write, replace or delete transcript text, the process mark
included, as long as it leaves the input alone.  Other points are left
to their markers.  Nested in another one for the same buffer, it leaves
them to the outer one, which saw them before anything changed."
  (declare (indent 0) (debug t))
  `(opencode--call-keeping-followers (lambda () ,@body)))

(defun opencode--line-place (position)
  "Return POSITION as (LINE . OFFSET): its line, and how far into it it is."
  (save-excursion
    (goto-char position)
    (cons (line-number-at-pos position t) (- position (pos-bol)))))

(defun opencode--line-position (place)
  "Return the position of PLACE, a (LINE . OFFSET), within the end of its line."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (car place)))
    (min (+ (point) (cdr place)) (pos-eol))))

(defun opencode-save-viewport ()
  "Return what puts this buffer's point and windows back once it is rendered again.
Restore it with `opencode-restore-viewport'.  Markers do not survive
the text they are in, so places are numbers: the points that follow
output (see `opencode-follows-output-p') keep their distance from the
end of the buffer, where the typed input is put back, and other points
their line and their offset in it, their windows the line they start
at."
  (save-restriction
    (widen)
    (let ((follow (opencode--follow-start)))
      (cl-flet ((place (position)
                  (if (>= position follow)
                      (list 'end (- (point-max) position))
                    (list 'line (opencode--line-place position)))))
        (cons (list t (place (point)))
              (mapcar (lambda (window)
                        (list window (place (window-point window))
                              (opencode--line-place (window-start window))))
                      (get-buffer-window-list nil nil t)))))))

(defun opencode-restore-viewport (viewport)
  "Put this buffer's point and windows back as `opencode-save-viewport' saved VIEWPORT.
A window whose point follows output keeps the start the new text gave
it, which redisplay moves as it needs to show the point."
  (save-restriction
    (widen)
    (cl-flet ((position (place)
                (pcase place
                  (`(end ,distance) (max (point-min) (- (point-max) distance)))
                  (`(line ,line) (opencode--line-position line)))))
      (pcase-dolist (`(,who ,place ,start) viewport)
        (cond ((eq who t) (goto-char (position place)))
              ((and (window-live-p who) (eq (window-buffer who) (current-buffer)))
               (unless (eq (car place) 'end)
                 (set-window-start who (opencode--line-position start) t))
               (set-window-point who (position place))))))))

;;;; Output

(defun opencode-output (string)
  "Write STRING at the process mark as Comint output."
  (let ((start (save-excursion
                 (goto-char (opencode-pmark))
                 (let ((inhibit-field-text-motion t))
                   (line-beginning-position)))))
    (comint-output-filter (get-buffer-process (current-buffer)) string)
    (opencode-buttonize start (opencode-pmark))))

(defun opencode-insert-output-at (position string)
  "Insert STRING at POSITION as Comint output and return its end.
Unlike `opencode-output', this writes inside an already rendered
transcript, for expanded blocks.  Markers exactly at POSITION that
track the stream (the process mark, the response start and the table
scan marker) move past STRING, so later output stays after it, and so
do the points that follow output (see `opencode-keeping-followers')."
  (let* ((process (get-buffer-process (current-buffer)))
         (markers (seq-filter
                   (lambda (marker)
                     (and (markerp marker)
                          (eq (marker-buffer marker) (current-buffer))
                          (= position (marker-position marker))))
                   (list (and process (process-mark process))
                         opencode-last-response-start
                         opencode--table-scan-marker)))
         (inhibit-read-only t)
         end)
    (opencode-keeping-followers
      (save-excursion
        (goto-char position)
        (insert string)
        (setq end (point))
        (comint--mark-as-output position end))
      (dolist (marker markers)
        (set-marker marker end)))
    (opencode-buttonize position end t)
    end))

(defun opencode-set-last-response-start (position)
  "Record POSITION as the start of the latest assistant text block."
  (when (markerp opencode-last-response-start)
    (set-marker opencode-last-response-start nil))
  (setq opencode-last-response-start (copy-marker position)))

(defun opencode-open-block (kind)
  "Separate the block of KIND about to be written from the previous one.
Adjacent compact blocks (tools and reasoning) are kept on consecutive
lines so a run of them reads as one group; every other pair gets one
blank line.  Spacing is measured at the process mark and only added."
  (unless (= (point-max) (point-min))
    (let* ((compact '(tool reasoning))
           (want (if (and (memq kind compact) (memq opencode--last-block-kind compact)) 1 2))
           (newlines 0))
      (save-excursion
        (goto-char (opencode-pmark))
        (while (and (> (point) (point-min)) (eq (char-before) ?\n))
          (setq newlines (1+ newlines))
          (backward-char 1)))
      (when (< newlines want)
        (opencode-output (make-string (- want newlines) ?\n)))))
  (setq opencode--last-block-kind kind))

(defun opencode-margin (face)
  "Return the `left-margin' display string drawn in FACE."
  (propertize ">" 'display `((margin left-margin) ,(propertize "▎" 'face face))))

(defun opencode-add-margin (start end face)
  "Draw a FACE margin from START up to, excluding, END."
  (let ((overlay (make-overlay start (max start (1- end))))
        (margin (opencode-margin face)))
    (overlay-put overlay 'line-prefix margin)
    (overlay-put overlay 'wrap-prefix margin)
    overlay))

(defun opencode-lift-prompt ()
  "Remove the input prompt ending the transcript; return non-nil if there was one.
Output written while a session is idle belongs before its prompt."
  (when (opencode-prompt-shown-p)
    (let ((start (marker-position opencode--prompt-start))
          (inhibit-read-only t))
      (remove-overlays start (opencode-pmark))
      (delete-region start (opencode-pmark))
      (set-marker (process-mark (get-buffer-process (current-buffer))) start)
      (set-marker opencode--prompt-start nil)
      (setq opencode--prompt-start nil)
      t)))

(defmacro opencode-before-prompt (&rest body)
  "Run BODY writing output before the input prompt, which is shown again after."
  (declare (indent 0) (debug t))
  (let ((lifted (make-symbol "lifted")))
    `(let ((,lifted (opencode-lift-prompt)))
       (unwind-protect (progn ,@body)
         (when ,lifted
           (opencode-open-block 'user)
           (opencode-show-prompt))))))

(defun opencode-show-prompt ()
  "Write the hidden input prompt and its margin after the transcript.
Nothing is written when the transcript already ends with one."
  (unless (opencode-prompt-shown-p)
    (let ((start (opencode-pmark)))
      (opencode-output "> ")
      (overlay-put (make-overlay start (opencode-pmark)) 'invisible t)
      (opencode-add-margin start (opencode-pmark) 'opencode-request-margin-highlight)
      (setq opencode--prompt-start (copy-marker start)))))

(defun opencode-highlight-input (&optional _process _string)
  "Margin and mark the input just sent or replayed as prompt text."
  (opencode-add-margin comint-last-input-start comint-last-input-end
                       'opencode-request-margin-highlight)
  (with-silent-modifications
    (put-text-property comint-last-input-start comint-last-input-end 'opencode-prompt t)))

(defun opencode-insert-block-with-margin (text face)
  "Write TEXT with a FACE margin; return the start of TEXT."
  (unless (string-empty-p text)
    (let ((beginning (opencode-pmark)))
      (opencode-output text)
      (opencode-add-margin beginning
                           (save-excursion
                             (goto-char (opencode-pmark))
                             (skip-chars-backward "\r\n[:blank:]")
                             (point))
                           face)
      beginning)))

(defun opencode-call-preserving-viewport (buffer function)
  "Call FUNCTION in BUFFER without moving its point or any window's viewport.
A point in the transcript stays at its text, and its window keeps its
start.  A point at the end of the transcript or in the input follows
the output FUNCTION writes, as Comint's points do: see
`opencode-keeping-followers'."
  (with-current-buffer buffer
    (let* ((follow (opencode--follow-start))
           (point-marker (and (< (point) follow) (copy-marker (point))))
           (windows (delq nil (mapcar (lambda (window)
                                        (when (< (window-point window) follow)
                                          (list window
                                                (copy-marker (window-start window))
                                                (copy-marker (window-point window)))))
                                      (get-buffer-window-list buffer nil t)))))
      (unwind-protect
          (opencode--call-keeping-followers function)
        (when point-marker
          (goto-char point-marker)
          (set-marker point-marker nil))
        (pcase-dolist (`(,window ,start ,window-point) windows)
          (when (and (window-live-p window) (eq (window-buffer window) buffer))
            (set-window-start window start t)
            (set-window-point window window-point))
          (set-marker start nil)
          (set-marker window-point nil))))))

(defun opencode-output-notice (text &optional face)
  "Append TEXT as a notice line in FACE without moving any viewport."
  (when (get-buffer-process (current-buffer))
    (opencode-call-preserving-viewport
     (current-buffer)
     (lambda ()
       (opencode-before-prompt
         (opencode-open-block 'notice)
         (opencode-output (opencode-fontify-as
                           (propertize text 'font-lock-face (or face 'opencode-notice))
                           'verbatim))
         (opencode-output "\n"))))))

;;;; Properties surviving fontification

(defun opencode-fontify-as (string kind)
  "Return STRING marked to show as KIND whatever its Markdown reading."
  (propertize string 'opencode-fontify kind))

(defun opencode-fontify-kinds (position)
  "Return the list of `opencode-fontify' kinds marked at POSITION."
  (ensure-list (get-text-property position 'opencode-fontify)))

(defun opencode-apply-fontify-properties (start end &optional _loudly)
  "Reassert `opencode-fontify' effects and durable buttons between START and END.
Text may be marked with several kinds, such as a link in verbatim text:
verbatim and hidden text first lose what Markdown gave them, then links
get their face."
  (with-silent-modifications
    (let ((pos start))
      (while (< pos end)
        (let ((next (next-single-property-change pos 'opencode-fontify nil end))
              (kinds (opencode-fontify-kinds pos)))
          (when (memq 'verbatim kinds)
            (remove-text-properties pos next '(face nil invisible nil display nil button nil
                                                    category nil action nil)))
          (when (memq 'hidden kinds)
            (remove-text-properties pos next '(face nil invisible nil button nil category nil
                                                    action nil help-echo nil mouse-face nil))
            (put-text-property pos next 'display ""))
          (when (memq 'link kinds)
            (add-face-text-property pos next 'link))
          (setq pos next))))
    (let ((pos start))
      (while (< pos end)
        (let ((next (next-single-property-change pos 'opencode-button nil end)))
          (when-let* ((properties (get-text-property pos 'opencode-button)))
            (add-text-properties pos next properties))
          (setq pos next))))))

(defun opencode-mark-fontify (start end kind &optional add)
  "Mark START..END to show as KIND, and apply it at once.
With ADD, KIND joins the kinds already marked there instead of
replacing them, as a link does in verbatim text."
  (with-silent-modifications
    (if (not add)
        (put-text-property start end 'opencode-fontify kind)
      (let ((pos start))
        (while (< pos end)
          (let ((next (next-single-property-change pos 'opencode-fontify nil end))
                (kinds (opencode-fontify-kinds pos)))
            (unless (memq kind kinds)
              (put-text-property pos next 'opencode-fontify
                                 (if kinds (append kinds (list kind)) kind)))
            (setq pos next))))))
  (opencode-apply-fontify-properties start end))

(defun opencode-make-button (start end &rest properties)
  "Make a text button from START to END with PROPERTIES surviving fontification.
The button's category, which gives it its keys, survives too: verbatim
text drops the buttons Markdown would make there."
  (apply #'make-text-button start end properties)
  (with-silent-modifications
    (put-text-property start end 'opencode-button
                       (append (list 'button (get-text-property start 'button)
                                     'category (get-text-property start 'category))
                               properties))))

(defvar opencode-buttonize-functions nil
  "Functions turning references between START and END into buttons.
Run over every chunk of output, from the start of its first line, so a
reference is found once its closing delimiter arrives.  Each function
must skip references it already buttonized.  A reference without a
closing delimiter, such as a bare URL, ends at END only when
`opencode-buttonize-complete' is non-nil.")

(defvar opencode-buttonize-complete nil
  "Non-nil while the output being buttonized is complete.
The text at the end of streamed output may still grow, so a reference
that ends there without a closing delimiter is left until more output
follows it or the block writing it ends.")

(defun opencode-buttonize (start end &optional complete)
  "Run `opencode-buttonize-functions' between START and END.
COMPLETE means nothing written later continues the text before END."
  (let ((inhibit-read-only t)
        (opencode-buttonize-complete complete))
    (run-hook-with-args 'opencode-buttonize-functions start end)))

(defun opencode-buttonize-output-end ()
  "Buttonize the last line of the output as complete.
Called when the block writing at the process mark ended, so that a
reference ending the output, which more text might have continued,
becomes a button."
  (let ((pmark (opencode-pmark)))
    (opencode-buttonize (save-excursion
                          (goto-char pmark)
                          (let ((inhibit-field-text-motion t))
                            (line-beginning-position)))
                        pmark t)))

;;;; Fontification

(defun opencode--fontify-code-block (mode beg end)
  "Return the MODE faces of BEG..END, collected in an isolated buffer.
markdown-ts fontifies code blocks in an indirect buffer sharing every
session property, where mode setup and cleanup disturb the transcript."
  (let ((text (buffer-substring-no-properties beg end))
        faces)
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (funcall mode))
      (font-lock-ensure)
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let ((next (next-single-property-change pos 'face nil (point-max)))
                (face (get-text-property pos 'face)))
            (when face
              (push (list (+ beg (1- pos)) (+ beg (1- next)) face) faces))
            (setq pos next)))))
    (nreverse faces)))

(defun opencode--prompt-face-region (start end)
  "Show START..END in `opencode-prompt' instead of the quote face."
  (let ((pos start))
    (while (< pos end)
      (let* ((next (next-single-property-change pos 'face nil end))
             (face (get-text-property pos 'face))
             (faces (remq 'markdown-ts-block-quote
                          (if (and (consp face) (not (keywordp (car face))))
                              face
                            (and face (list face))))))
        (unless (memq 'opencode-prompt faces)
          (setq faces (append faces '(opencode-prompt))))
        (put-text-property pos next 'face faces)
        (setq pos next)))))

(defun opencode--fontify-prompts (start end)
  "Show prompt text between START and END in `opencode-prompt'.
Every prompt follows the hidden \"> \", so markdown-ts reads it as a
block quote; answers keep their quote face.  The input being typed
shows its Markdown as typed: none of it is hidden or displayed as
something else."
  (let* ((process (get-buffer-process (current-buffer)))
         (pmark (and process (marker-position (process-mark process)))))
    (with-silent-modifications
      (let ((pos start))
        (while (< pos end)
          (let ((next (next-single-property-change pos 'opencode-prompt nil end)))
            (when (and pmark (< pos pmark next))
              (setq next pmark))
            (when (and pmark (>= pos pmark))
              (remove-list-of-text-properties pos next '(invisible display)))
            (when (or (get-text-property pos 'opencode-prompt)
                      (and pmark (>= pos pmark)))
              (opencode--prompt-face-region pos next))
            (setq pos next)))))))

(defun opencode-fontify-markdown (start end &optional loudly)
  "Fontify START..END as Markdown, LOUDLY, then reassert session properties."
  (let ((collect (symbol-function 'markdown-ts--fontify-non-ts-collect-faces)))
    (cl-letf (((symbol-function 'markdown-ts--fontify-non-ts-collect-faces)
               (lambda (mode beg finish)
                 (if (memq mode markdown-ts-code-block-force-conventional-modes)
                     (funcall collect mode beg finish)
                   (opencode--fontify-code-block mode beg finish)))))
      (treesit-font-lock-fontify-region start end loudly)))
  (opencode-apply-fontify-properties start end)
  (opencode--fontify-prompts start end))

(defun opencode-require-grammar (language)
  "Signal an error unless the tree-sitter grammar of LANGUAGE is available."
  (unless (treesit-language-available-p language)
    (error "OpenCode sessions need the %s tree-sitter grammar" language)))

(defun opencode-render-setup ()
  "Set up Markdown display and prompt protection in this session buffer."
  (dolist (language '(markdown markdown-inline))
    (funcall opencode-ensure-grammar-function language))
  (setq-local markdown-ts-hide-markup t
              markdown-ts-enable-code-block-context-mode nil
              markdown-ts-enable-table-mode nil)
  (markdown-ts-mode--initialize)
  (setq-local font-lock-defaults
              '(nil nil nil nil
                    (font-lock-fontify-syntactically-function . opencode-fontify-markdown)))
  (setq-local font-lock-fontify-syntactically-function #'opencode-fontify-markdown)
  ;; Buttons are made from output, not by font-lock.
  (setq-local font-lock-extra-managed-props
              (cl-set-difference font-lock-extra-managed-props
                                 '(button category action help-echo)))
  ;; markdown-ts links bare URLs at redisplay, in the input too, and opens
  ;; them as mail; opencode-links makes them from output instead.
  (jit-lock-unregister 'markdown-ts--fontify-bare-uri)
  ;; Half-typed input can wrap the whole transcript in one error node.
  ;; The settings are shared between markdown buffers, so copy them first.
  (setq-local treesit-font-lock-settings (mapcar #'copy-sequence treesit-font-lock-settings))
  (treesit-font-lock-recompute-features nil '(error))
  ;; Typed text inherits from the text before it, and redisplay never
  ;; fontifies hidden text, so text typed after hidden markup would stay hidden.
  (setq-local text-property-default-nonsticky
              (append '((invisible . t) (display . t)) text-property-default-nonsticky))
  (setq-local comint-prompt-read-only t)
  (when opencode-remap-headings
    (dolist (face '(markdown-ts-heading-1 markdown-ts-heading-2 markdown-ts-heading-3
                    markdown-ts-heading-4 markdown-ts-heading-5 markdown-ts-heading-6
                    markdown-ts-setext-heading))
      (face-remap-add-relative face 'opencode-heading)))
  (opencode-undo-setup)
  (add-hook 'window-size-change-functions #'opencode--rerender-tables nil t))

;;;; Undo of the prompt only

(defvar-local opencode--undo-snapshot nil
  "List holding the undo list from before the output change in progress.")

(defun opencode--output-change-p (beg end)
  "Return non-nil when changing BEG..END rewrites output rather than the prompt.
Output is before the process mark, or inserted at it with
`inhibit-read-only', which every writer binds and typing never does."
  (when-let* ((process (get-buffer-process (current-buffer)))
              (pmark (marker-position (process-mark process))))
    (or (< beg pmark)
        (and inhibit-read-only (= beg end pmark)))))

(defun opencode--undo-shift (list from delta seen)
  "Shift positions at or after FROM by DELTA in undo LIST, in place.
SEEN holds the cells and records already shifted, which LIST may share."
  (cl-flet ((move (pos) (if (>= pos from) (+ pos delta) pos)))
    (while (and (consp list) (not (gethash list seen)))
      (puthash list t seen)
      (let ((elt (car list)))
        (cond
         ((integerp elt) (setcar list (move elt)))
         ((or (atom elt) (gethash elt seen)))
         (t
          (puthash elt t seen)
          (pcase elt
            (`(,(and (pred integerp) beg) . ,(and (pred integerp) end))
             (setcar elt (move beg))
             (setcdr elt (move end)))
            (`(,(pred stringp) . ,(and (pred integerp) pos))
             (setcdr elt (if (< pos 0) (- (move (- pos))) (move pos))))
            (`(nil ,_ ,_ ,(pred integerp) . ,(pred integerp))
             (let ((range (nthcdr 3 elt)))
               (setcar range (move (car range)))
               (setcdr range (move (cdr range)))))
            (`(apply ,(pred integerp) ,(pred integerp) ,(pred integerp) . ,_)
             (let ((range (nthcdr 2 elt)))
               (setcar range (move (car range)))
               (setcar (cdr range) (move (cadr range)))))))))
      (setq list (cdr list)))))

(defun opencode--undo-break-stale-run (seen)
  "End this buffer's undo run when its pending records escaped SEEN."
  (let ((head buffer-undo-list))
    (while (and (consp head) (null (car head)))
      (setq head (cdr head)))
    (let ((pending (gethash head undo-equiv-table)))
      (when (or (eq pending 'undo-in-region)
                (and (consp pending) (not (gethash pending seen))))
        (remhash head undo-equiv-table)))))

(defun opencode--undo-before-change (beg end)
  "Remember the undo list when BEG..END is about to change output."
  (setq opencode--undo-snapshot
        (and (listp buffer-undo-list)
             (opencode--output-change-p beg end)
             (list buffer-undo-list))))

(defun opencode--undo-after-change (beg end old-len)
  "Drop the records of the output change at BEG..END of OLD-LEN, shifting the prompt's."
  (when-let* ((snapshot opencode--undo-snapshot))
    (setq opencode--undo-snapshot nil
          buffer-undo-list (car snapshot))
    (let ((delta (- end beg old-len))
          (seen (make-hash-table :test #'eq)))
      (unless (zerop delta)
        (opencode--undo-shift buffer-undo-list (+ beg old-len) delta seen)
        (opencode--undo-break-stale-run seen)))))

(defun opencode--undo-forget-sent-input (&rest _)
  "Drop the undo history of a prompt that became part of the conversation."
  (when (listp buffer-undo-list)
    (setq buffer-undo-list nil)))

(defun opencode-undo-setup ()
  "Keep only prompt edits in this buffer's undo list.
The transcript is rewritten constantly (every streamed delta replaces a
summary), so recording output made undo walk into it and break block
overlays."
  (add-hook 'before-change-functions #'opencode--undo-before-change nil t)
  (add-hook 'after-change-functions #'opencode--undo-after-change nil t)
  (add-hook 'comint-input-filter-functions #'opencode--undo-forget-sent-input nil t))

;;;; Tables

(defun opencode-output-properties (position)
  "Return the Comint output properties at POSITION as a plist."
  (cl-loop for property in '(field read-only insert-in-front-hooks
                                   inhibit-line-move-field-capture front-sticky)
           for value = (get-text-property position property)
           when value append (list property value)))

(defun opencode-align-markdown-table (beg end)
  "Align the completed Markdown table between BEG and END in place.
markdown-ts aligns it in an isolated buffer and only the changed text is
applied back, so properties and markers on unchanged text survive."
  (let ((source (current-buffer))
        (text (buffer-substring-no-properties beg end))
        (output-properties (opencode-output-properties beg))
        (inhibit-read-only t))
    (save-excursion
      (with-temp-buffer
        ;; Context modes start timers that would run in the session later.
        (let ((markdown-ts-enable-table-mode nil)
              (markdown-ts-enable-code-block-context-mode nil))
          (delay-mode-hooks (markdown-ts-mode)))
        (insert text)
        (goto-char (point-min))
        (markdown-ts-in-table-mode 1)
        (markdown-ts-table-align-table t)
        ;; Alignment always ends with a newline; keep an unterminated table so.
        (unless (string-suffix-p "\n" text)
          (when (eq (char-before (point-max)) ?\n)
            (delete-region (1- (point-max)) (point-max))))
        (let ((aligned (current-buffer)))
          (with-current-buffer source
            (save-restriction
              (narrow-to-region beg end)
              (replace-region-contents (point-min) (point-max) aligned)
              ;; New padding also needs Comint's output field and protection.
              (when output-properties
                (let ((pos (point-min)))
                  (while (< pos (point-max))
                    (let ((next (next-single-property-change pos 'field nil (point-max))))
                      (unless (get-text-property pos 'field)
                        (add-text-properties pos next output-properties))
                      (setq pos next)))))
              (opencode-buttonize (point-min) (point-max) t))))))))

(defun opencode-align-finished-tables (&optional finished)
  "Render the tables of the current text block whose end has arrived.
A table is complete once a later complete line follows it; FINISHED
means the block ended, so a table at its end is complete too.  One line
of context is kept for a header whose delimiter row has yet to arrive.
A table ending the output may grow at the process mark, which then
moves past it with the points following output."
  (when opencode--table-scan-marker
    (opencode-keeping-followers
      (save-excursion
        (save-restriction
          ;; Pending input is not part of the assistant's Markdown.
          (narrow-to-region (point-min) (opencode-pmark))
          (let* ((inhibit-field-text-motion t)
                 (limit (copy-marker (opencode-pmark)))
                 (complete-end (copy-marker (if finished
                                                limit
                                              (goto-char limit)
                                              (line-beginning-position))))
                 waiting)
            (unwind-protect
                (progn
                  (goto-char opencode--table-scan-marker)
                  (while (and (< (point) complete-end) (not waiting))
                    (let ((node (treesit-node-at (point) 'markdown)))
                      (while (and node (not (equal (treesit-node-type node) "pipe_table")))
                        (setq node (treesit-node-parent node)))
                      ;; At whitespace the node can be the table before point.
                      (if (and node (< (point) (treesit-node-end node)))
                          (let ((beg (treesit-node-start node))
                                (end (treesit-node-end node)))
                            (if (and (<= end limit)
                                     (or finished
                                         (< (save-excursion
                                              (goto-char end)
                                              (if (bolp) (point) (line-beginning-position 2)))
                                            complete-end)))
                                (let ((next (copy-marker end t))
                                      (output-end (= end (opencode-pmark))))
                                  (unwind-protect
                                      (progn
                                        (funcall opencode-render-table-function beg end)
                                        ;; What aligning appends to a table ending the output is output too.
                                        (when output-end
                                          (set-marker (process-mark (get-buffer-process (current-buffer))) next))
                                        (goto-char next)
                                        (set-marker opencode--table-scan-marker next))
                                    (set-marker next nil)))
                              (set-marker opencode--table-scan-marker beg)
                              (setq waiting t)))
                        (set-marker opencode--table-scan-marker (point))
                        (forward-line 1))))
                  (when finished
                    (set-marker opencode--table-scan-marker nil)
                    (setq opencode--table-scan-marker nil)))
              (set-marker limit nil)
              (set-marker complete-end nil))))))))

(defun opencode--rerender-tables (&rest _)
  "Reflow rendered tables after a window resize, keeping the stream markers.
Markers are restored by their distance from the end of the buffer, which
a change before them does not alter, and so are the points following
output."
  (when opencode-rerender-tables-function
    (let* ((process (get-buffer-process (current-buffer)))
           (markers (delq nil (list (and process (process-mark process))
                                    opencode--table-scan-marker)))
           (distances (mapcar (lambda (marker) (- (point-max) marker)) markers)))
      (opencode-keeping-followers
        (funcall opencode-rerender-tables-function))
      (cl-mapc (lambda (marker distance) (set-marker marker (- (point-max) distance)))
               markers distances))))

(provide 'opencode-render)
;;; opencode-render.el ends here
