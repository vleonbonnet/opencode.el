;;; opencode-blocks.el --- Collapsible reasoning and tool blocks  -*- lexical-binding: t; -*-

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

;; Reasoning and tool calls are mostly output nobody reads (file reads,
;; thinking, command output), and fontifying all of it buries the answer.
;; Each is shown as one summary line instead, expanded on demand:
;;
;; - A reasoning block shows its size and a one-line preview, updated on
;;   every streamed delta.  Anthropic can return a block only as a
;;   signature; its undocumented header names the kind, so such blocks
;;   read "thinking hidden" or, in the attention face, "narration
;;   hidden".  Delivered narration is text meant for the user and is
;;   shown as prose once its signature arrives.
;; - A tool block shows an icon, a boxed kind and a description, colored
;;   by status: blue while running, orange while waiting on the user,
;;   the error face on failure.  Formatting a diff or a patch waits for
;;   expansion.
;;
;; The arrow is an overlay `before-string', never buffer text: text on an
;; overlay boundary drifts out from under it when edited.  Summaries are
;; `verbatim' so a command or path is never read as Markdown.  Each
;; window shows a summary cut to its own width (see the fitting section).

;;; Code:

(require 'diff)
(require 'diff-mode)
(require 'opencode-common)
(require 'opencode-render)

(defcustom opencode-tool-summary-max-width 500
  "Most characters kept in a collapsed summary line.
Only an upper bound on the text; each window shows what fits."
  :type 'integer
  :group 'opencode)

(defcustom opencode-question-summary-width 100
  "Characters an answered question summary aims for, answers included."
  :type 'integer
  :group 'opencode)

(defcustom opencode-tool-summary-functions nil
  "Alist of tool names and functions describing their calls.
Each function receives the call input and returns the description shown
after the tool's boxed label, or nil for the default description."
  :type '(alist :key-type string :value-type function)
  :group 'opencode)

(defcustom opencode-show-tool-output nil
  "When non-nil, write the output of finished tools below their summary."
  :type 'boolean
  :group 'opencode)

(defvar-local opencode--blocks nil
  "Hash table of block keys to their summary overlays.
Reasoning keys are \"MESSAGE/r/ORDINAL\", tool keys are call IDs.")

(defvar opencode-blocks-attach-functions nil
  "Functions called with each newly inserted tool block overlay.
Pending permissions and questions use it to mark the block they belong to.")

(defvar opencode-blocks-finished-functions nil
  "Functions called with a tool block overlay once its call has finished.")

(defvar opencode-collapsible-keymap
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'opencode-toggle-block)
    (keymap-set map "<mouse-1>" #'opencode-toggle-block)
    (keymap-set map "TAB" #'opencode-toggle-block)
    map)
  "Keymap of collapsible summary lines.")

(defun opencode-blocks-ensure ()
  "Initialize the block table of this buffer."
  (unless (hash-table-p opencode--blocks)
    (setq opencode--blocks (make-hash-table :test #'equal))))

(defun opencode-block (key)
  "Return the live summary overlay of block KEY, or nil."
  (when-let* (((hash-table-p opencode--blocks))
              (overlay (gethash key opencode--blocks)))
    (and (overlay-buffer overlay) overlay)))

(defun opencode-block-data (overlay)
  "Return the data plist of block OVERLAY."
  (overlay-get overlay 'opencode-data))

(defun opencode-reasoning-key (message-id ordinal)
  "Return the block key of reasoning ORDINAL of assistant MESSAGE-ID."
  (format "%s/r/%d" message-id ordinal))

(defun opencode-tool-blocks ()
  "Return the live tool block overlays of this buffer."
  (let (overlays)
    (when (hash-table-p opencode--blocks)
      (maphash (lambda (_key overlay)
                 (when (and (overlay-buffer overlay)
                            (eq (plist-get (opencode-block-data overlay) :type) 'tool))
                   (push overlay overlays)))
               opencode--blocks))
    overlays))

;;;; Disclosure arrows

(defun opencode-disclosure-string (expanded face)
  "Return the arrow of an EXPANDED or collapsed block, colored by FACE."
  (let ((text (propertize (if expanded "▾ " "▸ ")
                          'face `(:foreground ,(face-foreground face nil t)))))
    (add-face-text-property 0 1 'opencode-disclosure nil text)
    text))

;;;; Summaries

(defun opencode-tool-icon (tool &optional input)
  "Return the Nerd Font icon of TOOL called with INPUT."
  (pcase tool
    ("read" "\uf0f6")                          ; nf-fa-file_text_o
    ("write" "\uf15c")                         ; nf-fa-file_text
    ("edit" "\uf040")                          ; nf-fa-pencil
    ("patch" "\uf4d2")                         ; nf-fa-puzzle_piece
    ("shell" "\ue795")                         ; nf-dev-terminal
    ("grep" "\uf002")                          ; nf-fa-search
    ("glob" "\uf0c5")                          ; nf-fa-files_o
    ((or "websearch" "webfetch") "\uf0ac")     ; nf-fa-globe
    ("question" "\uf059")                      ; nf-fa-question_circle
    ("skill" "\uf02d")                         ; nf-fa-book
    ("execute" "\uf121")                       ; nf-fa-code
    ("subagent" (if (equal (alist-get 'agent input) "explore")
                    "\uf14e"                   ; nf-fa-compass
                  "\U000F06A9"))               ; nf-md-robot
    (_ "\uf0ad")))                             ; nf-fa-wrench

(defun opencode-tool-short-label (tool)
  "Return the boxed label of TOOL; MCP tools keep their last name segment."
  (pcase tool
    ("websearch" "search")
    ("webfetch" "fetch")
    ("execute" "code")
    (_ tool)))

(defun opencode-boxed-label (icon label face)
  "Return ICON in an inverse box and LABEL in a bordered box, colored as FACE."
  (let* ((foreground (face-foreground face nil t))
         (icon-face (list :foreground foreground :background (face-background 'default nil t)
                          :inverse-video t))
         (box-face (list :box (list :color foreground) :slant 'italic)))
    (concat (propertize (format " %s " icon) 'face icon-face 'font-lock-face icon-face)
            (propertize (format " %s " label) 'face box-face 'font-lock-face box-face))))

(defun opencode--generic-tool-description (input)
  "Describe INPUT by its only value, or by its number of arguments."
  (if (= 1 (length input))
      (format "%s" (cdar input))
    (format "[%d args]" (length input))))

(defun opencode--patch-description (patch-text)
  "Return the first file named by PATCH-TEXT, or \"patch\"."
  (or (and (stringp patch-text)
           (save-match-data
             (and (string-match "^\\*\\*\\* \\(?:Add\\|Update\\|Delete\\) File: \\(.+\\)$" patch-text)
                  (match-string 1 patch-text))))
      "patch"))

(defun opencode-question-label (question)
  "Return QUESTION's header, or its text when it has none."
  (let ((header (alist-get 'header question)))
    (if (and header (not (string-empty-p header))) header (alist-get 'question question))))

(defun opencode-question-description (input)
  "Describe question tool INPUT by its question, or by a batch's headers."
  (let ((questions (alist-get 'questions input)))
    (or (if (cdr questions)
            (mapconcat #'opencode-question-label questions "; ")
          (alist-get 'question (car questions)))
        "question")))

(defun opencode-question-answer-description (input answers)
  "Describe INPUT's questions, each followed by its entry of ANSWERS.
Questions are shortened so the answers stay within the summary width."
  (let* ((questions (alist-get 'questions input))
         (single (null (cdr questions))))
    (string-join
     (cl-loop for question in questions
              for index from 0
              for answer = (nth index answers)
              for text = (if answer (string-join answer ", ") "unanswered")
              for label = (string-trim
                           (replace-regexp-in-string
                            "[ \t\n\r]+" " "
                            (or (if single (alist-get 'question question)
                                  (opencode-question-label question))
                                "")))
              for width = (if single
                              (max 20 (- opencode-question-summary-width 25 (length text)))
                            30)
              collect (format "%s → %s"
                              (if (> (length label) width)
                                  (concat (string-trim-right (substring label 0 (1- width))) "…")
                                label)
                              text))
     "; ")))

(defun opencode-tool-description (tool input)
  "Return the raw, possibly multi-line, description of TOOL called with INPUT."
  (let ((custom (cdr (assoc tool opencode-tool-summary-functions))))
    (or (and custom (funcall custom input))
        (let-alist input
          (pcase tool
            ((or "read" "write" "edit")
             (or (and .path (opencode-relative-path-for-display .path)) "?"))
            ("patch" (opencode--patch-description .patchText))
            ("shell" (or .command "?"))
            ((or "grep" "glob") (format "%S" (or .pattern "?")))
            ("websearch" (format "%S" (or .query "?")))
            ("webfetch" (or .url "?"))
            ("question" (opencode-question-description input))
            ("skill" (or .id "?"))
            ("execute" (or (and (stringp .code) (car (split-string (string-trim .code) "\n")))
                           (opencode--generic-tool-description input)))
            ("subagent" (format "%s: %s" (capitalize (or .agent "subagent")) (or .description "")))
            (_ (opencode--generic-tool-description input)))))))

(defun opencode-tool-summary (tool input face &optional status-icon description note)
  "Return the one-line summary of TOOL called with INPUT, colored as FACE.
STATUS-ICON goes between the boxed label and the description, which
DESCRIPTION replaces; NOTE follows the description.  Whitespace is
flattened so a multi-line argument cannot break the line.  A failing
description falls back to the generic one: the block must exist for a
permission to expand it."
  (let* ((description
          (or description
              (condition-case err
                  (opencode-tool-description tool input)
                (error
                 (message "OpenCode %s summary failed: %s" tool
                          (truncate-string-to-width (error-message-string err) 200 nil nil "…"))
                 (opencode--generic-tool-description input)))))
         (full (concat (opencode-boxed-label (opencode-tool-icon tool input)
                                             (opencode-tool-short-label tool) face)
                       (when status-icon (concat " " status-icon))
                       " "
                       (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " (or description "")))
                       (when note (concat " · " note)))))
    (if (> (length full) opencode-tool-summary-max-width)
        (concat (substring full 0 opencode-tool-summary-max-width) "…")
      full)))

(defun opencode-pending-prompt-icon ()
  "Return the question mark of a block waiting on the user."
  (propertize "\U000F02D7" 'face 'opencode-attention 'font-lock-face 'opencode-attention)) ; nf-md-help_circle

(defun opencode-tool-error-info (error)
  "Return (KIND . SOURCE) describing tool ERROR, an alist with `type' and `message'."
  (let ((type (or (alist-get 'type error) ""))
        (message (or (alist-get 'message error) "")))
    (cond
     ((equal type "permission.rejected") '(rejected . "you"))
     ((string-prefix-p "permission." type) '(blocked . "an OpenCode permission rule"))
     ((string-match "\\`\\[\\([^]]+\\)\\]" message)
      (cons 'blocked (format "plugin %s" (match-string 1 message))))
     ((equal message "dismissed") '(dismissed . "you"))
     ((equal type "aborted") '(interrupted))
     (t '(failed)))))

(defun opencode-tool-error-message (error)
  "Return a readable failure message for tool ERROR."
  (pcase-let ((`(,kind . ,source) (opencode-tool-error-info error))
              (message (or (alist-get 'message error) "")))
    (pcase kind
      ('rejected (if (string-empty-p message) "Rejected"
                   (format "Rejected: %s" message)))
      ('blocked (format "Blocked by %s: %s" source
                        (if (string-match "\\`\\[[^]]+\\][[:space:]]*" message)
                            (substring message (match-end 0))
                          message)))
      ('dismissed "Question dismissed")
      ('interrupted "Interrupted")
      (_ (format "Failed: %s" message)))))

(defun opencode-tool-error-summary (tool input error)
  "Return TOOL's summary for INPUT with a status icon for ERROR."
  (let ((icon (propertize (if (memq (car (opencode-tool-error-info error))
                                    '(blocked rejected dismissed))
                              "\uf05e"          ; nf-fa-ban
                            "\uf057")           ; nf-fa-times_circle
                          'face 'error 'font-lock-face 'error)))
    (opencode-tool-summary tool input 'error icon)))

;;;; Reasoning summaries

(defun opencode-reasoning-block-kind (native)
  "Return the block kind named by the Anthropic signature in reasoning NATIVE data.
The decoded header names the type as a length-prefixed string such as
\"thinking\" or \"narration\".  The format is undocumented; an unknown
or malformed header gives nil."
  (let ((signature (alist-get 'signature native)))
    (when (stringp signature)
      (let ((head (ignore-errors
                    (base64-decode-string
                     (substring signature 0 (min 64 (- (length signature)
                                                       (mod (length signature) 4))))))))
        (save-match-data
          (when (and head
                     (string-match "B\\([\x01-\x20]\\)\\([a-z_]+\\)" head)
                     (= (aref (match-string 1 head) 0) (length (match-string 2 head))))
            (match-string 2 head)))))))

(defun opencode-reasoning-hidden-kind (text native)
  "Return the kind of a reasoning block whose TEXT the provider withheld.
Nil unless TEXT is empty and its NATIVE data carries a readable signature."
  (when (string-empty-p (or text ""))
    (opencode-reasoning-block-kind native)))

(defun opencode-narration-p (text native)
  "Return non-nil when reasoning TEXT with NATIVE data is narration for the user."
  (and (not (string-empty-p (or text "")))
       (equal (opencode-reasoning-block-kind native) "narration")))

(defun opencode-reasoning-preview (text)
  "Return a bounded one-line preview of reasoning TEXT.
The leading Markdown heading or emphasis wrapper is omitted; only enough
of TEXT is read to fill the summary."
  (save-match-data
    (let* ((text (or text ""))
           (end (length text))
           (limit (1+ (max 0 opencode-tool-summary-max-width)))
           (_ (string-match "\\`[ \t\r\n]*\\(?:#+[ \t]+\\)?\\(\\*\\*\\|__\\|`+\\|\\*\\|_\\)?" text))
           (pos (match-end 0))
           (delimiter (match-string 1 text))
           (size 0)
           chars space)
      (while (and (< pos end) (< size limit))
        (cond
         ((and delimiter
               (<= (+ pos (length delimiter)) end)
               (string= delimiter (substring text pos (+ pos (length delimiter)))))
          (setq pos (+ pos (length delimiter)) delimiter nil))
         ((memq (aref text pos) '(?\s ?\t ?\r ?\n))
          (string-match "[ \t\r\n]+" text pos)
          (setq pos (match-end 0) space (> size 0)))
         (t
          (when space
            (push ?\s chars)
            (setq size (1+ size) space nil))
          (when (< size limit)
            (push (aref text pos) chars)
            (setq size (1+ size) pos (1+ pos))))))
      (concat (apply #'string (nreverse chars)) (when (< pos end) "…")))))

(defun opencode-dropped-as-text-p (dropped)
  "Return non-nil when DROPPED thinking is replayed to the model as text."
  (and dropped (alist-get 'asText dropped)))

(defun opencode-reasoning-summary (text &optional hidden-kind dropped)
  "Return the summary of a reasoning block with raw TEXT so far.
HIDDEN-KIND names a block whose text the provider withheld; withheld
narration was addressed to the user and shows in the attention face.
DROPPED is the marker of thinking no longer sent to the model signed:
labelled \"as text\" when its text replaces it, \"dropped\" otherwise."
  (let* ((narration (equal hidden-kind "narration"))
         (boxes (opencode-boxed-label (if narration "\xf0e5" "\xf0eb")
                                      (or hidden-kind "thinking")
                                      (if narration 'opencode-attention
                                        'opencode-reasoning-margin-highlight)))
         (detail (cond
                  (narration (propertize "hidden" 'face 'opencode-attention
                                         'font-lock-face 'opencode-attention))
                  (hidden-kind "hidden")
                  (t (let ((preview (opencode-reasoning-preview text)))
                       (format "(%d chars) %s" (length text)
                               (if (string-empty-p preview) "…" preview))))))
         (full (concat boxes
                       (when dropped
                         (concat " " (propertize (if (opencode-dropped-as-text-p dropped)
                                                     "as text" "dropped")
                                                 'face 'opencode-attention
                                                 'font-lock-face 'opencode-attention)))
                       " " detail)))
    (truncate-string-to-width full opencode-tool-summary-max-width nil nil "…")))

;;;; Insertion and update

(defun opencode-insert-collapsed (summary face data)
  "Write SUMMARY as a collapsed block line with a FACE margin and return its overlay.
DATA, a plist, is kept on the overlay for expansion.  The margin lives on
this overlay: its end marker sits after the trailing newline and follows
every summary replacement."
  (let ((start (opencode-pmark)))
    (opencode-output (opencode-fontify-as (concat summary "\n") 'verbatim))
    (let ((overlay (make-overlay start (opencode-pmark) nil t nil))
          (margin (opencode-margin face)))
      (overlay-put overlay 'opencode-collapsible t)
      (overlay-put overlay 'opencode-expanded nil)
      (overlay-put overlay 'opencode-data data)
      (overlay-put overlay 'opencode-summary summary)
      (overlay-put overlay 'before-string (opencode-disclosure-string nil face))
      (overlay-put overlay 'keymap opencode-collapsible-keymap)
      (overlay-put overlay 'mouse-face 'highlight)
      (overlay-put overlay 'line-prefix margin)
      (overlay-put overlay 'wrap-prefix margin)
      (opencode-summary-fit-queue overlay)
      overlay)))

(defun opencode-replace-summary (overlay summary)
  "Replace OVERLAY's summary text with SUMMARY, unless it already shows it.
Faces count: a finished call's summary reads as its running one did,
in another color."
  (let ((inhibit-read-only t)
        (start (overlay-start overlay))
        (end (overlay-end overlay)))
    (unless (and (equal-including-properties summary (overlay-get overlay 'opencode-summary))
                 (equal summary (buffer-substring-no-properties start (1- end))))
      ;; Keep an adjacent expansion outside the summary while replacing it.
      (overlay-put overlay 'front-advance nil)
      (unwind-protect
          (save-excursion
            (goto-char start)
            (delete-region start (1- end))
            (insert (opencode-fontify-as summary 'verbatim))
            (move-overlay overlay start (1+ (point))))
        (overlay-put overlay 'front-advance t))
      (overlay-put overlay 'opencode-summary summary)
      (opencode-summary-fit-queue overlay))))

(defun opencode-style-block (overlay face)
  "Color OVERLAY's arrow and margins with FACE."
  (let ((margin (opencode-margin face)))
    (overlay-put overlay 'face nil)
    (overlay-put overlay 'before-string
                 (opencode-disclosure-string (overlay-get overlay 'opencode-expanded) face))
    (overlay-put overlay 'line-prefix margin)
    (overlay-put overlay 'wrap-prefix margin)))

(defun opencode-block-face (data)
  "Return the display face of block DATA."
  (cond
   ((plist-get data :error) 'error)
   ((plist-get data :waiting) 'opencode-attention)
   ((member (plist-get data :status) '("streaming" "running")) 'opencode-tool-running-margin)
   ((eq (plist-get data :type) 'reasoning) 'opencode-reasoning-margin-highlight)
   ((eq (plist-get data :type) 'notice) 'opencode-reasoning-margin-highlight)
   (t 'opencode-tool-margin-highlight)))

(defun opencode-tool-collapsed-summary (data)
  "Return the collapsed summary of tool block DATA."
  (let ((tool (plist-get data :tool))
        (input (plist-get data :input))
        (face (opencode-block-face data)))
    (cond
     ((plist-get data :waiting)
      (opencode-tool-summary tool input face (opencode-pending-prompt-icon)
                             nil (plist-get data :waiting-description)))
     ((plist-get data :error) (opencode-tool-error-summary tool input (plist-get data :error)))
     ((and (equal tool "question") (plist-get data :answers))
      (opencode-tool-summary tool input face nil
                             (opencode-question-answer-description input (plist-get data :answers))))
     ((equal (plist-get data :status) "streaming")
      (opencode-tool-summary tool input face nil "…"))
     (t (opencode-tool-summary tool input face)))))

(defun opencode-refresh-block (overlay)
  "Redraw OVERLAY's summary, colors and expanded content after its data changed."
  (when (and (overlayp overlay) (overlay-buffer overlay))
    (let ((data (opencode-block-data overlay)))
      (if (eq (plist-get data :type) 'reasoning)
          (opencode-replace-summary
           overlay (opencode-reasoning-summary (plist-get data :text) (plist-get data :hidden-kind)
                                               (plist-get data :dropped)))
        (opencode-replace-summary overlay (opencode-tool-collapsed-summary data)))
      (opencode-style-block overlay (opencode-block-face data))
      (opencode--refresh-expanded overlay))))

(defun opencode-block-update (overlay &rest properties)
  "Set PROPERTIES on OVERLAY's data and redraw it when they change anything."
  (let ((data (opencode-block-data overlay))
        changed)
    (while properties
      (let ((key (pop properties))
            (value (pop properties)))
        (unless (equal (plist-get data key) value)
          (setq data (plist-put data key value) changed t))))
    (when changed
      (overlay-put overlay 'opencode-data data)
      (opencode-refresh-block overlay))
    changed))

;;;; Expansion

(defun opencode--format-edit-diff (old new)
  "Return the unified diff body from OLD to NEW."
  (with-temp-buffer
    (let ((old-buffer (current-buffer)))
      (insert old "\n")
      (with-temp-buffer
        (let ((new-buffer (current-buffer)))
          (insert new "\n")
          (with-temp-buffer
            (diff-no-select old-buffer new-buffer nil t (current-buffer))
            (goto-char (point-min))
            (forward-line 3)
            (delete-region (point-min) (point))
            (goto-char (point-max))
            (forward-line -2)
            (delete-region (point) (point-max))
            (buffer-string)))))))

(defun opencode--format-patch (patch-text)
  "Return PATCH-TEXT with file names relative to the project, for display."
  (with-temp-buffer
    (insert patch-text)
    (goto-char (point-min))
    (while (re-search-forward "^\\*\\*\\* \\(?:Begin\\|End\\) Patch\n?" nil t)
      (replace-match "" t t))
    (goto-char (point-min))
    (while (re-search-forward "^\\*\\*\\* \\(Add File\\|Update File\\|Delete File\\|Move to\\): \\(.*\\)$" nil t)
      (replace-match (opencode-relative-path-for-display (match-string 2)) t t nil 2))
    (string-trim (buffer-string) "\n+" "\n+")))

(defun opencode-format-tool-call (tool input)
  "Return the full display of TOOL called with INPUT."
  (let-alist input
    (pcase tool
      ("edit" (concat "edit " (opencode-relative-path-for-display (or .path "")) ":\n"
                      (opencode-fenced-block
                       (opencode--format-edit-diff (or .oldString "") (or .newString "")) "diff")))
      ("patch" (concat "patch:\n" (opencode-fenced-block (opencode--format-patch (or .patchText "")) "diff")))
      ("write" (concat "write " (opencode-relative-path-for-display (or .path "")) ":\n"
                       (opencode-fenced-block (or .content "") "text")))
      ("read" (if .offset
                  (format "read `%s:%s`%s" (opencode-relative-path-for-display .path) .offset
                          (if .limit (format " limit=%s" .limit) ""))
                (format "read `%s`" (opencode-relative-path-for-display (or .path "")))))
      ("shell" (concat (format "$ %s" .command)
                       (when .workdir (format "\n# in %s" (opencode-relative-path-for-display .workdir)))
                       (when .background "\n# in the background")))
      ((or "grep" "glob") (concat (format "%s %S" tool (or .pattern ""))
                                  (when .path (format " in %s" .path))
                                  (when .include (format " (%s)" .include))))
      ("websearch" (format "websearch %S" .query))
      ("webfetch" (format "webfetch %s" .url))
      ("execute" (opencode-fenced-block (or .code (format "%S" input)) "javascript"))
      (_ (if (= 1 (length input))
             (let ((value (format "%s" (cdar input))))
               (format "%s%s%s" tool (if (string-search "\n" value) "\n" " ") value))
           (concat tool " ["
                   (mapconcat (lambda (pair) (format "%s=%s" (car pair) (cdr pair))) input ", ")
                   "]"))))))

(defvar opencode-block-content-functions nil
  "Functions rendering the expanded content of special blocks.
Each receives the block data and returns a string, or nil to defer.")

(defun opencode-render-block-content (data)
  "Return the expanded content of block DATA."
  (or (run-hook-with-args-until-success 'opencode-block-content-functions data)
      (pcase (plist-get data :type)
        ('reasoning
         (let* ((hidden (plist-get data :hidden-kind))
                (dropped (plist-get data :dropped))
                (content (cond
                          ((equal hidden "narration")
                           "The provider returned this narration encrypted, so its text never reached OpenCode.  Ask the agent to repeat it as plain text.")
                          (hidden (format "The provider returned this %s block encrypted, so its text never reached OpenCode." hidden))
                          (t (plist-get data :text)))))
           (if dropped
               (concat (format (if (opencode-dropped-as-text-p dropped)
                                   "_Replayed to the model as plain text from then on: %s._\n\n"
                                 "_Left out of later requests: %s._\n\n")
                               (alist-get 'reason dropped))
                       (or content ""))
             content)))
        ('notice (plist-get data :text))
        ('tool
         (let ((tool (plist-get data :tool))
               (input (plist-get data :input))
               (output (plist-get data :output)))
           (string-join
            (delq nil
                  (if (equal tool "subagent")
                      (list (alist-get 'prompt input) output)
                    (list (opencode-format-tool-call tool input)
                          (and output (not (string-empty-p output))
                               (opencode-fenced-block output "text")))))
            "\n"))))))

(defun opencode--expanded-content (overlay)
  "Return the full expansion text of OVERLAY, error included."
  (let* ((data (opencode-block-data overlay))
         (content (opencode-render-block-content data))
         (error (plist-get data :error)))
    (string-join (delq nil (list (and content (not (string-empty-p content)) content)
                                 (and error (opencode-tool-error-message error))))
                 "\n\n")))

(defun opencode--insert-expansion (overlay position)
  "Insert OVERLAY's expansion at POSITION and record its content overlay."
  (let ((content (opencode--expanded-content overlay))
        (face (opencode-block-face (opencode-block-data overlay))))
    (if (string-empty-p content)
        (overlay-put overlay 'opencode-content-overlay nil)
      (let* ((end (opencode-insert-output-at position (concat content "\n")))
             ;; Exclude output later inserted at either boundary.
             (content-overlay (make-overlay position end nil t nil))
             (margin (opencode-margin face)))
        (overlay-put content-overlay 'evaporate t)
        ;; Own the margin, or a tight following block would lend its own.
        (overlay-put content-overlay 'line-prefix margin)
        (overlay-put content-overlay 'wrap-prefix margin)
        (overlay-put content-overlay 'priority 1)
        (overlay-put content-overlay 'opencode-summary overlay)
        (overlay-put overlay 'opencode-content-overlay content-overlay)))))

(defun opencode--refresh-expanded (overlay)
  "Rewrite OVERLAY's expansion after its data changed."
  (when (overlay-get overlay 'opencode-expanded)
    (let ((old (overlay-get overlay 'opencode-content-overlay))
          (inhibit-read-only t))
      (if (and (overlayp old) (overlay-buffer old))
          (let ((start (overlay-start old))
                (end (overlay-end old)))
            (delete-overlay old)
            (delete-region start end)
            (opencode--insert-expansion overlay start))
        (opencode--insert-expansion overlay (overlay-end overlay))))))

(defun opencode-collapsible-at (position)
  "Return the collapsible summary overlay at POSITION, if any."
  (seq-find (lambda (overlay) (overlay-get overlay 'opencode-collapsible))
            (overlays-at position)))

(defun opencode-block-at (position)
  "Return the summary overlay of the block whose summary or content is at POSITION."
  (or (opencode-collapsible-at position)
      (seq-some (lambda (overlay)
                  (let ((summary (overlay-get overlay 'opencode-summary)))
                    (and (overlayp summary) (overlay-buffer summary) summary)))
                (overlays-at position))))

(defun opencode-expand-block (overlay)
  "Expand OVERLAY's block unless it is already expanded."
  (unless (overlay-get overlay 'opencode-expanded)
    (let ((inhibit-read-only t)
          (face (opencode-block-face (opencode-block-data overlay))))
      (overlay-put overlay 'opencode-expanded t)
      (opencode--insert-expansion overlay (overlay-end overlay))
      (overlay-put overlay 'before-string (opencode-disclosure-string t face)))))

(defun opencode-collapse-block (overlay)
  "Collapse OVERLAY's block unless it is already collapsed."
  (when (overlay-get overlay 'opencode-expanded)
    (let* ((content (overlay-get overlay 'opencode-content-overlay))
           (pmark (and (get-buffer-process (current-buffer)) (opencode-pmark)))
           (inhibit-read-only t))
      ;; An expansion that swallowed later output must not delete it.
      (if (and (overlayp content) (overlay-buffer content) pmark
               (< (overlay-start content) pmark (overlay-end content)))
          (message "Cannot collapse an expansion that contains later output")
        (when (and (overlayp content) (overlay-buffer content))
          (let ((start (overlay-start content))
                (end (overlay-end content)))
            (delete-overlay content)
            (delete-region start end)))
        (overlay-put overlay 'opencode-content-overlay nil)
        (overlay-put overlay 'opencode-expanded nil)
        (overlay-put overlay 'before-string
                     (opencode-disclosure-string nil (opencode-block-face (opencode-block-data overlay))))))))

(defun opencode-toggle-block (&optional position)
  "Expand or collapse the block whose summary is at POSITION or point."
  (interactive)
  (when-let* ((overlay (opencode-collapsible-at (or position (point)))))
    (if (overlay-get overlay 'opencode-expanded)
        (opencode-collapse-block overlay)
      (opencode-expand-block overlay))))

(defun opencode--next-block-start (position)
  "Return the start of the first collapsible summary after POSITION, or nil."
  (let (best)
    (dolist (overlay (overlays-in position (point-max)) best)
      (when (and (overlay-get overlay 'opencode-collapsible)
                 (> (overlay-start overlay) position)
                 (or (null best) (< (overlay-start overlay) best)))
        (setq best (overlay-start overlay))))))

(defun opencode-tab-dwim ()
  "Toggle the block at point, follow the button at point, or go to the next block.
Never switches the session agent; that is \\[opencode-cycle-agent]."
  (interactive)
  (if-let* ((summary (opencode-block-at (point))))
      (progn
        (goto-char (overlay-start summary))
        (opencode-toggle-block (point)))
    (if (button-at (point))
        (forward-button 1)
      (if-let* ((next (opencode--next-block-start (point))))
          (goto-char next)
        (message "No block after point")))))

;;;; Reasoning blocks

(defun opencode-reasoning-start (key &optional text native)
  "Write a collapsed reasoning block for KEY with TEXT and NATIVE data; return it."
  (opencode-blocks-ensure)
  (or (opencode-block key)
      (let* ((text (or text ""))
             (hidden (opencode-reasoning-hidden-kind text native))
             (overlay (opencode-insert-collapsed
                       (opencode-reasoning-summary text hidden)
                       'opencode-reasoning-margin-highlight
                       (list :type 'reasoning :key key :text text :hidden-kind hidden))))
        (puthash key overlay opencode--blocks)
        overlay)))

(defun opencode-reasoning-append (key delta)
  "Append DELTA to reasoning block KEY and update its summary."
  (when-let* ((overlay (opencode-block key)))
    (opencode-block-update overlay :text (concat (plist-get (opencode-block-data overlay) :text) delta))))

(defun opencode--show-narration (overlay text)
  "Replace narration block OVERLAY with its TEXT as prose.
Its signature only arrives at completion, after a provisional thinking
block was shown; an expanded copy goes too."
  (let* ((content (overlay-get overlay 'opencode-content-overlay))
         (start (overlay-start overlay))
         (end (if (and content (overlay-buffer content)) (overlay-end content) (overlay-end overlay)))
         (key (plist-get (opencode-block-data overlay) :key))
         (inhibit-read-only t))
    (when content (delete-overlay content))
    (delete-overlay overlay)
    (delete-region start end)
    (let* ((prefix (if (or (= start (point-min))
                           (and (> start (1+ (point-min)))
                                (equal (buffer-substring-no-properties (- start 2) start) "\n\n")))
                       "" "\n"))
           (finish (opencode-insert-output-at start (concat prefix (string-trim text) "\n\n")))
           (marker (make-overlay start finish nil t nil)))
      (overlay-put marker 'opencode-narration t)
      (overlay-put marker 'opencode-data (list :type 'narration :key key))
      (puthash key marker opencode--blocks)
      (opencode-set-last-response-start (+ start (length prefix))))))

(defun opencode-reasoning-end (key text native)
  "Finish reasoning block KEY with its full TEXT and the provider's NATIVE data."
  (opencode-blocks-ensure)
  (let ((overlay (or (opencode-block key) (opencode-reasoning-start key text native))))
    (unless (overlay-get overlay 'opencode-narration)
      (if (opencode-narration-p text native)
          (progn (opencode--show-narration overlay text)
                 (setq opencode--last-block-kind 'text))
        (opencode-block-update overlay :text (or text "")
                               :hidden-kind (opencode-reasoning-hidden-kind text native))))))

(defun opencode-reasoning-mark-dropped (key dropped)
  "Relabel reasoning block KEY with its DROPPED marker."
  (when-let* ((overlay (opencode-block key)))
    (unless (overlay-get overlay 'opencode-narration)
      (opencode-block-update overlay :dropped dropped))))

;;;; Tool blocks

(defun opencode-tool-start (id name &optional input status)
  "Write a collapsed tool block for call ID of tool NAME and return its overlay.
INPUT and STATUS default to an empty input still streaming."
  (opencode-blocks-ensure)
  (or (opencode-block id)
      (let* ((data (list :type 'tool :key id :tool name :input input
                         :status (or status "streaming")))
             (overlay (opencode-insert-collapsed (opencode-tool-collapsed-summary data)
                                                 (opencode-block-face data) data)))
        (puthash id overlay opencode--blocks)
        (run-hook-with-args 'opencode-blocks-attach-functions overlay)
        overlay)))

(defun opencode-tool-update (id &rest properties)
  "Update tool block ID with PROPERTIES, then run the finished hooks on completion."
  (when-let* ((overlay (opencode-block id)))
    (let ((before (plist-get (opencode-block-data overlay) :status)))
      (apply #'opencode-block-update overlay properties)
      (let ((after (plist-get (opencode-block-data overlay) :status)))
        (when (and (member after '("completed" "error"))
                   (not (member before '("completed" "error"))))
          (run-hook-with-args 'opencode-blocks-finished-functions overlay)
          (when (and opencode-show-tool-output (plist-get (opencode-block-data overlay) :output)
                     (not (overlay-get overlay 'opencode-expanded)))
            (opencode-expand-block overlay)))))
    overlay))

(defun opencode-tool-data-from-content (item)
  "Return the tool block properties of projected assistant tool content ITEM."
  (let-alist item
    (list :status .state.status
          :input (if (stringp .state.input) nil .state.input)
          :output (opencode-content-text .state.content)
          :metadata .state.metadata
          :error .state.error
          :session-id (alist-get 'sessionID .state.metadata)
          :answers (alist-get 'answers .state.metadata))))

;;;; Notice blocks

(defun opencode-notice-block (summary text &optional key)
  "Write a collapsed notice showing SUMMARY that expands to TEXT.
KEY, when given, identifies the block."
  (opencode-blocks-ensure)
  (let ((overlay (opencode-insert-collapsed
                  (opencode-fontify-as (propertize summary 'font-lock-face 'opencode-notice) 'verbatim)
                  'opencode-reasoning-margin-highlight
                  (list :type 'notice :key key :text text))))
    (when key (puthash key overlay opencode--blocks))
    overlay))

;;;; Fitting summaries to the window

;; Lines are only truncated per buffer, and prose must keep wrapping, so
;; an overflowing summary would wrap.  Each one that overflows a window
;; gets an overlay restricted to that window (its `window' property)
;; displaying "…" over the line's tail; one buffer in two windows of
;; different widths is cut separately.  Widths are pixels, measured with
;; the buffer's face remapping, since icons and boxes are wider than a
;; column.  Work stays proportional to what windows show: resizes, scrolls
;; and font scaling refit the summaries around a window's text, and each
;; summary caches its width per font and the room it was fitted to per
;; window.  Changed summaries are fitted at once, so redisplay never
;; flashes one wrapped; while redisplay is inhibited, as during replay,
;; they are batched into one timer.

(defvar-local opencode--fit-pending nil
  "Summary overlays changed since the last fit.")

(defvar-local opencode--fit-timer nil
  "Timer fitting `opencode--fit-pending'.")

(defun opencode--fit-p (overlay)
  "Return non-nil when collapsible OVERLAY has a summary line to fit."
  (and (overlay-buffer overlay)
       (< (overlay-start overlay) (overlay-end overlay))
       (overlay-get overlay 'opencode-collapsible)))

(defun opencode--fit-text (overlay)
  "Return OVERLAY's arrow and summary line, `font-lock-face' runs shown as `face'."
  (let* ((start (overlay-start overlay))
         (text (concat (or (overlay-get overlay 'before-string) "")
                       (buffer-substring start (save-excursion (goto-char start) (line-end-position)))))
         (pos 0))
    (while (< pos (length text))
      (let ((next (next-single-property-change pos 'font-lock-face text (length text)))
            (face (get-text-property pos 'font-lock-face text)))
        (when face (put-text-property pos next 'face face text))
        (setq pos next)))
    text))

(defun opencode--fit-chars (text room buffer)
  "Return how many leading characters of TEXT fit in ROOM pixels, as BUFFER shows them."
  (let ((remapping (buffer-local-value 'face-remapping-alist buffer)))
    (with-work-buffer
      (setq-local face-remapping-alist remapping)
      (insert text)
      (let ((low 0)
            (high (length text)))
        (while (< low high)
          (let ((mid (/ (+ low high 1) 2)))
            (save-restriction
              (narrow-to-region (point-min) (+ (point-min) mid))
              (if (<= (car (buffer-text-pixel-size nil nil t)) room)
                  (setq low mid)
                (setq high (1- mid))))))
        low))))

(defun opencode--fit-room (window)
  "Return the pixels one unwrapped line of WINDOW's text area holds."
  (* (window-max-chars-per-line window) (window-font-width window)))

(defun opencode--fit-cut (overlay window room width text fits)
  "Return FITS, the (WINDOW . CUT) overlays of OVERLAY, with WINDOW's cut for ROOM.
WIDTH is OVERLAY's full pixel width; TEXT its measured line, or nil."
  (let ((old (alist-get window fits)))
    (if (<= width room)
        (when old
          (delete-overlay old)
          (setq fits (assq-delete-all window fits)))
      (let* ((text (or text (opencode--fit-text overlay)))
             (kept (- (opencode--fit-chars text (- room (string-pixel-width "…" (current-buffer)))
                                           (current-buffer))
                      (length (or (overlay-get overlay 'before-string) ""))))
             (start (overlay-start overlay))
             (end (save-excursion (goto-char start) (line-end-position)))
             (cut (min end (+ start (max 0 kept)))))
        (if old
            (move-overlay old cut end)
          (setq old (make-overlay cut end))
          (overlay-put old 'window window)
          (overlay-put old 'display "…")
          (overlay-put old 'evaporate t)
          (push (cons window old) fits))))
    fits))

(defun opencode-summary-fit (overlay window room)
  "Cut OVERLAY's summary beyond ROOM pixels, in WINDOW only.
Skip it when its text, font and WINDOW's ROOM are those last fitted."
  (let* ((key (cons (window-font-width window) face-remapping-alist))
         (state (overlay-get overlay 'opencode-fit-state))
         (fresh (equal (car state) key)))
    (unless (and fresh (eql (alist-get window (nth 2 state)) room))
      (let (fits text width)
        (dolist (fit (overlay-get overlay 'opencode-fit-overlays))
          (if (and (window-live-p (car fit)) (overlay-buffer (cdr fit)))
              (push fit fits)
            (delete-overlay (cdr fit))))
        (setq width (if fresh
                        (nth 1 state)
                      (setq text (opencode--fit-text overlay))
                      (string-pixel-width text (current-buffer))))
        (setq fits (opencode--fit-cut overlay window room width text fits))
        (overlay-put overlay 'opencode-fit-overlays fits)
        (overlay-put overlay 'opencode-fit-state
                     (list key width
                           (cons (cons window room)
                                 (seq-filter (lambda (entry)
                                               (and (window-live-p (car entry))
                                                    (not (eq (car entry) window))))
                                             (and fresh (nth 2 state))))))))))

(defun opencode--fit-window (window overlays)
  "Fit the summaries among OVERLAYS to WINDOW, when it shows this buffer."
  (when (and (window-live-p window) (eq (window-buffer window) (current-buffer)))
    (with-demoted-errors "OpenCode summary fit: %S"
      (save-excursion
        (save-restriction
          (widen)
          (let ((room (opencode--fit-room window)))
            (dolist (overlay overlays)
              (when (opencode--fit-p overlay)
                (opencode-summary-fit overlay window room)))))))))

(defun opencode--fit-visible (window &optional start)
  "Fit the summaries from a screen above to two below START in WINDOW."
  (when (and (window-live-p window) (eq (window-buffer window) (current-buffer)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((height (window-body-height window)))
          (goto-char (or start (window-start window)))
          (opencode--fit-window window (overlays-in (line-beginning-position (- 1 height))
                                                    (line-end-position (* 2 height)))))))))

(defun opencode-summary-fit-all-windows (&optional overlays)
  "Fit OVERLAYS, or the visible summaries, in each window showing this buffer."
  (dolist (window (get-buffer-window-list nil nil t))
    (if overlays
        (opencode--fit-window window overlays)
      (opencode--fit-visible window))))

(defun opencode--fit-flush (buffer)
  "Fit BUFFER's pending summaries in the windows showing it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((pending (seq-uniq opencode--fit-pending #'eq)))
        (setq opencode--fit-pending nil
              opencode--fit-timer nil)
        (when pending
          (opencode-summary-fit-all-windows pending))))))

(defun opencode-summary-fit-queue (overlay)
  "Fit changed summary OVERLAY in every window showing its buffer."
  (overlay-put overlay 'opencode-fit-state nil)
  (if (not inhibit-redisplay)
      (opencode-summary-fit-all-windows (list overlay))
    (push overlay opencode--fit-pending)
    (unless (timerp opencode--fit-timer)
      (setq opencode--fit-timer (run-with-timer 0 nil #'opencode--fit-flush (current-buffer))))))

(defun opencode-blocks-setup ()
  "Prepare block state and window fitting in this session buffer."
  (opencode-blocks-ensure)
  (add-hook 'window-size-change-functions #'opencode--fit-visible nil t)
  (add-hook 'window-scroll-functions #'opencode--fit-visible nil t)
  (add-hook 'text-scale-mode-hook #'opencode-summary-fit-all-windows nil t))

(provide 'opencode-blocks)
;;; opencode-blocks.el ends here
