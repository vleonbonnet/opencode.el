;;; opencode-parsers-test.el --- Tests of the tree-sitter parsers of session buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; markdown-ts gives each inline region of the transcript a parser of
;; its own, held by an overlay over it.  These tests rewrite the
;; transcript every way a session does, fontify it as redisplay would,
;; and check that each parser parses one region of the text: none
;; outlives its text or its overlay, none is held twice, and no region
;; gets a second one.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-parser-overlays ()
  "Return the overlays holding this buffer's local parsers."
  (seq-filter (lambda (overlay) (overlay-get overlay 'treesit-parser-local-p))
              (overlays-in (point-min) (point-max))))

(defun opencode-test-parsed-regions ()
  "Return how many regions of this buffer's Markdown a local parser may parse.
Those are its inline regions and the contents of its code blocks."
  (let ((root (treesit-buffer-root-node 'markdown)))
    (+ (length (treesit-query-capture root '((inline) @region)))
       (length (treesit-query-capture root '((code_fence_content) @region))))))

(defun opencode-test-check-parsers (&optional unfontified)
  "Fontify this buffer, check each local parser parses one region, return how many.
No parser is left without an overlay, no overlay holds a deleted parser
or one another holds too, and there are no more than regions to parse.
With UNFONTIFIED, check the parsers fontification made so far."
  (unless unfontified
    (font-lock-ensure))
  (let ((parsers (treesit-parser-list nil nil 'embedded))
        (held (mapcar (lambda (overlay) (overlay-get overlay 'treesit-parser))
                      (opencode-test-parser-overlays))))
    (should (= (length held) (length (seq-uniq held #'eq))))
    (should (seq-every-p (lambda (parser) (memq parser parsers)) held))
    (should (= (length parsers) (length held)))
    (should (<= (length parsers) (opencode-test-parsed-regions)))
    (length parsers)))

(defun opencode-test-face-at (text face)
  "Return non-nil when FACE is among the faces of the last TEXT in this buffer."
  (save-excursion
    (goto-char (point-max))
    (search-backward text)
    (memq face (ensure-list (get-text-property (point) 'face)))))

(defun opencode-test-stream-fontified (session message text)
  "Stream TEXT as the answer of MESSAGE in SESSION, fontifying between its words.
Return the events ending the text, which are not dispatched."
  (let ((base `((sessionID . ,session) (assistantMessageID . ,message) (ordinal . 0))))
    (opencode-test-dispatch
     `(((type . "session.step.started") (data (sessionID . ,session) (assistantMessageID . ,message)))
       ((type . "session.text.started") (data ,@base))))
    (dolist (word (split-string text "\\b" t))
      (opencode-test-dispatch `(((type . "session.text.delta") (data ,@base (delta . ,word)))))
      (font-lock-fontify-region (max (point-min) (- (point-max) 2000)) (point-max)))
    `(((type . "session.text.ended") (data ,@base (text . ,text))))))

(defmacro opencode-test-with-parsed-session (messages &rest body)
  "Run BODY in a fontified session buffer whose transcript is MESSAGES."
  (declare (indent 1) (debug t))
  `(opencode-test-with-transcript ,messages
     (with-current-buffer (opencode-test-open (opencode-test-session))
       (font-lock-mode 1)
       (opencode-test-check-parsers)
       ,@body)))

(ert-deftest opencode-test-parsers-follow-resync-rewrites ()
  "Catching up deletes the parsers of the running step it writes again."
  (opencode-test-with-parsed-session (opencode-test-long-transcript 3)
    (let ((session opencode-session-id))
      (dotimes (n 3)
        (let ((prompt (format "msg_p%du" n))
              (step (format "msg_p%da" n)))
          (opencode-test-dispatch (opencode-test-inbox-events session prompt "Go on"))
          (opencode-test-stream-fontified session step "Streaming *one*\n\nand **two**\n\n- a\n- b\n")
          (opencode-test-check-parsers)
          (setq opencode-test-transcript
                (append opencode-test-transcript
                        (list (opencode-test-prompt prompt "Go on")
                              (opencode-test-step step `((type . "text") (text . ,(format "Done *%d*." n))))
                              (opencode-test-idle (format "msg_p%di" n)))))
          (opencode-test-reconnect)
          (should-not (string-search "Streaming" (opencode-test-text (current-buffer))))
          (opencode-test-check-parsers)
          (should (opencode-test-face-at (format "%d*." n) 'markdown-ts-emphasis)))))))

(ert-deftest opencode-test-parsers-follow-reloads ()
  "Reloading the transcript leaves as many parsers as the transcript before."
  (opencode-test-with-parsed-session (opencode-test-long-transcript 5)
    (let ((count (opencode-test-check-parsers)))
      (dotimes (_ 3)
        (opencode-session-reload)
        (should (= (opencode-test-check-parsers) count))
        (should (opencode-test-face-at "notes.txt" 'markdown-ts-code-span))))))

(ert-deftest opencode-test-parsers-follow-lifted-prompt ()
  "Lifting the prompt never splits the overlay of a parser, which two regions would then share.
An answer ending without a newline has its parser's overlay grow over
the prompt written after it and the input typed there, until the next
fontification puts it back."
  (opencode-test-with-parsed-session nil
    (let ((session opencode-session-id))
      (opencode-test-dispatch (opencode-test-inbox-events session "msg_lift_u" "Ask"))
      (opencode-test-turn session "started")
      (opencode-test-dispatch (opencode-test-stream-fontified session "msg_lift_a" "Answer *here*"))
      (opencode-test-turn session "succeeded")
      (should (opencode-prompt-shown-p))
      (goto-char (point-max))
      (insert "typed")
      (opencode-output-notice "A notice")
      (opencode-test-dispatch (opencode-test-inbox-events session "msg_lift_o" "Asked *elsewhere*"))
      (opencode-test-check-parsers)
      (should (opencode-test-face-at "here*" 'markdown-ts-emphasis))
      (should (opencode-test-face-at "elsewhere*" 'markdown-ts-emphasis))
      (should (equal (buffer-substring-no-properties (opencode-pmark) (point-max)) "typed")))))

(ert-deftest opencode-test-parsers-follow-blocks ()
  "Expanding and collapsing blocks deletes the parsers of the text they remove."
  (opencode-test-with-parsed-session (opencode-test-long-transcript 4)
    (let ((count (opencode-test-check-parsers))
          (blocks nil))
      (maphash (lambda (_ overlay) (when (overlay-get overlay 'opencode-collapsible) (push overlay blocks)))
               opencode--blocks)
      (should blocks)
      (dotimes (_ 2)
        (mapc #'opencode-expand-block blocks)
        (should (>= (opencode-test-check-parsers) count))
        (mapc #'opencode-collapse-block blocks)
        (should (= (opencode-test-check-parsers) count))))))

(ert-deftest opencode-test-parsers-once-per-region ()
  "Streaming many turns creates one parser per region, which fontifying again keeps."
  (opencode-test-with-parsed-session nil
    (let ((session opencode-session-id)
          (created 0)
          (create (symbol-function 'treesit-parser-create)))
      (cl-letf (((symbol-function 'treesit-parser-create)
                 (lambda (language &optional buffer no-reuse tag)
                   (when (eq tag 'embedded) (cl-incf created))
                   (funcall create language buffer no-reuse tag))))
        (dotimes (n 20)
          (opencode-test-dispatch (opencode-test-inbox-events session (format "msg_m%02du" n) "Next **one**"))
          (opencode-test-turn session "started")
          (opencode-test-dispatch
           (opencode-test-stream-fontified session (format "msg_m%02da" n)
                                 (format "Part *%d*.\n\nSee `x`.\n\n- a\n- b" n)))
          (opencode-test-turn session "succeeded")
          (goto-char (point-max))
          (insert "draft")
          (opencode-output-notice (format "Notice %d" n))
          (let ((inhibit-read-only t))
            (delete-region (opencode-pmark) (point-max))))
        (should (= (opencode-test-check-parsers) created))
        (let ((parsers (treesit-parser-list nil nil 'embedded)))
          (font-lock-flush)
          (opencode-test-check-parsers)
          (should (equal (treesit-parser-list nil nil 'embedded) parsers)))))))

(defun opencode-test-finished-parsers ()
  "Return how many local parsers parse the finished text, before `opencode-synced-end'."
  (seq-count (lambda (overlay) (<= (overlay-end overlay) opencode-synced-end))
             (opencode-test-parser-overlays)))

(ert-deftest opencode-test-parsers-dropped-when-finished ()
  "The parsers of finished text are deleted once it is fontified; its faces stay.
Those of a message still written stay until it ends, and those of text
fontified again are deleted again."
  (opencode-test-with-held-timers
    (opencode-test-with-parsed-session (opencode-test-long-transcript 3)
      (let ((session opencode-session-id))
        (should (> (opencode-test-finished-parsers) 0))
        (opencode-test-run-timers)
        (should (= (opencode-test-finished-parsers) 0))
        (should (opencode-test-face-at "notes.txt" 'markdown-ts-code-span))
        ;; A turn being written keeps the parsers of its text.
        (opencode-test-dispatch (opencode-test-inbox-events session "msg_f00u" "Next **one**"))
        (opencode-test-turn session "started")
        (let ((ending (opencode-test-stream-fontified session "msg_f00a" "Part *one*.\n\nSee `y`.")))
          (opencode-test-run-timers)
          (let ((finished (marker-position opencode-synced-end)))
            (should (seq-some (lambda (overlay) (> (overlay-end overlay) finished))
                              (opencode-test-parser-overlays))))
          (should (= (opencode-test-finished-parsers) 0))
          (opencode-test-dispatch ending)
          (opencode-test-turn session "succeeded"))
        (opencode-test-run-timers)
        (should (= (opencode-test-finished-parsers) 0))
        (should (opencode-test-face-at "one*." 'markdown-ts-emphasis))
        (should (opencode-test-face-at "y`" 'markdown-ts-code-span))
        ;; Fontified again, the text gets parsers anew, deleted again.
        (font-lock-flush)
        (font-lock-ensure)
        (should (> (opencode-test-finished-parsers) 0))
        (should (opencode-test-face-at "one*." 'markdown-ts-emphasis))
        (opencode-test-run-timers)
        (should (= (opencode-test-finished-parsers) 0))
        (opencode-test-check-parsers t)))))

(ert-deftest opencode-test-parsers-follow-deleted-text ()
  "Deleting text deletes the parsers of the regions it held; changing its properties does not."
  (opencode-test-with-parsed-session (opencode-test-long-transcript 2)
    (let* ((count (opencode-test-check-parsers))
           (start (opencode-test-find "Part 1:"))
           (end (+ start (length "Part 1:"))))
      (let ((inhibit-read-only t))
        (put-text-property start end 'opencode-test t)
        (should (= (length (treesit-parser-list nil nil 'embedded)) count))
        (delete-region start end))
      (should (= (length (treesit-parser-list nil nil 'embedded)) (1- count)))
      (opencode-test-check-parsers))))

(defun opencode-test-orphan-parsers ()
  "Leave orphan parsers as earlier versions did, in a buffer they set up.
Every local parser loses its overlay, and one gets two overlays, as
`remove-overlays' splitting one makes them.  Return how many parsers
there were."
  (let ((count (length (treesit-parser-list nil nil 'embedded)))
        (overlays (opencode-test-parser-overlays)))
    (let ((overlay (car overlays)))
      (move-overlay (copy-overlay overlay) (overlay-start overlay) (1+ (overlay-start overlay))))
    (mapc #'delete-overlay (cdr overlays))
    (setq opencode--parsers-checked nil)
    (remove-hook 'before-change-functions #'opencode--parsers-before-change t)
    (remove-hook 'after-change-functions #'opencode--parsers-after-change t)
    (font-lock-flush)
    count))

(ert-deftest opencode-test-parsers-pruned-once-when-fontified ()
  "A buffer holding orphan parsers prunes them from timers once first fontified."
  (opencode-test-with-held-timers
    (opencode-test-with-parsed-session (opencode-test-long-transcript 5)
      (let ((count (opencode-test-orphan-parsers))
            (opencode--prune-slice 0))
        (font-lock-ensure)
        (should (> (length (treesit-parser-list nil nil 'embedded)) count))
        (should (memq #'opencode--parsers-after-change after-change-functions))
        ;; One parser a slice, as none fits in no time.
        (should (> (opencode-test-run-timers) 1))
        (should (= (opencode-test-check-parsers) count))
        (should (opencode-test-face-at "notes.txt" 'markdown-ts-code-span))
        (font-lock-flush)
        (font-lock-ensure)
        ;; Fontified again, it deletes the parsers of its finished text
        ;; anew, and prunes nothing.
        (should-not (seq-some (lambda (timer) (eq (timer--function timer) #'opencode--prune-parsers-slice))
                              opencode-test-timers))))))

(ert-deftest opencode-test-parsers-pruned-by-command ()
  "`opencode-session-prune-parsers' deletes the orphan parsers and says how many."
  (opencode-test-with-parsed-session (opencode-test-long-transcript 3)
    (let ((count (opencode-test-orphan-parsers)))
      (opencode-test-with-held-timers
        (font-lock-ensure))
      (let ((orphans (- (length (treesit-parser-list nil nil 'embedded)) count)))
        (should (> orphans 0))
        (should (= (opencode-session-prune-parsers) orphans)))
      (should (= (opencode-test-check-parsers) count))
      (should (= (opencode-session-prune-parsers) 0)))))

(ert-deftest opencode-test-parsers-pruned-by-reload ()
  "Reloading a buffer that holds orphan parsers deletes them."
  (opencode-test-with-held-timers
    (opencode-test-with-parsed-session (opencode-test-long-transcript 3)
      (let ((count (opencode-test-orphan-parsers)))
        (opencode-session-reload)
        (opencode-test-run-timers)
        (should (= (opencode-test-check-parsers) count))))))

(ert-deftest opencode-test-parsers-dropped-on-screen ()
  "Redisplay fontifies what windows show, and the parsers of finished text then go.
The text keeps its faces and its layout, scrolled to or fontified anew."
  :tags '(gui)
  (skip-unless (display-graphic-p))
  (opencode-test-with-transcript (opencode-test-long-transcript 30)
    (let ((buffer (opencode-test-open (opencode-test-session)))
          (window (selected-window)))
      (save-window-excursion
        (set-window-buffer window buffer)
        (with-current-buffer buffer
          (cl-flet ((settle ()
                      (redisplay t)
                      ;; The timers deleting parsers run.
                      (sit-for 0.1)
                      (redisplay t))
                    (layout ()
                      (save-excursion
                        (goto-char (window-start window))
                        (search-forward "notes.txt")
                        (list (posn-x-y (posn-at-point (match-beginning 0) window))
                              (opencode-test-face-at "notes.txt" 'markdown-ts-code-span)))))
            (set-window-point window (point-max))
            (settle)
            (let ((shown (layout)))
              (should (cadr shown))
              (should (= (opencode-test-finished-parsers) 0))
              (should (equal (layout) shown))
              (font-lock-flush)
              (redisplay t)
              (should (> (opencode-test-finished-parsers) 0))
              (should (equal (layout) shown))
              (settle)
              (should (= (opencode-test-finished-parsers) 0))
              (should (equal (layout) shown)))
            ;; Scrolled through, the transcript fontifies and drops them.
            (goto-char (point-min))
            (while (progn (set-window-start window (point))
                          (settle)
                          (zerop (forward-line (window-body-height window)))))
            (should (= (opencode-test-finished-parsers) 0))
            (should (opencode-test-face-at "notes.txt" 'markdown-ts-code-span))
            (opencode-test-check-parsers t)))))))

(ert-deftest opencode-test-parsers-redisplayed ()
  "Redisplay, which fontifies what windows show, leaves one parser per region it parsed.
Turns stream into a window following them, the prompt is lifted over
typed input, a running step is written again by catching up, the
transcript reloads, and the window then scrolls through it all."
  :tags '(gui)
  (skip-unless (display-graphic-p))
  (opencode-test-with-transcript (opencode-test-long-transcript 20)
    (let ((buffer (opencode-test-open (opencode-test-session)))
          (window (selected-window)))
      (save-window-excursion
        (set-window-buffer window buffer)
        (with-current-buffer buffer
          (let ((session opencode-session-id))
            (cl-flet ((show () (set-window-point window (point-max)) (redisplay t)))
              (show)
              (dotimes (n 5)
                (let* ((step (format "msg_g%da" n))
                       (base `((sessionID . ,session) (assistantMessageID . ,step) (ordinal . 0))))
                  (opencode-test-dispatch (opencode-test-inbox-events session (format "msg_g%du" n) "Go *on*"))
                  (opencode-test-turn session "started")
                  (opencode-test-dispatch
                   `(((type . "session.step.started") (data (sessionID . ,session) (assistantMessageID . ,step)))
                     ((type . "session.text.started") (data ,@base))))
                  (dolist (delta (split-string (format "Answer *%d*.\n\nSee `x`.\n\n- a\n- b" n) "\\b" t))
                    (opencode-test-dispatch `(((type . "session.text.delta") (data ,@base (delta . ,delta)))))
                    (show))
                  (if (= n 4)
                      ;; The stream drops in the middle of this one.
                      (progn
                        (setq opencode-test-transcript
                              (append opencode-test-transcript
                                      (list (opencode-test-prompt (format "msg_g%du" n) "Go *on*")
                                            (opencode-test-step step '((type . "text") (text . "Caught *up*.")))
                                            (opencode-test-idle "msg_g4i"))))
                        (opencode-test-reconnect))
                    (opencode-test-dispatch
                     `(((type . "session.text.ended")
                        (data ,@base (text . ,(format "Answer *%d*.\n\nSee `x`.\n\n- a\n- b" n))))))
                    (opencode-test-turn session "succeeded"))
                  (show)
                  (goto-char (point-max))
                  (insert "typed")
                  (show)
                  (opencode-output-notice (format "Notice %d" n))
                  (show)
                  (let ((inhibit-read-only t))
                    (delete-region (opencode-pmark) (point-max)))
                  (opencode-test-check-parsers t)))
              (opencode-session-reload)
              (show)
              (opencode-test-check-parsers t)
              (goto-char (point-min))
              (while (progn (set-window-start window (point))
                            (redisplay t)
                            (zerop (forward-line (window-body-height window)))))
              (opencode-test-check-parsers t)
              (should (opencode-test-face-at "up*." 'markdown-ts-emphasis))
              (should (opencode-test-face-at "notes.txt" 'markdown-ts-code-span)))))))))

(provide 'opencode-parsers-test)
;;; opencode-parsers-test.el ends here
