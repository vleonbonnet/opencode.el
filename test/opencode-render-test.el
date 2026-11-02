;;; opencode-render-test.el --- Tests of typing into a session's input  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Typed text is checked as `self-insert-command' leaves it, before
;; font-lock runs again: redisplay never fontifies text that is
;; invisible, so text that arrives hidden stays hidden on screen.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-self-insert (string)
  "Insert STRING as typing does, one `self-insert-command' per character."
  (dolist (char (string-to-list string))
    (let ((last-command-event char))
      (self-insert-command 1))))

(defun opencode-test-hidden (start end)
  "Return the positions from START up to END that are not displayed."
  (seq-filter #'invisible-p (number-sequence start (1- end))))

(defmacro opencode-test-with-input (&rest body)
  "Run BODY in a fontified session buffer with point at its empty input."
  (declare (indent 0) (debug t))
  `(opencode-test-with-server
     (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
       (font-lock-mode 1)
       (font-lock-ensure)
       (goto-char (point-max))
       ,@body)))

(ert-deftest opencode-test-typing-after-prompt-shown ()
  "Text typed after the hidden prompt shows, though markdown-ts hides the prompt as quote markup."
  (opencode-test-with-input
    (let ((prompt (marker-position opencode--prompt-start))
          (pmark (opencode-pmark)))
      (should (equal (opencode-test-hidden prompt pmark) (list prompt (1+ prompt))))
      (opencode-test-self-insert "hi")
      (should (= (point-max) (+ pmark 2)))
      (should-not (opencode-test-hidden pmark (point-max))))))

(ert-deftest opencode-test-input-markup-shown ()
  "Markdown typed in the input keeps its delimiters in view, and text typed after them shows."
  (opencode-test-with-input
    (let ((pmark (opencode-pmark)))
      (opencode-test-self-insert "see `x` and **y**")
      (font-lock-ensure)
      (should-not (opencode-test-hidden pmark (point-max)))
      (should-not (text-property-not-all pmark (point-max) 'display nil))
      (opencode-test-self-insert " more")
      (should-not (opencode-test-hidden pmark (point-max)))
      (font-lock-ensure)
      (should (memq 'opencode-prompt (ensure-list (get-text-property (1+ pmark) 'face)))))))

(ert-deftest opencode-test-transcript-markup-hidden ()
  "Markup before the input stays hidden: sent prompts read as quotes, their \"> \" unseen."
  (opencode-test-with-input
    (let ((prompt (marker-position opencode--prompt-start)))
      (opencode-test-self-insert "`x`")
      (font-lock-ensure)
      (should (seq-some (lambda (position)
                          (and (eq (get-text-property position 'invisible) 'markdown-ts--markup)
                               (invisible-p position)))
                        (number-sequence (point-min) (1- prompt)))))))

(ert-deftest opencode-test-kept-display-survives-fontification ()
  "A display kept by `opencode-keep-display' outlasts fontification, which drops any other."
  (opencode-test-with-input
    (let ((inhibit-read-only t)
          (spacer '(space :width (5))))
      (goto-char (point-min))
      (insert "kept: x\nlost: x\n")
      (put-text-property 6 7 'display spacer)
      (put-text-property 14 15 'display spacer)
      (opencode-keep-display 1 9)
      (opencode-mark-fontify 1 17 'verbatim)
      (font-lock-fontify-region (point-min) (point-max))
      (should (equal (get-text-property 6 'display) spacer))
      (should-not (get-text-property 14 'display))
      (should-not (text-property-not-all 7 17 'opencode-display nil))
      ;; Keeping again forgets a display since removed.
      (remove-text-properties 1 9 '(display nil))
      (opencode-keep-display 1 9)
      (font-lock-fontify-region (point-min) (point-max))
      (should-not (text-property-not-all 1 17 'display nil)))))

(ert-deftest opencode-test-kept-display-not-typed-on ()
  "Text typed right after a kept display does not take it on."
  (opencode-test-with-input
    (let ((pmark (opencode-pmark))
          (spacer '(space :width (5))))
      (opencode-test-self-insert "a")
      (with-silent-modifications
        (put-text-property pmark (1+ pmark) 'display spacer))
      (opencode-keep-display pmark (1+ pmark))
      (opencode-test-self-insert "b")
      (should (equal (get-text-property pmark 'opencode-display) spacer))
      (should-not (get-text-property (1+ pmark) 'opencode-display)))))

;;;; Writing long transcripts

(defun opencode-test-replay-seconds (messages)
  "Return the fewest seconds of three that writing MESSAGES whole takes.
Garbage is collected rarely meanwhile, as in a configuration with a
large `gc-cons-threshold', where markers left behind slow every
insertion until a collection."
  (let ((best nil))
    (dotimes (_ 3)
      (with-temp-buffer
        (opencode-session-mode)
        (opencode--make-process (current-buffer))
        (setq opencode-session-id "ses_long")
        (buffer-enable-undo)
        (garbage-collect)
        (let ((gc-cons-threshold (* 64 1024 1024))
              (start (float-time)))
          (opencode-replay-messages messages)
          (let ((seconds (- (float-time) start)))
            (setq best (if best (min best seconds) seconds))))
        ;; Only the input's edits are undone, and there is no input.
        (should-not buffer-undo-list)))
    best))

(ert-deftest opencode-test-replay-time-linear ()
  "Writing a transcript takes time proportional to its length.
Every output change shifted the whole undo list, which every replayed
prompt grew, and each write left markers that every later insertion
adjusted: four times the parts took sixteen times as long."
  (opencode-test-with-server
    (let ((small (opencode-test-replay-seconds (opencode-test-long-transcript 200)))
          (large (opencode-test-replay-seconds (opencode-test-long-transcript 800))))
      (should (< large (* 8 small))))))

(ert-deftest opencode-test-undo-keeps-only-prompt-edits ()
  "Undo after output undoes what was typed, never the transcript.
Rendering another client's prompt runs Comint's input sending, whose
records undo would otherwise replay over the transcript."
  (opencode-test-with-input
    (let ((session opencode-session-id)
          (transcript (buffer-substring-no-properties (point-min) (opencode-pmark))))
      (buffer-enable-undo)
      (undo-boundary)
      (opencode-test-self-insert "draft")
      (undo-boundary)
      (opencode-test-dispatch (opencode-test-inbox-events session "msg_undo_other" "Asked elsewhere."))
      (opencode-test-answer session "msg_undo_answer" "Answered.")
      (let ((shown (buffer-substring-no-properties (point-min) (opencode-pmark))))
        (should (string-prefix-p transcript shown))
        (should (string-search "Asked elsewhere." shown))
        (should (equal (buffer-substring-no-properties (opencode-pmark) (point-max)) "draft"))
        (let ((last-command nil))
          (undo))
        (should (equal (buffer-substring-no-properties (opencode-pmark) (point-max)) ""))
        (should (equal (buffer-substring-no-properties (point-min) (opencode-pmark)) shown))))))

(provide 'opencode-render-test)
;;; opencode-render-test.el ends here
