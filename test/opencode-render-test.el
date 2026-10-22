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

(defun opencode-test-type (string)
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
      (opencode-test-type "hi")
      (should (= (point-max) (+ pmark 2)))
      (should-not (opencode-test-hidden pmark (point-max))))))

(ert-deftest opencode-test-input-markup-shown ()
  "Markdown typed in the input keeps its delimiters in view, and text typed after them shows."
  (opencode-test-with-input
    (let ((pmark (opencode-pmark)))
      (opencode-test-type "see `x` and **y**")
      (font-lock-ensure)
      (should-not (opencode-test-hidden pmark (point-max)))
      (should-not (text-property-not-all pmark (point-max) 'display nil))
      (opencode-test-type " more")
      (should-not (opencode-test-hidden pmark (point-max)))
      (font-lock-ensure)
      (should (memq 'opencode-prompt (ensure-list (get-text-property (1+ pmark) 'face)))))))

(ert-deftest opencode-test-transcript-markup-hidden ()
  "Markup before the input stays hidden: sent prompts read as quotes, their \"> \" unseen."
  (opencode-test-with-input
    (let ((prompt (marker-position opencode--prompt-start)))
      (opencode-test-type "`x`")
      (font-lock-ensure)
      (should (seq-some (lambda (position)
                          (and (eq (get-text-property position 'invisible) 'markdown-ts--markup)
                               (invisible-p position)))
                        (number-sequence (point-min) (1- prompt)))))))

(provide 'opencode-render-test)
;;; opencode-render-test.el ends here
