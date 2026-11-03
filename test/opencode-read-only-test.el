;;; opencode-read-only-test.el --- Tests of the read-only transcript  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The transcript, everything before the process mark, is read-only to
;; the user: typing or deleting there does nothing and says nothing,
;; other edits change nothing, killing copies, while the input stays
;; fully editable, its first character included.  Every writer of the
;; package still writes: streaming, blocks and groups, the pending
;; area, tables, a reload, a catch-up and a long transcript.  A buffer
;; an earlier version rendered becomes read-only in place.  RET in the
;; transcript quotes the line or the region, block summaries included,
;; without what overlays hide, while links keep their own RET.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-ro-transcript ()
  "Return the current buffer's transcript text, before the process mark."
  (buffer-substring-no-properties (point-min) (opencode-pmark)))

(defun opencode-test-ro-input ()
  "Return the current buffer's input text, after the process mark."
  (buffer-substring-no-properties (opencode-pmark) (point-max)))

(defun opencode-test-ro-check-insertions ()
  "Check that nothing can be inserted in the transcript, nor deleted from it.
Every position where a property changes is tried, and the start; text
inserted at the process mark goes, in the input."
  (let ((pmark (opencode-pmark))
        (positions (list (point-min)))
        (pos (point-min)))
    (while (and (setq pos (next-property-change pos nil pmark)) (< pos pmark))
      (push pos positions))
    (dolist (position positions)
      (should (eq 'refused (condition-case nil
                               (save-excursion (goto-char position) (insert "x") 'inserted)
                             (text-read-only 'refused))))
      (when (< position pmark)
        (should (eq 'refused (condition-case nil
                                 (progn (delete-region position (1+ position)) 'deleted)
                               (text-read-only 'refused))))))
    (when (< (point-min) pmark)
      (let ((input (opencode-test-ro-input)))
        (save-excursion
          (goto-char pmark)
          (insert "typed"))
        (should (equal (opencode-test-ro-input) (concat "typed" input)))
        (delete-region pmark (+ pmark 5))))
    (should-not (opencode-test-unprotected (current-buffer)))))

(defmacro opencode-test-ro-quietly (&rest body)
  "Run BODY, failing on any message or ding."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'message)
              (lambda (format &rest arguments)
                (unless (member format '(nil ""))
                  (ert-fail (list 'message (apply #'format-message format arguments))))))
             ((symbol-function 'ding) (lambda (&rest _) (ert-fail 'ding))))
     ,@body))

(defun opencode-test-ro-exchange ()
  "Send a prompt in the current buffer and stream an answer with a link."
  (goto-char (point-max))
  (insert "hello")
  (should-not (opencode-test-press (current-buffer) "RET"))
  (opencode-test-answer opencode-session-id "msg_ro_answer"
                        "An answer line, see https://example.com/page\n\nAnother paragraph"))

;;;; Typing and editing

(ert-deftest opencode-test-read-only-typing-does-nothing ()
  "Typing, deleting and yanking in the transcript change nothing and say nothing."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (opencode-test-ro-exchange)
      (goto-char (point-max))
      (insert "draft")
      (let ((transcript (opencode-test-ro-transcript))
            (kill-ring (list "pasted"))
            (kill-ring-yank-pointer nil))
        (setq kill-ring-yank-pointer kill-ring)
        (dolist (spot (list (point-min) (opencode-test-find "answer") (opencode-test-find "hello")
                            (opencode-test-find "Another") (1- (opencode-pmark))))
          (dolist (keys '("a" "/" "@" "C-j" "DEL" "C-d" "<deletechar>" "C-y" "C-q x" "C-u 3 DEL" "SPC"))
            (goto-char spot)
            (opencode-test-ro-quietly
              (should-not (opencode-test-press (current-buffer) keys)))
            (should (equal (opencode-test-ro-transcript) transcript))
            (should (equal (opencode-test-ro-input) "draft"))))
        ;; DEL at the input's start would delete the transcript's last character.
        (goto-char (opencode-pmark))
        (opencode-test-ro-quietly
          (should-not (opencode-test-press (current-buffer) "DEL")))
        (should (equal (opencode-test-ro-transcript) transcript))
        ;; Typing over a region of the transcript, or deleting it, as well.
        (let ((transient-mark-mode t))
          (dolist (delete-selection '(nil t))
            (delete-selection-mode (if delete-selection 1 -1))
            (dolist (keys '("x" "DEL" "C-d"))
              (goto-char (opencode-test-find "answer"))
              (push-mark (point) t t)
              (goto-char (+ (opencode-test-find "Another") 3))
              (opencode-test-ro-quietly
                (should-not (opencode-test-press (current-buffer) keys)))
              (deactivate-mark)
              (should (equal (opencode-test-ro-transcript) transcript))
              (should (equal (opencode-test-ro-input) "draft"))))
          (delete-selection-mode -1))))))

(ert-deftest opencode-test-read-only-other-edits ()
  "Other edits change nothing in the transcript; killing copies without an error."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (opencode-test-ro-exchange)
      (let ((transcript (opencode-test-ro-transcript))
            (kill-ring nil)
            (kill-ring-yank-pointer nil)
            (transient-mark-mode t))
        ;; Killing copies, as `kill-read-only-ok' says.
        (goto-char (opencode-test-find "answer"))
        (push-mark (point) t t)
        (goto-char (+ (opencode-test-find "answer") 11))
        (should-not (opencode-test-press (current-buffer) "C-w"))
        (should (equal (car kill-ring) "answer line"))
        (goto-char (opencode-test-find "Another"))
        (should-not (opencode-test-press (current-buffer) "C-k"))
        (should (equal (car kill-ring) "Another paragraph"))
        (goto-char (opencode-test-find "Another"))
        (should-not (opencode-test-press (current-buffer) "M-d"))
        (should (equal (car kill-ring) "Another"))
        (goto-char (opencode-test-find "answer"))
        (push-mark (point) t t)
        (goto-char (+ (opencode-test-find "answer") 6))
        (should-not (opencode-test-press (current-buffer) "M-w"))
        (should (equal (car kill-ring) "answer"))
        (should (equal (opencode-test-ro-transcript) transcript))
        ;; Transposing, changing case, undoing: refused or nothing to do.
        (dolist (keys '("C-t" "M-t" "M-u" "M-c" "C-x C-t" "C-/" "M-SPC" "M-\\" "C-o" "C-M-o"))
          (goto-char (+ (opencode-test-find "answer") 2))
          (let ((error (opencode-test-press (current-buffer) keys)))
            (should (or (null error)
                        (memq (car error) '(text-read-only user-error))))
            (should (equal (opencode-test-ro-transcript) transcript))))))))

(ert-deftest opencode-test-read-only-input-editable ()
  "The input stays fully editable, its first character included, and RET sends it."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open-empty session)
        (opencode-test-ro-exchange)
        (let ((kill-ring nil)
              (kill-ring-yank-pointer nil)
              (transient-mark-mode t))
          (goto-char (opencode-pmark))
          (should-not (opencode-test-press (current-buffer) "w o r l d"))
          (should (equal (opencode-test-ro-input) "world"))
          (goto-char (opencode-pmark))
          (should-not (opencode-test-press (current-buffer) "X"))
          (should (equal (opencode-test-ro-input) "Xworld"))
          (goto-char (opencode-pmark))
          (should-not (opencode-test-press (current-buffer) "C-d"))
          (should (equal (opencode-test-ro-input) "world"))
          (goto-char (1+ (opencode-pmark)))
          (should-not (opencode-test-press (current-buffer) "DEL"))
          (should (equal (opencode-test-ro-input) "orld"))
          (goto-char (+ 2 (opencode-pmark)))
          (should-not (opencode-test-press (current-buffer) "C-t"))
          (should (equal (opencode-test-ro-input) "olrd"))
          ;; Text copied from the transcript yanks there editable.
          (goto-char (opencode-test-find "answer"))
          (push-mark (point) t t)
          (goto-char (+ (opencode-test-find "answer") 6))
          (should-not (opencode-test-press (current-buffer) "M-w"))
          (goto-char (opencode-pmark))
          (should-not (opencode-test-press (current-buffer) "C-y SPC"))
          (should (equal (opencode-test-ro-input) "answer olrd"))
          (should-not (text-property-not-all (opencode-pmark) (point-max) 'opencode-transcript nil))
          (should-not (opencode-test-press (current-buffer) "DEL"))
          (should (equal (opencode-test-ro-input) "answerolrd"))
          (goto-char (opencode-pmark))
          (push-mark (point) t t)
          (goto-char (+ (opencode-pmark) 6))
          (should-not (opencode-test-press (current-buffer) "C-w"))
          (should (equal (opencode-test-ro-input) "olrd"))
          ;; While a turn runs, the prompt string is gone: the input
          ;; follows the output straight.
          (opencode-test-turn session "started")
          (opencode-test-answer session "msg_ro_running" "Working on it")
          (goto-char (opencode-pmark))
          (should-not (opencode-test-press (current-buffer) "n e w SPC"))
          (should (equal (opencode-test-ro-input) "new olrd"))
          (opencode-test-ro-check-insertions)
          (opencode-test-turn session "ended")
          (goto-char (point-max))
          (let ((prompts (length (opencode-test-requests-to 'post "/prompt"))))
            (should-not (opencode-test-press (current-buffer) "RET"))
            (should (= (length (opencode-test-requests-to 'post "/prompt")) (1+ prompts)))))
        (opencode-test-ro-check-insertions)))))

;;;; Writers

(ert-deftest opencode-test-read-only-writers-write ()
  "Every writer still writes the transcript, which stays read-only to the user.
Streaming text and a table, blocks expanded and collapsed, groups, the
pending area, a reload, a catch-up and a long transcript."
  (opencode-test-with-transcript (opencode-test-long-transcript 40)
    (let* ((session (opencode-test-session))
           (buffer (opencode-test-open session)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "draft")
        (opencode-test-ro-check-insertions)
        (should (opencode-test-groups-p))
        ;; Blocks and groups expanded and collapsed.
        (dolist (overlay (seq-filter (lambda (overlay) (overlay-get overlay 'opencode-collapsible))
                                     (overlays-in (point-min) (opencode-pmark))))
          (opencode-expand-block overlay))
        (opencode-test-ro-check-insertions)
        (dolist (overlay (seq-filter (lambda (overlay) (overlay-get overlay 'opencode-collapsible))
                                     (overlays-in (point-min) (opencode-pmark))))
          (opencode-collapse-block overlay))
        (opencode-test-ro-check-insertions)
        ;; A turn streaming a table, with a tool call; prompts pending.
        (opencode-test-turn session "started")
        (opencode-test-dispatch (list `((type . "session.inbox.enqueued")
                                        (data (sessionID . ,session) (inboxID . "msg_ro_pending")
                                              (item (type . "user") (delivery . "steer")
                                                    (payload (text . "pending one")))))))
        (opencode-test-answer session "msg_ro_table" "Here:\n\n| a | b |\n|-|-|\n| 1 | 2 |\n\nDone.")
        (should (string-search "pending one" (opencode-inbox-area-string)))
        (opencode-test-ro-check-insertions)
        (opencode-test-dispatch (list `((type . "session.inbox.delivered")
                                        (data (sessionID . ,session) (inboxID . "msg_ro_pending")))))
        (opencode-test-turn session "ended")
        (opencode-test-ro-check-insertions)
        (should (equal (opencode-test-ro-input) "draft"))
        ;; Reverted, then caught up with turns the service adds.
        (opencode-session-reload)
        (opencode-test-ro-check-insertions)
        (setq opencode-test-transcript (opencode-test-long-transcript 45))
        (opencode-test-reconnect)
        (should (string-search "Look at part 44" (opencode-test-ro-transcript)))
        (opencode-test-ro-check-insertions)
        (should (equal (opencode-test-ro-input) "draft"))))))

(defun opencode-test-groups-p ()
  "Return non-nil when the current buffer shows a group of blocks."
  (seq-some #'opencode-group-p (overlays-in (point-min) (point-max))))

(ert-deftest opencode-test-read-only-upgrades-earlier-buffers ()
  "A buffer an earlier version rendered becomes read-only in place, a slice at a time."
  (opencode-test-with-transcript (opencode-test-long-transcript 30)
    (let ((buffer (opencode-test-open (opencode-test-session))))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "draft")
        (goto-char (opencode-test-find "Look at part 12"))
        (let ((point (point))
              (transcript (opencode-test-ro-transcript))
              (opencode--protect-slice 1000))
          ;; As an earlier version left it: writable, no hooks.
          (let ((inhibit-read-only t))
            (with-silent-modifications
              (remove-list-of-text-properties (point-min) (point-max) '(opencode-transcript))))
          (remove-hook 'after-change-functions #'opencode--protect-change t)
          (remove-hook 'pre-command-hook #'opencode--quiet-transcript-edit t)
          (kill-local-variable 'char-property-alias-alist)
          (kill-local-variable 'kill-read-only-ok)
          (should (text-property-any (point-min) (opencode-pmark) 'opencode-transcript nil))
          (opencode-test-with-held-timers
            (opencode-read-only-upgrade)
            (should opencode-test-timers)
            ;; The first slice at once, the rest later.
            (should (get-text-property (point-min) 'read-only))
            (should (text-property-any (point-min) (opencode-pmark) 'opencode-transcript nil))
            (while (text-property-any (point-min) (opencode-pmark) 'opencode-transcript nil)
              (opencode-test-run-timers)))
          (should (= (point) point))
          (should (equal (opencode-test-ro-transcript) transcript))
          (should (equal (opencode-test-ro-input) "draft"))
          (opencode-test-ro-check-insertions)
          (opencode-test-ro-quietly
            (should-not (opencode-test-press buffer "a")))
          (should (equal (opencode-test-ro-transcript) transcript))
          ;; Upgraded already, it is left alone.
          (opencode-test-with-held-timers
            (opencode-read-only-upgrade)
            (should-not opencode-test-timers)))))))

;;;; Quoting

(defmacro opencode-test-ro-recording (&rest body)
  "Run BODY with `opencode-old-input-function' recording its texts in `recorded'."
  (declare (indent 0) (debug t))
  `(let* ((recorded nil)
          (opencode-old-input-function (lambda (text _buffer) (push text recorded))))
     ,@body))

(ert-deftest opencode-test-read-only-ret-quotes-summaries ()
  "RET on a block's or a group's summary quotes its line as shown; TAB still toggles."
  (opencode-test-with-transcript (opencode-test-long-transcript 3)
    (let ((buffer (opencode-test-open (opencode-test-session))))
      (with-current-buffer buffer
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (opencode-test-ro-recording
            ;; A group of an earlier turn, collapsed.
            (let* ((group (seq-find #'opencode-group-p (overlays-in (point-min) (point-max))))
                   (line (buffer-substring-no-properties (overlay-start group)
                                                         (save-excursion (goto-char (overlay-start group))
                                                                         (pos-eol)))))
              (should-not (overlay-get group 'opencode-expanded))
              (goto-char (+ (overlay-start group) 3))
              (should (eq (key-binding (kbd "RET")) 'opencode-send-input))
              (should-not (opencode-test-press buffer "RET"))
              (should (equal recorded (list (string-trim-right line))))
              (should-not (text-properties-at 0 (car recorded)))
              (should-not (overlay-get group 'opencode-expanded))
              ;; A region over the group leaves out the blocks it hides.
              (setq recorded nil)
              (let ((transient-mark-mode t))
                (goto-char (opencode-test-find "Look at part 0"))
                (push-mark (point) t t)
                (goto-char (+ (opencode-test-find "Part 0:") 7))
                (should-not (opencode-test-press buffer "RET"))
                (deactivate-mark))
              (should (string-search (string-trim line) (car recorded)))
              (should (string-prefix-p "Look at part 0" (car recorded)))
              (should (string-suffix-p "Part 0:" (car recorded)))
              (should-not (string-search "Reading" (car recorded)))
              (should-not (string-search "make check" (car recorded)))
              ;; TAB expands it; RET on a block's summary quotes that line.
              (goto-char (overlay-start group))
              (should-not (opencode-test-press buffer "TAB"))
              (should (overlay-get group 'opencode-expanded))
              (setq recorded nil)
              (let ((block (opencode-block "call_000000s")))
                (goto-char (1+ (overlay-start block)))
                (should-not (opencode-test-press buffer "RET"))
                (should-not (overlay-get block 'opencode-expanded))
                (should (string-search "make check" (car recorded)))
                (should-not (string-search "\n" (car recorded)))
                (should-not (string-search "▸" (car recorded)))))
            ;; A prompt's line leaves out the hidden prompt string.
            (setq recorded nil)
            (goto-char (opencode-test-find "Look at part 1"))
            (should-not (opencode-test-press buffer "RET"))
            (should (equal recorded '("Look at part 1")))
            ;; The agent's Markdown stays as written, its markup too.
            (setq recorded nil)
            (goto-char (opencode-test-find "uses `notes"))
            (should-not (opencode-test-press buffer "RET"))
            (should (equal recorded '("- uses `notes.txt`")))))))))

(ert-deftest opencode-test-read-only-link-keeps-its-ret ()
  "RET on a link of the agent's text follows it, quoting nothing."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (opencode-test-ro-exchange)
      (font-lock-ensure)
      (let ((browsed nil))
        (opencode-test-ro-recording
          (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (push url browsed))))
            (goto-char (+ (opencode-test-find "https://example.com") 4))
            (should (eq (key-binding (kbd "RET")) 'push-button))
            (should-not (opencode-test-press (current-buffer) "RET"))
            (should (equal browsed '("https://example.com/page")))
            (should-not recorded)
            (goto-char (opencode-test-find "An answer"))
            (should-not (opencode-test-press (current-buffer) "RET"))
            (should (equal recorded '("An answer line, see https://example.com/page")))))))))

(provide 'opencode-read-only-test)
;;; opencode-read-only-test.el ends here
