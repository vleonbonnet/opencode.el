;;; opencode-recap-test.el --- Where a session stands, above its input  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Once a turn ended, a session buffer shows the line
;; `opencode-session-summary-function' tells, just above its input, and
;; nothing while a turn runs.  These tests check where it shows, when
;; it is asked for, and that it changes nothing else: the transcript and
;; its read-only text, the pending area, groups, catching up and
;; reloading, the input and line motion.

;;; Code:

(require 'opencode-test-helpers)

(defvar opencode-test-recap-value nil
  "What the summary function of `opencode-test-with-recap' returns.")

(defvar opencode-test-recap-asked 0
  "How many times the summary function of `opencode-test-with-recap' was called.")

(defmacro opencode-test-with-recap (value &rest body)
  "Run BODY with a summary function returning VALUE, which BODY may change.
VALUE is `opencode-test-recap-value', and the calls are counted in
`opencode-test-recap-asked'."
  (declare (indent 1) (debug t))
  `(let* ((opencode-test-recap-value ,value)
          (opencode-test-recap-asked 0)
          (opencode-session-summary-recap nil)
          (opencode-session-summary-icon "※")
          (opencode-session-summary-function
           (lambda ()
             (cl-incf opencode-test-recap-asked)
             opencode-test-recap-value)))
     ,@body))

(defun opencode-test-recap ()
  "Return what the current buffer shows of where its session stands, or \"\"."
  (or (opencode-session-summary-string) ""))

(defun opencode-test-recap-redisplay ()
  "Run what redisplay runs first in the current buffer, as batch Emacs never redisplays."
  (run-hook-with-args 'pre-redisplay-functions (selected-window)))

(defun opencode-test-recap-turn (n)
  "Return the messages of turn N: a prompt, a step with two tool calls and text, its end."
  (list (opencode-test-prompt (format "msg_r%02du" n) (format "Question %d" n))
        (opencode-test-step (format "msg_r%02da" n)
                            `((type . "tool") (id . ,(format "call_r%02dr" n)) (name . "read")
                              (state (status . "completed") (input (path . "README.md"))
                                     (content ((type . "text") (text . "README.md\n")))))
                            `((type . "tool") (id . ,(format "call_r%02ds" n)) (name . "shell")
                              (state (status . "completed") (input (command . "make check"))
                                     (content ((type . "text") (text . "ok")))))
                            `((type . "text") (text . ,(format "Answer %d." n))))
        (opencode-test-idle (format "msg_r%02di" n))))

(defun opencode-test-recap-at-prompt-p ()
  "Return non-nil when the line shows after the idle prompt, after the pending area."
  (and (overlay-buffer opencode--recap-overlay)
       (opencode-prompt-shown-p)
       (= (overlay-start opencode--recap-overlay) (opencode-pmark))))

(defun opencode-test-recap-groups ()
  "Return the groups of the current buffer, in order, as (START MEMBERS)."
  (mapcar (lambda (overlay)
            (list (overlay-start overlay) (length (plist-get (opencode-block-data overlay) :members))))
          (sort (seq-filter (lambda (overlay) (and (overlay-get overlay 'opencode-collapsible)
                                                   (opencode-group-p overlay)))
                            (overlays-in (point-min) (point-max)))
                :key #'overlay-start)))

;;;; Showing

(ert-deftest opencode-test-recap-shows-at-idle-prompt ()
  "An idle session shows its summary just above its input, as overlay text alone."
  (opencode-test-with-recap '("Done: added search, tests pass" . "Added FTS.\nTests pass.")
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (with-current-buffer (opencode-test-open (opencode-test-session))
        (should (equal (opencode-test-recap) "※ Done: added search, tests pass …\n\n"))
        (should (opencode-test-recap-at-prompt-p))
        (should (= opencode-test-recap-asked 1))
        ;; Not in the text, nor in the input.
        (should-not (string-search "Done: added" (buffer-string)))
        (should (equal (opencode-test-input (current-buffer)) ""))
        ;; Redisplay asks nothing more while nothing changes.
        (opencode-test-recap-redisplay)
        (opencode-test-recap-redisplay)
        (should (= opencode-test-recap-asked 1))))))

(ert-deftest opencode-test-recap-drawn ()
  "Redisplay draws the line after the idle prompt it hides, alone or after the pending area."
  (opencode-test-with-recap '("Done: first" . "A recap.")
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((session (opencode-test-session)))
        (with-current-buffer (opencode-test-open session)
          (should (opencode-prompt-shown-p))
          (should (equal (opencode-test-area-drawn opencode--recap-overlay) '(2 . 2)))
          (opencode-test-dispatch
           `(((type . "session.inbox.enqueued")
              (data (sessionID . ,session) (inboxID . "msg_tui")
                    (item (type . "user") (delivery . "queue") (payload (text . "later")))))))
          (let ((pending (opencode-test-area-drawn opencode--inbox-overlay)))
            (should (cl-plusp (cdr pending)))
            (should (= (car pending) (cdr pending))))
          (should (equal (opencode-test-area-drawn opencode--recap-overlay) '(2 . 2))))))))

(ert-deftest opencode-test-recap-none-without-function ()
  "Without a function, or one telling nothing, nothing shows; one failing tells nothing."
  (opencode-test-with-transcript (opencode-test-recap-turn 1)
    (with-current-buffer (opencode-test-open (opencode-test-session))
      (should-not opencode--recap-overlay)
      (opencode-test-with-recap nil
        (opencode-session-summary-update)
        (should-not opencode--recap-overlay)
        (should (= opencode-test-recap-asked 1))
        (setq opencode-test-recap-value "  \n")
        (opencode-session-summary-update)
        (should-not opencode--recap-overlay))
      (let ((opencode-session-summary-function (lambda () (error "No summary"))))
        (let ((inhibit-message t))
          (opencode-session-summary-update))
        (should-not opencode--recap-overlay)))))

(ert-deftest opencode-test-recap-string-first-line ()
  "A string tells a line, its first; a recap keeps a few lines, blank ones dropped."
  (opencode-test-with-recap "Waiting for approval\nignored"
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (with-current-buffer (opencode-test-open (opencode-test-session))
        (should (equal (opencode-test-recap) "※ Waiting for approval\n\n"))
        (setq opencode-test-recap-value
              (cons "Done" (mapconcat (lambda (n) (format "line %d\n" n)) (number-sequence 1 12) "\n")))
        (opencode-session-summary-update)
        (opencode-toggle-session-summary-recap)
        (should (equal (opencode-test-recap)
                       (concat "※ Done\n"
                               (mapconcat (lambda (n) (format "  line %d\n" n)) (number-sequence 1 8) "")
                               "\n")))))))

(ert-deftest opencode-test-recap-hidden-while-turn-runs ()
  "The line goes as a turn starts, and is asked anew once it ends."
  (opencode-test-with-recap '("Done: first" . nil)
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((session (opencode-test-session)))
        (with-current-buffer (opencode-test-open session)
          (should (string-search "Done: first" (opencode-test-recap)))
          ;; A turn another client started.
          (opencode-test-turn session "started")
          (should-not (opencode-session-summary-string))
          (should-not opencode--recap-overlay)
          (setq opencode-test-recap-value '("Done: second" . nil))
          (opencode-test-answer session "msg_step" "Working on it.")
          (opencode-test-recap-redisplay)
          (should-not (opencode-session-summary-string))
          (let ((asked opencode-test-recap-asked))
            (opencode-test-turn session "succeeded")
            (opencode-test-recap-redisplay)
            (should (opencode-test-recap-at-prompt-p))
            (should (equal (opencode-test-recap) "※ Done: second\n\n"))
            (should (= opencode-test-recap-asked (1+ asked)))))))))

(ert-deftest opencode-test-recap-hidden-once-sent ()
  "Sending a prompt hides the line at once: it never shows after the prompt sent."
  (opencode-test-with-recap '("Done: first" . nil)
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (with-current-buffer (opencode-test-open (opencode-test-session))
        (should (opencode-session-summary-string))
        (goto-char (point-max))
        (insert "next")
        ;; Typing keeps it.
        (should (opencode-test-recap-at-prompt-p))
        (opencode-send-input)
        (should-not (opencode-session-summary-string))
        (opencode-test-recap-redisplay)
        (should-not (opencode-session-summary-string))
        (should (string-search "next" (buffer-substring-no-properties (point-min) (opencode-pmark))))))))

(ert-deftest opencode-test-recap-updated-in-place ()
  "Asked anew, the line changes in place, its overlay kept, points left where they are."
  (opencode-test-with-recap '("Done: first" . nil)
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((buffer (opencode-test-open (opencode-test-session))))
        (with-current-buffer buffer
          (save-window-excursion
            (set-window-buffer (selected-window) buffer)
            (goto-char (point-max))
            (insert "draft")
            (let ((overlay opencode--recap-overlay)
                  (point (point))
                  (window-point (window-point))
                  (tick (buffer-modified-tick)))
              (setq opencode-test-recap-value '("Done: revised" . nil))
              (opencode-session-summary-update buffer)
              (should (eq opencode--recap-overlay overlay))
              (should (equal (opencode-test-recap) "※ Done: revised\n\n"))
              (should (= (point) point))
              (should (= (window-point) window-point))
              (should (= (buffer-modified-tick) tick))
              ;; Told nothing any more, it goes.
              (setq opencode-test-recap-value nil)
              (opencode-session-summary-update buffer)
              (should-not (opencode-session-summary-string))
              (should (equal (opencode-test-input buffer) "draft")))))))))

(ert-deftest opencode-test-recap-unfolds ()
  "A recap shows folded, unfolds on a click, and a new summary folds it again."
  (opencode-test-with-recap '("Done: search" . "Added FTS.\n\n  Tests pass.  ")
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((buffer (opencode-test-open (opencode-test-session))))
        (with-current-buffer buffer
          (should (equal (opencode-test-recap) "※ Done: search …\n\n"))
          (let* ((string (overlay-get opencode--recap-overlay 'before-string))
                 (start (string-search "Done" string)))
            (should (eq (lookup-key (get-text-property start 'keymap string) [mouse-1])
                        #'opencode-session-summary-click))
            (should (string-search "Tests pass." (get-text-property start 'help-echo string))))
          (save-window-excursion
            (set-window-buffer (selected-window) buffer)
            (opencode-session-summary-click
             `(mouse-1 (,(selected-window) ,(marker-position opencode--prompt-start) (0 . 0) 0))))
          (should (equal (opencode-test-recap) "※ Done: search\n  Added FTS.\n  Tests pass.\n\n"))
          (opencode-toggle-session-summary-recap)
          (should (equal (opencode-test-recap) "※ Done: search …\n\n"))
          (opencode-toggle-session-summary-recap)
          (setq opencode-test-recap-value '("Done: more" . "Another recap."))
          (opencode-session-summary-update)
          (should (equal (opencode-test-recap) "※ Done: more …\n\n"))
          (let ((opencode-session-summary-recap t))
            (setq opencode-test-recap-value '("Done: again" . "Shown."))
            (opencode-session-summary-update)
            (should (equal (opencode-test-recap) "※ Done: again\n  Shown.\n\n")))
          (setq opencode-test-recap-value '("Done: none" . nil))
          (opencode-session-summary-update)
          (should-error (opencode-toggle-session-summary-recap) :type 'user-error))))))

;;;; What it leaves alone

(ert-deftest opencode-test-recap-after-pending-area ()
  "With pending prompts, the line shows after them, the pending area as it was."
  (opencode-test-with-recap '("Done: first" . nil)
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((session (opencode-test-session)))
        (with-current-buffer (opencode-test-open session)
          (let ((alone (progn
                         (let ((opencode-session-summary-function nil))
                           (opencode-session-summary-update))
                         (opencode-test-dispatch
                          `(((type . "session.inbox.enqueued")
                             (data (sessionID . ,session) (inboxID . "msg_tui")
                                   (item (type . "user") (delivery . "queue") (payload (text . "later")))))))
                         (opencode-inbox-area-string))))
            (should (string-search "later" alone))
            (opencode-session-summary-update)
            (should (equal (opencode-inbox-area-string) alone))
            (should (= (overlay-start opencode--inbox-overlay) (overlay-start opencode--recap-overlay)))
            ;; Before-strings show by increasing priority: the pending area first.
            (should (< (or (overlay-get opencode--inbox-overlay 'priority) 0)
                       (overlay-get opencode--recap-overlay 'priority)))
            ;; The pending area ends with a blank line, the line needs none before it.
            (should (string-suffix-p "\n\n" alone))
            (should (equal (opencode-test-recap) "※ Done: first\n\n"))
            (opencode-test-dispatch
             `(((type . "session.inbox.cancelled") (data (sessionID . ,session) (inboxID . "msg_tui")))))
            (should-not (opencode-inbox-area-string))
            (should (opencode-test-recap-at-prompt-p))))))))

(ert-deftest opencode-test-recap-after-partial-line ()
  "The line starts after a blank line whatever ends the transcript before it."
  (opencode-test-with-transcript (opencode-test-recap-turn 1)
    (with-current-buffer (opencode-test-open (opencode-test-session))
      (should (equal (opencode-area-lead (point-min)) ""))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert "partial")))
      (should (equal (opencode-area-lead (point-max)) "\n\n")))))

(ert-deftest opencode-test-recap-input-and-motion ()
  "The line takes no point: the input is typed and sent as ever, line motion passes over it."
  (opencode-test-with-recap '("Done: first" . "A recap.")
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((buffer (opencode-test-open (opencode-test-session))))
        (with-current-buffer buffer
          (save-window-excursion
            (set-window-buffer (selected-window) buffer)
            (set-window-start (selected-window) (point-min))
            (goto-char (point-max))
            (insert "first line\nsecond line")
            (cl-flet ((places ()
                        (goto-char (point-max))
                        (let ((places nil))
                          (dotimes (_ 3)
                            (let ((this-command 'previous-line) (last-command 'previous-line))
                              (line-move -1 t))
                            (push (point) places))
                          (dotimes (_ 3)
                            (let ((this-command 'next-line) (last-command 'next-line))
                              (line-move 1 t))
                            (push (point) places))
                          (nreverse places))))
              (should (opencode-session-summary-string))
              (let ((with (places))
                    (pmark (opencode-pmark)))
                ;; Up to the input's first line, then into the transcript.
                (should (>= (nth 0 with) pmark))
                (should (<= (nth 0 with) (+ pmark (length "first line"))))
                (should (< (nth 1 with) (marker-position opencode--prompt-start)))
                (should (opencode-session-summary-string))
                (let ((opencode-session-summary-function nil))
                  (opencode-session-summary-update))
                (should-not (opencode-session-summary-string))
                ;; As without the line.
                (should (equal with (places)))))))))))

(ert-deftest opencode-test-recap-groups-as-without ()
  "A prompt sent while the line shows groups the turn before it as without the line."
  (cl-flet ((groups-after-send (function)
              (opencode-test-with-transcript (append (opencode-test-recap-turn 1) (opencode-test-recap-turn 2))
                (let ((opencode-session-summary-function function))
                  (with-current-buffer (opencode-test-open (opencode-test-session))
                    (should (eq (and (opencode-session-summary-string) t) (and function t)))
                    (let ((before (opencode-test-recap-groups)))
                      (goto-char (point-max))
                      (insert "Question 3")
                      (opencode-send-input)
                      (list before (opencode-test-recap-groups) (opencode-test-text (current-buffer)))))))))
    (let ((with (groups-after-send (lambda () '("Done: second" . "Recap."))))
          (without (groups-after-send nil)))
      (should (= (length (car with)) 1))
      (should (= (length (cadr with)) 2))
      (should (equal with without)))))

(ert-deftest opencode-test-recap-catch-up ()
  "Catching up shows the line at the new idle prompt, asked anew, the transcript as opened afresh."
  (opencode-test-with-recap '("Done: first" . nil)
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let* ((id (opencode-test-session))
             (buffer (opencode-test-open id)))
        (with-current-buffer buffer
          (should (opencode-test-recap-at-prompt-p))
          (setq opencode-test-transcript (append opencode-test-transcript (opencode-test-recap-turn 2))
                opencode-test-recap-value '("Done: second" . nil))
          (let ((asked opencode-test-recap-asked))
            (opencode-test-reconnect)
            (should-not opencode--holding)
            (should (string-search "Answer 2." (opencode-test-text buffer)))
            (should (opencode-test-recap-at-prompt-p))
            (should (equal (opencode-test-recap) "※ Done: second\n\n"))
            (should (> opencode-test-recap-asked asked))))
        (let ((text (opencode-test-text buffer)))
          (let ((opencode-session-summary-function nil))
            (with-current-buffer buffer
              (rename-buffer "opencode-test-old" t)
              (remhash id opencode-session-buffers))
            (let ((fresh (opencode-test-open id)))
              (unwind-protect (should (equal text (opencode-test-text fresh)))
                (kill-buffer fresh)
                (puthash id buffer opencode-session-buffers)))))))))

(ert-deftest opencode-test-recap-catch-up-renders-whole ()
  "A catch-up rendering the transcript whole, as after a revert, ends with the line at the prompt."
  (opencode-test-with-recap '("Done: second" . nil)
    (opencode-test-with-transcript (append (opencode-test-recap-turn 1) (opencode-test-recap-turn 2))
      (let ((buffer (opencode-test-open (opencode-test-session))))
        (setq opencode-test-transcript (append (opencode-test-recap-turn 1) (opencode-test-recap-turn 3))
              opencode-test-recap-value '("Done: third" . nil))
        (opencode-test-reconnect)
        (with-current-buffer buffer
          (should-not (string-search "Answer 2." (opencode-test-text buffer)))
          (should (string-search "Answer 3." (opencode-test-text buffer)))
          (should (opencode-test-recap-at-prompt-p))
          (should (equal (opencode-test-recap) "※ Done: third\n\n"))
          ;; One line, whatever was deleted and written meanwhile.
          (should (= 1 (seq-count (lambda (overlay) (overlay-get overlay 'opencode-recap))
                                  (overlays-in (point-min) (point-max))))))))))

(ert-deftest opencode-test-recap-reload ()
  "Reverting a session buffer shows the line again at its prompt, the transcript unchanged."
  (opencode-test-with-recap '("Done: first" . nil)
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((buffer (opencode-test-open (opencode-test-session))))
        (with-current-buffer buffer
          (let ((text (opencode-test-text buffer)))
            (revert-buffer)
            (should-not opencode--holding)
            (should (equal (opencode-test-text buffer) text))
            (should (opencode-test-recap-at-prompt-p))
            (should (equal (opencode-test-recap) "※ Done: first\n\n"))))))))

(ert-deftest opencode-test-recap-withheld-until-applied ()
  "While a buffer withholds its events, the line does not show anew; it may go."
  (opencode-test-with-recap '("Done: first" . nil)
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (with-current-buffer (opencode-test-open (opencode-test-session))
        (let ((opencode--holding t))
          (opencode-lift-prompt)
          (should-not (opencode-session-summary-string))
          (opencode-show-prompt)
          (opencode-session-summary-update)
          (should-not (opencode-session-summary-string)))
        (opencode-session-summary-update)
        (should (opencode-test-recap-at-prompt-p))))))

(ert-deftest opencode-test-recap-overlay-deleted-made-again ()
  "An overlay a rewrite deleted is made again where the line belongs."
  (opencode-test-with-recap '("Done: first" . nil)
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (with-current-buffer (opencode-test-open (opencode-test-session))
        (delete-overlay opencode--recap-overlay)
        (opencode-test-recap-redisplay)
        (should (opencode-test-recap-at-prompt-p))
        (should (equal (opencode-test-recap) "※ Done: first\n\n"))
        (should (= opencode-test-recap-asked 1))))))

(provide 'opencode-recap-test)
;;; opencode-recap-test.el ends here
