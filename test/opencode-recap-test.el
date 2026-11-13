;;; opencode-recap-test.el --- Where a session stands, above its input  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Once a turn ended, a session buffer shows the summary
;; `opencode-session-summary-function' tells, as text just above its
;; input, and nothing while a turn runs.  These tests check where it
;; shows, when it is asked for and when it goes, that it reads, moves
;; and quotes as the transcript does, and that it changes nothing else:
;; the transcript as the service has it, the pending area, groups,
;; catching up and reloading, the input, its undo and line motion.

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
          (opencode-session-summary-icon "※")
          (opencode-session-summary-function
           (lambda ()
             (cl-incf opencode-test-recap-asked)
             opencode-test-recap-value)))
     ,@body))

(defun opencode-test-recap ()
  "Return what the current buffer shows of where its session stands, or \"\"."
  (or (opencode-session-summary-string) ""))

(defun opencode-test-recap-text ()
  "Return the current buffer's text without the summary, trailing whitespace dropped.
As the transcript reads without it."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (string-trim-right
     (if-let* ((start (opencode-idle-start))
               ((< start opencode--prompt-start)))
         (concat (substring text 0 (1- start)) (substring text (1- opencode--prompt-start)))
       text))))

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
  "Return non-nil when the summary shows as text just before the idle prompt."
  (and (opencode-prompt-shown-p)
       (< (opencode-idle-start) opencode--prompt-start)
       (string-prefix-p "※ " (opencode-test-recap))))

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
  "An idle session shows its summary as read-only text just above its input."
  (opencode-test-with-recap "Adding search: the index is done, tests pass. Next, ranking."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (with-current-buffer (opencode-test-open (opencode-test-session))
        (should (equal (opencode-test-recap)
                       "※ Adding search: the index is done, tests pass. Next, ranking.\n\n"))
        (should (opencode-test-recap-at-prompt-p))
        ;; Text after the transcript, a blank line before it, the input empty.
        (should (string-search "Answer 1.\n\n※ Adding search" (buffer-string)))
        (should (equal (opencode-test-input (current-buffer)) ""))
        (let ((start (opencode-idle-start)))
          (should (get-text-property start 'read-only))
          (should (eq (get-text-property start 'font-lock-face) 'opencode-session-summary-icon))
          (should (eq (get-text-property (+ start 2) 'font-lock-face) 'opencode-session-summary))
          ;; It is no part of what was synced.
          (should (<= opencode-synced-end start)))
        ;; Redisplay asks nothing.
        (let ((asked opencode-test-recap-asked))
          (run-hook-with-args 'pre-redisplay-functions (selected-window))
          (should (= opencode-test-recap-asked asked)))))))

(ert-deftest opencode-test-recap-none-without-function ()
  "Without a function, or one telling nothing, nothing shows; one failing tells nothing."
  (opencode-test-with-transcript (opencode-test-recap-turn 1)
    (with-current-buffer (opencode-test-open (opencode-test-session))
      (let ((text (buffer-string)))
        (should-not (opencode-session-summary-string))
        (opencode-test-with-recap nil
          (opencode-session-summary-update)
          (should-not (opencode-session-summary-string))
          (should (= opencode-test-recap-asked 1))
          (setq opencode-test-recap-value "  \n")
          (opencode-session-summary-update)
          (should-not (opencode-session-summary-string)))
        (let ((opencode-session-summary-function (lambda () (error "No summary"))))
          (let ((inhibit-message t))
            (opencode-session-summary-update))
          (should-not (opencode-session-summary-string)))
        (should (equal (buffer-string) text))))))

(ert-deftest opencode-test-recap-lines ()
  "A line and a recap show as lines; a summary keeps a few lines, blank ones dropped."
  (opencode-test-with-recap '("Waiting for approval" . "To push the fix.")
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (with-current-buffer (opencode-test-open (opencode-test-session))
        (should (equal (opencode-test-recap) "※ Waiting for approval\n  To push the fix.\n\n"))
        (setq opencode-test-recap-value
              (mapconcat (lambda (n) (format "line %d\n" n)) (number-sequence 1 12) "\n"))
        (opencode-session-summary-update)
        (should (equal (opencode-test-recap)
                       (concat "※ line 1\n"
                               (mapconcat (lambda (n) (format "  line %d\n" n)) (number-sequence 2 8) "")
                               "\n")))
        ;; Wrapped, a line starts after the icon's width.
        (should (equal (get-text-property (opencode-idle-start) 'wrap-prefix) "  "))
        (setq opencode-test-recap-value (make-string 5000 ?a))
        (opencode-session-summary-update)
        (should (= (length (opencode-test-recap)) (+ 2 2000 2)))))))

(ert-deftest opencode-test-recap-hidden-while-turn-runs ()
  "The summary goes as a turn starts, and is asked anew once it ends."
  (opencode-test-with-recap "First turn done."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((session (opencode-test-session)))
        (with-current-buffer (opencode-test-open session)
          (should (opencode-test-recap-at-prompt-p))
          ;; A turn another client started.
          (opencode-test-turn session "started")
          (should-not (opencode-session-summary-string))
          (should-not (string-search "First turn done." (buffer-string)))
          (setq opencode-test-recap-value "Second turn done.")
          (opencode-test-answer session "msg_step" "Working on it.")
          (should-not (string-search "turn done." (buffer-string)))
          (let ((asked opencode-test-recap-asked))
            (opencode-test-turn session "succeeded")
            (should (opencode-test-recap-at-prompt-p))
            (should (equal (opencode-test-recap) "※ Second turn done.\n\n"))
            (should (> opencode-test-recap-asked asked))
            ;; After the turn's text.
            (should (< (opencode-test-find "Working on it.") (opencode-test-find "Second turn done.")))
            (should (= 1 (how-many "turn done\\." (point-min) (point-max))))))))))

(ert-deftest opencode-test-recap-hidden-once-sent ()
  "Sending a prompt deletes the summary: it never stays in the transcript."
  (opencode-test-with-recap "First turn done."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (with-current-buffer (opencode-test-open (opencode-test-session))
        (should (opencode-session-summary-string))
        (goto-char (point-max))
        (insert "next")
        ;; Typing keeps it.
        (should (opencode-test-recap-at-prompt-p))
        (opencode-send-input)
        (should-not (opencode-session-summary-string))
        (should-not (string-search "First turn done." (buffer-string)))
        (should (string-search "Answer 1.\n\n> next" (buffer-substring-no-properties (point-min) (opencode-pmark))))))))

(ert-deftest opencode-test-recap-not-written-for-a-prompt-delivered ()
  "A prompt of another client delivered at the idle prompt writes no summary before it."
  (opencode-test-with-recap "First turn done."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((session (opencode-test-session)))
        (with-current-buffer (opencode-test-open session)
          (should (opencode-session-summary-string))
          (opencode-test-dispatch (opencode-test-inbox-events session "msg_tui" "from the TUI"))
          (should (string-search "from the TUI" (buffer-string)))
          (should-not (string-search "First turn done." (buffer-string))))))))

(ert-deftest opencode-test-recap-updated-in-place ()
  "Asked anew, the summary changes, the input, point and window point kept."
  (opencode-test-with-recap "First."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((buffer (opencode-test-open (opencode-test-session))))
        (with-current-buffer buffer
          (save-window-excursion
            (set-window-buffer (selected-window) buffer)
            (goto-char (point-max))
            (insert "draft")
            (backward-char 2)
            (set-window-point (selected-window) (point))
            (let ((from-end (- (point-max) (point))))
              (setq opencode-test-recap-value "A revised summary, longer than the first.")
              (opencode-session-summary-update buffer)
              (should (equal (opencode-test-recap) "※ A revised summary, longer than the first.\n\n"))
              (should (= (- (point-max) (point)) from-end))
              (should (= (- (point-max) (window-point)) from-end))
              (should (equal (opencode-test-input buffer) "draft"))
              ;; Told the same, nothing is written.
              (let ((tick (buffer-modified-tick)))
                (opencode-session-summary-update buffer)
                (should (= (buffer-modified-tick) tick)))
              ;; Told nothing any more, it goes.
              (setq opencode-test-recap-value nil)
              (opencode-session-summary-update buffer)
              (should-not (opencode-session-summary-string))
              (should (equal (opencode-test-input buffer) "draft")))))))))

(ert-deftest opencode-test-recap-keeps-input-undo ()
  "Writing the summary anew leaves the input's undo as it was."
  (opencode-test-with-recap "First."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((buffer (opencode-test-open (opencode-test-session))))
        (with-current-buffer buffer
          (buffer-enable-undo)
          (goto-char (point-max))
          (undo-boundary)
          (insert "draft")
          (undo-boundary)
          (setq opencode-test-recap-value "A revised summary, longer than the first.")
          (opencode-session-summary-update buffer)
          (let ((last-command nil))
            (undo))
          (should (equal (opencode-test-input buffer) ""))
          (should (equal (opencode-test-recap) "※ A revised summary, longer than the first.\n\n")))))))

;;;; Reading it

(ert-deftest opencode-test-recap-quoted ()
  "RET on the summary gives its line as it gives any line of the transcript."
  (opencode-test-with-recap "Adding search: the index is done."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (with-current-buffer (opencode-test-open (opencode-test-session))
        (let* ((given nil)
               (opencode-old-input-function (lambda (text _buffer) (setq given text))))
          (goto-char (opencode-test-find "the index"))
          (opencode-send-input)
          (should (equal given "※ Adding search: the index is done.")))))))

(ert-deftest opencode-test-recap-input-and-motion ()
  "C-p and C-n go through the summary as through the transcript, to the input and back."
  (opencode-test-with-recap "A summary."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((buffer (opencode-test-open (opencode-test-session))))
        (with-current-buffer buffer
          (let ((start (opencode-idle-start)))
            (cl-flet ((press (keys)
                        (should-not (opencode-test-press buffer keys))
                        (point)))
              (goto-char (point-max))
              ;; Up from the empty input: the blank line, then the summary.
              (press "C-p C-p")
              (should (= (pos-bol) start))
              ;; Down again, past the prompt, into the input.
              (should (= (press "C-n C-n") (opencode-pmark))))))))))

(ert-deftest opencode-test-recap-groups-as-without ()
  "A prompt sent while the summary shows groups the turn before it as without one."
  (cl-flet ((groups-after-send (function)
              (opencode-test-with-transcript (append (opencode-test-recap-turn 1) (opencode-test-recap-turn 2))
                (let ((opencode-session-summary-function function))
                  (with-current-buffer (opencode-test-open (opencode-test-session))
                    (should (eq (and (opencode-session-summary-string) t) (and function t)))
                    (let ((before (opencode-test-recap-groups)))
                      (goto-char (point-max))
                      (insert "Question 3")
                      (opencode-send-input)
                      (list (length before) (length (opencode-test-recap-groups))
                            (opencode-test-text (current-buffer)))))))))
    (let ((with (groups-after-send (lambda () "Second turn done.")))
          (without (groups-after-send nil)))
      (should (= (car with) 1))
      (should (= (cadr with) 2))
      (should (equal with without)))))

;;;; The pending area

(ert-deftest opencode-test-recap-before-pending-area ()
  "With pending prompts, the summary comes first, then the pending area, drawn whole."
  (opencode-test-with-recap "First turn done."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((session (opencode-test-session)))
        (with-current-buffer (opencode-test-open session)
          (opencode-test-dispatch
           `(((type . "session.inbox.enqueued")
              (data (sessionID . ,session) (inboxID . "msg_tui")
                    (item (type . "user") (delivery . "queue") (payload (text . "later")))))))
          (should (string-search "later" (opencode-inbox-area-string)))
          (should (= (overlay-start opencode--inbox-overlay) opencode--prompt-start))
          (should (< (opencode-idle-start) (overlay-start opencode--inbox-overlay)))
          ;; The summary ends with a blank line: the area needs none more.
          (should-not (string-prefix-p "\n" (opencode-inbox-area-string)))
          (let ((drawn (opencode-test-area-drawn opencode--inbox-overlay)))
            (should (cl-plusp (cdr drawn)))
            (should (= (car drawn) (cdr drawn))))
          ;; Written anew, the summary leaves the area as it was.
          (let ((area (opencode-inbox-area-string)))
            (setq opencode-test-recap-value "Revised.")
            (opencode-session-summary-update)
            (run-hook-with-args 'pre-redisplay-functions (selected-window))
            (should (equal (opencode-inbox-area-string) area))
            (should (= (overlay-start opencode--inbox-overlay) opencode--prompt-start))))))))

;; A graphical frame alone draws margins.
(defun opencode-test-recap-margin-faces (from to)
  "Return the faces of the left margins drawn on the rows from FROM to TO, by row.
FROM and TO are buffer positions shown in the selected window, which
was just redisplayed: its rows are read from what it drew."
  (let ((top (cdr (posn-x-y (posn-at-point from))))
        (bottom (cdr (posn-x-y (posn-at-point to))))
        (faces nil))
    (dotimes (row (window-body-height))
      (when-let* ((line (window-line-height row))
                  (y (nth 2 line))
                  ((<= top y bottom)))
        (let* ((posn (posn-at-x-y 0 (+ y (/ (car line) 2)) (selected-window) t))
               (string (and (eq (posn-area posn) 'left-margin) (car (posn-string posn)))))
          (push (and string (get-text-property (or (cdr (posn-string posn)) 0) 'face string)) faces))))
    (nreverse faces)))

(ert-deftest opencode-test-recap-margin-on-input ()
  "The prompt's margin is drawn on the input's row, not on the summary's nor the pending area's."
  :tags '(gui)
  (skip-unless (display-graphic-p))
  (opencode-test-with-recap "First turn done."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let* ((session (opencode-test-session))
             (buffer (opencode-test-open session)))
        (with-current-buffer buffer
          (save-window-excursion
            (set-window-buffer (selected-window) buffer)
            (cl-flet ((faces ()
                        (goto-char (point-max))
                        (recenter -1)
                        (redisplay t)
                        (opencode-test-recap-margin-faces (opencode-idle-start) (point-max))))
              (let ((faces (faces)))
                (should (eq (car (last faces)) 'opencode-request-margin-highlight))
                (should-not (memq 'opencode-request-margin-highlight (butlast faces))))
              (opencode-test-dispatch
               `(((type . "session.inbox.enqueued")
                  (data (sessionID . ,session) (inboxID . "msg_tui")
                        (item (type . "user") (delivery . "queue") (payload (text . "later")))))))
              (let ((faces (faces)))
                (should (> (length faces) 4))
                (should (eq (car (last faces)) 'opencode-request-margin-highlight))
                (should-not (memq 'opencode-request-margin-highlight (butlast faces)))))))))))

;;;; While the session waits for its user

(defun opencode-test-recap-waiting-p ()
  "Return non-nil when the summary ends the transcript, the session waiting, without a prompt."
  (and (not (opencode-prompt-shown-p))
       (opencode-end-text-start)
       (string-prefix-p "※ " (opencode-test-recap))))

(defun opencode-test-recap-return ()
  "Run the timer showing the summary again, as once output stopped; return non-nil if armed."
  (when (timerp opencode--recap-return)
    (let ((arguments (timer--args opencode--recap-return)))
      (cancel-timer opencode--recap-return)
      (apply #'opencode-recap--return arguments)
      t)))

(defun opencode-test-recap-forget-return ()
  "Cancel the timer showing the summary again, if armed."
  (when (timerp opencode--recap-return)
    (cancel-timer opencode--recap-return)
    (setq opencode--recap-return nil)))

(defvar opencode-test-recap-waited 0
  "How many times a waiting session was seen showing its summary.")

(defmacro opencode-test-recap-waiting (scenario event &rest body)
  "Run BODY in SCENARIO's session streamed up to its first EVENT, a request asked.
The requests' state is synced, as its timer does, so that the summary
shows; `rest' is bound to the events left."
  (declare (indent 2) (debug t))
  `(let* ((id (opencode-test-scenario ,scenario))
          (buffer (opencode-test-open-empty id))
          (rest (opencode-test-dispatch-until (opencode-test-events id) ,event)))
     (ignore rest)
     (with-current-buffer buffer
       (unwind-protect
           (progn
             (should (eq opencode-session-status 'busy))
             (opencode--flush-sync)
             ,@body)
         (opencode-test-recap-forget-return)))))

(ert-deftest opencode-test-recap-shows-while-permission-waits ()
  "A session waiting on a permission shows its summary ending the transcript, until it is answered."
  (opencode-test-with-server
    (opencode-test-with-recap "Cleaning up: removing junk.txt needs approval."
      (opencode-test-recap-waiting "permission" "permission.asked"
        (should (opencode-test-recap-waiting-p))
        (should (equal (opencode-test-recap) "※ Cleaning up: removing junk.txt needs approval.\n\n"))
        ;; After the call asking, a blank line before it, read-only, the input empty after it.
        (should (string-match-p "Run rm junk.txt.*\n\n※ Cleaning up"
                                (buffer-substring-no-properties (point-min) (opencode-pmark))))
        (should (get-text-property (opencode-test-find "Cleaning up") 'read-only))
        (should (eq (get-text-property (opencode-test-find "Cleaning up") 'font-lock-face)
                    'opencode-session-summary))
        (should (equal (opencode-test-input buffer) ""))
        ;; Told the same, nothing is written.
        (let ((tick (buffer-modified-tick)))
          (opencode-session-summary-update)
          (should (= (buffer-modified-tick) tick)))
        ;; Told anew, it changes in place.
        (setq opencode-test-recap-value "Revised.")
        (opencode-session-summary-update)
        (should (equal (opencode-test-recap) "※ Revised.\n\n"))
        (should (= 1 (how-many "※" (point-min) (point-max))))
        ;; An event applied gives way to it; it shows again once events stop.
        (opencode-test-dispatch (list (opencode-test-event id "session.usage.updated" "evt_usage"
                                                           '(cost . 0.5))))
        (should-not (opencode-session-summary-string))
        (should-not (string-search "Revised." (buffer-string)))
        (should (opencode-test-recap-return))
        (should (opencode-test-recap-waiting-p))
        ;; Answered, it goes.
        (opencode-test-dispatch-until rest "permission.replied")
        (opencode-test-recap-return)
        (opencode--flush-sync)
        (should-not (opencode-session-summary-string))
        (should-not (string-search "Revised." (buffer-string)))))))

(ert-deftest opencode-test-recap-none-while-only-busy ()
  "A turn running, no request waiting, shows no summary, whatever the function tells."
  (opencode-test-with-server
    (opencode-test-with-recap "Busy."
      (opencode-test-recap-waiting "permission" "session.tool.called"
        (should-not (opencode-pending-request-p))
        (let ((asked opencode-test-recap-asked))
          (opencode-session-summary-update)
          (should (= opencode-test-recap-asked asked)))
        (should-not (opencode-session-summary-string))
        (should-not (string-search "Busy." (buffer-string)))))))

(ert-deftest opencode-test-recap-gives-way-to-output ()
  "Output written while a session waits goes after the transcript; the summary shows again once it stopped."
  (opencode-test-with-server
    (opencode-test-with-recap "Waiting for approval."
      (opencode-test-recap-waiting "permission" "permission.asked"
        (should (opencode-test-recap-waiting-p))
        (let ((transcript (buffer-substring-no-properties (point-min) (opencode-end-text-start))))
          (opencode-output-notice "Still running")
          (should-not (opencode-session-summary-string))
          (should-not (string-search "Waiting for approval." (buffer-string)))
          (should (string-prefix-p transcript (buffer-substring-no-properties (point-min) (opencode-pmark))))
          (should (timerp opencode--recap-return))
          ;; Output since it gave way puts it off.
          (opencode-output-notice "More output")
          (should (opencode-test-recap-return))
          (should-not (opencode-session-summary-string))
          (should (opencode-test-recap-return))
          (should (opencode-test-recap-waiting-p))
          (should (< (opencode-test-find "More output") (opencode-test-find "Waiting for approval."))))))))

(ert-deftest opencode-test-recap-quoted-while-waiting ()
  "RET on the summary of a waiting session gives its line, as on the transcript."
  (opencode-test-with-server
    (opencode-test-with-recap "Waiting for approval to remove junk.txt."
      (opencode-test-recap-waiting "permission" "permission.asked"
        (let* ((given nil)
               (opencode-old-input-function (lambda (text _buffer) (setq given text))))
          (goto-char (opencode-test-find "to remove"))
          (opencode-send-input)
          (should (equal given "※ Waiting for approval to remove junk.txt."))
          (should (opencode-test-recap-waiting-p)))))))

(ert-deftest opencode-test-recap-gives-way-to-input-sent ()
  "A prompt queued while a session waits never leaves the summary in the transcript."
  (opencode-test-with-server
    (opencode-test-with-recap "Waiting for an answer."
      (opencode-test-recap-waiting "question" "form.created"
        (should (opencode-test-recap-waiting-p))
        (goto-char (point-max))
        (insert "use red instead")
        (cl-letf (((symbol-function 'read-multiple-choice)
                   (lambda (_prompt choices &rest _) (assq ?q choices))))
          (opencode-send-input))
        (should (equal (opencode-test-input buffer) ""))
        (should-not (string-search "Waiting for an answer." (buffer-string)))
        (should (opencode-test-recap-return))
        (should (opencode-test-recap-waiting-p))
        (should (= 1 (how-many "Waiting for an answer\\." (point-min) (point-max))))))))

(ert-deftest opencode-test-recap-question-then-idle ()
  "Once the turn of a session that waited ends, one summary shows, at its idle prompt."
  (opencode-test-with-server
    (opencode-test-with-recap "Asks which colour to use."
      (opencode-test-recap-waiting "question" "form.created"
        (should (opencode-test-recap-waiting-p))
        (setq opencode-test-recap-value "Done: painted it red.")
        (opencode-test-dispatch-until rest "session.execution.succeeded")
        (opencode-test-recap-return)
        (opencode--flush-sync)
        (should (opencode-test-recap-at-prompt-p))
        (should (equal (opencode-test-recap) "※ Done: painted it red.\n\n"))
        (should (= 1 (how-many "※" (point-min) (point-max))))
        (should-not (string-search "Asks which colour" (buffer-string)))))))

(ert-deftest opencode-test-recap-waiting-leaves-transcript ()
  "A session shown its summary whenever it waits ends with the transcript it has without one."
  (cl-flet ((stream (function)
              (opencode-test-with-server
                (let ((opencode-session-summary-function function))
                  (let* ((id (opencode-test-scenario "question"))
                         (buffer (opencode-test-open-empty id)))
                    (with-current-buffer buffer
                      (dolist (event (opencode-test-events id))
                        (opencode-test-dispatch (list event))
                        (opencode-test-recap-return)
                        (opencode--flush-sync)
                        (when (opencode-end-text-start)
                          (cl-incf opencode-test-recap-waited)))
                      (opencode-test-recap-forget-return)
                      (let ((opencode-session-summary-function nil))
                        (opencode-session-summary-update))
                      (opencode-test-text buffer)))))))
    (let ((opencode-test-recap-waited 0))
      (should (equal (stream (lambda () "Waiting, or done."))
                     (stream nil)))
      ;; Once each of its three questions.
      (should (>= opencode-test-recap-waited 3)))))

(ert-deftest opencode-test-recap-waiting-withheld ()
  "Withholding events deletes the summary of a waiting session; it shows again once they are applied."
  (opencode-test-with-server
    (opencode-test-with-recap "Waiting for approval."
      (opencode-test-recap-waiting "permission" "permission.asked"
        (should (opencode-test-recap-waiting-p))
        ;; Settled, the transcript is in sync up to the summary, not past it.
        (let ((opencode--open-messages nil))
          (opencode--settle))
        (should (<= opencode-synced-end (opencode-end-text-start)))
        (opencode--hold)
        (should-not (opencode-session-summary-string))
        (should-not (string-search "Waiting for approval." (buffer-string)))
        (opencode-session-summary-update)
        (should-not (opencode-session-summary-string))
        (opencode--release)
        (opencode-session-summary-update)
        (should (opencode-test-recap-waiting-p))))))

;;;; Catching up and reloading

(ert-deftest opencode-test-recap-catch-up ()
  "Catching up shows the summary at the new idle prompt, asked anew, the transcript as opened afresh."
  (opencode-test-with-recap "First turn done."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let* ((id (opencode-test-session))
             (buffer (opencode-test-open id)))
        (with-current-buffer buffer
          (should (opencode-test-recap-at-prompt-p))
          (setq opencode-test-transcript (append opencode-test-transcript (opencode-test-recap-turn 2))
                opencode-test-recap-value "Second turn done.")
          (let ((asked opencode-test-recap-asked))
            (opencode-test-reconnect)
            (should-not opencode--holding)
            (should (string-search "Answer 2." (opencode-test-text buffer)))
            (should (opencode-test-recap-at-prompt-p))
            (should (equal (opencode-test-recap) "※ Second turn done.\n\n"))
            (should-not (string-search "First turn done." (buffer-string)))
            (should (> opencode-test-recap-asked asked))))
        (let ((text (with-current-buffer buffer (opencode-test-recap-text))))
          (let ((opencode-session-summary-function nil))
            (with-current-buffer buffer
              (rename-buffer "opencode-test-old" t)
              (remhash id opencode-session-buffers))
            (let ((fresh (opencode-test-open id)))
              (unwind-protect (should (equal text (opencode-test-text fresh)))
                (kill-buffer fresh)
                (puthash id buffer opencode-session-buffers)))))))))

(ert-deftest opencode-test-recap-catch-up-renders-whole ()
  "A catch-up rendering the transcript whole, as after a revert, ends with one summary."
  (opencode-test-with-recap "Second turn done."
    (opencode-test-with-transcript (append (opencode-test-recap-turn 1) (opencode-test-recap-turn 2))
      (let ((buffer (opencode-test-open (opencode-test-session))))
        (setq opencode-test-transcript (append (opencode-test-recap-turn 1) (opencode-test-recap-turn 3))
              opencode-test-recap-value "Third turn done.")
        (opencode-test-reconnect)
        (with-current-buffer buffer
          (should-not (string-search "Answer 2." (opencode-test-text buffer)))
          (should (string-search "Answer 3." (opencode-test-text buffer)))
          (should (opencode-test-recap-at-prompt-p))
          (should (equal (opencode-test-recap) "※ Third turn done.\n\n"))
          (should (= 1 (how-many "turn done\\." (point-min) (point-max)))))))))

(ert-deftest opencode-test-recap-reload ()
  "Reverting a session buffer shows the summary again at its prompt, the transcript unchanged."
  (opencode-test-with-recap "First turn done."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (let ((buffer (opencode-test-open (opencode-test-session))))
        (with-current-buffer buffer
          (let ((text (opencode-test-text buffer)))
            (revert-buffer)
            (should-not opencode--holding)
            (should (equal (opencode-test-text buffer) text))
            (should (opencode-test-recap-at-prompt-p))
            (should (= 1 (how-many "turn done\\." (point-min) (point-max))))))))))

(ert-deftest opencode-test-recap-withheld-until-applied ()
  "While a buffer withholds its events, the summary is not written; it is once they are applied."
  (opencode-test-with-recap "First turn done."
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

(ert-deftest opencode-test-recap-upgrades-earlier-buffers ()
  "A buffer of an earlier version loses its overlay and hooks, its prompt displayed as its margin."
  (opencode-test-with-recap "First turn done."
    (opencode-test-with-transcript (opencode-test-recap-turn 1)
      (with-current-buffer (opencode-test-open (opencode-test-session))
        (let ((prompt (seq-find (lambda (overlay) (overlay-get overlay 'opencode-prompt-string))
                                (overlays-at opencode--prompt-start)))
              (margin (opencode-add-margin opencode--prompt-start (opencode-pmark)
                                           'opencode-request-margin-highlight))
              (old (make-overlay (opencode-pmark) (opencode-pmark))))
          (overlay-put prompt 'display nil)
          (overlay-put prompt 'opencode-prompt-string nil)
          (overlay-put prompt 'invisible t)
          (overlay-put old 'opencode-recap t)
          (add-hook 'after-change-functions 'opencode-recap--anchor nil t)
          (add-hook 'pre-redisplay-functions 'opencode-recap--anchor-window nil t)
          (opencode-prompt-upgrade)
          (opencode-recap-setup)
          (should (equal (overlay-get prompt 'display)
                         (opencode-margin-spec 'opencode-request-margin-highlight)))
          (should (overlay-get prompt 'opencode-prompt-string))
          (should-not (overlay-get prompt 'invisible))
          (should-not (overlay-buffer margin))
          (should-not (overlay-buffer old))
          (should-not (memq 'opencode-recap--anchor after-change-functions))
          (should-not (memq 'opencode-recap--anchor-window pre-redisplay-functions))
          (should (opencode-test-recap-at-prompt-p)))))))

(provide 'opencode-recap-test)
;;; opencode-recap-test.el ends here
