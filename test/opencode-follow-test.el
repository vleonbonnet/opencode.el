;;; opencode-follow-test.el --- Tests of windows following output  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A point at the end of the transcript or in the input follows output,
;; as in any Comint buffer: it keeps its place in the input, after
;; whatever was written.  That holds for the buffer's point and for every
;; window showing the buffer, selected or not.  A point elsewhere in the
;; transcript keeps its text, and its window its start.  Batch Emacs has
;; windows but never redisplays them, so these tests check window points
;; and starts rather than what the windows would show.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-windows (&rest buffers)
  "Show BUFFERS in windows of the selected frame and return the windows.
The first one is the selected window, each other one is split below
the previous one."
  (delete-other-windows)
  (let ((windows (list (selected-window))))
    (dolist (_ (cdr buffers))
      (push (split-window (car windows) nil 'below) windows))
    (setq windows (nreverse windows))
    (cl-mapc #'set-window-buffer windows buffers)
    windows))

(defun opencode-test-place (buffer position windows)
  "Put BUFFER's point and the points of WINDOWS at POSITION.
POSITION is a buffer position, or `end' for the end of BUFFER."
  (with-current-buffer buffer
    (let ((position (if (eq position 'end) (point-max) position)))
      (goto-char position)
      (dolist (window windows)
        (set-window-point window position)))))

(defun opencode-test-offsets (buffer windows)
  "Return how far BUFFER's point, then each point of WINDOWS, is into its input.
An offset of 0 is the start of the input; a negative one is in the transcript."
  (with-current-buffer buffer
    (let ((pmark (opencode-pmark)))
      (cons (- (point) pmark)
            (mapcar (lambda (window) (- (window-point window) pmark)) windows)))))

(defun opencode-test-follow-input (buffer)
  "Return the input typed in BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties (opencode-pmark) (point-max))))

(defun opencode-test-dispatch-tool (session message id &optional output)
  "Dispatch shell call ID of assistant MESSAGE in SESSION, finished with OUTPUT if given."
  (let ((base `((sessionID . ,session) (assistantMessageID . ,message) (id . ,id))))
    (opencode-test-dispatch
     `(((type . "session.tool.input.started") (data ,@base (name . "shell")))
       ((type . "session.tool.called") (data ,@base (input (command . "make check"))))
       ,@(when output
           `(((type . "session.tool.success")
              (data ,@base (content ((type . "text") (text . ,output)))))))))))

(defun opencode-test-dispatch-narration (session message)
  "Dispatch a narration of assistant MESSAGE in SESSION, first shown as thinking."
  (let ((base `((sessionID . ,session) (assistantMessageID . ,message) (ordinal . 0)))
        (native `((signature . ,(opencode-test-signature "narration")))))
    (opencode-test-dispatch
     `(((type . "session.reasoning.started") (data ,@base (native . ,native)))
       ((type . "session.reasoning.delta") (data ,@base (delta . "Narrated for the user.")))
       ((type . "session.reasoning.ended") (data ,@base (text . "Narrated for the user.") (native . ,native)))))))

(defun opencode-test-dispatch-notices (session)
  "Dispatch events of SESSION that write notices, through every path notices take."
  (opencode-test-dispatch
   `(((type . "session.revert.staged") (data (sessionID . ,session) (revert (messageID . "msg_cut"))))
     ((type . "session.agent.selected") (data (sessionID . ,session) (agent . "plan")))
     ((type . "session.synthetic") (id . "msg_follow_synthetic")
      (data (sessionID . ,session) (description . "Reminder") (text . "Keep going.")))
     ((type . "session.revert.cleared") (data (sessionID . ,session))))))

(defun opencode-test-dispatch-inbox (session id text)
  "Dispatch another client's prompt ID of SESSION with TEXT, then a synthetic item."
  (opencode-test-dispatch
   `(,@(opencode-test-inbox-events session id text)
     ((type . "session.inbox.enqueued")
      (data (sessionID . ,session) (inboxID . ,(concat id "_note"))
            (item (type . "synthetic") (payload (description . "Background note") (text . "Noted.")))))
     ((type . "session.inbox.delivered") (data (sessionID . ,session) (inboxID . ,(concat id "_note")))))))

(defmacro opencode-test-with-busy-session (bindings &rest body)
  "Run BODY during a turn of a new session shown in two windows.
BINDINGS is a list (SESSION BUFFER WINDOWS) of the variables bound to the
session's ID, its buffer and the windows, the selected one first."
  (declare (indent 1) (debug t))
  (pcase-let ((`(,session ,buffer ,windows) bindings))
    `(opencode-test-with-server
       (let* ((,session (opencode-test-scenario "text"))
              (,buffer (opencode-test-open-empty ,session)))
         (save-window-excursion
           (let ((,windows (opencode-test-windows ,buffer ,buffer)))
             (opencode-test-turn ,session "started")
             ,@body))))))

(ert-deftest opencode-test-follow-streamed-text ()
  "Points at the end follow streamed text."
  (opencode-test-with-busy-session (session buffer windows)
    (opencode-test-place buffer 'end windows)
    (opencode-test-answer session "msg_follow_text" "Streamed text.")
    (should (equal (opencode-test-offsets buffer windows) '(0 0 0)))))

(ert-deftest opencode-test-follow-expansion ()
  "Points at the end follow a finished call's output expanding there."
  (opencode-test-with-busy-session (session buffer windows)
    (let ((opencode-show-tool-output t))
      (opencode-test-place buffer 'end windows)
      (opencode-test-dispatch-tool session "msg_follow_tool" "call_follow_done" "All checks passed.")
      (should (overlay-get (with-current-buffer buffer (opencode-block "call_follow_done")) 'opencode-expanded))
      (should (equal (opencode-test-offsets buffer windows) '(0 0 0))))))

(ert-deftest opencode-test-follow-rewritten-expansion ()
  "Points at the end follow an expansion ending the output rewritten as its call progresses."
  (opencode-test-with-busy-session (session buffer windows)
    (opencode-test-dispatch-tool session "msg_follow_tool" "call_follow_running")
    (with-current-buffer buffer (opencode-expand-block (opencode-block "call_follow_running")))
    (opencode-test-place buffer 'end windows)
    (opencode-test-dispatch
     `(((type . "session.tool.progress")
        (data (sessionID . ,session) (assistantMessageID . "msg_follow_tool") (id . "call_follow_running")
              (metadata (output . "half way"))))))
    (should (equal (opencode-test-offsets buffer windows) '(0 0 0)))))

(ert-deftest opencode-test-follow-narration ()
  "Points at the end follow narration replacing the thinking block it was first shown as."
  (opencode-test-with-busy-session (session buffer windows)
    (let* ((base `((sessionID . ,session) (assistantMessageID . "msg_follow_narration") (ordinal . 0)))
           (native `((signature . ,(opencode-test-signature "narration")))))
      (opencode-test-dispatch
       `(((type . "session.reasoning.started") (data ,@base (native . ,native)))
         ((type . "session.reasoning.delta") (data ,@base (delta . "Narrated for the user.")))))
      (opencode-test-place buffer 'end windows)
      (opencode-test-dispatch
       `(((type . "session.reasoning.ended") (data ,@base (text . "Narrated for the user.") (native . ,native)))))
      (should-not (with-current-buffer buffer
                    (overlay-get (opencode-block (opencode-reasoning-key "msg_follow_narration" 0))
                                 'opencode-collapsible)))
      (should (equal (opencode-test-offsets buffer windows) '(0 0 0))))))

(ert-deftest opencode-test-follow-table ()
  "Points at the end follow a table ending the answer aligned in place, all of it output."
  (opencode-test-with-busy-session (session buffer windows)
    (opencode-test-place buffer 'end windows)
    (opencode-test-answer session "msg_follow_table" "| a | longer |\n|---|---|\n| 1 | 2")
    (should (string-suffix-p "|1|2     |" (opencode-test-text buffer)))
    (should (equal (opencode-test-follow-input buffer) ""))
    (should (equal (opencode-test-offsets buffer windows) '(0 0 0)))))

(ert-deftest opencode-test-follow-turn-end ()
  "Points at the end of a running turn stay at the input once it ends, and follow the next turn."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty session)))
      (save-window-excursion
        (let ((windows (opencode-test-windows buffer buffer)))
          (opencode-test-turn session "started")
          (opencode-test-answer session "msg_follow_first" "First answer.")
          (opencode-test-place buffer 'end windows)
          (opencode-test-turn session "succeeded")
          (should (with-current-buffer buffer (opencode-prompt-shown-p)))
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0)))
          ;; A turn another client starts writes after the prompt it lifts.
          (opencode-test-turn session "started")
          (opencode-test-answer session "msg_follow_second" "Second answer.")
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0)))
          (opencode-test-turn session "interrupted")
          (should (string-search "Interrupted" (opencode-test-text buffer)))
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0))))))))

(ert-deftest opencode-test-follow-notices ()
  "Points at the end follow notices, written before the idle prompt or during a turn."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty session)))
      (save-window-excursion
        (let ((windows (opencode-test-windows buffer buffer)))
          (opencode-test-place buffer 'end windows)
          (opencode-test-dispatch-notices session)
          (should (string-search "The staged revert was cancelled" (opencode-test-text buffer)))
          (should (with-current-buffer buffer (opencode-prompt-shown-p)))
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0)))
          (opencode-test-turn session "started")
          (opencode-test-dispatch-notices session)
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0))))))))

(ert-deftest opencode-test-follow-question ()
  "Points at the end follow a question expanding its block there, then collapsing it."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "question"))
           (buffer (opencode-test-open-empty id))
           (events (opencode-test-events id))
           (asked (seq-position events "form.created"
                                (lambda (event type) (equal (alist-get 'type event) type)))))
      (save-window-excursion
        (let ((windows (opencode-test-windows buffer buffer)))
          (opencode-test-dispatch (seq-take events asked))
          (opencode-test-place buffer 'end windows)
          (opencode-test-dispatch (list (nth asked events)))
          (with-current-buffer buffer
            (let ((overlay (opencode--form-block (car opencode-pending-forms))))
              (should (overlay-get overlay 'opencode-expanded))
              ;; The question was the last output, so it expanded where points follow.
              (should (= (overlay-end (overlay-get overlay 'opencode-content-overlay)) (opencode-pmark)))))
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0)))
          ;; Answered, it collapses; the turn goes on and ends.
          (opencode-test-dispatch (nthcdr (1+ asked) events))
          (should (with-current-buffer buffer (opencode-prompt-shown-p)))
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0))))))))

(ert-deftest opencode-test-follow-permission-fallback ()
  "Points at the end follow the line standing for a permission request whose call is not shown."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty session)))
      (save-window-excursion
        (let ((windows (opencode-test-windows buffer buffer)))
          (opencode-test-turn session "started")
          (opencode-test-answer session "msg_follow_asking" "About to clean up.")
          (opencode-test-place buffer 'end windows)
          (opencode-test-dispatch
           `(((type . "permission.asked")
              (data (id . "per_follow") (sessionID . ,session) (action . "shell") (resources "rm junk.txt")
                    (source (type . "tool") (messageID . "msg_follow_asking") (id . "call_unshown"))))))
          (should (string-search "? Shell command: rm junk.txt" (opencode-test-text buffer)))
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0)))
          (opencode-test-dispatch
           `(((type . "permission.replied")
              (data (sessionID . ,session) (requestID . "per_follow") (reply . "once")))))
          (should (string-search "? Shell command: rm junk.txt [accepted]" (opencode-test-text buffer)))
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0))))))))

(ert-deftest opencode-test-follow-inbox ()
  "Points at the end follow prompts of other clients and other inbox items, idle or busy."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty session)))
      (save-window-excursion
        (let ((windows (opencode-test-windows buffer buffer)))
          (opencode-test-place buffer 'end windows)
          (opencode-test-dispatch-inbox session "msg_follow_idle" "From the TUI.")
          (should (string-search "From the TUI." (opencode-test-text buffer)))
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0)))
          (opencode-test-turn session "started")
          (opencode-test-answer session "msg_follow_busy" "Working.")
          (opencode-test-dispatch-inbox session "msg_follow_steer" "Steered from the TUI.")
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0))))))))

(ert-deftest opencode-test-follow-in-input ()
  "Points in a typed input keep their place in it, at its start, middle or end."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty session)))
      (save-window-excursion
        (let ((windows (opencode-test-windows buffer buffer buffer)))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "draft")
            (let ((pmark (opencode-pmark)))
              (cl-mapc #'set-window-point windows (list pmark (+ pmark 2) (point-max)))))
          (should (equal (opencode-test-offsets buffer windows) '(0 0 2 5)))
          (dolist (step (list (lambda () (opencode-test-dispatch-notices session))
                              (lambda () (opencode-test-dispatch-inbox session "msg_follow_typed" "Meanwhile."))
                              (lambda () (opencode-test-turn session "started"))
                              (lambda () (opencode-test-answer session "msg_follow_answer" "An answer."))
                              (lambda () (opencode-test-dispatch-inbox session "msg_follow_typed_busy" "Steer."))
                              (lambda () (opencode-test-turn session "succeeded"))))
            (funcall step)
            (should (equal (opencode-test-follow-input buffer) "draft"))
            (should (equal (opencode-test-offsets buffer windows) '(0 0 2 5)))))))))

(ert-deftest opencode-test-rewritten-block-points-stay ()
  "Points in a block rewritten at the end stay at its start, while those after it follow."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty session)))
      (save-window-excursion
        (pcase-let ((`(,selected ,inside ,end) (opencode-test-windows buffer buffer buffer)))
          (cl-flet ((place (position)
                      (with-current-buffer buffer
                        (goto-char position)
                        (set-window-point inside position)
                        (set-window-point end (point-max))))
                    (check (position)
                      (with-current-buffer buffer
                        (should (equal (list (point) (window-point selected) (window-point inside))
                                       (list position position position)))
                        (should (= (window-point end) (point-max))))))
            (opencode-test-turn session "started")
            ;; An expanded running call is rewritten as it progresses.
            (opencode-test-dispatch-tool session "msg_follow_rewrite" "call_follow_rewrite")
            (let* ((block (with-current-buffer buffer (opencode-block "call_follow_rewrite")))
                   (start (with-current-buffer buffer
                            (opencode-expand-block block)
                            (overlay-start (overlay-get block 'opencode-content-overlay)))))
              (place (+ start 3))
              (opencode-test-dispatch
               `(((type . "session.tool.progress")
                  (data (sessionID . ,session) (assistantMessageID . "msg_follow_rewrite")
                        (id . "call_follow_rewrite") (metadata (output . "half way"))))))
              (check start))
            ;; Narration replaces the thinking block it was first shown as.
            (let* ((base `((sessionID . ,session) (assistantMessageID . "msg_follow_narrated") (ordinal . 0)))
                   (native `((signature . ,(opencode-test-signature "narration")))))
              (opencode-test-dispatch
               `(((type . "session.reasoning.started") (data ,@base (native . ,native)))
                 ((type . "session.reasoning.delta") (data ,@base (delta . "Narrated.")))))
              (let ((start (with-current-buffer buffer
                             (overlay-start (opencode-block (opencode-reasoning-key "msg_follow_narrated" 0))))))
                (place (+ start 2))
                (opencode-test-dispatch
                 `(((type . "session.reasoning.ended") (data ,@base (text . "Narrated.") (native . ,native)))))
                (should (string-search "Narrated." (opencode-test-text buffer)))
                (check start)))))))))

(ert-deftest opencode-test-follow-table-reflow ()
  "Points at the end stay there when a resize reflows a table ending the output."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty session))
           ;; As a reflow does when the table ending the output grows.
           (opencode-rerender-tables-function
            (lambda ()
              (let ((inhibit-read-only t))
                (save-excursion
                  (goto-char (opencode-pmark))
                  (insert (propertize "  |" 'field 'output)))))))
      (save-window-excursion
        (let ((windows (opencode-test-windows buffer buffer)))
          (opencode-test-turn session "started")
          (opencode-test-answer session "msg_follow_reflow" "| a | b |\n|-|-|\n| 1 | 2 |")
          (opencode-test-place buffer 'end windows)
          (with-current-buffer buffer (opencode--rerender-tables (car windows)))
          (should (string-suffix-p "  |" (opencode-test-text buffer)))
          (should (equal (opencode-test-follow-input buffer) ""))
          (should (equal (opencode-test-offsets buffer windows) '(0 0 0))))))))

(defun opencode-test-follow-places (buffer windows)
  "Return BUFFER's point and the point and start of each of WINDOWS."
  (with-current-buffer buffer
    (cons (point) (mapcar (lambda (window) (list (window-point window) (window-start window))) windows))))

(ert-deftest opencode-test-transcript-points-stay ()
  "Points and windows elsewhere in the transcript keep their place whatever is written at the end."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (buffer (opencode-test-open session))
           (opencode-show-tool-output t))
      (save-window-excursion
        (let ((windows (opencode-test-windows buffer buffer)))
          (with-current-buffer buffer
            (let ((prompt (opencode-test-find "hello fixture")))
              (goto-char prompt)
              (set-window-start (car windows) (point-min))
              (set-window-point (cadr windows) (+ prompt 3))
              (set-window-start (cadr windows) (1+ (point-min)))))
          (let ((places (opencode-test-follow-places buffer windows)))
            (dolist (step (list (lambda () (opencode-test-dispatch-notices session))
                                (lambda () (opencode-test-dispatch-inbox session "msg_follow_other" "Another prompt."))
                                (lambda () (opencode-test-turn session "started"))
                                (lambda () (opencode-test-answer session "msg_follow_more" "| a | b |\n|-|-|\n| 1 | 2"))
                                (lambda () (opencode-test-dispatch-tool session "msg_follow_more" "call_follow_more" "done"))
                                (lambda () (opencode-test-dispatch-narration session "msg_follow_more"))
                                (lambda () (opencode-test-turn session "succeeded"))))
              (funcall step)
              (should (equal (opencode-test-follow-places buffer windows) places)))))))))

(ert-deftest opencode-test-follow-unselected-windows ()
  "Windows of a session not shown in the selected window follow too, and so does its buffer's point."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (buffer (opencode-test-open session))
           (other (get-buffer-create "*follow elsewhere*")))
      (unwind-protect
          (save-window-excursion
            (pcase-let* ((`(,_selected . ,windows) (opencode-test-windows other buffer buffer))
                         (`(,end ,middle) windows)
                         (text (with-current-buffer buffer (opencode-test-find "hello fixture"))))
              (opencode-test-place buffer 'end (list end))
              (with-current-buffer buffer (set-window-point middle text))
              (opencode-test-turn session "started")
              (opencode-test-answer session "msg_follow_hidden" "Answered unseen.")
              (opencode-test-turn session "succeeded")
              (opencode-test-dispatch-notices session)
              (opencode-test-dispatch-inbox session "msg_follow_hidden_prompt" "Asked unseen.")
              (should (equal (opencode-test-offsets buffer (list end)) '(0 0)))
              (should (= (window-point middle) text))))
        (kill-buffer other)))))

(ert-deftest opencode-test-follow-reload ()
  "Reloading a transcript that grew keeps points at the end there, and others at their line."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "shell"))
           (messages (alist-get 'data (opencode-test-fixture (format "messages-%s.json" session))))
           (shown (seq-take messages 2))
           (opencode-test-routes
            (cons (list 'get "/message\\'" (lambda (_) `((data . ,shown) (cursor)))) opencode-test-routes))
           (buffer (opencode-test-open session)))
      (should (> (length messages) 2))
      (save-window-excursion
        (let ((windows (opencode-test-windows buffer buffer buffer))
              (before (opencode-test-text buffer)))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "draft")
            (set-window-point (cadr windows) (1+ (opencode-pmark)))
            (set-window-point (nth 2 windows) (+ (point-min) 2))
            (set-window-start (nth 2 windows) (point-min)))
          (setq shown messages)
          (with-current-buffer buffer (revert-buffer))
          (should (> (length (opencode-test-text buffer)) (+ (length before) (length "draft"))))
          (should (equal (opencode-test-follow-input buffer) "draft"))
          (should (equal (butlast (opencode-test-offsets buffer windows)) '(5 5 1)))
          (should (= (window-point (nth 2 windows)) (+ (point-min) 2)))
          (should (= (window-start (nth 2 windows)) (point-min))))))))

(ert-deftest opencode-test-follow-sent-prompt ()
  "A prompt sent past the input brings the buffer's point and its windows to the end of the input."
  (opencode-test-with-server
   (let* ((session (opencode-test-scenario "text"))
          (buffer (opencode-test-open session))
          (other (get-buffer-create "*follow sender*")))
     (unwind-protect
         (save-window-excursion
           (pcase-let ((`(,_selected . ,windows) (opencode-test-windows other buffer buffer)))
             (with-current-buffer buffer
               (goto-char (point-max))
               (insert "draft")
               (let ((text (opencode-test-find "hello fixture")))
                 (goto-char text)
                 (set-window-point (car windows) (+ (opencode-pmark) 2))
                 (set-window-point (cadr windows) text)
                 (set-window-start (cadr windows) (point-min)))
               (let ((id (opencode-session-send "from drover" :show t)))
                 (should (equal (opencode-test-offsets buffer windows) '(5 5 5)))
                 (should (equal (opencode-test-follow-input buffer) "draft"))
                 ;; At the end, they follow the prompt delivered and the turn it starts.
                 (opencode-test-dispatch (opencode-test-inbox-events session id "from drover"))
                 (opencode-test-turn session "started")
                 (opencode-test-answer session "msg_follow_sent" "Answered.")
                 (should (string-search "Answered." (opencode-test-text buffer)))
                 (should (equal (opencode-test-follow-input buffer) "draft"))
                 (should (equal (opencode-test-offsets buffer windows) '(5 5 5)))))))
       (kill-buffer other)))))

(ert-deftest opencode-test-follow-sent-prompt-busy ()
  "A prompt sent past the input while a turn runs brings the points to the end, after it pending."
  (opencode-test-with-busy-session (session buffer windows)
    (opencode-test-answer session "msg_follow_working" "Working.")
    (opencode-test-place buffer (with-current-buffer buffer (opencode-test-find "Working.")) windows)
    (with-current-buffer buffer
      (opencode-session-send "steer from drover" :show t)
      (should (string-search "steer from drover" (opencode-inbox-area-string))))
    (should (equal (opencode-test-offsets buffer windows) '(0 0 0)))))

(ert-deftest opencode-test-follow-sent-prompt-pinned ()
  "A prompt sent while the transcript is written whole leaves its points to the load's end."
  (opencode-test-with-server
   (let* ((session (opencode-test-scenario "text"))
          (buffer (opencode-test-open session)))
     (save-window-excursion
       (let ((windows (opencode-test-windows buffer buffer))
             (start (with-current-buffer buffer (point-min))))
         (opencode-test-place buffer start windows)
         (with-current-buffer buffer
           (setq opencode--pinned t)
           (opencode-session-send "while loading" :show t)
           (should (equal (cons (point) (mapcar #'window-point windows))
                          (list start start start)))
           (opencode--unpin))
         (should (equal (opencode-test-offsets buffer windows) '(0 0 0))))))))

(provide 'opencode-follow-test)
;;; opencode-follow-test.el ends here
