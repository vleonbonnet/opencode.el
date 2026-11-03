;;; opencode-resync-test.el --- Tests of catching up after a reconnection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Nothing is replayed after the stream reconnects, so a session buffer
;; catches up from the service's messages.  These tests serve a
;; transcript that changes while the stream is down, through the route
;; paging it as the service does, and check that a buffer writes only
;; what it missed, ends as a buffer opened afresh would, and applies the
;; events arriving meanwhile once, after what it fetched.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-tool-part (id status &optional output)
  "Return the content of shell call ID in STATUS, with OUTPUT."
  `((type . "tool") (id . ,id) (name . "shell")
    (state (status . ,status) (input (command . "make check"))
           ,@(when output `((content ((type . "text") (text . ,output))))))))

(defun opencode-test-turn-messages (n)
  "Return the messages of turn N: a prompt, a step with a tool call and text, its end."
  (list (opencode-test-prompt (format "msg_t%02du" n) (format "Question %d" n))
        (opencode-test-step (format "msg_t%02da" n)
                            (opencode-test-tool-part (format "call_t%02d" n) "completed" "ok")
                            `((type . "text") (text . ,(format "Answer %d." n))))
        (opencode-test-idle (format "msg_t%02di" n))))

(defun opencode-test-fresh-text (id)
  "Return the text a buffer opened afresh on session ID shows, and kill it."
  (let ((buffer (gethash id opencode-session-buffers)))
    (with-current-buffer buffer
      (rename-buffer "opencode-test-old" t)
      (remhash id opencode-session-buffers))
    (let* ((fresh (opencode-test-open id))
           (text (opencode-test-text fresh)))
      (kill-buffer fresh)
      (puthash id buffer opencode-session-buffers)
      text)))

(defun opencode-test-message-requests ()
  "Return the queries of the transcript requests made, oldest first."
  (seq-keep (lambda (request)
              (when (string-suffix-p "/message" (plist-get request :path))
                (plist-get request :query)))
            (reverse opencode-test-requests)))

(ert-deftest opencode-test-resync-up-to-date-writes-nothing ()
  "A buffer that missed nothing writes nothing: one page of the newest messages tells."
  (opencode-test-with-transcript (append (opencode-test-turn-messages 1) (opencode-test-turn-messages 2))
    (let ((buffer (opencode-test-open (opencode-test-session))))
      (with-current-buffer buffer
        (let ((text (opencode-test-text buffer))
              (tick (buffer-chars-modified-tick)))
          (setq opencode-test-requests nil)
          (opencode-test-reconnect)
          (should (= tick (buffer-chars-modified-tick)))
          (should (equal (opencode-test-text buffer) text))
          (should (equal (opencode-test-message-requests) '(((limit . 5) (order . "desc")))))
          (should (eq opencode-session-status 'idle))
          (should (opencode-prompt-shown-p))
          (should-not opencode--holding))))))

(ert-deftest opencode-test-resync-writes-missed-turns ()
  "Turns run while the stream was down are written after the transcript, as opened afresh."
  (opencode-test-with-transcript (opencode-test-turn-messages 1)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id)))
      (with-current-buffer buffer
        (let ((before (buffer-substring-no-properties (point-min) opencode-synced-end)))
          (setq opencode-test-transcript
                (append opencode-test-transcript (opencode-test-turn-messages 2) (opencode-test-turn-messages 3))
                opencode-test-requests nil)
          (opencode-test-reconnect)
          ;; What was in sync is left as it was.
          (should (string-prefix-p before (opencode-test-text buffer)))
          (should (string-search "Answer 3." (opencode-test-text buffer)))
          (should-not (seq-some (lambda (query) (equal (alist-get 'order query) "asc"))
                                (opencode-test-message-requests)))
          (should (equal (plist-get opencode--synced :id) "msg_t03i"))
          (should (opencode-prompt-shown-p))))
      (should (equal (opencode-test-text buffer) (opencode-test-fresh-text id))))))

(ert-deftest opencode-test-resync-pages-back-to-sync ()
  "Missing many turns, a buffer fetches growing pages back to where it is in sync."
  (opencode-test-with-transcript (opencode-test-turn-messages 1)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id)))
      (setq opencode-test-transcript
            (append opencode-test-transcript
                    (apply #'append (mapcar #'opencode-test-turn-messages (number-sequence 2 20))))
            opencode-test-requests nil)
      (opencode-test-reconnect)
      (should (equal (mapcar (lambda (query) (alist-get 'limit query)) (opencode-test-message-requests))
                     '(5 25 100)))
      (should (equal (opencode-test-text buffer) (opencode-test-fresh-text id))))))

(ert-deftest opencode-test-resync-turn-streamed-while-away ()
  "A turn streaming when the stream dropped shows whole and ended once caught up."
  (opencode-test-with-transcript (opencode-test-turn-messages 1)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id))
           (step `((assistantMessageID . "msg_t02a"))))
      (opencode-test-dispatch
       (list (opencode-test-event id "session.inbox.enqueued" "evt_t02q" '(inboxID . "msg_t02u")
                                  '(item (type . "user") (payload (text . "Question 2"))))
             (opencode-test-event id "session.execution.started" "evt_t02s")
             (opencode-test-event id "session.inbox.delivered" "evt_t02d" '(inboxID . "msg_t02u"))
             (apply #'opencode-test-event id "session.step.started" "evt_t02b" step)
             (apply #'opencode-test-event id "session.text.started" "evt_t02c" '(ordinal . 0) step)
             (apply #'opencode-test-event id "session.text.delta" "evt_t02e" '(ordinal . 0) '(delta . "Answ") step)))
      (with-current-buffer buffer
        (should (eq opencode-session-status 'busy))
        (should (string-search "Answ" (opencode-test-text buffer))))
      (setq opencode-test-transcript
            (append opencode-test-transcript (opencode-test-turn-messages 2)))
      (opencode-test-reconnect)
      (with-current-buffer buffer
        (should (eq opencode-session-status 'idle))
        (should (opencode-prompt-shown-p))
        (should (string-search "Answer 2." (opencode-test-text buffer)))
        (should-not opencode--open-messages))
      (should (equal (opencode-test-text buffer) (opencode-test-fresh-text id))))))

(ert-deftest opencode-test-resync-step-still-running ()
  "Caught up during a step, a buffer takes the step's next events as they come.
Its finished parts are not written twice, and a part in progress, whose
deltas were missed, shows whole once it ends."
  (opencode-test-with-transcript (opencode-test-turn-messages 1)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id))
           (step '((assistantMessageID . "msg_t02a")))
           (final (append (opencode-test-turn-messages 1)
                          (list (opencode-test-prompt "msg_t02u" "Question 2")
                                (opencode-test-step "msg_t02a"
                                                    '((type . "text") (text . "First part."))
                                                    (opencode-test-tool-part "call_t02" "completed" "ok")
                                                    '((type . "text") (text . "Second part.")))
                                (opencode-test-idle "msg_t02i")))))
      (setq opencode-test-transcript
            (append (opencode-test-turn-messages 1)
                    (list (opencode-test-prompt "msg_t02u" "Question 2")
                          (opencode-test-running-step "msg_t02a"
                                                      '((type . "text") (text . "First part."))
                                                      (opencode-test-tool-part "call_t02" "running")
                                                      '((type . "text") (text . ""))))))
      (let ((opencode-test-routes
             (cons (list 'get "\\`/api/session/active\\'" (lambda (_) `((data (,(intern id) . t)))))
                   opencode-test-routes)))
        (opencode-test-reconnect))
      (with-current-buffer buffer
        (should (eq opencode-session-status 'busy))
        (should-not (opencode-prompt-shown-p)))
      (opencode-test-dispatch
       (list (apply #'opencode-test-event id "session.text.ended" "evt_late" '(ordinal . 0) '(text . "First part.") step)
             (apply #'opencode-test-event id "session.text.delta" "evt_d1" '(ordinal . 1) '(delta . "part.") step)
             (apply #'opencode-test-event id "session.tool.success" "evt_ok" '(id . "call_t02")
                    '(content ((type . "text") (text . "ok"))) step)
             (apply #'opencode-test-event id "session.text.ended" "evt_e1" '(ordinal . 1) '(text . "Second part.") step)
             (apply #'opencode-test-event id "session.step.ended" "evt_se" step)
             (opencode-test-event id "session.execution.succeeded" "evt_t02i")))
      (setq opencode-test-transcript final)
      (with-current-buffer buffer
        (should (eq opencode-session-status 'idle))
        (should (= 1 (with-temp-buffer
                       (insert (opencode-test-text buffer))
                       (how-many "First part" (point-min) (point-max))))))
      (should (equal (opencode-test-text buffer) (opencode-test-fresh-text id))))))

(ert-deftest opencode-test-resync-applies-events-once ()
  "Events arriving while a buffer catches up apply after what it fetched, once.
Those the fetched transcript already holds change nothing; the others
follow it."
  (opencode-test-with-transcript (opencode-test-turn-messages 1)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id))
           (step '((assistantMessageID . "msg_t02a"))))
      (setq opencode-test-transcript
            (append opencode-test-transcript (opencode-test-turn-messages 2)))
      (let ((opencode-test-routes
             (cons (list 'get "\\`/api/session/ses_[A-Za-z0-9]+/message\\'"
                         (lambda (request)
                           (cons :defer (lambda () (opencode-test-message-page opencode-test-transcript request)))))
                   opencode-test-routes)))
        (opencode-test-reconnect)
        (with-current-buffer buffer
          (should opencode--holding))
        ;; In the transcript fetched already, then after it.
        (opencode-test-dispatch
         (list (apply #'opencode-test-event id "session.text.started" "evt_x1" '(ordinal . 0) step)
               (apply #'opencode-test-event id "session.text.delta" "evt_x2" '(ordinal . 0) '(delta . "Answer 2.") step)
               (apply #'opencode-test-event id "session.text.ended" "evt_x3" '(ordinal . 0) '(text . "Answer 2.") step)
               (apply #'opencode-test-event id "session.step.ended" "evt_x4" step)
               (opencode-test-event id "session.execution.succeeded" "evt_t02i")
               (opencode-test-event id "session.agent.selected" "evt_t03g" '(agent . "plan"))))
        (with-current-buffer buffer
          (should-not (string-search "Answer 2." (opencode-test-text buffer))))
        (while (opencode-test-release)))
      (with-current-buffer buffer
        (should-not opencode--holding)
        (should (eq opencode-session-status 'idle))
        (should (equal opencode-session-agent "plan")))
      (setq opencode-test-transcript
            (append opencode-test-transcript
                    `(((id . "msg_t03g") (type . "agent-switched") (agent . "plan") (time (created . 4))))))
      (let ((text (opencode-test-text buffer)))
        (should (= 1 (with-temp-buffer (insert text) (how-many "Answer 2\\." (point-min) (point-max)))))
        (should (equal text (opencode-test-fresh-text id)))))))

(ert-deftest opencode-test-resync-after-revert-renders-whole ()
  "A buffer whose last message in sync is gone, as after a revert, renders its transcript whole."
  (opencode-test-with-transcript (append (opencode-test-turn-messages 1) (opencode-test-turn-messages 2))
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "draft"))
      (setq opencode-test-transcript (append (opencode-test-turn-messages 1) (opencode-test-turn-messages 3))
            opencode-test-requests nil)
      (opencode-test-reconnect)
      ;; The first page lacks it, and the service says it is gone: no
      ;; paging back through the transcript.
      (should (equal (seq-take (opencode-test-message-requests) 2)
                     '(((limit . 5) (order . "desc")) ((limit . 200) (order . "asc")))))
      (should (seq-some (lambda (request) (string-suffix-p "/message/msg_t02i" (plist-get request :path)))
                        opencode-test-requests))
      (should-not (string-search "Answer 2." (opencode-test-text buffer)))
      (should (string-search "Answer 3." (opencode-test-text buffer)))
      (should (equal (opencode-test-resync-input buffer) "draft"))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (delete-region (opencode-pmark) (point-max))))
      (should (equal (opencode-test-text buffer) (opencode-test-fresh-text id))))))

(defun opencode-test-resync-input (buffer)
  "Return the input typed in BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties (opencode-pmark) (point-max))))

(ert-deftest opencode-test-resync-keeps-input-and-its-undo ()
  "Catching up keeps the input being typed, and undoing still undoes only it."
  (opencode-test-with-transcript (opencode-test-turn-messages 1)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id)))
      (with-current-buffer buffer
        (buffer-enable-undo)
        (goto-char (point-max))
        (undo-boundary)
        (insert "draft")
        (undo-boundary))
      (setq opencode-test-transcript (append opencode-test-transcript (opencode-test-turn-messages 2)))
      (opencode-test-reconnect)
      (with-current-buffer buffer
        (should (equal (opencode-test-resync-input buffer) "draft"))
        (let ((transcript (buffer-substring-no-properties (point-min) (opencode-pmark))))
          (should (string-search "Answer 2." transcript))
          (let ((last-command nil))
            (undo))
          (should (equal (opencode-test-resync-input buffer) ""))
          (should (equal (buffer-substring-no-properties (point-min) (opencode-pmark)) transcript)))))))

(ert-deftest opencode-test-resync-in-slices ()
  "A long catch-up is written a slice at a time, its events withheld until it is done."
  (opencode-test-with-transcript (opencode-test-turn-messages 1)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id))
           (opencode-replay-slice 1e-9))
      (setq opencode-test-transcript
            (append opencode-test-transcript
                    (apply #'append (mapcar #'opencode-test-turn-messages (number-sequence 2 6)))))
      (opencode-test-with-held-timers
        (opencode-test-reconnect)
        (with-current-buffer buffer
          (should opencode--holding)
          (should-not (string-search "Answer 6." (opencode-test-text buffer))))
        (opencode-test-dispatch (list (opencode-test-event id "session.agent.selected" "evt_t07g" '(agent . "plan"))))
        (with-current-buffer buffer
          (should-not (equal opencode-session-agent "plan")))
        ;; One message a slice: a timer for each but the first.
        (should (>= (opencode-test-run-timers) 14))
        (with-current-buffer buffer
          (should-not opencode--holding)
          (should (equal opencode-session-agent "plan"))
          (should (string-search "Answer 6." (opencode-test-text buffer))))))))

(ert-deftest opencode-test-resync-shown-buffers-first ()
  "After a reconnection, buffers shown in a window catch up first, a few at a time."
  (opencode-test-with-transcript (opencode-test-turn-messages 1)
    (let* ((ids (mapcar #'opencode-test-scenario '("text" "shell" "switch" "steer")))
           (buffers (mapcar #'opencode-test-open ids))
           (opencode-resync-requests 2)
           (order nil)
           (record (lambda (&rest _) (push (current-buffer) order))))
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (selected-window) (nth 3 buffers))
        (let ((opencode-test-routes
               (cons (list 'get "\\`/api/session/ses_[A-Za-z0-9]+/message\\'"
                           (lambda (request)
                             (cons :defer (lambda () (opencode-test-message-page opencode-test-transcript request)))))
                     opencode-test-routes)))
          (advice-add 'opencode--resync-buffer :before record)
          (unwind-protect
              (progn
                (opencode-test-reconnect)
                (should (= (length order) 2))
                (should (eq (car (last order)) (nth 3 buffers)))
                (while (opencode-test-release))
                (should (= (length order) 4))
                (should (equal (sort (copy-sequence order) :key #'buffer-name)
                               (sort (copy-sequence buffers) :key #'buffer-name))))
            (advice-remove 'opencode--resync-buffer record))
          (dolist (buffer buffers)
            (should-not (buffer-local-value 'opencode--holding buffer))))))))

(ert-deftest opencode-test-resync-abandoned-by-next-reconnection ()
  "A reconnection during a catch-up begins it again; the first one's answers change nothing."
  (opencode-test-with-transcript (opencode-test-turn-messages 1)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id)))
      (setq opencode-test-transcript (append opencode-test-transcript (opencode-test-turn-messages 2)))
      (let ((opencode-test-routes
             (cons (list 'get "\\`/api/session/ses_[A-Za-z0-9]+/message\\'"
                         (lambda (request)
                           (cons :defer (lambda () (opencode-test-message-page opencode-test-transcript request)))))
                   opencode-test-routes)))
        (opencode-test-reconnect)
        (opencode-test-reconnect)
        (while (opencode-test-release)))
      (with-current-buffer buffer
        (should-not opencode--holding))
      (should (= 1 (with-temp-buffer (insert (opencode-test-text buffer))
                                     (how-many "Answer 2\\." (point-min) (point-max)))))
      (should (equal (opencode-test-text buffer) (opencode-test-fresh-text id))))))

(ert-deftest opencode-test-resync-keeps-expansion-at-sync-point ()
  "A block expanded where the buffer is in sync keeps its expansion when catching up.
Expanding the block writes at that point, which must stay after it."
  (opencode-test-with-transcript (opencode-test-turn-messages 1)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id))
           (step '((assistantMessageID . "msg_t02a")))
           (call `(,@step (id . "call_t02"))))
      (opencode-test-dispatch
       (list (opencode-test-event id "session.inbox.enqueued" "evt_x0" '(inboxID . "msg_t02u")
                                  '(item (type . "user") (payload (text . "Question 2"))))
             (opencode-test-event id "session.execution.started" "evt_x1")
             (opencode-test-event id "session.inbox.delivered" "evt_x2" '(inboxID . "msg_t02u"))
             (apply #'opencode-test-event id "session.step.started" "evt_x3" step)
             (apply #'opencode-test-event id "session.tool.input.started" "evt_x4" '(name . "shell") call)
             (apply #'opencode-test-event id "session.tool.called" "evt_x5" '(input (command . "make check")) call)
             (apply #'opencode-test-event id "session.tool.success" "evt_x6"
                    '(content ((type . "text") (text . "expanded output"))) call)
             (apply #'opencode-test-event id "session.step.ended" "evt_x7" step)))
      (with-current-buffer buffer
        (should (= opencode-synced-end (opencode-pmark)))
        (opencode-expand-block (opencode-block "call_t02"))
        (should (= opencode-synced-end (opencode-pmark))))
      (setq opencode-test-transcript
            (append opencode-test-transcript
                    (list (opencode-test-prompt "msg_t02u" "Question 2")
                          (opencode-test-step "msg_t02a" (opencode-test-tool-part "call_t02" "completed" "expanded output"))
                          (opencode-test-step "msg_t02b" '((type . "text") (text . "Answer 2.")))
                          (opencode-test-idle "msg_t02i"))))
      (opencode-test-reconnect)
      (with-current-buffer buffer
        (should (overlay-get (opencode-block "call_t02") 'opencode-expanded))
        (let ((text (opencode-test-text buffer)))
          (should (= 1 (with-temp-buffer (insert text) (how-many "expanded output" (point-min) (point-max)))))
          (should (string-search "Answer 2." text))
          (opencode-collapse-block (opencode-block "call_t02"))))
      (should (equal (opencode-test-text buffer) (opencode-test-fresh-text id))))))

(ert-deftest opencode-test-load-withholds-racing-events ()
  "Events arriving while a transcript loads apply after it, once."
  (opencode-test-with-transcript (opencode-test-turn-messages 1)
    (let* ((id (opencode-test-session))
           (opencode-test-routes
            (cons (list 'get "\\`/api/session/ses_[A-Za-z0-9]+/message\\'"
                        (lambda (request)
                          (cons :defer (lambda () (opencode-test-message-page opencode-test-transcript request)))))
                  opencode-test-routes))
           (buffer (opencode-test-open id)))
      (setq opencode-test-transcript (append opencode-test-transcript (opencode-test-turn-messages 2)))
      ;; The prompt is in the transcript fetched; the switch is not.
      (opencode-test-dispatch
       (list (opencode-test-event id "session.inbox.enqueued" "evt_l1" '(inboxID . "msg_t02u")
                                  '(item (type . "user") (payload (text . "Question 2"))))
             (opencode-test-event id "session.inbox.delivered" "evt_l2" '(inboxID . "msg_t02u"))
             (opencode-test-event id "session.agent.selected" "evt_t03g" '(agent . "plan"))))
      (while (opencode-test-release))
      (should (= 1 (with-temp-buffer (insert (opencode-test-text buffer))
                                     (how-many "Question 2" (point-min) (point-max)))))
      (should (string-search "Switched to the plan agent" (opencode-test-text buffer))))))

(provide 'opencode-resync-test)
;;; opencode-resync-test.el ends here
