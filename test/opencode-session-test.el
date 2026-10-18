;;; opencode-session-test.el --- Tests of session rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(defconst opencode-test-scenarios
  '("text" "read-tools" "edit-tools" "shell" "permission" "question" "subagent"
    "interrupt" "switch" "steer" "history" "shell-input" "reasoning")
  "Recorded scenarios whose sessions can be opened.")

(ert-deftest opencode-test-replay-every-scenario ()
  "Every recorded session replays without error and ends at an input prompt."
  (opencode-test-with-server
    (dolist (scenario opencode-test-scenarios)
      (let* ((id (opencode-test-scenario scenario))
             (buffer (opencode-test-open id)))
        (with-current-buffer buffer
          (should (eq opencode-session-status 'idle))
          (should (opencode-prompt-shown-p))
          (should (equal opencode-session-id id)))))))

(ert-deftest opencode-test-replay-text ()
  "A plain answer is rendered after its prompt."
  (opencode-test-with-server
    (let ((text (opencode-test-text (opencode-test-open (opencode-test-scenario "text")))))
      (should (string-match-p "Reply with exactly the words: hello fixture" text))
      (should (string-match-p "hello fixture" text)))))

(ert-deftest opencode-test-replay-tools ()
  "Tool calls become collapsed summaries with their v2 names."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "shell"))
      (let ((tools (mapcar (lambda (overlay) (plist-get (opencode-block-data overlay) :tool))
                           (opencode-tool-blocks))))
        (should (member "shell" tools))
        (should (seq-every-p (lambda (overlay)
                               (member (plist-get (opencode-block-data overlay) :status)
                                       '("completed" "error")))
                             (opencode-tool-blocks))))
      (should (string-match-p "git status --short" (opencode-test-text (current-buffer)))))))

(ert-deftest opencode-test-live-matches-replay ()
  "Streaming a scenario's events renders what replaying its messages renders."
  (dolist (scenario '("text" "shell" "edit-tools" "reasoning"))
    (opencode-test-with-server
      (let* ((id (opencode-test-scenario scenario))
             (replayed (opencode-test-text (opencode-test-open id))))
        (kill-buffer (gethash id opencode-session-buffers))
        (let ((live (opencode-test-open-empty id)))
          (opencode-test-dispatch (opencode-test-events id))
          (should (equal (opencode-test-text live) replayed)))))))

(ert-deftest opencode-test-own-prompt-not-echoed ()
  "A prompt sent from Emacs is not rendered again when the server echoes it."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty id)))
      (with-current-buffer buffer
        (let ((inbox (opencode-session-send "hello")))
          (should (equal (plist-get (car opencode-test-requests) :path)
                         (format "/api/session/%s/prompt" id)))
          (should (equal (alist-get 'id (plist-get (car opencode-test-requests) :body)) inbox))
          (let ((before (opencode-test-text buffer)))
            (opencode-test-dispatch
             `(((type . "session.inbox.enqueued")
                (data (sessionID . ,id) (inboxID . ,inbox)
                      (item (type . "user") (delivery . "steer") (payload (text . "hello")))))))
            (should (equal (opencode-test-text buffer) before))))))))

(ert-deftest opencode-test-foreign-prompt-rendered ()
  "A prompt sent by another client appears in the transcript."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-dispatch
       `(((type . "session.inbox.enqueued")
          (data (sessionID . ,id) (inboxID . "msg_foreign")
                (item (type . "user") (delivery . "queue") (payload (text . "from the TUI")))))))
      ;; Shown where it enters the conversation, as replay places it.
      (should-not (string-match-p "from the TUI" (opencode-test-text buffer)))
      (opencode-test-dispatch
       `(((type . "session.inbox.delivered") (data (sessionID . ,id) (inboxID . "msg_foreign")))))
      (should (string-match-p "from the TUI" (opencode-test-text buffer))))))

(ert-deftest opencode-test-generated-ids-ascend ()
  "Generated message IDs have the server's layout and sort by creation."
  (let ((first (opencode-generate-id "msg_"))
        (second (opencode-generate-id "msg_")))
    (should (string-match-p "\\`msg_[0-9a-f]\\{12\\}[0-9A-Za-z]\\{14\\}\\'" first))
    (should (string< first second))))

(ert-deftest opencode-test-icons-are-glyphs ()
  "Every tool and status icon is one Nerd Font glyph."
  (dolist (tool '("read" "write" "edit" "patch" "shell" "grep" "glob" "websearch" "webfetch"
                  "question" "skill" "execute" "subagent" "slack_send_message"))
    (should (= (length (opencode-tool-icon tool)) 1)))
  (should (= (length (opencode-tool-icon "subagent" '((agent . "explore")))) 1))
  (should (= (length (opencode-pending-prompt-icon)) 1))
  (dolist (entry opencode-provider-icons)
    (should (= (length (cdr entry)) 1))))

(provide 'opencode-session-test)
;;; opencode-session-test.el ends here
