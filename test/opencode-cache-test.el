;;; opencode-cache-test.el --- Tests of the prompt-cache guard  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(defvar opencode-test-reports nil
  "Preflight reports the fake service answers, in order.")

(defvar opencode-test-questions nil
  "Questions asked, newest first.")

(defmacro opencode-test-with-guard (answer &rest body)
  "Run BODY in an idle recorded session whose questions get ANSWER.
The fake service answers preflights with `opencode-test-reports'."
  (declare (indent 1) (debug t))
  `(opencode-test-with-server
     (let ((opencode-test-questions nil)
           (opencode-test-routes
            (cons (list 'post "/preflight\\'"
                        (lambda (_)
                          (let ((report (pop opencode-test-reports)))
                            (if (eq (car-safe report) :error) report `((data . ,report))))))
                  opencode-test-routes)))
       (cl-letf (((symbol-function 'y-or-n-p)
                  (lambda (question) (push question opencode-test-questions) ,answer)))
         (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
           ,@body)))))

(defun opencode-test-type-and-send (text)
  "Type TEXT at the prompt and press RET."
  (goto-char (point-max))
  (insert text)
  (opencode-send-input))

(defun opencode-test-sent-body (suffix)
  "Return the body of the latest request whose path ends in SUFFIX."
  (plist-get (seq-find (lambda (request) (string-suffix-p suffix (plist-get request :path)))
                       opencode-test-requests)
             :body))

(ert-deftest opencode-test-preflight-body ()
  "Prompts and commands are preflighted; shell commands start no turn."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
      (should (equal (opencode-cache-preflight-body (opencode-input-request "fix it"))
                     '((text . "fix it"))))
      (should (equal (opencode-cache-preflight-body (opencode-input-request "/review HEAD~1"))
                     '((command (name . "review") (arguments . "HEAD~1")))))
      (should-not (opencode-cache-preflight-body (opencode-input-request "!ls"))))))

(ert-deftest opencode-test-cache-hit-sends-silently ()
  "A hit, or a small loss, sends without asking."
  (let ((opencode-test-reports '(((status . "hit") (reusableTokens . 90000) (lostTokens . 0))
                                 ((status . "partial") (lostTokens . 5000)))))
    (opencode-test-with-guard t
      (opencode-test-type-and-send "one")
      (opencode-test-type-and-send "two")
      (should-not opencode-test-questions)
      (should (equal (alist-get 'text (opencode-test-sent-body "/prompt")) "two")))))

(ert-deftest opencode-test-cache-large-loss-asks ()
  "A large loss asks, shows why, and declining keeps the input in the prompt."
  (let ((opencode-test-reports
         '(((status . "miss") (lostTokens . 142000) (reasons "the tool list changed")
            (divergence (unit . "tools[5]") (index . 5))))))
    (opencode-test-with-guard nil
      (opencode-test-type-and-send "fix it")
      (should (string-match-p "~142k cached tokens would be re-sent uncached (miss)" (car opencode-test-questions)))
      (should (string-match-p "the tool list changed" (car opencode-test-questions)))
      (should (string-match-p "first difference: tools\\[5\\]" (car opencode-test-questions)))
      (should-not (opencode-test-sent-body "/prompt"))
      (should (equal (buffer-substring-no-properties (opencode-pmark) (point-max)) "fix it")))))

(ert-deftest opencode-test-cache-fails-safe ()
  "An unpredictable turn, or a failed check, asks when the context is large."
  (let ((opencode-test-reports '(((status . "unknown") (lostTokens . 0) (reasons "no history"))
                                 (:error 500 "Connection refused"))))
    (opencode-test-with-guard nil
      (setq opencode-session-tokens 150000)
      (opencode-test-type-and-send "one")
      (should (string-match-p "up to ~150k context tokens could be re-sent uncached" (car opencode-test-questions)))
      (delete-region (opencode-pmark) (point-max))
      (opencode-test-type-and-send "two")
      (should (string-match-p "cache check failed: Connection refused (HTTP 500)" (car opencode-test-questions)))
      ;; A small context is sent whatever the check says.
      (setq opencode-session-tokens 1000
            opencode-test-reports '(((status . "unknown") (lostTokens . 0))))
      (delete-region (opencode-pmark) (point-max))
      (opencode-test-type-and-send "three")
      (should (= (length opencode-test-questions) 2))
      (should (equal (alist-get 'text (opencode-test-sent-body "/prompt")) "three")))))

(ert-deftest opencode-test-cache-stale-thinking-consent ()
  "Stale thinking asks for consent, which travels on that prompt only."
  (let ((opencode-test-reports
         '(((status . "partial") (lostTokens . 142000)
            (staleThinking (count . 12) (reason . "the system prompt changed")))
           ((status . "hit")))))
    (opencode-test-with-guard t
      (opencode-test-type-and-send "go on")
      (should (string-match-p "12 thinking blocks from earlier turns no longer match the conversation"
                              (car opencode-test-questions)))
      (should (string-match-p "also re-sends ~142k cached tokens" (car opencode-test-questions)))
      (should (equal (alist-get 'metadata (opencode-test-sent-body "/prompt")) '((thinkingLoss . "accept"))))
      ;; Later prompts ask the server to refuse stale thinking again.
      (opencode-test-type-and-send "next")
      (should (equal (alist-get 'metadata (opencode-test-sent-body "/prompt")) '((thinkingLoss . "ask")))))))

(ert-deftest opencode-test-cache-consent-loss ()
  "The consent question counts the cache the consented turn re-sends."
  (let ((opencode-test-reports
         '(((status . "hit") (lostTokens . 0)
            (staleThinking (count . 3) (reason . "the provider rejected a block")
                           (consent (reusableTokens . 2000) (lostTokens . 180000)))))))
    (opencode-test-with-guard t
      (opencode-test-type-and-send "retry")
      (should (string-match-p "also re-sends ~180k cached tokens" (car opencode-test-questions))))))

(ert-deftest opencode-test-cache-unknown-binding-not-asked ()
  "Thinking the server holds no binding for is sent without a consent question."
  (let ((opencode-test-reports
         '(((status . "hit") (lostTokens . 0)
            (staleThinking (count . 4) (reason . "unknown binding"))))))
    (opencode-test-with-guard nil
      (opencode-test-type-and-send "go on")
      (should-not opencode-test-questions)
      (should (equal (alist-get 'metadata (opencode-test-sent-body "/prompt")) '((thinkingLoss . "ask")))))))

(ert-deftest opencode-test-preflight-target-agent-and-model ()
  "A check of a turn after a switch preflights that agent and model."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
      (let ((model '((id . "gpt-6-luna") (providerID . "openai"))))
        (should (equal (opencode-cache-preflight-body
                        (append (opencode-input-request "fix it") (list :agent "build" :model model)))
                       `((text . "fix it") (agent . "build") (model . ,model))))))))

(ert-deftest opencode-test-cache-stale-thinking-declined ()
  "Declining to drop thinking keeps the input; a command carries the consent too."
  (let ((opencode-test-reports
         '(((status . "hit") (staleThinking (count . 1) (reason . "earlier messages changed"))))))
    (opencode-test-with-guard nil
      (opencode-test-type-and-send "/review")
      (should (string-match-p "1 thinking block from earlier turns no longer matches" (car opencode-test-questions)))
      (should-not (opencode-test-sent-body "/command"))))
  (let ((opencode-test-reports
         '(((status . "hit") (staleThinking (count . 1) (reason . "earlier messages changed"))))))
    (opencode-test-with-guard t
      (opencode-test-type-and-send "/review")
      (should (equal (alist-get 'metadata (opencode-test-sent-body "/command")) '((thinkingLoss . "accept")))))))

(ert-deftest opencode-test-cache-unsupported-service ()
  "A service without the preflight route is never asked again, nor the user."
  (let ((opencode-test-reports '((:error 404 nil)))
        (opencode-cache--unsupported nil)
        (opencode-service-connection '(:id "svc_stock")))
    (opencode-test-with-guard nil
      (setq opencode-session-tokens 500000)
      (opencode-test-type-and-send "one")
      (opencode-test-type-and-send "two")
      (should-not opencode-test-questions)
      (should (equal opencode-cache--unsupported '("svc_stock")))
      (should (= (seq-count (lambda (request) (string-suffix-p "/preflight" (plist-get request :path)))
                            opencode-test-requests)
                 1)))))

(ert-deftest opencode-test-cache-busy-not-checked ()
  "Input steering a running turn, and shell commands, skip the check."
  (let ((opencode-test-reports nil))
    (opencode-test-with-guard nil
      (setq opencode-session-status 'busy)
      (opencode-test-type-and-send "also this")
      (setq opencode-session-status 'idle)
      (opencode-test-type-and-send "!ls")
      (should-not opencode-test-questions)
      (should-not (opencode-test-sent-body "/preflight"))
      (should (opencode-test-sent-body "/prompt")))))

(ert-deftest opencode-test-dropped-thinking-relabelled ()
  "Dropped blocks are relabelled and reported in one notice."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "reasoning"))
           (key (opencode-reasoning-key "msg_14fb6a141001g6EKRFFNVpwLGN" 0)))
      (with-current-buffer (opencode-test-open id)
        (should (opencode-block key))
        (opencode-test-dispatch
         `(((type . "session.thinking.dropped")
            (data (sessionID . ,id)
                  (blocks ((messageID . "msg_14fb6a141001g6EKRFFNVpwLGN") (ordinal . 0)
                           (reason . "dropped with consent: no longer matches the conversation")
                           (asText . t)))))))
        (let ((overlay (opencode-block key)))
          (should (string-match-p "as text" (buffer-substring (overlay-start overlay) (overlay-end overlay)))))
        (should (string-match-p "Dropped 1 thinking block from earlier turns that no longer match the conversation (1 with your consent, replayed as text)"
                                (opencode-test-text (current-buffer))))))))

(ert-deftest opencode-test-dropped-thinking-on-replay ()
  "Reopening a session relabels the blocks the server dropped before."
  (opencode-test-with-server
    (let* ((opencode-test-routes
            (cons (list 'get "/thinking\\'"
                        (lambda (_) '((data (dropped ((messageID . "msg_14fb6a141001g6EKRFFNVpwLGN")
                                                       (ordinal . 0) (asText)
                                                       (reason . "dropped by the provider: no longer matches the conversation")))))))
                  opencode-test-routes)))
      (with-current-buffer (opencode-test-open (opencode-test-scenario "reasoning"))
        (let ((overlay (opencode-block (opencode-reasoning-key "msg_14fb6a141001g6EKRFFNVpwLGN" 0))))
          (should (string-match-p "dropped" (buffer-substring (overlay-start overlay) (overlay-end overlay)))))
        (should-not (string-match-p "Dropped 1" (opencode-test-text (current-buffer))))))))

(ert-deftest opencode-test-dropped-notice ()
  "The notice counts blocks by who dropped them, and which are replayed as text."
  (let ((kept '((reason . "dropped with consent: no longer matches the conversation") (asText . t)))
        (consent '((reason . "dropped with consent: no longer matches the conversation")))
        (provider '((reason . "dropped by the provider: no longer matches the conversation"))))
    (should (equal (opencode-cache-dropped-notice (list provider kept kept))
                   "Dropped 3 thinking blocks from earlier turns that no longer match the conversation (2 with your consent, replayed as text; 1 by the provider)"))
    (should (equal (opencode-cache-dropped-notice (list consent provider '((reason . "other cause"))))
                   "Dropped 3 thinking blocks from earlier turns that no longer match the conversation (1 with your consent; 1 by the provider; 1: other cause)"))))

(ert-deftest opencode-test-refused-turn ()
  "Only a turn refused before anything ran, without attachments, is undone."
  (let ((user '((id . "msg_u") (type . "user") (text . "fix it")))
        (idle '((id . "msg_i") (type . "idle") (outcome . "failed"))))
    (should (equal (opencode-cache-refused-turn (list idle user)) user))
    (should-not (opencode-cache-refused-turn
                 (list idle '((type . "assistant") (content ((type . "text") (text . "x")))) user)))
    (should-not (opencode-cache-refused-turn
                 (list idle '((id . "msg_u") (type . "user") (text . "see") (files ((uri . "file:///x")))))))
    (should (opencode-cache-thinking-refusal-p '((type . "thinking.stale") (message . "Stale thinking: 2 …"))))
    (should (opencode-cache-thinking-refusal-p
             '((type . "provider.invalid-request")
               (message . "messages.1.content.0: Invalid `signature` in `thinking` block"))))
    (should-not (opencode-cache-thinking-refusal-p '((type . "provider.rate-limit") (message . "Slow down"))))))

(ert-deftest opencode-test-refused-turn-resend ()
  "A refused turn is reverted and its text put back in the prompt."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (opencode-test-routes
            (cons (list 'get "/message\\'"
                        (lambda (request)
                          (cond
                           ((equal (alist-get 'order (plist-get request :query)) "desc")
                            '((data ((id . "msg_i") (type . "idle"))
                                    ((id . "msg_u") (type . "user") (text . "fix it")))
                              (cursor)))
                           ((plist-get request :cursor-seen) '((data) (cursor)))
                           (t (opencode-test-fixture
                               (format "messages-%s.json" (opencode-test-scenario "text")))))))
                  opencode-test-routes)))
      (with-current-buffer (opencode-test-open id)
        (opencode-test-dispatch
         `(((type . "session.execution.failed")
            (data (sessionID . ,id)
                  (error (type . "thinking.stale") (message . "Stale thinking: 1 thinking block …"))))))
        (should (equal (opencode-test-sent-body "/revert/stage") '((messageID . "msg_u") (files . :false))))
        (should (equal (buffer-substring-no-properties (opencode-pmark) (point-max)) "fix it"))
        (should (string-match-p "Your message is back in the prompt" (opencode-test-text (current-buffer))))))))

(provide 'opencode-cache-test)
;;; opencode-cache-test.el ends here
