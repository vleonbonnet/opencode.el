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

(ert-deftest opencode-test-shown-prompt-rendered-on-delivery ()
  "A prompt sent with :show appears once the server delivers it, consenting if asked."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty id)))
      (with-current-buffer buffer
        (let ((inbox (opencode-session-send "scheduled check" :show t :accept-thinking-loss t)))
          (should (equal (alist-get 'metadata (plist-get (car opencode-test-requests) :body))
                         '((thinkingLoss . "accept"))))
          (opencode-test-dispatch
           `(((type . "session.inbox.enqueued")
              (data (sessionID . ,id) (inboxID . ,inbox)
                    (item (type . "user") (delivery . "steer") (payload (text . "scheduled check")))))
             ((type . "session.inbox.delivered") (data (sessionID . ,id) (inboxID . ,inbox)))))
          (should (string-match-p "scheduled check" (opencode-test-text buffer))))))))

(defconst opencode-test-pinned-model '((id . "claude-opus-5-5") (providerID . "anthropic") (variant . "high"))
  "The model `opencode-test-agent-routes' has the plan agent pin.")

(defun opencode-test-agent-routes (&optional agents)
  "Return the routes with AGENTS as the agent catalog.
By default: build first, as the service lists its default agent, then a
subagent, a hidden agent, plan pinning `opencode-test-pinned-model' and
an agent of mode `all'."
  (cons (list 'get "\\`/api/agent\\'"
              (lambda (_)
                `((data . ,(or agents
                               `(((id . "build") (mode . "primary"))
                                 ((id . "general") (mode . "subagent"))
                                 ((id . "title") (mode . "primary") (hidden . t))
                                 ((id . "plan") (mode . "primary") (model . ,opencode-test-pinned-model))
                                 ((id . "review") (mode . "all"))))))))
        opencode-test-routes))

(defun opencode-test-requests-to (method suffix)
  "Return the bodies of the METHOD requests whose path ends in SUFFIX, oldest first."
  (nreverse (delq nil (mapcar (lambda (request)
                                (when (and (eq (plist-get request :method) method)
                                           (string-suffix-p suffix (plist-get request :path)))
                                  (or (plist-get request :body) t)))
                              opencode-test-requests))))

(ert-deftest opencode-test-selectable-agents ()
  "Sessions can run every agent the service can select, the default first."
  (opencode-test-with-server
    (let ((opencode-test-routes (opencode-test-agent-routes)))
      (opencode-catalog-fetch 'agent #'ignore opencode-test-workspace)
      (should (equal (mapcar (lambda (agent) (alist-get 'id agent))
                             (opencode-primary-agents opencode-test-workspace))
                     '("build" "plan" "review")))
      (should (equal (opencode-default-agent opencode-test-workspace) "build")))))

(ert-deftest opencode-test-catalog-failure-callback ()
  "A failed catalog fetch reaches its ELSE, and is reported only to callers without one."
  (opencode-test-with-server
    (let ((opencode-test-routes (cons (list 'get "\\`/api/agent\\'" (lambda (_) '(:error 500 "boom")))
                                      opencode-test-routes))
          (reported nil)
          (failure nil))
      (cl-letf (((symbol-function 'opencode-report-error) (lambda (text) (push text reported))))
        (opencode-catalog-fetch 'agent (lambda (_) (ert-fail "Fetched")) opencode-test-workspace
                                (lambda (error) (setq failure error)))
        (should (eql (opencode-error-status failure) 500))
        (should-not reported)
        (opencode-catalog-fetch 'agent (lambda (_) (ert-fail "Fetched")) opencode-test-workspace)
        (should (string-match-p "boom" (car reported)))))))

(ert-deftest opencode-test-create-model-precedence ()
  "A new session runs the explicit model, else its agent's pin, else the last pick."
  (opencode-test-with-server
    (let* ((info (alist-get 'data (opencode-test-fixture
                                   (format "session-%s.json" (opencode-test-scenario "text")))))
           (opencode-test-routes
            (cons (list 'post "\\`/api/session\\'" (lambda (_) `((data . ,info))))
                  (opencode-test-agent-routes)))
           (opencode-last-model '((id . "gpt-6-luna") (providerID . "openai")))
           (explicit '((id . "gpt-6-sol") (providerID . "openai"))))
      (cl-flet ((created-model (&rest arguments)
                  (setq opencode-test-requests nil)
                  (apply #'opencode-session-create :directory opencode-test-workspace :no-display t arguments)
                  (kill-buffer (gethash (alist-get 'id info) opencode-session-buffers))
                  (alist-get 'model (car (opencode-test-requests-to 'post "/api/session")))))
        (should (equal (created-model :agent "plan") opencode-test-pinned-model))
        (should (equal (created-model :agent "plan" :model explicit) explicit))
        (should (equal (created-model :agent "review") opencode-last-model))
        ;; Without an agent, the default agent's pin decides.
        (should (equal (created-model) opencode-last-model))
        (let ((opencode-test-routes
               (opencode-test-agent-routes
                `(((id . "plan") (mode . "primary") (model . ,opencode-test-pinned-model))
                  ((id . "build") (mode . "primary"))))))
          (clrhash opencode--catalogs)
          (should (equal (created-model) opencode-test-pinned-model)))))))

(ert-deftest opencode-test-switch-agent-follows-pin ()
  "Switching to an agent that pins a model switches to that model, then calls back."
  (opencode-test-with-server
    (let ((opencode-test-routes (opencode-test-agent-routes))
          (opencode-last-model nil)
          (done nil))
      (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
        (setq opencode-test-requests nil)
        (opencode-switch-agent "plan" :then (lambda () (setq done (current-buffer))))
        (should (eq done (current-buffer)))
        (should (equal (opencode-test-requests-to 'post "/agent") '(((agent . "plan")))))
        (should (equal (opencode-test-requests-to 'post "/model") `(((model . ,opencode-test-pinned-model)))))
        (should (equal opencode-session-agent "plan"))
        (should (equal opencode-session-model opencode-test-pinned-model))
        (should (equal opencode-last-model opencode-test-pinned-model))
        ;; An agent without a pin keeps the session's model.
        (setq opencode-test-requests nil done nil)
        (opencode-switch-agent "review" :then (lambda () (setq done t)))
        (should done)
        (should-not (opencode-test-requests-to 'post "/model"))
        ;; An explicit model replaces the pin rather than following it.
        (let ((explicit '((id . "gpt-6-sol") (providerID . "openai"))))
          (setq opencode-test-requests nil)
          (opencode-switch-agent "plan" :model explicit)
          (should (equal (opencode-test-requests-to 'post "/model") `(((model . ,explicit)))))
          (should (equal opencode-session-model explicit)))))))

(ert-deftest opencode-test-switch-agent-refuses-unknown ()
  "An agent the session cannot run is refused before the service, which would select it."
  (opencode-test-with-server
    (let ((opencode-test-routes (opencode-test-agent-routes))
          (failure nil))
      (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
        (setq opencode-test-requests nil)
        (dolist (agent '("general" "title" "nope"))
          (setq failure nil)
          (opencode-switch-agent agent :then (lambda () (ert-fail "Switched"))
                                 :else (lambda (error) (setq failure error)))
          (should (string-match-p (format "cannot run as agent %s" agent) (opencode-error-describe failure))))
        (should-not (opencode-test-requests-to 'post "/agent"))))))

(ert-deftest opencode-test-switch-model-same-skips ()
  "Selecting the model and variant a session runs sends nothing, and calls back."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
      (let ((done nil)
            (same (copy-tree opencode-session-model)))
        (setq opencode-test-requests nil)
        (opencode-switch-model same :then (lambda () (setq done t)))
        (should done)
        (should-not (opencode-test-requests-to 'post "/model"))
        (opencode-switch-model (assq-delete-all 'variant same) :then #'ignore)
        (should (opencode-test-requests-to 'post "/model"))))))

(ert-deftest opencode-test-open-connects-events ()
  "Opening a session opens the event stream it follows."
  (opencode-test-with-server
    (let ((connects 0))
      (cl-letf (((symbol-function 'opencode-events-connect) (lambda (&rest _) (cl-incf connects))))
        (opencode-test-open (opencode-test-scenario "text"))
        (should (> connects 0))))))

(ert-deftest opencode-test-prompts-ask-for-stale-thinking ()
  "Prompts and commands ask the server to refuse stale thinking unless they consent."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (opencode-session-send "plain")
      (should (equal (alist-get 'metadata (plist-get (car opencode-test-requests) :body))
                     '((thinkingLoss . "ask"))))
      (opencode-session-send "tagged" :metadata '((source . "scheduler")))
      (should (equal (alist-get 'metadata (plist-get (car opencode-test-requests) :body))
                     '((thinkingLoss . "ask") (source . "scheduler"))))
      (opencode-session-command "review" "HEAD" :accept-thinking-loss t)
      (should (equal (alist-get 'metadata (plist-get (car opencode-test-requests) :body))
                     '((thinkingLoss . "accept")))))))

(ert-deftest opencode-test-open-missing-session ()
  "Opening a session the server does not know reaches the error callback."
  (opencode-test-with-server
    (let (failure)
      (opencode-session-open-by-id "ses_missing" :no-display t
                                   :callback (lambda (_) (ert-fail "Opened"))
                                   :else (lambda (error) (setq failure error)))
      (should (eql (opencode-error-status failure) 404)))))

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
