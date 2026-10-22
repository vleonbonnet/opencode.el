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

(defun opencode-test-label-boxes (buffer)
  "Return the box colors of BUFFER's tool block labels, in buffer order."
  (with-current-buffer buffer
    (mapcar (lambda (overlay)
              (let ((start (overlay-start overlay)))
                (seq-some (lambda (position)
                            (let ((face (get-text-property position 'font-lock-face)))
                              (and (consp face) (plist-get (plist-get face :box) :color))))
                          (number-sequence start (save-excursion (goto-char start) (1- (line-end-position)))))))
            (sort (opencode-tool-blocks) :key #'overlay-start))))

(ert-deftest opencode-test-live-tool-colors-match-replay ()
  "Finished live tool blocks take the colors replayed ones are drawn in, labels included."
  (let ((running (face-attribute 'opencode-tool-running-margin :foreground))
        (finished (face-attribute 'opencode-tool-margin-highlight :foreground)))
    (unwind-protect
        (progn
          (set-face-attribute 'opencode-tool-running-margin nil :foreground "blue")
          (set-face-attribute 'opencode-tool-margin-highlight nil :foreground "green")
          (dolist (scenario '("read-tools" "shell" "edit-tools"))
            (opencode-test-with-server
              (let* ((id (opencode-test-scenario scenario))
                     (replayed (opencode-test-label-boxes (opencode-test-open id))))
                (kill-buffer (gethash id opencode-session-buffers))
                (let ((live (opencode-test-open-empty id)))
                  (opencode-test-dispatch (opencode-test-events id))
                  (should (member "green" replayed))
                  (should-not (member "blue" replayed))
                  (should (equal (opencode-test-label-boxes live) replayed)))))))
      (set-face-attribute 'opencode-tool-running-margin nil :foreground running)
      (set-face-attribute 'opencode-tool-margin-highlight nil :foreground finished))))

;; The shell scenario runs one command in the background; the service posts its completion as a synthetic message.
(defun opencode-test-summary-text (overlay)
  "Return the summary line of block OVERLAY, without properties."
  (with-current-buffer (overlay-buffer overlay)
    (buffer-substring-no-properties (overlay-start overlay) (1- (overlay-end overlay)))))

(defun opencode-test-shell-messages (function)
  "Return the shell scenario's messages, its synthetic ones passed through FUNCTION."
  (mapcan (lambda (message)
            (if (equal (alist-get 'type message) "synthetic")
                (funcall function message)
              (list message)))
          (alist-get 'data (opencode-test-fixture
                            (format "messages-%s.json" (opencode-test-scenario "shell"))))))

(defun opencode-test-open-shell-with (messages)
  "Open the shell scenario's session, its transcript being MESSAGES."
  (let ((opencode-test-routes
         (cons (list 'get "/message\\'" (lambda (_) `((data . ,messages) (cursor)))) opencode-test-routes)))
    (opencode-test-open (opencode-test-scenario "shell"))))

(ert-deftest opencode-test-background-shell-completion ()
  "A call that moved its command to the background says so; the completion is a shell block, replayed or live."
  (let* ((completion (seq-find (lambda (message) (equal (alist-get 'type message) "synthetic"))
                               (opencode-test-shell-messages #'list)))
         (key (alist-get 'id completion))
         (command (alist-get 'description completion)))
    (should (equal (alist-get 'source (alist-get 'metadata completion)) "shell"))
    (dolist (live '(nil t))
      (opencode-test-with-server
        (let* ((id (opencode-test-scenario "shell"))
               (buffer (if live (opencode-test-open-empty id) (opencode-test-open id))))
          (when live (opencode-test-dispatch (opencode-test-events id)))
          (with-current-buffer buffer
            (let* ((calls (seq-filter (lambda (overlay)
                                        (equal (alist-get 'shellID (plist-get (opencode-block-data overlay) :metadata))
                                               (alist-get 'shellID (alist-get 'metadata completion))))
                                      (opencode-tool-blocks)))
                   (call (seq-find (lambda (overlay) (not (eq overlay (opencode-block key)))) calls))
                   (block (opencode-block key))
                   (data (opencode-block-data block)))
              (should call)
              (should (string-suffix-p (concat command " · in the background") (opencode-test-summary-text call)))
              (should (equal (plist-get data :type) 'tool))
              (should (equal (plist-get data :tool) "shell"))
              (should (equal (plist-get data :status) "completed"))
              (should (equal (string-trim (plist-get data :output)) "background-done"))
              (should (eq (opencode-block-face data) 'opencode-tool-margin-highlight))
              (should (string-suffix-p (concat command " · finished in the background")
                                       (opencode-test-summary-text block)))
              (should (string-match-p "\\`\\$ sleep 3 && echo background-done\n# in the background\n"
                                      (opencode--expanded-content block)))
              (should-not (string-match-p "<shell" (opencode--expanded-content block))))))))))

(ert-deftest opencode-test-background-completion-failures ()
  "A background command that failed or was cancelled shows in the error face, with its reason."
  (pcase-dolist (`(,state ,text ,note ,reason)
                 '(("error" "Command failed" "failed in the background" "Failed: Command failed")
                   ("cancelled" "Cancelled" "cancelled in the background" "Interrupted")))
    (opencode-test-with-server
      (let* ((key nil)
             (messages
              (opencode-test-shell-messages
               (lambda (message)
                 (let-alist message
                   (setq key .id)
                   (list `((id . ,.id) (type . "synthetic") (time . ,.time) (description . ,.description)
                           (text . ,(format "<shell id=\"%s\" state=\"%s\" command=\"%s\">\n%s\n</shell>"
                                            .metadata.jobID state .description text))
                           (metadata (source . "shell") (shellID . ,.metadata.shellID)
                                     (jobID . ,.metadata.jobID) (state . ,state))))))))
             (buffer (opencode-test-open-shell-with messages)))
        (with-current-buffer buffer
          (let* ((block (opencode-block key))
                 (data (opencode-block-data block)))
            (should (equal (plist-get data :status) "error"))
            (should (eq (opencode-block-face data) 'error))
            (should (string-suffix-p note (opencode-test-summary-text block)))
            (should (string-suffix-p reason (opencode--expanded-content block)))))))))

(ert-deftest opencode-test-background-command-outcomes ()
  "A completed background command that exited non-zero, was killed or timed out shows in the error face."
  (pcase-dolist (`(,extra ,note ,face)
                 '((((exit . 0)) "finished in the background" opencode-tool-margin-highlight)
                   (((exit . 255)) "exited with code 255 in the background" error)
                   (((signal . "SIGKILL")) "killed by SIGKILL in the background" error)
                   (((timeout . t)) "timed out in the background" error)))
    (opencode-test-with-server
      (let* ((key nil)
             (messages (opencode-test-shell-messages
                        (lambda (message)
                          (setq key (alist-get 'id message))
                          (let ((metadata (append extra (assq-delete-all
                                                         'exit (copy-alist (alist-get 'metadata message))))))
                            (list (cons (cons 'metadata metadata) (assq-delete-all 'metadata (copy-alist message))))))))
             (buffer (opencode-test-open-shell-with messages)))
        (with-current-buffer buffer
          (let* ((block (opencode-block key))
                 (data (opencode-block-data block)))
            (should (equal (plist-get data :status) "completed"))
            (should (equal (string-trim (plist-get data :output)) "background-done"))
            (should (eq (opencode-block-face data) face))
            (should (string-suffix-p note (opencode-test-summary-text block)))))))))

(ert-deftest opencode-test-background-subagent-completion ()
  "A background subagent's completion is a subagent block holding its answer and opening its session."
  (opencode-test-with-server
    (let* ((completion
            '((id . "msg_test_background_subagent") (type . "synthetic") (time (created . 1))
              (description . "Survey the fixtures")
              (text . "<subagent sessionID=\"ses_test_child\" state=\"completed\" description=\"Survey the fixtures\">\nThree fixtures.\n</subagent>")
              (metadata (source . "subagent") (childID . "ses_test_child") (agent . "explore")
                        (state . "completed"))))
           (buffer (opencode-test-open-shell-with
                    (opencode-test-shell-messages (lambda (message) (list message completion)))))
           opened)
      (with-current-buffer buffer
        (let* ((block (opencode-block "msg_test_background_subagent"))
               (data (opencode-block-data block)))
          (should (equal (plist-get data :tool) "subagent"))
          (should (equal (plist-get data :session-id) "ses_test_child"))
          (should (equal (plist-get data :output) "Three fixtures."))
          (should (string-suffix-p "Explore: Survey the fixtures · finished in the background"
                                   (opencode-test-summary-text block)))
          (cl-letf (((symbol-function 'opencode-session-open-by-id) (lambda (id &rest _) (setq opened id))))
            (goto-char (overlay-start block))
            (opencode-open-link-at-point))
          (should (equal opened "ses_test_child")))))))

(ert-deftest opencode-test-background-completion-unwrapped ()
  "A completion whose text lacks the wrapper the service writes shows all of its text."
  (opencode-test-with-server
    (let* ((key nil)
           (messages (opencode-test-shell-messages
                      (lambda (message)
                        (setq key (alist-get 'id message))
                        (list (cons '(text . "background-done, unwrapped")
                                    (assq-delete-all 'text (copy-alist message)))))))
           (buffer (opencode-test-open-shell-with messages)))
      (with-current-buffer buffer
        (should (equal (plist-get (opencode-block-data (opencode-block key)) :output)
                       "background-done, unwrapped"))))))

(ert-deftest opencode-test-background-note ()
  "Only a shell or subagent call that left its job running is marked as in the background."
  (should (equal (opencode-tool-background-note
                  '(:tool "subagent" :status "completed" :metadata ((sessionID . "ses_x") (status . "running"))))
                 "in the background"))
  (should-not (opencode-tool-background-note
               '(:tool "subagent" :status "completed" :metadata ((sessionID . "ses_x") (status . "completed")))))
  (should-not (opencode-tool-background-note
               '(:tool "shell" :status "running" :metadata ((status . "running")))))
  (should-not (opencode-tool-background-note
               '(:tool "slack_post" :status "completed" :metadata ((status . "running"))))))

(ert-deftest opencode-test-other-synthetic-messages-stay-notices ()
  "Synthetic messages that are no background job's completion, or whose text is not one, stay notices."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "switch"))
      (should-not (opencode-tool-blocks))
      (should (string-match-p "synthetic message" (opencode-test-text (current-buffer))))))
  (opencode-test-with-server
    (let* ((key nil)
           (messages (opencode-test-shell-messages
                      (lambda (message)
                        (setq key (alist-get 'id message))
                        ;; A user's shell command, whose own shell block is drawn already.
                        (list (cons '(text . "The following shell command was executed by the user:\n<shell>")
                                    (assq-delete-all 'description (assq-delete-all 'text (copy-alist message))))))))
           (buffer (opencode-test-open-shell-with messages)))
      (with-current-buffer buffer
        (should (eq (plist-get (opencode-block-data (opencode-block key)) :type) 'notice))))))

;; Signed reasoning as Anthropic sends it: the signature's decoded header names the block's kind.
(defun opencode-test-signature (kind)
  "Return a signature whose decoded header names block KIND."
  (base64-encode-string (concat (string #x12 #x05) "B" (string (length kind)) kind (string 0) (make-string 45 7)) t))

(defconst opencode-test-signed-reasoning
  `(((text . "Narrated for the user.") (native (signature . ,(opencode-test-signature "narration"))))
    ((text . "") (native (signature . ,(opencode-test-signature "thinking")))))
  "Reasoning blocks of one assistant message: narration, then withheld thinking.")

(defun opencode-test-signed-reasoning-labels (buffer message)
  "Return how BUFFER shows the signed reasoning blocks of MESSAGE."
  (with-current-buffer buffer
    ;; Narration is prose: no reasoning block holds it, replayed or live.
    (list (let ((block (opencode-block (opencode-reasoning-key message 0))))
            (or (null block) (overlay-get block 'opencode-narration)))
          (plist-get (opencode-block-data (opencode-block (opencode-reasoning-key message 1))) :hidden-kind)
          (and (string-match-p "Narrated for the user\\." (opencode-test-text buffer)) t))))

(ert-deftest opencode-test-signed-reasoning ()
  "Narration shows as prose and withheld thinking by its kind, replayed or live."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "reasoning"))
           (messages (alist-get 'data (opencode-test-fixture (format "messages-%s.json" id))))
           (assistant (seq-find (lambda (message) (equal (alist-get 'type message) "assistant")) messages))
           (message (alist-get 'id assistant))
           (signed (mapcar (lambda (message)
                             (if (eq message assistant)
                                 (cons (cons 'content (mapcar (lambda (block) (cons '(type . "reasoning") block))
                                                              opencode-test-signed-reasoning))
                                       (assq-delete-all 'content (copy-alist message)))
                               message))
                           messages))
           (opencode-test-routes
            (cons (list 'get "/message\\'" (lambda (_) `((data . ,signed) (cursor))))
                  opencode-test-routes)))
      (should (equal (opencode-test-signed-reasoning-labels (opencode-test-open id) message)
                     '(t "thinking" t)))
      (kill-buffer (gethash id opencode-session-buffers))
      (let ((live (opencode-test-open-empty id)))
        (seq-do-indexed
         (lambda (block ordinal)
           (let ((base `((sessionID . ,id) (assistantMessageID . ,message) (ordinal . ,ordinal))))
             (opencode-test-dispatch
              `(((type . "session.reasoning.started") (data ,@base (native . ,(alist-get 'native block))))
                ,@(unless (string-empty-p (alist-get 'text block))
                    `(((type . "session.reasoning.delta") (data ,@base (delta . ,(alist-get 'text block))))))
                ((type . "session.reasoning.ended") (data ,@base ,@block))))))
         opencode-test-signed-reasoning)
        (should (equal (opencode-test-signed-reasoning-labels live message) '(t "thinking" t)))))))

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

(ert-deftest opencode-test-transcript-pages ()
  "A transcript spread over pages replays whole; later pages ask by cursor alone."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "shell"))
           (whole (opencode-test-text (opencode-test-open id)))
           (messages (alist-get 'data (opencode-test-fixture (format "messages-%s.json" id))))
           (half (/ (length messages) 2))
           (opencode-test-routes
            (cons (list 'get "/message\\'"
                        (lambda (request)
                          (pcase (alist-get 'cursor (plist-get request :query))
                            ('nil `((data . ,(seq-take messages half)) (cursor (next . "c1"))))
                            ("c1" `((data . ,(seq-drop messages half)) (cursor (next . "c2"))))
                            (_ '((data) (cursor (next . "c3")))))))
                  opencode-test-routes)))
      (should (> half 0))
      (kill-buffer (gethash id opencode-session-buffers))
      (setq opencode-test-requests nil)
      (should (equal (opencode-test-text (opencode-test-open id)) whole))
      (let ((pages (seq-filter (lambda (request) (string-suffix-p "/message" (plist-get request :path)))
                               (reverse opencode-test-requests))))
        (should (= (length pages) 3))
        (should (assq 'order (plist-get (car pages) :query)))
        (dolist (page (cdr pages))
          (should-not (opencode-test-cursor-with-order-p page)))))))

(ert-deftest opencode-test-session-command-requests ()
  "Each session command asks the service with the route and body it documents."
  (opencode-test-with-server
    (let* ((other (alist-get 'data (opencode-test-fixture
                                    (format "session-%s.json" (opencode-test-scenario "shell")))))
           (opencode-test-routes
            (append (list (list 'post "/fork\\'" (lambda (_) `((data . ,other))))
                          (list 'post "/interrupt\\'" (lambda (_) '((interrupted . t)))))
                    opencode-test-routes)))
      (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
        (let ((id opencode-session-id)
              (prompt (text-property-any (point-min) (opencode-pmark) 'opencode-prompt t)))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Renamed"))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'switch-to-buffer) #'ignore))
            (setq opencode-test-requests nil)
            (opencode-interrupt)
            (opencode-rename-session)
            (opencode-compact-session)
            (goto-char prompt)
            (opencode-fork-session)
            (opencode-revert-to-point)
            (opencode-revert-clear)
            (opencode-delete-session))
          (should (equal (mapcar (lambda (request)
                                   (list (plist-get request :method)
                                         (string-remove-prefix (concat "/api/session/" id)
                                                               (plist-get request :path))))
                                 (seq-filter (lambda (request)
                                               (string-prefix-p (concat "/api/session/" id)
                                                                (plist-get request :path)))
                                             (reverse opencode-test-requests)))
                         '((post "/interrupt") (patch "") (post "/compact") (post "/fork")
                           (post "/revert/stage") (delete "/revert") (delete ""))))
          (should (equal (plist-get (seq-find (lambda (request) (eq (plist-get request :method) 'patch))
                                              opencode-test-requests)
                                    :body)
                         '((title . "Renamed"))))
          (should (alist-get 'before (plist-get (seq-find (lambda (request)
                                                            (string-suffix-p "/fork" (plist-get request :path)))
                                                          opencode-test-requests)
                                                :body))))))))

(ert-deftest opencode-test-boundary-drops-tool-steps ()
  "A fork or revert at a prompt cuts before its answer's first step, tool calls included."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "read-tools"))
           (messages (alist-get 'data (opencode-test-fixture (format "messages-%s.json" id))))
           (user (seq-position messages "user" (lambda (message type) (equal (alist-get 'type message) type))))
           (answer (alist-get 'id (nth (1+ user) messages))))
      (with-current-buffer (opencode-test-open id)
        (should (equal (alist-get 'type (nth (1+ user) messages)) "assistant"))
        (should (opencode-tool-blocks))
        (let ((prompt (text-property-any (point-min) (opencode-pmark) 'opencode-prompt t)))
          (should (equal (opencode--message-boundary prompt) answer)))
        ;; Within the answer, the cut keeps it: the next prompt starts what is dropped.
        (let ((block (car (sort (opencode-tool-blocks) :key #'overlay-start))))
          (should (equal (opencode-message-at (overlay-start block)) answer)))))))

(ert-deftest opencode-test-pickers-wait-for-catalogs ()
  "Commands that prompt with a catalog fetch it first, then ask."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
      (let ((offered nil))
        (cl-letf (((symbol-function 'opencode-annotated-completion)
                   (lambda (_prompt entries) (setq offered (mapcar #'car entries)) nil)))
          (clrhash opencode--catalogs)
          (goto-char (opencode-pmark))
          (opencode-insert-slash-command)
          (should offered)
          (setq offered nil)
          (clrhash opencode--catalogs)
          (opencode-select-agent)
          (should (equal offered '("build" "plan"))))))))

(ert-deftest opencode-test-report-error ()
  "Errors of asynchronous callbacks are reported like a command's, never exiting Emacs."
  (let ((calls nil))
    (let ((command-error-function (lambda (&rest arguments) (push arguments calls))))
      ;; Batch Emacs has no frame to show it in.
      (opencode-report-error "first")
      (should-not calls)
      (with-current-buffer "*Messages*"
        (should (string-match-p "^first$" (buffer-string))))
      (let ((noninteractive nil))
        (cl-letf (((symbol-function 'frame-initial-p) #'ignore))
          (opencode-report-error "second")))
      (should (equal calls '(((user-error "second") "" nil)))))))

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
