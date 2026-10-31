;;; opencode-requests-test.el --- Tests of permissions and questions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(ert-deftest opencode-test-permission-marks-its-tool ()
  "A permission request marks the block of the call that asked, with its command and rule."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "permission"))
           (buffer (opencode-test-open-empty id))
           (rest (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")))
      (with-current-buffer buffer
        (should (= (length opencode-pending-permissions) 1))
        (let* ((permission (car opencode-pending-permissions))
               (overlay (opencode--permission-block permission)))
          (should (equal (plist-get permission :title) "Shell command: rm junk.txt"))
          (should overlay)
          (should (eq (plist-get (opencode-block-data overlay) :waiting-kind) 'permission))
          (should (string-match-p "rm junk.txt · asked by rule \"rm \\*\""
                                  (buffer-substring (overlay-start overlay) (overlay-end overlay))))
          (should (opencode-pending-request-p))
          ;; Answered in another client.
          (opencode-test-dispatch-until rest "permission.replied")
          (should-not opencode-pending-permissions)
          (should-not (plist-get (opencode-block-data overlay) :waiting)))))))

(ert-deftest opencode-test-permission-summary-keeps-command ()
  "A block waiting on a permission shows its command, then the rule that asked."
  (should (string-match-p "rm junk.txt · asked by rule \"rm \\*\""
                          (opencode-tool-collapsed-summary
                           '(:type tool :tool "shell" :input ((command . "rm junk.txt"))
                             :waiting t :waiting-kind permission
                             :waiting-description "asked by rule \"rm *\"")))))

(ert-deftest opencode-test-permission-reply-request ()
  "Answering sends the decision to the session's reply route."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "permission"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")
      (with-current-buffer buffer
        (let ((permission (car opencode-pending-permissions)))
          (opencode-permission-reply permission 'reject "Not that one.")
          (let ((request (car opencode-test-requests)))
            (should (equal (plist-get request :path)
                           (format "/api/session/%s/permission/%s/reply" id (plist-get permission :id))))
            (should (equal (plist-get request :body)
                           '((decision . "reject") (message . "Not that one.")))))
          (should-not opencode-pending-permissions))))))

(ert-deftest opencode-test-permission-reasons ()
  "Reasons are grouped by rule; a long resource shows the line its rule matched."
  (let ((script "python3 - <<'EOF'\nimport os\nmode = 'kanopy-oidc'\nEOF")
        (oidc '((action . "shell") (resource . "*kanopy-oidc*") (effect . "ask")))
        (dynamic '((action . "shell") (resource . "\"$*") (effect . "ask"))))
    (should-not (opencode-permission-reasons-text nil))
    (should (equal (substring-no-properties
                    (opencode-permission-reasons-text
                     (list `((resource . ,script) (rule . ,oidc))
                           `((resource . "\"$W\" status 2>&1") (rule . ,dynamic))
                           `((resource . "\"$W\" status") (rule . ,dynamic))
                           '((resource . "curl example.com")))))
                   (concat "\n\nAsked because:"
                           "\n  shell rule \"*kanopy-oidc*\" asks for:"
                           "\n    python3 - <<'EOF' …"
                           "\n      line 3: mode = 'kanopy-oidc'"
                           "\n  shell rule \"\"$*\" asks for:"
                           "\n    \"$W\" status 2>&1"
                           "\n  no rule allows:"
                           "\n    curl example.com")))
    (should (equal (opencode-permission-reasons-summary
                    `(((resource . "a") (rule . ,dynamic)) ((resource . "c"))))
                    "asked by rule \"\"$*\"; no rule allows the rest"))))

(ert-deftest opencode-test-permission-reject-commands ()
  "Plain rejection omits feedback; the feedback command sends its trimmed text."
  (dolist (feedback '(nil "  Use a patch instead.  "))
    (opencode-test-with-server
      (let* ((id (opencode-test-scenario "permission"))
             (buffer (opencode-test-open-empty id)))
        (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")
        (with-current-buffer buffer
          (let ((opencode--permission-transient-state
                 (list :buffer buffer :permission (car opencode-pending-permissions))))
            (if feedback
                (opencode-permission-reject-with-feedback feedback)
              (opencode-permission-reject))
            (should (equal (plist-get (car opencode-test-requests) :body)
                           (if feedback
                               '((decision . "reject") (message . "Use a patch instead."))
                             '((decision . "reject")))))
            (should-not opencode-pending-permissions)))))))

(ert-deftest opencode-test-permission-empty-feedback-stays-pending ()
  "Empty feedback never silently becomes a plain rejection."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "permission"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")
      (with-current-buffer buffer
        (let* ((permission (car opencode-pending-permissions))
               (opencode--permission-transient-state (list :buffer buffer :permission permission))
               (requests (length opencode-test-requests)))
          (should-error (opencode-permission-reject-with-feedback "  ") :type 'user-error)
          (should (equal (length opencode-test-requests) requests))
          (should (memq permission opencode-pending-permissions))
          (should (eq (plist-get opencode--permission-transient-state :permission) permission)))))))

(ert-deftest opencode-test-permission-reasons-spelling ()
  "A rule matching another spelling of a command shows that spelling, and locates its match there."
  (let ((push '((action . "shell") (resource . "git push *") (effect . "ask")))
        (oidc '((action . "shell") (resource . "*kanopy-oidc*") (effect . "ask"))))
    (should (equal (substring-no-properties
                    (opencode-permission-reasons-text
                     (list `((resource . "/usr/bin/git push origin") (spelling . "git push origin") (rule . ,push))
                           `((resource . "bash -c \"$S\"")
                             (spelling . "set -e\ncd src\nmode=kanopy-oidc")
                             (rule . ,oidc)))))
                   (concat "\n\nAsked because:"
                           "\n  shell rule \"git push *\" asks for:"
                           "\n    /usr/bin/git push origin"
                           "\n      as: git push origin"
                           "\n  shell rule \"*kanopy-oidc*\" asks for:"
                           "\n    bash -c \"$S\""
                           "\n      as: set -e …"
                           "\n      line 3: mode=kanopy-oidc")))))

(ert-deftest opencode-test-question-form-on-its-block ()
  "A question form marks its tool block and is answered with option values per field."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "question"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-dispatch-until (opencode-test-events id) "form.created")
      (with-current-buffer buffer
        (should (= (length (opencode-open-forms)) 1))
        (let* ((entry (car opencode-pending-forms))
               (overlay (opencode--form-block entry)))
          (should overlay)
          (should (eq (plist-get (opencode-block-data overlay) :waiting-kind) 'question))
          (should (overlay-get overlay 'opencode-expanded))
          ;; Blue in the first question, Apple and Plum in the second.
          (aset (plist-get entry :answers) 0 (cons (vector nil t) nil))
          (aset (plist-get entry :answers) 1 (cons (vector t nil t) nil))
          (opencode-form-send entry)
          (let ((request (car opencode-test-requests)))
            (should (string-suffix-p "/reply" (plist-get request :path)))
            (should (equal (plist-get request :body)
                           '((answer . ((q0 . "Blue") (q1 . ["Apple" "Plum"])))))))
          (should-not opencode-pending-forms))))))

(defun opencode-test-tool-form (call-id)
  "Return a web search consent form, as tool call CALL-ID asks it."
  `((id . "frm_websearch") (sessionID . "ses_test") (title . "Web Search")
    (metadata (kind . "websearch.provider") (tool (messageID . "msg_test") (id . ,call-id)))
    (fields ((key . "choice") (type . "string") (required . t) (custom . :false)
             (description . "Allow OpenCode to search the web for up-to-date information?")
             (options ((value . "allow") (label . "Allow search via Exa"))
                      ((value . "disable") (label . "Disable web search")))))))

(defun opencode-test-summary (overlay)
  "Return block OVERLAY's summary line."
  (buffer-substring-no-properties (overlay-start overlay) (1- (overlay-end overlay))))

(ert-deftest opencode-test-tool-form-on-its-block ()
  "A form another tool asks marks that tool's block, named by its title, without expanding it."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "read-tools"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-dispatch-until (opencode-test-events id) "session.tool.called")
      (with-current-buffer buffer
        (let* ((overlay (car (sort (opencode-tool-blocks) :key #'overlay-start)))
               (entry (opencode-form-queue
                       (opencode-test-tool-form (plist-get (opencode-block-data overlay) :key)))))
          (should (eq (opencode--form-block entry) overlay))
          (should (plist-get (opencode-block-data overlay) :waiting))
          (should (eq (opencode-block-face (opencode-block-data overlay)) 'opencode-attention))
          (should (string-suffix-p " · Web Search" (opencode-test-summary overlay)))
          (should-not (overlay-get overlay 'opencode-expanded))
          ;; The block shows the form, so no separate notice does.
          (should-not (string-search "Form: Web Search" (buffer-string)))
          (opencode--form-resolved "frm_websearch")
          (should-not (plist-get (opencode-block-data overlay) :waiting))
          (should-not (string-search "Web Search" (opencode-test-summary overlay))))))))

(ert-deftest opencode-test-tool-form-before-its-block ()
  "A form queued before its tool's block is drawn marks the block once it is."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "read-tools"))
           (buffer (opencode-test-open-empty id))
           (events (opencode-test-events id))
           (call (alist-get 'id (alist-get 'data (seq-find (lambda (event)
                                                             (equal (alist-get 'type event)
                                                                    "session.tool.input.started"))
                                                           events)))))
      (with-current-buffer buffer
        (opencode-form-queue (opencode-test-tool-form call)))
      (opencode-test-dispatch-until events "session.tool.called")
      (with-current-buffer buffer
        (let ((overlay (opencode-block call)))
          (should overlay)
          (should (plist-get (opencode-block-data overlay) :waiting))
          (should (string-suffix-p " · Web Search" (opencode-test-summary overlay))))))))

(ert-deftest opencode-test-question-answers-in-summary ()
  "A completed question block summarizes each question with its answers."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "question"))
      (let ((summaries (mapcar (lambda (overlay)
                                 (buffer-substring-no-properties (overlay-start overlay)
                                                                 (1- (overlay-end overlay))))
                               (seq-filter (lambda (overlay)
                                             (equal (plist-get (opencode-block-data overlay) :tool) "question"))
                                           (opencode-tool-blocks)))))
        (should (seq-some (lambda (summary) (string-match-p "Color → blue; Fruits → apple, plum" summary))
                          summaries))))))

(ert-deftest opencode-test-live-matches-replay-everywhere ()
  "Every recorded scenario renders the same live as when replayed."
  (dolist (scenario '("read-tools" "permission" "question" "subagent" "interrupt"
                      "switch" "steer" "shell-input"))
    (opencode-test-with-server
      (let* ((id (opencode-test-scenario scenario))
             (replayed (opencode-test-text (opencode-test-open id))))
        (kill-buffer (gethash id opencode-session-buffers))
        (let ((live (opencode-test-open-empty id)))
          (opencode-test-dispatch (opencode-test-events id))
          (should (equal (cons scenario (opencode-test-text live)) (cons scenario replayed))))))))

;;;; Sending while a request waits

(defvar opencode-test-choices nil
  "Keys `read-multiple-choice' answers under `opencode-test-choosing', in order.")

(defvar opencode-test-asked nil
  "Prompts `read-multiple-choice' showed under `opencode-test-choosing', newest first.")

(defvar opencode-test-answered nil
  "Answering commands called under `opencode-test-choosing', newest first.")

(defmacro opencode-test-choosing (choices &rest body)
  "Run BODY with `read-multiple-choice' answering CHOICES, keys in order.
The prompts it shows go to `opencode-test-asked'; asking more often
than CHOICES fails, and so do the answering commands, which are recorded
in `opencode-test-answered' instead."
  (declare (indent 1) (debug t))
  `(let ((opencode-test-choices ,choices)
         (opencode-test-asked nil)
         (opencode-test-answered nil))
     (cl-letf (((symbol-function 'read-multiple-choice)
                (lambda (prompt choices &rest _)
                  (push prompt opencode-test-asked)
                  (or (assq (or (pop opencode-test-choices) (ert-fail "Asked once too often")) choices)
                      (ert-fail "No such choice"))))
               ((symbol-function 'opencode-respond)
                (lambda () (interactive) (push (list 'respond (current-buffer)) opencode-test-answered)))
               ((symbol-function 'opencode-respond-session)
                (lambda (id &rest _) (push (list 'respond-session id) opencode-test-answered))))
       ,@body)))

(defun opencode-test-type (text)
  "Type TEXT at the end of the input."
  (goto-char (point-max))
  (insert text))

(defun opencode-test-typed ()
  "Return the input typed at the current buffer's prompt."
  (buffer-substring-no-properties (opencode-pmark) (point-max)))

(defun opencode-test-sends ()
  "Return the bodies of the prompts, commands and shell commands requested, oldest first."
  (nreverse (delq nil (mapcar (lambda (request)
                                (when (string-match "/\\(prompt\\|command\\|shell\\)\\'" (plist-get request :path))
                                  (cons (intern (match-string 1 (plist-get request :path)))
                                        (plist-get request :body))))
                              opencode-test-requests))))

(defun opencode-test-open-waiting (scenario event)
  "Open SCENARIO's session as it streamed, up to its first EVENT; return its buffer."
  (let* ((id (opencode-test-scenario scenario))
         (buffer (opencode-test-open-empty id)))
    (opencode-test-dispatch-until (opencode-test-events id) event)
    (with-current-buffer buffer
      (should (eq opencode-session-status 'busy))
      (setq opencode-test-requests nil))
    buffer))

(ert-deftest opencode-test-send-while-question-asks ()
  "Sending while a question waits asks first: answering or cancelling keeps the input."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-waiting "question" "form.created")
      (should (opencode-open-forms))
      (opencode-test-type "use red instead")
      (opencode-test-choosing '(?c ?a ?s)
        (opencode-send-input)
        (should (equal (car opencode-test-asked)
                       "A question is pending; the agent reads a prompt only once it is answered or dismissed."))
        (should-not (opencode-test-sends))
        (should (equal (opencode-test-typed) "use red instead"))
        ;; Answering opens the question, keeping the input, and says nothing more.
        (let ((messages nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format &rest arguments)
                       (when format (push (apply #'format-message format arguments) messages)))))
            (opencode-send-input))
          (should-not (member "Not sent" messages)))
        (should (equal opencode-test-answered `((respond ,(current-buffer)))))
        (should-not (opencode-test-sends))
        (should (equal (opencode-test-typed) "use red instead"))
        ;; Sent anyway, it steers as usual.
        (opencode-send-input)
        (should (equal (length opencode-test-asked) 3))
        (pcase-let ((`((prompt . ,body)) (opencode-test-sends)))
          (should (equal (alist-get 'text body) "use red instead"))
          (should (equal (alist-get 'delivery body) "steer")))
        (should (equal (opencode-test-typed) ""))))))

(ert-deftest opencode-test-empty-input-sends-nothing ()
  "RET on an empty or blank input starts no turn."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-waiting "question" "form.created")
      (dolist (input '("" "  \n "))
        (opencode-test-type input)
        (opencode-test-choosing nil
          (opencode-send-input))
        (should-not (opencode-test-sends))
        (should (equal (opencode-test-typed) input))
        (delete-region (opencode-pmark) (point-max))))))

(ert-deftest opencode-test-send-while-permission-asks ()
  "A command sent while a permission request waits asks too, naming the request."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-waiting "permission" "permission.asked")
      (opencode-test-type "/review HEAD")
      (opencode-test-choosing '(?s)
        (opencode-send-input)
        (should (string-prefix-p "A permission request is pending;" (car opencode-test-asked)))
        (should (equal (mapcar #'car (opencode-test-sends)) '(command)))))))

(ert-deftest opencode-test-send-while-waiting-without-asking ()
  "Queued input, shell commands and sends without a check go out unasked, to wait."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-waiting "question" "form.created")
      (opencode-test-choosing nil
        (opencode-test-type "after this turn")
        (opencode-queue-input)
        (opencode-test-type "!ls")
        (opencode-send-input)
        ;; Scheduled sends call the function directly.
        (opencode-session-send "scheduled" :show t)
        (let ((opencode-prompt-delivery 'queue))
          (opencode-test-type "queued by default")
          (opencode-send-input))
        (should-not opencode-test-asked)
        (should (equal (mapcar (lambda (send) (list (car send) (or (alist-get 'delivery (cdr send))
                                                                   (alist-get 'command (cdr send)))))
                               (opencode-test-sends))
                       '((prompt "queue") (shell "ls") (prompt "steer") (prompt "queue"))))))))

(ert-deftest opencode-test-send-without-request-unasked ()
  "With nothing waiting, sending never asks."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (opencode-test-choosing nil
        (opencode-test-type "hello")
        (opencode-send-input)
        (should-not opencode-test-asked)
        (should (equal (mapcar #'car (opencode-test-sends)) '(prompt)))))))

(ert-deftest opencode-test-send-while-subagent-asks ()
  "A subagent's request makes its parent ask, and answering goes through the parent."
  (opencode-test-with-server
    (let* ((parent (opencode-test-scenario "subagent"))
           (child (opencode-test-scenario "subagent-child"))
           (buffer (opencode-test-open-empty parent)))
      (opencode-test-dispatch-until
       (seq-filter (lambda (event)
                     (member (or (alist-get 'sessionID (alist-get 'data event))
                                 (alist-get 'sessionID (alist-get 'form (alist-get 'data event))))
                             (list parent child)))
                   (opencode-test-events))
       "permission.asked")
      (should (buffer-live-p (gethash child opencode-session-buffers)))
      (opencode--flush-sync)
      (with-current-buffer buffer
        (should (equal opencode--pending-children (list child)))
        (setq opencode-test-requests nil)
        (opencode-test-type "never mind")
        (opencode-test-choosing '(?a)
          (opencode-send-input)
          (should (string-prefix-p "A subagent's permission request is pending;" (car opencode-test-asked)))
          (should (equal opencode-test-answered `((respond ,buffer))))
          (should-not (opencode-test-sends)))))))

(ert-deftest opencode-test-send-while-registry-knows-request ()
  "Requests the registry knows of subagents without a buffer count too, and are answered there."
  (opencode-test-with-server
    (let* ((parent (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty parent))
           (opencode-state--ready t)
           (opencode-state--stream opencode-events--process))
      (puthash parent (list :id parent :running t) opencode-state--sessions)
      (puthash "ses_test_child" (list :id "ses_test_child" :parent-id parent :running t
                                      :requests '((:kind question :id "frm_child" :time 1.0)))
               opencode-state--sessions)
      (puthash "ses_test_grandchild" (list :id "ses_test_grandchild" :parent-id "ses_test_child" :running t
                                           :requests '((:kind permission :id "per_grandchild" :time 2.0)))
               opencode-state--sessions)
      (puthash "ses_test_other" (list :id "ses_test_other" :running t
                                      :requests '((:kind question :id "frm_other" :time 3.0)))
               opencode-state--sessions)
      (with-current-buffer buffer
        (should (equal (mapcar (lambda (request) (plist-get request :id)) (opencode-awaited-requests))
                       '("frm_child" "per_grandchild")))
        (opencode-test-type "go on")
        (opencode-test-choosing '(?a)
          (opencode-send-input)
          (should (string-prefix-p "2 requests are pending;" (car opencode-test-asked)))
          (should (equal opencode-test-answered '((respond-session "ses_test_child"))))
          (should-not (opencode-test-sends))
          (should (equal (opencode-test-typed) "go on")))
        ;; A registry that no longer follows the service is not trusted.
        (let ((opencode-state--ready nil))
          (should-not (opencode-awaited-requests)))))))

;;;; Key hints

(defun opencode-test-drover-keys (command)
  "Return keys a package binding COMMAND in its own maps would report, or nil."
  (alist-get command '((opencode-respond . "C-' A") (opencode-revert-clear . "C-' R")
                       (opencode-interrupt . "C-' k"))))

(defmacro opencode-test-capturing-messages (&rest body)
  "Run BODY and return the messages it showed, oldest first."
  (declare (indent 0) (debug t))
  `(let ((messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest arguments)
                  (when format (push (apply #'format-message format arguments) messages)))))
       ,@body)
     (nreverse messages)))

(ert-deftest opencode-test-command-keys ()
  "Hints name the keys the function reports, else those of the keymaps, else M-x."
  (with-temp-buffer
    (let ((opencode-command-keys-function nil))
      (should (equal (opencode-command-keys 'opencode-respond) "M-x opencode-respond"))
      (should (equal (opencode-command-keys 'opencode-desktop-retry 'opencode-desktop-pending-mode-map) "g")))
    (let* ((asked nil)
           (opencode-command-keys-function
            (lambda (command) (push (cons command (current-buffer)) asked) (opencode-test-drover-keys command))))
      (let ((keys (opencode-command-keys 'opencode-respond)))
        (should (equal keys "C-' A"))
        (should (eq (get-text-property 0 'face keys) 'help-key-binding)))
      (should (equal asked `((opencode-respond . ,(current-buffer)))))
      ;; A command it knows no keys for falls back on the keymaps.
      (should (equal (opencode-command-keys 'opencode-desktop-retry 'opencode-desktop-pending-mode-map) "g"))
      (should (equal (opencode-command-keys 'opencode-select-model) "M-x opencode-select-model")))
    (let ((opencode-command-keys-function (lambda (_) "")))
      (should (equal (opencode-command-keys 'opencode-respond) "M-x opencode-respond")))
    (let ((opencode-command-keys-function (lambda (_) (propertize "C-' A" 'face 'bold))))
      (should (eq (get-text-property 0 'face (opencode-command-keys 'opencode-respond)) 'bold)))))

(ert-deftest opencode-test-question-hints-name-keys ()
  "Question hints name the keys of `opencode-command-keys-function'."
  (opencode-test-with-server
    (let* ((opencode-command-keys-function #'opencode-test-drover-keys)
           (id (opencode-test-scenario "question"))
           (buffer (opencode-test-open-empty id))
           (messages (cl-letf (((symbol-function 'opencode-buffer-active-p) (lambda (_) t)))
                       (opencode-test-capturing-messages
                         (opencode-test-dispatch-until (opencode-test-events id) "form.created")))))
      (should (member "OpenCode question pending; C-' A answers it" messages))
      (with-current-buffer buffer
        (let* ((entry (car opencode-pending-forms))
               (overlay (opencode--form-block entry)))
          (should (equal (overlay-get overlay 'help-echo) "Question pending.  C-' A answers it."))
          (should (equal (opencode-test-capturing-messages
                           (let ((opencode-question--handled nil))
                             (opencode--question-on-exit)))
                         '("OpenCode question deferred; C-' A resumes it")))
          (let ((opencode-test-routes (cons (list 'post "/reply\\'" (lambda (_) '(:error 500 "busy")))
                                            opencode-test-routes)))
            (should (seq-some (lambda (message) (string-suffix-p "; C-' A retries" message))
                              (opencode-test-capturing-messages (opencode-form-send entry))))))))))

(ert-deftest opencode-test-permission-hints-name-keys ()
  "Permission hints name the keys of `opencode-command-keys-function'."
  (opencode-test-with-server
    (let* ((opencode-command-keys-function #'opencode-test-drover-keys)
           (id (opencode-test-scenario "permission"))
           (buffer (opencode-test-open-empty id))
           (messages (cl-letf (((symbol-function 'opencode-buffer-active-p) (lambda (_) t)))
                       (opencode-test-capturing-messages
                         (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")))))
      (should (member "OpenCode permission pending; C-' A answers it" messages))
      (with-current-buffer buffer
        (let ((permission (car opencode-pending-permissions)))
          (should (equal (overlay-get (opencode--permission-block permission) 'help-echo)
                         "Permission pending: Shell command: rm junk.txt.  C-' A responds."))
          (should (equal (opencode-test-capturing-messages
                           (let ((opencode--active-permission (list :buffer buffer :id (plist-get permission :id))))
                             (opencode--permission-on-exit)))
                         '("OpenCode permission deferred; C-' A resumes it")))
          (let ((opencode-test-routes (cons (list 'post "/reply\\'" (lambda (_) '(:error 500 "busy")))
                                            opencode-test-routes)))
            (should (seq-some (lambda (message) (string-suffix-p "; C-' A retries" message))
                              (opencode-test-capturing-messages
                                (opencode-permission-reply permission 'once))))))))))

(ert-deftest opencode-test-session-hints-name-keys ()
  "The staged revert notice and Comint refusals name the keys of `opencode-command-keys-function'."
  (opencode-test-with-server
    (let ((opencode-command-keys-function #'opencode-test-drover-keys)
          (id (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open-empty id)
        (opencode-test-dispatch `(((type . "session.revert.staged")
                                   (data (sessionID . ,id) (revert (messageID . "msg_cut"))))))
        (should (string-search "A revert is staged: the next prompt drops everything after it, C-' R cancels it"
                               (buffer-string)))
        (should (equal (opencode--comint-refusal 'comint-quit-subjob)
                       "An OpenCode session takes no quit signal: C-' k stops the turn"))
        ;; What the function does not know comes from the keymaps.
        (should (equal (substring-no-properties (opencode--comint-refusal 'comint-clear-buffer))
                       (format "The transcript is the server's and cannot be cleared: %s renders it again"
                               (substitute-command-keys "\\[revert-buffer]"))))))))

(provide 'opencode-requests-test)
;;; opencode-requests-test.el ends here
