;;; opencode-requests-test.el --- Tests of permissions and questions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(ert-deftest opencode-test-permission-marks-its-tool ()
  "A permission marks its tool with an action; expansion holds the asking rule."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "permission"))
           (buffer (opencode-test-open-empty id))
           (rest (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")))
      (with-current-buffer buffer
        (should (= (length opencode-pending-permissions) 1))
        (let* ((permission (car opencode-pending-permissions))
               (overlay (opencode--permission-block permission)))
          (should (equal (plist-get permission :title) "Run rm junk.txt"))
          (should overlay)
          (should (eq (plist-get (opencode-block-data overlay) :waiting-kind) 'permission))
          (should (string-match-p "Run rm junk.txt"
                                  (buffer-substring (overlay-start overlay) (overlay-end overlay))))
          (should-not (string-search "rule" (overlay-get overlay 'opencode-summary)))
          (opencode-expand-block overlay)
          (should (string-search "Approval required\n  shell rule \"rm *\"" (opencode-test-text buffer)))
          (should (opencode-pending-request-p))
          ;; Answered in another client.
          (opencode-test-dispatch-until rest "permission.replied")
          (should-not opencode-pending-permissions)
          (should-not (plist-get (opencode-block-data overlay) :waiting)))))))

(ert-deftest opencode-test-permission-summary-shows-action ()
  "A block waiting on a permission uses the readable action as its summary."
  (should (string-match-p "Send TERM to node (222209)"
                          (opencode-tool-collapsed-summary
                           '(:type tool :tool "shell" :input ((command . "kill -TERM 222209"))
                                   :waiting t :waiting-kind permission
                                   :waiting-summary "Send TERM to node (222209)")))))

(ert-deftest opencode-test-permission-process-details ()
  "Names come from the service; the expansion has context and the form only its command."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty id))
           (command "kill -TERM 222209 221757")
           (request `((id . "per_processes") (sessionID . ,id) (action . "shell")
                      (resources ,command) (save "kill *")
                      (source (type . "tool") (messageID . "msg_processes") (id . "call_processes"))
                      (reasons ((resource . ,command) (rule (action . "shell") (resource . "kill *") (effect . "ask"))))
                      (metadata (command . ,command) (cwd . ,opencode-test-workspace) (shell . "bash") (timeout . 0)
                                (signals ((command . ,command) (signal . "TERM")
                                          (targets ((target . "222209") (kind . "pid") (name . "node")
                                                    (command . "node server.js --port 3000"))
                                                   ((target . "221757") (kind . "pid")))))))))
      (with-current-buffer buffer
        (opencode-tool-start "call_processes" "shell" '((command . "kill 222209") (timeout . 1000) (background . t)) "running")
        (cl-letf (((symbol-function 'process-attributes) (lambda (&rest _) (ert-fail "Looked up a local PID"))))
          (opencode-permission-queue request))
        (let* ((permission (car opencode-pending-permissions))
               (block (opencode--permission-block permission))
               (opencode--permission-transient-state (list :buffer buffer :permission permission)))
          (should (equal (plist-get permission :title) "Send TERM to node (222209), PID 221757"))
          (should (equal (opencode--permission-description) command))
          (opencode-expand-block block)
          (let ((text (opencode--expanded-content block)))
            (should (string-prefix-p (concat "$ " command) text))
            (should (string-search "node server.js --port 3000" text))
            (should (string-match-p "221757 +unavailable" text))
            (should (string-search "Directory  " text))
            (should (string-search "Execution  Background" text))
            (should (string-search "Shell      bash" text))
            (should (string-search "Timeout    Unlimited" text))
            (should (string-suffix-p "Approval required\n  shell rule \"kill *\"" text))
            (should (= 2 (length (split-string text (regexp-quote command))))))
          (opencode-permission-reply permission 'once)
          (should-not (plist-get (opencode-block-data block) :waiting-summary))
          (should-not (plist-get (opencode-block-data block) :permission-display)))))))

(ert-deftest opencode-test-permission-before-tool-keeps-full-command ()
  "A request received before its tool gets a compact prompt and later a complete expansion."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (let* ((command "kill -0 $$\nprintf '%s' '**literal**'")
             (request `((id . "per_late_process") (action . "shell") (resources "kill -0 $$")
                        (source (type . "tool") (id . "call_late_process")))))
        (opencode-permission-queue request)
        (opencode-tool-start "call_late_process" "shell" `((command . ,command)) "running")
        (let* ((permission (car opencode-pending-permissions))
               (opencode--permission-transient-state (list :permission permission)))
          (should (equal (opencode--permission-description) "kill -0 $$ …"))
          (should (string-search command (opencode--expanded-content (opencode-block "call_late_process")))))))))

(ert-deftest opencode-test-permission-signal-target-kinds ()
  "Signal zero and non-PID targets have accurate action labels."
  (dolist (entry '(("shell" "$$" "Check the current shell ($$)")
                   ("group" "-22" "Check process group 22 (-22)")
                   ("group" "0" "Check the current process group (0)")
                   ("all" "-1" "Check all permitted processes (-1)")
                   ("job" "%1" "Check shell job %1")
                   ("dynamic" "$PIDS" "Check target $PIDS (resolved at execution)")))
    (pcase-let* ((`(,kind ,target ,expected) entry)
                 (command (concat "kill -0 -- " target)))
      (should (equal (opencode-permission-title
                      "shell" (list command)
                      `((command . ,command)
                        (signals ((command . ,command) (signal . "0")
                                  (targets ((kind . ,kind) (target . ,target)))))) nil)
                     expected)))))

(ert-deftest opencode-test-permission-remote-context ()
  "A remote request displays the execution directory as that host spells it."
  (cl-letf (((symbol-function 'opencode-service-files-p) (lambda () nil))
            ((symbol-function 'opencode-service-label) (lambda () "remote-host")))
    (let ((text (opencode--permission-display
                 '(:action "shell" :metadata ((cwd . "/srv/project") (command . "pwd")))
                 "shell" '((command . "pwd")))))
      (should (string-search "Directory  /srv/project\nHost       remote-host" text)))))

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
                   (concat "\n\nApproval required"
                           "\n  shell rule \"*kanopy-oidc*\":"
                           "\n    python3 - <<'EOF' …"
                           "\n      line 3: mode = 'kanopy-oidc'"
                           "\n  shell rule \"\"$*\":"
                           "\n    \"$W\" status 2>&1"
                           "\n  no rule allows it:"
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
                   (concat "\n\nApproval required"
                           "\n  shell rule \"git push *\":"
                           "\n    /usr/bin/git push origin"
                           "\n      as: git push origin"
                           "\n  shell rule \"*kanopy-oidc*\":"
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

;;;; Answering in the transients

(defvar opencode-test-transient-buffer nil
  "Session buffer whose request `opencode-test-outer-respond' answers.")

(transient-define-suffix opencode-test-outer-respond ()
  "Answer the request of `opencode-test-transient-buffer', as drover's menu does."
  (interactive)
  (with-current-buffer opencode-test-transient-buffer
    (opencode-respond)))

(transient-define-prefix opencode-test-outer-transient ()
  "Stand for another package's menu answering requests, such as `drover-dispatch'."
  [("A" "Answer" opencode-test-outer-respond)])

(defmacro opencode-test-with-transients (buffer &rest body)
  "Run BODY with session BUFFER selected, as when answering its requests by keys.
Transients show their menu, when they do, in a window of their own
below, and every one still active afterwards is left."
  (declare (indent 1) (debug t))
  `(let ((opencode-test-transient-buffer ,buffer)
         (opencode--active-form nil)
         (opencode--active-permission nil)
         (opencode--permission-transient-state nil))
     (save-window-excursion
       (delete-other-windows)
       (set-window-buffer (selected-window) opencode-test-transient-buffer)
       (cl-letf (((symbol-function 'display-buffer)
                  (lambda (buffer &rest _)
                    (unless (equal (buffer-name (get-buffer buffer)) transient--buffer-name)
                      (ert-fail "Unexpected display"))
                    (let ((window (split-window (frame-root-window) nil 'below)))
                      (set-window-buffer window buffer)
                      window))))
         (unwind-protect (progn ,@body)
           (transient--emergency-exit)
           (remove-hook 'transient-exit-hook #'opencode--question-on-exit)
           (remove-hook 'transient-exit-hook #'opencode--permission-on-exit))))))

(defun opencode-test-keys (keys)
  "Type KEYS, then run the timers showing the next question, as the command loop would."
  (execute-kbd-macro (kbd keys))
  (let ((deadline (+ (float-time) 5)))
    (while (and (seq-find (lambda (timer) (eq (timer--function timer) #'opencode--question-show)) timer-list)
                (< (float-time) deadline))
      (accept-process-output nil 0.01))))

(defun opencode-test-five-questions (session)
  "Return a question form of SESSION asking five questions, the third of several answers.
Their options, three or four, have descriptions."
  `((id . "frm_five") (sessionID . ,session) (title . "Questions")
    (metadata (kind . "question"))
    (fields ,@(cl-loop for index below 5
                       collect `((key . ,(format "q%d" index)) (title . ,(format "Topic %d" index))
                                 (description . ,(format "Question %d?" index))
                                 (type . ,(if (= index 2) "multiselect" "string"))
                                 (custom . t)
                                 (options ,@(cl-loop for option below (if (cl-oddp index) 4 3)
                                                     collect `((value . ,(format "v%d%d" index option))
                                                               (label . ,(format "Option %d.%d" index option))
                                                               (description . ,(format "Means %d.%d" index option))))))))))

(defun opencode-test-answer-five (between &optional direct)
  "Answer five questions opened from another transient, calling BETWEEN after each.
BETWEEN receives the session buffer and the index of the question just
answered.  DIRECT opens them with `opencode-respond' instead.  Return
the messages shown."
  (let* ((id (opencode-test-scenario "question"))
         (buffer (opencode-test-open-empty id))
         (form (opencode-test-five-questions id))
         (messages nil))
    (setq opencode-test-routes
          (cons (list 'get "/form\\'" (lambda (_) `((data ,form)))) opencode-test-routes))
    (with-current-buffer buffer (opencode-form-queue form))
    (opencode-test-with-transients buffer
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest arguments)
                   (when format (push (apply #'format-message format arguments) messages)))))
        (if direct
            (with-current-buffer buffer (opencode-respond))
          (call-interactively #'opencode-test-outer-transient)
          (opencode-test-keys "A"))
        (should (opencode-transient-displayed-p 'opencode-question-transient))
        (should (equal (plist-get (opencode--active-entry) :index) 0))
        (opencode-test-keys "2")
        (funcall between buffer 0)
        (should (equal opencode-question--heading
                       (concat "Question 2/5 · Topic 1" (propertize "   M-p previous · M-n next" 'face 'shadow)
                               "\nQuestion 1?")))
        (opencode-test-keys "4")
        (funcall between buffer 1)
        (should opencode-question--multiple)
        (opencode-test-keys "1 3 C-c C-c")
        (funcall between buffer 2)
        (opencode-test-keys "! m i n e RET")
        (funcall between buffer 3)
        ;; Back to the typed answer, kept, and on.
        (opencode-test-keys "M-p")
        (should (equal opencode-question--custom-text "mine"))
        (opencode-test-keys "M-n")
        (opencode-test-keys "3")))
    (with-current-buffer buffer
      (should (equal (car (opencode-test-requests-to 'post (format "/form/%s/reply" "frm_five")))
                     '((answer (q0 . "v01") (q1 . "v13") (q2 . ["v20" "v22"]) (q3 . "mine") (q4 . "v42")))))
      (should-not opencode-pending-forms))
    (should-not opencode--active-form)
    messages))

(ert-deftest opencode-test-question-from-another-transient ()
  "A form opened from another transient's suffix, as drover's menu does, is answered to the end.
That transient's exit, which follows the question transient's setup,
neither defers the question nor leaves it without a form.  Opened
directly, it is answered alike."
  (dolist (direct '(nil t))
    (opencode-test-with-server
      (let ((messages (opencode-test-answer-five #'ignore direct)))
        (should-not (seq-find (lambda (message) (string-prefix-p "OpenCode question deferred" message))
                              messages))))))

(ert-deftest opencode-test-question-survives-events ()
  "Between questions, the form asked again, synchronized, reloaded and reconnected stays answerable."
  (opencode-test-with-server
    (let ((opencode--stream-seen t))
      (opencode-test-dispatch '(((type . "server.connected") (data))))
      (opencode-test-answer-five
       (lambda (buffer index)
         (pcase index
           (0 (opencode-test-dispatch
               `(((type . "form.created")
                  (data (form . ,(opencode-test-five-questions (buffer-local-value 'opencode-session-id buffer))))))))
           (1 (with-current-buffer buffer (opencode-form-sync)))
           (2 (with-current-buffer buffer (opencode-session-reload)))
           (3 (opencode-test-dispatch '(((type . "server.connected") (data))))))
         (with-current-buffer buffer
           (should (= (length opencode-pending-forms) 1))
           (should (eq (car opencode-pending-forms) (opencode--active-entry)))
           (should (= (plist-get (opencode--active-entry) :index) (1+ index)))))))))

(ert-deftest opencode-test-question-resolved-while-typing ()
  "A form answered elsewhere while a custom answer is typed is let go, without a reply or an error."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "question"))
           (buffer (opencode-test-open-empty id))
           (messages nil))
      (with-current-buffer buffer (opencode-form-queue (opencode-test-five-questions id)))
      (opencode-test-with-transients buffer
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _)
                     (opencode-test-dispatch
                      `(((type . "form.replied") (data (sessionID . ,id) (id . "frm_five")))))
                     "late"))
                  ((symbol-function 'message)
                   (lambda (format &rest arguments)
                     (when format (push (apply #'format-message format arguments) messages)))))
          (with-current-buffer buffer (opencode-respond))
          (opencode-test-keys "!")))
      (should (member "The OpenCode question is no longer pending" messages))
      (should-not (opencode-test-requests-to 'post "/reply"))
      (should-not opencode--active-form)
      (with-current-buffer buffer (should-not opencode-pending-forms)))))

(ert-deftest opencode-test-permission-from-another-transient ()
  "A permission opened from another transient's suffix stays the one shown, closed once answered elsewhere."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "permission"))
           (buffer (opencode-test-open-empty id))
           (rest (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")))
      (opencode-test-with-transients buffer
        (call-interactively #'opencode-test-outer-transient)
        (opencode-test-keys "A")
        (should (opencode-transient-displayed-p 'opencode-permission-transient))
        (should (equal (plist-get opencode--active-permission :id)
                       (plist-get (car (buffer-local-value 'opencode-pending-permissions buffer)) :id)))
        (opencode-test-dispatch-until rest "permission.replied")
        (should-not (opencode-transient-displayed-p 'opencode-permission-transient))
        (should-not opencode--active-permission)))))
;;;; Answering in place

(defvar opencode-test-in-place-session nil
  "Session whose request `opencode-test-list-respond' answers in place.")

(transient-define-suffix opencode-test-list-respond ()
  "Answer the request of `opencode-test-in-place-session' in place, as drover's list may."
  (interactive)
  (opencode-respond-session opencode-test-in-place-session nil t))

(transient-define-prefix opencode-test-list-transient ()
  "Stand for drover's menu over its session list."
  [("A" "Answer" opencode-test-list-respond)])

(defmacro opencode-test-in-place (session &rest body)
  "Run BODY answering SESSION's requests in place from a list in the only window.
The list's buffer stands for drover's; showing any buffer but a
transient's menu fails, and so does showing a session buffer otherwise."
  (declare (indent 1) (debug t))
  `(let ((opencode-test-in-place-session ,session)
         (list (get-buffer-create " *opencode-test-list*")))
     (unwind-protect
         (opencode-test-with-transients list
           ,@body
           (should (eq (window-buffer (frame-first-window)) list))
           (should-not (seq-some (lambda (buffer) (get-buffer-window buffer t))
                                 (hash-table-values opencode-session-buffers))))
       (kill-buffer list))))

(defun opencode-test-heading-line (heading)
  "Return the first line of transient HEADING."
  (car (split-string heading "\n")))

(ert-deftest opencode-test-permission-in-place ()
  "A permission answered from a list has only its command above the controls."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "permission"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")
      (let ((permission (car (buffer-local-value 'opencode-pending-permissions buffer))))
        (opencode-test-in-place id
          ;; Menus delayed or off still show: the menu holds the request.
          (cl-progv (list opencode--transient-menu-variable) (list nil)
            (call-interactively #'opencode-test-list-transient)
            (opencode-test-keys "A"))
          (should (opencode-transient-displayed-p 'opencode-permission-transient))
          (should (window-live-p transient--window))
          (should (equal (opencode--permission-description) "rm junk.txt"))
          (opencode-test-keys "o"))
        (should (equal (car (opencode-test-requests-to
                             'post (format "/api/session/%s/permission/%s/reply" id (plist-get permission :id))))
                       '((decision . "once"))))
        (with-current-buffer buffer
          (should-not opencode-pending-permissions)
          (should-not (plist-get (opencode-block-data (opencode--permission-block permission)) :waiting)))
        (should-not opencode--active-permission)))))

(ert-deftest opencode-test-permission-in-place-scrolls-menu ()
  "Answered in place, the transcript keys scroll the menu, not the list they were called from."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "permission"))
           (buffer (opencode-test-open-empty id))
           (scrolled nil))
      (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")
      (opencode-test-in-place id
        (with-current-buffer buffer (opencode-respond-session id nil t))
        (cl-letf (((symbol-function 'scroll-up-command)
                   (lambda (&rest _) (push (selected-window) scrolled))))
          (opencode-test-keys "C-v"))
        (should (equal scrolled (list transient--window)))
        (should (opencode-transient-displayed-p 'opencode-permission-transient)))
      ;; Answered in its buffer, they scroll its transcript.
      (setq scrolled nil)
      (opencode-test-with-transients buffer
        (with-current-buffer buffer (opencode-respond))
        (cl-letf (((symbol-function 'scroll-up-command)
                   (lambda (&rest _) (push (selected-window) scrolled))))
          (opencode-test-keys "C-v"))
        (should (equal scrolled (list (get-buffer-window buffer))))))))

(ert-deftest opencode-test-form-in-place ()
  "A form answered field by field in place names the session in its prompts."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "read-tools"))
           (buffer (opencode-test-open-empty id))
           (prompts nil))
      (with-current-buffer buffer (opencode-form-queue (opencode-test-tool-form "call_none")))
      (opencode-test-in-place id
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt &rest _) (push prompt prompts) "Allow search via Exa")))
          (opencode-respond-session id nil t)))
      (should (equal prompts (list (concat (opencode-test-session-title id)
                                           " · Allow OpenCode to search the web for up-to-date information?: "))))
      (should (equal (car (opencode-test-requests-to 'post "/form/frm_websearch/reply"))
                     '((answer (choice . "Allow search via Exa"))))))))

(defun opencode-test-edit-permission (session message call)
  "Dispatch an edit CALL of assistant MESSAGE in SESSION asking a permission; return the request's ID."
  (let ((base `((sessionID . ,session) (assistantMessageID . ,message) (id . ,call))))
    (opencode-test-dispatch
     `(((type . "session.step.started") (data (sessionID . ,session) (assistantMessageID . ,message)))
       ((type . "session.tool.input.started") (data ,@base (name . "edit")))
       ((type . "session.tool.called")
        (data ,@base (input (path . "notes.txt") (oldString . "alpha\nbeta\ngamma")
                            (newString . "alpha\nBETA\ngamma\ndelta"))))
       ((type . "permission.asked")
        (data (id . ,(concat "per_" call)) (sessionID . ,session) (action . "edit")
              (resources "notes.txt") (save "*")
              (source (type . "tool") (messageID . ,message) (id . ,call))))))
    (concat "per_" call)))

(ert-deftest opencode-test-permission-change-stays-in-expansion ()
  "File changes stay in the expansion, whether the compact form is opened here or from a list."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-edit-permission id "msg_test_edit" "call_test_edit")
      (opencode-test-in-place id
        (opencode-respond-session id nil t)
        (should (equal (opencode--permission-description) "Edit notes.txt"))
        (with-current-buffer buffer
          (let ((block (opencode-block "call_test_edit")))
            (opencode-expand-block block)
            (should (string-search "-beta" (opencode-test-text buffer)))
            (should (string-search "+BETA" (opencode-test-text buffer)))
            (should (string-search "+delta" (opencode-test-text buffer)))))
        (opencode-test-keys "n"))
      (should (equal (car (opencode-test-requests-to 'post "/permission/per_call_test_edit/reply"))
                     '((decision . "reject"))))
      (with-current-buffer buffer (should-not opencode-pending-permissions))
      ;; Answered in its buffer, the transcript shows the change instead.
      (opencode-test-edit-permission id "msg_test_edit_3" "call_test_edit_3")
      (opencode-test-with-transients buffer
        (with-current-buffer buffer (opencode-respond))
        (should (equal (opencode--permission-description) "Edit notes.txt"))))))

(ert-deftest opencode-test-question-in-place ()
  "Questions are answered from a list to the last one, each naming the session, its buffer shown nowhere."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "question"))
           (buffer (opencode-test-open-empty id))
           (title (opencode-test-session-title id)))
      (with-current-buffer buffer (opencode-form-queue (opencode-test-five-questions id)))
      (opencode-test-in-place id
        (call-interactively #'opencode-test-list-transient)
        (opencode-test-keys "A")
        (should (opencode-transient-displayed-p 'opencode-question-transient))
        (dolist (keys '("2" "4" "1 3 C-c C-c" "! m i n e RET"))
          (should (string-prefix-p title (opencode-test-heading-line opencode-question--heading)))
          (opencode-test-keys keys))
        (should (string-suffix-p "Question 4?" opencode-question--heading))
        (opencode-test-keys "3"))
      (should (equal (car (opencode-test-requests-to 'post "/form/frm_five/reply"))
                     '((answer (q0 . "v01") (q1 . "v13") (q2 . ["v20" "v22"]) (q3 . "mine") (q4 . "v42")))))
      (with-current-buffer buffer (should-not opencode-pending-forms))
      (should-not opencode--active-form))))

(ert-deftest opencode-test-question-resumed-in-place ()
  "A question suspended in its buffer is resumed in place, then named by its session."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "question"))
           (buffer (opencode-test-open-empty id))
           (title (opencode-test-session-title id)))
      (with-current-buffer buffer (opencode-form-queue (opencode-test-five-questions id)))
      (opencode-test-with-transients buffer
        (with-current-buffer buffer (opencode-respond))
        (should-not (string-prefix-p title opencode-question--heading))
        (opencode-test-keys "C-z")
        (should (opencode-transient-suspended-p 'opencode-question-transient)))
      (opencode-test-in-place id
        (opencode-respond-session id nil t)
        (should (opencode-transient-displayed-p 'opencode-question-transient))
        (should (string-prefix-p title opencode-question--heading))
        (opencode-test-keys "1")
        (should (= (plist-get (opencode--active-entry) :index) 1))))))

(ert-deftest opencode-test-in-place-opens-in-background ()
  "A session without a buffer gets one in the background, loads its request, and is answered in place."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (permission `((id . "per_test_background") (sessionID . ,id) (action . "shell")
                         (resources "make check") (save "make *")))
           (opencode-test-routes
            (cons (list 'get "/permission\\'" (lambda (_) `((data ,permission)))) opencode-test-routes)))
      (opencode-test-in-place id
        (opencode-respond-session id nil t)
        (should (buffer-live-p (gethash id opencode-session-buffers)))
        (should (opencode-transient-displayed-p 'opencode-permission-transient))
        (should (equal "make check" (opencode--permission-description)))
        (opencode-test-keys "a"))
      (should (equal (car (opencode-test-requests-to 'post "/permission/per_test_background/reply"))
                     '((decision . "always"))))
      (with-current-buffer (gethash id opencode-session-buffers)
        (should-not opencode-pending-permissions)))))

(ert-deftest opencode-test-in-place-loads-request ()
  "A buffer not knowing the session's request yet loads it, then answers it in place."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (buffer (opencode-test-open-empty id))
           (permission `((id . "per_test_late") (sessionID . ,id) (action . "shell")
                         (resources "make check") (save "make *"))))
      (with-current-buffer buffer (should-not (opencode-pending-request-p)))
      (let ((opencode-test-routes
             (cons (list 'get "/permission\\'" (lambda (_) `((data ,permission)))) opencode-test-routes)))
        (opencode-test-in-place id
          (opencode-respond-session id nil t)
          (should (opencode-transient-displayed-p 'opencode-permission-transient))
          (opencode-test-keys "o")))
      (should (opencode-test-requests-to 'post "/permission/per_test_late/reply"))
      (with-current-buffer buffer (should-not opencode-pending-permissions)))))

(ert-deftest opencode-test-subagent-in-place ()
  "Answered in place, a parent waiting only on its subagent answers the subagent's request, showing neither."
  (opencode-test-with-server
    (let* ((parent (opencode-test-scenario "subagent"))
           (child (opencode-test-scenario "subagent-child")))
      (opencode-test-open-empty parent)
      (opencode-test-dispatch-until
       (seq-filter (lambda (event)
                     (member (or (alist-get 'sessionID (alist-get 'data event))
                                 (alist-get 'sessionID (alist-get 'form (alist-get 'data event))))
                             (list parent child)))
                   (opencode-test-events))
       "permission.asked")
      (opencode--flush-sync)
      (let* ((child-buffer (gethash child opencode-session-buffers))
             (permission (car (buffer-local-value 'opencode-pending-permissions child-buffer))))
        (opencode-test-in-place parent
          (opencode-respond-session parent nil t)
          (should (eq (plist-get opencode--permission-transient-state :buffer) child-buffer))
          (should (equal (opencode--permission-description) "rm junk3.txt"))
          (opencode-test-keys "o"))
        (should (opencode-test-requests-to
                 'post (format "/api/session/%s/permission/%s/reply" child (plist-get permission :id))))
        (with-current-buffer child-buffer (should-not opencode-pending-permissions))))))

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
                       "This session waits for your answer to a question; it reads the prompt only once the question is answered or dismissed."))
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
        (should (string-prefix-p "This session waits for your answer to a permission request;"
                                (car opencode-test-asked)))
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

(defun opencode-test-subagent-events (&optional background)
  "Return the events of the subagent scenario's parent and child, oldest first.
With BACKGROUND, the parent runs the subagent in the background: its
call says so and ends at once, the subagent still running, as the
service reports such a call."
  (let* ((parent (opencode-test-scenario "subagent"))
         (child (opencode-test-scenario "subagent-child"))
         (events (seq-filter (lambda (event)
                               (member (or (alist-get 'sessionID (alist-get 'data event))
                                           (alist-get 'sessionID (alist-get 'form (alist-get 'data event))))
                                       (list parent child)))
                             (opencode-test-events))))
    (if (not background)
        events
      (mapcan
       (lambda (event)
         (let ((type (alist-get 'type event))
               (data (alist-get 'data event)))
           (cl-flet ((with-data (data) (cons (cons 'data data) (assq-delete-all 'data (copy-alist event)))))
             (cond
              ((not (equal (alist-get 'sessionID data) parent)) (list event))
              ((and (equal type "session.tool.called") (alist-get 'agent (alist-get 'input data)))
               (list (with-data (cons (cons 'input (append (alist-get 'input data) '((background . t))))
                                      (assq-delete-all 'input (copy-alist data))))))
              ((and (equal type "session.tool.progress")
                    (equal (alist-get 'sessionID (alist-get 'metadata data)) child))
               (list event
                     `((type . "session.tool.success")
                       (data (sessionID . ,parent)
                             (assistantMessageID . ,(alist-get 'assistantMessageID data))
                             (id . ,(alist-get 'id data))
                             (content ((type . "text")
                                       (text . ,(format "The subagent is working in the background (sessionID: %s)."
                                                        child))))
                             (metadata (sessionID . ,child) (status . "running"))))))
              ;; Its foreground completion.
              ((and (equal type "session.tool.success")
                    (equal (alist-get 'sessionID (alist-get 'metadata data)) child))
               nil)
              (t (list event))))))
       events))))

(defun opencode-test-open-subagent-asking (&optional background)
  "Open the subagent scenario's parent and stream until its child asks permission.
BACKGROUND runs the child in the background, as for
`opencode-test-subagent-events'.  Return the parent's buffer."
  (let ((buffer (opencode-test-open-empty (opencode-test-scenario "subagent"))))
    (opencode-test-dispatch-until (opencode-test-subagent-events background) "permission.asked")
    (opencode--flush-sync)
    (should (buffer-live-p (gethash (opencode-test-scenario "subagent-child") opencode-session-buffers)))
    buffer))

(defun opencode-test-subagent-block (buffer)
  "Return the data of the block of the subagent call in BUFFER."
  (with-current-buffer buffer
    (opencode-block-data (seq-find (lambda (overlay)
                                     (equal (plist-get (opencode-block-data overlay) :tool) "subagent"))
                                   (opencode-tool-blocks)))))

(ert-deftest opencode-test-send-while-subagent-asks ()
  "A subagent run in the foreground and asking makes its parent ask, naming it, and is answered in its buffer."
  (opencode-test-with-server
    (let* ((child (opencode-test-scenario "subagent-child"))
           (buffer (opencode-test-open-subagent-asking)))
      (with-current-buffer buffer
        (should (equal (plist-get (opencode-test-subagent-block buffer) :status) "running"))
        (should (equal opencode--pending-children (list child)))
        (setq opencode-test-requests nil)
        (opencode-test-type "never mind")
        (opencode-test-choosing '(?a)
          (opencode-send-input)
          (should (equal (car opencode-test-asked)
                         (format-message "Its subagent `%s' waits for your answer to a permission request; this session reads the prompt only once the request is answered or dismissed."
                                         (opencode-test-session-title child))))
          (should (equal opencode-test-answered `((respond-session ,child))))
          (should-not (opencode-test-sends))
          (should (equal (opencode-test-typed) "never mind")))))))

(ert-deftest opencode-test-send-while-background-subagent-asks ()
  "A subagent run in the background holds up nothing: its parent sends at once, and still answers it."
  (opencode-test-with-server
    (let* ((child (opencode-test-scenario "subagent-child"))
           (buffer (opencode-test-open-subagent-asking t)))
      (with-current-buffer buffer
        (let ((block (opencode-test-subagent-block buffer)))
          (should (equal (plist-get block :status) "completed"))
          (should (equal (opencode-tool-background-note block) "in the background")))
        (should-not opencode--pending-children)
        (should-not (opencode-awaited-requests))
        (should (equal (mapcar (lambda (request) (plist-get request :session)) (opencode-tree-requests))
                       (list child)))
        (setq opencode-test-requests nil)
        (opencode-test-type "meanwhile, look at the docs")
        (opencode-test-choosing nil
          (opencode-send-input)
          (should-not opencode-test-asked)
          (pcase-let ((`((prompt . ,body)) (opencode-test-sends)))
            (should (equal (alist-get 'text body) "meanwhile, look at the docs"))
            (should (equal (alist-get 'delivery body) "steer"))))
        ;; Answering from the parent still reaches the subagent.
        (cl-letf (((symbol-function 'opencode-respond-session)
                   (lambda (id &rest arguments) (push (cons id arguments) opencode-test-answered))))
          (let ((opencode-test-answered nil))
            (opencode-respond)
            (should (equal opencode-test-answered `((,child))))))))))

(ert-deftest opencode-test-send-while-registry-knows-request ()
  "Requests the registry knows count when they hold up the session: its own, and its foreground subagents'."
  (opencode-test-with-server
    (let* ((parent (opencode-test-scenario "subagent"))
           (child (opencode-test-scenario "subagent-child"))
           (buffer (opencode-test-open-empty parent))
           (opencode-state--ready t)
           (opencode-state--stream opencode-events--stream))
      ;; The parent runs its child in the foreground; the child has no buffer.
      (opencode-test-dispatch-until (opencode-test-events parent) "session.tool.progress")
      (should-not (gethash child opencode-session-buffers))
      (puthash parent (list :id parent :running t) opencode-state--sessions)
      (puthash child (list :id child :parent-id parent :running t :title "Delete junk"
                           :requests '((:kind question :id "frm_child" :time 2.0)))
               opencode-state--sessions)
      ;; Below a subagent without a buffer, nothing tells how it runs its own.
      (puthash "ses_test_grandchild" (list :id "ses_test_grandchild" :parent-id child :running t
                                           :requests '((:kind permission :id "per_grandchild" :time 1.0)))
               opencode-state--sessions)
      ;; No call of the parent runs it: it runs in the background.
      (puthash "ses_test_background" (list :id "ses_test_background" :parent-id parent :running t
                                           :requests '((:kind question :id "frm_background" :time 3.0)))
               opencode-state--sessions)
      (puthash "ses_test_other" (list :id "ses_test_other" :running t
                                      :requests '((:kind question :id "frm_other" :time 0.5)))
               opencode-state--sessions)
      (with-current-buffer buffer
        (should (equal (mapcar (lambda (request) (plist-get request :id)) (opencode-tree-requests))
                       '("per_grandchild" "frm_child" "frm_background")))
        (should (equal (mapcar (lambda (request) (plist-get request :id)) (opencode-awaited-requests))
                       '("frm_child")))
        (opencode-test-type "go on")
        (opencode-test-choosing '(?a)
          (opencode-send-input)
          (should (equal (car opencode-test-asked)
                         (format-message "Its subagent `Delete junk' waits for your answer to a question; this session reads the prompt only once the question is answered or dismissed.")))
          (should (equal opencode-test-answered `((respond-session ,child))))
          (should-not (opencode-test-sends))
          (should (equal (opencode-test-typed) "go on")))
        ;; Its own request comes first, and both are told.
        (puthash parent (list :id parent :running t
                              :requests '((:kind permission :id "per_parent" :time 4.0)))
                 opencode-state--sessions)
        (should (equal (mapcar (lambda (request) (plist-get request :id)) (opencode-awaited-requests))
                       '("per_parent" "frm_child")))
        (should (equal (opencode--describe-awaited (opencode-awaited-requests))
                       "This session and its subagents wait for your answer to 2 requests; this session reads the prompt only once they are answered or dismissed."))
        (should (equal (opencode--describe-awaited (list (car (opencode-awaited-requests))))
                       "This session waits for your answer to a permission request; it reads the prompt only once the request is answered or dismissed."))
        ;; A registry that no longer follows the service is not trusted.
        (let ((opencode-state--ready nil))
          (should-not (opencode-awaited-requests))
          (should-not (opencode-tree-requests)))))))

;;;; Answering a subagent's request from its parent

(defmacro opencode-test-capturing-messages (&rest body)
  "Run BODY and return the messages it showed, oldest first."
  (declare (indent 0) (debug t))
  `(let ((messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest arguments)
                  (when format (push (apply #'format-message format arguments) messages)))))
       ,@body)
     (nreverse messages)))

(ert-deftest opencode-test-background-subagent-in-place ()
  "Answered in place, a parent answers the request of a subagent it runs in the background."
  (opencode-test-with-server
    (let* ((parent (opencode-test-scenario "subagent"))
           (child (opencode-test-scenario "subagent-child"))
           (buffer (opencode-test-open-subagent-asking t))
           (child-buffer (gethash child opencode-session-buffers))
           (permission (car (buffer-local-value 'opencode-pending-permissions child-buffer))))
      (with-current-buffer buffer (should-not (opencode-pending-request-p)))
      (opencode-test-in-place parent
        (opencode-respond-session parent nil t)
        (should (eq (plist-get opencode--permission-transient-state :buffer) child-buffer))
        (opencode-test-keys "o"))
      (should (opencode-test-requests-to
               'post (format "/api/session/%s/permission/%s/reply" child (plist-get permission :id))))
      (with-current-buffer child-buffer (should-not opencode-pending-permissions)))))

(ert-deftest opencode-test-nested-subagent-in-place ()
  "Answered in place, a session answers the request of a subagent of its subagent, none of them shown."
  (opencode-test-with-server
    (let* ((top (opencode-test-scenario "text"))
           (child (opencode-test-scenario "subagent-child"))
           (buffer (opencode-test-open-empty top)))
      (opencode-test-dispatch-until (opencode-test-subagent-events) "permission.asked")
      (let* ((child-buffer (gethash child opencode-session-buffers))
             (permission (car (buffer-local-value 'opencode-pending-permissions child-buffer)))
             (opencode-state--ready t))
        ;; The child asking runs under a subagent of TOP, which has no buffer.
        (puthash top (list :id top :running t) opencode-state--sessions)
        (puthash "ses_test_middle" (list :id "ses_test_middle" :parent-id top :running t) opencode-state--sessions)
        (puthash child (list :id child :parent-id "ses_test_middle" :running t
                             :requests (list (list :kind 'permission :id (plist-get permission :id) :time 1.0)))
                 opencode-state--sessions)
        (with-current-buffer buffer
          (should-not (opencode-pending-request-p))
          (should (equal (opencode--waiting-subagent) child)))
        (opencode-test-in-place top
          (opencode-respond-session top nil t)
          (should (eq (plist-get opencode--permission-transient-state :buffer) child-buffer))
          (opencode-test-keys "o"))
        (should (opencode-test-requests-to
                 'post (format "/api/session/%s/permission/%s/reply" child (plist-get permission :id))))))))

(ert-deftest opencode-test-own-request-before-subagent-in-place ()
  "Answered in place, a session's own request comes before an older one of its subagent."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "permission"))
           (child (opencode-test-scenario "subagent-child"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")
      (opencode-test-dispatch-until (opencode-test-events child) "permission.asked")
      (let ((child-buffer (gethash child opencode-session-buffers))
            (opencode-state--ready t))
        (puthash id (list :id id :running t) opencode-state--sessions)
        (puthash child (list :id child :parent-id id :running t
                             :requests '((:kind permission :id "per_older" :time 1.0)))
                 opencode-state--sessions)
        (with-current-buffer buffer
          (should (equal (opencode--waiting-subagent) child))
          (should (equal (plist-get (car (opencode-tree-requests)) :session) id)))
        (opencode-test-in-place id
          (opencode-respond-session id nil t)
          (should (eq (plist-get opencode--permission-transient-state :buffer) buffer))
          (should-not (eq (plist-get opencode--permission-transient-state :buffer) child-buffer))
          (opencode-test-keys "C-g"))))))

(ert-deftest opencode-test-nothing-pending-in-place ()
  "Answered in place, a session whose tree awaits nothing says so."
  (opencode-test-with-server
    (let* ((parent (opencode-test-scenario "subagent"))
           (buffer (opencode-test-open-empty parent)))
      (opencode-test-dispatch-until (opencode-test-events parent) "session.tool.progress")
      (with-current-buffer buffer
        (should-not (opencode-tree-requests))
        (should-not (opencode--waiting-subagent)))
      (opencode-test-in-place parent
        (should (member "No pending OpenCode request"
                        (opencode-test-capturing-messages
                          (opencode-respond-session parent nil t))))
        (should-not (bound-and-true-p transient--prefix))))))

;;;; Key hints

(defun opencode-test-drover-keys (command)
  "Return keys a package binding COMMAND in its own maps would report, or nil."
  (alist-get command '((opencode-respond . "C-' A") (opencode-revert-clear . "C-' R")
                       (opencode-interrupt . "C-' k"))))

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
                         "Permission pending: Run rm junk.txt.  C-' A responds."))
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
