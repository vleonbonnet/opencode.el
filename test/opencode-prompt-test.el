;;; opencode-prompt-test.el --- Tests of session keys and prompt commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-own-keys (map)
  "Return the key descriptions MAP binds itself, its parents left out."
  (let (keys)
    (cl-labels ((walk (map prefix)
                  (map-keymap-internal
                   (lambda (event binding)
                     (let ((key (vconcat prefix (vector event))))
                       (if (keymapp binding)
                           (walk binding key)
                         (push (key-description key) keys))))
                   map)))
      (walk map []))
    (sort keys #'string<)))

(defun opencode-test-press (buffer keys)
  "Press KEYS in BUFFER, shown in the selected window.
Return the error they signaled, or nil.  Running them through the
command loop applies key remapping as typing them would."
  (save-window-excursion
    (set-window-buffer (selected-window) buffer)
    (with-current-buffer buffer
      (condition-case err
          (progn (execute-kbd-macro (key-parse keys)) nil)
        (error err)))))

(defun opencode-test-input (buffer)
  "Return the input typed at BUFFER's prompt."
  (with-current-buffer buffer
    (buffer-substring-no-properties (opencode-pmark) (point-max))))

(defun opencode-test-sent-requests ()
  "Return the prompts, commands and shell commands requested, newest first."
  (seq-filter (lambda (request)
                (string-match-p "/\\(prompt\\|command\\|shell\\)\\'" (plist-get request :path)))
              opencode-test-requests))

;;;; Keys

(ert-deftest opencode-test-session-keys ()
  "Session buffers bind only the standard keys, and remap the Comint commands."
  (should (equal (opencode-test-own-keys opencode-session-mode-map)
                 (sort (list "/" "<backtab>" "@" "C-c C-o" "M-n" "M-p" "RET" "TAB"
                             "<remap> <comint-clear-buffer>" "<remap> <comint-continue-subjob>"
                             "<remap> <comint-delchar-or-maybe-eof>" "<remap> <comint-delete-output>"
                             "<remap> <comint-interrupt-subjob>" "<remap> <comint-kill-subjob>"
                             "<remap> <comint-quit-subjob>" "<remap> <comint-send-eof>"
                             "<remap> <comint-stop-subjob>")
                       #'string<)))
  (should-not (opencode-test-own-keys opencode-session-list-mode-map))
  (should (eq (keymap-parent opencode-session-list-mode-map) special-mode-map))
  (should (equal (opencode-test-own-keys opencode-desktop-pending-mode-map) '("g"))))

(ert-deftest opencode-test-comint-keys-in-sessions ()
  "Comint's keys keep their meaning in a session, interrupting it or refusing harm."
  (opencode-test-with-server
    (let ((buffer (opencode-test-open (opencode-test-scenario "text"))))
      (with-current-buffer buffer
        (should (eq (key-binding (kbd "C-c C-o")) 'opencode-open-link-at-point))
        (should (eq (key-binding (kbd "C-c C-c")) 'opencode-interrupt))
        (should (eq (key-binding (kbd "C-d")) 'delete-char))
        (goto-char (point-max))
        (insert "typed"))
      (let ((transcript (opencode-test-text buffer)))
        ;; Each would send the input, mark it as sent without sending it, or erase the transcript.
        (pcase-dolist (`(,keys ,command)
                       '(("C-c C-d" comint-send-eof) ("C-c C-z" comint-stop-subjob)
                         ("C-c C-\\" comint-quit-subjob) ("C-c M-o" comint-clear-buffer)))
          (setq opencode-test-requests nil)
          (with-current-buffer buffer (goto-char (point-max)))
          (let ((error (opencode-test-press buffer keys)))
            (should (eq (car error) 'user-error))
            (should (equal (cadr error) (with-current-buffer buffer (opencode--comint-refusal command)))))
          (should-not opencode-test-requests)
          (should (equal (opencode-test-text buffer) transcript))
          (should (equal (opencode-test-input buffer) "typed")))
        ;; Commands only the menu offers are refused as well.
        (with-current-buffer buffer
          ;; Refusals name the keys of what helps instead.
          (should (equal (substring-no-properties (opencode--comint-refusal 'comint-stop-subjob))
                         "An OpenCode turn cannot be suspended: C-c C-c stops it"))
          (dolist (command '(comint-continue-subjob comint-kill-subjob comint-delete-output))
            (let ((remapped (command-remapping command)))
              (should (eq remapped 'opencode-comint-unsupported))
              (let ((this-original-command command))
                (should-error (call-interactively remapped) :type 'user-error)))))
        (should (equal (opencode-test-text buffer) transcript))
        ;; C-d deletes, and never sends the empty input as an end of file.
        (with-current-buffer buffer (goto-char (- (point-max) 2)))
        (should-not (opencode-test-press buffer "C-d"))
        (should (equal (opencode-test-input buffer) "typd"))
        (with-current-buffer buffer (delete-region (opencode-pmark) (point-max)))
        (setq opencode-test-requests nil)
        (should (eq (car (opencode-test-press buffer "C-d")) 'end-of-buffer))
        (should-not (opencode-test-sent-requests))
        (should (process-live-p (get-buffer-process buffer)))
        ;; C-c C-c interrupts the turn.
        (should-not (opencode-test-press buffer "C-c C-c"))
        (should (equal (mapcar (lambda (request) (plist-get request :path)) opencode-test-requests)
                       (list (format "/api/session/%s/interrupt" (opencode-test-scenario "text")))))))))

(ert-deftest opencode-test-revert-session-buffer ()
  "Reverting a session buffer renders its transcript again, keeping the input."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "text"))
           (buffer (opencode-test-open id))
           (transcript (opencode-test-text buffer)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "draft")
        (setq opencode-test-requests nil)
        (revert-buffer)
        (should (equal (mapcar (lambda (request) (plist-get request :path))
                               (seq-take (reverse opencode-test-requests) 2))
                       (list (concat "/api/session/" id) (format "/api/session/%s/message" id))))
        (should (equal (opencode-test-input buffer) "draft"))
        (should (equal (string-trim-right (string-remove-suffix "draft" (opencode-test-text buffer)))
                       transcript))))))

;;;; Attachments

(defun opencode-test-attachments (buffer)
  "Return BUFFER's attachments, oldest first."
  (reverse (buffer-local-value 'opencode--attachments buffer)))

(ert-deftest opencode-test-attach-region ()
  "With an active region, `opencode-attach' attaches its text to the last session."
  (opencode-test-with-server
    (let ((session (opencode-test-open (opencode-test-scenario "text")))
          (transient-mark-mode t))
      (setq opencode-last-session-buffer session)
      (with-temp-buffer
        (insert "first line\nsecond line")
        (set-mark 1)
        (goto-char 11)
        (activate-mark)
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "Prompted"))))
          (opencode-attach)))
      (let ((file (plist-get (car (opencode-test-attachments session)) :file)))
        (should (string-prefix-p "region of " (alist-get 'name file)))
        (should (equal (alist-get 'uri file) (opencode--data-uri "first line"))))
      (should (string-match-p "`region: first line` \\'" (opencode-test-input session))))))

(ert-deftest opencode-test-attach-file-or-buffer ()
  "`opencode-attach' offers files and buffers apart, and attaches to this session buffer."
  (opencode-test-with-server
    (let* ((other (opencode-test-open (opencode-test-scenario "shell")))
           (session (opencode-test-open (opencode-test-scenario "text")))
           (source (get-buffer-create "attach-source"))
           (hidden (get-buffer-create " attach-hidden"))
           (choice nil)
           (offered nil)
           (metadata nil))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (setq offered (all-completions "" collection)
                             metadata (completion-metadata "" collection nil))
                       choice)))
            (with-current-buffer source (insert "buffer text"))
            ;; The last selected session is another one: commands in a session buffer act on its own.
            (setq opencode-last-session-buffer other)
            (with-current-buffer session
              (goto-char (point-min))
              (setq choice "notes.txt")
              (opencode-attach)
              (should (member "notes.txt" offered))
              (should (member "buffer:attach-source" offered))
              (should-not (member (concat "buffer:" (buffer-name session)) offered))
              (should-not (member "buffer: attach-hidden" offered))
              (let ((group (completion-metadata-get metadata 'group-function)))
                (should (equal (funcall group "buffer:attach-source" nil) "Buffers"))
                (should (equal (funcall group "buffer:attach-source" t) "attach-source"))
                (should (equal (funcall group "notes.txt" nil) "Files")))
              (should (string-match-p "fundamental-mode"
                                      (funcall (completion-metadata-get metadata 'annotation-function)
                                               "buffer:attach-source")))
              (setq choice "buffer:attach-source")
              (opencode-attach))
            (should-not (opencode-test-attachments other))
            (pcase-let ((`(,file ,text) (mapcar (lambda (attachment) (plist-get attachment :file))
                                                (opencode-test-attachments session))))
              (should (equal (alist-get 'uri file)
                             (opencode--file-uri (expand-file-name "notes.txt" (buffer-local-value 'default-directory session)))))
              (should (equal (alist-get 'name text) "attach-source"))
              (should (equal (alist-get 'uri text) (opencode--data-uri "buffer text"))))
            ;; Point was in the transcript, so both went to the end of the input.
            (should (equal (opencode-test-input session) "`notes.txt` `attach-source` ")))
        (kill-buffer source)
        (kill-buffer hidden)))))

(ert-deftest opencode-test-attachment-names-stay-apart ()
  "A file whose name reads as a buffer is offered as a relative name."
  (let* ((directory (file-name-as-directory (make-temp-file "opencode-attach-" t)))
         (default-directory directory))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "buffer:x" directory) (insert "x"))
          (pcase-let ((`(,table ,keys) (opencode--attachment-candidates nil)))
            (should (member "./buffer:x" keys))
            (should (equal (gethash "./buffer:x" table)
                           (cons 'file (expand-file-name "buffer:x" directory))))))
      (delete-directory directory t))))

(ert-deftest opencode-test-attach-commands-target-this-session ()
  "The older attachment commands target this session buffer too."
  (opencode-test-with-server
    (let* ((other (opencode-test-open (opencode-test-scenario "shell")))
           (session (opencode-test-open (opencode-test-scenario "text")))
           (source (get-buffer-create "attach-source")))
      (unwind-protect
          (progn
            (setq opencode-last-session-buffer other)
            (cl-letf (((symbol-function 'read-buffer) (lambda (&rest _) "attach-source")))
              (with-current-buffer session (opencode-add-buffer-dwim)))
            (should (equal (opencode-test-input session) "`attach-source` "))
            (should-not (opencode-test-attachments other))
            ;; Elsewhere, the current buffer goes to the last selected session.
            (with-current-buffer source (opencode-add-buffer-dwim))
            (should (equal (opencode-test-input other) "`attach-source` ")))
        (kill-buffer source)))))

;;;; Mentions

(defun opencode-test-subagent-routes (agents)
  "Return the routes with AGENTS as the agent catalog."
  (cons (list 'get "\\`/api/agent\\'" (lambda (_) `((data . ,agents)))) opencode-test-routes))

(ert-deftest opencode-test-mention-subagent ()
  "A subagent is mentioned at the end of the input from anywhere in the buffer."
  (opencode-test-with-server
    (let ((offered nil))
      (cl-letf (((symbol-function 'opencode-annotated-completion)
                 (lambda (_prompt candidates) (setq offered (mapcar #'car candidates)) "explore")))
        (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
          (goto-char (point-max))
          (insert "ask ")
          (goto-char (point-min))
          (let ((last-command-event ?x))
            (opencode-mention-subagent))
          (should (equal offered '("general" "explore")))
          (should (equal (opencode-test-input (current-buffer)) "ask `@explore`"))
          (should (= (point) (point-max)))
          (should (equal (opencode-test-attachments (current-buffer)) '((:agent "explore")))))))))

(ert-deftest opencode-test-mention-key ()
  "@ at the input becomes the chosen mention, and stays @ without subagents."
  (opencode-test-with-server
    (let ((buffer (opencode-test-open (opencode-test-scenario "text"))))
      (cl-letf (((symbol-function 'opencode-annotated-completion) (lambda (&rest _) "general")))
        (with-current-buffer buffer (goto-char (point-max)))
        (should-not (opencode-test-press buffer "@"))
        (should (equal (opencode-test-input buffer) "`@general`"))
        (with-current-buffer buffer
          (delete-region (opencode-pmark) (point-max))
          (setq opencode--attachments nil)))
      (let ((opencode-test-routes (opencode-test-subagent-routes '(((id . "build") (mode . "primary")))))
            (opencode--catalogs (make-hash-table :test #'equal)))
        (cl-letf (((symbol-function 'opencode-annotated-completion) (lambda (&rest _) (ert-fail "Prompted"))))
          (should-not (opencode-test-press buffer "@"))
          (should (equal (opencode-test-input buffer) "@"))
          (with-current-buffer buffer
            (should-error (opencode-mention-subagent) :type 'user-error)))))))

;;;; Slash commands

(ert-deftest opencode-test-insert-slash-command ()
  "A command goes before the input, replacing a command already there."
  (opencode-test-with-server
    (cl-letf (((symbol-function 'opencode-annotated-completion) (lambda (&rest _) "review")))
      (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
        (let ((last-command-event ?x))
          (goto-char (point-max))
          (opencode-insert-slash-command)
          (should (equal (opencode-test-input (current-buffer)) "/review "))
          (should (= (point) (point-max)))
          (delete-region (opencode-pmark) (point-max))
          (insert "HEAD~2")
          (goto-char (point-min))
          (opencode-insert-slash-command)
          (should (equal (opencode-test-input (current-buffer)) "/review HEAD~2"))
          (should (= (point) (+ (opencode-pmark) (length "/review "))))
          (delete-region (opencode-pmark) (point-max))
          (insert "/init HEAD~2")
          (opencode-insert-slash-command)
          (should (equal (opencode-test-input (current-buffer)) "/review HEAD~2")))))))

(ert-deftest opencode-test-slash-key ()
  "/ at the start of the input picks a command; elsewhere it is a slash."
  (opencode-test-with-server
    (let ((buffer (opencode-test-open (opencode-test-scenario "text")))
          (asked 0))
      (cl-letf (((symbol-function 'opencode-annotated-completion)
                 (lambda (&rest _) (cl-incf asked) "review")))
        (with-current-buffer buffer (goto-char (point-max)) (insert "a"))
        (should-not (opencode-test-press buffer "/"))
        (should (equal (opencode-test-input buffer) "a/"))
        (with-current-buffer buffer (goto-char (opencode-pmark)))
        (should-not (opencode-test-press buffer "/"))
        (should (equal (opencode-test-input buffer) "/review a/"))
        (should (= asked 1))))))

;;;; Message IDs of sent input

(defun opencode-test-send-typed (text)
  "Type TEXT at the end of the input and send it as RET does."
  (goto-char (point-max))
  (insert text)
  (opencode-send-input))

(defun opencode-test-sent-id ()
  "Return the message ID of the latest prompt requested."
  (alist-get 'id (plist-get (seq-find (lambda (request) (string-suffix-p "/prompt" (plist-get request :path)))
                                      opencode-test-requests)
                            :body)))

(defun opencode-test-inbox-events (session id text &optional metadata)
  "Return the events admitting, then delivering, prompt ID of SESSION with TEXT and METADATA."
  `(((type . "session.inbox.enqueued")
     (data (sessionID . ,session) (inboxID . ,id)
           (item (type . "user") (delivery . "steer")
                 (payload (text . ,text) ,@(when metadata `((metadata . ,metadata)))))))
    ((type . "session.inbox.delivered") (data (sessionID . ,session) (inboxID . ,id)))))

(defun opencode-test-answer (session message text)
  "Stream TEXT as the answer of assistant MESSAGE in SESSION."
  (let ((base `((sessionID . ,session) (assistantMessageID . ,message) (ordinal . 0))))
    (opencode-test-dispatch
     `(((type . "session.step.started") (data (sessionID . ,session) (assistantMessageID . ,message)))
       ((type . "session.text.started") (data ,@base))
       ((type . "session.text.delta") (data ,@base (delta . ,text)))
       ((type . "session.text.ended") (data ,@base (text . ,text)))))))

(defun opencode-test-turn (session event)
  "Dispatch the execution EVENT of SESSION, such as \"started\"."
  (opencode-test-dispatch `(((type . ,(concat "session.execution." event)) (data (sessionID . ,session))))))

(defun opencode-test-find (text)
  "Return the position of the first TEXT in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (match-beginning 0)))

(ert-deftest opencode-test-typed-prompt-gets-its-id ()
  "A prompt typed and sent gets its message ID once delivered, for revert and fork."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open-empty session)
        (opencode-test-send-typed "first question")
        (let ((first (opencode-test-sent-id)))
          ;; Admitted but not delivered: no message exists to cut at yet.
          (opencode-test-dispatch (list (car (opencode-test-inbox-events session first "first question"))))
          (should-not (opencode-message-at (opencode-test-find "first question")))
          (opencode-test-dispatch (cdr (opencode-test-inbox-events session first "first question")))
          ;; Shown once, as typed.
          (should (= (how-many "first question" (point-min) (point-max)) 1))
          (let ((prompt (opencode-test-find "first question")))
            (should (equal (opencode-message-at prompt) first))
            (should (equal (opencode-message-at (+ prompt (length "first question"))) first))
            (opencode-test-turn session "started")
            (opencode-test-answer session "msg_answer_one" "First answer.")
            (opencode-test-turn session "succeeded")
            ;; At the prompt, the cut keeps it and drops its answer.
            (should (equal (opencode--message-boundary (opencode-test-find "first question")) "msg_answer_one"))
            ;; In the answer, nothing follows yet to drop.
            (should-not (opencode--message-boundary (opencode-test-find "First answer."))))
          (opencode-test-send-typed "second question")
          (let ((second (opencode-test-sent-id)))
            (should-not (equal second first))
            (opencode-test-dispatch (opencode-test-inbox-events session second "second question"))
            ;; In the first answer, the next prompt starts what is dropped.
            (should (equal (opencode--message-boundary (opencode-test-find "First answer.")) second))
            (goto-char (opencode-test-find "First answer."))
            (setq opencode-test-requests nil)
            (opencode-revert-to-point)
            (should (equal (opencode-test-requests-to 'post "/revert/stage")
                           `(((messageID . ,second) (files . t)))))
            (should (zerop (hash-table-count opencode--sent-inputs)))))))))

(ert-deftest opencode-test-typed-prompt-delivered-before-reply ()
  "A prompt delivered before the service answers its request gets its ID too."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes
            (cons (list 'post "/prompt\\'"
                        (lambda (request)
                          (let ((body (plist-get request :body)))
                            (opencode-test-dispatch
                             (opencode-test-inbox-events session (alist-get 'id body) (alist-get 'text body)))
                            `((data (id . ,(alist-get 'id body)))))))
                  opencode-test-routes)))
      (with-current-buffer (opencode-test-open-empty session)
        (opencode-test-send-typed "quick one")
        (should (equal (opencode-message-at (opencode-test-find "quick one")) (opencode-test-sent-id)))
        (should (= (how-many "quick one" (point-min) (point-max)) 1))
        (should (zerop (hash-table-count opencode--sent-inputs)))))))

(ert-deftest opencode-test-steering-prompts-get-their-ids ()
  "Prompts sent while a turn streams each get their ID, in any order, and cut in the conversation's order."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text"))
          (ids nil))
      (with-current-buffer (opencode-test-open-empty session)
        (opencode-test-send-typed "start working")
        (push (opencode-test-sent-id) ids)
        (opencode-test-dispatch (opencode-test-inbox-events session (car ids) "start working"))
        (opencode-test-turn session "started")
        (let ((base `((sessionID . ,session) (assistantMessageID . "msg_busy") (ordinal . 0))))
          (opencode-test-dispatch
           `(((type . "session.step.started") (data (sessionID . ,session) (assistantMessageID . "msg_busy")))
             ((type . "session.text.started") (data ,@base))
             ((type . "session.text.delta") (data ,@base (delta . "Working on it")))))
          (dolist (text '("steer one" "steer two"))
            (opencode-test-send-typed text)
            (push (opencode-test-sent-id) ids)
            ;; Output keeps streaming between the sends.
            (opencode-test-dispatch `(((type . "session.text.delta") (data ,@base (delta . ,(concat " after " text)))))))
          (opencode-test-dispatch
           `(((type . "session.text.ended")
              (data ,@base (text . "Working on it after steer one after steer two")))))
          (pcase-let ((`(,two ,one ,start) ids))
            (should (= (length (delete-dups (copy-sequence ids))) 3))
            (should (= (hash-table-count opencode--sent-inputs) 2))
            ;; The second one is reported delivered first.
            (opencode-test-dispatch (cdr (opencode-test-inbox-events session two "steer two")))
            (should (equal (opencode-message-at (opencode-test-find "steer two")) two))
            (should-not (opencode-message-at (opencode-test-find "steer one")))
            (opencode-test-dispatch (cdr (opencode-test-inbox-events session one "steer one")))
            (should (equal (opencode-message-at (opencode-test-find "steer one")) one))
            ;; Each ID covers its own input and nothing of the output around it.
            (should (equal (opencode-message-at (opencode-test-find " after steer one")) "msg_busy"))
            (should (equal (opencode-message-at (opencode-test-find " after steer two")) "msg_busy"))
            (should-not (text-property-any (opencode-test-find "steer one")
                                           (+ (opencode-test-find "steer one") (length "steer one"))
                                           'opencode-message two))
            (opencode-test-answer session "msg_next" "Next step.")
            ;; The conversation reads: start, busy, two, one, next.
            (should (equal (opencode--message-boundary (opencode-test-find "start working")) "msg_busy"))
            (should (equal (opencode--message-boundary (opencode-test-find "steer two")) one))
            (should (equal (opencode--message-boundary (opencode-test-find "steer one")) "msg_next"))
            (dolist (answer '("Working on it" " after steer one" " after steer two"))
              (should (equal (opencode--message-boundary (opencode-test-find answer)) two)))
            (should-not (opencode--message-boundary (opencode-test-find "Next step.")))
            (should (equal start (opencode-message-at (opencode-test-find "start working"))))))))))

(ert-deftest opencode-test-failed-prompt-gets-no-id ()
  "A prompt the service refused keeps no ID, even when an event names it later."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text"))
          (opencode-test-routes (cons (list 'post "/prompt\\'" (lambda (_) '(:error 500 "boom")))
                                      opencode-test-routes)))
      (with-current-buffer (opencode-test-open-empty session)
        (opencode-test-send-typed "doomed")
        (let ((id (opencode-test-sent-id)))
          (should (string-match-p "boom" (buffer-string)))
          (should-not (opencode-message-at (opencode-test-find "doomed")))
          (should (zerop (hash-table-count opencode--sent-inputs)))
          (should-not (gethash id opencode--own-inputs))
          ;; Had the service taken it after all, its prompt shows as delivered, with its ID.
          (opencode-test-dispatch (opencode-test-inbox-events session id "doomed"))
          (should-not (opencode-message-at (opencode-test-find "doomed")))
          (should (= (how-many "doomed" (point-min) (point-max)) 2)))))))

(ert-deftest opencode-test-cancelled-prompt-gets-no-id ()
  "A queued prompt cancelled before delivery keeps no ID."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open-empty session)
        (goto-char (point-max))
        (insert "later maybe")
        (opencode-queue-input)
        (let ((id (opencode-test-sent-id)))
          (opencode-test-dispatch
           (list (car (opencode-test-inbox-events session id "later maybe"))
                 `((type . "session.inbox.cancelled") (data (sessionID . ,session) (inboxID . ,id)))))
          (should (string-match-p "A queued prompt was cancelled" (buffer-string)))
          (should-not (opencode-message-at (opencode-test-find "later maybe")))
          (should (zerop (hash-table-count opencode--sent-inputs))))))))

(ert-deftest opencode-test-reloaded-pending-prompt-shown-once-delivered ()
  "A prompt whose input a reload erased before its delivery is shown when delivered."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes
            (cons (list 'get "/message\\'" (lambda (_) '((data) (cursor)))) opencode-test-routes)))
      (with-current-buffer (opencode-test-open-empty session)
        (opencode-test-send-typed "survive the reload")
        (let ((id (opencode-test-sent-id)))
          (opencode-test-dispatch (list (car (opencode-test-inbox-events session id "survive the reload"))))
          (opencode-session-reload)
          (should-not (string-search "survive the reload" (buffer-string)))
          (opencode-test-dispatch (cdr (opencode-test-inbox-events session id "survive the reload")))
          (should (= (how-many "survive the reload" (point-min) (point-max)) 1))
          (should (equal (opencode-message-at (opencode-test-find "survive the reload")) id))
          (should (zerop (hash-table-count opencode--sent-inputs))))))))

(ert-deftest opencode-test-typed-command-gets-its-prompt-id ()
  "A slash command's input gets the ID of the prompt the service made for it."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open-empty session)
        (opencode-test-send-typed "/review HEAD")
        (let* ((metadata (alist-get 'metadata (car (opencode-test-requests-to 'post "/command"))))
               (key (alist-get opencode--command-key metadata)))
          (should (stringp key))
          ;; Another client's prompt does not claim it.
          (opencode-test-dispatch (opencode-test-inbox-events session "msg_other" "from the TUI"))
          (should-not (opencode-message-at (opencode-test-find "/review HEAD")))
          (opencode-test-dispatch
           (opencode-test-inbox-events session "msg_review" "Review the changes of HEAD" metadata))
          (should (equal (opencode-message-at (opencode-test-find "/review HEAD")) "msg_review"))
          ;; The prompt the command expanded to shows too, as replay shows it.
          (should (equal (opencode-message-at (opencode-test-find "Review the changes")) "msg_review"))
          (opencode-test-turn session "started")
          (opencode-test-answer session "msg_review_answer" "Looks fine.")
          (should (equal (opencode--message-boundary (opencode-test-find "/review HEAD")) "msg_review_answer"))
          (should (zerop (hash-table-count opencode--sent-inputs))))))))

(ert-deftest opencode-test-command-without-prompt-forgotten ()
  "A command whose prompt never comes, as one running a subagent, is forgotten in time."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open-empty session)
        (opencode-test-send-typed "/delegate it")
        (let ((key (alist-get opencode--command-key
                              (alist-get 'metadata (car (opencode-test-requests-to 'post "/command"))))))
          (should (plist-get (gethash key opencode--sent-inputs) :answered))
          ;; Its prompt may still come.
          (opencode-test-send-typed "first prompt")
          (let ((first (opencode-test-sent-id)))
            (should (gethash key opencode--sent-inputs))
            ;; Its request returned longer ago than its prompt may take.
            (plist-put (gethash key opencode--sent-inputs) :answered
                       (- (float-time) opencode--command-wait 1))
            (opencode-test-send-typed "next prompt")
            (should-not (gethash key opencode--sent-inputs))
            ;; Prompts wait for their delivery however long it takes.
            (should (equal (sort (hash-table-keys opencode--sent-inputs) #'string<)
                           (sort (list first (opencode-test-sent-id)) #'string<)))))))))

(ert-deftest opencode-test-shell-input-gets-no-id ()
  "A shell command makes no prompt, so its input gets no message ID."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (opencode-test-send-typed "!ls")
      (should (opencode-test-requests-to 'post "/shell"))
      (should (zerop (hash-table-count opencode--sent-inputs)))
      (should-not (opencode-message-at (opencode-test-find "!ls"))))))

;;;; Agents

(ert-deftest opencode-test-select-agent-toggles-two ()
  "With two agents to choose from, the other is selected without asking."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
      (cl-letf (((symbol-function 'opencode-annotated-completion) (lambda (&rest _) (ert-fail "Prompted"))))
        (should (equal opencode-session-agent "build"))
        (setq opencode-test-requests nil)
        (opencode-select-agent)
        (should (equal (opencode-test-requests-to 'post "/agent") '(((agent . "plan")))))
        (should (equal opencode-session-agent "plan"))
        (opencode-select-agent)
        (should (equal opencode-session-agent "build"))
        ;; A session that never chose runs the default agent.
        (setq opencode-session-agent nil opencode-test-requests nil)
        (opencode-select-agent)
        (should (equal (opencode-test-requests-to 'post "/agent") '(((agent . "plan")))))))))

(provide 'opencode-prompt-test)
;;; opencode-prompt-test.el ends here
