;;; opencode-session.el --- Session buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann
;; Copyright (C) 2026  Valentin Leon

;; Author: Scott Zimmermann <sczi@disroot.org>
;;         Valentin Leon <valentin@leon.click>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; One buffer per session renders its transcript and takes its input.
;; Opening a session replays the server's projected messages; from then
;; on the event stream drives it: a step is one assistant message, text
;; and reasoning stream as started/delta/ended fragments numbered by
;; ordinal within their step, and each tool call moves through input,
;; called, progress and success or failure.  A turn runs from the
;; execution starting until it succeeds, fails or is interrupted.
;;
;; The server keeps each session's agent and model; Emacs shows and
;; switches them.  Prompts carry an ID generated here, so the echo of
;; Emacs's own prompt on the stream is recognized while prompts sent by
;; other clients are rendered.  A prompt sent while a turn runs steers
;; it by default (`opencode-prompt-delivery').
;;
;; Nothing is replayed after the stream reconnects, so every open buffer
;; reconciles with the server then, and reloads its transcript when the
;; session changed meanwhile.

;;; Code:

(require 'comint)
(require 'opencode-blocks)
(require 'opencode-common)
(require 'opencode-events)
(require 'opencode-http)
(require 'opencode-links)
(require 'opencode-notify)
(require 'opencode-render)
(require 'savehist)
(require 'yank-media)

(declare-function opencode-copy-conversation "opencode-convert" ())

(defcustom opencode-prompt-delivery 'steer
  "How a prompt sent while a turn runs is delivered.
`steer' joins the running turn at its next step; `queue' waits for the
turn to finish and starts the next one.  An idle session starts a turn
either way."
  :type '(choice (const steer) (const queue))
  :group 'opencode)

(defcustom opencode-show-switch-notices t
  "When non-nil, show agent and model switches in the transcript."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-session-deleted-functions nil
  "Abnormal hook run with a session ID when that session is deleted."
  :type 'hook
  :group 'opencode)

(defcustom opencode-session-idle-functions nil
  "Abnormal hook run in a session buffer when its turn ends.
Each function receives the outcome: `succeeded', `failed' or `interrupted'."
  :type 'hook
  :group 'opencode)

(defcustom opencode-before-send-functions nil
  "Abnormal hook run before a prompt or command is sent.
Each function receives a plist (:kind :text :name) and may return
`cancel' to keep the input in the prompt instead."
  :type 'hook
  :group 'opencode)

(defvar opencode-step-ended-functions nil
  "Functions called in the session buffer with each `session.step.ended' data.")

(defvar opencode-turn-ended-functions nil
  "Functions called in the session buffer with the outcome of each turn that ends.
They run before `opencode-session-idle-functions'.")

(defvar opencode-step-failed-functions nil
  "Functions called with `session.step.failed' data; non-nil handles its display.")

(defvar opencode-session-replayed-hook nil
  "Hook run in a session buffer after its transcript was replayed.")

(defvar opencode-session-resync-functions nil
  "Functions called in each session buffer after the stream reconnects.
Each receives the session info fetched from the server.")

(defvar opencode-last-model nil
  "Model reference (alist id, providerID, variant) of the last prompt sent.")
(add-to-list 'savehist-additional-variables 'opencode-last-model)

(defvar-local opencode-session-info nil
  "Session info alist as last fetched from the server.")

(defvar-local opencode-session-status 'idle
  "`idle', or `busy' while a turn runs.")

(defvar-local opencode-session-agent nil
  "Agent ID the session runs with.")

(defvar-local opencode-session-model nil
  "Model reference (alist id, providerID, variant) the session runs with.")

(defvar-local opencode-session-tokens nil
  "Context tokens reported by the last step that reported usage.")

(defvar-local opencode-session-retry nil
  "Data of the retry the current step is waiting for, or nil.")

(defvar-local opencode-session-revert nil
  "Staged revert of this session, or nil.")

(defvar-local opencode--own-inputs nil
  "Hash table of inbox IDs sent from this buffer.")

(defvar-local opencode--text-blocks nil
  "Hash table of text block keys to plists (:start MARKER :text STRING).")

(defvar-local opencode--current-assistant nil
  "ID of the assistant message streaming in this buffer.")

(defvar-local opencode--attachments nil
  "Files and agents to send with the next prompt, newest first.
Each is a plist (:file URI-ALIST) or (:agent NAME).")

(defvar-local opencode--temp-files nil
  "Temporary files to delete once sent or when the buffer is killed.")

(defvar-local opencode--replaying nil
  "Non-nil while the transcript is being replayed.")

(defvar-local opencode--turn-errors nil
  "Error messages already shown during the current turn.")

(defun opencode-empty-object ()
  "Return a value `json-serialize' encodes as an empty object."
  (make-hash-table))

;;;; Catalogs

(defvar opencode--catalogs (make-hash-table :test #'equal)
  "Location catalogs, keyed by (DIRECTORY . KIND).")

(defvar opencode--catalog-waiters (make-hash-table :test #'equal)
  "Callbacks waiting for a catalog fetch in flight, keyed like `opencode--catalogs'.")

(defvar opencode-catalog-generation 0
  "Count of catalog changes, for caches of values derived from catalogs.")

(defconst opencode--catalog-paths
  '((agent . "/api/agent") (model . "/api/model") (provider . "/api/provider")
    (command . "/api/command") (skill . "/api/skill") (mcp . "/api/mcp"))
  "Catalog kinds and their routes.")

(defun opencode-catalog (kind &optional directory)
  "Return the cached KIND catalog of DIRECTORY, or nil when not fetched."
  (gethash (cons (opencode-normalize-directory (or directory default-directory)) kind)
           opencode--catalogs))

(defun opencode-catalog-fetch (kind callback &optional directory)
  "Call CALLBACK with the KIND catalog of DIRECTORY, fetching it when needed.
A location's catalogs fill in shortly after its first request, so an
empty answer is fetched again once before being cached."
  (let* ((directory (opencode-normalize-directory (or directory default-directory)))
         (key (cons directory kind))
         (cached (gethash key opencode--catalogs 'missing)))
    (if (not (eq cached 'missing))
        (funcall callback cached)
      (push callback (gethash key opencode--catalog-waiters))
      (when (null (cdr (gethash key opencode--catalog-waiters)))
        (cl-labels ((fetch (retries)
                      (opencode-request
                       'get (alist-get kind opencode--catalog-paths)
                       :directory directory :buffer nil
                       :then (lambda (response)
                               (let ((data (opencode-request-data response)))
                                 (if (and (null data) (> retries 0))
                                     (run-at-time 0.5 nil #'fetch (1- retries))
                                   (puthash key data opencode--catalogs)
                                   (cl-incf opencode-catalog-generation)
                                   (let ((waiters (gethash key opencode--catalog-waiters)))
                                     (remhash key opencode--catalog-waiters)
                                     (dolist (waiter (nreverse waiters))
                                       (funcall waiter data))))))
                       :else (lambda (error)
                               (remhash key opencode--catalog-waiters)
                               (opencode-report-error (opencode-error-describe error))))))
          (fetch 2))))))

(defun opencode-catalog-ensure (kind &optional directory)
  "Return the cached KIND catalog of DIRECTORY, else fetch it in the background.
Return nil while fetching; mode lines are redisplayed once it arrives.
Callers run at redisplay, so the request starts from a timer."
  (let* ((directory (opencode-normalize-directory (or directory default-directory)))
         (cached (gethash (cons directory kind) opencode--catalogs 'missing)))
    (if (not (eq cached 'missing))
        cached
      (unless (gethash (cons directory kind) opencode--catalog-waiters)
        (run-at-time 0 nil #'opencode-catalog-fetch kind
                     (lambda (_) (force-mode-line-update t))
                     directory))
      nil)))

(defun opencode--invalidate-catalog (event)
  "Forget the catalog EVENT says changed in its location."
  (when-let* ((directory (opencode-event-directory event))
              (kind (pcase (alist-get 'type event)
                      ("agent.updated" '(agent))
                      ((or "model.updated" "provider.updated" "integration.updated") '(model provider))
                      ("command.updated" '(command))
                      ("skill.updated" '(skill))
                      ("mcp.updated" '(mcp)))))
    (dolist (one kind)
      (remhash (cons directory one) opencode--catalogs))
    (cl-incf opencode-catalog-generation)))

(defun opencode--clear-catalogs (_event)
  "Forget every catalog when a stream opens: the service may have changed."
  (clrhash opencode--catalogs)
  (cl-incf opencode-catalog-generation))

(dolist (type '("agent.updated" "model.updated" "provider.updated" "integration.updated"
                "command.updated" "skill.updated" "mcp.updated"))
  (opencode-events-register type #'opencode--invalidate-catalog))
(opencode-events-register "server.connected" #'opencode--clear-catalogs)

(defun opencode-agent-info (agent &optional directory)
  "Return the catalog entry of AGENT in DIRECTORY, if fetched."
  (seq-find (lambda (entry) (equal (alist-get 'id entry) agent))
            (opencode-catalog 'agent directory)))

(defun opencode-model-info (model &optional directory)
  "Return the catalog entry of MODEL, a model reference, in DIRECTORY."
  (let-alist model
    (seq-find (lambda (entry)
                (and (equal (alist-get 'id entry) .id)
                     (equal (alist-get 'providerID entry) .providerID)))
              (opencode-catalog 'model directory))))

(defun opencode-provider-info (provider &optional directory)
  "Return the catalog entry of PROVIDER, a provider ID, in DIRECTORY, if fetched."
  (seq-find (lambda (entry) (equal (alist-get 'id entry) provider))
            (opencode-catalog 'provider directory)))

(defun opencode-primary-agents (&optional directory)
  "Return the selectable primary agents of DIRECTORY's catalog."
  (seq-filter (lambda (agent)
                (and (equal (alist-get 'mode agent) "primary")
                     (not (alist-get 'hidden agent))))
              (opencode-catalog 'agent directory)))

;;;; Mode

(defvar-keymap opencode-session-mode-map
  :parent comint-mode-map
  "TAB" #'opencode-tab-dwim
  "<backtab>" #'backward-button
  "C-c TAB" #'opencode-cycle-agent
  "C-c C-a" #'opencode-select-agent
  "C-c m" #'opencode-select-model
  "C-c v" #'opencode-select-variant
  "C-c C-c" #'opencode-interrupt
  "C-c C-o" #'opencode-open-link-at-point
  "C-c C-y" #'opencode-yank-code-block
  "C-c Y" #'opencode-copy-conversation
  "C-c x" #'opencode-delete-session
  "C-c r" #'opencode-rename-session
  "C-c n" #'opencode-new-session
  "C-c c" #'opencode-select-child-session
  "C-c C" #'opencode-compact-session
  "C-c t" #'opencode-scroll-to-last-response-start
  "C-c p" #'opencode-open-parent
  "C-c f" #'opencode-add-file
  "C-c b" #'opencode-add-buffer-dwim
  "C-c F" #'opencode-fork-session
  "C-c R" #'opencode-revert-to-point
  "C-c U" #'opencode-revert-clear
  "C-c g" #'opencode-session-reload
  "/" #'opencode-insert-slash-command
  "@" #'opencode-add-subagent)

(defun opencode--session-cleanup ()
  "Release this buffer's process, registration and temporary files."
  (when (and opencode-session-id
             (eq (gethash opencode-session-id opencode-session-buffers) (current-buffer)))
    (remhash opencode-session-id opencode-session-buffers))
  (when-let* ((process (get-buffer-process (current-buffer))))
    (delete-process process))
  (mapc (lambda (file) (ignore-errors (delete-file file))) opencode--temp-files)
  (setq opencode--temp-files nil))

(define-derived-mode opencode-session-mode comint-mode "OpenCode"
  "Major mode of an OpenCode session buffer.
\\{opencode-session-mode-map}"
  (setq-local comint-use-prompt-regexp nil
              comint-input-sender #'opencode--send-input
              comint-highlight-input nil
              left-margin-width (1+ left-margin-width))
  ;; Output never prompts for a password.
  (setq-local comint-output-filter-functions
              (remove #'comint-watch-for-password-prompt comint-output-filter-functions))
  (opencode-render-setup)
  (opencode-blocks-setup)
  (setq opencode--own-inputs (make-hash-table :test #'equal)
        opencode--text-blocks (make-hash-table :test #'equal))
  (add-hook 'kill-buffer-hook #'opencode--session-cleanup nil t)
  (add-hook 'comint-input-filter-functions
            (lambda (&rest _) (setq opencode--last-block-kind 'user)) nil t)
  (add-hook 'fill-nobreak-predicate #'opencode--in-attachment-p nil t)
  (visual-line-mode)
  (cursor-intangible-mode)
  (yank-media-handler "\\`image/" #'opencode--yank-image))

(defun opencode--make-process (buffer)
  "Create BUFFER's pipe process.
Input goes over HTTP, so neither a PTY nor window-size notifications are
needed; on Windows a PTY would run a synchronous helper on every resize."
  (let ((process (make-pipe-process :name "opencode-session" :buffer buffer :noquery t)))
    (process-put process 'adjust-window-size-function #'ignore)
    process))

;;;; Opening sessions

(defun opencode-session-buffer-name (info)
  "Return the buffer name of session INFO."
  (format "*OpenCode: %s*" (or (alist-get 'title info) (alist-get 'id info))))

(defun opencode--session-directory (info)
  "Return the directory of session INFO."
  (opencode-normalize-directory (alist-get 'directory (alist-get 'location info))))

(defun opencode--display (buffer no-display other-window)
  "Show BUFFER unless NO-DISPLAY: in another window when OTHER-WINDOW."
  (unless no-display
    (if other-window (pop-to-buffer buffer) (switch-to-buffer buffer)))
  buffer)

(defun opencode--adopt-info (info)
  "Record session INFO in this buffer."
  (setq opencode-session-info info
        opencode-session-agent (or (alist-get 'agent info) opencode-session-agent)
        opencode-session-model (or (alist-get 'model info) opencode-session-model)
        opencode-session-revert (alist-get 'revert info))
  (force-mode-line-update))

(cl-defun opencode-session-open (info &key buffer no-display (other-window t) callback)
  "Show the buffer of session INFO, creating and filling it when needed.
INFO is a session info alist.  BUFFER reuses an existing placeholder
buffer.  NO-DISPLAY leaves every window alone.  OTHER-WINDOW pops the
buffer instead of switching to it.  CALLBACK runs in the buffer with
INFO once the transcript is loaded.  Return the buffer."
  (let* ((id (alist-get 'id info))
         (existing (gethash id opencode-session-buffers)))
    (if (buffer-live-p existing)
        (progn
          (when callback (with-current-buffer existing (funcall callback info)))
          (opencode--display existing no-display other-window))
      (let ((buffer (or buffer (generate-new-buffer (opencode-session-buffer-name info)))))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer))
          (setq buffer-read-only nil)
          (opencode-session-mode)
          (setq opencode-session-id id
                default-directory (opencode--session-directory info))
          (opencode--adopt-info info)
          (hack-dir-local-variables-non-file-buffer)
          (puthash id buffer opencode-session-buffers)
          (opencode--make-process buffer)
          (opencode-session-load
           (lambda ()
             (when callback (funcall callback info)))))
        (opencode--display buffer no-display other-window)))))

(defun opencode-session-fetch (id callback &optional directory)
  "Call CALLBACK with the info of session ID; DIRECTORY scopes the request."
  (opencode-request 'get (concat "/api/session/" id)
                    :directory directory :buffer nil
                    :then (lambda (response) (funcall callback (opencode-request-data response)))))

(cl-defun opencode-session-open-by-id (id &key no-display (other-window t) callback)
  "Open session ID like `opencode-session-open' with NO-DISPLAY, OTHER-WINDOW, CALLBACK."
  (if-let* ((buffer (gethash id opencode-session-buffers))
            ((buffer-live-p buffer)))
      (progn
        (when callback (with-current-buffer buffer (funcall callback opencode-session-info)))
        (opencode--display buffer no-display other-window))
    (opencode-session-fetch
     id (lambda (info)
          (opencode-session-open info :no-display no-display :other-window other-window
                                 :callback callback)))))

(defun opencode--default-model ()
  "Return the model reference new sessions start with."
  opencode-last-model)

(cl-defun opencode-session-create (&key (directory default-directory) title agent model
                                        callback no-display)
  "Create a session in DIRECTORY and open it.
TITLE, AGENT and MODEL (a model reference) are optional; MODEL defaults
to `opencode-last-model'.  CALLBACK runs in the new buffer with its info;
NO-DISPLAY leaves every window alone."
  (let ((directory (opencode-normalize-directory directory))
        (model (or model (opencode--default-model))))
    (opencode-request
     'post "/api/session" :directory directory :buffer nil
     :body `((location . ((directory . ,(directory-file-name directory))))
             ,@(when title `((title . ,title)))
             ,@(when agent `((agent . ,agent)))
             ,@(when model `((model . ,model))))
     :then (lambda (response)
             (opencode-session-open (opencode-request-data response)
                                    :no-display no-display :callback callback)))))

(defun opencode-new-session (&optional title)
  "Create a session in the current project, prompting for TITLE with a prefix."
  (interactive (list (when current-prefix-arg (read-string "Title: "))))
  (opencode-events-connect)
  (opencode-session-create :directory (opencode-project-directory) :title title))

;;;; Loading the transcript

(defun opencode--fetch-messages (id callback &optional cursor accumulated)
  "Call CALLBACK with every projected message of session ID, oldest first.
CURSOR and ACCUMULATED page through the list."
  (opencode-request
   'get (format "/api/session/%s/message" id)
   :query `((order . "asc") (limit . 200) (cursor . ,cursor))
   :then (lambda (response)
           (let ((data (opencode-request-data response))
                 (next (alist-get 'next (alist-get 'cursor response))))
             (if (and data next)
                 (opencode--fetch-messages id callback next (nconc accumulated data))
               (funcall callback (nconc accumulated data)))))))

(defun opencode-session-load (&optional callback)
  "Replay this session's transcript, then adopt its status and run CALLBACK."
  (let ((id opencode-session-id))
    (opencode--fetch-messages
     id
     (lambda (messages)
       (opencode-replay-messages messages)
       (opencode-request
        'get "/api/session/active"
        :then (lambda (response)
                (let ((active (assq (intern id) (opencode-request-data response))))
                  (setq opencode-session-status (if active 'busy 'idle))
                  (unless active
                    (opencode-open-block 'user)
                    (opencode-show-prompt))
                  (force-mode-line-update)
                  (run-hooks 'opencode-session-replayed-hook)
                  (when callback (funcall callback)))))))))

(defun opencode-session-reload ()
  "Render this session's transcript again from the server.
Input typed at the prompt is kept."
  (interactive)
  (unless opencode-session-id (user-error "Not in an OpenCode session"))
  (let ((input (and (get-buffer-process (current-buffer))
                    (buffer-substring-no-properties (opencode-pmark) (point-max))))
        (line (line-number-at-pos)))
    (opencode-session-fetch
     opencode-session-id
     (lambda (info)
       (opencode-with-session-buffer (alist-get 'id info)
         (let ((inhibit-read-only t))
           (remove-overlays)
           (erase-buffer))
         (set-marker (process-mark (get-buffer-process (current-buffer))) (point-min))
         (clrhash opencode--text-blocks)
         (when (hash-table-p opencode--blocks) (clrhash opencode--blocks))
         (setq opencode--last-block-kind nil opencode--current-assistant nil)
         (opencode--adopt-info info)
         (opencode-session-load
          (lambda ()
            (goto-char (point-max))
            (when (and input (not (string-empty-p input)))
              (insert input))
            (unless (= line 1)
              (goto-char (point-min))
              (forward-line (1- line))))))))))

(defun opencode--render-user-prompt (text &optional message-id note)
  "Render a user prompt with TEXT for MESSAGE-ID, followed by NOTE when given.
Input still being typed is lifted out and put back afterwards."
  (let* ((process (get-buffer-process (current-buffer)))
         (pending (buffer-substring (opencode-pmark) (point-max)))
         (inhibit-read-only t))
    (delete-region (opencode-pmark) (point-max))
    (unless (opencode-prompt-shown-p)
      (opencode-open-block 'user)
      (opencode-show-prompt))
    (goto-char (point-max))
    (insert text)
    (let ((comint-input-sender #'opencode-highlight-input)
          (comint-input-filter-functions nil))
      (comint-send-input nil t))
    (when message-id
      (with-silent-modifications
        (put-text-property comint-last-input-start comint-last-input-end
                           'opencode-message message-id)))
    (when note
      (opencode-output (opencode-fontify-as (propertize (concat note "\n") 'font-lock-face 'opencode-notice)
                                            'verbatim)))
    (setq opencode--last-block-kind 'user)
    (goto-char (point-max))
    (insert pending)
    (set-marker (process-mark process) (- (point-max) (length pending)))))

(defun opencode--user-message-text (message)
  "Return the prompt text of user MESSAGE with its attachments named."
  (let-alist message
    (string-join
     (delq nil (append (list .text)
                       (mapcar (lambda (file) (format "`%s`" (or (alist-get 'name file) "file")))
                               .files)
                       (mapcar (lambda (agent) (concat "@" (alist-get 'name agent))) .agents)))
     " ")))

(defun opencode--render-assistant-content (message)
  "Render the content of projected assistant MESSAGE."
  (let ((id (alist-get 'id message))
        (text-ordinal 0)
        (reasoning-ordinal 0))
    (dolist (item (alist-get 'content message))
      (pcase (alist-get 'type item)
        ("text"
         (let ((text (string-trim (or (alist-get 'text item) ""))))
           (unless (string-empty-p text)
             (opencode-open-block 'text)
             (opencode-set-last-response-start (opencode-pmark))
             (setq opencode--table-scan-marker (copy-marker (opencode-pmark)))
             (opencode--output-message text id)
             (opencode-align-finished-tables t)))
         (cl-incf text-ordinal))
        ("reasoning"
         (let ((text (alist-get 'text item))
               (state (alist-get 'state item))
               (key (opencode-reasoning-key id reasoning-ordinal)))
           (if (opencode-narration-p text state)
               (progn (opencode-open-block 'text)
                      (opencode--output-message (string-trim text) id))
             (when (or (not (string-empty-p (or text "")))
                       (opencode-reasoning-hidden-kind text state))
               (opencode-open-block 'reasoning)
               (opencode-reasoning-end key text state))))
         (cl-incf reasoning-ordinal))
        ("tool"
         (opencode-open-block 'tool)
         (let ((overlay (opencode-tool-start (alist-get 'id item) (alist-get 'name item))))
           (apply #'opencode-block-update overlay :message id
                  (opencode-tool-data-from-content item))))))
    (opencode--note-tokens (alist-get 'tokens message))
    (when-let* ((error (alist-get 'error message))
                ;; The turn's idle marker says it was interrupted.
                ((not (equal (alist-get 'type error) "aborted"))))
      (opencode--output-error error))))

(defun opencode--output-message (text message-id)
  "Write TEXT for MESSAGE-ID, tagging it so commands find its message."
  (let ((start (opencode-pmark)))
    (opencode-output text)
    (with-silent-modifications
      (put-text-property start (opencode-pmark) 'opencode-message message-id))))

(defun opencode--output-error (error)
  "Write session ERROR, an alist with `type' and `message', in the error face.
An error already shown during the current turn is not repeated."
  (let ((text (or (alist-get 'message error) (format "%S" error))))
    (unless (member text opencode--turn-errors)
      (push text opencode--turn-errors)
      (opencode-open-block 'text)
      (opencode-output (opencode-fontify-as (propertize text 'font-lock-face 'error) 'verbatim))
      (opencode-output "\n"))))

(defun opencode--note-tokens (tokens)
  "Record TOKENS as the context use, unless they report no usage."
  (when tokens
    (let-alist tokens
      (let ((total (+ (or .input 0) (or .output 0) (or .reasoning 0)
                      (or .cache.read 0) (or .cache.write 0))))
        (when (> total 0)
          (setq opencode-session-tokens total)
          (force-mode-line-update))))))

(defun opencode-replay-message (message)
  "Render projected MESSAGE of the transcript."
  (pcase (alist-get 'type message)
    ("user"
     (opencode--render-user-prompt (opencode--user-message-text message) (alist-get 'id message)))
    ("assistant" (opencode--render-assistant-content message))
    ("synthetic"
     (opencode-open-block 'tool)
     (opencode-notice-block (or (alist-get 'description message) "synthetic message")
                            (alist-get 'text message) (alist-get 'id message)))
    ("system"
     (opencode-open-block 'tool)
     (opencode-notice-block (or (alist-get 'description message) "Instructions updated")
                            (alist-get 'text message) (alist-get 'id message)))
    ("skill"
     (opencode-open-block 'tool)
     (opencode-notice-block (format "Skill %s activated" (alist-get 'name message))
                            (alist-get 'text message) (alist-get 'id message)))
    ("shell"
     (opencode-open-block 'tool)
     (let ((overlay (opencode-tool-start (alist-get 'shellID message) "shell"
                                         `((command . ,(alist-get 'command message)))
                                         (if (equal (alist-get 'status message) "running")
                                             "running" "completed"))))
       (opencode-block-update overlay :output (opencode--shell-output (alist-get 'output message)))))
    ("compaction"
     (opencode-open-block 'tool)
     (opencode-notice-block (pcase (alist-get 'status message)
                              ("failed" "Compaction failed")
                              ("running" "Compacting…")
                              (_ "Conversation compacted"))
                            (or (alist-get 'summary message)
                                (alist-get 'message (alist-get 'error message)))
                            (alist-get 'id message)))
    ("agent-switched"
     (when opencode-show-switch-notices
       (opencode-output-notice (format "Switched to the %s agent" (alist-get 'agent message)))))
    ("model-switched"
     (when opencode-show-switch-notices
       (opencode-output-notice (format "Switched to %s" (opencode-model-label (alist-get 'model message))))))
    ("idle"
     (pcase (alist-get 'outcome message)
       ("failed" (when-let* ((error (alist-get 'error message))) (opencode--output-error error)))
       ("interrupted" (opencode-output-notice "Interrupted"))))))

(defun opencode--shell-output (output)
  "Return the text of shell OUTPUT, a string or an alist with `text'."
  (cond ((stringp output) output)
        ((consp output) (or (alist-get 'text output) (alist-get 'output output)))))

(defun opencode-replay-messages (messages)
  "Render MESSAGES, the projected transcript, oldest first."
  (let ((inhibit-redisplay t)
        (opencode--replaying t)
        (comint-output-filter-functions
         (remove #'comint-postoutput-scroll-to-bottom comint-output-filter-functions)))
    (dolist (message messages)
      (condition-case err
          (opencode-replay-message message)
        (error (message "OpenCode: cannot render message %s: %s"
                        (alist-get 'id message) (error-message-string err)))))))

(defun opencode-model-label (model)
  "Return a short display name of MODEL, a model reference."
  (let-alist model
    (let ((info (opencode-model-info model)))
      (concat (or (alist-get 'name info) .id "?")
              (if .variant (concat " " .variant) "")))))

;;;; Live events

(defmacro opencode--on-session-event (event &rest body)
  "Run BODY in the buffer of EVENT's session, with `data' bound to its data."
  (declare (indent 1))
  `(let ((data (alist-get 'data ,event)))
     (opencode-with-session-buffer (alist-get 'sessionID data)
       ,@body)))

(defun opencode--text-key (data)
  "Return the text block key of fragment event DATA."
  (format "%s/t/%d" (alist-get 'assistantMessageID data) (alist-get 'ordinal data)))

(defun opencode--event-step-started (event)
  "Handle `session.step.started' EVENT."
  (opencode--on-session-event event
    (setq opencode--current-assistant (alist-get 'assistantMessageID data)
          opencode-session-retry nil
          opencode-session-agent (or (alist-get 'agent data) opencode-session-agent)
          opencode-session-model (or (alist-get 'model data) opencode-session-model))
    (force-mode-line-update)))

(defun opencode--event-text-started (event)
  "Handle `session.text.started' EVENT."
  (opencode--on-session-event event
    (let ((key (opencode--text-key data)))
      (unless (gethash key opencode--text-blocks)
        (opencode-align-finished-tables t)
        (opencode-open-block 'text)
        (opencode-set-last-response-start (opencode-pmark))
        (setq opencode--table-scan-marker (copy-marker (opencode-pmark)))
        (puthash key (list :start (copy-marker (opencode-pmark)) :text "")
                 opencode--text-blocks)))))

(defun opencode--text-append (data delta)
  "Write DELTA of the text block of fragment DATA, when it is the open block."
  (when-let* ((block (gethash (opencode--text-key data) opencode--text-blocks)))
    (let* ((text (plist-get block :text))
           (delta (if (string-empty-p text) (string-trim-left delta) delta)))
      (unless (string-empty-p delta)
        (opencode--output-message delta (alist-get 'assistantMessageID data))
        (plist-put block :text (concat text delta))
        (when (string-search "\n" delta)
          (opencode-align-finished-tables))))))

(defun opencode--event-text-delta (event)
  "Handle `session.text.delta' EVENT."
  (opencode--on-session-event event
    (opencode--text-append data (alist-get 'delta data))))

(defun opencode--event-text-ended (event)
  "Handle `session.text.ended' EVENT: complete text missing from the deltas."
  (opencode--on-session-event event
    (unless (gethash (opencode--text-key data) opencode--text-blocks)
      (opencode--event-text-started event))
    (let* ((block (gethash (opencode--text-key data) opencode--text-blocks))
           (shown (plist-get block :text))
           (full (string-trim-left (or (alist-get 'text data) ""))))
      (cond
       ((string= shown full))
       ((string-prefix-p shown full)
        (opencode--text-append data (substring full (length shown))))
       (t (message "OpenCode: streamed text diverged from the final text; reloading")
          (opencode-session-reload)))
      (opencode-align-finished-tables t))))

(defvar-local opencode--reasoning-states nil
  "Hash table of reasoning keys to the state their start event carried.")

(defun opencode--event-reasoning (event)
  "Handle the `session.reasoning.*' EVENT.
A block is written once it has content: providers that keep their
reasoning encrypted stream empty blocks, which replay omits too."
  (opencode--on-session-event event
    (let ((key (opencode-reasoning-key (alist-get 'assistantMessageID data)
                                       (alist-get 'ordinal data))))
      (unless (hash-table-p opencode--reasoning-states)
        (setq opencode--reasoning-states (make-hash-table :test #'equal)))
      (pcase (alist-get 'type event)
        ("session.reasoning.started"
         (puthash key (alist-get 'state data) opencode--reasoning-states))
        ("session.reasoning.delta"
         (unless (opencode-block key)
           (opencode-align-finished-tables t)
           (opencode-open-block 'reasoning)
           (opencode-reasoning-start key "" (gethash key opencode--reasoning-states)))
         (opencode-reasoning-append key (alist-get 'delta data)))
        ("session.reasoning.ended"
         (let ((text (alist-get 'text data))
               (state (alist-get 'state data)))
           (remhash key opencode--reasoning-states)
           (when (or (opencode-block key)
                     (not (string-empty-p (or text "")))
                     (opencode-reasoning-hidden-kind text state))
             (unless (opencode-block key)
               (opencode-align-finished-tables t)
               (opencode-open-block (if (opencode-narration-p text state) 'text 'reasoning)))
             (opencode-reasoning-end key text state))))))))

(defun opencode--event-tool (event)
  "Handle the `session.tool.*' EVENT."
  (opencode--on-session-event event
    (let ((id (alist-get 'id data))
          (message (alist-get 'assistantMessageID data)))
      (pcase (alist-get 'type event)
        ("session.tool.input.started"
         (opencode-align-finished-tables t)
         (opencode-open-block 'tool)
         (opencode-block-update (opencode-tool-start id (alist-get 'name data)) :message message))
        ("session.tool.called"
         (unless (opencode-block id)
           (opencode-open-block 'tool)
           (opencode-tool-start id (or (alist-get 'name data) "tool")))
         (opencode-tool-update id :input (alist-get 'input data) :status "running" :message message))
        ("session.tool.progress"
         (when-let* ((overlay (opencode-block id)))
           (let* ((metadata (alist-get 'metadata data))
                  (known (plist-get (opencode-block-data overlay) :session-id))
                  (session (or (alist-get 'sessionID metadata) known)))
             (opencode-block-update overlay :metadata metadata :session-id session)
             ;; A subagent's session is first known here.
             (unless (equal session known)
               (run-hook-with-args 'opencode-blocks-attach-functions overlay)))))
        ("session.tool.success"
         (let ((metadata (alist-get 'metadata data)))
           (opencode-tool-update id :status "completed"
                                 :output (opencode-content-text (alist-get 'content data))
                                 :metadata metadata
                                 :answers (alist-get 'answers metadata)
                                 :session-id (or (alist-get 'sessionID metadata)
                                                 (when-let* ((overlay (opencode-block id)))
                                                   (plist-get (opencode-block-data overlay) :session-id))))))
        ("session.tool.failed"
         (opencode-tool-update id :status "error" :error (alist-get 'error data)
                               :output (opencode-content-text (alist-get 'content data))
                               :metadata (alist-get 'metadata data)))))))

(defun opencode--event-step-ended (event)
  "Handle `session.step.ended' EVENT."
  (opencode--on-session-event event
    (opencode-align-finished-tables t)
    (opencode--note-tokens (alist-get 'tokens data))
    (run-hook-with-args 'opencode-step-ended-functions data)))

(defun opencode--event-step-failed (event)
  "Handle `session.step.failed' EVENT."
  (opencode--on-session-event event
    (opencode-align-finished-tables t)
    (unless (or (run-hook-with-args-until-success 'opencode-step-failed-functions data)
                ;; The execution's interruption notice says it.
                (equal (alist-get 'type (alist-get 'error data)) "aborted"))
      (opencode--output-error (alist-get 'error data)))))

(defun opencode--event-execution (event)
  "Handle the `session.execution.*' EVENT: a turn starts or ends."
  (opencode--on-session-event event
    (pcase (alist-get 'type event)
      ("session.execution.started"
       (setq opencode-session-status 'busy
             opencode--turn-errors nil)
       ;; A turn another client started follows the idle prompt.
       (opencode-lift-prompt)
       (force-mode-line-update))
      (type
       (let ((outcome (pcase type
                        ("session.execution.succeeded" 'succeeded)
                        ("session.execution.failed" 'failed)
                        (_ 'interrupted))))
         (opencode-align-finished-tables t)
         (pcase outcome
           ('failed (opencode--output-error (alist-get 'error data)))
           ('interrupted
            (unless (equal (alist-get 'reason data) "shutdown")
              (opencode-output-notice "Interrupted"))))
         ;; A shutdown is resumed by the restarted service.
         (unless (equal (alist-get 'reason data) "shutdown")
           (setq opencode-session-status 'idle
                 opencode-session-retry nil
                 opencode--current-assistant nil)
           (opencode-call-preserving-viewport
            (current-buffer)
            (lambda ()
              (opencode-open-block 'user)
              (opencode-show-prompt)))
           (force-mode-line-update)
           (run-hook-with-args 'opencode-turn-ended-functions outcome)
           (opencode-notify-turn-ended outcome)
           (run-hook-with-args 'opencode-session-idle-functions outcome)))))))

(defvar-local opencode--pending-inbox nil
  "Hash table of inbox IDs to items enqueued by others, rendered once delivered.")

(defun opencode--render-inbox-item (id item)
  "Render inbox ITEM of inbox ID where it entered the conversation."
  (let ((payload (alist-get 'payload item)))
    (pcase (alist-get 'type item)
      ("user"
       (opencode--render-user-prompt (opencode--user-message-text payload) id))
      ("synthetic"
       (opencode-before-prompt
         (opencode-open-block 'tool)
         (opencode-notice-block (or (alist-get 'description payload) "synthetic message")
                                (alist-get 'text payload) id))))))

(defun opencode--event-inbox (event)
  "Handle the `session.inbox.*' EVENT.
Prompts sent from this buffer are already shown.  Other items, such as
prompts from other clients and background job notifications, are shown
once delivered: that is where the transcript places them."
  (opencode--on-session-event event
    (let ((id (alist-get 'inboxID data)))
      (unless (hash-table-p opencode--pending-inbox)
        (setq opencode--pending-inbox (make-hash-table :test #'equal)))
      (pcase (alist-get 'type event)
        ("session.inbox.enqueued"
         (unless (gethash id opencode--own-inputs)
           (puthash id (alist-get 'item data) opencode--pending-inbox)))
        ("session.inbox.delivered"
         (when-let* ((item (gethash id opencode--pending-inbox)))
           (remhash id opencode--pending-inbox)
           (opencode-call-preserving-viewport
            (current-buffer)
            (lambda () (opencode--render-inbox-item id item)))))
        ("session.inbox.cancelled"
         (remhash id opencode--pending-inbox)
         (when (gethash id opencode--own-inputs)
           (opencode-output-notice "A queued prompt was cancelled")))))))

(defun opencode--event-notice (event)
  "Handle transcript notices: synthetic messages, instruction updates, skills."
  (opencode--on-session-event event
    (let ((id (alist-get 'id event)))
      (opencode-before-prompt
        (pcase (alist-get 'type event)
          ("session.synthetic"
           (opencode-open-block 'tool)
           (opencode-notice-block (or (alist-get 'description data) "synthetic message")
                                  (alist-get 'text data) id))
          ("session.instructions.updated"
           (when-let* ((text (alist-get 'text data)))
             (opencode-open-block 'tool)
             (opencode-notice-block
              (format "Instructions updated: %s"
                      (mapconcat (lambda (entry) (symbol-name (car entry)))
                                 (alist-get 'delta data) ", "))
              text id)))
          ("session.skill.activated"
           (opencode-open-block 'tool)
           (opencode-notice-block (format "Skill %s activated" (alist-get 'name data))
                                  (alist-get 'text data) id)))))))

(defun opencode--event-shell (event)
  "Handle `session.shell.started' and `session.shell.ended' EVENT."
  (opencode--on-session-event event
    (let* ((shell (alist-get 'shell data))
           (id (alist-get 'id shell)))
      (pcase (alist-get 'type event)
        ("session.shell.started"
         (opencode-before-prompt
           (opencode-open-block 'tool)
           (opencode-tool-start id "shell" `((command . ,(alist-get 'command shell))) "running")))
        ("session.shell.ended"
         (opencode-tool-update id :status "completed"
                               :output (opencode--shell-output (alist-get 'output data))))))))

(defun opencode--event-compaction (event)
  "Handle the `session.compaction.*' EVENT."
  (opencode--on-session-event event
    (let ((key (concat (alist-get 'sessionID data) "/compaction")))
      (pcase (alist-get 'type event)
        ("session.compaction.started"
         (when (opencode-block key) (remhash key opencode--blocks))
         (opencode-before-prompt
           (opencode-open-block 'tool)
           (opencode-notice-block "Compacting…" "" key)))
        ("session.compaction.delta"
         (when-let* ((overlay (opencode-block key)))
           (opencode-block-update overlay :text (concat (plist-get (opencode-block-data overlay) :text)
                                                        (alist-get 'text data)))))
        ("session.compaction.ended"
         (when-let* ((overlay (opencode-block key)))
           (opencode-replace-summary
            overlay (opencode-fontify-as (propertize "Conversation compacted" 'font-lock-face 'opencode-notice)
                                         'verbatim))
           (opencode-block-update overlay :text (alist-get 'text data)))
         (opencode--note-tokens nil))
        ("session.compaction.failed"
         (when-let* ((overlay (opencode-block key)))
           (opencode-block-update overlay :error (alist-get 'error data))))))))

(defun opencode--event-session (event)
  "Handle session lifecycle EVENT: renames, selections, reverts, deletion."
  (let* ((data (alist-get 'data event))
         (id (alist-get 'sessionID data)))
    (pcase (alist-get 'type event)
      ("session.deleted"
       (run-hook-with-args 'opencode-session-deleted-functions id)
       (opencode-with-session-buffer id
         (opencode-output-notice "This session was deleted" 'error)
         (remhash id opencode-session-buffers)))
      (type
       (opencode-with-session-buffer id
         (pcase type
           ("session.renamed"
            (setf (alist-get 'title opencode-session-info) (alist-get 'title data))
            (rename-buffer (opencode-session-buffer-name opencode-session-info) t))
           ("session.agent.selected"
            (setq opencode-session-agent (alist-get 'agent data))
            (when opencode-show-switch-notices
              (opencode-output-notice (format "Switched to the %s agent" (alist-get 'agent data)))))
           ("session.model.selected"
            (setq opencode-session-model (alist-get 'model data))
            (when opencode-show-switch-notices
              (opencode-output-notice (format "Switched to %s" (opencode-model-label (alist-get 'model data))))))
           ("session.usage.updated"
            (setf (alist-get 'cost opencode-session-info) (alist-get 'cost data)
                  (alist-get 'tokens opencode-session-info) (alist-get 'tokens data)))
           ("session.retry.scheduled"
            (setq opencode-session-retry data)
            (opencode-notify-retry data))
           ("session.revert.staged"
            (setq opencode-session-revert (alist-get 'revert data))
            (opencode-output-notice
             (substitute-command-keys
              "A revert is staged: the next prompt drops everything after it, \\[opencode-revert-clear] cancels it")))
           ("session.revert.cleared"
            (setq opencode-session-revert nil)
            (opencode-output-notice "The staged revert was cancelled"))
           ("session.revert.committed"
            (setq opencode-session-revert nil)
            (opencode-session-reload))
           ("session.moved"
            (setq default-directory (opencode-normalize-directory
                                     (alist-get 'directory (alist-get 'location data))))
            (opencode-output-notice (format "Moved to %s" default-directory))))
         (force-mode-line-update))))))

(dolist (handler '(("session.step.started" . opencode--event-step-started)
                   ("session.step.ended" . opencode--event-step-ended)
                   ("session.step.failed" . opencode--event-step-failed)
                   ("session.text.started" . opencode--event-text-started)
                   ("session.text.delta" . opencode--event-text-delta)
                   ("session.text.ended" . opencode--event-text-ended)
                   ("session.reasoning.started" . opencode--event-reasoning)
                   ("session.reasoning.delta" . opencode--event-reasoning)
                   ("session.reasoning.ended" . opencode--event-reasoning)
                   ("session.tool.input.started" . opencode--event-tool)
                   ("session.tool.called" . opencode--event-tool)
                   ("session.tool.progress" . opencode--event-tool)
                   ("session.tool.success" . opencode--event-tool)
                   ("session.tool.failed" . opencode--event-tool)
                   ("session.execution.started" . opencode--event-execution)
                   ("session.execution.succeeded" . opencode--event-execution)
                   ("session.execution.failed" . opencode--event-execution)
                   ("session.execution.interrupted" . opencode--event-execution)
                   ("session.inbox.enqueued" . opencode--event-inbox)
                   ("session.inbox.delivered" . opencode--event-inbox)
                   ("session.inbox.cancelled" . opencode--event-inbox)
                   ("session.synthetic" . opencode--event-notice)
                   ("session.instructions.updated" . opencode--event-notice)
                   ("session.skill.activated" . opencode--event-notice)
                   ("session.shell.started" . opencode--event-shell)
                   ("session.shell.ended" . opencode--event-shell)
                   ("session.compaction.started" . opencode--event-compaction)
                   ("session.compaction.delta" . opencode--event-compaction)
                   ("session.compaction.ended" . opencode--event-compaction)
                   ("session.compaction.failed" . opencode--event-compaction)
                   ("session.deleted" . opencode--event-session)
                   ("session.renamed" . opencode--event-session)
                   ("session.agent.selected" . opencode--event-session)
                   ("session.model.selected" . opencode--event-session)
                   ("session.usage.updated" . opencode--event-session)
                   ("session.retry.scheduled" . opencode--event-session)
                   ("session.revert.staged" . opencode--event-session)
                   ("session.revert.cleared" . opencode--event-session)
                   ("session.revert.committed" . opencode--event-session)
                   ("session.moved" . opencode--event-session)))
  (opencode-events-register (car handler) (cdr handler)))

;;;; Reconnection

(defun opencode--resync-buffer ()
  "Reconcile this buffer with the server after the stream reopened."
  (let ((buffer (current-buffer))
        (known (alist-get 'updated (alist-get 'time opencode-session-info)))
        (was-busy (eq opencode-session-status 'busy)))
    (opencode-session-fetch
     opencode-session-id
     (lambda (info)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((changed (not (equal known (alist-get 'updated (alist-get 'time info))))))
             (opencode--adopt-info info)
             (if (or changed was-busy)
                 (opencode-session-reload)
               (run-hook-with-args 'opencode-session-resync-functions info)))))))))

(defun opencode--resync-all ()
  "Reconcile every open session buffer with the reconnected server."
  (maphash (lambda (_id buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (opencode--resync-buffer))))
           opencode-session-buffers))

(defvar opencode--stream-seen nil
  "Non-nil once a stream opened in this Emacs; later openings resynchronize.")

(defun opencode--on-connected ()
  "Resynchronize open sessions when a stream reopens."
  (if opencode--stream-seen
      (opencode--resync-all)
    (setq opencode--stream-seen t)))

(add-hook 'opencode-events-connected-hook #'opencode--on-connected)

;;;; Sending

(defun opencode-session-ready-p ()
  "Return non-nil when this session buffer is loaded and can send input."
  (and opencode-session-id (get-buffer-process (current-buffer))
       (opencode-events-connected-p)))

(defun opencode--prompt-files ()
  "Return the file attachments of the next prompt, as a vector."
  (vconcat (delq nil (mapcar (lambda (attachment) (plist-get attachment :file))
                             (reverse opencode--attachments)))))

(defun opencode--prompt-agents ()
  "Return the agent mentions of the next prompt, as a vector."
  (vconcat (delq nil (mapcar (lambda (attachment)
                               (when-let* ((name (plist-get attachment :agent)))
                                 `((name . ,name))))
                             (reverse opencode--attachments)))))

(cl-defun opencode-session-send (text &key files agents delivery metadata then else)
  "Send TEXT as a prompt of this session.
FILES and AGENTS are vectors of attachments; DELIVERY overrides
`opencode-prompt-delivery'; METADATA is an alist attached to the
prompt.  THEN runs with the admitted inbox item, ELSE with the error."
  (let ((id (opencode-generate-id "msg_")))
    (puthash id t opencode--own-inputs)
    (when opencode-session-model
      (setq opencode-last-model opencode-session-model))
    (opencode-request
     'post (format "/api/session/%s/prompt" opencode-session-id)
     :body `((id . ,id) (text . ,text)
             ,@(when (and files (> (length files) 0)) `((files . ,files)))
             ,@(when (and agents (> (length agents) 0)) `((agents . ,agents)))
             ,@(when metadata `((metadata . ,metadata)))
             (delivery . ,(symbol-name (or delivery opencode-prompt-delivery))))
     :then (lambda (response) (when then (funcall then (opencode-request-data response))))
     :else (lambda (error)
             (remhash id opencode--own-inputs)
             (if else (funcall else error)
               (opencode-output-notice (opencode-error-describe error) 'error))))
    id))

(cl-defun opencode-session-command (name arguments &key metadata then else)
  "Run slash command NAME with ARGUMENTS text in this session.
METADATA is attached to the command; THEN and ELSE as for `opencode-session-send'."
  (opencode-request
   'post (format "/api/session/%s/command" opencode-session-id)
   :body `((name . ,name) (text . ,arguments)
           ,@(when metadata `((metadata . ,metadata)))
           (delivery . ,(symbol-name opencode-prompt-delivery)))
   :then (lambda (_) (when then (funcall then nil)))
   :else (lambda (error)
           (if else (funcall else error)
             (opencode-output-notice (opencode-error-describe error) 'error)))))

(defun opencode--restore-input (text)
  "Put TEXT back at the prompt after a send was cancelled."
  (goto-char (point-max))
  (insert text))

(defun opencode--send-input (_process string)
  "Send STRING typed at the prompt: a prompt, a /command or a !shell command."
  (opencode-highlight-input)
  (opencode-output "\n")
  (let* ((string (string-trim-right string))
         (command (and (string-prefix-p "/" string)
                       (let ((space (string-search " " string)))
                         (list (substring string 1 space)
                               (if space (substring string (1+ space)) "")))))
         (kind (cond (command 'command) ((string-prefix-p "!" string) 'shell) (t 'prompt)))
         (files (opencode--prompt-files))
         (agents (opencode--prompt-agents))
         (temp-files opencode--temp-files))
    (if (eq 'cancel (run-hook-with-args-until-success
                     'opencode-before-send-functions
                     (list :kind kind :text string :name (car command))))
        (opencode--restore-input string)
      (setq opencode--attachments nil
            opencode--temp-files nil)
      (pcase kind
        ('command
         (opencode-session-command (car command) (cadr command)
                                   :then (lambda (_) (mapc #'delete-file temp-files))))
        ('shell
         (opencode-request 'post (format "/api/session/%s/shell" opencode-session-id)
                           :body `((command . ,(substring string 1)))))
        ('prompt
         (opencode-session-send string :files files :agents agents
                                :then (lambda (_) (mapc (lambda (file) (ignore-errors (delete-file file)))
                                                        temp-files))))))))

;;;; Attachments

(defun opencode--in-attachment-p ()
  "Return non-nil when point is inside an attachment label."
  (cl-loop for overlay in (overlays-at (point))
           thereis (overlay-get overlay 'opencode-attachment)))

(defun opencode--remove-attachment (overlay after _beg _end &optional _length)
  "Drop OVERLAY's attachment once its label is deleted, AFTER the change."
  (when (and after (not (eq this-command 'comint-send-input)))
    (let ((attachment (overlay-get overlay 'opencode-attachment))
          (start (overlay-start overlay))
          (end (overlay-end overlay)))
      (setq opencode--attachments (delq attachment opencode--attachments))
      (delete-overlay overlay)
      (let ((inhibit-modification-hooks t))
        (delete-region start end)))))

(defun opencode--insert-attachment (label attachment)
  "Insert an intangible LABEL standing for ATTACHMENT at point."
  (push attachment opencode--attachments)
  (let* ((start (point))
         (end (progn (insert "`" (propertize label 'cursor-intangible t)
                             (propertize "`" 'rear-nonsticky t))
                     (point)))
         (overlay (make-overlay start end)))
    (overlay-put overlay 'opencode-attachment attachment)
    (overlay-put overlay 'display (propertize label 'face 'markdown-ts-code))
    (overlay-put overlay 'modification-hooks '(opencode--remove-attachment))
    overlay))

(defmacro opencode--with-last-session (&rest body)
  "Run BODY at the prompt of the last selected session buffer."
  `(if (buffer-live-p opencode-last-session-buffer)
       (with-current-buffer opencode-last-session-buffer
         (unless (comint-after-pmark-p) (goto-char (point-max)))
         ,@body)
     (user-error "Open an OpenCode session first")))

(defun opencode--file-uri (file)
  "Return the file URI of FILE."
  (concat "file://" (if (string-prefix-p "/" (expand-file-name file)) "" "/")
          (url-hexify-string (expand-file-name file) url-path-allowed-chars)))

(defun opencode--attach-file (file &optional name)
  "Attach FILE to the next prompt, shown as NAME."
  (let ((name (or name (opencode-relative-path-for-display file))))
    (opencode--insert-attachment
     name (list :file `((uri . ,(opencode--file-uri file)) (name . ,name))))
    (insert " ")))

(defun opencode-add-file (&optional file directories)
  "Attach FILE, or a file of the current project, to the next prompt.
With prefix argument DIRECTORIES, offer project directories."
  (interactive (list nil current-prefix-arg))
  (let* ((project (project-current t))
         (root (project-root project))
         (files (project-files project))
         (file (or file
                   (project--read-file-name
                    project "Attach"
                    (if directories
                        (delete root (delete-dups (cons (concat root "./")
                                                        (mapcar #'file-name-directory files))))
                      files)
                    nil 'file-name-history))))
    (opencode--with-last-session (opencode--attach-file (expand-file-name file root)))))

(defun opencode-add-buffer-dwim ()
  "Attach a buffer's text to the next prompt.
From a session buffer prompt for the buffer, elsewhere attach the current one."
  (interactive)
  (let* ((name (if opencode-session-id (read-buffer "Attach buffer: " nil t) (buffer-name)))
         (text (with-current-buffer name (buffer-substring-no-properties (point-min) (point-max))))
         (uri (concat "data:text/plain;base64,"
                      (base64-encode-string (encode-coding-string text 'utf-8) t))))
    (opencode--with-last-session
     (opencode--insert-attachment name (list :file `((uri . ,uri) (name . ,name))))
     (insert " "))))

(defun opencode-add-region ()
  "Attach the active region to the next prompt."
  (interactive)
  (unless (use-region-p) (user-error "No active region"))
  (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
         (name (format "region of %s" (buffer-name)))
         (uri (concat "data:text/plain;base64,"
                      (base64-encode-string (encode-coding-string text 'utf-8) t))))
    (opencode--with-last-session
     (opencode--insert-attachment (format "region: %s" (truncate-string-to-width text 24 nil nil "…"))
                                  (list :file `((uri . ,uri) (name . ,name))))
     (insert " "))))

(defun opencode--yank-image (type data)
  "Attach pasted image DATA of MIME TYPE to the next prompt."
  (unless (comint-after-pmark-p)
    (user-error "Images can only be pasted at the prompt"))
  (let* ((mime (symbol-name type))
         (name (format "pasted %s" (or (cadr (split-string mime "[/;+]" t)) "image")))
         (uri (concat "data:" mime ";base64," (base64-encode-string data t))))
    (opencode--insert-attachment name (list :file `((uri . ,uri) (name . ,name))))
    (insert " ")))

(defun opencode-add-subagent ()
  "Insert a subagent mention at the prompt."
  (interactive)
  (call-interactively #'self-insert-command)
  (when (comint-after-pmark-p)
    (let ((subagents (seq-filter (lambda (agent)
                                   (and (equal (alist-get 'mode agent) "subagent")
                                        (not (alist-get 'hidden agent))))
                                 (opencode-catalog 'agent))))
      (when subagents
        (when-let* ((name (opencode-annotated-completion
                           "Subagent: "
                           (mapcar (lambda (agent)
                                     (list (alist-get 'id agent) (alist-get 'id agent)
                                           (alist-get 'description agent)))
                                   subagents))))
          (delete-char -1)
          (opencode--insert-attachment (concat "@" name) (list :agent name)))))))

(defun opencode-insert-slash-command ()
  "Insert a slash command at the start of the prompt, else insert a slash."
  (interactive)
  (if (and (comint-after-pmark-p) (= (point) (opencode-pmark)))
      (when-let* ((name (opencode-annotated-completion
                         "Command: "
                         (mapcar (lambda (command)
                                   (list (alist-get 'name command) (alist-get 'name command)
                                         (alist-get 'description command)))
                                 (opencode-catalog 'command)))))
        (insert "/" name " "))
    (call-interactively #'self-insert-command)))

;;;; Agent and model

(defun opencode--require-session ()
  "Signal an error outside a session buffer."
  (unless opencode-session-id (user-error "Not in an OpenCode session")))

(defun opencode-switch-agent (agent)
  "Switch this session to AGENT.
The session keeps its model unless AGENT pins one, as the TUI does."
  (opencode--require-session)
  (opencode-request 'post (format "/api/session/%s/agent" opencode-session-id)
                    :body `((agent . ,agent)))
  (when-let* ((pinned (alist-get 'model (opencode-agent-info agent))))
    (opencode-switch-model pinned)))

(defun opencode-switch-model (model)
  "Switch this session to MODEL, a model reference."
  (opencode--require-session)
  (setq opencode-last-model model)
  (opencode-request 'post (format "/api/session/%s/model" opencode-session-id)
                    :body `((model . ,model))))

(defun opencode-cycle-agent ()
  "Switch this session to the next primary agent."
  (interactive)
  (opencode--require-session)
  (let* ((agents (mapcar (lambda (agent) (alist-get 'id agent)) (opencode-primary-agents)))
         (next (or (cadr (member opencode-session-agent agents)) (car agents))))
    (if next
        (opencode-switch-agent next)
      (opencode-catalog-fetch 'agent (lambda (_) (message "Agents loaded; try again"))))))

(defun opencode-select-agent ()
  "Choose this session's primary agent."
  (interactive)
  (opencode--require-session)
  (opencode-catalog-fetch
   'agent
   (lambda (_)
     (when-let* ((agent (opencode-annotated-completion
                         "Agent: "
                         (mapcar (lambda (agent)
                                   (list (alist-get 'id agent) (alist-get 'id agent)
                                         (alist-get 'description agent)))
                                 (opencode-primary-agents)))))
       (opencode-switch-agent agent)))))

(defun opencode-select-model ()
  "Choose this session's model."
  (interactive)
  (opencode--require-session)
  (let ((buffer (current-buffer)))
    (opencode-catalog-fetch
     'model
     (lambda (models)
       (with-current-buffer buffer
         (when-let* ((model (opencode-annotated-completion
                             "Model: "
                             (mapcar (lambda (entry)
                                       (list (format "%s/%s" (alist-get 'providerID entry)
                                                     (alist-get 'id entry))
                                             entry (alist-get 'name entry)))
                                     models))))
           (let* ((variant (alist-get 'variant opencode-session-model))
                  (offered (mapcar (lambda (v) (alist-get 'id v)) (alist-get 'variants model))))
             (opencode-switch-model
              `((id . ,(alist-get 'id model)) (providerID . ,(alist-get 'providerID model))
                ,@(when (member variant offered) `((variant . ,variant))))))))))))

(defun opencode-select-variant ()
  "Choose the variant, such as the reasoning effort, of this session's model."
  (interactive)
  (opencode--require-session)
  (let ((buffer (current-buffer)))
    (opencode-catalog-fetch
     'model
     (lambda (_)
       (with-current-buffer buffer
         (let* ((info (opencode-model-info opencode-session-model))
                (variants (alist-get 'variants info)))
           (if (null variants)
               (message "This model has no variants")
             (let ((variant (completing-read
                             "Variant: "
                             (cons "default" (mapcar (lambda (v) (alist-get 'id v)) variants))
                             nil t)))
               (opencode-switch-model
                `((id . ,(alist-get 'id opencode-session-model))
                  (providerID . ,(alist-get 'providerID opencode-session-model))
                  ,@(unless (equal variant "default") `((variant . ,variant)))))))))))))

;;;; Session commands

(defun opencode-interrupt ()
  "Interrupt the running turn of this session."
  (interactive)
  (opencode--require-session)
  (opencode-request 'post (format "/api/session/%s/interrupt" opencode-session-id)
                    :then (lambda (response)
                            (unless (alist-get 'interrupted response)
                              (message "Nothing to interrupt")))))

(defun opencode-rename-session (&optional id title)
  "Rename session ID, this one by default, to TITLE."
  (interactive)
  (let* ((id (or id opencode-session-id (user-error "Not in an OpenCode session")))
         (title (or title (read-string "Title: " (alist-get 'title opencode-session-info)))))
    (opencode-request 'patch (concat "/api/session/" id) :body `((title . ,title)))))

(defun opencode-delete-session (&optional id)
  "Delete session ID, this one by default, after confirmation."
  (interactive)
  (let ((id (or id opencode-session-id (user-error "Not in an OpenCode session")))
        (buffer (and (null id) (current-buffer))))
    (when (yes-or-no-p "Delete this OpenCode session for good? ")
      (opencode-request 'delete (concat "/api/session/" id)
                        :then (lambda (_)
                                (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(defun opencode-compact-session ()
  "Compact this session's conversation."
  (interactive)
  (opencode--require-session)
  (opencode-request 'post (format "/api/session/%s/compact" opencode-session-id)
                    :body (opencode-empty-object)))

(defun opencode-message-at (position)
  "Return the ID of the message rendered at POSITION, or nil."
  (get-text-property position 'opencode-message))

(defun opencode--message-boundary (position)
  "Return the message after which a fork or revert at POSITION cuts, or nil.
In a prompt the cut keeps the prompt and drops its answer; in an answer
it keeps the answer; at the live prompt it keeps everything."
  (let ((here (opencode-message-at position))
        (prompt (get-text-property position 'opencode-prompt)))
    (cond
     ((>= position (opencode-pmark)) nil)
     ((and here prompt)
      ;; The first message rendered after the prompt starts its answer.
      (let ((next (next-single-property-change position 'opencode-message nil (opencode-pmark))))
        (while (and next (< next (opencode-pmark))
                    (or (null (opencode-message-at next)) (equal (opencode-message-at next) here)))
          (setq next (next-single-property-change next 'opencode-message nil (opencode-pmark))))
        (and next (< next (opencode-pmark)) (opencode-message-at next))))
     (t
      ;; The next prompt after point starts the part to drop.
      (let ((pos position) found)
        (while (and (not found) (setq pos (next-single-property-change pos 'opencode-prompt nil (opencode-pmark)))
                    (< pos (opencode-pmark)))
          (when (get-text-property pos 'opencode-prompt)
            (setq found (opencode-message-at pos))))
        found)))))

(defun opencode-fork-session ()
  "Fork this session through the content at point into a new session."
  (interactive)
  (opencode--require-session)
  (let ((before (opencode--message-boundary (point))))
    (opencode-request 'post (format "/api/session/%s/fork" opencode-session-id)
                      :body (if before `((before . ,before)) (opencode-empty-object))
                      :then (lambda (response)
                              (opencode-session-open (opencode-request-data response)
                                                     :other-window nil)))))

(defun opencode-revert-to-point ()
  "Stage a revert dropping everything after the content at point.
The files changed since are restored; the next prompt commits the revert."
  (interactive)
  (opencode--require-session)
  (let ((boundary (or (opencode--message-boundary (point))
                      (user-error "Nothing to revert after point"))))
    (opencode-request 'post (format "/api/session/%s/revert/stage" opencode-session-id)
                      :body `((messageID . ,boundary) (files . t)))))

(defun opencode-revert-clear ()
  "Cancel the staged revert of this session."
  (interactive)
  (opencode--require-session)
  (opencode-request 'delete (format "/api/session/%s/revert" opencode-session-id)))

(defun opencode-open-parent ()
  "Open the parent of this session."
  (interactive)
  (opencode--require-session)
  (if-let* ((parent (alist-get 'parentID opencode-session-info)))
      (opencode-session-open-by-id parent)
    (user-error "This session has no parent")))

(defun opencode--select-session (prompt sessions)
  "Open one of SESSIONS chosen with PROMPT, newest first."
  (if (null sessions)
      (message "No sessions")
    (when-let* ((info (opencode-annotated-completion
                       prompt
                       (mapcar (lambda (info)
                                 (let ((updated (alist-get 'updated (alist-get 'time info))))
                                   (list (or (alist-get 'title info) (alist-get 'id info)) info
                                         (opencode-format-time-ago (opencode-time-ago updated))
                                         (- (or updated 0)))))
                               sessions))))
      (opencode-session-open info :other-window nil))))

(defun opencode-select-child-session ()
  "Open a subagent session of this session."
  (interactive)
  (opencode--require-session)
  (opencode-request 'get "/api/session" :query `((parentID . ,opencode-session-id))
                    :then (lambda (response)
                            (opencode--select-session "Subagent: " (opencode-request-data response)))))

(defun opencode-scroll-to-last-response-start ()
  "Scroll to the start of the latest assistant text."
  (interactive)
  (unless opencode-last-response-start (user-error "No response yet"))
  (goto-char opencode-last-response-start)
  (recenter-top-bottom 0))

(defun opencode-yank-code-block ()
  "Copy the code of the fenced block at point."
  (interactive)
  (let* ((node (treesit-parent-until (treesit-node-at (point) 'markdown) "fenced_code_block" t))
         (content (and node (treesit-search-subtree node "code_fence_content"))))
    (unless node (user-error "Not in a fenced code block"))
    (if content
        (copy-region-as-kill (treesit-node-start content) (treesit-node-end content))
      (kill-new ""))))

(defun opencode--selection-changed (&optional _frame)
  "Remember the selected session buffer and forget its idle alert."
  (when opencode-session-id
    (setq opencode-alerted-sessions
          (seq-remove (lambda (info) (equal (alist-get 'id info) opencode-session-id))
                      opencode-alerted-sessions)
          opencode-last-session-buffer (current-buffer))))

(add-hook 'window-selection-change-functions #'opencode--selection-changed)

;;;; Navigating sessions

(defun opencode-select-open-session ()
  "Switch to an open session buffer."
  (interactive)
  (switch-to-buffer
   (read-buffer "Session: " nil t
                (lambda (entry)
                  (let ((buffer (if (consp entry) (cdr entry) (get-buffer entry))))
                    (and buffer (buffer-local-value 'opencode-session-id buffer)))))))

(defun opencode-visit-last-idle ()
  "Open the session that most recently finished while not displayed."
  (interactive)
  (if opencode-alerted-sessions
      (opencode-session-open (pop opencode-alerted-sessions) :other-window nil)
    (message "No idle sessions")))

(defun opencode-select-idle ()
  "Open a session that finished while not displayed."
  (interactive)
  (opencode--select-session "Idle session: " opencode-alerted-sessions))

(declare-function consult-line-multi "consult")

(defun opencode-consult-sessions ()
  "Search every open session buffer with `consult-line-multi'."
  (interactive)
  (unless (require 'consult nil t) (user-error "This command needs consult"))
  (consult-line-multi (list :predicate (lambda (buffer)
                                         (buffer-local-value 'opencode-session-id buffer)))))

(provide 'opencode-session)
;;; opencode-session.el ends here
