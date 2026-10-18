;;; opencode-permission.el --- Permission requests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  nineluj
;; Copyright (C) 2026  Valentin Leon

;; Author: nineluj <code@nineluj.com>
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

;; A permission request names the tool call that asked, so it marks that
;; call's block in the attention face; the transient shows the whole
;; command, patch or argument list.  A request whose block has not arrived
;; yet prints a fallback line that the block replaces.  The transient
;; accepts once (o), always (a, showing the patterns saved), rejects (n)
;; or hands the task off to another primary agent (h): the session
;; switches agent, a synthetic message tells the model, and the pending
;; call is accepted once, so the turn continues with its context intact.
;;
;; A shell request's resources are the command nodes rules match, without
;; operators or redirections; its title shows the command as written, from
;; the tool call, by its first line, and the transient shows all of it.
;; My fork reports the rule behind each resource; the transient lists
;; them, and the pending block names the rules instead of the command.

;;; Code:

(require 'opencode-blocks)
(require 'opencode-common)
(require 'opencode-events)
(require 'opencode-http)
(require 'opencode-requests)
(require 'opencode-session)
(require 'transient)

(defvar-local opencode-pending-permissions nil
  "Permission requests of this session awaiting an answer, oldest first.
Each is a plist (:id :action :resources :save :metadata :source :reasons
:title :detail :marker).")

(defvar opencode--active-permission nil
  "Plist (:buffer :id) of the permission shown or suspended in the transient.")

;;;; Titles

(defun opencode--permission-path (path)
  "Return PATH for display: relative inside the session directory, else abbreviated."
  (when (and path (not (string-empty-p path)))
    (let* ((expanded (expand-file-name path))
           (relative (file-relative-name expanded default-directory)))
      (if (string-prefix-p ".." relative)
          (abbreviate-file-name expanded)
        (if (string= relative "") "." relative)))))

(defun opencode-permission-command-summary (command)
  "Return COMMAND's first line, with an ellipsis when more lines follow."
  (let ((lines (split-string (string-trim command) "\n")))
    (if (cdr lines)
        (concat (string-trim-right (car lines)) " …")
      (car lines))))

(defun opencode--permission-tool-input (source)
  "Return the input of the tool call named by permission SOURCE, if rendered."
  (when-let* ((id (alist-get 'id source))
              (overlay (opencode-block id)))
    (plist-get (opencode-block-data overlay) :input)))

(defun opencode-permission-title (action resources metadata input)
  "Return the title of a permission request for ACTION on RESOURCES.
METADATA comes with the request; INPUT is the asking tool call's input."
  (let ((first (car resources)))
    (let-alist metadata
      (pcase action
        ("shell"
         (let ((command (or (alist-get 'command input) .command)))
           (format "Shell command: %s"
                   (if (and (stringp command) (not (string-empty-p command)))
                       (opencode-permission-command-summary command)
                     (string-join resources " ")))))
        ("edit" (format "Edit %s" (opencode--permission-path (or (alist-get 'path input) first ""))))
        ("read" (format "Read %s" (opencode--permission-path (or (alist-get 'path input) first ""))))
        ((or "glob" "grep") (format "%s %S" (capitalize action) (or (alist-get 'pattern input) first "")))
        ("webfetch" (format "Fetch %s" (or (alist-get 'url input) first "")))
        ("websearch" (format "Web search %S" (or (alist-get 'query input) first "")))
        ("subagent" (format "Run the %s subagent" (or (alist-get 'agent input) first "?")))
        ("external_directory"
         (format "Access the external directory %s"
                 (opencode--permission-path
                  (or .parentDir .path
                      (and first (if (string-match-p "\\*" first) (file-name-directory first) first))))))
        ("doom_loop" "Continue after repeated failures")
        (_ (format "Call %s%s" action
                   (if resources (format " on %s" (string-join resources ", ")) "")))))))

(defun opencode--permission-detail (action input)
  "Return the full text of an ACTION request with tool INPUT, or nil.
Nil means its title already shows everything."
  (let ((command (alist-get 'command input)))
    (when (and (equal action "shell") (stringp command)
               (string-search "\n" (string-trim command)))
      (concat "Shell command: " command))))

;;;; Reasons

(defun opencode--reason-line (text needle)
  "Return the numbered line of TEXT holding NEEDLE, or nil."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (when (search-forward needle nil t)
        (format "line %d: %s" (line-number-at-pos)
                (truncate-string-to-width
                 (string-trim (buffer-substring (line-beginning-position) (line-end-position)))
                 120 nil nil "…"))))))

(defun opencode--reason-resource (resource rule)
  "Return the lines showing RESOURCE, which RULE asks for.
When the longest literal part of RULE's wildcard pattern lies past the
first line, the line holding it is shown with its number, which locates
the match inside a long script."
  (let* ((summary (opencode-permission-command-summary resource))
         (needle (car (seq-sort-by #'length #'>
                                   (split-string (or (alist-get 'resource rule) "") "[*?]" t))))
         (line (and needle (not (string-search needle summary))
                    (opencode--reason-line resource needle))))
    (concat "\n    " summary
            (when line (concat "\n      " (propertize line 'face 'shadow))))))

(defun opencode-permission-reasons-summary (reasons)
  "Return a one-line summary of the rules asking for REASONS, or nil."
  (when reasons
    (let* ((rules (seq-uniq (delq nil (mapcar (lambda (reason)
                                                (alist-get 'resource (alist-get 'rule reason)))
                                              reasons))))
           (default (seq-some (lambda (reason) (null (alist-get 'rule reason))) reasons))
           (named (when rules
                    (format "asked by %s %s" (if (cdr rules) "rules" "rule")
                            (mapconcat (lambda (rule) (format "\"%s\"" rule)) rules ", ")))))
      (cond ((and named default) (concat named "; no rule allows the rest"))
            (named)
            (t "asked: no rule allows it")))))

(defun opencode-permission-reasons-text (reasons)
  "Return the text explaining why a request with REASONS asks, or nil.
Resources are grouped by the rule asking for them; one contained in
another of its group, such as a command without its redirections, is
left out."
  (when reasons
    (let (groups)
      (dolist (reason reasons)
        (push (alist-get 'resource reason)
              (alist-get (alist-get 'rule reason) groups nil nil #'equal)))
      (concat
       "\n\nAsked because:"
       (mapconcat
        (lambda (group)
          (let* ((rule (car group))
                 (resources (reverse (cdr group)))
                 (shown (seq-remove (lambda (resource)
                                      (seq-some (lambda (other)
                                                  (and (not (equal other resource))
                                                       (string-search resource other)))
                                                resources))
                                    resources)))
            (concat "\n  "
                    (if rule
                        (format "%s rule \"%s\" asks for:"
                                (alist-get 'action rule) (alist-get 'resource rule))
                      "no rule allows:")
                    (mapconcat (lambda (resource) (opencode--reason-resource resource rule))
                               shown ""))))
        (reverse groups) "")))))

;;;; Queue

(defun opencode--permission-find (id)
  "Return the queued permission ID of this buffer."
  (seq-find (lambda (permission) (equal (plist-get permission :id) id))
            opencode-pending-permissions))

(defun opencode--permission-block (permission)
  "Return the block of the tool call that asked PERMISSION, if rendered."
  (when-let* ((id (alist-get 'id (plist-get permission :source))))
    (opencode-block id)))

(defun opencode--permission-mark-block (permission)
  "Mark PERMISSION's tool block as waiting; return the block.
The transient shows the full command, so the block stays collapsed."
  (when-let* ((overlay (opencode--permission-block permission)))
    (opencode-block-update overlay :waiting t :waiting-kind 'permission
                           :waiting-description
                           (opencode-permission-reasons-summary (plist-get permission :reasons)))
    (overlay-put overlay 'help-echo
                 (substitute-command-keys
                  (format "Permission pending: %s.  \\[opencode-respond] responds."
                          (plist-get permission :title))))
    overlay))

(defun opencode--permission-clear-block (permission)
  "Clear the waiting mark of PERMISSION's tool block."
  (when-let* ((overlay (opencode--permission-block permission)))
    (when (eq (plist-get (opencode-block-data overlay) :waiting-kind) 'permission)
      (opencode-block-update overlay :waiting nil :waiting-kind nil :waiting-description nil)
      (overlay-put overlay 'help-echo nil))))

(defun opencode--permission-fallback (permission)
  "Write PERMISSION's fallback line, for a request whose block is not shown."
  (when (get-buffer-process (current-buffer))
    (opencode-call-preserving-viewport
     (current-buffer)
     (lambda ()
       (opencode-before-prompt
         (opencode-open-block 'notice)
         (opencode-output (opencode-fontify-as
                           (propertize (concat "? " (plist-get permission :title))
                                       'font-lock-face 'opencode-attention)
                           'verbatim))
         (plist-put permission :marker (copy-marker (opencode-pmark)))
         (opencode-output "\n"))))))

(defun opencode--permission-remove-fallback (permission)
  "Delete PERMISSION's fallback line when it is still exactly as written."
  (let ((marker (plist-get permission :marker))
        (text (concat "? " (plist-get permission :title))))
    (when (and (markerp marker) (eq (marker-buffer marker) (current-buffer)))
      (let ((start (- marker (length text)))
            (end (min (point-max) (1+ marker))))
        (when (and (>= start (point-min))
                   (equal (buffer-substring-no-properties start marker) text))
          (let ((inhibit-read-only t))
            (delete-region start end))))
      (set-marker marker nil)
      (plist-put permission :marker nil))))

(defun opencode--permission-label (permission label face)
  "Append LABEL in FACE to PERMISSION's fallback line, if it has one."
  (when-let* ((marker (plist-get permission :marker))
              ((marker-buffer marker)))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char marker)
        (insert (propertize label 'face face 'font-lock-face face))))
    (set-marker marker nil)))

(defun opencode-permission-queue (request)
  "Queue permission REQUEST, an alist from the server, in this session buffer."
  (let-alist request
    (unless (opencode--permission-find .id)
      (let* ((input (opencode--permission-tool-input .source))
             (permission (list :id .id :action .action :resources .resources :save .save
                               :metadata .metadata :source .source :reasons .reasons
                               :title (opencode-permission-title .action .resources .metadata input)
                               :detail (opencode--permission-detail .action input))))
        (setq opencode-pending-permissions (append opencode-pending-permissions (list permission)))
        (unless (opencode--permission-mark-block permission)
          (opencode--permission-fallback permission))
        (force-mode-line-update)
        (opencode-request-state-changed)
        (if (opencode-buffer-active-p (current-buffer))
            (message (substitute-command-keys
                      "OpenCode permission pending; \\[opencode-respond] answers it"))
          (opencode-notify-session "OpenCode permission request" (plist-get permission :title)
                                   'permission)
          (opencode-alert-session))))))

(defun opencode--permission-attach (overlay)
  "Attach a queued permission to newly inserted tool block OVERLAY."
  (let ((id (plist-get (opencode-block-data overlay) :key)))
    (when-let* ((permission (seq-find (lambda (permission)
                                        (equal (alist-get 'id (plist-get permission :source)) id))
                                      opencode-pending-permissions)))
      (opencode--permission-remove-fallback permission)
      (plist-put permission :title
                 (opencode-permission-title (plist-get permission :action)
                                            (plist-get permission :resources)
                                            (plist-get permission :metadata)
                                            (plist-get (opencode-block-data overlay) :input)))
      (opencode--permission-mark-block permission))))

(defun opencode--permission-forget (permission &optional label face)
  "Drop PERMISSION from the queue, labelling its fallback LABEL in FACE."
  (setq opencode-pending-permissions (delq permission opencode-pending-permissions))
  (opencode--permission-clear-block permission)
  (when label (opencode--permission-label permission label face))
  (when (and (equal (plist-get opencode--active-permission :id) (plist-get permission :id))
             (eq (plist-get opencode--active-permission :buffer) (current-buffer)))
    (opencode--permission-close-transient))
  (force-mode-line-update)
  (opencode-request-state-changed))

;;;; Replying

(defun opencode-permission-reply (permission decision &optional message)
  "Answer PERMISSION with DECISION, `once', `always' or `reject', and MESSAGE."
  (let ((buffer (current-buffer)))
    (opencode--permission-forget
     permission
     (pcase decision
       ('once " [accepted]") ('always " [accepted always]") (_ " [rejected]"))
     (if (eq decision 'reject) 'error 'success))
    (opencode-request
     'post (format "/api/session/%s/permission/%s/reply" opencode-session-id (plist-get permission :id))
     :body `((decision . ,(symbol-name decision)) ,@(when message `((message . ,message))))
     :else (lambda (error)
             (unless (eql (opencode-error-status error) 404)
               (with-current-buffer buffer
                 (setq opencode-pending-permissions
                       (cons permission opencode-pending-permissions))
                 (opencode--permission-mark-block permission)
                 (force-mode-line-update))
               (message "OpenCode permission reply failed (%s); %s retries"
                        (opencode-error-describe error)
                        (substitute-command-keys "\\[opencode-respond]")))))))

;;;; Hand-off

(defun opencode--handoff-agents ()
  "Return the other primary agents, the build agent first."
  (let ((agents (seq-remove (lambda (agent) (equal (alist-get 'id agent) opencode-session-agent))
                            (opencode-primary-agents))))
    (append (seq-filter (lambda (agent) (equal (alist-get 'id agent) "build")) agents)
            (seq-remove (lambda (agent) (equal (alist-get 'id agent) "build")) agents))))

(defun opencode-permission-hand-off (permission agent)
  "Continue the turn with AGENT, then accept PERMISSION once."
  (let ((buffer (current-buffer))
        (session opencode-session-id))
    (opencode--permission-forget permission (format " [handed off to %s]" agent) 'success)
    (cl-flet ((fail (error)
                (with-current-buffer buffer
                  (setq opencode-pending-permissions (cons permission opencode-pending-permissions))
                  (opencode--permission-mark-block permission)
                  (message "Hand-off failed (%s); the permission is still pending"
                           (opencode-error-describe error)))))
      (opencode-request
       'post (format "/api/session/%s/agent" session) :body `((agent . ,agent))
       :else #'fail
       :then (lambda (_)
               (opencode-request
                'post (format "/api/session/%s/synthetic" session)
                :body `((text . ,(format "The user approved handing off the current task to the %s agent.  Continue the task from the preceding research and complete the requested work." agent))
                        (description . ,(format "Handed off to %s" agent))
                        (delivery . "steer"))
                :else #'fail
                :then (lambda (_)
                        (opencode-request
                         'post (format "/api/session/%s/permission/%s/reply" session (plist-get permission :id))
                         :body '((decision . "once")))
                        (message "Handed off to %s; the current tool continues" agent))))))))

;;;; Transient

(defvar opencode--permission-transient-state nil
  "Plist (:buffer :permission) the permission transient acts on.")

(defun opencode--permission-transient-act (function &rest arguments)
  "Call FUNCTION with the transient's permission and ARGUMENTS in its buffer."
  (let* ((state opencode--permission-transient-state)
         (buffer (plist-get state :buffer)))
    (setq opencode--active-permission nil
          opencode--permission-transient-state nil)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (apply function (plist-get state :permission) arguments)))))

(transient-define-suffix opencode-permission-accept ()
  "Accept the request once."
  :transient nil
  (interactive)
  (opencode--permission-transient-act #'opencode-permission-reply 'once))

(transient-define-suffix opencode-permission-accept-always ()
  "Accept the request and save its patterns."
  :transient nil
  :description (lambda ()
                 (format "Accept always (%s)"
                         (string-join (or (plist-get (plist-get opencode--permission-transient-state :permission) :save)
                                          '("this request"))
                                      ", ")))
  (interactive)
  (opencode--permission-transient-act #'opencode-permission-reply 'always))

(transient-define-suffix opencode-permission-reject ()
  "Reject the request."
  :transient nil
  (interactive)
  (opencode--permission-transient-act #'opencode-permission-reply 'reject))

(transient-define-suffix opencode-permission-hand-off-suffix ()
  "Hand the task off to another primary agent."
  :transient nil
  (interactive)
  (let* ((buffer (plist-get opencode--permission-transient-state :buffer))
         (agents (with-current-buffer buffer (opencode--handoff-agents)))
         (names (mapcar (lambda (agent) (alist-get 'id agent)) agents)))
    (unless names (user-error "No other primary agent"))
    (let ((agent (completing-read (format-prompt "Hand off to" (car names)) names nil t nil nil (car names))))
      (opencode--permission-transient-act #'opencode-permission-hand-off agent))))

(defun opencode--permission-description ()
  "Return the heading of the permission transient."
  (let ((permission (plist-get opencode--permission-transient-state :permission)))
    (concat (or (plist-get permission :detail) (plist-get permission :title) "Permission")
            (opencode-permission-reasons-text (plist-get permission :reasons)))))

(transient-define-prefix opencode-permission-transient ()
  "Answer an OpenCode permission request."
  [:description opencode--permission-description
   ("o" "Accept once" opencode-permission-accept)
   ("a" opencode-permission-accept-always)
   ("n" "Reject" opencode-permission-reject)
   ("h" "Hand off to another agent" opencode-permission-hand-off-suffix)])

(opencode-install-hidden-navigation 'opencode-permission-transient opencode-transient-scroll-suffixes)

(defun opencode--permission-on-exit ()
  "Keep a dismissed request pending; only a suspended one stays active."
  (remove-hook 'transient-exit-hook #'opencode--permission-on-exit)
  (if (opencode-transient-suspended-p 'opencode-permission-transient)
      (message (substitute-command-keys "OpenCode permission suspended; \\[opencode-respond] resumes it"))
    (when opencode--active-permission
      (setq opencode--active-permission nil)
      (message (substitute-command-keys "OpenCode permission deferred; \\[opencode-respond] resumes it")))))

(defun opencode--permission-close-transient ()
  "Close the permission transient because its request was resolved elsewhere."
  (setq opencode--active-permission nil
        opencode--permission-transient-state nil)
  (cond
   ((opencode-transient-displayed-p 'opencode-permission-transient)
    (transient--emergency-exit))
   ((opencode-transient-suspended-p 'opencode-permission-transient)
    (pop transient--stack))))

(defun opencode-respond-permission (&optional permission)
  "Open the transient of PERMISSION, else the oldest pending one.
A transient suspended from this buffer is resumed instead."
  (interactive)
  (cond
   ((and (opencode-transient-suspended-p 'opencode-permission-transient)
         (eq (plist-get opencode--active-permission :buffer) (current-buffer))
         (or (null permission)
             (equal (plist-get permission :id) (plist-get opencode--active-permission :id))))
    (add-hook 'transient-exit-hook #'opencode--permission-on-exit)
    (transient-resume))
   ((bound-and-true-p transient--prefix)
    (user-error "Finish or suspend the active transient first"))
   (t
    (let ((permission (or permission (car opencode-pending-permissions)
                          (user-error "No pending OpenCode permission"))))
      (when (opencode-transient-suspended-p 'opencode-permission-transient)
        (pop transient--stack))
      (setq opencode--active-permission (list :buffer (current-buffer) :id (plist-get permission :id))
            opencode--permission-transient-state (list :buffer (current-buffer) :permission permission))
      (add-hook 'transient-exit-hook #'opencode--permission-on-exit)
      (opencode-permission-transient)))))

(defun opencode--permission-at-point ()
  "Return the queued permission of the tool block at point, if any."
  (when-let* ((overlay (opencode-block-at (point)))
              ((eq (plist-get (opencode-block-data overlay) :waiting-kind) 'permission)))
    (let ((id (plist-get (opencode-block-data overlay) :key)))
      (seq-find (lambda (permission) (equal (alist-get 'id (plist-get permission :source)) id))
                opencode-pending-permissions))))

(defun opencode--permission-provider ()
  "Describe this buffer's pending permissions for `opencode-respond'."
  (list :pending opencode-pending-permissions
        :at-point (when-let* ((permission (opencode--permission-at-point)))
                    (lambda () (opencode-respond-permission permission)))
        :respond #'opencode-respond-permission))

(add-to-list 'opencode-request-providers #'opencode--permission-provider)
(add-hook 'opencode-blocks-attach-functions #'opencode--permission-attach)

;;;; Events and synchronization

(defun opencode--permission-asked (event)
  "Queue the permission request of EVENT in its session's buffer.
A session without a buffer gets one in the background, so the request has
somewhere to wait."
  (let* ((request (alist-get 'data event))
         (session (alist-get 'sessionID request)))
    (if-let* ((buffer (gethash session opencode-session-buffers))
              ((buffer-live-p buffer)))
        (with-current-buffer buffer (opencode-permission-queue request))
      (opencode-session-open-by-id
       session :no-display t
       :callback (lambda (_info) (opencode-permission-queue request))))))

(defun opencode--permission-replied (event)
  "Forget the permission EVENT says was answered, by any client."
  (let ((data (alist-get 'data event)))
    (opencode-with-session-buffer (alist-get 'sessionID data)
      (when-let* ((permission (opencode--permission-find (alist-get 'requestID data))))
        (opencode--permission-forget
         permission
         (pcase (alist-get 'reply data)
           ("reject" " [rejected]") ("always" " [accepted always]") (_ " [accepted]"))
         (if (equal (alist-get 'reply data) "reject") 'error 'success))))))

(defun opencode-permission-sync (&optional _info)
  "Reconcile this buffer's permission queue with the server's pending requests.
A request the server no longer has is reported lost; one asked while no
stream listened is queued."
  (when opencode-session-id
    (let ((known (copy-sequence opencode-pending-permissions)))
      (opencode-request
       'get (format "/api/session/%s/permission" opencode-session-id)
       :then (lambda (response)
               (let* ((requests (opencode-request-data response))
                      (ids (mapcar (lambda (request) (alist-get 'id request)) requests)))
                 (dolist (permission known)
                   (when (and (memq permission opencode-pending-permissions)
                              (not (member (plist-get permission :id) ids)))
                     (opencode--permission-forget permission " [lost]" 'error)
                     (opencode-output-notice
                      (format "The OpenCode service no longer has the permission request “%s”."
                              (plist-get permission :title))
                      'opencode-attention)))
                 (dolist (request requests)
                   (opencode-permission-queue request))))
       :else (lambda (error) (message "OpenCode permission sync failed: %s"
                                      (opencode-error-describe error)))))))

(opencode-events-register "permission.asked" #'opencode--permission-asked)
(opencode-events-register "permission.replied" #'opencode--permission-replied)
(add-hook 'opencode-session-resync-functions #'opencode-permission-sync)
(add-hook 'opencode-session-replayed-hook #'opencode-permission-sync)

(provide 'opencode-permission)
;;; opencode-permission.el ends here
