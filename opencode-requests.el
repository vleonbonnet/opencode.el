;;; opencode-requests.el --- Requests waiting on the user  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
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

;; Permission requests and questions never take focus.  Arrival marks the
;; tool block that asked, in the attention face and with a question mark,
;; and \\[opencode-respond] answers the request at point, a suspended one,
;; or the oldest one.  Inside either transient, C-v, M-v and the mouse
;; wheel scroll the transcript, C-z suspends it so normal navigation
;; works, and C-g defers it; these keys are hidden from the popup.  Each
;; request keeps its own progress, so deferring or suspending one never
;; blocks another.
;;
;; `opencode-respond-session' with IN-PLACE answers a session's request
;; from wherever the user is, such as a session list, without showing
;; its buffer: the transient names the session and shows a file
;; permission's change, and the scrolling keys scroll it.
;;
;; A subagent asks in its own session, whose buffer opens in the
;; background, so its parent looks busy while it actually waits.  The
;; parent's block of a waiting subagent is marked the same way and its
;; mode line shows the pending state, through every level of nesting;
;; \\[opencode-respond] there switches to the child and answers it.  A
;; subagent run in the background ended its parent's call at once: it
;; marks no block, yet its parent answers its requests all the same.

;;; Code:

(require 'opencode-blocks)
(require 'opencode-common)
(require 'opencode-notify)
(require 'opencode-session)
(require 'transient)

(defvar opencode-request-providers nil
  "Functions describing the requests a session buffer waits on.
Each returns, in the current buffer, nil or a plist with :kind (`question'
or `permission'), :pending (the requests awaiting an answer, plists each
with its :id), :at-point (a function answering the request at point, or
nil), :respond (a function answering the oldest request), :dismiss (a
function of a session ID, the ID of a request of its kind and a message
for the agent, dismissing the request so that the agent goes on, told
the message, whichever buffer is current) and :sync (a function
reconciling the buffer's requests with the server, then calling the
function it receives with no argument), tried in order.")

(defvar-local opencode--pending-children nil
  "Child session IDs, oldest first, whose requests await the user.")

(defvar opencode--answering-in-place nil
  "Non-nil while a request is opened to be answered in place.
Its transient then shows the session asking and the request's context,
and leaves every window as it is: see `opencode-respond-session'.")

(defvar opencode--permission-transient-state)
(defvar opencode--active-form)

(defconst opencode--transient-menu-variable
  (if (boundp 'transient-show-menu) 'transient-show-menu 'transient-show-popup)
  "Transient's option deciding when menus show, by its name in this version.")

(defmacro opencode--with-transient-menu (show &rest body)
  "Run BODY, the transients it sets up showing their menu at once if SHOW.
A request answered in place has its context in its transient's menu,
which a user delaying menus would otherwise not see."
  (declare (indent 1) (debug t))
  (let ((variables (make-symbol "variables")))
    `(let ((,variables (and ,show (list opencode--transient-menu-variable))))
       (cl-progv ,variables (and ,variables (list t))
         ,@body))))

(defun opencode-transient-displayed-p (prefix)
  "Return non-nil when transient PREFIX is on screen."
  (and (bound-and-true-p transient--prefix)
       (eq (oref transient--prefix command) prefix)))

(defun opencode-transient-suspended-p (prefix)
  "Return non-nil when transient PREFIX tops the stack of suspended transients."
  (eq (caar (bound-and-true-p transient--stack)) prefix))

(defun opencode-install-hidden-navigation (prefix suffixes)
  "Keep the navigation SUFFIXES of transient PREFIX active but unlisted."
  (unless (ignore-errors (transient-get-suffix prefix (caar suffixes)))
    (transient-append-suffix prefix '(-1)
      `[:hide (lambda () t) ,@suffixes])))

(defun opencode--transient-session-buffer ()
  "Return the session buffer whose request the shown transient answers, or nil."
  (pcase (and (bound-and-true-p transient--prefix) (oref transient--prefix command))
    ('opencode-permission-transient (plist-get opencode--permission-transient-state :buffer))
    ('opencode-question-transient (plist-get opencode--active-form :buffer))))

(defun opencode--scroll-transient-session (command &optional arg)
  "Run scroll COMMAND with ARG in a window showing the request's transcript.
That is the window the transient was called from when it shows it, else
another one.  Answered in place, the session may be shown nowhere: the
transient's menu, which then holds the request's context, scrolls."
  (let* ((buffer (opencode--transient-session-buffer))
         (window (cond
                  ((and (window-live-p transient--original-window)
                        (eq (window-buffer transient--original-window) buffer))
                   transient--original-window)
                  ((and (buffer-live-p buffer) (get-buffer-window buffer)))
                  ((window-live-p transient--window) transient--window))))
    (when window
      (with-selected-window window
        (funcall command arg)))))

(transient-define-suffix opencode-transient-scroll-down ()
  "Scroll the transcript down."
  :transient t
  (interactive)
  (opencode--scroll-transient-session #'scroll-up-command))

(transient-define-suffix opencode-transient-scroll-up ()
  "Scroll the transcript up."
  :transient t
  (interactive)
  (opencode--scroll-transient-session #'scroll-down-command))

(transient-define-suffix opencode-transient-wheel-down ()
  "Scroll the transcript down by a mouse-wheel step."
  :transient t
  (interactive)
  (opencode--scroll-transient-session #'scroll-up-command 3))

(transient-define-suffix opencode-transient-wheel-up ()
  "Scroll the transcript up by a mouse-wheel step."
  :transient t
  (interactive)
  (opencode--scroll-transient-session #'scroll-down-command 3))

(defconst opencode-transient-scroll-suffixes
  '(("C-v" "Scroll transcript down" opencode-transient-scroll-down)
    ("M-v" "Scroll transcript up" opencode-transient-scroll-up)
    ("<wheel-down>" "Scroll transcript down" opencode-transient-wheel-down)
    ("<wheel-up>" "Scroll transcript up" opencode-transient-wheel-up))
  "Transcript scrolling keys of the request transients.")

(defun opencode-pending-request-p ()
  "Return non-nil when this session buffer awaits an answer from the user."
  (or opencode--pending-children
      (seq-some (lambda (provider) (plist-get (funcall provider) :pending))
                opencode-request-providers)))

(defun opencode-respond ()
  "Answer the request at point, a suspended one, or the oldest pending one.
On the block of a waiting subagent, or with only subagents waiting,
switch to the subagent's session and answer there.  Subagents at any
depth count, run in the foreground or in the background, the oldest
request among them first (see `opencode-tree-requests')."
  (interactive)
  (or (seq-some (lambda (provider)
                  (when-let* ((at-point (plist-get (funcall provider) :at-point)))
                    (funcall at-point)
                    t))
                opencode-request-providers)
      (when-let* ((child (opencode--child-at-point)))
        (opencode--respond-child child)
        t)
      (seq-some (lambda (provider)
                  (let ((state (funcall provider)))
                    (when (plist-get state :pending)
                      (funcall (plist-get state :respond))
                      t)))
                opencode-request-providers)
      (when-let* ((child (opencode--waiting-subagent)))
        (opencode-respond-session child)
        t)
      (user-error "No pending OpenCode request")))

(defun opencode--sync-requests (callback)
  "Reconcile this buffer's requests with the server, then call CALLBACK in it."
  (let* ((buffer (current-buffer))
         (syncs (delq nil (mapcar (lambda (provider) (plist-get (funcall provider) :sync))
                                  opencode-request-providers)))
         (pending (length syncs)))
    (cl-flet ((done ()
                (when (and (zerop (cl-decf pending)) (buffer-live-p buffer))
                  (with-current-buffer buffer (funcall callback)))))
      (if syncs
          (dolist (sync syncs) (funcall sync #'done))
        (cl-incf pending)
        (done)))))

(defun opencode--request-session-heading ()
  "Return the line naming this buffer's session, heading a request answered in place."
  (concat (propertize (or (alist-get 'title opencode-session-info) opencode-session-id)
                      'face 'bold)
          (propertize (concat "  " (if (opencode-service-files-p)
                                       (abbreviate-file-name default-directory)
                                     (format "%s on %s" (or (opencode-location) "")
                                             (opencode-service-label))))
                      'face 'shadow)
          "\n"))

(defun opencode--respond-in-place ()
  "Answer in place the request `opencode-respond' answers with point on none.
Nothing is shown but the request's transient, which names the session;
a subagent's request is answered in place too."
  (let ((opencode--answering-in-place t))
    (or (seq-some (lambda (provider)
                    (let ((state (funcall provider)))
                      (when (plist-get state :pending)
                        (funcall (plist-get state :respond))
                        t)))
                  opencode-request-providers)
        (when-let* ((child (opencode--waiting-subagent)))
          (opencode-respond-session child nil t)
          t)
        (user-error "No pending OpenCode request"))))

(defun opencode-respond-session (id &optional other-window in-place)
  "Show session ID's buffer, opening it if needed, and answer its request.
OTHER-WINDOW shows it in another window.  The request answered is the
one \\[opencode-respond] would answer there: the session's own first,
else the oldest of its subagents', at any depth, which is answered in
the subagent's buffer.  A buffer opened here, or one awaiting nothing
yet, first loads its transcript and the requests the session waits on,
then is shown again and answers.

IN-PLACE answers from the selected window instead, showing no session
buffer: the request's transient names the session and holds what the
request asks, its tool call's changes for a file permission.  The
request answered is the one \\[opencode-respond] answers there with
point on no request; a subagent's is answered in place too.  A buffer
without the request yet, or none at all, is opened in the background,
without display, and loads the session's requests first.  The answer
goes to the session asking, whose buffer shows it answered as when
answered there."
  (let ((buffer (gethash id (opencode-sv opencode-session-buffers))))
    (cl-flet ((answer ()
                ;; From a callback, a signal would only print "error in process filter".
                (condition-case err
                    (if in-place
                        (opencode--respond-in-place)
                      (with-current-buffer (opencode--display (current-buffer) nil other-window)
                        (opencode-respond)))
                  (user-error (message "%s" (error-message-string err))))))
      (cond
       ((not (buffer-live-p buffer))
        (opencode-session-open-by-id
         id :other-window other-window :no-display in-place
         :callback (lambda (_info) (opencode--sync-requests #'answer))))
       ((with-current-buffer buffer
          (or (opencode-pending-request-p) (opencode--waiting-subagent)))
        (with-current-buffer buffer
          (if in-place
              (opencode--respond-in-place)
            (with-current-buffer (opencode--display buffer nil other-window)
              (opencode-respond)))))
       (t (with-current-buffer buffer (opencode--sync-requests #'answer)))))))

;;;; Requests in a session's tree

(defun opencode--session-parent (id)
  "Return the ID of the parent of session ID, or nil.
The registry tells it, else the session's buffer."
  (or (plist-get (opencode-state-get id) :parent-id)
      (opencode-with-session-buffer id (alist-get 'parentID opencode-session-info))))

(defun opencode--subagent-of-p (id ancestor)
  "Return non-nil when session ID is a subagent of session ANCESTOR, at any depth."
  (let ((seen nil)
        (parent (opencode--session-parent id)))
    (while (and parent (not (equal parent ancestor)) (not (member parent seen)))
      (push parent seen)
      (setq parent (opencode--session-parent parent)))
    (equal parent ancestor)))

(defun opencode--own-requests ()
  "Return the requests this session buffer waits on, as `opencode-tree-requests' does."
  (mapcan (lambda (provider)
            (let ((state (funcall provider)))
              (mapcar (lambda (request)
                        (list :session opencode-session-id :kind (plist-get state :kind)
                              :id (plist-get request :id)))
                      (plist-get state :pending))))
          opencode-request-providers))

(defun opencode-tree-requests ()
  "Return the requests pending in this session's tree: its own and its subagents'.
Each is a plist (:session ID :kind KIND :id REQUEST-ID :time TIME),
KIND being `question' or `permission' and TIME when it was asked, or
nil when unknown.  The session's own come first, then those of its
subagents, at any depth, run in the foreground or in the background,
oldest first.  They come from the buffers of the sessions and from the
session registry while it follows the service, which also knows the
subagents that have no buffer."
  (let ((own nil)
        (below nil)
        (known (make-hash-table :test #'equal))
        (registry (and opencode-session-id (opencode-state-ready-p))))
    (cl-flet ((add (request)
                (let ((id (plist-get request :id)))
                  (unless (gethash id known)
                    (puthash id t known)
                    (let ((request (plist-put (copy-sequence request) :time
                                              (or (plist-get request :time)
                                                  (opencode-identifier-time id)))))
                      (if (equal (plist-get request :session) opencode-session-id)
                          (push request own)
                        (push request below)))))))
      (mapc #'add (opencode--own-requests))
      (when opencode-session-id
        (let ((session opencode-session-id))
          (maphash (lambda (id buffer)
                     (when (and (not (equal id session)) (buffer-live-p buffer)
                                (opencode--subagent-of-p id session))
                       (with-current-buffer buffer
                         (mapc #'add (opencode--own-requests)))))
                   (opencode-sv opencode-session-buffers))))
      (when registry
        (dolist (entry (cons (opencode-state-get opencode-session-id)
                             (opencode-state-descendants opencode-session-id)))
          (dolist (request (plist-get entry :requests))
            (add (list :session (plist-get entry :id) :kind (plist-get request :kind)
                       :id (plist-get request :id) :time (plist-get request :time)))))))
    (append (nreverse own)
            (sort (nreverse below)
                  :key (lambda (request) (or (plist-get request :time) most-positive-fixnum))))))

(defun opencode--waiting-subagent ()
  "Return the ID of the subagent whose request this session answers first, or nil.
That is the subagent, at any depth, run in the foreground or in the
background, asking the oldest of the requests `opencode-tree-requests'
lists below this session."
  (plist-get (seq-find (lambda (request)
                         (not (equal (plist-get request :session) opencode-session-id)))
                       (opencode-tree-requests))
             :session))

;;;; Sending while a request waits

;; The service delivers a steering prompt or command at the agent's next
;; step, and a queued one after the turn; a step waiting on a permission
;; request or a question ends only once it is answered or dismissed.  So
;; does a step running a subagent in the foreground, which ends with the
;; subagent's turn, itself held up by its own requests and foreground
;; subagents.  A prompt sent meanwhile thus waits, unread, and is neither
;; an answer nor a dismissal, so `opencode-send-input' asks first: to
;; dismiss the requests and send, the agents asking told why and going
;; on, or to send the prompt to wait, among others.  A subagent run in
;; the background ended its call at once: its requests hold up only
;; itself, and a prompt to its parent is read as usual.

(defun opencode--runs-in-foreground-p (session-id child-id)
  "Return non-nil when session SESSION-ID waits for its subagent CHILD-ID to end.
That is while a call of the subagent tool running CHILD-ID in the
foreground runs in SESSION-ID's buffer; a call running it in the
background ends at once, still running.  Nil without a buffer of
SESSION-ID, as nothing then tells how it runs CHILD-ID."
  (opencode-with-session-buffer session-id
    (and (seq-some (lambda (overlay)
                     (equal (plist-get (opencode-block-data overlay) :status) "running"))
                   (opencode--subagent-blocks child-id))
         t)))

(defun opencode--holds-up-p (id session-id)
  "Return non-nil when the requests of session ID hold up the turn of SESSION-ID.
Those of SESSION-ID itself do; a subagent's do when each session from
SESSION-ID down to it runs the next one in the foreground (see
`opencode--runs-in-foreground-p')."
  (let ((seen nil)
        (held t))
    (while (and held (not (equal id session-id)))
      (let ((parent (opencode--session-parent id)))
        (setq held (and parent (not (member parent seen))
                        (opencode--runs-in-foreground-p parent id)))
        (push parent seen)
        (setq id parent)))
    held))

(defun opencode-awaited-requests ()
  "Return the requests this session waits on before it reads a prompt.
They are those of `opencode-tree-requests' that hold up its turn: its
own, and those of the subagents it runs in the foreground, through
every level, as their tool calls end only with their turns.  A subagent
run in the background holds up nothing, its call having ended at once.
How a session runs a subagent shows in the session's buffer; below a
subagent that has none, nothing tells, and requests are left out."
  (let ((session opencode-session-id))
    (seq-filter (lambda (request) (opencode--holds-up-p (plist-get request :session) session))
                (opencode-tree-requests))))

(defun opencode--describe-awaited (requests)
  "Return the sentence telling that this session's prompt would wait on REQUESTS.
REQUESTS are plists as `opencode-awaited-requests' returns them."
  (let* ((sessions (delete-dups (mapcar (lambda (request) (plist-get request :session)) requests)))
         (own (equal sessions (list opencode-session-id)))
         (one (null (cdr requests)))
         (kind (if (eq (plist-get (car requests) :kind) 'permission) "permission request" "question")))
    (format-message
     "%s for your answer to %s; %s reads the prompt only once %s answered or dismissed."
     (cond (own "This session waits")
           ((cdr sessions)
            (if (member opencode-session-id sessions)
                "This session and its subagents wait"
              "Its subagents wait"))
           (t (format-message "Its subagent `%s' waits"
                              (or (opencode--session-title (car sessions)) (car sessions)))))
     (if one (concat "a " kind) (format "%d requests" (length requests)))
     (if own "it" "this session")
     (cond ((not one) "they are")
           ((eq (plist-get (car requests) :kind) 'permission) "the request is")
           (t "the question is")))))

(defun opencode--answer-awaited (requests)
  "Answer the first of REQUESTS, as `opencode-awaited-requests' returns them.
A request of this session is answered here, loaded first when only the
registry knew of it; a subagent's in the subagent's buffer."
  (let ((id (plist-get (car requests) :session)))
    (cond
     ((not (equal id opencode-session-id)) (opencode-respond-session id))
     ((opencode--own-requests) (opencode-respond))
     (t (opencode--sync-requests
         (lambda ()
           ;; From a callback, a signal would only print "error in process filter".
           (condition-case err
               (opencode-respond)
             (user-error (message "%s" (error-message-string err))))))))))

(defvar opencode--send-cancel-quietly)

(defcustom opencode-dismiss-to-send-message
  "The user dismissed this request to send you a new prompt instead."
  "What an agent is told of its request dismissed for input sent past it.
Sending a prompt or a command while the session waits on requests may
dismiss them first (see `opencode-check-pending-requests'): a permission
request is rejected with this, a question dismissed with it, and the
agent asking goes on, told why."
  :type 'string
  :group 'opencode)

(defun opencode-dismiss-awaited (requests &optional message)
  "Dismiss REQUESTS, as `opencode-awaited-requests' returns them.
Each request goes to the session asking, a subagent's to the subagent,
whose buffer, when it knows the request, shows it settled as when
answered there.  A permission request is rejected and a question
dismissed, each telling the agent MESSAGE, by default
`opencode-dismiss-to-send-message', so that the agent goes on with
it.  A dismissal that fails is told, its request still waiting."
  (let ((message (or message opencode-dismiss-to-send-message)))
    (dolist (request requests)
      (when-let* ((dismiss (seq-some (lambda (provider)
                                       (let ((state (funcall provider)))
                                         (and (eq (plist-get state :kind) (plist-get request :kind))
                                              (plist-get state :dismiss))))
                                     opencode-request-providers)))
        (funcall dismiss (plist-get request :session) (plist-get request :id) message)))))

(defun opencode-check-pending-requests (request)
  "Ask what to do with input REQUEST while the session waits on the user.
A send check (see `opencode-send-checks'): a prompt or command that
steers this session while it, or a subagent it runs in the foreground,
waits on a permission request or a question would wait unread until
that is answered or dismissed (see `opencode-awaited-requests').  A
subagent run in the background holds up nothing, so its requests never
ask.  Sending dismisses the requests (see `opencode-dismiss-awaited')
once every check let the input go, the agents asking going on, and
sends it; queueing sends it to wait, setting `opencode-send-waits';
answering opens the request and keeps the input, as cancelling does;
discarding deletes the input.  Queued input
\(`opencode-queue-input', or `opencode-prompt-delivery' set to `queue')
waits for the end of the turn anyway, and shell commands run at once,
so neither asks.  Sends that run no check, such as scheduled ones
through `opencode-session-send', wait the same way."
  (when-let* (((memq (plist-get request :kind) '(prompt command)))
              ((not (eq opencode-prompt-delivery 'queue)))
              (requests (opencode-awaited-requests)))
    (pcase (car (read-multiple-choice
                 (opencode--describe-awaited requests)
                 '((?s "send" "Dismiss the requests and send now; the agents asking go on, told why")
                   (?q "queue" "Send now; this session reads it once the requests are settled")
                   (?a "answer" "Answer it now; the input stays at the prompt")
                   (?c "cancel" "Keep the input at the prompt")
                   (?d "discard" "Delete the input; it goes to the kill ring"))))
      (?s (push (lambda () (opencode-dismiss-awaited requests)) opencode-send-commit-functions)
          nil)
      (?q (setq opencode-send-waits t)
          nil)
      (?a (opencode--answer-awaited requests)
          ;; The request it opens says enough.
          (setq opencode--send-cancel-quietly t)
          'cancel)
      (?d 'discard)
      (_ 'cancel))))

;; First, so that no other check asks before this one.
(add-hook 'opencode-send-checks #'opencode-check-pending-requests -50)

;;;; Subagent requests in their parents

(opencode-defvar-service opencode--sync-sessions nil
  "Session IDs whose pending state must reach their parents.")

(opencode-defvar-service opencode--sync-timer nil
  "Timer flushing `opencode--sync-sessions'.")

(defun opencode-request-state-changed (&optional session-id)
  "Propagate the pending state of SESSION-ID, this buffer's by default, to its parents.
Its buffer shows anew where it stands (see `opencode-session-summary-update').
Changes are gathered into one timer; each sync recomputes the state."
  (when-let* ((session-id (or session-id opencode-session-id)))
    (cl-pushnew session-id (opencode-sv opencode--sync-sessions) :test #'equal)
    (unless (timerp (opencode-sv opencode--sync-timer))
      (setf (opencode-sv opencode--sync-timer)
            (apply #'run-at-time 0 nil #'opencode--flush-sync (opencode--timer-arguments))))))

(defun opencode--flush-sync (&optional service)
  "Propagate every changed session's pending state to its parents.
The sessions are SERVICE's, nil for the local one."
  (opencode-with-service service
    (opencode--flush-sync-1)))

(defun opencode--flush-sync-1 ()
  "Propagate the current service's changed sessions, as `opencode--flush-sync'."
  (setf (opencode-sv opencode--sync-timer) nil)
  (let ((sessions (reverse (opencode-sv opencode--sync-sessions))))
    (setf (opencode-sv opencode--sync-sessions) nil)
    (dolist (session-id sessions)
      (with-demoted-errors "OpenCode subagent request sync: %S"
        (opencode--sync-parents session-id))
      ;; A session waiting for its user shows where it stands.
      (when-let* ((buffer (gethash session-id (opencode-sv opencode-session-buffers)))
                  ((buffer-live-p buffer)))
        (with-demoted-errors "OpenCode summary: %S"
          (opencode-session-summary-update buffer))))))

(defun opencode--child-pending-p (session-id)
  "Return non-nil when the open buffer of SESSION-ID awaits an answer."
  (when-let* ((buffer (gethash session-id (opencode-sv opencode-session-buffers))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer (opencode-pending-request-p))
         t)))

(defun opencode--subagent-blocks (session-id)
  "Return this buffer's subagent blocks running child SESSION-ID."
  (seq-filter (lambda (overlay)
                (let ((data (opencode-block-data overlay)))
                  (and (equal (plist-get data :tool) "subagent")
                       (equal (plist-get data :session-id) session-id))))
              (opencode-tool-blocks)))

(defun opencode--set-child-waiting (overlay pending)
  "Mark subagent block OVERLAY as PENDING; a finished subagent never waits.
A request of the subagent call itself keeps precedence."
  (let* ((data (opencode-block-data overlay))
         (kind (plist-get data :waiting-kind))
         (pending (and pending (equal (plist-get data :status) "running"))))
    (cond
     ((and pending (null kind))
      (opencode-block-update overlay :waiting t :waiting-kind 'child))
     ((and (not pending) (eq kind 'child))
      (opencode-block-update overlay :waiting nil :waiting-kind nil)))))

(defun opencode--update-pending-child (session-id)
  "Record whether child SESSION-ID awaits the user in this buffer."
  (let ((pending (seq-some (lambda (overlay) (plist-get (opencode-block-data overlay) :waiting))
                           (opencode--subagent-blocks session-id))))
    (if pending
        (unless (member session-id opencode--pending-children)
          (setq opencode--pending-children (append opencode--pending-children (list session-id))))
      (setq opencode--pending-children (remove session-id opencode--pending-children)))
    (force-mode-line-update)
    (opencode-request-state-changed)))

(defun opencode--sync-parents (session-id)
  "Mark every subagent block running SESSION-ID with its pending state."
  (let ((pending (opencode--child-pending-p session-id)))
    (maphash (lambda (id buffer)
               (when (and (not (equal id session-id)) (buffer-live-p buffer))
                 (with-current-buffer buffer
                   (when-let* ((overlays (opencode--subagent-blocks session-id)))
                     (dolist (overlay overlays)
                       (opencode--set-child-waiting overlay pending))
                     (opencode--update-pending-child session-id)))))
             (opencode-sv opencode-session-buffers))))

(defun opencode--attach-child (overlay)
  "Show whether the subagent run by OVERLAY's block awaits the user."
  (let ((data (opencode-block-data overlay)))
    (when-let* (((equal (plist-get data :tool) "subagent"))
                (session-id (plist-get data :session-id)))
      (opencode--set-child-waiting overlay (opencode--child-pending-p session-id))
      (opencode--update-pending-child session-id))))

(defun opencode--child-at-point ()
  "Return the child session ID of the waiting subagent block at point."
  (when-let* ((overlay (opencode-block-at (point)))
              (data (opencode-block-data overlay))
              ((eq (plist-get data :waiting-kind) 'child)))
    (plist-get data :session-id)))

(defun opencode--respond-child (session-id)
  "Show child SESSION-ID in the selected window and answer its oldest request."
  (let ((buffer (gethash session-id (opencode-sv opencode-session-buffers))))
    (unless (buffer-live-p buffer)
      (user-error "The subagent session is no longer open"))
    (pop-to-buffer-same-window buffer)
    (opencode-respond)))

(defun opencode--open-subagent (&optional overlay)
  "Open the child session of subagent block OVERLAY, or the block at point."
  (when-let* ((overlay (or overlay (opencode-block-at (point))))
              (data (opencode-block-data overlay))
              ((equal (plist-get data :tool) "subagent")))
    (if-let* ((session-id (plist-get data :session-id)))
        (opencode-session-open-by-id session-id)
      (user-error "The subagent session is not available yet"))
    t))

(defun opencode--forget-child ()
  "Clear this killed session's pending state from its parents."
  (opencode-with-service opencode--buffer-service
    (opencode-request-state-changed)))

(add-hook 'opencode-blocks-attach-functions #'opencode--attach-child)
(add-hook 'opencode-open-link-functions #'opencode--open-subagent)
(add-hook 'opencode-session-mode-hook
          (lambda () (add-hook 'kill-buffer-hook #'opencode--forget-child nil t)))

(defun opencode--refresh-subagent (overlay)
  "Recheck subagent OVERLAY once its call finished or reported its session."
  (opencode--attach-child overlay))

(add-hook 'opencode-blocks-finished-functions #'opencode--refresh-subagent)

(provide 'opencode-requests)
;;; opencode-requests.el ends here
