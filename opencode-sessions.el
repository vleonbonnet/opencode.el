;;; opencode-sessions.el --- Session pickers, projects and MCP servers  -*- lexical-binding: t; -*-

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

;; opencode.el lists no sessions in a buffer of its own: drover lists
;; those of every agent front end, this one's included, from the session
;; registry `opencode-state'.  Sessions are picked with completion
;; instead.  `opencode' offers the sessions of the current project, or
;; starts one; `opencode-select-project' picks a project first.  The
;; server keys a project by its repository, so a project's sessions
;; include those of all its worktrees.  Every picker shows a session's
;; status icon before its title, and its project, branch and age after
;; it, newest first.  Branches are read from each worktree's HEAD: asking
;; the server would activate a location for every directory offered, and
;; running Git would cost a process per directory.  Commands acting on a
;; session, such as `opencode-rename-session', pick one of the current
;; project's when called outside a session buffer.

;;; Code:

(require 'map)
(require 'opencode-common)
(require 'opencode-http)
(require 'opencode-session)
(require 'opencode-state)
(require 'url-util)

(defcustom opencode-worktree-directory "~/opencode_worktrees/"
  "Directory where `opencode-new-worktree' creates worktrees."
  :type 'directory
  :group 'opencode)

(define-obsolete-variable-alias 'opencode-session-list-branch-ttl 'opencode-git-branch-ttl "2.0.0")

(defcustom opencode-git-branch-ttl 30
  "Seconds `opencode-git-branch' caches the branch of a worktree."
  :type 'natnum
  :group 'opencode)

(defface opencode-session-status-waiting
  '((t :inherit opencode-attention))
  "Status of a session waiting on a permission request or a question."
  :group 'opencode-faces)

(defface opencode-session-status-working
  '((t :inherit font-lock-keyword-face))
  "Status of a session running a turn."
  :group 'opencode-faces)

(defface opencode-session-status-retrying
  '((t :inherit opencode-session-status-working :slant italic))
  "Status of a session whose turn waits to retry a failed request."
  :group 'opencode-faces)

(defface opencode-session-status-failed
  '((t :inherit error))
  "Status of a session whose turn failed unseen."
  :group 'opencode-faces)

(defface opencode-session-status-done
  '((t :inherit success))
  "Status of a session whose turn ended unseen."
  :group 'opencode-faces)

(defface opencode-session-status-idle
  '((t :inherit shadow))
  "Status of an idle session whose last turn was seen."
  :group 'opencode-faces)

(defcustom opencode-session-status-icons
  '((waiting "\U000F02D7" opencode-session-status-waiting)   ; nf-md-help_circle
    (working "\U000F051F" opencode-session-status-working)   ; nf-md-timer_sand
    (retrying "\uf021" opencode-session-status-retrying)     ; nf-fa-refresh
    (failed "\uf057" opencode-session-status-failed)         ; nf-fa-times_circle
    (done "\U000F05E0" opencode-session-status-done)         ; nf-md-check_circle
    (idle "\uf10c" opencode-session-status-idle))            ; nf-fa-circle_o
  "Session statuses, as `opencode-state-status' names them, with their icon and face.
Session pickers show the icon before the title, and name the status in
its tooltip.  The default icons come from Nerd Fonts, which must be
installed for them to show."
  :type '(alist :key-type (symbol :tag "Status")
                :value-type (list (string :tag "Icon") (face :tag "Face")))
  :group 'opencode)

;;;; Server data

(defvar opencode--projects nil
  "The projects `opencode-fetch-projects' last received, naming sessions in pickers.")

(defun opencode-fetch-projects (callback &optional else)
  "Call CALLBACK with the projects the service knows.
ELSE, when given, receives the `opencode-error' of a failed request."
  (opencode-request 'get "/api/project" :buffer nil :else else
                    :then (lambda (response)
                            (setq opencode--projects response)
                            (funcall callback response))))

(defun opencode-project-for-directory (projects directory)
  "Return the project of PROJECTS containing DIRECTORY, or nil.
A project contains its canonical directory and its sandboxes; the
innermost match wins."
  (let ((directory (opencode-normalize-directory directory))
        best best-length)
    (dolist (project projects)
      (dolist (root (cons (alist-get 'canonical project) (alist-get 'sandboxes project)))
        (when (stringp root)
          (let ((root (opencode-normalize-directory root)))
            (when (and (string-prefix-p root directory (memq system-type '(windows-nt ms-dos)))
                       (> (length root) (or best-length -1)))
              (setq best project best-length (length root)))))))
    best))

(defun opencode-project-name (project)
  "Return the display name of PROJECT."
  (or (alist-get 'name project)
      (file-name-nondirectory (directory-file-name (alist-get 'canonical project)))))

(defun opencode-fetch-sessions (query callback &optional else cursor accumulated)
  "Call CALLBACK with every session matching QUERY, newest first.
ELSE, when given, receives the `opencode-error' of a failed request.
CURSOR and ACCUMULATED page through the list; a cursor carries the
filters of the first page, so later pages send it alone."
  (opencode-request
   'get "/api/session" :buffer nil :else else
   :query (if cursor
              `((limit . 100) (cursor . ,cursor))
            `(,@query (order . "desc") (limit . 100)))
   :then (lambda (response)
           (let ((data (opencode-request-data response))
                 (next (alist-get 'next (alist-get 'cursor response))))
             (if (and data next)
                 (opencode-fetch-sessions query callback else next (append accumulated data))
               (funcall callback (append accumulated data)))))))

;;;; Branches

(defvar opencode--branches (make-hash-table :test #'equal)
  "Directories mapped to (BRANCH . TIME) as last read.")

(defun opencode--read-git-branch (directory)
  "Return the branch checked out in DIRECTORY's worktree, or a short commit.
Return nil outside Git.  HEAD is read directly, without running Git."
  (when-let* ((root (locate-dominating-file directory ".git"))
              (dotgit (expand-file-name ".git" root))
              (gitdir (if (file-directory-p dotgit)
                          dotgit
                        ;; A worktree's .git file names its Git directory.
                        (with-temp-buffer
                          (insert-file-contents dotgit)
                          (when (re-search-forward "^gitdir: *\\(.+?\\) *$" nil t)
                            (expand-file-name (match-string 1) root)))))
              (head (expand-file-name "HEAD" gitdir))
              ((file-readable-p head)))
    (with-temp-buffer
      (insert-file-contents head)
      (cond ((looking-at "ref: refs/heads/\\(.+?\\) *$") (match-string 1))
            ((looking-at "[0-9a-f]\\{7\\}") (match-string 0))))))

(defun opencode-git-branch (directory)
  "Return DIRECTORY's Git branch, cached for `opencode-git-branch-ttl' seconds."
  (let ((entry (gethash directory opencode--branches))
        (now (float-time)))
    (if (and entry (< (- now (cdr entry)) opencode-git-branch-ttl))
        (car entry)
      (let ((branch (and (file-directory-p directory)
                         (ignore-errors (opencode--read-git-branch directory)))))
        (puthash directory (cons branch now) opencode--branches)
        branch))))

;;;; Session pickers

(defun opencode-session-status-icon (status)
  "Return the icon of STATUS, a session status symbol, in its face.
Its tooltip names the status; an unknown status is \"-\"."
  (if-let* ((display (alist-get status opencode-session-status-icons)))
      (propertize (car display) 'face (cadr display) 'help-echo (symbol-name status))
    "-"))

(defun opencode--session-entry (info)
  "Return the registry entry of session INFO, else an entry made from INFO.
The registry follows the service, so its title, directory and status
are current where INFO, fetched earlier, may be stale.  An entry made
from INFO knows whether its last turn was seen, but not whether one runs."
  (or (opencode-state-get (alist-get 'id info))
      (opencode-state--info-entry info)))

(defun opencode--session-project-name (entry)
  "Return the name of the project of registry ENTRY, or nil when unknown.
The project is looked up among `opencode--projects', by ID, else by
directory."
  (let ((id (plist-get entry :project-id))
        (directory (plist-get entry :directory)))
    (when-let* ((project (or (and id (seq-find (lambda (project) (equal (alist-get 'id project) id))
                                               opencode--projects))
                             (and directory (opencode-project-for-directory opencode--projects directory))))
                ;; `opencode-project-name' needs one of them.
                ((or (alist-get 'name project) (stringp (alist-get 'canonical project))))
                (name (opencode-project-name project))
                ((not (string-empty-p name))))
      name)))

(defun opencode--session-candidates (sessions)
  "Return the candidates of `opencode-annotated-completion' for SESSIONS.
SESSIONS are session infos; each candidate's value is one of them.  Its
prefix is the session's status icon, its name the title, or the ID of an
untitled session, and its annotation the project, the branch and the
time since the session last changed, in columns.  A column no session
has a value for is left out.  The sort key puts the newest first."
  (let* ((rows (mapcar (lambda (info)
                         (let* ((entry (opencode--session-entry info))
                                (directory (plist-get entry :directory)))
                           (list info entry
                                 (or (opencode--session-project-name entry) "")
                                 (or (and directory (opencode-git-branch directory)) ""))))
                       sessions))
         (project-width (seq-max (cons 0 (mapcar (lambda (row) (string-width (nth 2 row))) rows))))
         (branch-width (seq-max (cons 0 (mapcar (lambda (row) (string-width (nth 3 row))) rows)))))
    (cl-flet ((column (text width)
                (unless (zerop width) (concat (string-pad text width) "  "))))
      (mapcar (pcase-lambda (`(,info ,entry ,project ,branch))
                (let ((title (plist-get entry :title))
                      (updated (plist-get entry :updated)))
                  (list (if (or (null title) (string-empty-p title)) (plist-get entry :id) title)
                        info
                        (concat (column project project-width)
                                (column branch branch-width)
                                (opencode-format-time-ago (and updated (- (float-time) updated))))
                        (- (or updated 0))
                        (concat (opencode-session-status-icon (opencode-state-status entry)) " "))))
              rows))))

(defun opencode-read-session (prompt sessions &optional new-directory)
  "Read one of SESSIONS, session infos, with PROMPT and return it.
Sessions are offered newest first, each after the icon of its status
\(see `opencode-session-status-icons') and followed by its project, its
branch when known and the time since it last changed.  The session
registry `opencode-state' gives them when it holds the session.  With
NEW-DIRECTORY, an entry starting a new session there is offered first,
and choosing it returns the symbol `new'.  Return nil when nothing is
chosen."
  ;; Statuses come from the registry, which needs the event stream.
  (opencode-state-ensure)
  (unless opencode--projects
    ;; Project names only decorate the sessions.
    (with-demoted-errors "OpenCode: cannot name the projects: %S"
      (opencode-wait #'opencode-fetch-projects)))
  (opencode-annotated-completion
   prompt
   (append (when new-directory
             (list (list "New session" 'new (abbreviate-file-name new-directory)
                         most-negative-fixnum "+ ")))
           (opencode--session-candidates sessions))))

(defun opencode--select-session (prompt sessions)
  "Open one of SESSIONS, session infos, chosen with PROMPT."
  (if (null sessions)
      (message "No sessions")
    (when-let* ((info (opencode-read-session prompt sessions)))
      (opencode-session-open info :other-window nil))))

(defun opencode--project-sessions (directory)
  "Return the sessions of DIRECTORY's project, newest first, once fetched.
They are those of every worktree of the project, subagents and archived
sessions left out.  A directory in no project the service knows has its
own sessions instead."
  (let ((project (opencode-project-for-directory (opencode-wait #'opencode-fetch-projects) directory)))
    (opencode-wait (lambda (then else)
                     (opencode-fetch-sessions
                      `((parentID . "null")
                        (archived . "false")
                        ,(if project
                             `(project . ,(alist-get 'id project))
                           `(directory . ,(opencode-directory-name directory))))
                      then else)))))

(defun opencode (&optional directory)
  "Open a session of DIRECTORY's project, or start one in DIRECTORY.
DIRECTORY defaults to the current project's root.  The project's
sessions, from all its worktrees, are offered newest first, after an
entry starting a new session; subagents and archived sessions are left
out.  When the project has none, a session starts at once.
See `opencode-read-session' for what each session shows."
  (interactive)
  (let* ((directory (opencode-project-directory directory))
         (sessions (opencode--project-sessions directory)))
    (pcase (if sessions (opencode-read-session "Session: " sessions directory) 'new)
      ('new (opencode-session-create :directory directory))
      ('nil nil)
      (info (opencode-session-open info :other-window nil)))))

(defalias 'opencode-select-session #'opencode)

(defun opencode-select-project ()
  "Pick a project the service knows, most recently active first, then a session.
The session is picked among the project's, or started in its directory,
as `opencode' does."
  (interactive)
  (when-let* ((project
               (opencode-annotated-completion
                "Project: "
                (mapcar (lambda (project)
                          (let ((active (or (map-nested-elt project '(time active))
                                            (map-nested-elt project '(time updated)))))
                            (list (abbreviate-file-name (alist-get 'canonical project)) project
                                  (opencode-format-time-ago (opencode-time-ago active))
                                  (- (or active 0)))))
                        (seq-filter (lambda (project) (stringp (alist-get 'canonical project)))
                                    (opencode-wait #'opencode-fetch-projects))))))
    (opencode (alist-get 'canonical project))))

(defun opencode--running-session-ids ()
  "Return the IDs of the sessions running a turn, once fetched."
  (opencode-wait (lambda (then else)
                   (opencode-request 'get "/api/session/active" :directory nil :buffer nil
                                     :then (lambda (response)
                                             (funcall then (opencode-state--active-ids response)))
                                     :else else))))

(defun opencode-read-project-session (prompt &optional running)
  "Read a session of the current project with PROMPT and return its info.
The sessions are those `opencode' offers; RUNNING offers only those
running a turn.  Signal a `user-error' when there is none or none is
chosen."
  (let* ((directory (opencode-project-directory))
         (active (and running (or (opencode--running-session-ids)
                                  (user-error "No OpenCode session is running"))))
         (sessions (seq-filter (lambda (info) (or (not running) (member (alist-get 'id info) active)))
                               (opencode--project-sessions directory))))
    (unless sessions
      (user-error (if running "No OpenCode session is running in %s" "No OpenCode session in %s")
                  (abbreviate-file-name directory)))
    (or (opencode-read-session prompt sessions)
        (user-error "No session chosen"))))

(defun opencode-select-child-session ()
  "Open a subagent session of this session."
  (interactive)
  (opencode--require-session)
  (opencode--select-session
   "Subagent: "
   (opencode-request-data
    (opencode-request-sync 'get "/api/session" :query `((parentID . ,opencode-session-id))))))

(defun opencode-select-open-session ()
  "Switch to the buffer of a session open in Emacs, picked among their sessions."
  (interactive)
  (let ((infos nil))
    (maphash (lambda (_id buffer)
               (when-let* (((buffer-live-p buffer))
                           (info (buffer-local-value 'opencode-session-info buffer)))
                 (push info infos)))
             opencode-session-buffers)
    (if (null infos)
        (message "No open sessions")
      (when-let* ((info (opencode-read-session "Open session: " infos))
                  (buffer (gethash (alist-get 'id info) opencode-session-buffers))
                  ((buffer-live-p buffer)))
        (switch-to-buffer buffer)))))

(defun opencode-select-idle ()
  "Open a session that finished while not displayed."
  (interactive)
  (opencode--select-session "Idle session: " opencode-alerted-sessions))

;;;; Worktrees

(defun opencode-new-worktree (name)
  "Create branch NAME in a new worktree of the current repository.
The worktree goes under `opencode-worktree-directory'; a session opens in it."
  (interactive (list (read-string "Worktree and branch name: ")))
  (when (string-empty-p name) (user-error "A name is needed"))
  (let ((default-directory (opencode-project-directory))
        (directory (expand-file-name name opencode-worktree-directory)))
    (with-temp-buffer
      (unless (zerop (process-file "git" nil t nil "worktree" "add" "-b" name directory "HEAD"))
        (user-error "Cannot create the worktree: %s" (string-trim (buffer-string)))))
    (opencode-session-create :directory directory)))

;;;; MCP servers

(defun opencode--mcp-status-label (server)
  "Return the status of MCP SERVER for display."
  (let-alist (alist-get 'status server)
    (pcase .status
      ("connected" "🟢 connected")
      ("disabled" "🔴 disabled")
      ("pending" "🟡 starting")
      ("needs_auth" (format "🔑 needs authorization: %s" .error))
      ("failed" (format "❌ failed: %s" .error))
      (_ .status))))

(defun opencode-toggle-mcp ()
  "Disconnect a connected MCP server of this location, or connect another.
The change lasts until the service restarts."
  (interactive)
  (when-let* ((server (opencode-annotated-completion
                       "MCP server: "
                       (mapcar (lambda (server)
                                 (list (alist-get 'name server) server
                                       (opencode--mcp-status-label server)))
                               (opencode-request-data (opencode-request-sync 'get "/api/mcp"))))))
    (let* ((name (alist-get 'name server))
           (connected (member (map-nested-elt server '(status status)) '("connected" "pending")))
           (action (if connected "disconnect" "connect")))
      (opencode-request
       'post (format "/api/experimental/mcp/%s/%s" (url-hexify-string name) action)
       :then (lambda (_)
               (message (if connected "Disconnected %s" "Connected %s") name))))))

(provide 'opencode-sessions)
;;; opencode-sessions.el ends here
