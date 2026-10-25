;;; opencode-sessions.el --- Session lists, projects and MCP servers  -*- lexical-binding: t; -*-

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

;; `opencode' lists the sessions of the current project in a table, the
;; session control panel; `opencode-list-all-sessions' lists those of
;; every project, with a Project column.  The server keys a project by
;; its repository, so a project's list includes the sessions of all its
;; worktrees, and the Branch column tells them apart.  Branches are read
;; from each worktree's HEAD: asking the server would activate a location
;; for every directory listed, and running Git would cost a process per
;; directory.  Subagent sessions are hidden until `v' shows them.  Lists
;; refresh when sessions are created, deleted, renamed, moved or forked,
;; and render again when their window's width changes, since column
;; limits are fractions of it.  Titles of sessions with live buffers are
;; highlighted, following local buffer opens and closes.
;;
;; OpenCode 2 reports no change summary with sessions, and its diff route
;; runs Git over the turn's snapshots, so the lists have no column of
;; changed files.

;;; Code:

(require 'map)
(require 'opencode-common)
(require 'opencode-events)
(require 'opencode-http)
(require 'opencode-session)
(require 'url-util)
(require 'vtable)

(defcustom opencode-worktree-directory "~/opencode_worktrees/"
  "Directory where `opencode-new-worktree' creates worktrees."
  :type 'directory
  :group 'opencode)

(defcustom opencode-session-list-branch-ttl 30
  "Seconds a worktree's branch is cached for the session lists."
  :type 'natnum
  :group 'opencode)

(defface opencode-session-list-open-session
  '((t :inherit success))
  "Title of a session with a live buffer in a session list."
  :group 'opencode-faces)

;;;; Server data

(defun opencode-fetch-projects (callback &optional else)
  "Call CALLBACK with the projects the service knows.
ELSE, when given, receives the `opencode-error' of a failed request."
  (opencode-request 'get "/api/project" :buffer nil :else else
                    :then (lambda (response) (funcall callback response))))

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
  "Return DIRECTORY's Git branch, cached for `opencode-session-list-branch-ttl'."
  (let ((entry (gethash directory opencode--branches))
        (now (float-time)))
    (if (and entry (< (- now (cdr entry)) opencode-session-list-branch-ttl))
        (car entry)
      (let ((branch (and (file-directory-p directory)
                         (ignore-errors (opencode--read-git-branch directory)))))
        (puthash directory (cons branch now) opencode--branches)
        branch))))

;;;; List buffers

(defvar-keymap opencode-session-list-mode-map
  :parent special-mode-map
  "r" #'opencode-session-list-refresh
  "n" #'opencode-session-list-new
  "M" #'opencode-toggle-mcp
  "v" #'opencode-session-list-toggle-subagents)

(define-derived-mode opencode-session-list-mode special-mode "Sessions"
  "Major mode of OpenCode session lists.
\\{opencode-session-list-mode-map}"
  (setq-local truncate-lines t
              revert-buffer-function (lambda (&rest _) (opencode-session-list-refresh))))

(defvar-local opencode--session-list-scope nil
  "What this list shows: `all', or (DIRECTORY . PROJECT-ID-OR-NIL).")

(defvar-local opencode--session-list-sessions nil
  "Sessions this list last fetched.")

(defvar-local opencode--session-list-projects nil
  "Project IDs mapped to names, for the Project column.")

(defvar-local opencode--session-list-subagents nil
  "Non-nil when this list shows subagent sessions too.")

(defvar-local opencode--session-list-width nil
  "Body width of the window this list was rendered for, or nil when hidden.")

(defun opencode--session-list-age (seconds)
  "Format SECONDS since a change; unknown times are infinite and show as -."
  (if (and (numberp seconds) (< seconds 1.0e+INF)) (opencode-format-time-ago seconds) "-"))

(defun opencode--session-list-time (info key)
  "Return the seconds since INFO's time KEY, infinite when unknown.
A sortable number, never nil: vtable would propertize the name of nil."
  (or (opencode-time-ago (alist-get key (alist-get 'time info))) 1.0e+INF))

(defun opencode--session-list-columns ()
  "Return the columns of this list."
  `((:name "Title" :max-width "45%")
    ,@(when (eq opencode--session-list-scope 'all)
        '((:name "Project" :min-width 8 :max-width "12%")))
    (:name "Branch" :min-width 6 :max-width ,(if (eq opencode--session-list-scope 'all) "10%" "12%"))
    (:name "Last Updated" :width 12 :formatter opencode--session-list-age :primary ascend)
    (:name "Created" :width 10 :formatter opencode--session-list-age)))

(defun opencode--session-list-value (info column table)
  "Return the cell of session INFO in COLUMN of TABLE."
  (pcase (vtable-column table column)
    ("Title" (let ((title (or (alist-get 'title info) (alist-get 'id info))))
               (if (buffer-live-p (gethash (alist-get 'id info) opencode-session-buffers))
                   (propertize title 'face 'opencode-session-list-open-session)
                 title)))
    ("Project" (or (gethash (alist-get 'projectID info) opencode--session-list-projects) "-"))
    ("Branch" (or (opencode-git-branch (opencode--session-directory info)) "-"))
    ("Last Updated" (opencode--session-list-time info 'updated))
    ("Created" (opencode--session-list-time info 'created))))

(defun opencode--session-list-render ()
  "Render this buffer's sessions as a table sized to its window."
  (let* ((inhibit-read-only t)
         (at (ignore-errors (vtable-current-object)))
         (sessions (if opencode--session-list-subagents
                       opencode--session-list-sessions
                     (seq-remove (lambda (info) (alist-get 'parentID info))
                                 opencode--session-list-sessions))))
    (erase-buffer)
    (if (null sessions)
        (insert (if (consp opencode--session-list-scope)
                    (format "No sessions in %s" (car opencode--session-list-scope))
                  "No sessions"))
      (make-vtable
       :columns (opencode--session-list-columns)
       :objects sessions
       :getter #'opencode--session-list-value
       :actions '("RET" opencode-session-list-open
                  "o" opencode-session-list-open
                  "x" opencode-session-list-delete
                  "R" opencode-session-list-rename)
       :separator-width 2
       :keymap opencode-session-list-mode-map)
      (if at
          (ignore-errors (vtable-goto-object at))
        (goto-char (point-min))))
    (setq opencode--session-list-width
          (when-let* ((window (get-buffer-window nil t)))
            (window-body-width window)))))

(defun opencode-session-list-refresh (&optional callback)
  "Fetch this list's sessions again and render them, then call CALLBACK."
  (interactive)
  (let ((buffer (current-buffer))
        (scope opencode--session-list-scope)
        (pending 2)
        sessions projects)
    (cl-flet ((done ()
                (when (and (zerop (cl-decf pending)) (buffer-live-p buffer))
                  (with-current-buffer buffer
                    (setq opencode--session-list-sessions sessions
                          opencode--session-list-projects (make-hash-table :test #'equal))
                    (dolist (project projects)
                      (puthash (alist-get 'id project) (opencode-project-name project)
                               opencode--session-list-projects))
                    (opencode--session-list-render)
                    (when callback (funcall callback))))))
      (opencode-fetch-projects (lambda (data) (setq projects data) (done)))
      (opencode-fetch-sessions
       (pcase scope
         ('all nil)
         (`(,_directory . ,(and project (pred stringp))) `((project . ,project)))
         (`(,directory . ,_) `((directory . ,(opencode-directory-name directory)))))
       (lambda (data) (setq sessions data) (done))))))

(defun opencode--session-list-show (buffer)
  "Display list BUFFER, rendering it again for its window's width."
  (pop-to-buffer buffer)
  (with-current-buffer buffer
    (unless (equal opencode--session-list-width (window-body-width))
      (opencode--session-list-render))))

(defun opencode--session-list-open-buffer (name scope)
  "Show the list buffer NAME of SCOPE, creating and fetching it when needed."
  (opencode-events-connect)
  (let ((buffer (get-buffer name)))
    (if (buffer-live-p buffer)
        (opencode--session-list-show buffer)
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (opencode-session-list-mode)
        (setq opencode--session-list-scope scope
              default-directory (if (consp scope) (car scope) (expand-file-name "~/")))
        (opencode-session-list-refresh (lambda () (opencode--session-list-show buffer)))))))

(defun opencode (&optional directory)
  "List the sessions of DIRECTORY's project, the current one by default."
  (interactive)
  (let ((directory (opencode-project-directory directory)))
    (opencode-fetch-projects
     (lambda (projects)
       (let ((project (opencode-project-for-directory projects directory)))
         (opencode--session-list-open-buffer
          (format "*OpenCode Sessions in %s*" (abbreviate-file-name directory))
          (cons directory (alist-get 'id project))))))))

(defun opencode-list-all-sessions ()
  "List the sessions of every project."
  (interactive)
  (opencode--session-list-open-buffer "*OpenCode All Sessions*" 'all))

(defun opencode-select-project ()
  "List the sessions of a project the service knows, most recently active first."
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

(defun opencode-select-session ()
  "Open a session of the current project."
  (interactive)
  (let* ((directory (opencode-project-directory))
         (project (opencode-project-for-directory (opencode-wait #'opencode-fetch-projects)
                                                  directory)))
    (opencode--select-session
     "Session: "
     (opencode-wait (lambda (then else)
                      (opencode-fetch-sessions
                       `((parentID . "null")
                         ,(if project
                              `(project . ,(alist-get 'id project))
                            `(directory . ,(opencode-directory-name directory))))
                       then else))))))

(defun opencode-session-list-open (info)
  "Open session INFO in the selected window."
  (opencode-session-open info :other-window nil))

(defun opencode-session-list-delete (info)
  "Delete session INFO after confirmation."
  (opencode-delete-session (alist-get 'id info)))

(defun opencode-session-list-rename (info)
  "Rename session INFO."
  (opencode-rename-session (alist-get 'id info)
                           (read-string "Title: " (alist-get 'title info))))

(defun opencode-session-list-new ()
  "Create a session in this list's directory."
  (interactive)
  (opencode-session-create :directory default-directory))

(defun opencode-session-list-toggle-subagents ()
  "Show or hide subagent sessions in this list."
  (interactive)
  (setq opencode--session-list-subagents (not opencode--session-list-subagents))
  (opencode--session-list-render)
  (message (if opencode--session-list-subagents "Showing subagents" "Hiding subagents")))

(defun opencode--session-list-buffers ()
  "Return the live session list buffers."
  (seq-filter (lambda (buffer)
                (eq (buffer-local-value 'major-mode buffer) 'opencode-session-list-mode))
              (buffer-list)))

(defun opencode--session-list-update-buffer (id)
  "Update session ID's row in each list after its buffer opens or closes."
  (dolist (buffer (opencode--session-list-buffers))
    (with-current-buffer buffer
      (let ((at (vtable-current-object))
            (offset (- (point) (line-beginning-position))))
        (save-excursion
          (goto-char (point-min))
          (when-let* ((table (vtable-current-table))
                      (info (seq-find (lambda (info) (equal (alist-get 'id info) id))
                                      (vtable-objects table))))
            (vtable-update-object table info)))
        (when at
          (vtable-goto-object at)
          (forward-char (min offset (- (line-end-position) (point)))))))))

(defun opencode--session-list-buffer-changed (id)
  "Schedule a local update of session ID's row in each list."
  ;; `kill-buffer' restores an excursion that row replacement would collapse.
  (when (opencode--session-list-buffers)
    (run-at-time 0 nil #'opencode--session-list-update-buffer id)))

(add-hook 'opencode-session-buffer-functions #'opencode--session-list-buffer-changed)

(defvar opencode--session-list-refresh-timer nil
  "Timer refreshing the session lists after sessions changed.")

(defun opencode--session-list-changed (_event)
  "Refresh every session list shortly, once a burst of session events ends."
  (when (opencode--session-list-buffers)
    (when (timerp opencode--session-list-refresh-timer)
      (cancel-timer opencode--session-list-refresh-timer))
    (setq opencode--session-list-refresh-timer
          (run-with-timer 1 nil (lambda ()
                                  (setq opencode--session-list-refresh-timer nil)
                                  (dolist (buffer (opencode--session-list-buffers))
                                    (with-current-buffer buffer
                                      (opencode-session-list-refresh))))))))

(dolist (type '("session.created" "session.deleted" "session.renamed" "session.moved" "session.forked"))
  (opencode-events-register type #'opencode--session-list-changed))

(defun opencode--session-lists-resize (frame)
  "Render the session lists shown in FRAME again where their width changed."
  (dolist (window (window-list frame 'no-minibuffer))
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'opencode-session-list-mode)
                 opencode--session-list-sessions
                 (not (equal (window-body-width window) opencode--session-list-width)))
        (with-selected-window window
          (opencode--session-list-render))))))

(add-hook 'window-size-change-functions #'opencode--session-lists-resize)

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
