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
(require 'opencode-look)
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

;;;; Full-text search

(defface opencode-search-match '((t :inherit match))
  "Face of the words a search matched in its snippets."
  :group 'opencode)

(defcustom opencode-search-snippet-length 320
  "Most characters of a search snippet, the service's own bound."
  :type 'natnum
  :group 'opencode)

(defvar opencode-search--unsupported nil
  "IDs of services without the search route, which only my fork has.")

(defun opencode-search--snippet (snippet)
  "Return SNIPPET, the service's text and match ranges, as one propertized line.
Matches get the face `opencode-search-match', and their ranges are
returned too, as (START . END) pairs of character positions, as the
cons (TEXT . RANGES).  The text is cut to `opencode-search-snippet-length'."
  (let* ((text (replace-regexp-in-string "[\n\r\t]" " " (or (alist-get 'text snippet) "")))
         (limit opencode-search-snippet-length)
         (cut (if (> (length text) limit) (concat (substring text 0 (max 0 (1- limit))) "…") text))
         (end (if (> (length text) limit) (max 0 (1- limit)) (length text)))
         (ranges nil))
    (dolist (match (alist-get 'matches snippet))
      (let ((start (alist-get 'start match))
            (stop (alist-get 'end match)))
        (when (and (natnump start) (natnump stop) (< start stop) (<= stop end))
          (add-face-text-property start stop 'opencode-search-match nil cut)
          (push (cons start stop) ranges))))
    (cons cut (nreverse ranges))))

(defun opencode-search--match (match)
  "Return the plist describing MATCH, one match of the service's answer."
  (let ((snippet (opencode-search--snippet (alist-get 'snippet match))))
    (list :session-id (alist-get 'sessionID match)
          :title (alist-get 'title match)
          :message-id (alist-get 'messageID match)
          :role (intern (alist-get 'role match))
          :time (when-let* ((created (alist-get 'created (alist-get 'time match))))
                  (/ created 1000.0))
          :snippet (car snippet)
          :matches (cdr snippet))))

(defun opencode-search (query &rest arguments)
  "Search the text of every session's messages for QUERY, asynchronously.
QUERY holds words, all of which a message must contain; double quotes
group a phrase, and a word ending in * matches as a prefix.  Only user
prompts and the visible text of assistant messages are searched, not
tool calls, their output or reasoning.

ARGUMENTS are keywords:

  :then     called with MATCHES and INFO once the service answers.
            MATCHES is a list of plists, best match first:
              :session-id  the session's ID
              :title       its title, or nil
              :message-id  the matching message's ID
              :role        `user' or `assistant'
              :time        when the message was created, in float seconds
              :snippet     one line of at most `opencode-search-snippet-length'
                           characters around the match, the matched words
                           in the face `opencode-search-match'
              :matches     those words' (START . END) positions in :snippet
            INFO is a plist: :next, the cursor of the following page or
            nil on the last; :complete, nil while the service is still
            indexing the history it held before it could search, when only
            what is indexed so far can match; and then :processed and
            :total, how many of those messages it went through.
  :else     called with REASON and MESSAGE when the search fails.
            REASON is `unsupported' when the service has no search
            route (an older build, or not my fork), so callers can skip
            it silently; `invalid' when it refused the query or cursor;
            `failed' otherwise.  MESSAGE describes the failure.  Without
            :else, failures other than `unsupported' are reported.
  :archived t searches archived sessions only, nil unarchived ones only;
            omitted, both.
  :project  a project ID; :directory a session directory; :parent a
            session ID, whose subagents are searched, or `root' for
            top-level sessions only.
  :limit    the most matches per page, at most 100; 20 by default.
  :cursor   the :next of an earlier page of the same search.

THEN and ELSE run in the buffer current at the call when it still lives,
at most one of them once, and neither once the search is cancelled.  Return a function that cancels the
search when called with no argument; it does nothing once the search
settled."
  (let* ((then (plist-get arguments :then))
         (else (plist-get arguments :else))
         (origin (current-buffer))
         (cancelled nil)
         (exchange nil)
         (service (plist-get opencode-service-connection :id))
         (query-alist
          `((q . ,query)
            ,@(when (plist-member arguments :archived)
                `((archived . ,(if (plist-get arguments :archived) "true" "false"))))
            ,@(when-let* ((project (plist-get arguments :project))) `((project . ,project)))
            ,@(when-let* ((directory (plist-get arguments :directory)))
                `((directory . ,(opencode-directory-name directory))))
            ,@(when-let* ((parent (plist-get arguments :parent)))
                `((parentID . ,(if (eq parent 'root) "null" parent))))
            ,@(when-let* ((limit (plist-get arguments :limit))) `((limit . ,limit)))
            ,@(when-let* ((cursor (plist-get arguments :cursor))) `((cursor . ,cursor))))))
    (cl-flet ((deliver (function &rest values)
                (unless cancelled
                  (setq cancelled t)
                  (when function
                    (if (buffer-live-p origin)
                        (with-current-buffer origin (apply function values))
                      (apply function values)))))
              (fail (reason message)
                (list (cond (else)
                            ((eq reason 'unsupported) nil)
                            (t (lambda (_reason message)
                                 (opencode-report-error (concat "OpenCode search: " message)))))
                      reason message)))
      (if (and service (member service opencode-search--unsupported))
          ;; Known to lack the route: fail as it would, after returning.
          (run-at-time 0 nil (lambda ()
                               (apply #'deliver (fail 'unsupported "this OpenCode service cannot search sessions"))))
        (setq exchange
              (opencode-request
               'get "/api/experimental/session/search" :directory nil :buffer nil :query query-alist
               :then (lambda (response)
                       (let ((index (alist-get 'index response)))
                         (deliver then
                                  (mapcar #'opencode-search--match (opencode-request-data response))
                                  (list :next (alist-get 'next (alist-get 'cursor response))
                                        :complete (and (alist-get 'complete index) t)
                                        :processed (alist-get 'processed index)
                                        :total (alist-get 'total index)))))
               :else (lambda (error)
                       (let ((status (opencode-error-status error)))
                         (apply #'deliver
                                (cond
                                 ;; An unknown route answers a 404 without an error tag.
                                 ((and (eql status 404) (null (opencode-error-tag error)))
                                  (when-let* ((id (plist-get opencode-service-connection :id)))
                                    (cl-pushnew id opencode-search--unsupported :test #'equal))
                                  (fail 'unsupported "this OpenCode service cannot search sessions"))
                                 ((eql status 400) (fail 'invalid (opencode-error-describe error)))
                                 (t (fail 'failed (opencode-error-describe error))))))))))
      (lambda ()
        (unless cancelled
          (setq cancelled t)
          (when (and (opencode-transport-exchange-p exchange) (opencode-transport-live-p exchange))
            (opencode-transport-cancel exchange)))))))

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
prefix is the session's status icon, then the archived icon for an
archived session (see `opencode-session-status-icons' and
`opencode-archived-icon'); its name the title, or the ID of an untitled
session, in the face of where the session is (see
`opencode-session-place'); its annotation the project, the branch and
the time since the session last changed, in columns.  A column no
session has a value for is left out.  The sort key puts the newest
first."
  (let* ((now (float-time))
         (rows (mapcar (lambda (info)
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
                (let* ((id (plist-get entry :id))
                       (title (plist-get entry :title))
                       (title (if (or (null title) (string-empty-p title)) id title))
                       (face (opencode-session-place-face (opencode-session-place id)))
                       (updated (plist-get entry :updated)))
                  (list (if face (propertize title 'face face) title)
                        info
                        (concat (column project project-width)
                                (column branch branch-width)
                                (if updated (opencode-format-age updated now) "-"))
                        (- (or updated 0))
                        (concat (opencode-session-status-icon (opencode-state-status entry)) " "
                                (and (plist-get entry :archived)
                                     (concat (propertize (opencode-archived-glyph) 'help-echo "Archived")
                                             " "))))))
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
                                  (if active (opencode-format-age (opencode-time-seconds active)) "-")
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
