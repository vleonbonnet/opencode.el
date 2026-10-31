;;; opencode-sessions-test.el --- Tests of session lists  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(defconst opencode-test-project-id "8e4c4b8195f77193ad57baf81b123929670d42cf"
  "Project ID of the recorded sessions.")

(defun opencode-test-recorded-sessions ()
  "Return the recorded session infos, newest first."
  (let (sessions)
    (dolist (file (directory-files (expand-file-name "fixtures/v2" opencode-test-directory)
                                   nil "\\`session-ses_.*\\.json\\'"))
      (when-let* ((info (alist-get 'data (opencode-test-fixture file))))
        (push info sessions)))
    (sort sessions :key (lambda (info) (alist-get 'updated (alist-get 'time info))) :reverse t)))

(defun opencode-test-projects ()
  "Return a project list holding the recorded sessions' project."
  `(((id . ,opencode-test-project-id)
     (canonical . ,(directory-file-name opencode-test-workspace))
     (time (created . 1) (updated . 2) (active . 3))
     (sandboxes))
    ((id . "other") (canonical . ,(directory-file-name temporary-file-directory))
     (time (created . 1) (updated . 1)) (sandboxes))))

(defun opencode-test-session-routes (sessions)
  "Return routes listing SESSIONS two per page, and the test projects.
As the service does, a cursor carries the filters of the first page,
and filters sent with it are ignored."
  (list
   (list 'get "\\`/api/project\\'" (lambda (_) (opencode-test-projects)))
   (list 'get "\\`/api/session\\'"
         (lambda (request)
           (let* ((cursor (and (alist-get 'cursor (plist-get request :query))
                               (read (alist-get 'cursor (plist-get request :query)))))
                  (query (if cursor (cdr cursor) (plist-get request :query)))
                  (start (or (car cursor) 0))
                  (matching (seq-filter (lambda (info)
                                          (and (or (not (alist-get 'project query))
                                                   (equal (alist-get 'projectID info) (alist-get 'project query)))
                                               (or (not (equal (alist-get 'parentID query) "null"))
                                                   (not (alist-get 'parentID info)))))
                                        sessions)))
             `((data . ,(seq-take (nthcdr start matching) 2))
               ;; The server always offers a next page, even past the end.
               (cursor (next . ,(prin1-to-string
                                 (cons (+ start 2)
                                       (seq-filter (lambda (pair) (memq (car pair) '(project parentID)))
                                                   query)))))))))))

(defmacro opencode-test-with-sessions (&rest body)
  "Run BODY with the recorded sessions served."
  (declare (indent 0) (debug t))
  `(opencode-test-with-server
     (let ((opencode-test-routes (append (opencode-test-session-routes (opencode-test-recorded-sessions))
                                         opencode-test-routes)))
       ,@body)))

(defun opencode-test-list-columns ()
  "Return the column names of the table at point."
  (mapcar #'vtable-column-name (vtable-columns (vtable-current-table))))

(defun opencode-test-make-head (directory content &optional gitdir)
  "Write HEAD CONTENT for a repository at DIRECTORY, whose Git directory is GITDIR."
  (let ((gitdir (or gitdir (expand-file-name ".git" directory))))
    (make-directory gitdir t)
    (with-temp-file (expand-file-name "HEAD" gitdir) (insert content))))

(ert-deftest opencode-test-git-branch-from-head ()
  "Branches are read from HEAD, in repositories and in linked worktrees."
  (let* ((root (file-name-as-directory (make-temp-file "opencode-git-" t)))
         (main (expand-file-name "main/" root))
         (linked (expand-file-name "linked/" root))
         (plain (expand-file-name "plain/" root))
         (opencode--branches (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (opencode-test-make-head main "ref: refs/heads/feature/x\n")
          (make-directory (expand-file-name "src" main))
          (opencode-test-make-head nil "ref: refs/heads/side\n"
                                   (expand-file-name ".git/worktrees/linked" main))
          (make-directory linked)
          (with-temp-file (expand-file-name ".git" linked)
            (insert "gitdir: ../main/.git/worktrees/linked\n"))
          (make-directory plain)
          (should (equal (opencode--read-git-branch (expand-file-name "src" main)) "feature/x"))
          (should (equal (opencode--read-git-branch linked) "side"))
          (opencode-test-make-head main "0123456789abcdef0123456789abcdef01234567\n")
          (should (equal (opencode--read-git-branch main) "0123456"))
          (should-not (let ((default-directory plain))
                        (and (not (locate-dominating-file plain ".git"))
                             (opencode--read-git-branch plain))))
          ;; Cached until the TTL passes.
          (should (equal (opencode-git-branch linked) "side"))
          (opencode-test-make-head nil "ref: refs/heads/renamed\n"
                                   (expand-file-name ".git/worktrees/linked" main))
          (should (equal (opencode-git-branch linked) "side"))
          (let ((opencode-session-list-branch-ttl 0))
            (should (equal (opencode-git-branch linked) "renamed")))
          (should-not (opencode-git-branch (expand-file-name "missing/" root))))
      (delete-directory root t))))

(ert-deftest opencode-test-project-for-directory ()
  "The innermost project or sandbox containing a directory is chosen."
  (let* ((root (file-name-as-directory (make-temp-file "opencode-projects-" t)))
         (inner (expand-file-name "repo/" root))
         (sandbox (expand-file-name "worktree/" root)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "lib" inner) t)
          (make-directory sandbox)
          (let ((projects `(((id . "outer") (canonical . ,root) (sandboxes))
                            ((id . "repo") (canonical . ,inner) (sandboxes ,sandbox)))))
            (should (equal (alist-get 'id (opencode-project-for-directory
                                           projects (expand-file-name "lib" inner)))
                           "repo"))
            (should (equal (alist-get 'id (opencode-project-for-directory projects sandbox)) "repo"))
            (should (equal (alist-get 'id (opencode-project-for-directory projects root)) "outer"))
            (should-not (opencode-project-for-directory projects temporary-file-directory))))
      (delete-directory root t))))

(ert-deftest opencode-test-fetch-sessions-pages ()
  "Every page is fetched until one comes back empty."
  (opencode-test-with-sessions
    (let (result)
      (opencode-fetch-sessions `((project . ,opencode-test-project-id) (parentID . "null"))
                               (lambda (sessions) (setq result sessions)))
      (should (equal (mapcar (lambda (info) (alist-get 'id info)) result)
                     (seq-keep (lambda (info) (and (not (alist-get 'parentID info)) (alist-get 'id info)))
                               (opencode-test-recorded-sessions))))
      (let ((pages (reverse opencode-test-requests)))
        (should (> (length pages) 1))
        (let ((query (plist-get (car pages) :query)))
          (should (equal (alist-get 'order query) "desc"))
          (should (equal (alist-get 'project query) opencode-test-project-id)))
        ;; Later pages carry only the cursor, which holds the filters.
        (dolist (page (cdr pages))
          (should (equal (mapcar #'car (plist-get page :query)) '(limit cursor))))))))

(ert-deftest opencode-test-opencode-lists-the-project ()
  "`opencode' lists the project of the current directory by its ID."
  (opencode-test-with-sessions
    (let (opened)
      (cl-letf (((symbol-function 'opencode--session-list-open-buffer)
                 (lambda (name scope) (setq opened (list name scope)))))
        (opencode opencode-test-workspace)
        (should (string-prefix-p "*OpenCode Sessions in " (car opened)))
        (should (equal (cdr (cadr opened)) opencode-test-project-id))
        (opencode temporary-file-directory)
        (should (equal (cdr (cadr opened)) "other"))))))

(ert-deftest opencode-test-session-list-render ()
  "A list shows its sessions' titles and hides subagents until asked."
  (opencode-test-with-sessions
    (with-temp-buffer
      (opencode-session-list-mode)
      (setq opencode--session-list-scope (cons opencode-test-workspace opencode-test-project-id))
      (opencode-session-list-refresh)
      (let ((text (buffer-string))
            (done (regexp-quote (opencode-session-status-icon 'done)))
            (child (regexp-quote (opencode-test-session-title (opencode-test-scenario "subagent-child")))))
        ;; Finished turns nobody viewed show as done.
        (should (string-match-p (concat "^" done " +history renamed ") text))
        (should (string-match-p (concat "^" done " +reasoning ") text))
        (should-not (string-match-p child text))
        (goto-char (point-min))
        (should (equal (opencode-test-list-columns) '("" "Title" "Branch" "Last Updated" "Created")))
        (opencode-session-list-toggle-subagents)
        (should (string-match-p child (buffer-string))))
      (goto-char (point-min))
      (forward-line 1)
      (should (alist-get 'id (vtable-current-object))))))

(ert-deftest opencode-test-session-list-all-projects ()
  "The all-projects list names each session's project."
  (opencode-test-with-sessions
    (with-temp-buffer
      (opencode-session-list-mode)
      (setq opencode--session-list-scope 'all)
      (opencode-session-list-refresh)
      (goto-char (point-min))
      (should (member "Project" (opencode-test-list-columns)))
      (should (equal (gethash opencode-test-project-id opencode--session-list-projects)
                     (file-name-nondirectory (directory-file-name opencode-test-workspace))))
      ;; Narrow batch windows truncate the column to its 12%.
      (should (string-match-p (concat "^" (regexp-quote (opencode-session-status-icon 'done))
                                      " +reasoning opencode")
                              (buffer-string))))))

(defun opencode-test-session-list-highlighted-p (info)
  "Return non-nil when INFO's title is highlighted in the current list."
  (save-excursion
    (goto-char (point-min))
    (let ((title (seq-position (opencode-test-list-columns) "Title")))
      (should (vtable-goto-object info))
      (should (vtable-goto-column title))
      (memq 'opencode-session-list-open-session
            (ensure-list (get-text-property (point) 'face))))))

(ert-deftest opencode-test-session-list-follows-open-buffers ()
  "Open and close update both list scopes by ID, preserving table navigation."
  (opencode-test-with-sessions
    (dolist (scope (list 'all (cons opencode-test-workspace opencode-test-project-id)))
      (with-temp-buffer
        (opencode-session-list-mode)
        (setq opencode--session-list-scope scope)
        (opencode-session-list-refresh)
        (let* ((id (opencode-test-scenario "text"))
               (info (seq-find (lambda (info) (equal (alist-get 'id info) id))
                               opencode--session-list-sessions))
               (other (seq-find (lambda (info) (equal (alist-get 'id info)
                                                      (opencode-test-scenario "reasoning")))
                                opencode--session-list-sessions)))
          ;; Equal titles must not make both rows look open.
          (setf (alist-get 'title info) "Same title"
                (alist-get 'title other) "Same title")
          (opencode--session-list-render)
          (vtable-sort-by-current-column)
          (vtable-narrow-current-column)
          (let* ((table (vtable-current-table))
                 (sort (copy-tree (vtable-sort-by table)))
                 (width (vtable-column-width (car (vtable-columns table)))))
            (should-not (opencode-test-session-list-highlighted-p info))
            (vtable-goto-object other)
            (forward-char 3)
            (setq opencode-test-requests nil)
            (let ((buffer (opencode-test-open id)))
              (accept-process-output nil 0.01)
              (should-not (get-buffer-window buffer t))
              (with-current-buffer buffer (rename-buffer "*renamed session*" t))
              (should (eq (vtable-current-object) other))
              (should (= (- (point) (line-beginning-position)) 3))
              (should (opencode-test-session-list-highlighted-p info))
              (should-not (opencode-test-session-list-highlighted-p other))
              (should-not (text-properties-at 0 (alist-get 'title info)))
              (should-not (seq-find (lambda (request)
                                     (member (plist-get request :path) '("/api/session" "/api/project")))
                                   opencode-test-requests))
              (vtable-goto-object info)
              (forward-char 2)
              (setq opencode-test-requests nil)
              (kill-buffer buffer)
              (accept-process-output nil 0.01)
              (should-not opencode-test-requests)
              (should (eq (vtable-current-object) info))
              (should (= (- (point) (line-beginning-position)) 2))
              (should-not (opencode-test-session-list-highlighted-p info)))
            (should (eq (vtable-current-table) table))
            (should (equal (vtable-sort-by table) sort))
            (should (equal (vtable-column-width (car (vtable-columns table))) width))))))))

(ert-deftest opencode-test-session-list-existing-and-dead-buffers ()
  "Initial rendering highlights live buffers, including ID-only titles, not dead ones."
  (opencode-test-with-sessions
    (let* ((id (opencode-test-scenario "text"))
           (open (opencode-test-open id)))
      (with-temp-buffer
        (opencode-session-list-mode)
        (setq opencode--session-list-scope 'all)
        (opencode-session-list-refresh)
        (let* ((info (seq-find (lambda (info) (equal (alist-get 'id info) id))
                               opencode--session-list-sessions))
               (other (seq-find (lambda (info) (not (equal (alist-get 'id info) id)))
                                (vtable-objects (vtable-current-table)))))
          (puthash (alist-get 'id other) (with-temp-buffer (current-buffer)) opencode-session-buffers)
          (setf (alist-get 'title info) nil)
          (opencode--session-list-render)
          (should (opencode-test-session-list-highlighted-p info))
          (should-not (opencode-test-session-list-highlighted-p other))
          (opencode-session-list-toggle-subagents)
          (should (opencode-test-session-list-highlighted-p info))
          (kill-buffer open)
          (accept-process-output nil 0.01)
          (should-not (opencode-test-session-list-highlighted-p info)))))))

(ert-deftest opencode-test-session-list-refreshes-after-events ()
  "A burst of session events refreshes the open lists once."
  (opencode-test-with-sessions
    (let ((timers nil)
          (opencode--session-list-refresh-timer nil))
      (with-temp-buffer
        (opencode-session-list-mode)
        (setq opencode--session-list-scope 'all)
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat function) (let ((timer (timer-create)))
                                                       (timer-set-function timer function)
                                                       (push timer timers)
                                                       timer))))
          (opencode-test-dispatch '(((type . "session.created") (data (sessionID . "ses_a")))
                                    ((type . "session.renamed") (data (sessionID . "ses_a")))))
          (should (= (length timers) 2))
          (should (eq opencode--session-list-refresh-timer (car timers)))
          (setq opencode-test-requests nil)
          (funcall (timer--function (car timers)))
          (should (seq-find (lambda (request) (equal (plist-get request :path) "/api/session"))
                            opencode-test-requests))
          (should opencode--session-list-sessions))))))

(ert-deftest opencode-test-session-list-status ()
  "The Status column shows the registry's status, and a change redraws only that row."
  (opencode-test-with-sessions
    (let ((timers nil)
          (opencode--session-list-refresh-timer nil)
          (opencode--session-list-pending nil))
      (with-temp-buffer
        (opencode-session-list-mode)
        (setq opencode--session-list-scope 'all)
        (opencode-session-list-refresh)
        ;; Archived sessions are left out; later pages carry the filter in their cursor.
        (should (equal (alist-get 'archived (plist-get (seq-find (lambda (request)
                                                                    (equal (plist-get request :path) "/api/session"))
                                                                  (reverse opencode-test-requests))
                                                        :query))
                       "false"))
        (opencode-test-dispatch '(((type . "server.connected") (data))))
        (should (opencode-state-ready-p))
        (let* ((info (car (vtable-objects (vtable-current-table))))
               (id (alist-get 'id info)))
          (should (opencode-state-get id))
          (cl-flet ((status-cell (&optional (status 'done))
                      (save-excursion
                        (goto-char (point-min))
                        (should (vtable-goto-object info))
                        (should (vtable-goto-column 0))
                        (list (string-trim (buffer-substring-no-properties
                                            (point) (next-single-property-change (point) 'vtable-column)))
                              (car (memq (get-text-property 0 'face (opencode-session-status-icon status))
                                         (ensure-list (get-text-property (point) 'face))))))))
            (should (equal (status-cell) (list (opencode-session-status-icon 'done) 'opencode-session-status-done)))
            (cl-letf (((symbol-function 'run-with-timer)
                       (lambda (_delay _repeat function)
                         (let ((timer (timer-create)))
                           (timer-set-function timer function)
                           (push timer timers)
                           timer))))
              (opencode-test-dispatch `(((type . "session.execution.started") (created . 1)
                                         (data (sessionID . ,id)))))
              (should (= (length timers) 1))
              (setq opencode-test-requests nil)
              (funcall (timer--function (car timers)))
              ;; Only the row is drawn again; nothing is fetched.
              (should-not opencode-test-requests)
              (should (equal (status-cell 'working)
                             (list (opencode-session-status-icon 'working) 'opencode-session-status-working)))
              ;; Archiving or restoring a session refreshes the lists.
              (opencode-test-dispatch `(((type . "session.archived") (created . 2) (data (sessionID . ,id)))))
              (should (= (length timers) 2))
              (should (eq opencode--session-list-pending t)))))))))

(ert-deftest opencode-test-session-status-icons ()
  "Every status has a one-glyph icon in its own face, named by its tooltip."
  (dolist (status '(waiting working retrying failed done idle))
    (let ((icon (opencode-session-status-icon status)))
      (should (= (length icon) 1))
      (should (eq (get-text-property 0 'face icon)
                  (intern (format "opencode-session-status-%s" status))))
      (should (facep (get-text-property 0 'face icon)))
      (should (equal (get-text-property 0 'help-echo icon) (symbol-name status)))))
  (should (equal (opencode-session-status-icon nil) "-")))

(ert-deftest opencode-test-session-list-status-column-width ()
  "The status column is unnamed and as wide as the widest icon, two columns for ⚑."
  (let ((column (car (let ((opencode--session-list-scope 'all)) (opencode--session-list-columns)))))
    (should (equal (plist-get column :name) ""))
    (should (= (plist-get column :width) 2)))
  (let ((opencode-session-status-icons '((done "✓" opencode-session-status-done))))
    (should (= (plist-get (car (opencode--session-list-columns)) :width) 1))))

(ert-deftest opencode-test-session-list-archive ()
  "A archives the session at point once confirmed."
  (opencode-test-with-sessions
    (with-temp-buffer
      (opencode-session-list-mode)
      (setq opencode--session-list-scope 'all)
      (opencode-session-list-refresh)
      (goto-char (point-min))
      (let* ((info (vtable-current-object))
             (command (keymap-lookup (get-char-property (point) 'keymap) "A"))
             (answer nil)
             (asked nil))
        (should command)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (setq asked prompt) answer)))
          (setq opencode-test-requests nil)
          (call-interactively command)
          (should (string-match-p (regexp-quote (alist-get 'title info)) asked))
          (should-not opencode-test-requests)
          (setq answer t)
          (call-interactively command))
        (should (equal (mapcar (lambda (request) (list (plist-get request :method) (plist-get request :path)
                                                       (plist-get request :body)))
                               opencode-test-requests)
                       `((patch ,(concat "/api/session/" (alist-get 'id info)) ((archived . t))))))))))

(ert-deftest opencode-test-session-lists-capital-drive ()
  "Lists of a directory outside any project filter on it with a capital drive letter."
  (skip-unless (memq system-type '(windows-nt ms-dos)))
  (opencode-test-with-sessions
    (let ((outside (opencode-test-lowercase-drive
                    (file-name-directory (directory-file-name temporary-file-directory)))))
      ;; Requests are recorded newest first; later pages carry only the cursor.
      (cl-flet ((filtered ()
                  (alist-get 'directory
                             (plist-get (seq-find (lambda (request)
                                                    (equal (plist-get request :path) "/api/session"))
                                                  (reverse opencode-test-requests))
                                        :query))))
        (with-temp-buffer
          (opencode-session-list-mode)
          (setq opencode--session-list-scope (cons outside nil))
          (setq opencode-test-requests nil)
          (opencode-session-list-refresh)
          (should (opencode-test-capital-drive-p (filtered))))
        (setq opencode-test-requests nil)
        (cl-letf (((symbol-function 'opencode-project-directory) (lambda () outside))
                  ((symbol-function 'opencode--select-session) #'ignore))
          (opencode-select-session))
        (should (opencode-test-capital-drive-p (filtered)))))))

(ert-deftest opencode-test-toggle-mcp ()
  "A connected server is disconnected and a disabled one connected."
  (opencode-test-with-server
    (let ((opencode-test-routes
           (cons (list 'get "\\`/api/mcp\\'"
                       (lambda (_) '((data ((name . "slack") (status (status . "connected")))
                                           ((name . "anvil eval") (status (status . "disabled")))))))
                 opencode-test-routes))
          (choice nil))
      (cl-letf (((symbol-function 'opencode-annotated-completion)
                 (lambda (_prompt candidates)
                   (should (equal (nth 2 (car candidates)) "🟢 connected"))
                   (cadr (assoc choice candidates)))))
        (setq choice "slack")
        (opencode-toggle-mcp)
        (should (equal (plist-get (car opencode-test-requests) :path)
                       "/api/experimental/mcp/slack/disconnect"))
        (setq choice "anvil eval")
        (opencode-toggle-mcp)
        (should (equal (plist-get (car opencode-test-requests) :path)
                       "/api/experimental/mcp/anvil%20eval/connect"))))))

(ert-deftest opencode-test-new-worktree ()
  "A worktree is added on a new branch and a session created in it."
  (opencode-test-with-server
    (let ((default-directory opencode-test-workspace)
          (opencode-worktree-directory (expand-file-name "trees/" temporary-file-directory))
          (git nil)
          (created nil))
      (cl-letf (((symbol-function 'process-file)
                 (lambda (program _infile _buffer _display &rest args)
                   (setq git (cons program args))
                   0))
                ((symbol-function 'opencode-session-create)
                 (lambda (&rest arguments) (setq created arguments))))
        (opencode-new-worktree "topic")
        (should (equal git (list "git" "worktree" "add" "-b" "topic"
                                 (expand-file-name "topic" opencode-worktree-directory) "HEAD")))
        (should (equal created (list :directory (expand-file-name "topic" opencode-worktree-directory))))))))

(provide 'opencode-sessions-test)
;;; opencode-sessions-test.el ends here
