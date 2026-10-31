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

(defun opencode-test-list-title-cell (info)
  "Return the Title cell of session INFO in the current list, with its properties.
The space padding the cell to the column's width is left out."
  (save-excursion
    (goto-char (point-min))
    (should (vtable-goto-object info))
    (should (vtable-goto-column (seq-position (opencode-test-list-columns) "Title")))
    (buffer-substring (point) (1- (next-single-property-change (point) 'vtable-column)))))

(defun opencode-test-list-title-cell-width (info)
  "Return the width the Title cell of session INFO takes, padding included."
  (save-excursion
    (goto-char (point-min))
    (should (vtable-goto-object info))
    (should (vtable-goto-column (seq-position (opencode-test-list-columns) "Title")))
    (string-pixel-width (buffer-substring (point) (next-single-property-change (point) 'vtable-column))
                        (current-buffer))))

(defun opencode-test-list-objects ()
  "Return the objects of the table at point in the order of its rows."
  (save-excursion
    (goto-char (point-min))
    (let (objects)
      (while (not (eobp))
        (when-let* ((object (vtable-current-object)))
          (push object objects))
        (forward-line 1))
      (nreverse objects))))

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
        ;; Finished turns nobody viewed show as done, one space before their title.
        (should (string-match-p (concat "^" done " history renamed ") text))
        (should (string-match-p (concat "^" done " reasoning ") text))
        (should-not (string-match-p child text))
        (goto-char (point-min))
        (should (equal (opencode-test-list-columns) '("Title" "Branch" "Last Updated" "Created")))
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
                                      " reasoning +opencode")
                              (buffer-string))))))

(defun opencode-test-session-list-highlighted-p (info)
  "Return non-nil when INFO's title is highlighted in the current list.
The title follows the status icon and a space, which are never highlighted."
  (let ((cell (opencode-test-list-title-cell info)))
    (should-not (memq 'opencode-session-list-open-session (ensure-list (get-text-property 0 'face cell))))
    (should-not (memq 'opencode-session-list-open-session (ensure-list (get-text-property 1 'face cell))))
    (memq 'opencode-session-list-open-session (ensure-list (get-text-property 2 'face cell)))))

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
  "Each title follows its status icon, and a change redraws only that row, as wide."
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
        (let* ((objects (vtable-objects (vtable-current-table)))
               ;; A short title shows whole in the narrow batch window.
               (info (seq-find (lambda (info) (< (length (alist-get 'title info)) 20)) objects))
               (id (alist-get 'id info))
               (widths (mapcar #'opencode-test-list-title-cell-width objects)))
          (should (opencode-state-get id))
          ;; Every row's Title cell takes the column's width, whatever its icon,
          ;; and the widest title still shows whole after its icon.
          (should (= (length (seq-uniq widths)) 1))
          (dolist (object objects)
            (should (string-suffix-p (concat " " (opencode--session-list-title-text object))
                                     (substring-no-properties (opencode-test-list-title-cell object)))))
          (cl-flet ((check (status)
                      (let ((cell (opencode-test-list-title-cell info)))
                        (should (equal (substring-no-properties cell)
                                       (concat (opencode-session-status-icon status) " " (alist-get 'title info))))
                        (should (memq (intern (format "opencode-session-status-%s" status))
                                      (ensure-list (get-text-property 0 'face cell))))
                        (should (equal (get-text-property 0 'help-echo cell) (symbol-name status)))
                        ;; Neither the space nor the title takes the icon's face or tooltip.
                        (dolist (position (list 1 2))
                          (should-not (get-text-property position 'help-echo cell))
                          (should-not (seq-find (lambda (face)
                                                  (string-prefix-p "opencode-session-status-" (format "%s" face)))
                                                (ensure-list (get-text-property position 'face cell))))))))
            (check 'done)
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
              (check 'working)
              (should (equal (mapcar #'opencode-test-list-title-cell-width objects) widths))
              ;; Archiving or restoring a session refreshes the lists.
              (opencode-test-dispatch `(((type . "session.archived") (created . 2) (data (sessionID . ,id)))))
              (should (= (length timers) 2))
              (should (eq opencode--session-list-pending t)))))))))

(ert-deftest opencode-test-session-list-sorts-by-title ()
  "The Title column sorts by title, not by the icons before them."
  (opencode-test-with-server
    (let ((statuses '(("ses_a" . done) ("ses_b" . working) ("ses_c" . retrying))))
      (cl-letf (((symbol-function 'opencode--session-list-status)
                 (lambda (info) (cdr (assoc (alist-get 'id info) statuses)))))
        (with-temp-buffer
          (opencode-session-list-mode)
          (setq opencode--session-list-scope (cons temporary-file-directory nil)
                opencode--session-list-sessions
                (mapcar (lambda (session)
                          `((id . ,(car session)) (title . ,(cadr session))
                            (location (directory . ,temporary-file-directory))
                            (time (created . 1) (updated . ,(caddr session)))))
                        '(("ses_a" "alpha" 1) ("ses_b" "beta" 2) ("ses_c" "gamma" 3))))
          (opencode--session-list-render)
          (cl-flet ((titles ()
                      (mapcar (lambda (info) (alist-get 'title info)) (opencode-test-list-objects)))
                    (sort-by-title ()
                      (goto-char (point-min))
                      (should (vtable-goto-column 0))
                      (vtable-sort-by-current-column)))
            ;; Newest first, which is also the order of the icons.
            (should (equal (titles) '("gamma" "beta" "alpha")))
            (should (string< (opencode-session-status-icon 'retrying) (opencode-session-status-icon 'working)))
            (should (string< (opencode-session-status-icon 'working) (opencode-session-status-icon 'done)))
            (sort-by-title)
            (should (equal (titles) '("alpha" "beta" "gamma")))
            (sort-by-title)
            (should (equal (titles) '("gamma" "beta" "alpha")))
            (should (string-match-p (concat "^" (regexp-quote (opencode-session-status-icon 'retrying)) " gamma ")
                                    (buffer-string)))))))))

(ert-deftest opencode-test-session-list-title-value ()
  "A Title value is the title, or the ID when there is none, carrying the status."
  (opencode-test-with-server
    (cl-letf (((symbol-function 'opencode--session-list-status) (lambda (_info) 'working)))
      (pcase-dolist (`(,info . ,shown) '((((id . "ses_x") (title . "Fix it")) . "Fix it")
                                         (((id . "ses_x") (title . "")) . "ses_x")
                                         (((id . "ses_x")) . "ses_x")))
        (let ((value (opencode--session-list-title info)))
          (should (equal (substring-no-properties value) shown))
          (should (eq (get-text-property 0 'opencode-session-status value) 'working))
          (should-not (get-text-property 0 'face value))))
      (with-temp-buffer
        (puthash "ses_x" (current-buffer) opencode-session-buffers)
        (should (eq (get-text-property 0 'face (opencode--session-list-title '((id . "ses_x") (title . "T"))))
                    'opencode-session-list-open-session))))))

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

(ert-deftest opencode-test-session-status-icons-default ()
  "The icons default to the Nerd Font ones drover uses, one column wide each."
  (let ((default (eval (car (get 'opencode-session-status-icons 'standard-value)) t)))
    (should (equal (mapcar (lambda (entry) (cons (car entry) (cadr entry))) default)
                   '((waiting . "\U000F02D7")     ; nf-md-help_circle
                     (working . "\U000F051F")     ; nf-md-timer_sand
                     (retrying . "\uf021")        ; nf-fa-refresh
                     (failed . "\uf057")          ; nf-fa-times_circle
                     (done . "\U000F05E0")        ; nf-md-check_circle
                     (idle . "\uf10c"))))         ; nf-fa-circle_o
    (dolist (entry default)
      (should (= (char-width (string-to-char (cadr entry))) 1))
      (should (eq (caddr entry) (intern (format "opencode-session-status-%s" (car entry))))))))

(ert-deftest opencode-test-session-list-long-title ()
  "A title too long for the column ends in an ellipsis, after its whole icon."
  (opencode-test-with-server
    (cl-letf (((symbol-function 'opencode--session-list-status) (lambda (_info) 'waiting)))
      (with-temp-buffer
        (opencode-session-list-mode)
        (setq opencode--session-list-scope (cons temporary-file-directory nil)
              opencode--session-list-sessions
              (mapcar (lambda (session)
                        `((id . ,(car session)) (title . ,(cdr session))
                          (location (directory . ,temporary-file-directory))
                          (time (created . 1) (updated . 1))))
                      `(("ses_a" . ,(make-string 200 ?x)) ("ses_b" . "short"))))
        (opencode--session-list-render)
        (let* ((long (car opencode--session-list-sessions))
               (cell (substring-no-properties (opencode-test-list-title-cell long)))
               (prefix (concat (opencode-session-status-icon 'waiting) " x")))
          (should (string-prefix-p prefix cell))
          (should (string-suffix-p (truncate-string-ellipsis) cell))
          (should (< (length cell) 200))
          (should (= (opencode-test-list-title-cell-width long)
                     (opencode-test-list-title-cell-width (cadr opencode--session-list-sessions)))))))))

(ert-deftest opencode-test-session-list-title-fits-pixels ()
  "On a graphic display, titles show whole after their icons, in aligned columns.
The `vtable' face is proportional and icons may come from another font,
so a status change must keep the columns where they were."
  :tags '(gui)
  (skip-unless (display-graphic-p))
  (opencode-test-with-server
    (let ((statuses (list (cons "ses_a" 'idle) (cons "ses_b" 'idle) (cons "ses_c" 'idle))))
      (cl-letf (((symbol-function 'opencode--session-list-status)
                 (lambda (info) (cdr (assoc (alist-get 'id info) statuses)))))
        (with-temp-buffer
          (set-window-buffer (selected-window) (current-buffer))
          (opencode-session-list-mode)
          (setq opencode--session-list-scope (cons temporary-file-directory nil)
                opencode--session-list-sessions
                (mapcar (lambda (session)
                          `((id . ,(car session)) (title . ,(cdr session))
                            (location (directory . ,temporary-file-directory))
                            (time (created . 1) (updated . 1))))
                        '(("ses_a" . "WWWW MMMM wide") ("ses_b" . "iiii llll") ("ses_c" . "Mixed Title 42"))))
          (opencode--session-list-render)
          (cl-flet ((check ()
                      (redisplay t)
                      (let ((branch (seq-position (opencode-test-list-columns) "Branch"))
                            (starts nil))
                        (dolist (info opencode--session-list-sessions)
                          (should (equal (substring-no-properties (opencode-test-list-title-cell info))
                                         (concat (opencode-session-status-icon
                                                  (cdr (assoc (alist-get 'id info) statuses)))
                                                 " " (alist-get 'title info))))
                          (save-excursion
                            (should (vtable-goto-object info))
                            (should (vtable-goto-column branch))
                            (push (car (posn-x-y (posn-at-point (point)))) starts)))
                        (should (= (length (seq-uniq starts)) 1)))))
            (check)
            ;; Rows take every status, each a different one from the others.
            (dotimes (turn (length opencode-session-status-icons))
              (seq-do-indexed (lambda (entry row)
                                (setcdr entry (car (nth (% (+ turn row) (length opencode-session-status-icons))
                                                        opencode-session-status-icons)))
                                (opencode--session-list-update-buffer (car entry)))
                              statuses)
              (check))))))))

(ert-deftest opencode-test-session-list-title-width ()
  "The Title column comes first, as wide as the widest title after the widest icon and a space."
  (let ((sessions '(((id . "ses_a") (title . "ab")) ((id . "ses_b") (title . "abcdef")))))
    (let ((columns (let ((opencode--session-list-scope 'all)) (opencode--session-list-columns sessions))))
      (should (equal (mapcar (lambda (column) (plist-get column :name)) columns)
                     '("Title" "Project" "Branch" "Last Updated" "Created")))
      (should (= (plist-get (car columns) :width) (+ 1 1 6))))
    ;; A two-column icon widens it.
    (let ((opencode-session-status-icons '((done "⚑" opencode-session-status-done))))
      (should (= (plist-get (car (opencode--session-list-columns sessions)) :width) (+ 2 1 6))))
    ;; An untitled session shows its ID.
    (should (= (plist-get (car (opencode--session-list-columns '(((id . "ses_0123456789"))))) :width)
               (+ 1 1 14)))))

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
