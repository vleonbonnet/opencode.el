;;; opencode-sessions-test.el --- Tests of session pickers and projects  -*- lexical-binding: t; -*-

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
                                               (pcase (alist-get 'parentID query)
                                                 ('nil t)
                                                 ("null" (not (alist-get 'parentID info)))
                                                 (parent (equal (alist-get 'parentID info) parent)))))
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

(defun opencode-test-root-titles ()
  "Return the titles of the recorded sessions other than subagents, newest first."
  (seq-keep (lambda (info) (and (not (alist-get 'parentID info)) (alist-get 'title info)))
            (opencode-test-recorded-sessions)))

(defun opencode-test-session-info (id)
  "Return the recorded info of session ID."
  (alist-get 'data (opencode-test-fixture (format "session-%s.json" id))))

(defvar opencode-test-offers nil
  "What `completing-read' offered under `opencode-test-picking', newest first.
Each is a plist: :prompt, :names (the collection as given) and :rows,
the candidates in display order as (NAME PREFIX SUFFIX).")

(defmacro opencode-test-picking (chooser &rest body)
  "Run BODY answering `completing-read' with CHOOSER.
CHOOSER receives the rows of the offer, see `opencode-test-offers', and
returns the name chosen.  The offer is displayed as completion UIs do:
sorted by its display sort function, then affixed."
  (declare (indent 1) (debug t))
  `(let ((opencode-test-offers nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest _)
                  (let* ((sort (plist-get completion-extra-properties :display-sort-function))
                         (affix (plist-get completion-extra-properties :affixation-function))
                         (rows (funcall affix (if sort (funcall sort (copy-sequence collection)) collection))))
                    (push (list :prompt prompt :names collection :rows rows) opencode-test-offers)
                    (funcall ,chooser rows)))))
       ,@body)))

(defun opencode-test-choose (name)
  "Return a chooser of `opencode-test-picking' picking NAME, which must be offered."
  (lambda (rows)
    (should (assoc name rows))
    name))

(defun opencode-test-offered-names (&optional offer)
  "Return the names OFFER displayed, the last offer by default, without properties."
  (mapcar (lambda (row) (substring-no-properties (car row)))
          (plist-get (or offer (car opencode-test-offers)) :rows)))

(defun opencode-test-make-head (directory content &optional gitdir)
  "Write HEAD CONTENT for a repository at DIRECTORY, whose Git directory is GITDIR."
  (let ((gitdir (or gitdir (expand-file-name ".git" directory))))
    (make-directory gitdir t)
    (with-temp-file (expand-file-name "HEAD" gitdir) (insert content))))

;;;; Server data

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
          (let ((opencode-git-branch-ttl 0))
            (should (equal (opencode-git-branch linked) "renamed")))
          ;; The TTL's former name still sets it.
          (opencode-test-make-head nil "ref: refs/heads/again\n"
                                   (expand-file-name ".git/worktrees/linked" main))
          (with-suppressed-warnings ((obsolete opencode-session-list-branch-ttl))
            (let ((opencode-session-list-branch-ttl 0))
              (should (equal (opencode-git-branch linked) "again"))))
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

(ert-deftest opencode-test-project-name ()
  "A project is named by its name, else by its canonical directory."
  (should (equal (opencode-project-name '((name . "Named") (canonical . "/a/b"))) "Named"))
  (should (equal (opencode-project-name '((canonical . "/a/repo"))) "repo"))
  (should (equal (opencode-project-name '((canonical . "/a/repo/"))) "repo")))

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

(ert-deftest opencode-test-fetch-projects-remembered ()
  "Fetched projects are remembered for naming sessions; a failure keeps the last ones."
  (opencode-test-with-sessions
    (let (result failure)
      (opencode-fetch-projects (lambda (projects) (setq result projects)))
      (should (equal result (opencode-test-projects)))
      (should (equal opencode--projects result))
      (let ((opencode-test-routes (cons (list 'get "\\`/api/project\\'" (lambda (_) '(:error 500 "down")))
                                        opencode-test-routes)))
        (opencode-fetch-projects (lambda (_) (ert-fail "Succeeded")) (lambda (error) (setq failure error))))
      (should (eql (opencode-error-status failure) 500))
      (should (equal opencode--projects result)))))

;;;; Completion

(ert-deftest opencode-test-annotated-completion ()
  "Candidates are sorted by key, affixed with aligned annotations, and all choosable."
  (let ((candidates `(("b" 2 "two" 2)
                      ("a" 1 "one" 1 "* ")
                      ("a" 3 ,(propertize "three" 'face 'bold) 3)
                      ("c" 4))))
    (opencode-test-picking (opencode-test-choose "a<2>")
      (should (eql (opencode-annotated-completion "Pick: " candidates) 3))
      (let ((offer (car opencode-test-offers)))
        (should (equal (plist-get offer :prompt) "Pick: "))
        ;; Prefixes and annotations are not matched against the input.
        (should (equal (mapcar #'substring-no-properties (plist-get offer :names)) '("a" "b" "a<2>" "c")))
        (should (equal (opencode-test-offered-names) '("a" "b" "a<2>" "c")))
        (pcase-let ((`((,_ ,a-prefix ,a-suffix) (,_ ,b-prefix ,b-suffix) (,_ ,_ ,three) (,_ ,_ ,c-suffix))
                     (plist-get offer :rows)))
          (should (equal a-prefix "* "))
          (should (equal b-prefix ""))
          ;; Four spaces after the widest name, \"a<2>\".
          (should (equal (substring-no-properties a-suffix) (concat (make-string 7 ?\s) "one")))
          (should (equal (substring-no-properties b-suffix) (concat (make-string 7 ?\s) "two")))
          (should (equal (substring-no-properties c-suffix) (make-string 7 ?\s)))
          (should (eq (get-text-property 0 'face a-suffix) 'completions-annotations))
          ;; An annotation's own face comes first.
          (should (equal (get-text-property (- (length three) 1) 'face three) '(bold completions-annotations))))))
    (opencode-test-picking (opencode-test-choose "a")
      (should (eql (opencode-annotated-completion "Pick: " candidates) 1)))
    ;; Without sort keys, the order is kept.
    (opencode-test-picking (opencode-test-choose "y")
      (should (eq (opencode-annotated-completion "Pick: " '(("z" z) ("y" y))) 'y))
      (should (equal (opencode-test-offered-names) '("z" "y"))))
    (should-not (opencode-annotated-completion "Pick: " nil))))

;;;; Session pickers

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

(ert-deftest opencode-test-read-session-annotations ()
  "A picker shows each session's status icon, title, project, branch and age, newest first."
  (let* ((root (file-name-as-directory (make-temp-file "opencode-picker-" t)))
         (repo (expand-file-name "repo/" root))
         (opencode--branches (make-hash-table :test #'equal))
         (now (float-time)))
    (unwind-protect
        (opencode-test-with-server
          (opencode-test-make-head repo "ref: refs/heads/topic\n")
          (setq opencode--projects
                `(((id . "repo") (name . "My repo") (canonical . ,(directory-file-name repo)) (sandboxes))
                  ((id . "other") (canonical . ,(directory-file-name temporary-file-directory)) (sandboxes))))
          (cl-flet ((info (id title directory project updated &optional idle viewed)
                      `((id . ,id) (title . ,title) (projectID . ,project)
                        (location (directory . ,(directory-file-name directory)))
                        (time (created . 1) (updated . ,(round (* 1000 updated)))
                              ,@(when idle `((idle . ,(round (* 1000 idle)))))
                              ,@(when viewed `((viewed . ,(round (* 1000 viewed)))))))))
            (let* ((older (info "ses_1" "Older" repo "repo" (- now 3600) (- now 3600)))
                   (untitled (info "ses_2" "" temporary-file-directory "other" (- now 120)
                                   (- now 120) (- now 100)))
                   (stale (info "ses_3" "Old title" temporary-file-directory "other" (- now 7200)))
                   ;; Found by its directory: its project's ID is unknown.
                   (moved (info "ses_4" "Moved" repo "gone" (- now 86400))))
              ;; The registry knows ses_3 better than its fetched info.
              (puthash "ses_3" (opencode-state--make-entry
                                :id "ses_3" :title "Renamed" :directory (opencode-normalize-directory repo)
                                :project-id "repo" :running t :updated (- now 10))
                       opencode-state--sessions)
              (opencode-test-picking (opencode-test-choose "Renamed")
                ;; Ages under a minute show hundredths: the clock must not move under them.
                (should (eq (cl-letf* ((real (symbol-function 'float-time))
                                       ((symbol-function 'float-time)
                                        (lambda (&optional time) (if time (funcall real time) now))))
                              (opencode-read-session "Session: " (list older untitled stale moved) root))
                            stale))
                (should (equal (plist-get (car opencode-test-offers) :prompt) "Session: "))
                (should (equal (opencode-test-offered-names) '("New session" "Renamed" "ses_2" "Older" "Moved")))
                (let ((rows (plist-get (car opencode-test-offers) :rows)))
                  (should (equal (mapcar (lambda (row) (substring-no-properties (cadr row))) rows)
                                 (list "+ "
                                       (concat (opencode-session-status-icon 'working) " ")
                                       (concat (opencode-session-status-icon 'idle) " ")
                                       (concat (opencode-session-status-icon 'done) " ")
                                       (concat (opencode-session-status-icon 'idle) " "))))
                  ;; The icon keeps its face and tooltip.
                  (should (eq (get-text-property 0 'face (cadr (nth 1 rows))) 'opencode-session-status-working))
                  (should (string-suffix-p (abbreviate-file-name root) (caddr (car rows))))
                  (let ((other (file-name-nondirectory (directory-file-name temporary-file-directory))))
                    (pcase-dolist (`(,name ,project ,branch ,age)
                                   `(("Renamed" "My repo" "topic" 10)
                                     ("ses_2" ,other "" 120)
                                     ("Older" "My repo" "topic" 3600)
                                     ("Moved" "My repo" "topic" 86400)))
                      (let ((suffix (substring-no-properties (caddr (assoc name rows)))))
                        (should (string-match
                                 (concat "\\` +" (regexp-quote project) " +" (regexp-quote branch)
                                         " +\\(.+\\)\\'")
                                 suffix))
                        (should (equal (match-string 1 suffix) (opencode-format-time-ago age))))))
                  ;; The columns line up, whatever the widths of titles and projects.
                  (cl-flet ((aligned (regexp)
                              (= 1 (length (seq-uniq
                                            (seq-keep (lambda (row)
                                                        (let ((suffix (substring-no-properties (caddr row))))
                                                          (when (string-match regexp suffix)
                                                            (+ (string-width (car row)) (match-beginning 0)))))
                                                      (cdr rows)))))))
                    (should (aligned "topic"))
                    (should (aligned "[^ ]+\\'")))))
              ;; Without a new entry, sessions alone are offered; choosing one returns its info.
              (opencode-test-picking (opencode-test-choose "Older")
                (should (eq (opencode-read-session "Session: " (list older untitled)) older))
                (should (equal (opencode-test-offered-names) '("ses_2" "Older"))))
              ;; No session has a branch: the column is left out.
              (opencode-test-picking (opencode-test-choose "ses_2")
                (opencode-read-session "Session: " (list untitled))
                (should (string-match-p (concat "\\` +" (regexp-quote (file-name-nondirectory
                                                                       (directory-file-name temporary-file-directory)))
                                                "  [^ ]+\\'")
                                        (substring-no-properties
                                         (caddr (car (plist-get (car opencode-test-offers) :rows)))))))
              (opencode-test-picking (opencode-test-choose "New session")
                (should (eq (opencode-read-session "Session: " (list older) root) 'new)))
              ;; Choosing nothing returns nil.
              (opencode-test-picking (lambda (_) "")
                (should-not (opencode-read-session "Session: " (list older) root))))))
      (delete-directory root t))))

(ert-deftest opencode-test-read-session-fetches-projects ()
  "A picker fetches the projects it names sessions by when none were fetched yet."
  (opencode-test-with-sessions
    (let ((info (opencode-test-session-info (opencode-test-scenario "text")))
          (project (file-name-nondirectory (directory-file-name opencode-test-workspace))))
      (opencode-test-picking (opencode-test-choose "text")
        (opencode-read-session "Session: " (list info))
        (should (= 1 (length (opencode-test-requests-to 'get "/api/project"))))
        (should (string-match-p (regexp-quote project)
                                (caddr (car (plist-get (car opencode-test-offers) :rows)))))
        (opencode-read-session "Session: " (list info))
        (should (= 1 (length (opencode-test-requests-to 'get "/api/project")))))
      ;; Names only decorate: without them, the sessions are still offered.
      (setq opencode--projects nil)
      (let ((opencode-test-routes (cons (list 'get "\\`/api/project\\'" (lambda (_) '(:error 500 "down")))
                                        opencode-test-routes)))
        (opencode-test-picking (opencode-test-choose "text")
          (should (eq (opencode-read-session "Session: " (list info)) info))
          (should-not (string-match-p (regexp-quote project)
                                      (caddr (car (plist-get (car opencode-test-offers) :rows))))))))))

(ert-deftest opencode-test-opencode-offers-project-sessions ()
  "`opencode' offers the project's sessions newest first, or starts one."
  (opencode-test-with-sessions
    (let ((opened nil)
          (created nil))
      (cl-letf (((symbol-function 'opencode-session-open)
                 (lambda (info &rest arguments) (setq opened (cons info arguments))))
                ((symbol-function 'opencode-session-create)
                 (lambda (&rest arguments) (setq created arguments))))
        (opencode-test-picking (opencode-test-choose "text")
          (opencode opencode-test-workspace)
          (should (equal (opencode-test-offered-names) (cons "New session" (opencode-test-root-titles))))
          (should (equal (alist-get 'id (car opened)) (opencode-test-scenario "text")))
          (should (equal (cdr opened) '(:other-window nil))))
        ;; The project's sessions, by its ID, without subagents nor archived ones.
        (let ((query (plist-get (seq-find (lambda (request) (equal (plist-get request :path) "/api/session"))
                                          (reverse opencode-test-requests))
                                :query)))
          (should (equal (alist-get 'project query) opencode-test-project-id))
          (should (equal (alist-get 'parentID query) "null"))
          (should (equal (alist-get 'archived query) "false")))
        (opencode-test-picking (opencode-test-choose "New session")
          (setq opened nil)
          (opencode opencode-test-workspace)
          (should-not opened)
          (should (equal created (list :directory (opencode-normalize-directory opencode-test-workspace)))))
        ;; A project without sessions starts one at once.
        (setq created nil)
        (opencode-test-picking (lambda (_) (ert-fail "Asked"))
          (opencode temporary-file-directory))
        (should (equal created (list :directory (opencode-normalize-directory temporary-file-directory))))
        ;; Choosing nothing does nothing.
        (setq created nil opened nil)
        (opencode-test-picking (lambda (_) "")
          (opencode opencode-test-workspace))
        (should-not (or created opened))
        ;; The command's former name is the same command.
        (should (eq (indirect-function 'opencode-select-session) (indirect-function 'opencode)))))))

(ert-deftest opencode-test-select-project ()
  "`opencode-select-project' picks a project, then one of its sessions or a new one."
  (opencode-test-with-sessions
    (let ((created nil)
          (answers (list (abbreviate-file-name (directory-file-name opencode-test-workspace)) "New session")))
      (cl-letf (((symbol-function 'opencode-session-create)
                 (lambda (&rest arguments) (setq created arguments))))
        (opencode-test-picking (lambda (_) (pop answers))
          (opencode-select-project)
          (should (equal (mapcar (lambda (offer) (plist-get offer :prompt)) (reverse opencode-test-offers))
                         '("Project: " "Session: ")))
          ;; Most recently active first.
          (should (equal (opencode-test-offered-names (cadr opencode-test-offers))
                         (mapcar (lambda (project) (abbreviate-file-name (alist-get 'canonical project)))
                                 (opencode-test-projects))))
          (should (equal (opencode-test-offered-names) (cons "New session" (opencode-test-root-titles))))
          (should (equal created (list :directory (opencode-normalize-directory opencode-test-workspace)))))))))

(ert-deftest opencode-test-commands-pick-outside-session-buffers ()
  "Renaming, archiving and deleting outside a session buffer pick a session of the project."
  (opencode-test-with-sessions
    (let ((id (opencode-test-scenario "reasoning"))
          (questions nil))
      (with-temp-buffer
        (setq default-directory opencode-test-workspace)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt questions) t))
                  ((symbol-function 'yes-or-no-p) (lambda (prompt) (push prompt questions) t))
                  ((symbol-function 'read-string) (lambda (_prompt initial &rest _) (push initial questions) "Typed")))
          (opencode-test-picking (opencode-test-choose "reasoning")
            (opencode-rename-session nil "Renamed")
            (opencode-archive-session)
            (opencode-delete-session)
            (opencode-rename-session)
            (should (equal (mapcar (lambda (offer) (plist-get offer :prompt)) (reverse opencode-test-offers))
                           '("Rename session: " "Archive session: " "Delete session: " "Rename session: ")))
            ;; No entry starting a session.
            (dolist (offer opencode-test-offers)
              (should (equal (opencode-test-offered-names offer) (opencode-test-root-titles)))))
          ;; The registry does not hold the session: its picked info names it.
          (should (equal (reverse questions)
                         '("Archive the OpenCode session reasoning? "
                           "Delete the OpenCode session reasoning for good? "
                           "reasoning")))
          (should (equal (seq-keep (lambda (request)
                                     (and (equal (plist-get request :path) (concat "/api/session/" id))
                                          (list (plist-get request :method) (plist-get request :body))))
                                   (reverse opencode-test-requests))
                         '((patch ((title . "Renamed"))) (patch ((archived . t))) (delete nil)
                           (patch ((title . "Typed"))))))
          ;; An explicit ID asks nothing.
          (setq opencode-test-requests nil)
          (opencode-test-picking (lambda (_) (ert-fail "Asked"))
            (opencode-rename-session id "Again"))
          (should (opencode-test-requests-to 'patch (concat "/api/session/" id)))
          (opencode-test-picking (lambda (_) "")
            (should-error (opencode-archive-session) :type 'user-error)))))))

(ert-deftest opencode-test-commands-act-on-the-buffer-session ()
  "In a session buffer, commands act on its session without asking."
  (opencode-test-with-sessions
    (let ((id (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open id)
        (setq opencode-test-requests nil)
        (cl-letf (((symbol-function 'y-or-n-p) #'always))
          (opencode-test-picking (lambda (_) (ert-fail "Asked"))
            (opencode-rename-session nil "Renamed")
            (opencode-archive-session)
            (opencode-interrupt)))
        (should (= 2 (length (opencode-test-requests-to 'patch (concat "/api/session/" id)))))
        (should (opencode-test-requests-to 'post (format "/api/session/%s/interrupt" id)))))))

(ert-deftest opencode-test-interrupt-picks-a-running-session ()
  "Outside a session buffer, interrupting picks among the project's running sessions."
  (opencode-test-with-sessions
    (let* ((id (opencode-test-scenario "steer"))
           (active nil)
           (opencode-test-routes
            (cons (list 'get "\\`/api/session/active\\'"
                        (lambda (_) `((data . ,(mapcar (lambda (id) (list (intern id) '(status . "running")))
                                                       active)))))
                  opencode-test-routes)))
      (with-temp-buffer
        (setq default-directory opencode-test-workspace)
        (setq active (list id "ses_elsewhere"))
        (opencode-test-picking (opencode-test-choose "steer")
          (opencode-interrupt)
          (should (equal (plist-get (car opencode-test-offers) :prompt) "Interrupt session: "))
          (should (equal (opencode-test-offered-names) '("steer"))))
        (should (opencode-test-requests-to 'post (format "/api/session/%s/interrupt" id)))
        (opencode-test-picking (lambda (_) (ert-fail "Asked"))
          ;; Running sessions of other projects only.
          (setq active (list "ses_elsewhere"))
          (should (string-match-p "No OpenCode session is running in"
                                  (cadr (should-error (opencode-interrupt) :type 'user-error))))
          (setq active nil opencode-test-requests nil)
          (should (equal (cadr (should-error (opencode-interrupt) :type 'user-error))
                         "No OpenCode session is running"))
          ;; Nothing running: the project's sessions are not even fetched.
          (should-not (seq-find (lambda (request) (equal (plist-get request :path) "/api/session"))
                                opencode-test-requests)))))))

(ert-deftest opencode-test-commands-without-sessions ()
  "A command picking a session in a project without one says so."
  (opencode-test-with-sessions
    (with-temp-buffer
      (setq default-directory temporary-file-directory)
      (opencode-test-picking (lambda (_) (ert-fail "Asked"))
        (should (string-match-p "\\`No OpenCode session in "
                                (cadr (should-error (opencode-rename-session) :type 'user-error))))))))

(ert-deftest opencode-test-select-open-session ()
  "The open sessions are picked by session, annotated, and their buffer shown."
  (opencode-test-with-sessions
    (let* ((first (opencode-test-open (opencode-test-scenario "text")))
           (second (opencode-test-open (opencode-test-scenario "reasoning")))
           (shown nil))
      (cl-letf (((symbol-function 'switch-to-buffer) (lambda (buffer &rest _) (setq shown buffer))))
        (opencode-test-picking (opencode-test-choose "text")
          (opencode-select-open-session)
          (should (eq shown first))
          (should (equal (opencode-test-offered-names) '("reasoning" "text")))
          (should (equal (substring-no-properties (cadr (car (plist-get (car opencode-test-offers) :rows))))
                         (concat (opencode-session-status-icon 'done) " "))))
        (kill-buffer first)
        (kill-buffer second)
        (setq shown nil)
        (opencode-test-picking (lambda (_) (ert-fail "Asked"))
          (opencode-select-open-session))
        (should-not shown)))))

(ert-deftest opencode-test-select-idle-and-child-sessions ()
  "The idle and subagent pickers are annotated session pickers too."
  (opencode-test-with-sessions
    (let ((opened nil))
      (cl-letf (((symbol-function 'opencode-session-open) (lambda (info &rest _) (setq opened info))))
        (let ((opencode-alerted-sessions (mapcar #'opencode-test-session-info
                                                 (list (opencode-test-scenario "text")
                                                       (opencode-test-scenario "steer")))))
          (opencode-test-picking (opencode-test-choose "text")
            (opencode-select-idle)
            (should (equal (plist-get (car opencode-test-offers) :prompt) "Idle session: "))
            (should (equal (opencode-test-offered-names) '("steer" "text")))
            (should (equal (alist-get 'id opened) (opencode-test-scenario "text")))))
        (let ((opencode-alerted-sessions nil))
          (opencode-test-picking (lambda (_) (ert-fail "Asked"))
            (opencode-select-idle))))
      (let ((child (opencode-test-session-info (opencode-test-scenario "subagent-child"))))
        (with-current-buffer (opencode-test-open (opencode-test-scenario "subagent"))
          (cl-letf (((symbol-function 'opencode-session-open) (lambda (info &rest _) (setq opened info))))
            (opencode-test-picking (opencode-test-choose (alist-get 'title child))
              (opencode-select-child-session)
              (should (equal (opencode-test-offered-names) (list (alist-get 'title child))))
              (should (equal (alist-get 'id opened) (alist-get 'id child))))))))))

(ert-deftest opencode-test-sessions-capital-drive ()
  "Sessions of a directory outside any project are asked for with a capital drive letter."
  (skip-unless (memq system-type '(windows-nt ms-dos)))
  (opencode-test-with-sessions
    (let ((outside (opencode-test-lowercase-drive
                    (file-name-directory (directory-file-name temporary-file-directory)))))
      (opencode--project-sessions outside)
      (should (opencode-test-capital-drive-p
               (alist-get 'directory
                          (plist-get (seq-find (lambda (request)
                                                 (equal (plist-get request :path) "/api/session"))
                                               (reverse opencode-test-requests))
                                     :query)))))))

;;;; Worktrees and MCP servers

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
