;;; opencode-directory-test.el --- Session directory synchronization  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Directory changes are server moves, whether requested through cd or
;; received from another client.  HTTP acceptance need not mean delivery.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-directory-event (id directory &optional subpath)
  "Return a move event for session ID to DIRECTORY with SUBPATH."
  `((type . "session.moved")
    (data (sessionID . ,id) (projectID . "project_moved") (subpath . ,subpath)
          (location (directory . ,(opencode-directory-name directory))))))

(defun opencode-test-directory-is (directory)
  "Assert that this buffer and its session metadata agree on DIRECTORY."
  (should (equal default-directory (opencode-normalize-directory directory)))
  (should (equal list-buffers-directory default-directory))
  (should (equal (opencode--session-directory opencode-session-info) default-directory)))

(ert-deftest opencode-test-cd-waits-for-server-move ()
  "Relative cd requests a move; only its delivery updates the session and buffer."
  (opencode-test-with-server
   (let ((target (make-temp-file (expand-file-name "move to " opencode-test-workspace) t)))
     (unwind-protect
         (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
           (opencode-test-directory-is opencode-test-workspace)
           (setq opencode-test-requests nil)
           (let ((cd-path '("./")))
             (cd (file-name-nondirectory target)))
           (opencode-test-directory-is opencode-test-workspace)
           (should (= (length opencode-test-requests) 1))
           (let ((request (car opencode-test-requests)))
             (should (eq (plist-get request :method) 'post))
             (should (equal (plist-get request :path) (format "/api/session/%s/move" opencode-session-id)))
             (should (equal (plist-get request :body) `((directory . ,(opencode-directory-name target)))))
             (when (memq system-type '(windows-nt ms-dos))
               (should (opencode-test-capital-drive-p (alist-get 'directory (plist-get request :body))))))
           (opencode-test-dispatch (list (opencode-test-directory-event opencode-session-id target "subdir")))
           (opencode-test-directory-is target)
           (should (equal (alist-get 'projectID opencode-session-info) "project_moved"))
           (should (equal (alist-get 'subpath opencode-session-info) "subdir"))
           (should (= (length opencode-test-requests) 1)))
       (delete-directory target)))))

(ert-deftest opencode-test-cd-keeps-native-path-resolution ()
  "CDPATH and native validation still apply, without changing the confirmed directory."
  (opencode-test-with-server
   (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
     (let ((cd-path (list (file-name-directory (directory-file-name opencode-test-workspace)))))
       (setq opencode-test-requests nil)
       (cd (file-name-nondirectory (directory-file-name opencode-test-workspace)))
       (should (equal (plist-get (car opencode-test-requests) :body)
                      `((directory . ,(opencode-directory-name opencode-test-workspace)))))
       (setq opencode-test-requests nil)
       (should-error (cd (expand-file-name "README.md" opencode-test-workspace)))
       (should-error (cd (expand-file-name "missing-directory" opencode-test-workspace)))
       (should-not opencode-test-requests)
       (opencode-test-directory-is opencode-test-workspace)))))

(ert-deftest opencode-test-cd-outside-session ()
  "Ordinary buffers and session-mode prompt helpers without an ID use native cd."
  (opencode-test-with-server
   (dolist (mode '(fundamental-mode opencode-session-mode))
     (with-temp-buffer
       (funcall mode)
       (cd opencode-test-workspace)
       (should (opencode-same-directory-p default-directory opencode-test-workspace))
       (should (equal list-buffers-directory default-directory))
       (should-not opencode-test-requests)))))

(ert-deftest opencode-test-cd-failure-keeps-confirmed-directory ()
  "A refused move reports the error without committing a local directory change."
  (opencode-test-with-server
   (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
     (let ((opencode-test-routes
            (cons (list 'post "/move\\'" (lambda (_) '(:error 400 "Move refused"))) opencode-test-routes))
           reported)
       (cl-letf (((symbol-function 'opencode-report-error) (lambda (text) (setq reported text))))
         (cd temporary-file-directory))
       (should (string-match-p "Move refused" reported))
       (opencode-test-directory-is opencode-test-workspace)))))

(ert-deftest opencode-test-cd-late-replies-do-not-undo-moves ()
  "An old request's acceptance or rejection cannot undo a newer server move."
  (opencode-test-with-server
   (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
     (let (callbacks reported)
       (cl-letf (((symbol-function 'opencode-request)
                  (lambda (_method _path &rest arguments) (push arguments callbacks)))
                 ((symbol-function 'opencode-report-error) (lambda (text) (setq reported text))))
         (cd temporary-file-directory)
         (cd opencode-test-workspace)
         (opencode-test-directory-is opencode-test-workspace)
         (opencode-test-dispatch (list (opencode-test-directory-event opencode-session-id temporary-file-directory)))
         (opencode-test-directory-is temporary-file-directory)
         (funcall (or (plist-get (car callbacks) :then) #'ignore) nil)
         (funcall (plist-get (cadr callbacks) :else)
                  (opencode-error--make :status 500 :message "Late failure"))
         (should (string-match-p "Late failure" reported))
         (opencode-test-directory-is temporary-file-directory)
         (should (= (length callbacks) 2)))))))

(ert-deftest opencode-test-agent-move-updates-only-owner ()
  "An unsolicited move updates its buffer and metadata without sending another request."
  (opencode-test-with-server
   (let ((owner (opencode-test-open (opencode-test-scenario "text")))
         (other (opencode-test-open (opencode-test-scenario "shell"))))
     (setq opencode-test-requests nil)
     (with-current-buffer owner
       (setf (alist-get 'subpath opencode-session-info) "old-subdir")
       (goto-char (point-max))
       (insert "unsent prompt")
       (opencode-test-dispatch (list (opencode-test-directory-event opencode-session-id temporary-file-directory)))
       (opencode-test-directory-is temporary-file-directory)
       (should-not (alist-get 'subpath opencode-session-info))
       (should (equal (alist-get 'projectID opencode-session-info) "project_moved"))
       (should (equal (buffer-substring-no-properties (opencode-pmark) (point-max)) "unsent prompt")))
     (with-current-buffer other
       (opencode-test-directory-is opencode-test-workspace))
     (should-not opencode-test-requests))))

(ert-deftest opencode-test-directory-resync-and-reload ()
  "Reconnect and reload restore the server's directory even without a live move event."
  (opencode-test-with-server
   (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
     (let* ((info (copy-tree opencode-session-info))
            (opencode-test-routes
             (cons (list 'get (concat "\\`/api/session/" opencode-session-id "\\'")
                         (lambda (_) `((data . ,(copy-tree info)))))
                   opencode-test-routes)))
       (setf (alist-get 'location info) `((directory . ,(opencode-directory-name temporary-file-directory)))
             (alist-get 'projectID info) "project_resynced"
             (alist-get 'subpath info) "resynced")
       ;; The unchanged timestamp takes the resync path without a transcript reload.
       (opencode--resync-buffer)
       (opencode-test-directory-is temporary-file-directory)
       (should (equal (alist-get 'subpath opencode-session-info) "resynced"))
       (setf (alist-get 'location info) `((directory . ,(opencode-directory-name opencode-test-workspace))))
       (opencode-session-reload)
       (opencode-test-directory-is opencode-test-workspace)
       (should-not (seq-some (lambda (request) (eq (plist-get request :method) 'post)) opencode-test-requests))))))

(ert-deftest opencode-test-directory-live-move-overtakes-fetch ()
  "A live move takes precedence over an older reload or reconnection response."
  (dolist (refresh '(opencode-session-reload opencode--resync-buffer))
    (opencode-test-with-server
     (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
       (let ((snapshot (copy-tree opencode-session-info)) callback)
         (cl-letf (((symbol-function 'opencode-session-fetch)
                    (lambda (_id then &rest _) (setq callback then))))
           (funcall refresh))
         (opencode-test-dispatch
          (list (opencode-test-directory-event opencode-session-id temporary-file-directory "new-subdir")))
         (funcall callback snapshot)
         (opencode-test-directory-is temporary-file-directory)
         (should (equal (alist-get 'projectID opencode-session-info) "project_moved"))
         (should (equal (alist-get 'subpath opencode-session-info) "new-subdir")))))))

(ert-deftest opencode-test-replay-move-is-only-a-notice ()
  "A historical move is rendered like a live move, without changing the current directory."
  (opencode-test-with-server
   (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
     (opencode-replay-message
      `((type . "location-switched") (location (directory . ,(opencode-directory-name temporary-file-directory)))))
     (should (string-match-p (regexp-quote (concat "Moved to " (opencode-normalize-directory temporary-file-directory)))
                             (opencode-test-text (current-buffer))))
     (opencode-test-directory-is opencode-test-workspace))))

(provide 'opencode-directory-test)
;;; opencode-directory-test.el ends here
