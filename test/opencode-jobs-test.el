;;; opencode-jobs-test.el --- Tests of background work  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Job snapshots and lifecycle events must agree even when the agent's
;; turn ended, a snapshot races a completion, or the stream reconnects.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-job (id session &optional kind)
  "Return running job ID of SESSION, of KIND or shell by default."
  `((id . ,id) (sessionID . ,session) (kind . ,(or kind "shell"))
    (status . "running") (time (started . 1000))
    ,@(pcase kind
        ("subagent" `((childSessionID . ,id) (agent . "explore") (description . "Inspect"))))
    ,@(unless kind `((shellID . ,id) (command . "make test")))))

(defmacro opencode-test-with-jobs (jobs &rest body)
  "Run BODY with a fake service reporting JOBS from its job listing."
  (declare (indent 1) (debug t))
  `(opencode-test-with-server
    (let* ((jobs ,jobs)
           (opencode-test-routes
            (cons (list 'get "\\`/api/experimental/job\\'" (lambda (_) `((data . ,jobs))))
                  opencode-test-routes)))
      ,@body)))

(defun opencode-test-job-owner (id &optional running)
  "Install session ID with a turn RUNNING, else one that ended unseen."
  (setq opencode-state--ready t opencode-state--stream opencode-events--stream)
  (opencode-state--put (opencode-state--make-entry :id id :running running :idle 2 :viewed 1
                                                   :outcome 'succeeded)))

(defun opencode-test-job-block (id tool metadata)
  "Insert finished call ID of TOOL whose result has METADATA."
  (opencode-tool-start id tool '((command . "make test")) "running")
  (opencode-tool-update id :status "completed" :metadata metadata))

(ert-deftest opencode-test-jobs-outlive-turn ()
  "Two background jobs keep the session and its block working after its turn ends."
  (let ((id (opencode-test-scenario "shell")))
    (opencode-test-with-jobs (list (opencode-test-job "sh_one" id) (opencode-test-job "sh_two" id))
      (let ((buffer (opencode-test-open-empty id)))
        (opencode-test-job-owner id t)
        (opencode-jobs--list)
        (with-current-buffer buffer
          (let ((block (opencode-test-job-block "call_one" "shell" '((status . "running") (shellID . "sh_one")))))
            (opencode-test-dispatch `(((type . "session.execution.succeeded") (created . 3000) (data (sessionID . ,id)))))
            (should-not (plist-get (opencode-state-get id) :running))
            (should (eq (opencode-state-status id) 'working))
            (should (= (opencode-state-since id) 1))
            (should (eq (car (opencode--mode-line-registry-part)) 'working))
            (should (eq (opencode-block-face (opencode-block-data block)) 'opencode-tool-running-margin))
            (should-not (opencode--groupable-p block))
            (opencode-test-dispatch '(((type . "shell.exited") (data (id . "sh_one") (status . "exited") (exit . 0)))))
            (should (eq (opencode-state-status id) 'working))
            (should (eq (opencode-block-face (opencode-block-data block)) 'opencode-tool-margin-highlight))
            (should (opencode--groupable-p block))
            (opencode-test-dispatch '(((type . "shell.exited") (data (id . "sh_two") (status . "exited") (exit . 0)))))
            (should (eq (opencode-state-status id) 'done))))))))

(ert-deftest opencode-test-jobs-shell-starts-before-turn-ends ()
  "Shell events keep an owner active before its first background listing answers."
  (opencode-test-with-jobs nil
    (opencode-test-job-owner "ses_jobs" t)
    (opencode-test-dispatch
     '(((type . "shell.created") (data (info (id . "sh_live") (status . "running")
                                             (command . "make test") (metadata (sessionID . "ses_jobs"))
                                             (time (started . 1000)))))
       ((type . "session.execution.succeeded") (created . 3000) (data (sessionID . "ses_jobs")))))
    (should (eq (opencode-state-status "ses_jobs") 'working))
    (opencode-test-dispatch '(((type . "shell.deleted") (data (id . "sh_live")))))
    (should (eq (opencode-state-status "ses_jobs") 'done))))

(ert-deftest opencode-test-jobs-snapshot-races-exit ()
  "A stale listing cannot revive a job that exited while the request was in flight."
  (opencode-test-with-jobs (list (opencode-test-job "sh_race" "ses_jobs"))
    (opencode-test-job-owner "ses_jobs")
    (opencode-jobs--list)
    (let ((opencode-test-routes
           (cons (list 'get "/job\\'" (lambda (_) (cons :defer (lambda () `((data . ,jobs))))))
                 opencode-test-routes)))
      (opencode-jobs--list)
      (opencode-test-dispatch '(((type . "shell.exited") (data (id . "sh_race") (status . "exited") (exit . 0)))))
      (opencode-test-release)
      (should-not (opencode-jobs "ses_jobs"))
      (should (eq (opencode-state-status "ses_jobs") 'done)))))

(ert-deftest opencode-test-jobs-reconnect-clears-missed-completion ()
  "A reconnection clears a finished job even when its completion was missed."
  (let ((id (opencode-test-scenario "shell")))
    (opencode-test-with-jobs (list (opencode-test-job "sh_reconnect" id))
      (let ((buffer (opencode-test-open-empty id)))
        (opencode-test-job-owner id)
        (opencode-jobs--connected nil)
        (with-current-buffer buffer
          (let ((block (opencode-test-job-block "call_reconnect" "shell"
                                                '((status . "running") (shellID . "sh_reconnect")))))
            (should (eq (opencode-block-face (opencode-block-data block)) 'opencode-tool-running-margin))
            (setq jobs nil)
            (opencode-jobs--connected nil)
            (should (eq (opencode-state-status id) 'done))
            (should (eq (opencode-block-face (opencode-block-data block)) 'opencode-tool-margin-highlight))))))))

(ert-deftest opencode-test-jobs-reopen-uses-live-job ()
  "Replaying a completed launch reads its live job instead of the stale call status."
  (let* ((id (opencode-test-scenario "shell"))
         (messages (alist-get 'data (opencode-test-fixture (format "messages-%s.json" id))))
         (completion (seq-find (lambda (message) (equal (alist-get 'type message) "synthetic")) messages)))
    (opencode-test-with-jobs (list (opencode-test-job (alist-get 'shellID (alist-get 'metadata completion)) id))
      (opencode-jobs--list)
      (let* ((opencode-test-routes
              (cons (list 'get "/message\\'"
                          (lambda (_) `((data . ,(seq-take-while (lambda (message) (not (eq message completion))) messages))
                                        (cursor)))) opencode-test-routes))
             (buffer (opencode-test-open id)))
        (with-current-buffer buffer
          (let ((block (seq-find (lambda (overlay) (opencode-job-id
                                                    (plist-get (opencode-block-data overlay) :tool)
                                                    (plist-get (opencode-block-data overlay) :metadata)))
                                 (opencode-tool-blocks))))
            (should block)
            (should (eq (opencode-block-face (opencode-block-data block)) 'opencode-tool-running-margin))))))))

(ert-deftest opencode-test-jobs-failure-colors-original-block ()
  "Nonzero exits, timeouts and cancellations color the original launch as a failure."
  (dolist (data '(((id . "sh_failed") (status . "exited") (exit . 7))
                  ((id . "sh_failed") (status . "timeout"))
                  ((id . "sh_failed") (status . "killed"))))
    (let ((id (opencode-test-scenario "shell")))
      (opencode-test-with-jobs (list (opencode-test-job "sh_failed" id))
        (opencode-jobs--list)
        (with-current-buffer (opencode-test-open-empty id)
          (let ((block (opencode-test-job-block "call_failed" "shell"
                                                '((status . "running") (shellID . "sh_failed")))))
            (opencode-test-dispatch `(((type . "shell.exited") (data . ,data))))
            (should (eq (opencode-block-face (opencode-block-data block)) 'error))
            (setq jobs nil)
            (opencode-jobs--list)
            (should (eq (opencode-block-face (opencode-block-data block)) 'error))))))))

(ert-deftest opencode-test-jobs-subagent-and-image ()
  "Subagent and image jobs keep their owner active and settle from their notifications."
  (opencode-test-with-jobs (list (opencode-test-job "ses_child" "ses_jobs" "subagent")
                                 (opencode-test-job "image_one" "ses_jobs" "image"))
    (opencode-test-job-owner "ses_jobs")
    (opencode-jobs--list)
    (should (eq (opencode-state-status "ses_jobs") 'working))
    (opencode-test-dispatch
     '(((type . "session.inbox.enqueued") (data (sessionID . "ses_jobs")
                                                (item (type . "synthetic")
                                                      (payload (metadata (source . "subagent") (childID . "ses_child") (state . "completed"))))))))
    (should (= (length (opencode-jobs "ses_jobs")) 1))
    (opencode-test-dispatch
     '(((type . "session.synthetic") (data (sessionID . "ses_jobs")
                                           (metadata (source . "experimental_image_generate")
                                                     (jobID . "image_one") (state . "error"))))))
    (should (eq (opencode-state-status "ses_jobs") 'done))))

(ert-deftest opencode-test-jobs-services-are-independent ()
  "Two services may run the same job and session IDs without sharing their state."
  (opencode-test-with-jobs (list (opencode-test-job "sh_same" "ses_jobs"))
    (opencode-test-job-owner "ses_jobs")
    (opencode-jobs--list)
    (let ((remote (opencode-svc--make :name "jobs-peer" :remote t))
          (opencode--service-states (make-hash-table :test #'equal)))
      (opencode-with-service remote
        (should-not (opencode-jobs "ses_jobs"))
        (opencode-jobs--list remote)
        (should (opencode-jobs "ses_jobs"))
        (setq jobs nil)
        (opencode-jobs--list remote)
        (should-not (opencode-jobs "ses_jobs")))
      (should (opencode-jobs "ses_jobs")))))

(ert-deftest opencode-test-jobs-launch-and-reused-child ()
  "A live launch is blue before REST answers; continuing a child leaves its old block settled."
  (let ((id (opencode-test-scenario "shell")))
    (opencode-test-with-jobs nil
      (opencode-test-job-owner id)
      (opencode-jobs--list)
      (with-current-buffer (opencode-test-open-empty id)
        (let ((event `((type . "session.tool.success") (created . 1000)
                       (data (sessionID . ,id) (id . "call_child")
                             (metadata (sessionID . "ses_child") (status . "running")))))
              (metadata '((sessionID . "ses_child") (status . "running"))))
          (opencode-jobs--event event)
          (let ((old (opencode-test-job-block "call_child" "subagent" metadata)))
            (should (eq (opencode-state-status id) 'working))
            (should (eq (opencode-block-face (opencode-block-data old)) 'opencode-tool-running-margin))
            (opencode-jobs--event `((type . "session.synthetic")
                                    (data (sessionID . ,id) (metadata (childID . "ses_child") (state . "completed")))))
            (should (eq (opencode-block-face (opencode-block-data old)) 'opencode-tool-margin-highlight))
            (opencode-blocks-job-finished "ses_child" '((state . "completed")))
            (opencode-jobs--event event)
            (let ((new (opencode-test-job-block "call_child_again" "subagent" metadata)))
              (should (eq (opencode-block-face (opencode-block-data new)) 'opencode-tool-running-margin))
              (opencode-blocks-job-finished "ses_child" '((state . "error")))
              (should (eq (opencode-block-face (opencode-block-data new)) 'error))
              (should (eq (opencode-block-face (opencode-block-data old)) 'opencode-tool-margin-highlight)))))))))

(ert-deftest opencode-test-jobs-request-priority-and-no-polling ()
  "Jobs preserve pending requests and foreground retries, and an idle listing starts no timer."
  (opencode-test-with-jobs (list (opencode-test-job "sh_wait" "ses_jobs"))
    (opencode-test-job-owner "ses_jobs")
    (opencode-jobs--list)
    (should-not opencode-jobs--timer)
    (let ((entry (copy-sequence (opencode-state-get "ses_jobs"))))
      (setq entry (plist-put entry :requests '((:kind permission :time 3))))
      (should (eq (opencode-state-status entry) 'waiting))
      (setq entry (plist-put entry :requests nil)
            entry (plist-put entry :running t)
            entry (plist-put entry :retry '(:attempt 1)))
      (should (eq (opencode-state-status entry) 'retrying)))))

(ert-deftest opencode-test-jobs-listing-superseded ()
  "A response from before reconnection cannot restore an obsolete job."
  (opencode-test-with-jobs nil
    (opencode-test-job-owner "ses_jobs")
    (let ((opencode-test-routes
           (cons (list 'get "/job\\'" (lambda (_) (cons :defer (lambda ()
                                                                 `((data . ,(list (opencode-test-job "sh_old" "ses_jobs"))))))))
                 opencode-test-routes)))
      (opencode-jobs--list))
    (opencode-jobs--connected nil)
    (opencode-test-release)
    (should-not (opencode-jobs "ses_jobs"))
    (should (eq (opencode-state-status "ses_jobs") 'done))))

(ert-deftest opencode-test-jobs-listing-failure-and-unsupported ()
  "A failed listing keeps known work and retries; a missing route waits for reconnect."
  (opencode-test-with-jobs (list (opencode-test-job "sh_known" "ses_jobs"))
    (opencode-test-job-owner "ses_jobs")
    (opencode-jobs--list)
    (dolist (status '(500 404))
      (let ((opencode-test-routes
             (cons (list 'get "/job\\'" (lambda (_) `(:error ,status "No jobs"))) opencode-test-routes)))
        (opencode-jobs--list)
        (should (eq (opencode-state-status "ses_jobs") 'working))
        (if (= status 500)
            (should (timerp opencode-jobs--timer))
          (should opencode-jobs--unsupported)
          (should-not opencode-jobs--timer))))
    (setq jobs nil)
    (opencode-jobs--connected nil)
    (should-not opencode-jobs--unsupported)
    (should-not (opencode-jobs "ses_jobs"))))

(ert-deftest opencode-test-jobs-deleting-finished-shell-keeps-outcome ()
  "Removing a finished shell cannot turn its successful completion into cancellation."
  (opencode-test-with-jobs (list (opencode-test-job "sh_done" "ses_jobs"))
    (opencode-jobs--list)
    (opencode-test-dispatch '(((type . "shell.exited") (data (id . "sh_done") (status . "exited") (exit . 0)))
                              ((type . "shell.deleted") (data (id . "sh_done")))))
    (should (equal (alist-get 'status (opencode-job "sh_done")) "completed"))
    (should (eql (alist-get 'exit (alist-get 'metadata (opencode-job "sh_done"))) 0))))

(ert-deftest opencode-test-jobs-replayed-child-completions-belong-to-calls ()
  "A child's latest cached outcome cannot replace the older calls' transcript outcomes."
  (let ((id (opencode-test-scenario "shell")))
    (opencode-test-with-jobs (list (opencode-test-job "ses_child" id "subagent"))
      (opencode-jobs--list)
      (opencode-jobs--event `((type . "session.synthetic")
                              (data (sessionID . ,id) (metadata (childID . "ses_child") (state . "error")))))
      (with-current-buffer (opencode-test-open-empty id)
        (let* ((metadata '((sessionID . "ses_child") (status . "running")))
               (old (opencode-test-job-block "call_old" "subagent" metadata)))
          (opencode-blocks-job-finished "ses_child" '((state . "completed")))
          (should (eq (opencode-block-face (opencode-block-data old)) 'opencode-tool-margin-highlight))
          (let ((new (opencode-test-job-block "call_new" "subagent" metadata)))
            (opencode-blocks-job-finished "ses_child" '((state . "error")))
            (should (eq (opencode-block-face (opencode-block-data old)) 'opencode-tool-margin-highlight))
            (should (eq (opencode-block-face (opencode-block-data new)) 'error))))))))

(ert-deftest opencode-test-jobs-nonterminal-synthetic-cannot-freeze-block ()
  "Unrelated synthetic metadata cannot keep a launch blue after its job ends."
  (let ((id (opencode-test-scenario "shell")))
    (opencode-test-with-jobs (list (opencode-test-job "sh_live" id))
      (opencode-jobs--list)
      (with-current-buffer (opencode-test-open-empty id)
        (let ((block (opencode-test-job-block "call_live" "shell" '((shellID . "sh_live") (status . "running")))))
          (opencode-blocks-job-finished "sh_live" '((state . "running")))
          (opencode-test-dispatch '(((type . "shell.exited") (data (id . "sh_live") (status . "killed")))))
          (should (equal (opencode-tool-background-outcome (opencode-block-data block)) '("cancelled" . t))))))))

(ert-deftest opencode-test-jobs-unsupported-does-not-trust-historical-launch ()
  "Without the listing route, historical launch metadata is no evidence of running work."
  (let ((id (opencode-test-scenario "shell")))
    (opencode-test-with-jobs nil
      (with-current-buffer (opencode-test-open-empty id)
        (let ((block (opencode-test-job-block "call_old" "shell" '((shellID . "sh_old") (status . "running"))))
              (opencode-test-routes
               (cons (list 'get "/job\\'" (lambda (_) '(:error 404 "Not found"))) opencode-test-routes)))
          (opencode-jobs--list)
          (should-not (equal (plist-get (opencode-block-data block) :job-state) "running"))
          (should-not opencode-jobs--timer))))))

(provide 'opencode-jobs-test)
;;; opencode-jobs-test.el ends here
