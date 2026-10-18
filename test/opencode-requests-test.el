;;; opencode-requests-test.el --- Tests of permissions and questions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(ert-deftest opencode-test-permission-marks-its-tool ()
  "A permission request marks the block of the call that asked, expanded."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "permission"))
           (buffer (opencode-test-open-empty id))
           (rest (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")))
      (with-current-buffer buffer
        (should (= (length opencode-pending-permissions) 1))
        (let* ((permission (car opencode-pending-permissions))
               (overlay (opencode--permission-block permission)))
          (should (equal (plist-get permission :title) "Shell command: rm junk.txt"))
          (should overlay)
          (should (eq (plist-get (opencode-block-data overlay) :waiting-kind) 'permission))
          (should (string-match-p "rm junk.txt" (buffer-substring (overlay-start overlay) (overlay-end overlay))))
          (should (opencode-pending-request-p))
          ;; Answered in another client.
          (opencode-test-dispatch-until rest "permission.replied")
          (should-not opencode-pending-permissions)
          (should-not (plist-get (opencode-block-data overlay) :waiting)))))))

(ert-deftest opencode-test-permission-reply-request ()
  "Answering sends the decision to the session's reply route."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "permission"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-dispatch-until (opencode-test-events id) "permission.asked")
      (with-current-buffer buffer
        (let ((permission (car opencode-pending-permissions)))
          (opencode-permission-reply permission 'reject "Not that one.")
          (let ((request (car opencode-test-requests)))
            (should (equal (plist-get request :path)
                           (format "/api/session/%s/permission/%s/reply" id (plist-get permission :id))))
            (should (equal (plist-get request :body)
                           '((decision . "reject") (message . "Not that one.")))))
          (should-not opencode-pending-permissions))))))

(ert-deftest opencode-test-permission-reasons ()
  "Reasons are grouped by rule; a long resource shows the line its rule matched."
  (let ((script "python3 - <<'EOF'\nimport os\nmode = 'kanopy-oidc'\nEOF")
        (oidc '((action . "shell") (resource . "*kanopy-oidc*") (effect . "ask")))
        (dynamic '((action . "shell") (resource . "\"$*") (effect . "ask"))))
    (should-not (opencode-permission-reasons-text nil))
    (should (equal (substring-no-properties
                    (opencode-permission-reasons-text
                     (list `((resource . ,script) (rule . ,oidc))
                           `((resource . "\"$W\" status 2>&1") (rule . ,dynamic))
                           `((resource . "\"$W\" status") (rule . ,dynamic))
                           '((resource . "curl example.com")))))
                   (concat "\n\nAsked because:"
                           "\n  shell rule \"*kanopy-oidc*\" asks for:"
                           "\n    python3 - <<'EOF' …"
                           "\n      line 3: mode = 'kanopy-oidc'"
                           "\n  shell rule \"\"$*\" asks for:"
                           "\n    \"$W\" status 2>&1"
                           "\n  no rule allows:"
                           "\n    curl example.com")))
    (should (equal (opencode-permission-reasons-summary
                    `(((resource . "a") (rule . ,dynamic)) ((resource . "c"))))
                   "asked by rule \"\"$*\"; no rule allows the rest"))))

(ert-deftest opencode-test-question-form-on-its-block ()
  "A question form marks its tool block and is answered with option values per field."
  (opencode-test-with-server
    (let* ((id (opencode-test-scenario "question"))
           (buffer (opencode-test-open-empty id)))
      (opencode-test-dispatch-until (opencode-test-events id) "form.created")
      (with-current-buffer buffer
        (should (= (length (opencode-open-forms)) 1))
        (let* ((entry (car opencode-pending-forms))
               (overlay (opencode--form-block entry)))
          (should overlay)
          (should (eq (plist-get (opencode-block-data overlay) :waiting-kind) 'question))
          (should (overlay-get overlay 'opencode-expanded))
          ;; Blue in the first question, Apple and Plum in the second.
          (aset (plist-get entry :answers) 0 (cons (vector nil t) nil))
          (aset (plist-get entry :answers) 1 (cons (vector t nil t) nil))
          (opencode-form-send entry)
          (let ((request (car opencode-test-requests)))
            (should (string-suffix-p "/reply" (plist-get request :path)))
            (should (equal (plist-get request :body)
                           '((answer . ((q0 . "Blue") (q1 . ["Apple" "Plum"])))))))
          (should-not opencode-pending-forms))))))

(ert-deftest opencode-test-question-answers-in-summary ()
  "A completed question block summarizes each question with its answers."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "question"))
      (let ((summaries (mapcar (lambda (overlay)
                                 (buffer-substring-no-properties (overlay-start overlay)
                                                                 (1- (overlay-end overlay))))
                               (seq-filter (lambda (overlay)
                                             (equal (plist-get (opencode-block-data overlay) :tool) "question"))
                                           (opencode-tool-blocks)))))
        (should (seq-some (lambda (summary) (string-match-p "Color → blue; Fruits → apple, plum" summary))
                          summaries))))))

(ert-deftest opencode-test-live-matches-replay-everywhere ()
  "Every recorded scenario renders the same live as when replayed."
  (dolist (scenario '("read-tools" "permission" "question" "subagent" "interrupt"
                      "switch" "steer" "shell-input"))
    (opencode-test-with-server
      (let* ((id (opencode-test-scenario scenario))
             (replayed (opencode-test-text (opencode-test-open id))))
        (kill-buffer (gethash id opencode-session-buffers))
        (let ((live (opencode-test-open-empty id)))
          (opencode-test-dispatch (opencode-test-events id))
          (should (equal (cons scenario (opencode-test-text live)) (cons scenario replayed))))))))

(provide 'opencode-requests-test)
;;; opencode-requests-test.el ends here
