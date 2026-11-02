;;; opencode-modeline-test.el --- Tests of the session mode line  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-fetch-catalogs ()
  "Fetch the catalogs the mode line reads for the current buffer."
  (dolist (kind '(agent model provider))
    (opencode-catalog-fetch kind #'ignore)))

(ert-deftest opencode-test-mode-line-title ()
  "The OpenCode wrapper is stripped; other names pass through."
  (should (equal (opencode-mode-line-title "*OpenCode: Fix 50% bug*") "Fix 50% bug"))
  (should (equal (opencode-mode-line-title "*OpenCode: Retry*<2>") "Retry<2>"))
  (should (equal (opencode-mode-line-title "*scratch*") "*scratch*")))

(ert-deftest opencode-test-mode-line-escape ()
  "Percent signs are doubled and keep their faces."
  (let ((escaped (opencode-mode-line-escape (propertize "8%" 'face 'error))))
    (should (equal escaped "8%%"))
    (should (eq (get-text-property 2 'face escaped) 'error))))

(ert-deftest opencode-test-mode-line-fit ()
  "The title shrinks to its minimum before details are dropped."
  (let ((candidates '(("full" . 300) ("short" . 200) ("agent" . 50))))
    ;; Room for the full title with every detail.
    (should (equal (opencode-mode-line-fit 40 candidates 600 7 24) '("full" . 40)))
    ;; Title truncated but above its minimum: details kept.
    (should (equal (opencode-mode-line-fit 40 candidates 500 7 24) '("full" . 28)))
    ;; Below the minimum: the next level is used.
    (should (equal (opencode-mode-line-fit 40 candidates 400 7 24) '("short" . 28)))
    ;; A short title only needs its own width.
    (should (equal (opencode-mode-line-fit 10 candidates 380 7 24) '("full" . 10)))
    ;; Nothing fits: the last level keeps at least one column.
    (should (equal (opencode-mode-line-fit 40 candidates 60 7 24) '("agent" . 1)))))

(ert-deftest opencode-test-mode-line-element ()
  "The session element shows the status, title and every detail that fits."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "switch"))
      (opencode-test-fetch-catalogs)
      (should (equal mode-line-format '((:eval (opencode-mode-line-format)))))
      (pcase-let ((`((" " ,title " ") ,details) (opencode-mode-line)))
        (should (equal (substring-no-properties title) "\uf10c switch"))
        (should (string-match-p "\\`  Build  GPT-6 Sol low  \uec81  .* [0-9]+%% \\'"
                                (substring-no-properties details)))))))

(ert-deftest opencode-test-mode-line-segments ()
  "Session details come from the session's agent, model and the catalogs."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "switch"))
      (opencode-test-fetch-catalogs)
      (let ((segments (opencode-mode-line-segments)))
        (should (equal (alist-get 'agent segments) "Build"))
        (should (equal (alist-get 'model segments) "GPT-6 Sol"))
        (should (equal (alist-get 'variant segments) "low"))
        (should (eq (get-text-property 0 'face (alist-get 'variant segments)) 'opencode-setting-chosen))
        (should (equal (alist-get 'provider segments) "\uec81"))
        (should (equal (get-text-property 0 'help-echo (alist-get 'provider segments))
                       "Provider: OpenAI (openai)"))
        (should (string-match-p "\\` [0-9]+%\\'"
                                (substring-no-properties (alist-get 'context segments) 1))))
      ;; An unselected variant shows dim, as the provider default or the configured one.
      (setf (alist-get 'variant opencode-session-model) nil)
      (should (equal (alist-get 'variant (opencode-mode-line-segments)) "default"))
      (let ((opencode-default-variant-function (lambda (_provider _model) (quote medium))))
        (let ((variant (alist-get 'variant (opencode-mode-line-segments))))
          (should (equal variant "medium"))
          (should (eq (get-text-property 0 'face variant) 'opencode-setting-inherited))))
      ;; By default, the variant sending the effort a request without one sends.
      (let ((opencode-session-model '((id . "gpt-5.5") (providerID . "openai"))))
        (let ((variant (alist-get 'variant (opencode-mode-line-segments))))
          (should (equal variant "medium"))
          (should (eq (get-text-property 0 'face variant) 'opencode-setting-inherited))
          (should (equal (get-text-property 0 'help-echo variant) "Variant: medium (default)")))
        (let ((opencode-default-variant-function nil))
          (should (equal (get-text-property 0 'help-echo (alist-get 'variant (opencode-mode-line-segments)))
                         "Variant: provider default"))))
      ;; Unknown providers show their name.
      (let ((opencode-provider-icons nil))
        (should (string-suffix-p " OpenAI" (alist-get 'provider (opencode-mode-line-segments))))))))

(ert-deftest opencode-test-mode-line-segments-follow-catalogs ()
  "Details computed before the catalogs arrive are recomputed once they do."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "switch"))
      (cl-letf (((symbol-function 'run-at-time) #'ignore))
        (should (equal (alist-get 'agent (opencode-mode-line-segments)) "build")))
      (opencode-test-fetch-catalogs)
      (should (equal (alist-get 'agent (opencode-mode-line-segments)) "Build")))))

(ert-deftest opencode-test-long-context-tiers ()
  "Price tiers come from catalog pricing or `opencode-long-context-tiers'."
  (let ((model '((id . "gpt-6-sol") (providerID . "github-copilot")))
        (priced '((id . "big") (providerID . "acme")
                  (cost ((input . 3) (output . 15) (cache (read . 0) (write . 0)))
                        ((tier (type . "context") (size . 200000))
                         (input . 6) (output . 22.5) (cache (read . 0) (write . 0))))))
        (opencode-session-tokens 100000))
    (should-not (opencode-long-context-threshold model '((cost))))
    (let ((opencode-long-context-tiers '(("\\`github-copilot\\'" "\\`gpt-6-sol\\'" 272000 "2×"))))
      (should (equal (opencode-long-context-threshold model '((cost))) '(272000 . "2×")))
      (should-not (opencode-long-context-tier model '((cost))))
      (setq opencode-session-tokens 250000)
      (should (equal (opencode-long-context-tier model '((cost))) '(near 272000 "2×")))
      (setq opencode-session-tokens 280000)
      (should (equal (opencode-long-context-tier model '((cost))) '(over 272000 "2×")))
      (should (equal (opencode-long-context-description '(over 272000 "2×"))
                     "Past 272k tokens: requests cost 2×"))
      (should (equal (opencode-long-context-description '(near 272000 "2×") 250000 t)
                     "Estimated near 272k tokens, past which requests cost 2×")))
    (should (equal (opencode-long-context-threshold '((id . "big") (providerID . "acme")) priced)
                   '(200000 . "2×")))))

(ert-deftest opencode-test-long-context-reported-once ()
  "Crossing a price tier is reported once, and not again after reopening."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "switch"))
      (let ((opencode-long-context-tiers '(("\\`openai\\'" "\\`gpt-6-sol\\'" 272000 "2×")))
            (reports nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args) (push (apply #'format format args) reports))))
          (setq opencode-session-tokens 280000)
          (opencode--long-context-check)
          (opencode--long-context-check)
          (should (equal reports '("OpenCode: switch exceeds 272k tokens of context; requests cost 2×")))
          (setq opencode-session-tokens 1000)
          (opencode--long-context-check)
          (should-not opencode--long-context-notified)
          (setq opencode-session-tokens 280000)
          (opencode--long-context-replayed)
          (opencode--long-context-check)
          (should (= (length reports) 1)))))))

(defun opencode-test-compaction-ended (session &optional context)
  "Return the end of a manual compaction of SESSION, estimating CONTEXT.
Its usage is the compaction request's, which read the conversation it replaced."
  `((type . "session.compaction.ended")
    (data (sessionID . ,session) (reason . "manual") (model (id . "gpt-6-sol") (providerID . "openai"))
          (text . "## Objective\n- go on") ,@(when context `((context . ,context))) (cost . 0.4)
          (tokens (input . 2000) (output . 900) (reasoning . 0) (cache (read . 118000) (write . 0))))))

(defun opencode-test-step-ended (session input)
  "Return the end of a step of SESSION whose usage counts INPUT tokens."
  `((type . "session.step.ended")
    (data (sessionID . ,session) (assistantMessageID . "msg_step") (finish . "stop") (cost . 0)
          (tokens (input . ,input) (output . 0) (reasoning . 0) (cache (read . 0) (write . 0))))))

(ert-deftest opencode-test-mode-line-context-after-compaction ()
  "After a compaction the context is the service's estimate, with a ~, until a step measures it."
  (let ((id (opencode-test-scenario "switch")))
    (opencode-test-with-server
      (with-current-buffer (opencode-test-open-empty id)
        (opencode-test-fetch-catalogs)
        (cl-flet ((context () (alist-get 'context (opencode-mode-line-segments))))
          (opencode-test-dispatch (list (opencode-test-step-ended id 120000)))
          (should (equal (context) "\U000F0AA0 30%"))
          (should-not opencode-session-tokens-estimated)
          ;; Not the usage of the compaction's request, which read the 120k replaced.
          (opencode-test-dispatch (list (opencode-test-compaction-ended id 4200)))
          (should (equal opencode-session-tokens 4200))
          (should opencode-session-tokens-estimated)
          (should (equal (context) "\U000F0A9E ~1%"))
          (should (equal (get-text-property 0 'help-echo (context))
                         (concat "Context: ~4.2k of 400k tokens (1%)\n"
                                 "Estimated by the service after compaction; the next step measures it")))
          (opencode-test-dispatch (list (opencode-test-step-ended id 4900)))
          (should (equal (context) "\U000F0A9E 1%"))
          (should-not opencode-session-tokens-estimated)
          ;; Without an estimate, as from a native compaction or an older
          ;; service, no size shows until the next step.
          (opencode-test-dispatch (list (opencode-test-compaction-ended id)))
          (should-not opencode-session-tokens)
          (should-not (context)))))
    ;; Replayed, the newest of the usage and the completed compaction holds the size.
    (pcase-dolist (`(,messages ,tokens ,estimated)
                   `(((((id . "msg_a") (type . "assistant") (time (created . 10))
                        (tokens (input . 120000) (output . 0) (reasoning . 0) (cache (read . 0) (write . 0))))
                       ((id . "msg_c") (type . "compaction") (status . "completed") (reason . "manual")
                        (summary . "## Objective") (context . 4200) (time (created . 20))
                        (tokens (input . 2000) (output . 900) (reasoning . 0) (cache (read . 118000) (write . 0)))))
                      4200 t)
                     ((((id . "msg_a") (type . "assistant") (time (created . 10))
                        (tokens (input . 120000) (output . 0) (reasoning . 0) (cache (read . 0) (write . 0))))
                       ((id . "msg_c") (type . "compaction") (status . "completed") (reason . "manual")
                        (summary . "## Objective") (time (created . 20))))
                      nil nil)
                     ((((id . "msg_a") (type . "assistant") (time (created . 10))
                        (tokens (input . 120000) (output . 0) (reasoning . 0) (cache (read . 0) (write . 0))))
                       ((id . "msg_c") (type . "compaction") (status . "failed") (reason . "manual")
                        (error (type . "compaction.failed") (message . "no summary")) (time (created . 20)))
                       ((id . "msg_r") (type . "compaction") (status . "running") (reason . "manual")
                        (summary . "") (time (created . 30))))
                      120000 nil)))
      (opencode-test-with-server
        (let ((opencode-test-routes
               (cons (list 'get "/message\\'" (lambda (_) `((data . ,messages) (cursor))))
                     opencode-test-routes)))
          (with-current-buffer (opencode-test-open id)
            (should (equal (list opencode-session-tokens opencode-session-tokens-estimated)
                           (list tokens estimated)))))))))

(ert-deftest opencode-test-context-reverted ()
  "A revert finds the size again in what remains of the transcript, or none."
  (let ((id (opencode-test-scenario "switch")))
    (opencode-test-with-server
      (with-current-buffer (opencode-test-open-empty id)
        (opencode-test-dispatch (list (opencode-test-step-ended id 120000)
                                      (opencode-test-compaction-ended id 4200)))
        (should (equal (list opencode-session-tokens opencode-session-tokens-estimated) '(4200 t)))
        (let ((opencode-test-routes
               (cons (list 'get "/message\\'" (lambda (_) '((data) (cursor)))) opencode-test-routes)))
          (opencode-test-dispatch `(((type . "session.revert.committed")
                                     (data (sessionID . ,id) (to . "msg_first"))))))
        (should (equal (list opencode-session-tokens opencode-session-tokens-estimated) '(nil nil)))))))

(ert-deftest opencode-test-long-context-estimate-not-reported ()
  "An estimate past a price tier is shown, not reported; the next step past it is."
  (let ((id (opencode-test-scenario "switch")))
    (opencode-test-with-server
      (with-current-buffer (opencode-test-open-empty id)
        (opencode-test-fetch-catalogs)
        (let ((opencode-long-context-tiers '(("\\`openai\\'" "\\`gpt-6-sol\\'" 272000 "2×")))
              (reports nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format &rest args) (push (apply #'format format args) reports))))
            (opencode-test-dispatch (list (opencode-test-step-ended id 280000)))
            (should (= (length reports) 1))
            (opencode-test-dispatch (list (opencode-test-compaction-ended id 290000)))
            (should (= (length reports) 1))
            (should-not opencode--long-context-notified)
            (let ((context (alist-get 'context (opencode-mode-line-segments))))
              (should (equal context "\U000F0AA3 ~72% 2×"))
              (should (eq (get-text-property 0 'face context) 'opencode-context-over))
              (should (string-suffix-p "\nEstimated past 272k tokens: requests cost 2×"
                                       (get-text-property 0 'help-echo context))))
            ;; Reopened on the estimate, the next step past the tier still reports it.
            (opencode--long-context-replayed)
            (should-not opencode--long-context-notified)
            (opencode-test-dispatch (list (opencode-test-step-ended id 280000)))
            (should (= (length reports) 2))))))))

(ert-deftest opencode-test-mode-line-format ()
  "The default mode line keeps its place around the session element."
  (let ((mode-line-modes '("%[(" (:propertize ("" mode-name)) ("" mode-line-process) " ok" ")%]"))
        (default '("%e" mode-front-space mode-line-modified mode-line-remote
                   mode-line-buffer-identification "   " mode-line-position
                   mode-line-modes mode-line-misc-info))
        (opencode--mode-line-format-warned nil)
        (saved (default-value 'mode-line-format)))
    (with-temp-buffer
      (setq-default mode-line-format default)
      (unwind-protect
          (progn
            (should (equal (opencode-mode-line-format)
                           `("%e" mode-front-space ,opencode-mode-line-element
                             ("%[(" " ok" ")%]") mode-line-misc-info)))
            (setq-default mode-line-format '("%e" mode-line-misc-info))
            (let ((delayed-warnings-list nil))
              (should (equal (opencode-mode-line-format) '("%e" mode-line-misc-info)))
              (opencode-mode-line-format)
              (should (= (length delayed-warnings-list) 1))))
        (setq-default mode-line-format saved)))))

(ert-deftest opencode-test-mode-line-fitted-once ()
  "A mode line is fitted again only when what it shows, or its room, changed.
It is redrawn on every streamed chunk, in every window showing the session."
  (opencode-test-with-server
    (opencode-test-with-held-timers
      (let* ((renamed nil)
             (opencode-test-routes
              (cons (list 'get "\\`/api/agent\\'"
                          (lambda (_)
                            (let ((agents (opencode-test-fixture "agent.json")))
                              (if renamed
                                  `((data . ,(mapcar (lambda (agent)
                                                       (if (equal (alist-get 'id agent) "plan")
                                                           (cons '(name . "Planner") agent)
                                                         agent))
                                                     (alist-get 'data agents))))
                                agents))))
                    opencode-test-routes)))
        (with-current-buffer (opencode-test-open (opencode-test-scenario "switch"))
          (opencode-test-fetch-catalogs)
          (opencode-test-run-timers)
          (save-window-excursion
            (delete-other-windows)
            (set-window-buffer (selected-window) (current-buffer))
            (let ((fits 0)
                  (computed 0))
              (cl-letf* ((fit (symbol-function 'opencode-mode-line-fit))
                         ((symbol-function 'opencode-mode-line-fit)
                          (lambda (&rest arguments) (cl-incf fits) (apply fit arguments)))
                         (compute (symbol-function 'opencode--mode-line-compute-segments))
                         ((symbol-function 'opencode--mode-line-compute-segments)
                          (lambda () (cl-incf computed) (funcall compute))))
                (cl-flet ((fits (expected)
                            (setq fits 0)
                            (opencode-mode-line)
                            (should (= fits expected)))
                          (title () (substring-no-properties (cadr (car (opencode-mode-line)))))
                          (details () (substring-no-properties (cadr (opencode-mode-line)))))
                  (fits 1)
                  (fits 0)
                  (should (eq (cadr (opencode-mode-line)) (cadr (opencode-mode-line))))
                  (setq opencode-session-status 'busy)
                  (fits 1)
                  ;; A running turn's icon is a frame of its animation, and
                  ;; the next frame fits nothing again.
                  (let ((frames (alist-get 'working opencode-session-status-animations)))
                    (cl-letf (((symbol-function 'opencode-session-status-frame-index) (lambda (&rest _) 0)))
                      (should (string-prefix-p (nth 0 frames) (title))))
                    (cl-letf (((symbol-function 'opencode-session-status-frame-index) (lambda (&rest _) 3)))
                      (fits 0)
                      (should (string-prefix-p (nth 3 frames) (title)))))
                  (fits 0)
                  (setq opencode-pending-permissions (list (list :id "per_test")))
                  (fits 1)
                  (should (string-prefix-p "\U000F02D7" (title)))
                  (setq opencode-pending-permissions nil)
                  (fits 1)
                  (setq opencode-session-agent "plan")
                  (fits 1)
                  (should (string-prefix-p "  Plan  " (details)))
                  (rename-buffer "*OpenCode: renamed*" t)
                  (fits 1)
                  (should (string-suffix-p " renamed" (title)))
                  (fits 0)
                  (opencode-mode-line-flush)
                  (fits 1)
                  ;; Another location's catalog leaves the details alone.
                  (setq computed 0)
                  (opencode-catalog-fetch 'agent #'ignore (make-temp-file "opencode-location-" t))
                  (fits 0)
                  (should (zerop computed))
                  ;; Its own location's catalog changing renews them.
                  (setq renamed t)
                  (opencode-test-dispatch
                   `(((type . "agent.updated")
                      (location (directory . ,(directory-file-name opencode-test-workspace))) (data))))
                  (opencode-test-run-timers)
                  (fits 1)
                  (should (= computed 1))
                  (should (string-prefix-p "  Planner  " (details)))
                  ;; A narrower window has less room.
                  (split-window-right)
                  (fits 1)
                  (fits 0))))))))))

(ert-deftest opencode-test-mode-line-format-follows-default ()
  "The adapted format follows the default one, even when changed in place."
  (let ((mode-line-modes '("%[(" (:propertize ("" mode-name)) ("" mode-line-process) " ok" ")%]"))
        (default (list "%e" 'mode-front-space 'mode-line-buffer-identification "   "
                       'mode-line-modes 'mode-line-misc-info 'mode-line-end-spaces))
        (saved (default-value 'mode-line-format)))
    (setq-default mode-line-format default)
    (unwind-protect
        (progn
          (should (memq 'mode-line-misc-info (opencode-mode-line-format)))
          (should (eq (opencode-mode-line-format) (opencode-mode-line-format)))
          (delq 'mode-line-misc-info default)
          (should-not (memq 'mode-line-misc-info (opencode-mode-line-format)))
          (setcar (last mode-line-modes) "]")
          (should (equal (car (last (nth 3 (opencode-mode-line-format)))) "]")))
      (setq-default mode-line-format saved))))

(ert-deftest opencode-test-mode-line-follows-the-rest ()
  "Whatever widens the rest of the mode line leaves the session details less room.
Batch Emacs formats every mode line as an empty string."
  :tags '(gui)
  (skip-unless (display-graphic-p))
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "switch"))
      (opencode-test-fetch-catalogs)
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (selected-window) (current-buffer))
        ;; Other tests may leave global modes with lighters behind.
        (let* ((minor-mode-alist nil)
               (mode-line-misc-info nil)
               (before (cadr (opencode-mode-line))))
          (should (string-match-p "GPT-6 Sol low" before))
          (setq mode-line-misc-info (list (make-string (/ (window-width) 2) ?x)))
          (should (< (length (cadr (opencode-mode-line))) (length before)))
          (setq mode-line-misc-info nil)
          (should (equal (cadr (opencode-mode-line)) before)))))))

(ert-deftest opencode-test-mode-line-renders ()
  "A session buffer's mode line renders its title and agent.
Batch Emacs formats every mode line as an empty string."
  :tags '(gui)
  (skip-unless (display-graphic-p))
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "switch"))
      (opencode-test-fetch-catalogs)
      (should (equal mode-line-format '((:eval (opencode-mode-line-format)))))
      (let ((text (format-mode-line mode-line-format nil nil (current-buffer))))
        (should (string-match-p "switch" text))
        (should (string-match-p "Build" text))))))

(ert-deftest opencode-test-mode-line-status-shares-the-look ()
  "The mode line's icon is the session's status as every display of sessions shows it.
Subagents in the middle of a turn and the archive follow it, as in lists."
  (opencode-test-with-server
    (opencode-test-with-held-timers
      (let ((id (opencode-test-scenario "switch")))
        (with-current-buffer (opencode-test-open id)
          (cl-flet ((icons () (substring-no-properties (plist-get (opencode--mode-line-status) :still)))
                    (face () (get-text-property 0 'face (plist-get (opencode--mode-line-status) :still)))
                    (help () (get-text-property 0 'help-echo (plist-get (opencode--mode-line-status) :still))))
            (should (equal (icons) "\uf10c"))
            (should (eq (face) 'opencode-session-status-idle))
            ;; Its own request, then only a subagent's.
            (setq opencode-pending-permissions (list (list :id "per_test")))
            (should (equal (icons) "\U000F02D7"))
            (should (eq (face) 'opencode-session-status-waiting))
            (should (equal (help) "Waiting for your answer"))
            (setq opencode-pending-permissions nil
                  opencode--pending-children (list "ses_child"))
            (should (equal (icons) (concat "\uf10c " (opencode-subagent-glyph 'waiting))))
            (setq opencode--pending-children nil)
            ;; What the registry tells: a turn ended unseen, subagents at
            ;; any depth at work, the archive.
            (let ((entry (opencode-state--info-entry opencode-session-info)))
              (opencode-state--store id (plist-put (plist-put (copy-sequence entry) :idle 2.0) :viewed 1.0))
              (should (equal (icons) "\U000F05E0"))
              (should (eq (face) 'opencode-session-status-done))
              (opencode-state--store "ses_child" (opencode-state--make-entry :id "ses_child" :parent-id id
                                                                             :running t))
              (should (equal (icons) (concat "\U000F05E0 " (opencode-subagent-glyph 'working))))
              (opencode-state--store "ses_grandchild"
                                     (opencode-state--make-entry
                                      :id "ses_grandchild" :parent-id "ses_child" :running t
                                      :requests '((:kind question :id "frm_test" :time 1.0))))
              (should (equal (icons) (concat "\U000F05E0 " (opencode-subagent-glyph 'waiting)
                                             " " (opencode-subagent-glyph 'working))))
              (opencode-state--store "ses_child" nil)
              (opencode-state--store "ses_grandchild" nil)
              (opencode-state--store id (plist-put (copy-sequence (opencode-state-get id)) :archived 3.0))
              (should (equal (icons) (concat "\U000F05E0 " opencode-archived-icon))))
            ;; A turn waiting to retry turns the clock, its tooltip telling why.
            (setq opencode-session-status 'busy
                  opencode-session-retry '((attempt . 2) (error (message . "rate limited"))))
            (let ((status (opencode--mode-line-status)))
              (should (equal (substring-no-properties (plist-get status :still) 0 1) "\uf021"))
              (should (equal (mapcar (lambda (frame) (substring-no-properties frame 0 1)) (plist-get status :frames))
                             (alist-get 'retrying opencode-session-status-animations)))
              (should (equal (get-text-property 0 'help-echo (car (plist-get status :frames)))
                             "Retry #2: rate limited")))))))))

(ert-deftest opencode-test-mode-line-animates ()
  "A running turn's icon turns from one timer, while a window of a visible frame shows it.
A tick updates the mode lines of the buffers animated, and only those."
  (opencode-test-with-server
    (opencode-test-with-held-timers
      (let ((one (opencode-test-open (opencode-test-scenario "switch")))
            (two (opencode-test-open (opencode-test-scenario "text")))
            (other (get-buffer-create " *opencode-test-other*"))
            (forced nil))
        (unwind-protect
            (cl-letf (((symbol-function 'force-mode-line-update)
                       (lambda (&optional all) (push (if all 'all (current-buffer)) forced))))
              (save-window-excursion
                (delete-other-windows)
                (set-window-buffer (selected-window) one)
                (set-window-buffer (split-window) two)
                (cl-flet ((draw (buffer)
                            (with-selected-window (get-buffer-window buffer)
                              (opencode-mode-line)))
                          (ticks ()
                            (seq-filter (lambda (timer) (eq (timer--function timer) #'opencode--mode-line-tick))
                                        opencode-test-timers)))
                  ;; Idle, nothing animates.
                  (draw one)
                  (should-not (ticks))
                  (should-not opencode--mode-line-animation-timer)
                  (dolist (buffer (list one two))
                    (with-current-buffer buffer
                      (setq opencode-session-status 'busy))
                    (draw buffer))
                  (should (= (length (ticks)) 1))
                  (should (seq-set-equal-p opencode--mode-line-animated (list one two)))
                  (setq forced nil)
                  (opencode--mode-line-tick)
                  (should (seq-set-equal-p forced (list one two)))
                  ;; Hidden, a buffer is dropped until drawn again.
                  (set-window-buffer (get-buffer-window two) other)
                  (setq forced nil)
                  (opencode--mode-line-tick)
                  (should (equal forced (list one)))
                  (should (equal opencode--mode-line-animated (list one)))
                  ;; Drawn still once its turn ended, the last one stops the timer.
                  (with-current-buffer one
                    (setq opencode-session-status 'idle))
                  (draw one)
                  (setq forced nil)
                  (opencode--mode-line-tick)
                  (should-not forced)
                  (should-not opencode--mode-line-animated)
                  (should-not opencode--mode-line-animation-timer)
                  (opencode--mode-line-tick)
                  ;; Without animations, a running turn's icon stays still.
                  (let ((opencode-session-status-animation-interval nil))
                    (with-current-buffer one
                      (setq opencode-session-status 'busy))
                    (setq opencode-test-timers nil)
                    (should (equal (substring-no-properties (cadr (car (draw one))) 0 1) "\U000F051F"))
                    (should-not (ticks)))
                  ;; A pace changed while it ticks: the mode lines start anew at it.
                  (draw one)
                  (should opencode--mode-line-animation-timer)
                  (let ((opencode-session-status-animation-interval 0.5))
                    (setq forced nil)
                    (opencode--mode-line-tick)
                    (should (equal forced (list one)))
                    (should-not opencode--mode-line-animation-timer)))))
          (kill-buffer other))))))

(ert-deftest opencode-test-mode-line-follows-the-registry ()
  "A registry change updates the mode lines whose icons it changes, and only those."
  (opencode-test-with-server
    (opencode-test-with-held-timers
      (let* ((id (opencode-test-scenario "switch"))
             (one (opencode-test-open id))
             (two (opencode-test-open (opencode-test-scenario "text")))
             (forced nil))
        (cl-letf (((symbol-function 'force-mode-line-update)
                   (lambda (&optional all) (push (if all 'all (current-buffer)) forced))))
          (save-window-excursion
            (delete-other-windows)
            (set-window-buffer (selected-window) one)
            (set-window-buffer (split-window) two)
            (dolist (buffer (list one two))
              (with-selected-window (get-buffer-window buffer)
                (opencode-mode-line)))
            (opencode-test-run-timers)
            (setq forced nil)
            (opencode-state--store "ses_child" (opencode-state--make-entry :id "ses_child" :parent-id id
                                                                           :running t))
            (opencode-state--store "ses_unrelated" (opencode-state--make-entry :id "ses_unrelated"
                                                                               :running t))
            ;; One check for any number of changes in a tick.
            (should (= (opencode-test-run-timers) 1))
            (should (equal forced (list one)))
            (setq forced nil)
            (opencode-state--store "ses_unrelated" nil)
            (opencode-test-run-timers)
            (should-not forced)))))))

(ert-deftest opencode-test-mode-line-frames-keep-room ()
  "Every frame of an animated icon takes the room of its still string, to the pixel.
Batch Emacs measures characters, not pixels."
  :tags '(gui)
  (skip-unless (display-graphic-p))
  (opencode-test-with-server
    (opencode-test-with-held-timers
      (with-current-buffer (opencode-test-open (opencode-test-scenario "switch"))
        (dolist (kind '(working retrying))
          (setq opencode-session-status 'busy
                opencode-session-retry (and (eq kind 'retrying) '((attempt . 1)))
                opencode--mode-line-status nil)
          (let* ((status (opencode--mode-line-status))
                 (still (opencode--mode-line-measure (plist-get status :still))))
            (should (= (length (plist-get status :frames))
                       (length (alist-get kind opencode-session-status-animations))))
            (dolist (frame (plist-get status :frames))
              (should (= (opencode--mode-line-measure frame) still)))))))))

(provide 'opencode-modeline-test)
;;; opencode-modeline-test.el ends here
