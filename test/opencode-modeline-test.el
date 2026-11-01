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
        (should (equal (substring-no-properties title) "\U000F05E0 switch"))
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
        (should (eq (get-text-property 0 'face (alist-get 'variant segments)) 'success))
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
          (should (eq (get-text-property 0 'face variant) 'shadow))))
      ;; By default, the variant sending the effort a request without one sends.
      (let ((opencode-session-model '((id . "gpt-5.5") (providerID . "openai"))))
        (let ((variant (alist-get 'variant (opencode-mode-line-segments))))
          (should (equal variant "medium"))
          (should (eq (get-text-property 0 'face variant) 'shadow))
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
                     "Context over 272k: requests cost 2×")))
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
                         (concat "Context: ~4k/400k tokens\n"
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
              (should (eq (get-text-property 0 'face context) 'error))
              (should (string-suffix-p "\nContext estimated over 272k: requests may cost 2×"
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

(provide 'opencode-modeline-test)
;;; opencode-modeline-test.el ends here
