;;; opencode-display-test.el --- Tests of model and context displays  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The catalogs are the recorded ones of fixtures/v2, with a Claude
;; model priced in tiers, as models.dev lists Anthropic's long-context
;; pricing, and a provider without an icon, both shaped like them.

;;; Code:

(require 'opencode-test-helpers)

(defconst opencode-test-display-models
  (opencode-test-parse
   "[{\"id\": \"claude-opus-5-5\", \"modelID\": \"claude-opus-5-5\", \"providerID\": \"anthropic\",
      \"family\": \"claude-opus\", \"name\": \"Claude Opus 5.5\",
      \"package\": \"@opencode/ai/providers/anthropic\",
      \"variants\": [{\"id\": \"low\", \"settings\": {\"thinking\": {\"type\": \"adaptive\"}, \"effort\": \"low\"}},
                     {\"id\": \"medium\", \"settings\": {\"thinking\": {\"type\": \"adaptive\"}, \"effort\": \"medium\"}},
                     {\"id\": \"high\", \"settings\": {\"thinking\": {\"type\": \"adaptive\"}, \"effort\": \"high\"}}],
      \"cost\": [{\"input\": 5, \"output\": 25, \"cache\": {\"read\": 0.5, \"write\": 6.25}},
                 {\"tier\": {\"type\": \"context\", \"size\": 200000},
                  \"input\": 10, \"output\": 37.5, \"cache\": {\"read\": 1, \"write\": 12.5}}],
      \"limit\": {\"context\": 1000000, \"output\": 128000}},
     {\"id\": \"rocket\", \"modelID\": \"rocket\", \"providerID\": \"acme\", \"name\": \"Rocket\",
      \"package\": \"@opencode/ai/providers/openai-compatible\", \"cost\": []}]")
  "Model catalog entries added to the recorded ones.
Rocket has neither variants nor a context size.")

(defconst opencode-test-display-providers
  (opencode-test-parse
   "[{\"id\": \"anthropic\", \"name\": \"Anthropic\", \"package\": \"@opencode/ai/providers/anthropic\"},
     {\"id\": \"acme\", \"name\": \"Acme Cloud\", \"package\": \"@opencode/ai/providers/openai-compatible\"}]")
  "Provider catalog entries added to the recorded ones.")

(defun opencode-test-display-routes ()
  "Return routes serving the recorded catalogs with the added entries."
  (append
   (list (list 'get "\\`/api/model\\'"
               (lambda (_)
                 `((data . ,(append (alist-get 'data (opencode-test-fixture "model.json"))
                                    opencode-test-display-models)))))
         (list 'get "\\`/api/provider\\'"
               (lambda (_)
                 `((data . ,(append (alist-get 'data (opencode-test-fixture "provider.json"))
                                    opencode-test-display-providers))))))
   opencode-test-routes))

(defmacro opencode-test-with-catalogs (&rest body)
  "Run BODY with the catalogs of `opencode-test-display-routes' fetched for the workspace.
BODY runs in another directory, so displays read the workspace's
catalogs only when given its directory."
  (declare (indent 0) (debug t))
  `(opencode-test-with-server
     (let ((opencode-test-routes (opencode-test-display-routes)))
       (dolist (kind '(model provider))
         (opencode-catalog-fetch kind #'ignore opencode-test-workspace))
       (let ((default-directory temporary-file-directory))
         ,@body))))

(defconst opencode-test-claude '((id . "claude-opus-5-5") (providerID . "anthropic"))
  "Reference of the Claude model of `opencode-test-display-models'.")

(defconst opencode-test-sol '((id . "gpt-6-sol") (providerID . "openai") (variant . "low"))
  "Reference of a recorded model, with the variant the switch scenario selected.")

(ert-deftest opencode-test-display-model ()
  "Models show their catalog name, compactly without the family prefix."
  (opencode-test-with-catalogs
    (let ((workspace opencode-test-workspace))
      (should (equal (opencode-model-name opencode-test-claude workspace) "Claude Opus 5.5"))
      (should (equal (opencode-model-display opencode-test-claude workspace) "Opus 5.5"))
      (should (equal (opencode-model-name opencode-test-sol workspace) "GPT-6 Sol"))
      (should (equal (opencode-model-display opencode-test-sol workspace) "GPT-6 Sol"))
      ;; A model the catalog lost goes by its ID.
      (should (equal (opencode-model-display '((id . "gone") (providerID . "openai")) workspace) "gone"))
      (should-not (opencode-model-display nil workspace)))))

(ert-deftest opencode-test-display-provider ()
  "Providers show their icon, else a generic one and their name, named in the tooltip."
  (opencode-test-with-catalogs
    (let ((workspace opencode-test-workspace))
      (let ((icon (opencode-provider-display "anthropic" workspace)))
        (should (equal icon "\uec82"))
        (should (equal (get-text-property 0 'help-echo icon) "Provider: Anthropic (anthropic)")))
      (let ((icon (opencode-provider-display "acme" workspace)))
        (should (equal icon "\U000F048D Acme Cloud"))
        (should (equal (get-text-property 0 'help-echo icon) "Provider: Acme Cloud (acme)")))
      (should (equal (opencode-provider-display "ghost" workspace) "\U000F048D ghost"))
      (let ((opencode-provider-icons '(("\\`acme\\'" . "A"))))
        (should (equal (opencode-provider-display "acme" workspace) "A")))
      (should-not (opencode-provider-display nil workspace)))))

(ert-deftest opencode-test-display-variant ()
  "A selected variant shows bright, the default one dim, and models without variants none."
  (opencode-test-with-catalogs
    (let ((workspace opencode-test-workspace))
      (let ((variant (opencode-variant-display opencode-test-sol workspace)))
        (should (equal variant "low"))
        (should (eq (get-text-property 0 'face variant) 'success))
        (should (equal (get-text-property 0 'help-echo variant) "Variant: low (selected)")))
      ;; Claude Opus 5.5 runs adaptive thinking at medium effort by default.
      (dolist (model (list opencode-test-claude (append opencode-test-claude '((variant . "default")))))
        (let ((variant (opencode-variant-display model workspace)))
          (should (equal variant "medium"))
          (should (eq (get-text-property 0 'face variant) 'shadow))
          (should (equal (get-text-property 0 'help-echo variant) "Variant: medium (default)"))))
      (let ((opencode-default-variant-function nil))
        (let ((variant (opencode-variant-display opencode-test-claude workspace)))
          (should (equal variant "default"))
          (should (equal (get-text-property 0 'help-echo variant) "Variant: provider default"))))
      (should-not (opencode-variant-display '((id . "rocket") (providerID . "acme")) workspace))
      (should-not (opencode-variant-display nil workspace)))))

(ert-deftest opencode-test-model-label ()
  "A label names the model and the variant it runs, the default one resolved."
  (opencode-test-with-catalogs
    (let ((workspace opencode-test-workspace))
      (should (equal (opencode-model-label opencode-test-sol workspace) "GPT-6 Sol low"))
      ;; "default" means none was selected: the label tells which runs.
      (should (equal (opencode-model-label (append opencode-test-claude '((variant . "default"))) workspace)
                     "Claude Opus 5.5 medium"))
      (should (equal (opencode-model-label '((id . "rocket") (providerID . "acme")) workspace) "Rocket")))
    ;; Before the catalogs arrive, the model's ID and only a selected variant.
    (should (equal (opencode-model-label opencode-test-sol) "gpt-6-sol low"))
    (should (equal (opencode-model-label (append opencode-test-claude '((variant . "default"))))
                   "claude-opus-5-5"))))

(ert-deftest opencode-test-display-context ()
  "Context use is a pie and a percentage of the model's size, flagged near a price tier."
  (opencode-test-with-catalogs
    (let ((workspace opencode-test-workspace))
      (should (equal (opencode-context-usage 100000 opencode-test-sol workspace)
                     '(:tokens 100000 :limit 400000 :fraction 0.25 :tier nil)))
      (let ((context (opencode-context-display 100000 opencode-test-sol workspace)))
        (should (equal context "\U000F0A9F 25%"))
        (should-not (get-text-property 0 'face context))
        (should (equal (get-text-property 0 'help-echo context) "Context: 100k/400k tokens")))
      ;; The pie fills by eighths and stays full past the model's size.
      (should (equal (opencode-context-display 0 opencode-test-sol workspace) "\U000F0A9E 0%"))
      (should (equal (opencode-context-display 50000 opencode-test-sol workspace) "\U000F0A9E 12%"))
      (should (equal (opencode-context-display 400000 opencode-test-sol workspace) "\U000F0AA5 100%"))
      (should (equal (opencode-context-display 500000 opencode-test-sol workspace) "\U000F0AA5 125%"))
      (should (equal (plist-get (opencode-context-usage 500000 opencode-test-sol workspace) :fraction) 1.25))
      ;; Claude's catalog pricing doubles past 200k tokens.
      (let ((usage (opencode-context-usage 190000 opencode-test-claude workspace))
            (context (opencode-context-display 190000 opencode-test-claude workspace)))
        (should (equal (plist-get usage :tier) '(near 200000 "2×")))
        (should (equal context "\U000F0A9F 19%"))
        (should (eq (get-text-property 0 'face context) 'opencode-attention))
        (should (equal (get-text-property 0 'help-echo context)
                       "Context: 190k/1000k tokens\nContext 190k/200k: near the 2× tier")))
      (let ((context (opencode-context-display 250000 opencode-test-claude workspace)))
        (should (equal context "\U000F0A9F 25% 2×"))
        (should (eq (get-text-property 0 'face context) 'error))
        (should (equal (get-text-property 0 'help-echo context)
                       "Context: 250k/1000k tokens\nContext over 200k: requests cost 2×")))
      (let ((opencode-long-context-tiers '(("\\`openai\\'" "\\`gpt-6-sol\\'" 272000 "1.5×"))))
        (should (equal (plist-get (opencode-context-usage 300000 opencode-test-sol workspace) :tier)
                       '(over 272000 "1.5×"))))
      ;; Unknown use, or a model of unknown size, shows nothing.
      (should-not (opencode-context-usage nil opencode-test-sol workspace))
      (should-not (opencode-context-display nil opencode-test-sol workspace))
      (should-not (opencode-context-display 1000 '((id . "rocket") (providerID . "acme")) workspace))
      (should-not (opencode-context-display 1000 nil workspace)))))

(ert-deftest opencode-test-display-waits-for-catalogs ()
  "Displays needing a catalog not fetched yet are nil, and fetch it without waiting.
Its arrival runs `opencode-catalog-changed-functions'."
  (opencode-test-with-server
    (let ((opencode-test-routes (opencode-test-display-routes))
          (workspace (opencode-normalize-directory opencode-test-workspace))
          (timers nil)
          (changes nil))
      (let ((opencode-catalog-changed-functions
             (list (lambda (kind directory) (push (list kind directory) changes)))))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_time _repeat function &rest arguments)
                     (push (cons function arguments) timers))))
          (should-not (opencode-model-display opencode-test-claude workspace))
          (should-not (opencode-provider-display "anthropic" workspace))
          (should-not (opencode-variant-display opencode-test-claude workspace))
          (should-not (opencode-context-display 1000 opencode-test-claude workspace))
          ;; A selected variant needs no catalog.
          (should (equal (opencode-variant-display opencode-test-sol workspace) "low"))
          (should-not opencode-test-requests)
          ;; One fetch per catalog, however many displays asked for it.
          (should (= (length timers) 2)))
        (should-not changes)
        (dolist (timer (reverse timers))
          (apply (car timer) (cdr timer)))
        (should (equal (sort (mapcar (lambda (request) (plist-get request :path)) opencode-test-requests))
                       '("/api/model" "/api/provider")))
        (should (equal (sort (copy-sequence changes) :key #'car)
                       `((model ,workspace) (provider ,workspace))))
        (should (equal (opencode-model-display opencode-test-claude workspace) "Opus 5.5"))
        (should (equal (opencode-provider-display "anthropic" workspace) "\uec82"))
        (should (equal (opencode-variant-display opencode-test-claude workspace) "medium"))
        (should (equal (opencode-context-display 1000 opencode-test-claude workspace) "\U000F0A9E 0%"))))))

(ert-deftest opencode-test-catalog-changed-hook ()
  "Forgetting a cached catalog runs the hook; forgetting none does not."
  (opencode-test-with-catalogs
    (let* ((workspace (opencode-normalize-directory opencode-test-workspace))
           (changes nil)
           (opencode-catalog-changed-functions
            (list (lambda (kind directory) (push (list kind directory) changes))))
           (event (lambda (type)
                    `((type . ,type) (location (directory . ,(directory-file-name opencode-test-workspace)))
                      (data)))))
      (opencode-test-dispatch (list (funcall event "agent.updated")))
      (should-not changes)
      (opencode-test-dispatch (list (funcall event "model.updated")))
      (should (equal (reverse changes) `((model ,workspace) (provider ,workspace))))
      (should-not (opencode-catalog-cached-p 'model workspace))
      (cl-letf (((symbol-function 'run-at-time) #'ignore))
        (should-not (opencode-model-display opencode-test-claude workspace)))
      (opencode-catalog-fetch 'model #'ignore workspace)
      (should (equal (car changes) `(model ,workspace)))
      (setq changes nil)
      (opencode-test-dispatch '(((type . "server.connected") (data))))
      (should (equal changes '((nil nil))))
      (opencode-test-dispatch '(((type . "server.connected") (data))))
      (should (equal changes '((nil nil)))))))

(ert-deftest opencode-test-mode-line-uses-displays ()
  "The mode line's details are the displays of the session's model and context."
  (opencode-test-with-server
    (let ((opencode-test-routes (opencode-test-display-routes)))
      (with-current-buffer (opencode-test-open (opencode-test-scenario "switch"))
        (dolist (kind '(agent model provider))
          (opencode-catalog-fetch kind #'ignore))
        (dolist (model (list opencode-session-model opencode-test-claude))
          (setq opencode-session-model model
                opencode-session-tokens 250000)
          (let ((segments (opencode-mode-line-segments)))
            (cl-flet ((same (key value)
                        (should (equal-including-properties (alist-get key segments) value))))
              (same 'model (opencode-model-name model))
              (same 'short-model (opencode-model-display model))
              (same 'variant (opencode-variant-display model))
              (same 'provider (opencode-provider-display (alist-get 'providerID model)))
              (same 'context (opencode-context-display 250000 model)))))
        (should (equal (alist-get 'short-model (opencode-mode-line-segments)) "Opus 5.5"))
        (should (equal (alist-get 'context (opencode-mode-line-segments)) "\U000F0A9F 25% 2×"))))))

(provide 'opencode-display-test)
;;; opencode-display-test.el ends here
