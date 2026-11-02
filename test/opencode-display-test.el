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

(defconst opencode-test-display-agents
  (opencode-test-parse
   "[{\"id\": \"review\", \"name\": \"Review\", \"mode\": \"primary\", \"color\": \"#ff8800\"},
     {\"id\": \"docs\", \"name\": \"Docs\", \"mode\": \"primary\", \"color\": \"no-such-color\"}]")
  "Agent catalog entries added to the recorded ones.
Review configures a color Emacs knows, Docs one it does not.")

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
                                    opencode-test-display-providers)))))
         (list 'get "\\`/api/agent\\'"
               (lambda (_)
                 `((data . ,(append (alist-get 'data (opencode-test-fixture "agent.json"))
                                    opencode-test-display-agents))))))
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

(ert-deftest opencode-test-display-context-estimated ()
  "An estimated context shows a ~ before its percentage, and its tooltip says why."
  (opencode-test-with-catalogs
    (let ((workspace opencode-test-workspace))
      (let ((context (opencode-context-display 100000 opencode-test-sol workspace t)))
        (should (equal context "\U000F0A9F ~25%"))
        (should (equal (get-text-property 0 'help-echo context)
                       (concat "Context: ~100k/400k tokens\n"
                               "Estimated by the service after compaction; the next step measures it"))))
      (let ((context (opencode-context-display 190000 opencode-test-claude workspace t)))
        (should (equal context "\U000F0A9F ~19%"))
        (should (eq (get-text-property 0 'face context) 'opencode-attention))
        (should (string-suffix-p "\nContext ~190k/200k: near the 2× tier" (get-text-property 0 'help-echo context))))
      (let ((context (opencode-context-display 250000 opencode-test-claude workspace t)))
        (should (equal context "\U000F0A9F ~25% 2×"))
        (should (string-suffix-p "\nContext estimated over 200k: requests may cost 2×"
                                 (get-text-property 0 'help-echo context)))))))

(ert-deftest opencode-test-display-waits-for-catalogs ()
  "Displays needing a catalog not fetched yet are nil, and fetch it without waiting.
Its arrival runs `opencode-catalog-changed-functions'."
  (opencode-test-with-server
    (let ((opencode-test-routes (opencode-test-display-routes))
          (workspace (opencode-normalize-directory opencode-test-workspace))
          (changes nil))
      (let ((opencode-catalog-changed-functions
             (list (lambda (kind directory) (push (list kind directory) changes)))))
        (opencode-test-with-held-timers
          (should-not (opencode-model-display opencode-test-claude workspace))
          (should-not (opencode-provider-display "anthropic" workspace))
          (should-not (opencode-variant-display opencode-test-claude workspace))
          (should-not (opencode-context-display 1000 opencode-test-claude workspace))
          ;; A selected variant needs no catalog.
          (should (equal (opencode-variant-display opencode-test-sol workspace) "low"))
          (should-not opencode-test-requests)
          ;; One timer starts every fetch asked for.
          (should (= (length opencode-test-timers) 1))
          (should-not changes)
          (opencode-test-run-timers))
        ;; One fetch per catalog, however many displays asked for it.
        (should (equal (sort (mapcar (lambda (request) (plist-get request :path)) opencode-test-requests))
                       '("/api/model" "/api/provider")))
        (should (equal (sort (copy-sequence changes) :key #'car)
                       `((model ,workspace) (provider ,workspace))))
        (should (equal (opencode-model-display opencode-test-claude workspace) "Opus 5.5"))
        (should (equal (opencode-provider-display "anthropic" workspace) "\uec82"))
        (should (equal (opencode-variant-display opencode-test-claude workspace) "medium"))
        (should (equal (opencode-context-display 1000 opencode-test-claude workspace) "\U000F0A9E 0%"))))))

(ert-deftest opencode-test-catalog-changed-hook ()
  "The hook runs when a catalog arrives different, not when it is renewed unchanged."
  (opencode-test-with-catalogs
    (let* ((workspace (opencode-normalize-directory opencode-test-workspace))
           (changes nil)
           (opencode-catalog-changed-functions
            (list (lambda (kind directory) (push (list kind directory) changes))))
           (renamed nil)
           (opencode-test-routes
            (cons (list 'get "\\`/api/model\\'"
                        (lambda (_)
                          `((data . ,(append (alist-get 'data (opencode-test-fixture "model.json"))
                                             (mapcar (lambda (model)
                                                       (if (and renamed (equal (alist-get 'id model) "claude-opus-5-5"))
                                                           (cons '(name . "Claude Opus 5.6") model)
                                                         model))
                                                     opencode-test-display-models))))))
                  (opencode-test-display-routes)))
           (event (lambda (type)
                    `((type . ,type) (location (directory . ,(directory-file-name opencode-test-workspace)))
                      (data)))))
      (opencode-test-with-held-timers
        ;; The agent catalog was never fetched: nothing to renew.
        (opencode-test-dispatch (list (funcall event "agent.updated")))
        (opencode-test-run-timers)
        (should-not changes)
        (should-not (opencode-catalog-cached-p 'agent workspace))
        ;; Both catalogs are renewed, and found unchanged.
        (setq opencode-test-requests nil)
        (opencode-test-dispatch (list (funcall event "model.updated")))
        (should (equal (opencode-model-display opencode-test-claude workspace) "Opus 5.5"))
        (opencode-test-run-timers)
        (should (equal (sort (mapcar (lambda (request) (plist-get request :path)) opencode-test-requests))
                       '("/api/model" "/api/provider")))
        (should-not changes)
        (setq renamed t)
        (opencode-test-dispatch (list (funcall event "model.updated")))
        (opencode-test-run-timers)
        (should (equal changes `((model ,workspace))))
        (should (equal (opencode-model-display opencode-test-claude workspace) "Opus 5.6"))
        ;; A reconnection announces nothing by itself.
        (setq changes nil)
        (opencode-test-dispatch '(((type . "server.connected") (data))))
        (opencode-test-run-timers)
        (should-not changes)))))

(ert-deftest opencode-test-display-agent ()
  "Agents show their catalog name, bold, in their color, else a face by their place."
  (opencode-test-with-server
    (let ((opencode-test-routes (opencode-test-display-routes))
          (workspace opencode-test-workspace)
          (opencode-mode-line-agent-faces '(font-lock-keyword-face font-lock-type-face
                                            font-lock-constant-face font-lock-string-face)))
      (cl-letf (((symbol-function 'run-at-time) #'ignore))
        ;; Until the catalog arrives, nothing, and it is not waited for.
        (should-not (opencode-agent-display "build" workspace))
        (should-not opencode-test-requests))
      (opencode-catalog-fetch 'agent #'ignore workspace)
      (let ((default-directory temporary-file-directory))
        (cl-flet ((face (id) (get-text-property 0 'face (opencode-agent-display id workspace))))
          (let ((build (opencode-agent-display "build" workspace)))
            (should (equal build "Build"))
            (should (equal (get-text-property 0 'help-echo build) "Agent: Build")))
          ;; Primary agents that are not hidden take the faces in turn,
          ;; in the catalog's order: build, plan, review, docs.
          (should (equal (face "build") '(bold font-lock-keyword-face)))
          (should (equal (face "plan") '(bold font-lock-type-face)))
          (should (equal (face "review") '(bold (:foreground "#ff8800"))))
          ;; A color Emacs does not know leaves the agent its face.
          (should (equal (face "docs") '(bold font-lock-string-face)))
          ;; Subagents, and agents the catalog lacks, take the first.
          (should (equal (opencode-agent-display "general" workspace) "General"))
          (should (equal (face "general") '(bold font-lock-keyword-face)))
          (should (equal (opencode-agent-display "ghost" workspace) "ghost"))
          (let ((opencode-mode-line-agent-faces nil))
            (should (equal (face "plan") '(bold nil))))
          (should-not (opencode-agent-display nil workspace)))))))

(ert-deftest opencode-test-mode-line-agent-is-the-display ()
  "The mode line's agent is the agent's display, its ID until the catalog arrives."
  (opencode-test-with-server
    (let ((opencode-test-routes (opencode-test-display-routes)))
      (with-current-buffer (opencode-test-open (opencode-test-scenario "switch"))
        (cl-letf (((symbol-function 'run-at-time) #'ignore))
          (let ((agent (alist-get 'agent (opencode-mode-line-segments))))
            (should (equal agent "build"))
            (should (equal (get-text-property 0 'face agent)
                           (list 'bold (car opencode-mode-line-agent-faces))))))
        (opencode-catalog-fetch 'agent #'ignore)
        (dolist (id '("build" "plan" "review"))
          (setq opencode-session-agent id)
          (should (equal-including-properties (alist-get 'agent (opencode-mode-line-segments))
                                              (opencode-agent-display id))))))))

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

;;;; Catalogs

;; Displays ask for catalogs at redisplay.  A reconnection leaves the
;; cached ones stale, still shown, and renews only those displays ask
;; for again; only a catalog arriving different counts as a change, and
;; it updates the mode lines of its location's session buffers alone,
;; once per tick.

(defun opencode-test-catalog-paths ()
  "Return the (PATH DIRECTORY) of the catalog requests made, oldest first."
  (seq-keep (lambda (request)
              (when (member (plist-get request :path) '("/api/agent" "/api/model" "/api/provider"))
                (list (plist-get request :path) (opencode-normalize-directory (plist-get request :directory)))))
            (reverse opencode-test-requests)))

(defun opencode-test-other-location ()
  "Return a new scratch directory, normalized, standing for another location."
  (opencode-normalize-directory (make-temp-file "opencode-location-" t)))

(ert-deftest opencode-test-catalog-arrivals-update-mode-lines-once ()
  "Catalogs arriving update the shown mode lines of their location alone, in one pass.
Forcing every mode line of every frame once per catalog froze Emacs for
seconds after a reconnection, with many frames and locations."
  (opencode-test-with-server
    (opencode-test-with-held-timers
      (let* ((opencode-test-routes (opencode-test-display-routes))
             (here (opencode-test-open (opencode-test-scenario "switch")))
             (there (opencode-test-open (opencode-test-scenario "text")))
             ;; It shares the location, but no window shows it.
             (hidden (opencode-test-open (opencode-test-scenario "shell")))
             (updates nil)
             (passes 0))
        (with-current-buffer there
          (setq default-directory (opencode-test-other-location)))
        (opencode-test-run-timers)
        (clrhash opencode--catalogs)
        (setq opencode-test-requests nil)
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) here)
          (set-window-buffer (split-window) there)
          (cl-letf* ((update (symbol-function 'opencode--update-mode-lines-now))
                     ((symbol-function 'opencode--update-mode-lines-now)
                      (lambda () (cl-incf passes) (funcall update)))
                     ((symbol-function 'force-mode-line-update)
                      (lambda (&optional all) (push (if all 'all (current-buffer)) updates))))
            (with-current-buffer here
              (should-not (opencode-agent-display "build"))
              (should-not (opencode-model-display opencode-session-model))
              (should-not (opencode-provider-display "openai")))
            (opencode-test-run-timers)
            (should (= (length (opencode-test-catalog-paths)) 3))
            (should (= passes 1))
            (should (equal updates (list here)))
            ;; Renewed unchanged, a catalog redraws nothing.
            (setq updates nil passes 0)
            (opencode-test-dispatch
             `(((type . "agent.updated")
                (location (directory . ,(directory-file-name opencode-test-workspace))) (data))))
            (opencode-test-run-timers)
            (should (= (length (opencode-test-catalog-paths)) 4))
            (should (= passes 0))
            (should-not updates)))
        (should (buffer-live-p hidden))))))

(ert-deftest opencode-test-catalogs-stale-across-reconnection ()
  "A reconnection keeps catalogs shown and renews only those asked for again.
A renewed catalog counts as a change only when it differs."
  (opencode-test-with-catalogs
    (let* ((workspace (opencode-normalize-directory opencode-test-workspace))
           (other (opencode-test-other-location))
           (changes nil)
           (opencode-catalog-changed-functions
            (list (lambda (kind directory) (push (list kind directory) changes))))
           (renamed nil)
           (opencode-test-routes
            (cons (list 'get "\\`/api/model\\'"
                        (lambda (_)
                          `((data . ,(append (alist-get 'data (opencode-test-fixture "model.json"))
                                             (mapcar (lambda (model)
                                                       (if (and renamed (equal (alist-get 'id model) "claude-opus-5-5"))
                                                           (cons '(name . "Claude Opus 5.6") model)
                                                         model))
                                                     opencode-test-display-models))))))
                  opencode-test-routes)))
      (opencode-catalog-fetch 'model #'ignore other)
      (setq changes nil
            opencode-test-requests nil)
      (let ((generation opencode-catalog-generation)
            (location (opencode-catalog-location-generation workspace))
            (other-location (opencode-catalog-location-generation other)))
        (opencode-test-with-held-timers
          (opencode-test-dispatch '(((type . "server.connected") (data))))
          (opencode-test-run-timers)
          ;; Shown as before, and nothing fetched until asked for.
          (should-not (opencode-test-catalog-paths))
          (should (equal (opencode-model-display opencode-test-claude workspace) "Opus 5.5"))
          (should (equal (opencode-model-display opencode-test-claude workspace) "Opus 5.5"))
          (opencode-test-run-timers)
          ;; Only the catalog asked for: neither the provider's, nor the other location's.
          (should (equal (opencode-test-catalog-paths) `(("/api/model" ,workspace))))
          (should-not changes)
          (should (= generation opencode-catalog-generation))
          (should (= location (opencode-catalog-location-generation workspace)))
          ;; Renewed, it is current: asking again fetches nothing.
          (opencode-model-display opencode-test-claude workspace)
          (should (zerop (opencode-test-run-timers)))
          (should (= (length (opencode-test-catalog-paths)) 1))
          ;; A service restarted with another catalog.
          (setq renamed t)
          (opencode-test-dispatch '(((type . "server.connected") (data))))
          (should (equal (opencode-model-display opencode-test-claude workspace) "Opus 5.5"))
          (opencode-test-run-timers)
          (should (equal changes `((model ,workspace))))
          (should (= (1+ generation) opencode-catalog-generation))
          (should (= (1+ location) (opencode-catalog-location-generation workspace)))
          (should (= other-location (opencode-catalog-location-generation other)))
          (should (equal (opencode-model-display opencode-test-claude workspace) "Opus 5.6"))
          ;; The other location's stale catalog is still shown, and renewed once asked for.
          (should (equal (opencode-model-display opencode-test-claude other) "Opus 5.5"))
          (opencode-test-run-timers)
          (should (equal (car (last (opencode-test-catalog-paths))) `("/api/model" ,other)))
          (should (equal (opencode-model-display opencode-test-claude other) "Opus 5.6")))))))

(ert-deftest opencode-test-catalog-fetch-renews-stale ()
  "Commands fetching a stale catalog get it renewed, not the stale one."
  (opencode-test-with-catalogs
    (let ((workspace opencode-test-workspace)
          (received nil))
      (setq opencode-test-requests nil)
      (opencode-test-dispatch '(((type . "server.connected") (data))))
      (opencode-catalog-fetch 'provider (lambda (catalog) (push catalog received)) workspace)
      (should (equal (mapcar #'car (opencode-test-catalog-paths)) '("/api/provider")))
      (should (= (length received) 1))
      (opencode-catalog-fetch 'provider (lambda (catalog) (push catalog received)) workspace)
      (should (= (length (opencode-test-catalog-paths)) 1))
      (should (equal (car received) (cadr received))))))

(ert-deftest opencode-test-catalog-invalidated-while-fetching ()
  "An event saying a catalog changed while it is fetched fetches it once more."
  (opencode-test-with-catalogs
    (let* ((held nil)
           (opencode-test-routes
            (cons (list 'get "\\`/api/agent\\'"
                        (lambda (_)
                          (if held
                              (cons :defer (lambda () (opencode-test-fixture "agent.json")))
                            (opencode-test-fixture "agent.json"))))
                  opencode-test-routes))
           (event `((type . "agent.updated")
                    (location (directory . ,(directory-file-name opencode-test-workspace))) (data))))
      (opencode-test-with-held-timers
        (should-not (opencode-agent-display "build" opencode-test-workspace))
        (setq held t)
        (opencode-test-run-timers)
        (should (= (length (opencode-test-catalog-paths)) 3))
        (should opencode-test-deferred)
        (opencode-test-dispatch (list event event))
        (opencode-test-run-timers)
        (should (= (length (opencode-test-catalog-paths)) 3))
        (setq held nil)
        (opencode-test-release)
        ;; Shown at once, and renewed, since its answer may predate the change.
        (should (equal (opencode-agent-display "build" opencode-test-workspace) "Build"))
        (opencode-test-run-timers)
        (should (equal (mapcar #'car (last (opencode-test-catalog-paths) 2)) '("/api/agent" "/api/agent")))
        (should (= (length (opencode-test-catalog-paths)) 4))
        (opencode-agent-display "build" opencode-test-workspace)
        (should (zerop (opencode-test-run-timers)))))))

(ert-deftest opencode-test-catalog-fetches-capped ()
  "Displays fetch a few catalogs at a time, those not cached before stale ones."
  (opencode-test-with-catalogs
    (let* ((opencode-catalog-requests 2)
           (stale (opencode-normalize-directory opencode-test-workspace))
           (first (opencode-test-other-location))
           (second (opencode-test-other-location))
           (opencode-test-routes
            (cons (list 'get "\\`/api/model\\'"
                        (lambda (_) (cons :defer (lambda () (opencode-test-fixture "model.json")))))
                  opencode-test-routes)))
      (setq opencode-test-requests nil)
      (opencode-test-with-held-timers
        (opencode-test-dispatch '(((type . "server.connected") (data))))
        (dolist (directory (list stale first second))
          (opencode-model-display opencode-test-sol directory))
        (opencode-test-run-timers)
        (should (equal (opencode-test-catalog-paths) `(("/api/model" ,first) ("/api/model" ,second))))
        (opencode-test-release)
        (opencode-test-run-timers)
        (should (equal (car (last (opencode-test-catalog-paths))) `("/api/model" ,stale)))
        (should (= (length (opencode-test-catalog-paths)) 3))
        (while (opencode-test-release))
        (opencode-test-run-timers)
        (should (= (length (opencode-test-catalog-paths)) 3))
        (should (equal (opencode-model-display opencode-test-sol first) "GPT-6 Sol"))))))

(ert-deftest opencode-test-catalog-failure-waits-for-reconnection ()
  "A catalog that failed to arrive is not asked for at every redisplay.
The next stream asks again, and so does an event saying it changed."
  (opencode-test-with-server
    (let ((opencode-test-routes
           (cons (list 'get "\\`/api/model\\'" (lambda (_) '(:error 500 "Broken")))
                 opencode-test-routes))
          (event `((type . "model.updated")
                   (location (directory . ,(directory-file-name opencode-test-workspace))) (data))))
      (cl-letf (((symbol-function 'message) #'ignore))
        (opencode-test-with-held-timers
          (should-not (opencode-model-display opencode-test-sol opencode-test-workspace))
          (opencode-test-run-timers)
          (should (= (length (opencode-test-catalog-paths)) 1))
          (should-not (opencode-model-display opencode-test-sol opencode-test-workspace))
          (should (zerop (opencode-test-run-timers)))
          (opencode-test-dispatch '(((type . "server.connected") (data))))
          (should-not (opencode-model-display opencode-test-sol opencode-test-workspace))
          (opencode-test-run-timers)
          (should (= (length (opencode-test-catalog-paths)) 2))
          (opencode-test-dispatch (list event))
          (should-not (opencode-model-display opencode-test-sol opencode-test-workspace))
          (opencode-test-run-timers)
          (should (= (length (opencode-test-catalog-paths)) 3)))))))

(provide 'opencode-display-test)
;;; opencode-display-test.el ends here
