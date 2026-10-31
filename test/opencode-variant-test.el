;;; opencode-variant-test.el --- Tests of default variants  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Catalogs are shaped like the recorded responses in fixtures/v2: the
;; OpenAI and OpenCode entries come from there, trimmed, and the Claude
;; entries carry the variants OpenCode's variant.ts gives them.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-variant--json (text)
  "Parse JSON TEXT the way `opencode-request' decodes responses."
  (json-parse-string text :object-type 'alist :array-type 'list :null-object nil :false-object nil))

(defun opencode-test-variant--claude (&rest efforts)
  "Return the adaptive variants of EFFORTS as OpenCode's variant.ts spells them."
  (mapcar (lambda (effort)
            `((id . ,effort)
              (settings (thinking (type . "adaptive") (display . "summarized"))
                        (effort . ,effort))))
          efforts))

(defun opencode-test-variant--bedrock-claude (&rest efforts)
  "Return the Bedrock Converse Claude variants of EFFORTS, as variant.ts spells them."
  (mapcar (lambda (effort)
            `((id . ,effort)
              (body (additionalModelRequestFields
                     (thinking (type . "adaptive") (display . "summarized"))
                     (output_config (effort . ,effort))))))
          efforts))

(defconst opencode-test-variant-providers
  (opencode-test-variant--json
   "[{\"id\": \"opencode\", \"name\": \"OpenCode\", \"activation\": \"enabled\",
      \"package\": \"@opencode/ai/providers/openai-compatible\",
      \"settings\": {\"apiKey\": \"public\", \"baseURL\": \"https://opencode.ai/inference/openai/v1\"}},
     {\"id\": \"openai\", \"name\": \"OpenAI\", \"activation\": \"auto\",
      \"package\": \"@opencode/ai/providers/openai\",
      \"settings\": {\"transport\": \"websocket\", \"encoding\": \"zstd\",
                     \"baseURL\": \"https://chatgpt.com/backend-api/codex\"},
      \"headers\": {\"originator\": \"opencode\"}},
     {\"id\": \"anthropic\", \"name\": \"Anthropic\", \"package\": \"@opencode/ai/providers/anthropic\"},
     {\"id\": \"github-copilot\", \"name\": \"GitHub Copilot\",
      \"package\": \"aisdk:@ai-sdk/github-copilot\"},
     {\"id\": \"amazon-bedrock\", \"name\": \"Amazon Bedrock\",
      \"package\": \"@opencode/ai/providers/amazon-bedrock\"}]")
  "Provider catalog: the OpenCode and OpenAI entries of fixtures/v2/provider.json,
and Anthropic, Copilot and Bedrock entries shaped like them.")

(defconst opencode-test-variant-models
  (append
   (opencode-test-variant--json
    "[{\"id\": \"gpt-5.5\", \"modelID\": \"gpt-5.5\", \"providerID\": \"openai\", \"family\": \"gpt\",
       \"name\": \"GPT-5.5\", \"package\": \"@opencode/ai/providers/openai\",
       \"settings\": {\"encoding\": \"zstd\", \"baseURL\": \"https://chatgpt.com/backend-api/codex\"},
       \"variants\": [
         {\"id\": \"none\", \"settings\": {\"reasoningEffort\": \"none\", \"reasoningSummary\": \"auto\",
                                       \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"low\", \"settings\": {\"reasoningEffort\": \"low\", \"reasoningSummary\": \"auto\",
                                      \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"medium\", \"settings\": {\"reasoningEffort\": \"medium\", \"reasoningSummary\": \"auto\",
                                         \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"high\", \"settings\": {\"reasoningEffort\": \"high\", \"reasoningSummary\": \"auto\",
                                       \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"xhigh\", \"settings\": {\"reasoningEffort\": \"xhigh\", \"reasoningSummary\": \"auto\",
                                        \"include\": [\"reasoning.encrypted_content\"]}}],
       \"status\": \"active\", \"enabled\": true},
      {\"id\": \"gpt-6.1-sol-fast\", \"modelID\": \"gpt-6.1-sol\", \"providerID\": \"openai\",
       \"family\": \"gpt-sol\", \"name\": \"GPT-6.1 Sol Fast\", \"package\": \"@opencode/ai/providers/openai\",
       \"settings\": {\"encoding\": \"zstd\", \"baseURL\": \"https://chatgpt.com/backend-api/codex\"},
       \"body\": {\"service_tier\": \"priority\"},
       \"variants\": [
         {\"id\": \"low\", \"settings\": {\"reasoningEffort\": \"low\", \"reasoningSummary\": \"auto\",
                                      \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"high\", \"settings\": {\"reasoningEffort\": \"high\", \"reasoningSummary\": \"auto\",
                                       \"include\": [\"reasoning.encrypted_content\"]}},
         {\"id\": \"max\", \"settings\": {\"reasoningEffort\": \"max\", \"reasoningSummary\": \"auto\",
                                      \"include\": [\"reasoning.encrypted_content\"]}}],
       \"status\": \"active\", \"enabled\": true},
      {\"id\": \"big-pickle\", \"modelID\": \"big-pickle\", \"providerID\": \"opencode\",
       \"family\": \"big-pickle\", \"name\": \"Big Pickle\",
       \"compatibility\": {\"reasoningField\": \"reasoning_content\"},
       \"package\": \"@opencode/ai/providers/openai-compatible\",
       \"settings\": {\"apiKey\": \"public\", \"baseURL\": \"https://opencode.ai/inference/openai/v1\",
                      \"provider\": \"opencode\"},
       \"variants\": [], \"status\": \"active\", \"enabled\": true}]")
   `(((id . "claude-opus-5-5") (providerID . "anthropic") (name . "Claude Opus 5.5")
      (package . "@opencode/ai/providers/anthropic")
      (variants . ,(opencode-test-variant--claude "low" "medium" "high" "xhigh" "max")))
     ((id . "claude-sonnet-5-5") (providerID . "anthropic") (name . "Claude Sonnet 5.5")
      (package . "@opencode/ai/providers/anthropic")
      (variants . ,(opencode-test-variant--claude "low" "medium" "high" "xhigh" "max")))
     ((id . "claude-opus-4-6") (providerID . "anthropic") (name . "Claude Opus 4.6")
      (package . "@opencode/ai/providers/anthropic")
      (variants . ,(opencode-test-variant--claude "low" "medium" "high" "max")))
     ((id . "claude-opus-5.5") (modelID . "claude-opus-5.5") (providerID . "github-copilot")
      (name . "Claude Opus 5.5") (package . "@opencode/ai/providers/anthropic")
      (settings (baseURL . "https://api.githubcopilot.com/v1") (endpoint . "messages"))
      (variants . ,(opencode-test-variant--claude "low" "medium" "high")))
     ((id . "claude-opus-5-5") (modelID . "anthropic.claude-opus-5-5-v1:0")
      (providerID . "amazon-bedrock") (name . "Claude Opus 5.5")
      (package . "@opencode/ai/providers/amazon-bedrock")
      (variants . ,(opencode-test-variant--bedrock-claude "low" "medium" "high")))))
  "Model catalog: OpenAI and OpenCode entries of fixtures/v2/model.json,
trimmed, and Claude entries shaped like the variants variant.ts gives them.")

(defun opencode-test-variant--entry (catalog id &optional provider-id)
  "Return the entry of CATALOG whose `id' is ID, of PROVIDER-ID when given."
  (seq-find (lambda (entry)
              (and (equal (alist-get 'id entry) id)
                   (or (null provider-id) (equal (alist-get 'providerID entry) provider-id))))
            catalog))

(defun opencode-test-variant--default (provider-id id &optional settings)
  "Return the default variant of PROVIDER-ID's model ID, with model SETTINGS added."
  (let ((model (copy-alist (opencode-test-variant--entry opencode-test-variant-models id provider-id))))
    (when settings
      (setf (alist-get 'settings model) (opencode--merge-settings (alist-get 'settings model) settings)))
    (opencode-model-default-variant
     (opencode-test-variant--entry opencode-test-variant-providers provider-id)
     model)))

(ert-deftest opencode-test-default-variant ()
  "A model's default variant sends the effort a request without one sends."
  ;; OpenCode gives GPT-5 medium effort, GPT-6 nothing.
  (should (eq (opencode-test-variant--default "openai" "gpt-5.5") 'medium))
  (should-not (opencode-test-variant--default "openai" "gpt-6.1-sol-fast"))
  (should (eq (opencode-test-variant--default "openai" "gpt-6.1-sol-fast" '((reasoningEffort . "high")))
              'high))
  ;; A model without variants has none to name.
  (should-not (opencode-test-variant--default "opencode" "big-pickle" '((reasoningEffort . "high"))))
  ;; Claude 5.5 runs adaptive thinking at the API's default effort.
  (should (eq (opencode-test-variant--default "anthropic" "claude-opus-5-5") 'medium))
  (should (eq (opencode-test-variant--default "anthropic" "claude-sonnet-5-5") 'high))
  (should (eq (opencode-test-variant--default "anthropic" "claude-sonnet-5-5" '((output_config (effort . "low"))))
              'low))
  (should-not (opencode-test-variant--default "anthropic" "claude-opus-5-5" '((thinking (type . "disabled")))))
  ;; Opus 4.6 thinks only when configured to.
  (should-not (opencode-test-variant--default "anthropic" "claude-opus-4-6"))
  (should (eq (opencode-test-variant--default "anthropic" "claude-opus-4-6"
                                              '((thinking (type . "adaptive")) (effort . "max")))
              'max))
  ;; Copilot decides Claude's effort unless it is configured.
  (should-not (opencode-test-variant--default "github-copilot" "claude-opus-5.5"))
  (should (eq (opencode-test-variant--default "github-copilot" "claude-opus-5.5" '((effort . "high"))) 'high))
  ;; Bedrock carries Claude's thinking in the request body; Converse ignores an effort setting.
  (should (eq (opencode-test-variant--default "amazon-bedrock" "claude-opus-5-5") 'medium))
  (should (eq (opencode-test-variant--default "amazon-bedrock" "claude-opus-5-5" '((effort . "low"))) 'medium))
  ;; Adaptive thinking comes from binding thinking blocks, which the catalog can turn off.
  (let ((haiku `((id . "claude-haiku-5-5") (providerID . "anthropic")
                 (package . "@opencode/ai/providers/anthropic")
                 (settings (effort . "low"))
                 (variants . ,(opencode-test-variant--claude "low" "high")))))
    (should (eq (opencode-model-default-variant nil haiku) 'low))
    (should-not (opencode-model-default-variant
                 nil (cons '(compatibility (supportsThinkingBlockBinding)) haiku)))))

(ert-deftest opencode-test-default-variant-packages ()
  "Packages renamed from @opencode-ai count as their @opencode names."
  (let ((model (copy-alist (opencode-test-variant--entry opencode-test-variant-models "gpt-5.5"))))
    (setf (alist-get 'package model) "@opencode-ai/ai/providers/openai")
    (should (eq (opencode-model-default-variant nil model) 'medium))
    ;; The provider's package stands for a model naming none.
    (setf (alist-get 'package model nil t) nil)
    (should-not (opencode-model-default-variant nil model))
    (should (eq (opencode-model-default-variant
                 (opencode-test-variant--entry opencode-test-variant-providers "openai") model)
                'medium))))

(ert-deftest opencode-test-request-effort ()
  "A variant's effort is read where its settings or body put it."
  (cl-flet ((effort (provider-id id variant)
              (let ((entry (opencode-test-variant--entry
                            (alist-get 'variants (opencode-test-variant--entry opencode-test-variant-models
                                                                               id provider-id))
                            variant)))
                (opencode-request-effort (alist-get 'settings entry) (alist-get 'body entry)))))
    (should (equal (effort "openai" "gpt-5.5" "high") "high"))
    (should (equal (effort "amazon-bedrock" "claude-opus-5-5" "low") "low"))
    (should (equal (effort "anthropic" "claude-opus-5-5" "max") "max"))
    ;; The body overlay takes the request field by field, so it comes first.
    (should (equal (opencode-request-effort '((reasoningEffort . "low")) '((reasoning (effort . "high"))))
                   "high"))
    (should-not (opencode-request-effort '((reasoningSummary . "auto")) nil))))

(ert-deftest opencode-test-anthropic-model-key ()
  "Claude model IDs give their family and version, whichever order they use."
  (should (equal (opencode--anthropic-model-key "claude-opus-5-5") "opus-5-5"))
  (should (equal (opencode--anthropic-model-key "claude-opus-5.5-fast") "opus-5-5"))
  (should (equal (opencode--anthropic-model-key "anthropic.claude-opus-5-5-v1:0") "opus-5-5"))
  (should (equal (opencode--anthropic-model-key "claude-3-7-sonnet-20250219") "sonnet-3-7"))
  (should (equal (opencode--anthropic-model-key "claude-sonnet-4-20250514") "sonnet-4"))
  (should (equal (opencode--anthropic-model-key "claude-mythos-preview") "mythos-preview"))
  (should-not (opencode--anthropic-model-key "gpt-5.5")))

(ert-deftest opencode-test-anthropic-binds-thinking ()
  "OpenCode binds Claude's thinking from 5.1 on, unless the catalog says otherwise."
  (should (opencode--anthropic-binds-thinking-p "claude-opus-5-5"))
  (should (opencode--anthropic-binds-thinking-p "anthropic.claude-opus-5-1-v1:0"))
  (should (opencode--anthropic-binds-thinking-p "claude-opus-6"))
  (should-not (opencode--anthropic-binds-thinking-p "claude-opus-5"))
  (should-not (opencode--anthropic-binds-thinking-p "claude-opus-4-6"))
  (should-not (opencode--anthropic-binds-thinking-p "claude-opus-5-5" '((supportsThinkingBlockBinding))))
  (should (opencode--anthropic-binds-thinking-p "claude-opus-4-6" '((supportsThinkingBlockBinding . t)))))

(ert-deftest opencode-test-default-variant-recorded-catalogs ()
  "The recorded catalogs resolve: GPT-5 runs medium, GPT-6 and models without variants none."
  (let ((models (alist-get 'data (opencode-test-fixture "model.json")))
        (providers (alist-get 'data (opencode-test-fixture "provider.json"))))
    (cl-flet ((default (id)
                (let ((model (opencode-test-variant--entry models id)))
                  (should model)
                  (opencode-model-default-variant
                   (opencode-test-variant--entry providers (alist-get 'providerID model))
                   model))))
      (should (eq (default "gpt-5.5") 'medium))
      (should (eq (default "gpt-5.5-fast") 'medium))
      (should-not (default "gpt-6.1-sol"))
      (should-not (default "big-pickle")))))

(ert-deftest opencode-test-default-variant-is-the-default ()
  "The mode line names inherited variants with `opencode-model-default-variant' by default."
  (should (eq (default-value 'opencode-default-variant-function) #'opencode-model-default-variant))
  (should (eq (eval (car (get 'opencode-default-variant-function 'standard-value)) t)
              #'opencode-model-default-variant)))

(provide 'opencode-variant-test)
;;; opencode-variant-test.el ends here
