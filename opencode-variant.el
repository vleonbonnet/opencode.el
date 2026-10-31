;;; opencode-variant.el --- The variant a model runs without one  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A session whose model reference names no variant sends neither a
;; variant's settings nor its body: the request carries what the
;; provider and model catalog entries configure, and the provider applies
;; its own defaults to the rest.  The service never says which variant
;; that amounts to, so `opencode-model-default-variant' works it out:
;; the effort such a request sends, read where OpenCode's variants put
;; it, then the variant sending the same effort.  For Claude models the
;; effort counts only under adaptive thinking, the one mode every
;; Anthropic variant pairs with its effort, and what the request leaves
;; unset takes the API's defaults, except behind Copilot, which decides
;; Claude's effort itself.

;;; Code:

(require 'map)
(require 'opencode-common)

(defcustom opencode-anthropic-default-efforts
  '(("opus-5-5" . "medium")
    ("opus-5" . "high")
    ("sonnet-5-5" . "high")
    ("sonnet-5" . "high")
    ("fable-5-1" . "high")
    ("fable-5" . "high")
    ("mythos-5-1" . "high")
    ("mythos-5" . "high")
    ("mythos-preview" . "high"))
  "Claude models defaulting to adaptive thinking, with their default effort.
Keys are \"FAMILY-VERSION\" model keys, such as \"opus-5-5\" for
claude-opus-5-5 or claude-opus-5.5.  Models absent here either default
to thinking off (Opus 4.6 to 4.8, Sonnet 4.6, the 4.5 generation) or are
unknown, so no variant reproduces their default request."
  :type '(alist :key-type (string :tag "Model key") :value-type (string :tag "Effort"))
  :group 'opencode)

(defconst opencode--anthropic-packages
  '("@opencode/ai/providers/anthropic" "@opencode/ai/providers/anthropic-compatible"
    "@opencode/ai/providers/amazon-bedrock" "@opencode/ai/providers/amazon-bedrock/mantle/messages"
    "@opencode/ai/providers/google-vertex/messages" "@opencode/ai/providers/google-vertex-messages"
    "@opencode/ai/providers/vercel-ai-gateway" "@opencode/ai/providers/vercel-ai-gateway/messages"
    "@opencode/ai/providers/cloudflare-ai-gateway")
  "OpenCode packages sending Claude models Anthropic's thinking fields.
Their protocols, Anthropic Messages or Bedrock Converse, bind Claude's
thinking blocks.")

(defconst opencode--openai-packages
  '("@opencode/ai/providers/openai" "@opencode/ai/providers/openai/chat"
    "@opencode/ai/providers/openai/responses" "@opencode/ai/providers/azure"
    "@opencode/ai/providers/azure/chat" "@opencode/ai/providers/azure/responses"
    "@opencode/ai/providers/amazon-bedrock/mantle"
    "@opencode/ai/providers/amazon-bedrock/mantle/chat"
    "@opencode/ai/providers/amazon-bedrock/mantle/responses"
    "@opencode/ai/providers/amazon-bedrock-mantle")
  "OpenCode packages adding OpenAI's GPT-5 defaults to every request.")

(defconst opencode--effort-paths
  '((body reasoning effort) (body reasoning_effort) (body output_config effort)
    (body additionalModelRequestFields output_config effort)
    (body additionalModelRequestFields reasoningConfig maxReasoningEffort)
    (body additionalModelRequestFields reasoning_effort)
    (body additionalModelRequestFields reasoning effort)
    (body chat_template_kwargs reasoning_effort)
    (settings reasoningEffort) (settings effort) (settings output_config effort)
    (settings outputConfig effort) (settings reasoning effort)
    (settings thinkingConfig thinkingLevel) (settings thinkingLevel) (settings thinking effort)
    (settings modelParams reasoning_effort)
    (settings modelParams thinkingConfig thinkingLevel)
    (settings modelParams additionalModelRequestFields output_config effort))
  "Where OpenCode requests carry their effort, as (SOURCE . PATH).
SOURCE is `body' or `settings'.  The body overlay comes first, as it
takes the request field by field.  These are the keys the variants of
OpenCode's packages/core/src/variant.ts set.")

(defun opencode--json-object-p (value)
  "Return non-nil when VALUE is a decoded JSON object."
  (and (consp value) (consp (car value)) (symbolp (caar value))))

(defun opencode--merge-settings (base over)
  "Return settings OVER merged onto BASE, objects merging key by key."
  (let ((merged (copy-alist base)))
    (dolist (entry over merged)
      (let ((old (alist-get (car entry) merged)))
        (setf (alist-get (car entry) merged)
              (if (and (opencode--json-object-p old) (opencode--json-object-p (cdr entry)))
                  (opencode--merge-settings old (cdr entry))
                (cdr entry)))))))

(defun opencode-request-effort (settings body)
  "Return the effort an OpenCode request with SETTINGS and BODY names, or nil.
SETTINGS and BODY are a request's settings and body overlay, such as a
variant's.  See `opencode--effort-paths'."
  (seq-some (lambda (path)
              (let ((value (map-nested-elt (if (eq (car path) 'body) body settings) (cdr path))))
                (and (stringp value) value)))
            opencode--effort-paths))

(defun opencode--anthropic-model-key (api-id)
  "Return \"FAMILY-VERSION\" for the Claude model API-ID, or nil.
Accepts family-first (claude-opus-5-5, claude-opus-5.5-fast,
anthropic.claude-opus-5-5-v1:0) and version-first (claude-3-7-sonnet)
IDs.  Minors are limited to two digits so a date suffix is not read as
one."
  (let ((id (replace-regexp-in-string "\\." "-" (downcase api-id))))
    (cond
     ((string-match "claude-\\([a-z]+\\)-\\([0-9]+\\(?:-[0-9]\\{1,2\\}\\)?\\|preview\\)\\(?:[^0-9]\\|\\'\\)" id)
      (concat (match-string 1 id) "-" (match-string 2 id)))
     ((string-match "claude-\\([0-9]+\\(?:-[0-9]\\{1,2\\}\\)?\\)-\\([a-z]+\\)" id)
      (concat (match-string 2 id) "-" (match-string 1 id))))))

(defun opencode--anthropic-default-effort (api-id options &optional api-default-p)
  "Return the effort a Claude API-ID request with OPTIONS runs at, or nil.
Nil unless that request runs adaptive thinking, the one mode every
Anthropic variant pairs with its effort.  OPTIONS hold the request's
thinking and effort as `opencode--anthropic-options' returns them.  With
API-DEFAULT-P, what they leave unset comes from the API defaults in
`opencode-anthropic-default-efforts'."
  (let* ((default (and api-default-p
                       (cdr (assoc (opencode--anthropic-model-key api-id)
                                   opencode-anthropic-default-efforts))))
         (thinking (map-nested-elt options '(thinking type)))
         (effort (or (alist-get 'effort options) default)))
    (when (if thinking (equal thinking "adaptive") default)
      effort)))

(defun opencode--anthropic-binds-thinking-p (api-id &optional compatibility)
  "Return non-nil when OpenCode binds the thinking blocks of Claude API-ID.
It does from Claude 5.1 on unless the model's catalog COMPATIBILITY
says otherwise, and binding turns adaptive thinking on for a request
naming no thinking type.  Mirrors `supportsThinkingBlockBinding' in
OpenCode's packages/ai/src/protocols/utils/claude-model.ts."
  (if-let* ((override (assq 'supportsThinkingBlockBinding compatibility)))
      (cdr override)
    (let ((id (downcase api-id)))
      (when (string-match "\\(?:\\`\\|[./]\\)claude-[a-z]+-\\([0-9]+\\)\\(?:[.-]\\([0-9]\\{1,2\\}\\)\\)?\\(?:\\'\\|[-:@]\\)" id)
        (let ((major (string-to-number (match-string 1 id)))
              (minor (string-to-number (or (match-string 2 id) "0"))))
          (or (> major 5) (and (= major 5) (>= minor 1))))))))

(defun opencode--anthropic-options (settings body &optional binds bedrock)
  "Return the thinking and effort an OpenCode request to Claude sends.
The alist holds `thinking', with its `type', and `effort'.  SETTINGS and
BODY are the request's settings and body overlay, which takes the
request field by field.  BINDS stands for OpenCode binding thinking
blocks, which turns adaptive thinking on when nothing names a thinking
type.  BEDROCK reads them as Bedrock Converse takes them: only a
thinking budget from SETTINGS, the rest from the body's
`additionalModelRequestFields'."
  (let ((fields (if bedrock (alist-get 'additionalModelRequestFields body) body))
        (settings (if bedrock
                      (and (assq 'thinking settings) (list (assq 'thinking settings)))
                    settings)))
    `((thinking (type . ,(or (map-nested-elt fields '(thinking type))
                             (map-nested-elt settings '(thinking type))
                             (and binds "adaptive"))))
      (effort . ,(or (map-nested-elt fields '(output_config effort))
                     (alist-get 'effort settings)
                     (map-nested-elt settings '(output_config effort))
                     (map-nested-elt settings '(outputConfig effort)))))))

(defun opencode-model-default-variant (provider model)
  "Return the variant MODEL runs under OpenCode when none is selected, or nil.
PROVIDER and MODEL are entries of the service's provider and model
catalogs; the variant is returned as a symbol.  A request without a
variant sends PROVIDER's settings with MODEL's merged over them, and
MODEL's body overlay; the OpenAI packages add a medium effort for GPT-5
API IDs other than its chat and pro models.  A Claude model on an
Anthropic package runs adaptive thinking at the effort its settings or
`opencode-anthropic-default-efforts' give, except behind Copilot, which
fronts Claude with its own effort handling.  The variant returned sends
the same effort, with adaptive thinking for Claude; nil leaves the
default to the provider."
  (let* ((package (replace-regexp-in-string
                   "\\`@opencode-ai/ai/" "@opencode/ai/"
                   (or (alist-get 'package model) (alist-get 'package provider) "")))
         (api-id (or (alist-get 'modelID model) (alist-get 'id model) ""))
         (provider-id (or (alist-get 'providerID model) (alist-get 'id provider)))
         (settings (opencode--merge-settings (alist-get 'settings provider)
                                             (alist-get 'settings model)))
         (body (alist-get 'body model))
         (claude (and (member package opencode--anthropic-packages)
                      (opencode--anthropic-model-key api-id)))
         (bedrock (equal package "@opencode/ai/providers/amazon-bedrock"))
         (effort (if claude
                     (opencode--anthropic-default-effort
                      api-id
                      (opencode--anthropic-options
                       settings body
                       (opencode--anthropic-binds-thinking-p api-id (alist-get 'compatibility model))
                       bedrock)
                      (not (equal provider-id "github-copilot")))
                   ;; Converse takes no effort setting, only body fields.
                   (or (opencode-request-effort (unless bedrock settings) body)
                       (and (member package opencode--openai-packages)
                            (let ((id (downcase api-id)))
                              (and (string-search "gpt-5" id)
                                   (not (string-match-p "gpt-5-\\(?:chat\\|pro\\)" id))))
                            "medium"))))
         (variant (and effort
                       (seq-find
                        (lambda (variant)
                          (let ((settings (alist-get 'settings variant))
                                (body (alist-get 'body variant)))
                            (if claude
                                (let ((options (opencode--anthropic-options settings body nil bedrock)))
                                  (and (equal (map-nested-elt options '(thinking type)) "adaptive")
                                       (equal (alist-get 'effort options) effort)))
                              (equal (opencode-request-effort settings body) effort))))
                        (alist-get 'variants model)))))
    (and variant (intern (alist-get 'id variant)))))

(provide 'opencode-variant)
;;; opencode-variant.el ends here
