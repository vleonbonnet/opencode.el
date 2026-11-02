;;; opencode-display.el --- Compact displays of models and context use  -*- lexical-binding: t; -*-

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

;; What the session mode line shows of a session's agent, model,
;; provider, variant and context use, as functions of an agent, a model
;; reference and a token count rather than of a session buffer, so that
;; other packages' session lists, such as drover's, show the same for
;; every session the registry knows: its entries carry the agent and
;; the model, and `opencode-state-context' the context size.
;;
;; They read the location's cached catalogs and never wait: a value
;; needing a catalog that is not fetched yet is nil, and the fetch
;; starts.  Once it arrives, the mode lines of the location's session
;; buffers are redrawn and `opencode-catalog-changed-functions' run, for
;; other displays to compute theirs again.  A reconnection keeps the
;; catalogs, which displays go on showing while the first asking for
;; one again has it renewed; it counts as a change only if it differs.
;;
;; A model can cost more per token once a request exceeds a context size,
;; as listed in its catalog pricing or in `opencode-long-context-tiers'.
;; The context use turns to the attention face near that size and to the
;; error face past it.
;;
;; Right after a compaction the context size is the service's estimate
;; of the next request, until that request's step measures it: it shows
;; with a "~", and its tooltip says so.

;;; Code:

(require 'opencode-common)
(require 'opencode-session)
(require 'opencode-variant)

(defcustom opencode-provider-icons
  '(("\\`github-copilot\\'" . "\uf4b8")  ; nf-oct-copilot
    ("\\`anthropic\\'" . "\uec82")       ; nf-cod-claude
    ("\\`openai\\'" . "\uec81"))         ; nf-cod-openai
  "Alist of (PROVIDER-ID-REGEXP . ICON) for the provider of a session.
Providers without a match show a generic icon followed by their name."
  :type '(alist :key-type regexp :value-type string)
  :group 'opencode)

(defcustom opencode-default-variant-function #'opencode-model-default-variant
  "Function naming the variant a model runs with when none is selected.
Called with the provider and model catalog entries; returns a variant ID,
a string or symbol, or nil when the provider decides.  The default,
`opencode-model-default-variant', names the variant sending the effort
such a request sends.  A function returning nil, or nil itself, shows a
dim \"default\"."
  :type '(choice (const :tag "Provider default" nil) function)
  :group 'opencode)

(defcustom opencode-long-context-tiers nil
  "Context sizes past which models cost more, beyond their catalog pricing.
Each entry is (PROVIDER-REGEXP MODEL-REGEXP SIZE LABEL): a model whose
provider and model IDs match costs what LABEL says, such as \"2×\",
once a request exceeds SIZE tokens."
  :type '(repeat (list (regexp :tag "Provider ID")
                       (regexp :tag "Model ID")
                       (natnum :tag "Tokens")
                       (string :tag "Label")))
  :group 'opencode)

(defcustom opencode-long-context-near 0.9
  "Fraction of a long-context tier's size from which context use warns."
  :type 'number
  :group 'opencode)

(defcustom opencode-mode-line-agent-faces
  '(font-lock-keyword-face font-lock-type-face font-lock-constant-face font-lock-string-face)
  "Faces cycled over primary agents that configure no color.
The session mode line and `opencode-agent-display' draw an agent in
one of them, chosen by its place among the primary agents."
  :type '(repeat face)
  :group 'opencode)

;;;; Long-context pricing

(defun opencode-long-context-threshold (&optional model info)
  "Return (SIZE . LABEL) of the higher price tier of MODEL, or nil.
MODEL is a model reference, by default the session's; INFO its catalog
entry, by default the cached one."
  (let* ((model (or model opencode-session-model))
         (info (or info (opencode-model-info model))))
    (or (when-let* ((entry (seq-find (lambda (entry)
                                       (and (string-match-p (nth 0 entry) (or (alist-get 'providerID model) ""))
                                            (string-match-p (nth 1 entry) (or (alist-get 'id model) ""))))
                                     opencode-long-context-tiers)))
          (cons (nth 2 entry) (nth 3 entry)))
        (let* ((costs (alist-get 'cost info))
               (base (seq-find (lambda (cost) (null (alist-get 'tier cost))) costs))
               (tier (car (sort (seq-filter (lambda (cost) (alist-get 'tier cost)) costs)
                                :key (lambda (cost) (alist-get 'size (alist-get 'tier cost)))))))
          (when tier
            (cons (alist-get 'size (alist-get 'tier tier))
                  (let ((from (alist-get 'input base))
                        (to (alist-get 'input tier)))
                    (if (and (numberp from) (numberp to) (> from 0))
                        (format "%g×" (/ (float to) from))
                      "a higher rate"))))))))

(defun opencode-long-context-tier (&optional model info tokens)
  "Return (STATE SIZE LABEL) when a context of TOKENS nears a price tier.
STATE is `near' or `over'; MODEL and INFO are as for
`opencode-long-context-threshold'.  TOKENS defaults to the session's
context size, `opencode-session-tokens'."
  (let ((tokens (or tokens opencode-session-tokens)))
    (when-let* (((numberp tokens))
                (threshold (opencode-long-context-threshold model info)))
      (cond ((> tokens (car threshold))
             (list 'over (car threshold) (cdr threshold)))
            ((>= tokens (* opencode-long-context-near (car threshold)))
             (list 'near (car threshold) (cdr threshold)))))))

(defun opencode-long-context-description (tier &optional tokens estimated)
  "Return the tooltip line describing TIER, from `opencode-long-context-tier'.
TOKENS is the context size TIER was found for, by default the session's.
ESTIMATED non-nil means TOKENS is an estimate, as after a compaction."
  (pcase-let ((`(,state ,size ,label) tier))
    (if (eq state 'over)
        (format (if estimated
                    "Context estimated over %dk: requests may cost %s"
                  "Context over %dk: requests cost %s")
                (/ size 1000) label)
      (format "Context %s%dk/%dk: near the %s tier"
              (if estimated "~" "") (/ (or tokens opencode-session-tokens) 1000) (/ size 1000) label))))

;;;; Catalog values

(defun opencode--catalogs-cached-p (directory &rest kinds)
  "Return non-nil when DIRECTORY's catalogs of KINDS are all cached.
Those that are not are fetched in the background.  A location the
service said does not exist counts as cached, with empty catalogs, so
that displays show identifiers rather than nothing."
  (let ((cached t))
    (dolist (kind kinds)
      ;; Every missing catalog starts at once, not one per redisplay.
      (opencode-catalog-ensure kind directory)
      (unless (opencode-catalog-cached-p kind directory)
        (setq cached nil)))
    (or cached (opencode-location-missing-p directory))))

(defun opencode--model-short-name (name)
  "Return model NAME without the family prefix every model of it shares."
  (replace-regexp-in-string "\\`Claude " "" name))

(defun opencode-model-name (model &optional directory)
  "Return the catalog name of MODEL, a model reference, in DIRECTORY.
It is the model's ID when the catalog lacks the model.  Nil when MODEL
is nil or DIRECTORY's model catalog is not fetched yet; see
`opencode-catalog-changed-functions'."
  (when (and model (opencode--catalogs-cached-p directory 'model))
    (or (alist-get 'name (opencode-model-info model directory))
        (alist-get 'id model))))

(defun opencode-model-display (model &optional directory)
  "Return the compact name of MODEL, a model reference, in DIRECTORY.
That is its catalog name without the family prefix: \"Opus 5.5\" for
\"Claude Opus 5.5\".  Nil when `opencode-model-name' is."
  (when-let* ((name (opencode-model-name model directory)))
    (opencode--model-short-name name)))

(defun opencode--provider-string (id name)
  "Return the icon of provider ID, or a generic icon and NAME.
NAME, the provider's display name, is in its tooltip either way."
  (propertize (or (cdr (seq-find (lambda (entry) (string-match-p (car entry) id))
                                 opencode-provider-icons))
                  (concat "\U000F048D " name)) ; nf-md-server_network
              'help-echo (format "Provider: %s (%s)" name id)))

(defun opencode-provider-display (provider-id &optional directory)
  "Return the icon of the provider PROVIDER-ID in DIRECTORY.
The icon comes from `opencode-provider-icons'; a provider without one
shows a generic icon and its name.  The tooltip names the provider.
Nil when PROVIDER-ID is nil or DIRECTORY's provider catalog, which has
the name, is not fetched yet; see `opencode-catalog-changed-functions'."
  (when (and provider-id (opencode--catalogs-cached-p directory 'provider))
    (opencode--provider-string
     provider-id
     (or (alist-get 'name (opencode-provider-info provider-id directory)) provider-id))))

(defun opencode--agent-string (id agents)
  "Return agent ID styled as AGENTS, its location's agent catalog, has it.
It is the catalog's name for ID, bold, in its configured `color' when
Emacs knows that color, else in the face of
`opencode-mode-line-agent-faces' its place among the primary agents
that are not hidden picks, so that each agent keeps its face; an agent
AGENTS lacks, or every agent while AGENTS is nil, goes by ID in the
first face.  The tooltip names it."
  (let* ((info (seq-find (lambda (agent) (equal (alist-get 'id agent) id)) agents))
         (name (or (alist-get 'name info) id))
         (color (alist-get 'color info))
         (primary (seq-keep (lambda (agent)
                              (and (equal (alist-get 'mode agent) "primary")
                                   (not (alist-get 'hidden agent))
                                   (alist-get 'id agent)))
                            agents))
         (index (or (seq-position primary id) 0)))
    (propertize name
                'face (list 'bold
                            (cond ((and (stringp color) (color-defined-p color))
                                   (list :foreground color))
                                  (opencode-mode-line-agent-faces
                                   (nth (mod index (length opencode-mode-line-agent-faces))
                                        opencode-mode-line-agent-faces))))
                'help-echo (format "Agent: %s" name))))

(defun opencode-agent-display (id &optional directory)
  "Return the agent ID as the session mode line shows it, in DIRECTORY.
That is the name DIRECTORY's agent catalog gives it, bold, in the
color the catalog configures for it, else in a face of
`opencode-mode-line-agent-faces' chosen by its place among the primary
agents, the same for every session; the tooltip names it.  Nil when ID
is nil or DIRECTORY's agent catalog is not fetched yet; see
`opencode-catalog-changed-functions'."
  (when (and id (opencode--catalogs-cached-p directory 'agent))
    (opencode--agent-string id (opencode-catalog 'agent directory))))

(defun opencode--variant-string (variant provider info)
  "Return selected VARIANT for display, or the default of the model INFO.
PROVIDER and INFO are the provider and model catalog entries; the
default is computed only when VARIANT is nil or \"default\".  Nil when
neither applies, as for a model without variants."
  (cond
   ((and variant (not (equal variant "default")))
    (propertize variant 'face 'success 'help-echo (format "Variant: %s (selected)" variant)))
   ((alist-get 'variants info)
    (let ((default (when-let* ((variant (and opencode-default-variant-function
                                             (funcall opencode-default-variant-function provider info))))
                     (format "%s" variant))))
      (propertize (or default "default")
                  'face 'shadow
                  'help-echo (if default
                                 (format "Variant: %s (default)" default)
                               "Variant: provider default"))))))

(defun opencode-variant-display (model &optional directory)
  "Return the variant MODEL runs, a model reference, in DIRECTORY.
A selected variant shows in the `success' face.  Without one, the
variant the model runs by default, as `opencode-default-variant-function'
names it, shows in the `shadow' face, or \"default\" when the provider
decides.  The tooltip tells which.  Nil when the model has no variants,
or when no variant is selected and DIRECTORY's model and provider
catalogs, which the default comes from, are not fetched yet; see
`opencode-catalog-changed-functions'."
  (let ((variant (alist-get 'variant model)))
    (if (and variant (not (equal variant "default")))
        (opencode--variant-string variant nil nil)
      (when (and model (opencode--catalogs-cached-p directory 'model 'provider))
        (opencode--variant-string nil
                                  (opencode-provider-info (alist-get 'providerID model) directory)
                                  (opencode-model-info model directory))))))

(defun opencode-model-label (model &optional directory)
  "Return the display name of MODEL, a model reference, with its variant.
The variant is the one selected, else the one the model runs by default,
as `opencode-variant-display' shows it, read in DIRECTORY's catalogs."
  (let ((variant (opencode-variant-display model directory)))
    (concat (or (opencode-model-name model directory) (alist-get 'id model) "?")
            (and variant (concat " " variant)))))

(defun opencode-context-usage (tokens model &optional directory)
  "Return how much of the context of MODEL a context of TOKENS fills.
MODEL is a model reference, read in DIRECTORY's catalogs.  The value is
a plist: :tokens, TOKENS; :limit, the model's context size in tokens;
:fraction, TOKENS over :limit, a float beyond 1 past it; and :tier, the
long-context price tier TOKENS nears or passes, as
`opencode-long-context-tier' returns it, or nil.  Nil when TOKENS is not
a number, the catalog has no context size for the model, or
DIRECTORY's model catalog is not fetched yet; see
`opencode-catalog-changed-functions'.  Session lists drawing their own
gauge start from this; `opencode-context-display' draws the mode line's."
  (when (and (numberp tokens) model (opencode--catalogs-cached-p directory 'model))
    (let* ((info (opencode-model-info model directory))
           (limit (alist-get 'context (alist-get 'limit info))))
      (when (and (numberp limit) (> limit 0))
        (list :tokens tokens :limit limit :fraction (/ (float tokens) limit)
              :tier (opencode-long-context-tier model info tokens))))))

(defun opencode-context-display (tokens model &optional directory estimated)
  "Return a context of TOKENS shown against the context size of MODEL.
MODEL is a model reference, read in DIRECTORY's catalogs.  A pie filling
by eighths precedes the percentage used, followed past a long-context
price tier by its label, such as \"2×\".  The whole is in the
`opencode-attention' face near the tier and in the `error' face past it,
and its tooltip has the counts and the tier.  ESTIMATED non-nil means
TOKENS is the service's estimate after a compaction, which a \"~\"
before the percentage and the tooltip tell, as
`opencode-state-context-estimated-p' and `opencode-session-tokens-estimated'
report it.  Nil when `opencode-context-usage' is."
  (when-let* ((usage (opencode-context-usage tokens model directory)))
    (let* ((limit (plist-get usage :limit))
           (tier (plist-get usage :tier))
           ;; Computed as the mode line always did, so rounding at the
           ;; slice and percentage boundaries is unchanged.
           (used (/ (* 100.0 tokens) limit))
           (slice (min 7 (max 0 (1- (ceiling used 12.5))))))
      (propertize (concat (string (+ #xF0A9E slice)) ; nf-md-circle_slice_1..8
                          (format " %s%.0f%%" (if estimated "~" "") used)
                          (when (eq (car tier) 'over) (concat " " (nth 2 tier))))
                  'face (pcase (car tier) ('over 'error) ('near 'opencode-attention))
                  'help-echo (concat (format "Context: %s%dk/%dk tokens" (if estimated "~" "")
                                             (/ tokens 1000) (/ limit 1000))
                                     (when estimated
                                       "\nEstimated by the service after compaction; the next step measures it")
                                     (when tier
                                       (concat "\n" (opencode-long-context-description
                                                      tier tokens estimated))))))))

(provide 'opencode-display)
;;; opencode-display.el ends here
