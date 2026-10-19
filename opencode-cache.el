;;; opencode-cache.el --- Prompt-cache guard and stale thinking  -*- lexical-binding: t; -*-

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

;; Re-sending a long conversation without the provider's prompt cache
;; costs far more than reading it: a cache read is billed at a tenth of
;; the input rate, a write at more than the full rate.  Whether the next
;; turn can reuse the cache depends on what Emacs cannot see (the agent's
;; tool list, the system prompt, the gateway, how long the session has
;; been idle), so the server decides: my OpenCode fork's preflight route
;; dry-runs the request the turn would send and compares it with what the
;; provider confirmed it cached.  Before an idle session sends a prompt or
;; command, Emacs asks for that report and asks for confirmation when more
;; than `opencode-cache-guard-threshold' previously cached tokens would be
;; re-sent uncached, showing why.  The check fails safe: when the server
;; cannot predict reuse, or the check fails, it asks whenever the context
;; exceeds the threshold.  Declining leaves the input in the prompt.
;; Shell commands start no turn, and input sent while a turn runs joins
;; its cached conversation, so neither is checked.
;;
;; Thinking signatures are bound to everything before them.  When the
;; system prompt, the tool list or earlier messages changed, or the
;; provider already rejected a block, earlier thinking is stale.  Other
;; clients let the provider drop it silently; Emacs marks each prompt
;; and command `thinkingLoss: "ask"', so the server refuses such a turn
;; instead, and when the report counts stale thinking Emacs asks whether
;; to send with `thinkingLoss: "accept"', replacing those blocks with
;; their text, tagged as earlier reasoning.  Blocks without a binding,
;; from before the server's ledger, are sent as they are.  Blocks
;; replaced or dropped are relabelled in the transcript, with one notice
;; per batch.
;;
;; A turn refused for stale thinking anyway, sent by another client or
;; rejected by the provider for blocks nobody could vouch for, is undone
;; when nothing had run yet: its message is reverted and its text put
;; back in the prompt, so sending it again asks the question.

;;; Code:

(require 'map)
(require 'opencode-blocks)
(require 'opencode-common)
(require 'opencode-events)
(require 'opencode-http)
(require 'opencode-render)
(require 'opencode-session)

(defcustom opencode-cache-guard t
  "When non-nil, check the prompt cache before an idle session sends a turn."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-cache-guard-threshold 100000
  "Ask before a turn re-sends more than this many cached tokens uncached.
nil never asks about the cache; stale thinking is still asked about."
  :type '(choice (const :tag "Never ask" nil) natnum)
  :group 'opencode)

(defcustom opencode-cache-guard-timeout 20
  "Seconds to wait for the server's prompt-cache preflight."
  :type 'number
  :group 'opencode)

;;;; Preflight

(defun opencode-cache-preflight-body (request)
  "Return the preflight body for input REQUEST, or nil when it starts no turn.
REQUEST is the plist of `opencode-input-request', where :agent and
:model, a model reference, name the agent and model the turn will run,
when they differ from the session's: a caller switching before sending
checks the turn the switch leads to."
  (when-let* ((body (pcase (plist-get request :kind)
                      ('prompt (let ((files (opencode--prompt-files)))
                                 `((text . ,(plist-get request :text))
                                   ,@(when (> (length files) 0) `((files . ,files))))))
                      ('command `((command (name . ,(plist-get request :name))
                                           (arguments . ,(plist-get request :arguments))))))))
    `(,@body
      ,@(when-let* ((agent (plist-get request :agent))) `((agent . ,agent)))
      ,@(when-let* ((model (plist-get request :model))) `((model . ,model))))))

(defvar opencode-cache--unsupported nil
  "IDs of services without the preflight route, which only my fork has.")

(defun opencode-cache-preflight (body)
  "Return the server's prompt-cache report for sending BODY now.
A failed check is reported as an unknown reuse, giving its reason.  A
service without the route, which answers an untagged 404, is reported
as `unsupported' and not asked again."
  (opencode-request-data
   (opencode-request-sync
    'post (format "/api/experimental/session/%s/preflight" opencode-session-id)
    :body body :timeout opencode-cache-guard-timeout
    :else (lambda (error)
            (if (and (eql (opencode-error-status error) 404) (null (opencode-error-tag error)))
                (progn
                  (push (plist-get opencode-service-connection :id) opencode-cache--unsupported)
                  (message "OpenCode: this service cannot preflight the prompt cache; the cache guard is off for it")
                  '((data (status . "unsupported"))))
              `((data (status . "unknown")
                      (lostTokens . 0)
                      (reasons ,(concat "cache check failed: "
                                        (or (opencode-error-message error) "unknown error")
                                        (if-let* ((status (opencode-error-status error)))
                                            (format " (HTTP %s)" status)
                                          ""))))))))))

(defun opencode-cache-loss (report)
  "Return the previously cached tokens REPORT would re-send, or nil.
A turn the server cannot predict may lose the whole context."
  (let-alist report
    (pcase .status
      ((or "hit" "unsupported") nil)
      ("unknown" (max (or .lostTokens 0)
                      (if (numberp opencode-session-tokens) opencode-session-tokens 0)))
      (_ .lostTokens))))

(defun opencode-cache--tokens (n)
  "Format token count N compactly."
  (if (< n 1000) (format "%d" n) (format "%dk" (round n 1000))))

(defun opencode-cache--line (text)
  "Return TEXT as an indented line of a question, cut to the frame width."
  (concat "  - " (truncate-string-to-width text (max 40 (- (frame-width) 6)) nil nil "…") "\n"))

(defun opencode-cache-question (report loss)
  "Return the question for REPORT's possible LOSS of cached tokens."
  (let-alist report
    (concat
     (if (equal .status "unknown")
         (format "OpenCode: cache reuse is unknown; up to ~%s context tokens could be re-sent uncached.\n"
                 (opencode-cache--tokens loss))
       (format "OpenCode: ~%s cached tokens would be re-sent uncached (%s%s).\n"
               (opencode-cache--tokens loss) .status
               (if (and .reusableTokens (> .reusableTokens 0))
                   (format ", %s still cached" (opencode-cache--tokens .reusableTokens))
                 "")))
     (mapconcat #'opencode-cache--line (seq-take .reasons 4) "")
     (when .divergence.unit
       (opencode-cache--line (format "first difference: %s" .divergence.unit)))
     "Send anyway? ")))

(defconst opencode-cache-unknown-binding "unknown binding"
  "The stale-thinking reason for blocks the server holds no binding for.
They predate its ledger, so they are sent and may well still match;
only a refusal by the provider fails the turn.")

(defun opencode-cache-consent-loss (report loss)
  "Return the cached tokens sending with consent would re-send, per REPORT.
The server predicts the turn that replays the stale blocks as text when
it can; else LOSS, the turn as it stands, is the closest estimate."
  (or (map-nested-elt report '(staleThinking consent lostTokens)) loss))

(defun opencode-cache-thinking-question (report loss)
  "Return the question for REPORT's stale thinking, mentioning a LOSS of cache."
  (let-alist report
    (let ((count .staleThinking.count)
          (loss (opencode-cache-consent-loss report loss)))
      (concat
       (format "OpenCode: %d thinking block%s from earlier turns no longer match%s the conversation:\n"
               count (if (= count 1) "" "s") (if (= count 1) "es" ""))
       (opencode-cache--line (or .staleThinking.reason "unknown reason"))
       (when (and loss opencode-cache-guard-threshold (> loss opencode-cache-guard-threshold))
         (format (if (and (equal .status "unknown") (not .staleThinking.consent))
                     "The turn could also re-send up to ~%s context tokens uncached.\n"
                   "The turn also re-sends ~%s cached tokens uncached.\n")
                 (opencode-cache--tokens loss)))
       "Replace them with their text and send? "))))

(defun opencode-cache-stale-thinking-p (report)
  "Return non-nil when REPORT counts thinking the turn would be refused for."
  (and (> (or (map-nested-elt report '(staleThinking count)) 0) 0)
       (not (equal (map-nested-elt report '(staleThinking reason)) opencode-cache-unknown-binding))))

(defun opencode-cache-check (request)
  "Check the prompt cache before input REQUEST starts a turn.
Return nil to send, `cancel' to keep the input, or the consent metadata."
  (when-let* (((and opencode-cache-guard (eq opencode-session-status 'idle)))
              ((not (member (plist-get opencode-service-connection :id) opencode-cache--unsupported)))
              (body (opencode-cache-preflight-body request)))
    (let* ((report (opencode-cache-preflight body))
           (loss (opencode-cache-loss report)))
      (cond
       ((opencode-cache-stale-thinking-p report)
        (if (y-or-n-p (opencode-cache-thinking-question report loss))
            '((thinkingLoss . "accept"))
          'cancel))
       ((or (null loss) (null opencode-cache-guard-threshold)
            (<= loss opencode-cache-guard-threshold))
        nil)
       ((y-or-n-p (opencode-cache-question report loss)) nil)
       (t 'cancel)))))

(add-hook 'opencode-send-checks #'opencode-cache-check)

;;;; Dropped thinking

(defun opencode-cache-dropped-notice (drops)
  "Return the transcript notice for DROPS, the dropped thinking markers."
  (let* ((reason (lambda (drop) (or (alist-get 'reason drop) "")))
         (as-text (seq-count #'opencode-dropped-as-text-p drops))
         (rest (seq-remove #'opencode-dropped-as-text-p drops))
         (consent (seq-count (lambda (drop) (string-prefix-p "dropped with consent" (funcall reason drop))) rest))
         (provider (seq-count (lambda (drop) (string-prefix-p "dropped by the provider" (funcall reason drop))) rest))
         (other (- (length rest) consent provider)))
    (format "Dropped %d thinking block%s from earlier turns that no longer match the conversation (%s)"
            (length drops) (if (= (length drops) 1) "" "s")
            (string-join
             (delq nil (list (and (> as-text 0) (format "%d with your consent, replayed as text" as-text))
                             (and (> consent 0) (format "%d with your consent" consent))
                             (and (> provider 0) (format "%d by the provider" provider))
                             (and (> other 0) (format "%d: %s" other (funcall reason (car (last rest)))))))
             "; "))))

(defun opencode--cache-mark-dropped (drops)
  "Relabel the reasoning blocks of DROPS, dropped thinking markers."
  (dolist (drop drops)
    (let-alist drop
      (when (and .messageID (numberp .ordinal))
        (opencode-reasoning-mark-dropped (opencode-reasoning-key .messageID .ordinal) drop)))))

(defun opencode--cache-thinking-dropped (event)
  "Relabel and report the blocks `session.thinking.dropped' EVENT lists."
  (let ((data (alist-get 'data event)))
    (opencode-with-session-buffer (alist-get 'sessionID data)
      (when-let* ((drops (alist-get 'blocks data)))
        (opencode--cache-mark-dropped drops)
        (opencode-output-notice (opencode-cache-dropped-notice drops))))))

(opencode-events-register "session.thinking.dropped" #'opencode--cache-thinking-dropped)

(defun opencode--cache-load-dropped ()
  "Relabel the reasoning blocks the server dropped earlier, after a replay."
  (when opencode-session-id
    (opencode-request
     'get (format "/api/experimental/session/%s/thinking" opencode-session-id)
     :then (lambda (response)
             (opencode--cache-mark-dropped (alist-get 'dropped (opencode-request-data response))))
     ;; A service without the route has nothing to relabel.
     :else #'ignore)))

(add-hook 'opencode-session-replayed-hook #'opencode--cache-load-dropped)

;;;; Refused turns

(defun opencode-cache-thinking-refusal-p (error)
  "Return non-nil when turn ERROR refused thinking that no longer matches."
  (let ((message (alist-get 'message error)))
    (or (equal (alist-get 'type error) "thinking.stale")
        (and (stringp message)
             (string-match-p "Invalid `signature` in `\\(?:redacted_\\)?thinking` block" message)))))

(defun opencode-cache-refused-turn (messages)
  "Return the user message whose turn was refused before anything ran, or nil.
MESSAGES are the session's latest messages, newest first.  A message
with attachments is not returned: its text alone would not resend it."
  (let ((newer nil)
        (user nil))
    (while (and messages (not user))
      (let ((message (pop messages)))
        (if (equal (alist-get 'type message) "user")
            (setq user message)
          (push message newer))))
    (when (and user
               (not (alist-get 'files user)) (not (alist-get 'agents user))
               (not (seq-some (lambda (message)
                                (and (equal (alist-get 'type message) "assistant")
                                     (alist-get 'content message)))
                              newer)))
      user)))

(defun opencode--cache-restore (text)
  "Put TEXT in this buffer's prompt when it is empty."
  (when (and (get-buffer-process (current-buffer))
             (string-blank-p (buffer-substring-no-properties (opencode-pmark) (point-max))))
    (save-excursion
      (goto-char (point-max))
      (insert text))))

(defun opencode--cache-execution-failed (event)
  "Prepare resending the turn EVENT reports refused for stale thinking."
  (let ((data (alist-get 'data event)))
    (when (opencode-cache-thinking-refusal-p (alist-get 'error data))
      (opencode-with-session-buffer (alist-get 'sessionID data)
        (opencode-request
         'get (format "/api/session/%s/message" opencode-session-id)
         :query '((order . "desc") (limit . 20))
         :then
         (lambda (response)
           (if-let* ((user (opencode-cache-refused-turn (opencode-request-data response))))
               (opencode-request
                'post (format "/api/session/%s/revert/stage" opencode-session-id)
                :body `((messageID . ,(alist-get 'id user)) (files . :false))
                :then (lambda (_)
                        (opencode--cache-restore (alist-get 'text user))
                        (opencode-output-notice
                         "Earlier thinking no longer matches the conversation.  Your message is back in the prompt: send it to choose whether to drop that thinking."))
                :else (lambda (_)
                        (opencode-output-notice
                         "Earlier thinking no longer matches the conversation.  Resend your message to choose whether to drop that thinking.")))
             (opencode-output-notice
              "Earlier thinking no longer matches the conversation.  Send a message, such as \"continue\", to choose whether to drop it."))))))))

(opencode-events-register "session.execution.failed" #'opencode--cache-execution-failed)

(provide 'opencode-cache)
;;; opencode-cache.el ends here
