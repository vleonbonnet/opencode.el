;;; opencode-convert.el --- Sessions as Markdown transcripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann
;; Copyright (C) 2026  Valentin Leon

;; Author: Scott Zimmermann <sczi@disroot.org>
;;         Valentin Leon <valentin@leon.click>
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

;; `opencode-copy-conversation' copies a session as a Markdown transcript
;; built from the server's messages, so tool output hidden in the buffer
;; is included.  It lets an agent gather context and start a plan here,
;; then the plan be iterated on elsewhere, such as in a web chat.

;;; Code:

(require 'json)
(require 'opencode-common)
(require 'opencode-session)

(defun opencode--conversation-json (value)
  "Return VALUE encoded as indented JSON."
  (let ((json-encoding-pretty-print t))
    (json-encode value)))

(defun opencode--conversation-tool (item)
  "Return Markdown for tool call content ITEM."
  (let-alist item
    (string-join
     (delq nil
           (list (format "### Tool call: %s" (or .name "unknown"))
                 (when .id (format "Call ID: `%s`" .id))
                 (when .state.status (format "Status: `%s`" .state.status))
                 (when (and .state.input (not (stringp .state.input)))
                   (concat "Input:\n\n"
                           (opencode-fenced-block (opencode--conversation-json .state.input) "json")))
                 (when-let* ((output (opencode-content-text .state.content)))
                   (concat "Output:\n\n" (opencode-fenced-block output "text")))
                 (when .state.error.message
                   (concat "Error:\n\n" (opencode-fenced-block .state.error.message "text")))))
     "\n\n")))

(defun opencode--conversation-content (item)
  "Return Markdown for assistant content ITEM, or nil when it has nothing to show."
  (let-alist item
    (pcase .type
      ("text" (and .text (not (string-empty-p .text)) .text))
      ;; Withheld reasoning arrives encrypted, with no text.
      ("reasoning" (and .text (not (string-empty-p .text))
                        (concat "### Reasoning\n\n" (opencode-fenced-block .text "text"))))
      ("tool" (opencode--conversation-tool item)))))

(defun opencode--conversation-user (message)
  "Return the parts of user MESSAGE."
  (let-alist message
    (append (list .text)
            (mapcar (lambda (file)
                      (let-alist file
                        (string-join (delq nil (list "### Context file"
                                                     (when .name (format "- Name: `%s`" .name))
                                                     (when .mime (format "- MIME: `%s`" .mime))
                                                     (when .uri (format "- URI: `%s`"
                                                                        (if (string-prefix-p "data:" .uri)
                                                                            "data:…"
                                                                          .uri)))))
                                     "\n")))
                    .files)
            (mapcar (lambda (agent) (format "### Context agent\n\n@%s" (alist-get 'name agent)))
                    .agents))))

(defun opencode--conversation-message (message)
  "Return Markdown for MESSAGE, or nil when it has nothing to show."
  (let-alist message
    (let ((parts
           (pcase .type
             ("user" (cons "## User" (opencode--conversation-user message)))
             ("assistant" (cons "## Assistant" (mapcar #'opencode--conversation-content .content)))
             ((or "synthetic" "system")
              (list (format "## %s context%s" (capitalize .type)
                            (if .description (concat ": " .description) ""))
                    (opencode-fenced-block (or .text "") "text")))
             ("skill" (list (format "## Skill: %s" (or .name .skill))
                            (opencode-fenced-block (or .text "") "text")))
             ("shell" (list "## Shell"
                            (opencode-fenced-block (or .command "") "sh")
                            (when-let* ((output (alist-get 'output .output)))
                              (opencode-fenced-block output "text"))))
             ("compaction" (when (equal .status "completed")
                             (list "## Compaction summary" .summary)))
             ("agent-switched" (list (format "_Switched to the %s agent._" .agent)))
             ("model-switched" (list (format "_Switched to %s._" (opencode-model-label .model)))))))
      (when-let* ((parts (delq nil (seq-remove (lambda (part) (and (stringp part) (string-empty-p part)))
                                               parts))))
        (when (or (cdr parts) (not (string-prefix-p "## " (car parts))))
          (string-join parts "\n\n"))))))

(defun opencode-conversation-markdown (messages)
  "Return session MESSAGES as a Markdown transcript."
  (let ((formatted (delq nil (mapcar #'opencode--conversation-message messages))))
    (concat "# OpenCode Conversation\n\n"
            (if formatted (string-join formatted "\n\n") "_No messages._")
            "\n")))

(defun opencode-copy-conversation ()
  "Copy this session as a Markdown transcript, including hidden tool output."
  (interactive)
  (unless opencode-session-id (user-error "Not in an OpenCode session"))
  (message "Fetching the conversation…")
  (opencode--fetch-messages
   opencode-session-id
   (lambda (messages)
     (let ((transcript (opencode-conversation-markdown messages)))
       (kill-new transcript)
       (message "Copied the conversation (%d messages, %d characters)"
                (length messages) (length transcript))))))

(provide 'opencode-convert)
;;; opencode-convert.el ends here
