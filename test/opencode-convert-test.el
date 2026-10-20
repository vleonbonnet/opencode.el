;;; opencode-convert-test.el --- Tests of Markdown transcripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-transcript (scenario)
  "Return the Markdown transcript of recorded SCENARIO."
  (opencode-conversation-markdown
   (alist-get 'data (opencode-test-fixture
                     (format "messages-%s.json" (opencode-test-scenario scenario))))))

(ert-deftest opencode-test-transcript-tools ()
  "Prompts, tool calls with their input and output, and answers are listed."
  (let ((text (opencode-test-transcript "edit-tools")))
    (should (string-prefix-p "# OpenCode Conversation\n\n" text))
    (should (string-match-p "^## User\n\nCreate hello.txt" text))
    (should (string-match-p "^### Tool call: patch$" text))
    (should (string-match-p "\"patchText\": \"\\*\\*\\* Begin Patch" text))
    (should (string-match-p "^Success. Updated the following files:$" text))
    (should (string-match-p "^_Switched to the build agent._$" text))
    ;; Messages with nothing to show leave no empty headings.
    (should-not (string-match-p "^## Assistant\n\n## " text))))

(ert-deftest opencode-test-fenced-line-ends ()
  "Verbatim text reads as a terminal shows it: CRLF ends lines, a lone CR rewrites its line."
  (should (equal (opencode-fenced-block "alpha\r\nbeta\r\n" "text") "```text\nalpha\nbeta\n```"))
  (should (equal (opencode-terminal-text "10%\r50%\r100%\ndone") "100%\ndone"))
  (should (equal (opencode-terminal-text "abcdef\rXY") "XYcdef"))
  (should (equal (opencode-terminal-text "end\r") "end"))
  (should (equal (opencode-terminal-text "plain\n") "plain\n")))

(ert-deftest opencode-test-transcript-other-messages ()
  "Shell commands, compaction summaries and system context are included."
  (let ((shell (opencode-test-transcript "shell-input"))
        (history (opencode-test-transcript "history"))
        (switch (opencode-test-transcript "switch")))
    (should (string-match-p "^## Shell\n\n```sh\necho from-user-shell\n```\n\n```text\nfrom-user-shell\n```" shell))
    (should (string-match-p "^## Compaction summary\n\n## Objective" history))
    (should (string-match-p "^## System context: Instructions updated" switch))))

(ert-deftest opencode-test-copy-conversation ()
  "The transcript of the current session lands in the kill ring."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
      (let ((kill-ring nil))
        (opencode-copy-conversation)
        (should (string-match-p "hello fixture" (car kill-ring)))))))

(provide 'opencode-convert-test)
;;; opencode-convert-test.el ends here
