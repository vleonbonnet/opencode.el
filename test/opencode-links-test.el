;;; opencode-links-test.el --- Tests of links in session transcripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; URLs written out in the agent's text are buttons, as Markdown links
;; are: RET and mouse-1 follow them in the transcript, while RET in the
;; input still sends it.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-links (&optional start end)
  "Return the link buttons from START to END as a list of (TEXT . URL)."
  (let ((end (or end (point-max)))
        (button (or (button-at (or start (point-min))) (next-button (or start (point-min)))))
        links)
    (while (and button (< (button-start button) end))
      (push (cons (buffer-substring-no-properties (button-start button) (button-end button))
                  (button-get button 'url))
            links)
      (setq button (next-button (button-end button))))
    (nreverse links)))

(defun opencode-test-stream (session message &rest deltas)
  "Stream DELTAS as text of assistant MESSAGE in SESSION, without ending it."
  (let ((base `((sessionID . ,session) (assistantMessageID . ,message) (ordinal . 0))))
    (opencode-test-dispatch
     `(((type . "session.step.started") (data (sessionID . ,session) (assistantMessageID . ,message)))
       ((type . "session.text.started") (data ,@base))
       ,@(mapcar (lambda (delta) `((type . "session.text.delta") (data ,@base (delta . ,delta))))
                 deltas)))))

(defun opencode-test-end-stream (session message text)
  "End the text of assistant MESSAGE in SESSION, whose full TEXT it was."
  (opencode-test-dispatch
   `(((type . "session.text.ended")
      (data (sessionID . ,session) (assistantMessageID . ,message) (ordinal . 0) (text . ,text))))))

(defmacro opencode-test-with-answer (bindings &rest body)
  "Run BODY in a session buffer during a turn.
BINDINGS is a list (SESSION) of the variable bound to the session's ID."
  (declare (indent 1) (debug t))
  `(opencode-test-with-server
     (let* ((,(car bindings) (opencode-test-scenario "text")))
       (with-current-buffer (opencode-test-open-empty ,(car bindings))
         (opencode-test-turn ,(car bindings) "started")
         ,@body))))

(ert-deftest opencode-test-bare-url-is-a-link ()
  "A URL in the agent's text is a button that RET and mouse-1 follow."
  (opencode-test-with-answer (session)
    (opencode-test-answer session "msg_link" "See https://example.com/docs for more.")
    (let ((position (opencode-test-find "https://example.com/docs"))
          browsed)
      (should (equal (opencode-test-links) '(("https://example.com/docs" . "https://example.com/docs"))))
      (goto-char (+ position 3))
      (should (eq (key-binding (kbd "RET")) 'push-button))
      (should (mouse-on-link-p (+ position 3)))
      (should (equal (get-text-property position 'help-echo) "Open https://example.com/docs"))
      (should (memq 'link (ensure-list (get-text-property position 'face))))
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url))))
        (push-button (+ position 3))
        (should (equal browsed "https://example.com/docs"))
        (setq browsed nil)
        (opencode-open-link-at-point)
        (should (equal browsed "https://example.com/docs"))))))

(ert-deftest opencode-test-bare-url-boundaries ()
  "Punctuation around a URL stays out of it, brackets it opens stay in it."
  (pcase-dolist (`(,text ,url)
                 '(("Read https://example.com/a." "https://example.com/a")
                   ("Read https://example.com/a, then" "https://example.com/a")
                   ("Is it https://example.com/a? Yes" "https://example.com/a")
                   ("(see https://example.com/a)" "https://example.com/a")
                   ("(see https://en.wikipedia.org/wiki/Lisp_(language))" "https://en.wikipedia.org/wiki/Lisp_(language)")
                   ("[https://example.com/a]" "https://example.com/a")
                   ("'https://example.com/a'" "https://example.com/a")
                   ("\"https://example.com/a\"" "https://example.com/a")
                   ("“https://example.com/a”." "https://example.com/a")
                   ("**https://example.com/a**" "https://example.com/a")
                   ("<https://example.com/a>" "https://example.com/a")
                   ("at https://example.com/a?q=1&r=2#top." "https://example.com/a?q=1&r=2#top")
                   ("at http://[::1]:8080/x;" "http://[::1]:8080/x")
                   ("open file:///tmp/notes.txt" "file:///tmp/notes.txt")))
    (opencode-test-with-answer (session)
      (opencode-test-answer session "msg_bounds" text)
      (should (equal (cons text (opencode-test-links)) (list text (cons url url))))))
  (pcase-dolist (text '("https:// is a scheme" "nohttps://example.com" "http://."))
    (opencode-test-with-answer (session)
      (opencode-test-answer session "msg_none" text)
      (should (equal (cons text (opencode-test-links)) (list text))))))

(ert-deftest opencode-test-bare-url-in-links ()
  "A URL a Markdown or Org link holds is that link, never a link of its own."
  (opencode-test-with-answer (session)
    (opencode-test-answer session "msg_md"
                          "See [the docs](https://example.com/docs \"Docs\"), [https://a.example](https://b.example) and [[https://c.example][the Org page]].")
    (should (equal (opencode-test-links)
                   '(("the docs" . "https://example.com/docs")
                     ("https://a.example" . "https://b.example")
                     ("the Org page")))))
  (opencode-test-with-answer (session)
    (opencode-test-answer session "msg_org" "Bare Org: [[https://d.example]] done.")
    (should (equal (mapcar #'car (opencode-test-links)) '("https://d.example")))
    (should (button-get (button-at (opencode-test-find "https://d.example")) 'org-link))))

(ert-deftest opencode-test-bare-url-in-code ()
  "URLs in code spans and code blocks are links, which keep their code faces."
  (opencode-test-with-answer (session)
    (opencode-test-answer session "msg_code" "Run `curl https://example.com/a` or:\n\n```sh\ncurl https://example.com/b\n```\n\nDone.")
    (font-lock-ensure)
    (should (equal (opencode-test-links)
                   '(("https://example.com/a" . "https://example.com/a")
                     ("https://example.com/b" . "https://example.com/b"))))
    (let ((faces (ensure-list (get-text-property (opencode-test-find "https://example.com/a") 'face))))
      (should (memq 'link faces))
      (should (memq 'markdown-ts-code-span faces)))
    ;; The backticks around the code span stay code markup.
    (should-not (button-at (1- (opencode-test-find "curl https://example.com/a"))))))

(ert-deftest opencode-test-bare-url-streamed ()
  "A URL arriving over several deltas is one link once complete, never a partial one."
  (opencode-test-with-answer (session)
    (opencode-test-stream session "msg_stream" "Look at https://exa" "mple.com/pa")
    (should-not (opencode-test-links))
    (opencode-test-dispatch
     `(((type . "session.text.delta")
        (data (sessionID . ,session) (assistantMessageID . "msg_stream") (ordinal . 0) (delta . "th.")))))
    ;; The final period may yet be part of the URL.
    (should-not (opencode-test-links))
    (opencode-test-dispatch
     `(((type . "session.text.delta")
        (data (sessionID . ,session) (assistantMessageID . "msg_stream") (ordinal . 0) (delta . " Then https://second.example")))))
    (should (equal (opencode-test-links) '(("https://example.com/path" . "https://example.com/path"))))
    ;; The end of the text completes a URL ending it.
    (opencode-test-end-stream session "msg_stream" "Look at https://example.com/path. Then https://second.example")
    (should (equal (opencode-test-links) '(("https://example.com/path" . "https://example.com/path")
                                           ("https://second.example" . "https://second.example"))))))

(ert-deftest opencode-test-bare-url-yields-to-later-link ()
  "A URL made a link before the Org or Markdown link holding it closed gives way to that link."
  (opencode-test-with-answer (session)
    (opencode-test-stream session "msg_late" "See [[https://a.example][the A" " page]] and [https://b.example and" " more](https://c.example) now")
    (should (equal (opencode-test-links) '(("the A page") ("https://b.example and more" . "https://c.example"))))
    (let ((url (opencode-test-find "https://a.example")))
      (should-not (memq 'link (opencode-fontify-kinds url)))
      (should-not (get-text-property url 'opencode-bare-url)))
    (should (equal (button-get (button-at (opencode-test-find "the A page")) 'org-link-target)
                   "https://a.example"))))

(ert-deftest opencode-test-bare-url-live-matches-replay ()
  "Replaying a message makes the links streaming it made."
  (let* ((text "First https://example.com/one, then `https://example.com/two` and [x](https://example.com/three) https://example.com/end")
         (live (opencode-test-with-answer (session)
                 (opencode-test-stream session "msg_same" (substring text 0 20) (substring text 20 50) (substring text 50))
                 (opencode-test-end-stream session "msg_same" text)
                 (opencode-test-links)))
         (replayed (opencode-test-with-server
                     (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
                       (opencode-replay-message
                        `((type . "assistant") (id . "msg_same") (content ((type . "text") (text . ,text)))))
                       (opencode-test-links)))))
    (should (= (length live) 4))
    (should (equal live replayed))))

(ert-deftest opencode-test-bare-url-in-verbatim-text ()
  "A URL in an error keeps its keys and link face over the error's own face."
  (opencode-test-with-answer (session)
    (opencode--output-error '((type . "api") (message . "Rate limited; see https://status.example/.")))
    (let ((position (opencode-test-find "https://status.example/")))
      (should (equal (opencode-test-links) '(("https://status.example/" . "https://status.example/"))))
      ;; Fontification drops the buttons Markdown makes in verbatim text.
      (opencode-apply-fontify-properties (point-min) (point-max))
      (should (eq (get-text-property position 'category) 'default-button))
      (goto-char (1+ position))
      (should (eq (key-binding (kbd "RET")) 'push-button))
      (should (memq 'link (ensure-list (get-text-property position 'face))))
      (should (equal (opencode-fontify-kinds position) '(verbatim link)))
      (should (eq (get-text-property position 'font-lock-face) 'error)))))

(ert-deftest opencode-test-bare-url-in-blocks ()
  "URLs are links in a tool's output and in reasoning, not in block summaries."
  (opencode-test-with-answer (session)
    (let ((opencode-show-tool-output t)
          (base `((sessionID . ,session) (assistantMessageID . "msg_tool") (id . "call_url"))))
      (opencode-test-dispatch
       `(((type . "session.tool.input.started") (data ,@base (name . "shell")))
         ((type . "session.tool.called") (data ,@base (input (command . "curl https://api.example/v1"))))
         ((type . "session.tool.success")
          (data ,@base (content ((type . "text") (text . "Moved to https://api.example/v2\n")))))))
      (let ((block (opencode-block "call_url")))
        (should (overlay-get block 'opencode-expanded))
        (should-not (opencode-test-links (overlay-start block) (overlay-end block)))
        (should (member '("https://api.example/v2" . "https://api.example/v2") (opencode-test-links)))
        (should (member '("https://api.example/v1" . "https://api.example/v1")
                        (opencode-test-links (overlay-end block)))))
      (let ((reasoning `((sessionID . ,session) (assistantMessageID . "msg_tool") (ordinal . 0))))
        (opencode-test-dispatch
         `(((type . "session.reasoning.started") (data ,@reasoning))
           ((type . "session.reasoning.delta") (data ,@reasoning (delta . "Check https://think.example first.")))
           ((type . "session.reasoning.ended") (data ,@reasoning (text . "Check https://think.example first.")))))
        (let ((block (opencode-block (opencode-reasoning-key "msg_tool" 0))))
          (opencode-expand-block block)
          (should (equal (opencode-test-links (overlay-end block))
                         '(("https://think.example" . "https://think.example")))))))))

(ert-deftest opencode-test-url-in-input-is-not-a-link ()
  "Links typed in the input stay input: RET sends it, even once fontified as Markdown."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
      (should-not (memq 'markdown-ts--fontify-bare-uri jit-lock-functions))
      (goto-char (point-max))
      (insert "fetch https://example.com/x, [the docs](https://example.com/y) and <https://example.com/z>")
      (when jit-lock-mode (jit-lock-fontify-now))
      (font-lock-ensure)
      (should-not (opencode-test-links (opencode-pmark)))
      (dolist (text '("example.com/x" "the docs" "example.com/z"))
        (goto-char (opencode-test-find text))
        (should (eq (key-binding (kbd "RET")) 'opencode-send-input))))))

(provide 'opencode-links-test)
;;; opencode-links-test.el ends here
