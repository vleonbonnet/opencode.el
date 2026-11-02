;;; opencode-old-input-test.el --- Tests of editing earlier prompts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; RET or mouse-2 on an earlier prompt sends nothing: the prompt goes,
;; whole, to `opencode-old-input-function', by default into the input,
;; its attachments with it when they can go again.  A link in the
;; prompt keeps its own RET, and RET in the input still sends.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-old-send (text)
  "Type TEXT at the current buffer's prompt and send it with RET."
  (goto-char (point-max))
  (insert text)
  (should-not (opencode-test-press (current-buffer) "RET")))

(defun opencode-test-prompt-count ()
  "Return how many prompts were requested."
  (length (opencode-test-requests-to 'post "/prompt")))

(defun opencode-test-typed ()
  "Return the input typed at the current buffer's prompt."
  (buffer-substring-no-properties (opencode-pmark) (point-max)))

(defmacro opencode-test-with-recorded-old-input (&rest body)
  "Run BODY with `opencode-old-input-function' recording its calls.
The calls are in `recorded', newest first, as (TEXT BUFFER CURRENT)."
  (declare (indent 0) (debug t))
  `(let* ((recorded nil)
          (opencode-old-input-function
           (lambda (text buffer) (push (list text buffer (current-buffer)) recorded))))
     ,@body))

(ert-deftest opencode-test-old-input-ret-gives-whole-prompt ()
  "RET on an earlier prompt, of one line or several, gives it whole and sends nothing."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (opencode-test-old-send "a single line")
      (opencode-test-old-send "first line\nsecond line\nthird line")
      (let ((prompts (opencode-test-prompt-count)))
        (should (= prompts 2))
        (opencode-test-with-recorded-old-input
          (dolist (spot '(("single" . 0) ("second" . 3) ("first" . 0) ("third line" . 10)))
            (goto-char (+ (opencode-test-find (car spot)) (cdr spot)))
            (should-not (opencode-test-press (current-buffer) "RET")))
          (should (equal (mapcar #'car (reverse recorded))
                         '("a single line" "first line\nsecond line\nthird line"
                           "first line\nsecond line\nthird line" "first line\nsecond line\nthird line")))
          (dolist (call recorded)
            (should (eq (nth 1 call) (current-buffer)))
            (should (eq (nth 2 call) (current-buffer))))
          ;; The input is left alone, and nothing is sent.
          (should (equal (opencode-test-typed) ""))
          (should (= (opencode-test-prompt-count) prompts)))))))

(ert-deftest opencode-test-old-input-archived-restores-nothing ()
  "RET on an earlier prompt of an archived session restores nothing, sending nothing."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (opencode-test-old-send "before the archive")
      (opencode--note-archived opencode-session-id 1000)
      (setq opencode-test-requests nil)
      (opencode-test-with-recorded-old-input
        (goto-char (opencode-test-find "before the archive"))
        (should-not (opencode-test-press (current-buffer) "RET"))
        (should (equal (mapcar #'car recorded) '("before the archive"))))
      (should-not opencode-test-requests)
      (should (opencode-session-archived-p)))))

(ert-deftest opencode-test-old-input-default-puts-it-in-the-input ()
  "Without a function of its own, the earlier prompt goes into the input, a draft kept."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (opencode-test-old-send "first line\nsecond line")
      (let ((prompts (opencode-test-prompt-count))
            (opencode-old-input-function nil))
        (goto-char (opencode-test-find "second"))
        (should-not (opencode-test-press (current-buffer) "RET"))
        (should (equal (opencode-test-typed) "first line\nsecond line"))
        (should (= (point) (point-max)))
        ;; A draft stays, the prompt following it after a blank line.
        (delete-region (opencode-pmark) (point-max))
        (goto-char (point-max))
        (insert "my draft")
        (goto-char (opencode-test-find "first"))
        (let ((messages nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format &rest arguments)
                       (when format (push (apply #'format-message format arguments) messages)))))
            (should-not (opencode-test-press (current-buffer) "RET")))
          (should (member "The earlier prompt follows the input already typed" messages)))
        (should (equal (opencode-test-typed) "my draft\n\nfirst line\nsecond line"))
        (should (= (opencode-test-prompt-count) prompts))
        ;; A blank input is replaced.
        (delete-region (opencode-pmark) (point-max))
        (goto-char (point-max))
        (insert "  \n")
        (goto-char (opencode-test-find "first"))
        (should-not (opencode-test-press (current-buffer) "RET"))
        (should (equal (opencode-test-typed) "first line\nsecond line"))
        ;; RET in the input still sends it.
        (should-not (opencode-test-press (current-buffer) "RET"))
        (should (= (opencode-test-prompt-count) (1+ prompts)))
        (should (equal (alist-get 'text (car (last (opencode-test-requests-to 'post "/prompt"))))
                       "first line\nsecond line"))))))

(ert-deftest opencode-test-old-input-link-keeps-its-ret ()
  "RET on a link of an earlier prompt follows it; elsewhere on the prompt, edits it."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (opencode-test-old-send "Look at https://example.com/page please")
      (font-lock-ensure)
      (let ((prompts (opencode-test-prompt-count))
            (browsed nil))
        (opencode-test-with-recorded-old-input
          (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (push url browsed))))
            (goto-char (+ (opencode-test-find "https://example.com") 4))
            (should (eq (key-binding (kbd "RET")) 'push-button))
            (should-not (opencode-test-press (current-buffer) "RET"))
            (should (equal browsed '("https://example.com/page")))
            (should-not recorded)
            (goto-char (opencode-test-find "please"))
            (should-not (opencode-test-press (current-buffer) "RET"))
            (should (equal (mapcar #'car recorded) '("Look at https://example.com/page please")))))
        (should (= (opencode-test-prompt-count) prompts))))))

(ert-deftest opencode-test-old-input-ret-on-output-sends-nothing ()
  "RET on the transcript outside a prompt sends nothing, as Comint would."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (opencode-test-old-send "hello")
      (opencode-test-answer opencode-session-id "msg_answer" "An answer line")
      (let ((prompts (opencode-test-prompt-count)))
        (opencode-test-with-recorded-old-input
          (goto-char (opencode-test-find "answer line"))
          (should-not (get-text-property (point) 'opencode-prompt))
          (should-not (opencode-test-press (current-buffer) "RET"))
          (should-not recorded))
        (should (= (opencode-test-prompt-count) prompts))
        (should (equal (opencode-test-typed) ""))))))

(ert-deftest opencode-test-old-input-attachments-go-again ()
  "An earlier prompt's mention and file on disk go with it again; others are text."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (let* ((readme (expand-file-name "README.md" opencode-test-workspace))
             (uri (opencode--file-uri readme))
             (gone (make-temp-file "opencode-gone-" nil ".txt"))
             (gone-uri (opencode--file-uri gone))
             (opencode-old-input-function nil))
        (goto-char (point-max))
        (insert "look at ")
        (opencode--attach-file readme "README.md")
        (opencode--insert-mention "explore")
        (insert " and ")
        (opencode--attach-file gone "gone.txt")
        (opencode--insert-attachment "region: some text" (list :file `((uri . ,(opencode--data-uri "x"))
                                                                        (name . "region"))))
        (should-not (opencode-test-press (current-buffer) "RET"))
        (should (equal (mapcar (lambda (file) (alist-get 'uri file)) (alist-get 'files (opencode-test-sent-prompt)))
                       (list uri gone-uri (opencode--data-uri "x"))))
        (delete-file gone)
        (let ((text (progn (goto-char (opencode-test-find "look at")) (opencode-old-input))))
          (should (equal (substring-no-properties text)
                         "look at `README.md` `@explore` and `gone.txt` `region: some text`"))
          (should (equal (get-text-property (string-search "README" text) 'opencode-sent-attachment text)
                         (list :file `((uri . ,uri) (name . "README.md")))))
          (should (equal (get-text-property (string-search "@explore" text) 'opencode-sent-attachment text)
                         '(:agent "explore")))
          (should-not (get-text-property (string-search "gone" text) 'opencode-sent-attachment text))
          (should-not (get-text-property (string-search "region" text) 'opencode-sent-attachment text)))
        (should-not (opencode-test-press (current-buffer) "RET"))
        (should (equal (opencode-test-typed)
                       "look at `README.md` `@explore` and `gone.txt` `region: some text`"))
        (should (equal (sort (mapcar #'car opencode--attachments)) '(:agent :file)))
        (should (= (length (opencode-test-attachment-overlays)) 2))
        ;; They go with the prompt sent again.
        (setq opencode-test-requests nil)
        (should-not (opencode-test-press (current-buffer) "RET"))
        (let ((body (opencode-test-sent-prompt)))
          (should (equal (mapcar (lambda (file) (alist-get 'uri file)) (alist-get 'files body)) (list uri)))
          (should (equal (alist-get 'agents body) [((name . "explore"))])))))))

(ert-deftest opencode-test-old-input-other-clients-attachments ()
  "Another client's prompt, rendered from the service, gives its attachments again."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (file (expand-file-name "notes.txt" opencode-test-workspace))
           (uri (opencode--file-uri file)))
      (with-current-buffer (opencode-test-open-empty session)
        (opencode-test-dispatch
         `(((type . "session.inbox.enqueued")
            (data (sessionID . ,session) (inboxID . "msg_other")
                  (item (type . "user") (delivery . "steer")
                        (payload (text . "from the TUI")
                                 (files . [((data . "bm90ZXMK") (mime . "text/plain") (name . "notes.txt")
                                            (source (type . "uri") (uri . ,uri)))
                                           ((data . "eA==") (mime . "text/plain") (name . "inline")
                                            (source (type . "inline")))])
                                 (agents . [((name . "explore"))])))))
           ((type . "session.inbox.delivered") (data (sessionID . ,session) (inboxID . "msg_other")))))
        (let ((opencode-old-input-function nil))
          (goto-char (opencode-test-find "from the TUI"))
          (should-not (opencode-test-press (current-buffer) "RET")))
        (should (equal (opencode-test-typed) "from the TUI `notes.txt` `inline` `@explore`"))
        (should (equal (sort (mapcar #'car opencode--attachments)) '(:agent :file)))
        (should (equal (alist-get 'uri (plist-get (seq-find (lambda (attachment) (plist-get attachment :file))
                                                            opencode--attachments)
                                                  :file))
                       uri))))))

(ert-deftest opencode-test-old-input-mouse-and-copy ()
  "mouse-2 on an earlier prompt does what RET does; C-c RET puts it in the input."
  (opencode-test-with-server
    (let ((buffer (opencode-test-open-empty (opencode-test-scenario "text"))))
      (with-current-buffer buffer
        (opencode-test-old-send "first line\nsecond line")
        (should (eq (command-remapping 'comint-insert-input) 'opencode-mouse-old-input))
        (should (eq (key-binding (kbd "C-c RET") nil t) 'comint-copy-old-input))
        (should (eq (key-binding (kbd "C-c RET")) 'opencode-copy-old-input))
        (should (eq (command-remapping 'comint-copy-old-input) 'opencode-copy-old-input))
        (should (equal (get-text-property (opencode-test-find "second") 'help-echo)
                       "RET, mouse-2: edit this prompt to send it again"))
        (let ((prompts (opencode-test-prompt-count)))
          (opencode-test-with-recorded-old-input
            (save-window-excursion
              (set-window-buffer (selected-window) buffer)
              (let ((position (opencode-test-find "second")))
                (opencode-mouse-old-input
                 (list 'mouse-2 (list (selected-window) position '(0 . 0) 0 nil position nil nil nil nil)))))
            (should (equal (mapcar #'car recorded) '("first line\nsecond line")))
            ;; C-c RET puts it in the input whatever the function.
            (goto-char (opencode-test-find "first"))
            (should-not (opencode-test-press buffer "C-c RET"))
            (should (= (length recorded) 1))
            (should (equal (opencode-test-typed) "first line\nsecond line")))
          (should (= (opencode-test-prompt-count) prompts)))))))

(provide 'opencode-old-input-test)
;;; opencode-old-input-test.el ends here
