;;; opencode-input-test.el --- Tests of the input kept across rendering and sends  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Rendering lifts the input typed at the prompt out and puts it back:
;; another client's prompt goes before it, and a transcript rendered
;; again, by a reload or a catch-up, erases it.  The input comes back
;; with its attachment labels standing for their attachments.  Input
;; sent through `comint-send-input' itself goes as RET sends it.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-typed-input ()
  "Return the input typed at the current buffer's prompt."
  (buffer-substring-no-properties (opencode-pmark) (point-max)))

(defun opencode-test-check-kept-attachments (uri)
  "Check that the input still holds the labels of the file of URI and of the mention.
Each label stands for its attachment, which the next prompt carries."
  (let ((input (opencode-test-typed-input))
        (overlays (sort (opencode-test-attachment-overlays) :key #'overlay-start)))
    (should (string-prefix-p "look at `" input))
    (should (string-suffix-p "`@explore`" input))
    (should (= (length overlays) 2))
    (should (equal (mapcar (lambda (overlay) (car (overlay-get overlay 'opencode-attachment))) overlays)
                   '(:file :agent)))
    (dolist (overlay overlays)
      (should (>= (overlay-start overlay) (opencode-pmark)))
      (should (memq (overlay-get overlay 'opencode-attachment) opencode--attachments))
      (should (equal (buffer-substring-no-properties (overlay-start overlay) (1+ (overlay-start overlay))) "`"))
      (should (equal (buffer-substring-no-properties (1- (overlay-end overlay)) (overlay-end overlay)) "`"))
      (should (overlay-get overlay 'display))
      (should (equal (overlay-get overlay 'modification-hooks) '(opencode--remove-attachment))))
    (should (= (length opencode--attachments) 2))
    (should (equal (alist-get 'uri (plist-get (seq-find (lambda (attachment) (plist-get attachment :file))
                                                        opencode--attachments)
                                              :file))
                   uri))))

(defun opencode-test-check-sends-kept-attachments (uri)
  "Send the input with RET and check the prompt carried the file of URI and the mention."
  (setq opencode-test-requests nil)
  (opencode-send-input)
  (let ((body (opencode-test-sent-prompt)))
    (should (equal (mapcar (lambda (file) (alist-get 'uri file)) (alist-get 'files body)) (list uri)))
    (should (equal (alist-get 'agents body) [((name . "explore"))])))
  (should-not opencode--attachments)
  (should-not (opencode-test-attachment-overlays)))

(defun opencode-test-delete-mention-label ()
  "Delete the mention's label at the end of the input, as typing would."
  (goto-char (point-max))
  (delete-char -1))

;;;; Another client's prompt

(ert-deftest opencode-test-foreign-prompt-keeps-attachments ()
  "Another client's prompt rendered while the input holds attachments keeps them."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open-empty session)
        (let ((uri (opencode-test-type-attachments)))
          (opencode-test-dispatch (opencode-test-inbox-events session "msg_other" "from the TUI"))
          (should (string-search "from the TUI" (buffer-substring-no-properties (point-min) (opencode-pmark))))
          (opencode-test-check-kept-attachments uri)
          ;; Twice, and the labels still delete as typed ones do.
          (opencode-test-dispatch (opencode-test-inbox-events session "msg_again" "again from the TUI"))
          (opencode-test-check-kept-attachments uri)
          (opencode-test-delete-mention-label)
          (should (equal (mapcar #'car opencode--attachments) '(:file)))
          (should-not (string-search "explore" (opencode-test-typed-input)))
          (setq opencode-test-requests nil)
          (opencode-send-input)
          (let ((body (opencode-test-sent-prompt)))
            (should (equal (mapcar (lambda (file) (alist-get 'uri file)) (alist-get 'files body)) (list uri)))
            (should-not (alist-get 'agents body))))))))

(ert-deftest opencode-test-foreign-prompt-then-send-keeps-attachments ()
  "Input lifted around another client's prompt sends its attachments."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open-empty session)
        (let ((uri (opencode-test-type-attachments)))
          (opencode-test-dispatch (opencode-test-inbox-events session "msg_other" "from the TUI"))
          (opencode-test-check-sends-kept-attachments uri))))))

;;;; Rendering again

(ert-deftest opencode-test-reload-keeps-attachments ()
  "Reloading the transcript keeps the input's attachments, standing for their labels."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
      (let ((uri (opencode-test-type-attachments))
            (transcript (buffer-substring-no-properties (point-min) (opencode-pmark))))
        (revert-buffer)
        (should (equal (buffer-substring-no-properties (point-min) (opencode-pmark)) transcript))
        (opencode-test-check-kept-attachments uri)
        ;; A label rendered again deletes as a typed one does.
        (opencode-test-delete-mention-label)
        (should (equal (mapcar #'car opencode--attachments) '(:file)))
        (should (= (length (opencode-test-attachment-overlays)) 1))
        (opencode--insert-mention "explore")
        (opencode-test-check-sends-kept-attachments uri)))))

(ert-deftest opencode-test-failed-reload-keeps-input ()
  "A transcript that cannot be loaded again leaves the input and its attachments."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open (opencode-test-scenario "text"))
      (let ((uri (opencode-test-type-attachments))
            (reported nil)
            (opencode-test-routes (cons (list 'get "/message\\'" (lambda (_) '(:error 500 "boom")))
                                        opencode-test-routes)))
        (cl-letf (((symbol-function 'opencode-report-error) (lambda (text) (push text reported))))
          (opencode--render-again))
        (should (= (length reported) 1))
        (should (string-search "boom" (car reported)))
        (opencode-test-check-kept-attachments uri)))))

(ert-deftest opencode-test-catch-up-rendering-whole-keeps-attachments ()
  "Catching up by rendering the whole transcript again keeps the input's attachments."
  (opencode-test-with-transcript (opencode-test-long-transcript 2)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id))
           (uri (with-current-buffer buffer (opencode-test-type-attachments))))
      ;; The message the buffer is in sync with is gone, as after a revert.
      (setq opencode-test-transcript (opencode-test-long-transcript 1))
      (opencode-test-reconnect)
      (with-current-buffer buffer
        (should-not (string-search "part 1" (buffer-substring-no-properties (point-min) (opencode-pmark))))
        (opencode-test-check-kept-attachments uri)
        (opencode-test-check-sends-kept-attachments uri)))))

;;;; Sending through Comint

(ert-deftest opencode-test-comint-send-input-sends-as-ret ()
  "`comint-send-input' run in a session buffer runs the checks and sends the attachments."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (let* ((checked 0)
             (opencode-send-checks (list (lambda (_) (cl-incf checked) nil)))
             (uri (opencode-test-type-attachments)))
        (let ((this-command 'comint-send-input))
          (call-interactively #'comint-send-input))
        (should (= checked 1))
        (let ((body (opencode-test-sent-prompt)))
          (should (equal (mapcar (lambda (file) (alist-get 'uri file)) (alist-get 'files body)) (list uri)))
          (should (equal (alist-get 'agents body) [((name . "explore"))])))
        (should-not opencode--attachments)
        (should (equal (opencode-test-typed-input) ""))))))

(ert-deftest opencode-test-comint-send-input-restores-archived ()
  "`comint-send-input' in an archived session's buffer restores it first, as RET does."
  (opencode-test-with-server
    (let ((opencode-test-routes
           (cons (list 'patch "\\`/api/session/\\(ses_[A-Za-z0-9]+\\)\\'"
                       (lambda (request) (cons :defer (lambda () `((data (id . ,(plist-get request :match))))))))
                 opencode-test-routes)))
      (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
        (opencode--note-archived opencode-session-id 1000)
        (let ((uri (opencode-test-type-attachments)))
          (setq opencode-test-requests nil)
          (comint-send-input)
          (should (equal (seq-keep (lambda (request)
                                     (cond ((eq (plist-get request :method) 'patch) 'patch)
                                           ((string-suffix-p "/prompt" (plist-get request :path)) 'prompt)))
                                   opencode-test-requests)
                         '(patch)))
          (opencode-test-check-kept-attachments uri)
          (should (opencode-test-release))
          (let ((body (opencode-test-sent-prompt)))
            (should (equal (mapcar (lambda (file) (alist-get 'uri file)) (alist-get 'files body)) (list uri)))))))))

(ert-deftest opencode-test-comint-cancelled-send-puts-input-back ()
  "Input Comint took that a send check keeps comes back with its attachments."
  (opencode-test-with-server
    (with-current-buffer (opencode-test-open-empty (opencode-test-scenario "text"))
      (let ((opencode-send-checks (list (lambda (_) 'cancel)))
            (uri (opencode-test-type-attachments)))
        ;; Comint's own sends, with arguments, are not RET's: Comint takes the input.
        (comint-send-input nil t)
        (should-not (opencode-test-sent-prompt))
        (opencode-test-check-kept-attachments uri)
        (opencode-test-delete-mention-label)
        (should (equal (mapcar #'car opencode--attachments) '(:file)))))))

(ert-deftest opencode-test-comint-send-input-elsewhere-unchanged ()
  "`comint-send-input' outside session buffers sends as Comint does."
  (with-temp-buffer
    (comint-mode)
    (let ((process (make-pipe-process :name "opencode-test-comint" :buffer (current-buffer) :noquery t))
          (sent nil))
      (unwind-protect
          (progn
            (setq-local comint-input-sender (lambda (_process string) (push string sent)))
            (set-marker (process-mark process) (point-max))
            (insert "hello")
            (comint-send-input)
            (should (equal sent '("hello"))))
        (delete-process process)))))

(provide 'opencode-input-test)
;;; opencode-input-test.el ends here
