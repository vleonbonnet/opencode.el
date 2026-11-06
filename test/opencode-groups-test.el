;;; opencode-groups-test.el --- Tests of block boxes and groups of earlier turns  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A block's summary takes the mouse on its box only, which keeps its
;; look and its width under the mouse.  Once a later prompt is sent, the
;; runs of blocks of earlier turns collapse into groups: live, on a
;; reload and on a catch-up alike, the current turn and blocks waiting
;; on the user never, points hidden going to the group's summary.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-group-tool (id name status &optional input)
  "Return the content of tool call ID of NAME in STATUS, called with INPUT."
  `((type . "tool") (id . ,id) (name . ,name)
    (state (status . ,status) (input . ,(or input '((command . "make check"))))
           (content ((type . "text") (text . "ok")))
           ,@(when (equal status "error") '((error (type . "") (message . "boom")))))))

(defun opencode-test-group-turn (n &optional blocks)
  "Return the messages of turn N: a prompt, a step of BLOCKS tool calls then text, its end.
BLOCKS defaults to three: reasoning, a file read and a shell command."
  (list (opencode-test-prompt (format "msg_g%02du" n) (format "Question %d" n))
        (apply #'opencode-test-step (format "msg_g%02da" n)
               (append (if blocks
                           (mapcar (lambda (index)
                                     (opencode-test-group-tool (format "call_g%02d_%d" n index) "shell" "completed"))
                                   (number-sequence 1 blocks))
                         (list `((type . "reasoning") (text . ,(format "Thinking about %d." n)))
                               (opencode-test-group-tool (format "call_g%02dr" n) "read" "completed"
                                                         '((path . "README.md")))
                               (opencode-test-group-tool (format "call_g%02ds" n) "shell" "error")))
                       (list `((type . "text") (text . ,(format "Answer %d." n))))))
        (opencode-test-idle (format "msg_g%02di" n))))

(defun opencode-test-visible-text ()
  "Return the text of the current buffer that shows, hidden text left out.
That is invisible text, and text an overlay hides otherwise, such as
the prompt string before each prompt (see `opencode--hidden-p')."
  (let ((parts nil)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-char-property-change pos)))
        (unless (or (invisible-p pos) (opencode--hidden-p pos))
          (push (buffer-substring-no-properties pos next) parts))
        (setq pos next)))
    (string-trim-right (apply #'concat (nreverse parts)))))

(defun opencode-test-groups ()
  "Return the groups of the current buffer, in order, as (SUMMARY MEMBERS EXPANDED)."
  (mapcar (lambda (overlay)
            (list (buffer-substring-no-properties (overlay-start overlay) (1- (overlay-end overlay)))
                  (length (plist-get (opencode-block-data overlay) :members))
                  (and (overlay-get overlay 'opencode-expanded) t)))
          (sort (seq-filter (lambda (overlay) (and (overlay-get overlay 'opencode-collapsible)
                                                   (opencode-group-p overlay)))
                            (overlays-in (point-min) (point-max)))
                :key #'overlay-start)))

(defun opencode-test-group-at (text)
  "Return the group overlay whose summary holds TEXT, the first in the buffer."
  (opencode-collapsible-at (opencode-test-find text)))

(defun opencode-test-fresh-look (id)
  "Return (TEXT VISIBLE GROUPS) of a buffer opened afresh on session ID, and kill it."
  (let ((buffer (gethash id opencode-session-buffers)))
    (with-current-buffer buffer
      (rename-buffer "opencode-test-old" t)
      (remhash id opencode-session-buffers))
    (let ((fresh (opencode-test-open id)))
      (prog1 (with-current-buffer fresh
               (list (opencode-test-text fresh) (opencode-test-visible-text) (opencode-test-groups)))
        (kill-buffer fresh)
        (puthash id buffer opencode-session-buffers)))))

(defun opencode-test-look ()
  "Return (TEXT VISIBLE GROUPS) of the current buffer."
  (list (opencode-test-text (current-buffer)) (opencode-test-visible-text) (opencode-test-groups)))

;;;; The box

(defun opencode-test-summary-runs (overlay)
  "Return the (START END PROPERTY-VALUES) runs of OVERLAY's summary line.
PROPERTY-VALUES holds the `mouse-face' and `opencode-box' there."
  (let ((pos (overlay-start overlay))
        (end (1- (overlay-end overlay)))
        (runs nil))
    (while (< pos end)
      (let ((next (min (next-single-property-change pos 'mouse-face nil end)
                       (next-single-property-change pos 'opencode-box nil end))))
        (push (list pos next (get-text-property pos 'mouse-face) (get-text-property pos 'opencode-box)) runs)
        (setq pos next)))
    (nreverse runs)))

(ert-deftest opencode-test-group-box-alone-takes-the-mouse ()
  "Only a summary's box has a mouse face, one for icon and label, the icon's own face."
  (opencode-test-with-transcript (opencode-test-group-turn 1)
    (with-current-buffer (opencode-test-open (opencode-test-session))
      (font-lock-ensure)
      (dolist (overlay (list (opencode-block "call_g01r") (opencode-block "call_g01s")
                             (opencode-block (opencode-reasoning-key "msg_g01a" 0))))
        (should-not (overlay-get overlay 'mouse-face))
        (should-not (overlay-get overlay 'help-echo))
        (let* ((runs (opencode-test-summary-runs overlay))
               (boxed (seq-filter (lambda (run) (nth 3 run)) runs)))
          ;; The box starts the line, and nothing else has a mouse face.
          (should (= (car (car boxed)) (overlay-start overlay)))
          (should (seq-every-p (lambda (run) (eq (and (nth 2 run) t) (and (nth 3 run) t))) runs))
          (should (seq-every-p (lambda (run) (null (get-text-property (car run) 'help-echo))) runs))
          ;; One object, so that the box highlights as one.
          (should (= (length (seq-uniq (mapcar (lambda (run) (nth 2 run)) boxed) #'eq)) 1))
          (let ((icon-face (get-text-property (overlay-start overlay) 'font-lock-face))
                (mouse-face (nth 2 (car boxed))))
            (should (equal mouse-face icon-face))
            ;; The label keeps its box and slant under the mouse.
            (let ((label-face (get-text-property (car (car (last boxed))) 'font-lock-face)))
              (should (equal (plist-get mouse-face :box) (plist-get label-face :box)))
              (should (eq (plist-get mouse-face :slant) (plist-get label-face :slant)))))))
      ;; A failed call's box hovers in its own color.
      (should (equal (plist-get (get-text-property (overlay-start (opencode-block "call_g01s")) 'mouse-face)
                                :foreground)
                     (face-foreground 'error nil t))))))

(ert-deftest opencode-test-group-click-on-box-only ()
  "A click on a summary's box toggles its block; elsewhere on the line, it runs the global click."
  (opencode-test-with-transcript (opencode-test-group-turn 1)
    (let ((buffer (opencode-test-open (opencode-test-session)))
          (clicked nil))
      (with-current-buffer buffer
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (let* ((overlay (opencode-block "call_g01r"))
                 (global-map (make-sparse-keymap)))
            (keymap-set global-map "<mouse-1>" (lambda (event) (interactive "e") (push event clicked)))
            (cl-flet ((click (position)
                        (opencode-mouse-toggle-block
                         (list 'mouse-1 (list (selected-window) position '(0 . 0) 0 nil position
                                              nil nil nil nil)))))
              (should (eq (lookup-key (overlay-get overlay 'keymap) [mouse-1]) 'opencode-mouse-toggle-block))
              (click (opencode-test-find "README.md"))
              (should-not (overlay-get overlay 'opencode-expanded))
              (should (= (length clicked) 1))
              (click (1+ (overlay-start overlay)))
              (should (overlay-get overlay 'opencode-expanded))
              (click (opencode-test-find " read "))
              (should-not (overlay-get overlay 'opencode-expanded))
              (should (= (length clicked) 1))
              ;; TAB stays on the whole line; RET quotes it, as on any line.
              (goto-char (opencode-test-find "README.md"))
              (should (eq (key-binding (kbd "TAB")) 'opencode-toggle-block))
              (should (eq (key-binding (kbd "RET")) 'opencode-send-input)))))))))

(defun opencode-test-pixel-positions (buffer start end)
  "Return the x pixel positions of the characters of BUFFER from START to END.
BUFFER shows in the selected window, whose start is START."
  (set-window-buffer (selected-window) buffer)
  (with-current-buffer buffer
    (set-window-start (selected-window) start t)
    (redisplay t)
    (mapcar (lambda (position) (car (posn-x-y (posn-at-point position))))
            (number-sequence start end))))

(defun opencode-test-hovered-copy (overlay)
  "Return a buffer drawing OVERLAY's summary line as Emacs draws it under the mouse.
The text with a `mouse-face' shows in it alone, as the display does,
which merges no face of the text under it."
  (let* ((start (overlay-start overlay))
         (line (buffer-substring start (1- (overlay-end overlay))))
         (remapping face-remapping-alist)
         (copy (generate-new-buffer " *opencode hovered*")))
    (with-current-buffer copy
      (setq-local face-remapping-alist remapping)
      (insert line)
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let ((next (next-single-property-change pos 'font-lock-face nil (point-max))))
            (put-text-property pos next 'face (get-text-property pos 'font-lock-face))
            (setq pos next)))
        (setq pos (point-min))
        (while (< pos (point-max))
          (let ((next (next-single-property-change pos 'mouse-face nil (point-max))))
            (when-let* ((mouse-face (get-text-property pos 'mouse-face)))
              (put-text-property pos next 'face mouse-face))
            (setq pos next)))))
    copy))

(defun opencode-test-plain-copy (overlay)
  "Return a buffer drawing OVERLAY's summary line as Emacs draws it without the mouse."
  (let ((line (buffer-substring (overlay-start overlay) (1- (overlay-end overlay))))
        (remapping face-remapping-alist)
        (copy (generate-new-buffer " *opencode plain*")))
    (with-current-buffer copy
      (setq-local face-remapping-alist remapping)
      (insert line)
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let ((next (next-single-property-change pos 'font-lock-face nil (point-max))))
            (put-text-property pos next 'face (get-text-property pos 'font-lock-face))
            (setq pos next)))))
    copy))

(ert-deftest opencode-test-group-hover-keeps-widths ()
  "Under the mouse, a summary's box keeps every glyph where it was, its box drawn still.
Run in a graphical Emacs, with the user's font and theme when given."
  :tags '(gui)
  (skip-unless (display-graphic-p))
  (opencode-test-with-transcript (opencode-test-group-turn 1)
    (let ((buffer (opencode-test-open (opencode-test-session))))
      (with-current-buffer buffer
        (save-window-excursion
          (dolist (overlay (list (opencode-block "call_g01r") (opencode-block "call_g01s")
                                 (opencode-block (opencode-reasoning-key "msg_g01a" 0))))
            (let ((plain (opencode-test-plain-copy overlay))
                  (hovered (opencode-test-hovered-copy overlay))
                  ;; As the summary drew before: the line's mouse face
                  ;; replaced the box's faces.
                  (former (opencode-test-plain-copy overlay)))
              (unwind-protect
                  (progn
                    (with-current-buffer former
                      (put-text-property (point-min) (point-max) 'face 'highlight))
                    (let ((end (with-current-buffer plain (point-max))))
                      (should (equal (opencode-test-pixel-positions plain 1 end)
                                     (opencode-test-pixel-positions hovered 1 end)))
                      ;; The test tells the two looks apart.
                      (should-not (equal (opencode-test-pixel-positions plain 1 end)
                                         (opencode-test-pixel-positions former 1 end))))
                    ;; The label, after the icon's three characters, keeps its box.
                    (with-current-buffer hovered
                      (should (plist-get (get-text-property 5 'face) :box))))
                (mapc #'kill-buffer (list plain hovered former))))))))))

(ert-deftest opencode-test-group-arrow-click-on-box ()
  "A click on a summary's arrow lands on its box, and so toggles the block."
  :tags '(gui)
  (skip-unless (display-graphic-p))
  (opencode-test-with-transcript (opencode-test-group-turn 1)
    (let ((buffer (opencode-test-open (opencode-test-session))))
      (with-current-buffer buffer
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (let ((overlay (opencode-block "call_g01r")))
            (set-window-start (selected-window) (point-min) t)
            (redisplay t)
            (let* ((y (cdr (posn-x-y (posn-at-point (overlay-start overlay)))))
                   (posn (posn-at-x-y 2 y)))
              ;; The arrow is the overlay's string, shown at its start.
              (should (equal (car (posn-string posn)) (overlay-get overlay 'before-string)))
              (should (= (posn-point posn) (overlay-start overlay)))
              (opencode-mouse-toggle-block (list 'mouse-1 posn))
              (should (overlay-get overlay 'opencode-expanded)))))))))

;;;; Groups

(ert-deftest opencode-test-group-reload-groups-earlier-turns ()
  "Opened, a session groups the runs of its earlier turns, never the current one."
  (opencode-test-with-transcript (append (opencode-test-group-turn 1) (opencode-test-group-turn 2)
                                         (opencode-test-group-turn 3))
    (with-current-buffer (opencode-test-open (opencode-test-session))
      (should (equal (opencode-test-groups)
                     '((" 󰌨  3 blocks  1 thinking · 1 read · 1 shell · 1 failed" 3 nil)
                       (" 󰌨  3 blocks  1 thinking · 1 read · 1 shell · 1 failed" 3 nil))))
      (let ((visible (opencode-test-visible-text)))
        ;; Earlier turns show their answers between groups.
        (should (string-search "Question 1\n\n 󰌨  3 blocks" visible))
        (should (string-search "1 failed\n\nAnswer 1." visible))
        (should-not (string-search "Thinking about 1." visible))
        (should-not (string-search "Thinking about 2." visible))
        ;; The current turn shows each of its blocks.
        (should (string-search "Thinking about 3." visible))
        (should (= (cl-count ?\n (opencode-test-text (current-buffer)))
                   (+ 2 (cl-count ?\n (opencode-test-visible-text)) 6)))))))

(ert-deftest opencode-test-group-single-block-stays ()
  "A run of one block of an earlier turn is no group."
  (opencode-test-with-transcript (append (opencode-test-group-turn 1 1) (opencode-test-group-turn 2 2)
                                         (opencode-test-group-turn 3 2))
    (with-current-buffer (opencode-test-open (opencode-test-session))
      (should (equal (mapcar #'cadr (opencode-test-groups)) '(2)))
      (should (opencode-block "call_g01_1"))
      (should-not (overlay-get (opencode-block "call_g01_1") 'opencode-group))
      (should (eq (overlay-get (opencode-block "call_g02_1") 'opencode-group)
                  (overlay-get (opencode-block "call_g02_2") 'opencode-group))))))

(ert-deftest opencode-test-group-live-on-new-prompt ()
  "A prompt sent here groups the turn before it, as one rendered from the service does."
  (opencode-test-with-transcript (opencode-test-group-turn 1)
    (let ((id (opencode-test-session)))
      (with-current-buffer (opencode-test-open id)
        (should-not (opencode-test-groups))
        (goto-char (point-max))
        (insert "Question 2")
        (should-not (opencode-test-press (current-buffer) "RET"))
        (should (equal (mapcar #'cadr (opencode-test-groups)) '(3)))
        (should-not (string-search "Thinking about 1." (opencode-test-visible-text)))
        ;; Another client's prompt groups the turn before it too.
        (opencode-test-dispatch (opencode-test-inbox-events id "msg_g02x" "From the TUI"))
        (should (equal (mapcar #'cadr (opencode-test-groups)) '(3)))))))

(ert-deftest opencode-test-group-live-matches-reload ()
  "The groups a turn's live events end with are those a reload draws."
  (opencode-test-with-transcript (opencode-test-group-turn 1)
    (let* ((id (opencode-test-session))
           (step '((assistantMessageID . "msg_g02a"))))
      (with-current-buffer (opencode-test-open id)
        (opencode-test-dispatch
         (append
          (opencode-test-inbox-events id "msg_g02u" "Question 2")
          (list (opencode-test-event id "session.execution.started" "evt_g02s")
                (apply #'opencode-test-event id "session.step.started" "evt_g02b" step))
          (apply #'append
                 (mapcar (lambda (call)
                           (list (apply #'opencode-test-event id "session.tool.called" (format "evt_%s_c" call)
                                        `(id . ,call) '(name . "shell") '(input (command . "make check")) step)
                                 (apply #'opencode-test-event id "session.tool.success" (format "evt_%s_s" call)
                                        `(id . ,call) '(content ((type . "text") (text . "ok"))) step)))
                         '("call_g02_1" "call_g02_2")))))
        (opencode-test-answer id "msg_g02a" "Answer 2.")
        ;; The current turn is not grouped; the earlier one is.
        (should (equal (mapcar #'cadr (opencode-test-groups)) '(3)))
        (opencode-test-dispatch (opencode-test-inbox-events id "msg_g03u" "Question 3"))
        (should (equal (mapcar #'cadr (opencode-test-groups)) '(3 2)))
        (let ((live (opencode-test-look)))
          (setq opencode-test-transcript
                (append (opencode-test-group-turn 1)
                        (list (opencode-test-prompt "msg_g02u" "Question 2")
                              (opencode-test-step "msg_g02a"
                                                  (opencode-test-group-tool "call_g02_1" "shell" "completed")
                                                  (opencode-test-group-tool "call_g02_2" "shell" "completed")
                                                  '((type . "text") (text . "Answer 2.")))
                              (opencode-test-prompt "msg_g03u" "Question 3"))))
          (let ((fresh (opencode-test-fresh-look id)))
            (should (equal (nth 2 live) (nth 2 fresh)))
            (should (equal (opencode-test-before "Question 3" (nth 1 live))
                           (opencode-test-before "Question 3" (nth 1 fresh))))))))))

(defun opencode-test-before (marker text)
  "Return TEXT up to MARKER, a string it holds."
  (substring text 0 (string-search marker text)))

(ert-deftest opencode-test-group-catch-up-as-reload ()
  "Catching up after a reconnection groups as a buffer opened afresh does."
  (opencode-test-with-transcript (opencode-test-group-turn 1)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id)))
      (setq opencode-test-transcript
            (append opencode-test-transcript (opencode-test-group-turn 2) (opencode-test-group-turn 3 2)
                    (opencode-test-group-turn 4)))
      (opencode-test-reconnect)
      (with-current-buffer buffer
        (should (equal (mapcar #'cadr (opencode-test-groups)) '(3 3 2)))
        (should (equal (opencode-test-look) (opencode-test-fresh-look id)))))))

(ert-deftest opencode-test-group-revert-keeps-groups-and-point ()
  "Reverting draws the same groups, and a point at a group's summary stays there."
  (opencode-test-with-transcript (append (opencode-test-group-turn 1) (opencode-test-group-turn 2))
    (with-current-buffer (opencode-test-open (opencode-test-session))
      (let ((look (opencode-test-look)))
        (goto-char (overlay-start (opencode-test-group-at "3 blocks")))
        (forward-char 3)
        (let ((line (line-number-at-pos)))
          (revert-buffer)
          (should (equal (opencode-test-look) look))
          (should (= (line-number-at-pos) line))
          (should (opencode-test-group-at "3 blocks"))
          (should (eq (opencode-collapsible-at (point)) (opencode-test-group-at "3 blocks"))))))))

(ert-deftest opencode-test-group-expand-and-collapse ()
  "TAB on a group shows its blocks as they were; again, hides them, points coming back."
  (opencode-test-with-transcript (append (opencode-test-group-turn 1) (opencode-test-group-turn 2))
    (let ((buffer (opencode-test-open (opencode-test-session))))
      (with-current-buffer buffer
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (let ((group (opencode-test-group-at "3 blocks")))
            (goto-char (overlay-start group))
            (should-not (opencode-test-press buffer "TAB"))
            (should (overlay-get group 'opencode-expanded))
            (let ((visible (opencode-test-visible-text)))
              (should (string-search "Thinking about 1." visible))
              (should (string-search "README.md" visible))
              ;; The blocks show collapsed, as they were.
              (should-not (string-search "ok" (opencode-test-before "Answer 1." visible))))
            ;; A block of an open group expands and collapses as any.
            (goto-char (overlay-start (opencode-block "call_g01r")))
            (should-not (opencode-test-press buffer "TAB"))
            (should (overlay-get (opencode-block "call_g01r") 'opencode-expanded))
            ;; Collapsing the group brings a point it hides to its summary.
            (set-window-point (selected-window) (point))
            (opencode-collapse-block group)
            (should (= (point) (overlay-start group)))
            (should (= (window-point) (overlay-start group)))
            (should-not (string-search "README.md" (opencode-test-before "Answer 1." (opencode-test-visible-text))))
            ;; The expansion of its block hides with it.
            (should-not (string-search "ok" (opencode-test-before "Answer 1." (opencode-test-visible-text))))
            (should-not (opencode-test-press buffer "TAB"))
            (should (overlay-get group 'opencode-expanded))
            (should (string-search "ok" (opencode-test-before "Answer 1." (opencode-test-visible-text))))
            ;; A click on its box toggles it.
            (opencode-mouse-toggle-block
             (list 'mouse-1 (list (selected-window) (1+ (overlay-start group)) '(0 . 0) 0 nil
                                  (1+ (overlay-start group)) nil nil nil nil)))
            (should-not (overlay-get group 'opencode-expanded))
            ;; TAB elsewhere moves over the blocks it hides.
            (goto-char (opencode-test-find "Question 1"))
            (should-not (opencode-test-press buffer "TAB"))
            (should (= (point) (overlay-start group)))
            (goto-char (opencode-test-find "Answer 1."))
            (should-not (opencode-test-press buffer "TAB"))
            (should (= (point) (overlay-start (opencode-block (opencode-reasoning-key "msg_g02a" 0)))))))))))

(ert-deftest opencode-test-group-keeps-points ()
  "Grouping brings the points its blocks held to its summary, and leaves the others at their text."
  (opencode-test-with-transcript (opencode-test-group-turn 1)
    (let* ((id (opencode-test-session))
           (buffer (opencode-test-open id)))
      (with-current-buffer buffer
        (save-window-excursion
          (let* ((top (selected-window))
                 (bottom (split-window top)))
            (set-window-buffer top buffer)
            (set-window-buffer bottom buffer)
            (goto-char (+ 3 (opencode-test-find "README.md")))
            (set-window-point top (point))
            (set-window-point bottom (+ 2 (opencode-test-find "Answer 1.")))
            (set-window-start bottom (save-excursion (goto-char (overlay-start (opencode-block "call_g01s")))
                                                     (pos-bol))
                              t)
            (opencode-test-dispatch (opencode-test-inbox-events id "msg_g02u" "Question 2"))
            (let ((group (opencode-test-group-at "3 blocks")))
              (should (= (point) (overlay-start group)))
              (should (= (window-point top) (overlay-start group)))
              (should (= (window-point bottom) (+ 2 (opencode-test-find "Answer 1."))))
              (should (= (window-start bottom) (overlay-start group))))))))))

(ert-deftest opencode-test-group-waits-for-blocks-done ()
  "A block running or waiting on the user joins no group until done, then its turn groups again."
  (opencode-test-with-transcript (opencode-test-group-turn 1)
    (let* ((id (opencode-test-session))
           (step '((assistantMessageID . "msg_g02a"))))
      (with-current-buffer (opencode-test-open id)
        (opencode-test-dispatch
         (append
          (opencode-test-inbox-events id "msg_g02u" "Question 2")
          (list (opencode-test-event id "session.execution.started" "evt_g02s")
                (apply #'opencode-test-event id "session.step.started" "evt_g02b" step))
          (apply #'append
                 (mapcar (lambda (call)
                           (list (apply #'opencode-test-event id "session.tool.called" (format "evt_%s_c" call)
                                        `(id . ,call) '(name . "shell") '(input (command . "make check")) step)
                                 (apply #'opencode-test-event id "session.tool.success" (format "evt_%s_s" call)
                                        `(id . ,call) '(content ((type . "text") (text . "ok"))) step)))
                         '("call_g02_1" "call_g02_2")))
          (list (apply #'opencode-test-event id "session.tool.called" "evt_g02_3c"
                       '(id . "call_g02_3") '(name . "shell") '(input (command . "rm junk")) step)
                `((type . "permission.asked")
                  (data (id . "per_g02") (sessionID . ,id) (action . "shell") (resources "rm junk")
                        (source (type . "tool") (messageID . "msg_g02a") (id . "call_g02_3")))))))
        (should (plist-get (opencode-block-data (opencode-block "call_g02_3")) :waiting))
        ;; A prompt steering the turn while the call waits.
        (opencode-test-dispatch (opencode-test-inbox-events id "msg_g02v" "Steer"))
        (should (equal (mapcar #'cadr (opencode-test-groups)) '(3 2)))
        (should-not (overlay-get (opencode-block "call_g02_3") 'opencode-group))
        (should (string-search "rm junk" (opencode-test-visible-text)))
        ;; A request coming to a block of a group takes it out.
        (let ((member (opencode-block "call_g02_2")))
          (opencode-block-update member :waiting t)
          (should-not (overlay-get member 'opencode-group))
          (should (equal (mapcar #'cadr (opencode-test-groups)) '(3)))
          (opencode-block-update member :waiting nil)
          (should (equal (mapcar #'cadr (opencode-test-groups)) '(3 2))))
        (opencode-test-dispatch
         (list `((type . "permission.replied")
                 (data (sessionID . ,id) (requestID . "per_g02") (reply . "once")))
               (apply #'opencode-test-event id "session.tool.success" "evt_g02_3s"
                      '(id . "call_g02_3") '(content ((type . "text") (text . "ok"))) step)))
        (should (equal (mapcar #'cadr (opencode-test-groups)) '(3 3)))
        (should-not (string-search "rm junk" (opencode-test-visible-text)))))))

(ert-deftest opencode-test-group-expanded-stays-expanded ()
  "A group the user expanded stays expanded when its turn groups again."
  (opencode-test-with-transcript (append (opencode-test-group-turn 1 3) (opencode-test-group-turn 2))
    (with-current-buffer (opencode-test-open (opencode-test-session))
      (let ((group (opencode-test-group-at "3 blocks")))
        (opencode-expand-block group)
        (opencode-block-update (opencode-block "call_g01_3") :waiting t)
        (should (equal (opencode-test-groups) '((" 󰌨  2 blocks  2 shell" 2 t))))
        (opencode-block-update (opencode-block "call_g01_3") :waiting nil)
        (should (equal (opencode-test-groups) '((" 󰌨  3 blocks  3 shell" 3 t))))))))

(ert-deftest opencode-test-group-upgrades-earlier-look ()
  "A buffer an earlier version drew takes this version's look in place, as drawn afresh."
  (opencode-test-with-transcript (append (opencode-test-group-turn 1) (opencode-test-group-turn 2))
    (let ((id (opencode-test-session)))
      (with-current-buffer (opencode-test-open id)
        (let ((inhibit-read-only t))
          ;; As drawn before: no groups, the whole line highlighted, the
          ;; prompts highlighted and helping, the summaries unboxed.
          (dolist (group (seq-filter #'opencode-group-p (overlays-in (point-min) (point-max))))
            (opencode--dissolve-group group))
          (dolist (overlay (overlays-in (point-min) (point-max)))
            (when (overlay-get overlay 'opencode-collapsible)
              (overlay-put overlay 'mouse-face 'highlight)
              (with-silent-modifications
                (remove-list-of-text-properties (overlay-start overlay) (overlay-end overlay)
                                                '(mouse-face opencode-box)))
              (overlay-put overlay 'opencode-summary nil)))
          (let ((start (opencode-test-find "Question 1")))
            (with-silent-modifications
              (add-text-properties start (+ start 10) '(mouse-face highlight help-echo "edit")))))
        (goto-char (opencode-test-find "README.md"))
        (should-not (opencode-test-groups))
        (opencode--unhighlight-sent-prompts)
        (opencode-blocks-upgrade)
        (should (= (point) (overlay-start (opencode-test-group-at "3 blocks"))))
        (should-not (get-text-property (opencode-test-find "Question 1") 'mouse-face))
        (should-not (get-text-property (opencode-test-find "Question 1") 'help-echo))
        (should-not (seq-some (lambda (overlay) (overlay-get overlay 'mouse-face))
                              (overlays-in (point-min) (point-max))))
        (should (get-text-property (1+ (overlay-start (opencode-block "call_g02r"))) 'mouse-face))
        (let ((look (opencode-test-look)))
          (should (equal look (opencode-test-fresh-look id))))))))

(ert-deftest opencode-test-group-replay-cost ()
  "Grouping the earlier turns of a long transcript adds little to rendering it."
  (let ((transcript (opencode-test-long-transcript 300)))
    (cl-flet ((render-time ()
                (opencode-test-with-transcript transcript
                  (garbage-collect)
                  (let ((start (float-time)))
                    (with-current-buffer (opencode-test-open (opencode-test-session))
                      (prog1 (- (float-time) start)
                        (should (= (length (opencode-test-groups))
                                   (if (eq (symbol-function 'opencode-group-earlier-turn) #'ignore)
                                       0 299)))))))))
      (let* ((grouped (render-time))
             (ungrouped (cl-letf (((symbol-function 'opencode-group-earlier-turn) #'ignore))
                          (render-time))))
        (message "Rendering 300 turns: %.3fs grouped, %.3fs ungrouped" grouped ungrouped)
        (should (< grouped (* 1.5 (+ ungrouped 0.05))))))))

(provide 'opencode-groups-test)
;;; opencode-groups-test.el ends here
