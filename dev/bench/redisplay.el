;;; redisplay.el --- Time what a session window costs redisplay  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; dev/bench/redisplay.sh runs this in a graphical `emacs -Q', launched
;; hidden on macOS, dressed as the user's (FiraCode Nerd Font,
;; tron-legacy, moody and minions): a frame of 240 columns, a session
;; window holding a long transcript, its point at the end, and a side
;; window of 50 lines beside it, as drover's sidebar.  It times
;; redisplays, each preceded by one way of asking for the session's mode
;; line, alone or with a text property of the side window changed in the
;; same redisplay, as the sidebar's animation does; then a whole layout
;; of the session window, as its settings and parsers change it; then the
;; ticks of the animated mode line over 5 s, with focus and without.
;; Results go to $BENCH_OUT.

;;; Code:

(require 'opencode-test-helpers)

(defvar redisplay-bench-output (getenv "BENCH_OUT")
  "File the results go to.")

(defvar redisplay-bench-turns (string-to-number (or (getenv "BENCH_TURNS") "1000"))
  "Turns of the transcript.")

(defvar redisplay-bench-runs 100
  "Redisplays each measure averages.")

(defun redisplay-bench-log (format-string &rest arguments)
  "Write FORMAT-STRING with ARGUMENTS as a line of the results."
  (let ((line (apply #'format format-string arguments)))
    (with-temp-buffer
      (insert line "\n")
      (append-to-file (point-min) (point-max) redisplay-bench-output))))

(defun redisplay-bench-appearance ()
  "Dress the frame as the user does."
  (let ((builds (expand-file-name "~/.emacs.d/elpaca/builds")))
    (dolist (package '("moody" "minions" "tron-legacy-theme"))
      (add-to-list 'load-path (expand-file-name package builds) t))
    (add-to-list 'custom-theme-load-path (expand-file-name "tron-legacy-theme" builds)))
  (setq gc-cons-threshold (* 1024 1024 1024)
        gc-cons-percentage 0.5
        frame-title-format nil)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 120)
  (load-theme 'tron-legacy t)
  (require 'moody)
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (require 'minions)
  (setq mode-line-modes-delimiters nil)
  (minions-mode))

(defun redisplay-bench-transcript ()
  "Return the transcript: `opencode-test-long-transcript' with answers in prose.
Each answer gains a paragraph of a few hundred characters with inline
code and emphasis, as an agent's answers read."
  (let ((prose (mapconcat (lambda (n)
                            (format "Sentence %d reads `opencode--mode-line-tick` and **the window** it updates, then the *next* one." n))
                          (number-sequence 1 6) " ")))
    (mapcar (lambda (message)
              (if (equal (alist-get 'type message) "assistant")
                  (let ((content (mapcar (lambda (part)
                                           (if (equal (alist-get 'type part) "text")
                                               `((type . "text") (text . ,(concat (alist-get 'text part) "\n\n" prose)))
                                             part))
                                         (alist-get 'content message))))
                    (cons (cons 'content content) (assq-delete-all 'content (copy-sequence message))))
                message))
            (opencode-test-long-transcript redisplay-bench-turns))))

(defun redisplay-bench-time (function)
  "Return the milliseconds FUNCTION and the redisplay after it take, averaged."
  (redisplay t)
  (garbage-collect)
  (let ((start (float-time)))
    (dotimes (_ redisplay-bench-runs)
      (funcall function)
      (redisplay t))
    (/ (* 1000 (- (float-time) start)) redisplay-bench-runs)))

(defvar redisplay-bench-side nil
  "Buffer of the side window.")

(defun redisplay-bench-side-change ()
  "Change a text property in the side window, as a frame of the sidebar does."
  (with-current-buffer redisplay-bench-side
    (let ((inhibit-read-only t))
      (with-silent-modifications
        (put-text-property 3 4 'face (if (eq (get-text-property 3 'face) 'bold) 'italic 'bold))))))

(defun redisplay-bench-ask (session way)
  "Return a function asking for SESSION's mode line the WAY it names."
  (pcase way
    ('nothing #'ignore)
    ('buffer (lambda () (with-current-buffer session (force-mode-line-update))))
    ('modified (lambda () (with-current-buffer session (set-buffer-modified-p (buffer-modified-p)))))
    ('all-hidden (lambda () (with-temp-buffer (force-mode-line-update t))))
    ('tick (lambda () (opencode--mode-line-tick)))))

(defun redisplay-bench-ways (session label)
  "Log what each way of asking for SESSION's mode line costs, under LABEL."
  (dolist (way '(nothing buffer modified all-hidden tick))
    (let ((ask (redisplay-bench-ask session way)))
      (redisplay-bench-log "%-28s %-10s alone %6.3f ms | with the side window changed %6.3f ms"
                           label way
                           (redisplay-bench-time ask)
                           (redisplay-bench-time (lambda ()
                                                   (funcall ask)
                                                   (redisplay-bench-side-change)))))))

(defun redisplay-bench-layout (session)
  "Return the milliseconds a whole layout of SESSION's window takes."
  (let ((window (get-buffer-window session)))
    (redisplay-bench-time (lambda () (force-window-update window)))))

(defun redisplay-bench-run ()
  "Run the benchmark and exit."
  ;; The user works on: the frame takes no focus.
  (modify-frame-parameters nil '((no-accept-focus . t) (no-focus-on-map . t)))
  (redisplay-bench-appearance)
  (set-frame-size nil 240 66)
  (setq redisplay-bench-side (get-buffer-create "*side*"))
  (with-current-buffer redisplay-bench-side
    (dotimes (line 50)
      (insert (format "%2d  session %d of the sidebar, a title\n" line line))))
  (opencode-test-with-transcript (redisplay-bench-transcript)
				 (let* ((session (opencode-test-open (opencode-test-session)))
					(main (selected-window)))
				   (delete-other-windows)
				   (set-window-buffer main session)
				   (with-current-buffer session
				     (goto-char (point-max))
				     (set-window-point main (point-max)))
				   (let ((side (split-window main -36 'right)))
				     (set-window-buffer side redisplay-bench-side))
				   (dotimes (_ 5) (redisplay t))
				   ;; Timers deleting parsers, if any, run.
				   (sit-for 0.5)
				   (redisplay t)
				   (with-current-buffer session
				     (setq opencode-session-status 'busy)
				     (force-mode-line-update))
				   (redisplay t)
				   (redisplay-bench-log "transcript: %d turns, %d characters, %d lines, %d overlays, %d parsers; window %dx%d; paragraphs %s, bracket pairing %s"
							redisplay-bench-turns (buffer-size session)
							(with-current-buffer session (count-lines (point-min) (point-max)))
							(with-current-buffer session (length (overlays-in (point-min) (point-max))))
							(with-current-buffer session (length (treesit-parser-list nil nil t)))
							(window-width main) (window-height main)
							(buffer-local-value 'bidi-paragraph-direction session)
							(if (buffer-local-value 'bidi-inhibit-bpa session) "off" "on"))
				   (dolist (window (window-list))
				     (let ((format (buffer-local-value 'mode-line-format (window-buffer window))))
				       (redisplay-bench-log "mode line of %s: %.3f ms" (window-buffer window)
							    (let ((start (float-time)))
							      (dotimes (_ 100) (format-mode-line format nil window))
							      (/ (* 1000 (- (float-time) start)) 100)))))
				   (redisplay-bench-ways session "session selected")
				   (with-selected-window (next-window main)
				     (redisplay-bench-ways session "side selected"))
				   (redisplay-bench-log "whole layout of the session window, as set up: %.3f ms"
							(redisplay-bench-layout session))
				   (let ((scroll (buffer-local-value 'window-scroll-functions session))
					 (direction (buffer-local-value 'bidi-paragraph-direction session))
					 (pairing (buffer-local-value 'bidi-inhibit-bpa session)))
				     (cl-flet ((set (functions paragraphs no-pairing)
						 (with-current-buffer session
						   (setq window-scroll-functions functions
							 bidi-paragraph-direction paragraphs
							 bidi-inhibit-bpa no-pairing))))
				       (set scroll nil nil)
				       (redisplay-bench-log "  paragraph direction looked for: %.3f ms" (redisplay-bench-layout session))
				       (set scroll 'left-to-right t)
				       (redisplay-bench-log "  left to right, no pairing:      %.3f ms" (redisplay-bench-layout session))
				       (set nil nil nil)
				       (redisplay-bench-ways session "no window-scroll-functions")
				       (set nil 'left-to-right t)
				       (redisplay-bench-ways session "and left to right")
				       (set scroll nil nil)
				       (with-current-buffer session
					 (opencode-delete-parsers (point-min) (point-max)))
				       (redisplay-bench-log "  every parser deleted:           %.3f ms" (redisplay-bench-layout session))
				       (set scroll 'left-to-right t)
				       (redisplay-bench-log "  and left to right:              %.3f ms" (redisplay-bench-layout session))
				       (set scroll direction pairing)))
				   (redisplay-bench-live session t)
				   (redisplay-bench-live session nil)
				   (with-current-buffer session
				     (setq opencode-session-status 'idle))
				   (opencode--mode-line-stop-animation))))

(defun redisplay-bench-cpu ()
  "Return the processor time Emacs used so far, in seconds."
  (let ((time (current-cpu-time)))
    (/ (float (car time)) (cdr time))))

(defvar redisplay-bench-ticks 0
  "Ticks of the mode-line animation counted.")

(defun redisplay-bench-count (&rest _)
  "Count a tick of the mode-line animation."
  (setq redisplay-bench-ticks (1+ redisplay-bench-ticks)))

(defun redisplay-bench-live (session focused)
  "Log the ticks and the processor time of SESSION's animated mode line over 5 s.
Emacs has focus or not as FOCUSED says, as `frame-focus-state' tells."
  (setq redisplay-bench-ticks 0)
  (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _frame) focused)))
    (when (fboundp 'opencode--mode-line-refocus)
      (opencode--mode-line-refocus))
    (with-current-buffer session
      (force-mode-line-update))
    (redisplay t)
    (advice-add 'opencode--mode-line-tick :before #'redisplay-bench-count)
    (unwind-protect
        (let ((cpu (redisplay-bench-cpu))
              (start (float-time)))
          (while (< (float-time) (+ start 5))
            (sit-for 0.05))
          (redisplay-bench-log "live, %s: %d ticks in 5 s, processor %.2f%%"
                               (if focused "focused" "unfocused") redisplay-bench-ticks
                               (/ (* 100 (- (redisplay-bench-cpu) cpu)) (- (float-time) start))))
      (advice-remove 'opencode--mode-line-tick #'redisplay-bench-count))))

(add-hook 'window-setup-hook
          (lambda ()
            (run-at-time 0.5 nil
                         (lambda ()
                           (condition-case err
                               (redisplay-bench-run)
                             (error (redisplay-bench-log "error: %S" err)))
                           (kill-emacs 0)))))

;;; redisplay.el ends here
