;;; client.el --- Measure a stream reconnection in a GUI Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Loaded by run.sh into a GUI `emacs -Q' whose load path starts with
;; the opencode.el sources to measure.  It talks only to the fake
;; service of service.el, through $BENCH_ROOT/service.json, and never
;; runs the OpenCode CLI.  The scene stands for a heavy user's: moody
;; and minions in the mode line, FiraCode Nerd Font, the tron-legacy
;; theme, $BENCH_FRAMES frames showing session buffers of distinct
;; locations, a few of them split, and a list of every session drawn
;; with the displays of opencode-display.el as drover draws its own,
;; redrawn 0.1 s after the registry or a catalog changed.
;;
;; It measures the first load, then $BENCH_RECONNECTS reconnections of
;; the event stream, each from the stream's `server.connected' until
;; nothing is in flight for two seconds: the longest the main thread
;; went without running a 10 ms timer, the time it spent in gaps over
;; 50 ms, the redraws of every mode line forced, the mode-line updates
;; of one buffer, the catalog requests and announcements, and the
;; calls of `opencode-mode-line' with their cost.  It then measures a
;; steady redisplay, unforced and with every mode line forced.  Results
;; go to $BENCH_OUT as an alist.

;;; Code:

(require 'cl-lib)

(defvar bench-root (file-name-as-directory (getenv "BENCH_ROOT"))
  "Scratch directory of the benchmark, holding the service's registration.")
(defvar bench-out (getenv "BENCH_OUT")
  "File the results are written to.")
(defvar bench-frames (string-to-number (or (getenv "BENCH_FRAMES") "22"))
  "Number of frames; the last shows the list, three others are split.")
(defvar bench-reconnects (string-to-number (or (getenv "BENCH_RECONNECTS") "3"))
  "Number of reconnections measured.")
(defvar bench-builds (expand-file-name (or (getenv "BENCH_BUILDS") "~/.emacs.d/elpaca/builds"))
  "Directory of the packages dressing the mode line, one subdirectory each.")

;; Never the user's service, nor the CLI.
(setq opencode-service-registration-file (expand-file-name "service.json" bench-root)
      opencode-command "opencode-bench-missing-cli")

(dolist (package '("compat" "moody" "minions" "tron-legacy-theme"))
  (add-to-list 'load-path (expand-file-name package bench-builds) t))
(add-to-list 'custom-theme-load-path (expand-file-name "tron-legacy-theme" bench-builds))

(require 'opencode)

;;;; The scene

(defun bench-appearance ()
  "Dress the mode line, font and theme as the user does."
  (set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 120)
  (load-theme 'tron-legacy t)
  (setq-default mode-line-mule-info nil
                mode-line-frame-identification nil
                mode-line-position nil)
  (require 'moody)
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (require 'minions)
  (setq mode-line-modes-delimiters nil)
  (minions-mode))

(defvar bench-list-timer nil
  "Timer redrawing the list of sessions.")

(defun bench-list-draw ()
  "Draw every registry session as drover does, with the shared displays."
  (setq bench-list-timer nil)
  (with-current-buffer (get-buffer-create "*bench-list*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (entry (opencode-state-sessions))
        (let* ((directory (plist-get entry :directory))
               (model (plist-get entry :model))
               (id (plist-get entry :id)))
          (insert (format "%-50s %s %s %s %s %s\n"
                          (plist-get entry :title)
                          (or (opencode-agent-display (plist-get entry :agent) directory) "")
                          (or (opencode-model-display model directory) "")
                          (or (opencode-variant-display model directory) "")
                          (or (opencode-provider-display (alist-get 'providerID model) directory) "")
                          (or (opencode-context-display (opencode-state-context id) model directory) ""))))))))

(defun bench-list-changed (&rest _)
  "Redraw the list after 0.1 s without another change, as drover does."
  (when (timerp bench-list-timer) (cancel-timer bench-list-timer))
  (setq bench-list-timer (run-at-time 0.1 nil #'bench-list-draw)))

(add-hook 'opencode-state-updated-functions #'bench-list-changed)
(add-hook 'opencode-catalog-changed-functions #'bench-list-changed)

(defvar bench-results nil
  "Results measured so far, newest first.")

(defun bench-finish (&optional error)
  "Write the results, with ERROR when the run failed, and leave Emacs."
  (when error
    (push `(error . ,error) bench-results)
    (push `(counts . ,bench-counts) bench-results)
    (push `(messages . ,(with-current-buffer "*Messages*" (buffer-string))) bench-results))
  (with-temp-file bench-out
    (let ((print-length nil) (print-level nil))
      (pp (reverse bench-results) (current-buffer))))
  (kill-emacs 0))

(defun bench-when (predicate timeout what then)
  "Call THEN once PREDICATE holds, at most TIMEOUT seconds later, waiting for WHAT.
It polls from a timer, so that Emacs waits in its command loop, as it
does for a user, rather than in a `sit-for' nested in a timer."
  (let ((deadline (+ (float-time) timeout)))
    (cl-labels ((poll ()
                  (condition-case err
                      (cond ((funcall predicate) (funcall then))
                            ((> (float-time) deadline)
                             (bench-finish (format "Timed out waiting for %s; busy: %S" what (bench-busy-reasons))))
                            (t (run-at-time 0.05 nil #'poll)))
                    (error (bench-finish (error-message-string err))))))
      (run-at-time 0.05 nil #'poll))))

(defun bench-make-frames ()
  "Make the frames of the scene, while nothing else runs.
The Mac port can deadlock making a frame while processes and timers
run, so they are made first, showing nothing yet."
  (dotimes (_ (1- bench-frames))
    (make-frame '((width . 100) (height . 14)))))

(defun bench-frames ()
  "Return the frames of the scene, the initial one first."
  (cons (selected-frame) (remq (selected-frame) (reverse (frame-list)))))

(defun bench-scene (then)
  "Open frames showing session buffers of distinct locations and the list, then call THEN."
  (opencode-state-ensure)
  (bench-when
   #'opencode-state-ready-p 30 "the registry"
   (lambda ()
     (let* ((seen nil)
            (sessions (seq-filter (lambda (entry)
                                    (unless (member (plist-get entry :directory) seen)
                                      (push (plist-get entry :directory) seen)))
                                  (opencode-state-sessions)))
            (buffers nil)
            (wanted (+ bench-frames 2)))
       (dolist (entry (seq-take sessions wanted))
         (opencode-session-open-by-id (plist-get entry :id) :no-display t
                                      :callback (lambda (_) (push (current-buffer) buffers))))
       (bench-when
        (lambda () (>= (length buffers) wanted)) 60 "the session buffers"
        (lambda ()
          (setq buffers (nreverse buffers))
          (let ((frames (bench-frames)))
            ;; The last frame shows the list; three others are split in two.
            (cl-loop for frame in (butlast frames)
                     for index from 0
                     do (let ((window (frame-root-window frame)))
                          (delete-other-windows window)
                          (set-window-buffer window (pop buffers))
                          (when (< index 3)
                            (set-window-buffer (split-window window nil 'right) (pop buffers)))))
            (set-window-buffer (frame-root-window (car (last frames))) (get-buffer-create "*bench-list*"))
            (bench-list-changed))
          (funcall then)))))))

;;;; Measures

(defvar bench-counts nil
  "Alist of the counters of the measure running.")

(defun bench-count (key &optional amount)
  "Add AMOUNT, by default 1, to counter KEY."
  (setf (alist-get key bench-counts) (+ (or (alist-get key bench-counts) 0) (or amount 1))))

(defvar bench-last-tick nil
  "Time of the previous tick of the 10 ms timer.")

(defun bench-tick ()
  "Note the gap since the previous tick of this 10 ms timer."
  (let ((now (float-time)))
    (when bench-last-tick
      (let ((gap (- now bench-last-tick)))
        (setf (alist-get 'max-gap bench-counts) (max gap (or (alist-get 'max-gap bench-counts) 0)))
        (when (> gap 0.05)
          (bench-count 'blocked gap))))
    (setq bench-last-tick now))
  (when (bench-busy-p)
    (setf (alist-get 'last-busy bench-counts) (float-time))))

(advice-add 'force-mode-line-update :before
            (lambda (&optional all) (bench-count (if all 'forced-all 'forced-one))))

(advice-add 'opencode-mode-line :around
            (lambda (function &rest arguments)
              (let ((start (float-time)))
                (prog1 (apply function arguments)
                  (bench-count 'mode-line-calls)
                  (bench-count 'mode-line-seconds (- (float-time) start))))))

(advice-add 'opencode-request :before
            (lambda (_method path &rest _)
              (when (member path '("/api/agent" "/api/model" "/api/provider" "/api/command"))
                (bench-count 'catalog-requests))))

(add-hook 'opencode-catalog-changed-functions (lambda (&rest _) (bench-count 'catalog-announcements)))

(when (getenv "BENCH_LOG")
  (advice-add 'opencode-request :around
              (lambda (function method path &rest arguments)
                (let ((then (plist-get arguments :then))
                      (else (plist-get arguments :else))
                      (start (float-time)))
                  (message "%.3f start %s %s" start method path)
                  (apply function method path
                         (plist-put (plist-put (copy-sequence arguments)
                                               :then (lambda (&rest values)
                                                       (message "%.3f done %s" (float-time) path)
                                                       (when then (apply then values))))
                                    :else (lambda (&rest values)
                                            (message "%.3f failed %s %S" (float-time) path values)
                                            (if else (apply else values)
                                              (opencode-report-error
                                               (opencode-error-describe (car values)))))))))))

(defvar bench-connected nil
  "Time the last `server.connected' was handled.")

(puthash "server.connected"
         (cons (lambda (_) (setq bench-connected (float-time)))
               (gethash "server.connected" opencode-events--handlers))
         opencode-events--handlers)

(defun bench-busy-reasons ()
  "Return what is busy: requests running, the registry, or the list."
  (delq nil
        (list (and (seq-some (lambda (process)
                               (and (process-live-p process)
                                    (eq (process-type process) 'real)
                                    (not (eq process opencode-events--process))
                                    (string-prefix-p "plz-request-curl" (process-name process))))
                             (process-list))
                   'requests)
              (and (not (opencode-state-ready-p)) 'registry)
              (and (timerp bench-list-timer) 'list))))

(defun bench-busy-p ()
  "Return non-nil while requests run or results wait to be drawn."
  (bench-busy-reasons))

(defun bench-measure (name start-function then)
  "Measure NAME: call START-FUNCTION with a continuation, settle, then call THEN."
  (setq bench-counts nil
        bench-connected nil)
  (let ((start (float-time)))
    (funcall
     start-function
     (lambda ()
       (setf (alist-get 'last-busy bench-counts) (float-time))
       (bench-when
        (lambda () (> (- (float-time) (alist-get 'last-busy bench-counts)) 2)) 120 "quiet"
        (lambda ()
          (push `(,name
                  (settle-seconds . ,(- (alist-get 'last-busy bench-counts) (or bench-connected start)))
                  ,@(seq-remove (lambda (pair) (eq (car pair) 'last-busy)) bench-counts))
                bench-results)
          (garbage-collect)
          (funcall then)))))))

(defun bench-reconnect (then)
  "Drop the stream as a lost connection does, and call THEN once it reopened after its backoff."
  (delete-process opencode-events--process)
  (bench-when (lambda () (and bench-connected (opencode-events-connected-p))) 30 "the stream" then))

(defun bench-redisplay (force)
  "Return the seconds a redisplay takes, every mode line forced when FORCE."
  (setq bench-counts nil)
  (let ((runs 20)
        (total 0))
    (redisplay t)
    (dotimes (_ runs)
      (when force (force-mode-line-update t))
      (let ((start (float-time)))
        (redisplay t)
        (cl-incf total (- (float-time) start))))
    `((seconds . ,(/ total runs))
      (mode-line-calls . ,(/ (or (alist-get 'mode-line-calls bench-counts) 0) (float runs)))
      (mode-line-seconds . ,(/ (or (alist-get 'mode-line-seconds bench-counts) 0) runs)))))

(defun bench-parts ()
  "Return the microseconds each part of `opencode-mode-line' takes, in one session window."
  (let* ((window (seq-find (lambda (window)
                             (with-current-buffer (window-buffer window)
                               (derived-mode-p 'opencode-session-mode)))
                           (window-list-1 nil 'never t)))
         (runs 500))
    (with-selected-window window
      (cl-flet ((time (function)
                  (funcall function)
                  (let ((start (float-time)))
                    (dotimes (_ runs) (funcall function))
                    (/ (* 1e6 (- (float-time) start)) runs))))
        `((whole . ,(time #'opencode-mode-line))
          (status . ,(time #'opencode-mode-line-status))
          (pending . ,(time #'opencode-pending-request-p))
          (rest-width . ,(let ((rest (format-mode-line (remove opencode-mode-line-element (opencode-mode-line-format)) t)))
                           (time (lambda () (opencode-mode-line-pixel-width rest)))))
          (segments . ,(time #'opencode-mode-line-segments))
          (format . ,(time #'opencode-mode-line-format))
          (rest . ,(time (lambda ()
                           (format-mode-line (remove opencode-mode-line-element (opencode-mode-line-format)) t))))
          (tab . ,(time (lambda () (format-mode-line (opencode-mode-line-tab "x ") t))))
          (whole-line . ,(time (lambda () (format-mode-line mode-line-format nil window)))))))))

(defun bench-run ()
  "Run the benchmark and write its results."
  (bench-appearance)
  (bench-make-frames)
  (let ((tick (run-at-time 0 0.01 #'bench-tick))
        (reconnects 0))
    (cl-labels ((reconnect ()
                  (if (< reconnects bench-reconnects)
                      (bench-measure (intern (format "reconnect-%d" (cl-incf reconnects)))
                                     #'bench-reconnect #'reconnect)
                    (cancel-timer tick)
                    (push `(redisplay-unforced ,@(bench-redisplay nil)) bench-results)
                    (push `(redisplay-forced ,@(bench-redisplay t)) bench-results)
                    (push `(mode-line-parts-us ,@(bench-parts)) bench-results)
                    (push `(windows . ,(length (seq-mapcat (lambda (frame) (window-list frame 'never))
                                                           (frame-list))))
                          bench-results)
                    (push `(frames . ,(length (frame-list))) bench-results)
                    (bench-finish))))
      (bench-measure 'first-load #'bench-scene #'reconnect))))

(add-hook 'window-setup-hook (lambda () (run-at-time 0.5 nil #'bench-run)))

;; Whatever stalls the run, its results and messages are written.
(run-at-time (string-to-number (or (getenv "BENCH_DEADLINE") "400")) nil
             (lambda () (bench-finish (format "Deadline passed; busy: %S" (bench-busy-reasons)))))

;;; client.el ends here
