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
;; theme, the user's garbage collection thresholds, $BENCH_OPEN session
;; buffers of distinct locations with long transcripts, as many as
;; $BENCH_FRAMES frames have windows shown in them, a few frames split,
;; the others hidden, and a list of every session drawn with the
;; displays of opencode-display.el as drover draws its own, redrawn
;; 0.1 s after the registry or a catalog changed.
;;
;; It measures the first load, then $BENCH_RECONNECTS reconnections of
;; the event stream.  Before each, with the stream down, the service
;; adds a turn to half of the open sessions and starts or ends one in
;; another, as agents working while Emacs is disconnected do.  Each is
;; measured from the stream's `server.connected' until nothing is in
;; flight for two seconds: the longest the main thread went without
;; running a 10 ms timer, the time it spent in gaps over 50 and over
;; 100 ms, the garbage collections, the transcript requests by kind,
;; the redraws of every mode line forced, the mode-line updates of one
;; buffer, the catalog requests and announcements, the reports of
;; locations not found, and the calls of `opencode-mode-line' with their
;; cost.  It then checks that every open buffer shows what a buffer
;; rendering its whole transcript afresh shows, and measures a steady
;; redisplay, unforced and with every mode line forced, and what the
;; animated status icon of a running turn costs per tick, in one shown
;; session buffer, then in all of them.  Results go to $BENCH_OUT as an
;; alist.

;;; Code:

(require 'cl-lib)

(defvar bench-root (file-name-as-directory (getenv "BENCH_ROOT"))
  "Scratch directory of the benchmark, holding the service's registration.")
(defvar bench-out (getenv "BENCH_OUT")
  "File the results are written to.")
(defvar bench-frames (string-to-number (or (getenv "BENCH_FRAMES") "22"))
  "Number of frames; the last shows the list, three others are split.")
(defvar bench-open (string-to-number (or (getenv "BENCH_OPEN") "26"))
  "Number of session buffers open.")
(defvar bench-reconnects (string-to-number (or (getenv "BENCH_RECONNECTS") "3"))
  "Number of reconnections measured.")
(defvar bench-builds (expand-file-name (or (getenv "BENCH_BUILDS") "~/.emacs.d/elpaca/builds"))
  "Directory of the packages dressing the mode line, one subdirectory each.")

;; As the user's configuration has them: collections are rare.
(setq gc-cons-threshold (string-to-number (or (getenv "BENCH_GC_THRESHOLD") "104857600"))
      gc-cons-percentage 0.5)

;; Never the user's service, nor the CLI.
(setq opencode-service-registration-file (expand-file-name "service.json" bench-root)
      opencode-command "opencode-bench-missing-cli")

(dolist (package '("compat" "moody" "minions" "tron-legacy-theme"))
  (add-to-list 'load-path (expand-file-name package bench-builds) t))
(add-to-list 'custom-theme-load-path (expand-file-name "tron-legacy-theme" bench-builds))

(require 'opencode)

(when (getenv "BENCH_SLICE")
  (setq opencode-replay-slice (string-to-number (getenv "BENCH_SLICE"))))

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

(defvar bench-buffers nil
  "The session buffers open, in the order their windows were made.")

(defun bench-scene (then)
  "Open session buffers of distinct locations, show them in frames with the list, then call THEN.
Those that no window shows stay open, hidden."
  (opencode-state-ensure)
  (bench-when
   #'opencode-state-ready-p 30 "the registry"
   (lambda ()
     (let* ((seen nil)
            (sessions (seq-filter (lambda (entry)
                                    (unless (member (plist-get entry :directory) seen)
                                      (push (plist-get entry :directory) seen)))
                                  (opencode-state-sessions)))
            (buffers nil))
       (dolist (entry (seq-take sessions bench-open))
         (opencode-session-open-by-id (plist-get entry :id) :no-display t
                                      :callback (lambda (_) (push (current-buffer) buffers))))
       (bench-when
        (lambda () (>= (length buffers) bench-open)) 300 "the session buffers"
        (lambda ()
          (setq buffers (sort buffers :key #'buffer-name)
                bench-buffers (copy-sequence buffers))
          (let ((frames (bench-frames)))
            ;; The last frame shows the list; three others are split in two.
            (cl-loop for frame in (butlast frames)
                     for index from 0
                     do (let ((window (frame-root-window frame)))
                          (delete-other-windows window)
                          (when buffers (set-window-buffer window (pop buffers)))
                          (when (and (< index 3) buffers)
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
          (bench-count 'blocked gap))
        (when (> gap 0.1)
          (bench-count 'blocks-over-100ms)
          (bench-count 'blocked-over-100ms gap))))
    (setq bench-last-tick now))
  (when (bench-busy-p)
    (setf (alist-get 'last-busy bench-counts) (float-time))))

(advice-add 'force-mode-line-update :before
            (lambda (&optional all) (bench-count (if all 'forced-all 'forced-one))))

(when (getenv "BENCH_TRACE")
  (advice-add 'force-mode-line-update :before
              (lambda (&rest _)
                (let* ((frames (backtrace-frames))
                       (callers (seq-take (seq-keep (lambda (frame)
                                                      (let ((function (cadr frame)))
                                                        (and (symbolp function)
                                                             (string-prefix-p "opencode" (symbol-name function))
                                                             function)))
                                                    frames)
                                          3))
                       (forced (alist-get 'forced-by bench-counts)))
                  (setf (alist-get callers forced nil nil #'equal) (1+ (alist-get callers forced 0 nil #'equal)))
                  (setf (alist-get 'forced-by bench-counts) forced)))))

;; BENCH_TIME names functions whose longest and total times are counted.
(dolist (name (split-string (or (getenv "BENCH_TIME") "") "[ ,]+" t))
  (let ((function (intern name)))
    (advice-add function :around
                (lambda (original &rest arguments)
                  (let ((start (float-time)))
                    (prog1 (apply original arguments)
                      (let ((seconds (- (float-time) start))
                            (timed (alist-get 'timed bench-counts)))
                        (setf (alist-get function timed)
                              (list (max seconds (or (car (alist-get function timed)) 0))
                                    (+ seconds (or (cadr (alist-get function timed)) 0))))
                        (setf (alist-get 'timed bench-counts) timed))))))))

(advice-add 'opencode-mode-line :around
            (lambda (function &rest arguments)
              (let ((start (float-time)))
                (prog1 (apply function arguments)
                  (bench-count 'mode-line-calls)
                  (bench-count 'mode-line-seconds (- (float-time) start))))))

(advice-add 'opencode-request :before
            (lambda (_method path &rest arguments)
              (cond
               ((member path '("/api/agent" "/api/model" "/api/provider" "/api/command"))
                (bench-count 'catalog-requests))
               ((and (string-suffix-p "/message" path)
                     ;; The registry's context sizes ask for one type.
                     (not (alist-get 'type (plist-get arguments :query))))
                (let ((query (plist-get arguments :query)))
                  (bench-count (cond ((equal (alist-get 'order query) "asc") 'transcripts-whole)
                                     ((alist-get 'cursor query) 'transcript-pages-more)
                                     (t 'transcript-pages-newest))))))))

(advice-add 'opencode-report-error :before
            (lambda (text)
              (when (string-match-p "Location not found" text)
                (bench-count 'location-reports))))

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

(defun bench-stream ()
  "Return the event stream: an exchange, or the curl process of older sources."
  (or (bound-and-true-p opencode-events--stream) (bound-and-true-p opencode-events--process)))

(defun bench-busy-reasons ()
  "Return what is busy: requests running, the registry, the list, or buffers catching up."
  (delq nil
        (list (and (seq-some (lambda (process)
                               (and (process-live-p process)
                                    (if-let* ((exchange (process-get process 'opencode-transport-exchange)))
                                        (not (eq exchange (bench-stream)))
                                      ;; Sources from before requests had their own sockets.
                                      (and (eq (process-type process) 'real)
                                           (not (eq process (bench-stream)))
                                           (string-prefix-p "plz-request-curl" (process-name process))))))
                             (process-list))
                   'requests)
              (and (not (opencode-state-ready-p)) 'registry)
              (and (timerp bench-list-timer) 'list)
              ;; Absent from versions that reload transcripts whole.
              (and (or (bound-and-true-p opencode--resync-queue)
                       (bound-and-true-p opencode--resync-running)
                       (seq-some (lambda (buffer)
                                   (and (buffer-live-p buffer)
                                        (or (buffer-local-value 'opencode--holding buffer)
                                            (buffer-local-value 'opencode--loading buffer))))
                                 (and (boundp 'opencode--holding) bench-buffers)))
                   'catching-up))))

(defun bench-busy-p ()
  "Return non-nil while requests run or results wait to be drawn."
  (bench-busy-reasons))

(defun bench-measure (name start-function then)
  "Measure NAME: call START-FUNCTION with a continuation, settle, then call THEN.
With BENCH_PROFILE, a CPU profile of each measure is written next to the results."
  (setq bench-counts nil
        bench-connected nil)
  (when (getenv "BENCH_PROFILE")
    (profiler-start 'cpu))
  (let ((start (float-time))
        (gcs gcs-done)
        (gc-seconds gc-elapsed))
    (funcall
     start-function
     (lambda ()
       (setf (alist-get 'last-busy bench-counts) (float-time))
       (bench-when
        (lambda () (> (- (float-time) (alist-get 'last-busy bench-counts)) 2)) 300 "quiet"
        (lambda ()
          (when (getenv "BENCH_PROFILE")
            (profiler-stop)
            (profiler-write-profile (profiler-cpu-profile) (format "%s.%s.prof" bench-out name)))
          (push `(,name
                  (settle-seconds . ,(- (alist-get 'last-busy bench-counts) (or bench-connected start)))
                  (garbage-collections . ,(- gcs-done gcs))
                  (garbage-collection-seconds . ,(- gc-elapsed gc-seconds))
                  ;; Copied, as counting goes on in the same cells.
                  ,@(copy-tree (seq-remove (lambda (pair) (eq (car pair) 'last-busy)) bench-counts)))
                bench-results)
          (setq bench-counts nil)
          (garbage-collect)
          (funcall then)))))))

(defun bench-reconnect (then)
  "Drop the stream as a lost connection does, and call THEN once it reopened after its backoff.
While it is down, agents work: the service adds turns to sessions."
  (let ((stream (bench-stream)))
    (delete-process (if (processp stream) stream (opencode-transport-exchange-connection stream))))
  (opencode-request-sync 'post "/api/bench/activity" :directory nil :timeout 10)
  (setq bench-counts nil
        bench-last-tick nil)
  (bench-when (lambda () (and bench-connected (opencode-events-connected-p))) 30 "the stream" then))

;;;; Checks

(defun bench-transcript-text (buffer)
  "Return the transcript BUFFER shows, without its idle prompt or the input."
  (with-current-buffer buffer
    (let ((end (or (opencode-idle-start) (opencode-pmark))))
      (string-trim-right (buffer-substring-no-properties (point-min) end)))))

(defun bench-fetch-transcript (id)
  "Return the whole transcript of session ID, oldest first, waiting for it."
  (let ((messages nil)
        (cursor nil)
        (done nil))
    (while (not done)
      (let* ((response (opencode-request-sync
                        'get (format "/api/session/%s/message" id) :directory nil :timeout 30
                        :query `((limit . 200) ,@(if cursor `((cursor . ,cursor)) '((order . "asc"))))))
             (data (alist-get 'data response)))
        (setq messages (append messages data)
              cursor (alist-get 'next (alist-get 'cursor response))
              done (or (null data) (null cursor)))))
    messages))

(defun bench-check ()
  "Return how many open buffers show other than a whole transcript rendered afresh.
The value is an alist: the count, and the IDs of those that differ."
  (let ((different nil)
        (opencode-replay-slice nil))
    (dolist (buffer bench-buffers)
      (when (buffer-live-p buffer)
        (let* ((id (buffer-local-value 'opencode-session-id buffer))
               (directory (buffer-local-value 'default-directory buffer))
               (fresh (with-temp-buffer
                        (setq default-directory directory)
                        (opencode-session-mode)
                        (opencode--make-process (current-buffer))
                        (setq opencode-session-id id)
                        (opencode-replay-messages (bench-fetch-transcript id))
                        (bench-transcript-text (current-buffer)))))
          (unless (equal fresh (bench-transcript-text buffer))
            (push id different)))))
    `((buffers . ,(length bench-buffers)) (different . ,(length different)) (ids . ,different))))

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
          (status-animated . ,(let ((opencode-session-status 'busy))
                                (time #'opencode-mode-line-status)))
          (whole-animated . ,(let ((opencode-session-status 'busy))
                               (prog1 (time #'opencode-mode-line)
                                 (opencode--mode-line-stop-animation))))
          (pending . ,(time #'opencode-pending-request-p))
          (rest-width . ,(let ((rest (format-mode-line (remove opencode-mode-line-element (opencode-mode-line-format)) t)))
                           (time (lambda () (opencode-mode-line-pixel-width rest)))))
          (segments . ,(time #'opencode-mode-line-segments))
          (format . ,(time #'opencode-mode-line-format))
          (rest . ,(time (lambda ()
                           (format-mode-line (remove opencode-mode-line-element (opencode-mode-line-format)) t))))
          (tab . ,(time (lambda () (format-mode-line (opencode-mode-line-tab "x ") t))))
          (whole-line . ,(time (lambda () (format-mode-line mode-line-format nil window)))))))))

(defun bench-animation (count)
  "Return what animating the icons of COUNT shown session buffers costs per tick.
Their sessions are made to run a turn, so that their mode lines animate.
The value is an alist: the windows and frames showing them; per tick,
the seconds of the tick itself, of the redisplay it causes, and the
calls of `opencode-mode-line' with their seconds, measured over 40
ticks each followed by a redisplay; then the ticks of the timer over 3
seconds left alone, the longest gap of a 10 ms timer and the mode-line
calls meanwhile.  Their sessions are idle again after."
  (let* ((buffers (seq-take (seq-filter (lambda (buffer)
                                          (and (buffer-live-p buffer) (get-buffer-window buffer 'visible)))
                                        bench-buffers)
                            count))
         (windows (seq-mapcat (lambda (buffer) (get-buffer-window-list buffer nil 'visible)) buffers))
         (runs 40)
         (ticks 0)
         (tick-seconds 0)
         (redisplay-seconds 0))
    (opencode--mode-line-stop-animation)
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (setq opencode-session-status 'busy)
        (force-mode-line-update)))
    (redisplay t)
    (setq bench-counts nil)
    (dotimes (_ runs)
      (let ((start (float-time)))
        (opencode--mode-line-tick)
        (cl-incf tick-seconds (- (float-time) start)))
      (let ((start (float-time)))
        (redisplay t)
        (cl-incf redisplay-seconds (- (float-time) start))))
    (let ((calls (or (alist-get 'mode-line-calls bench-counts) 0))
          (seconds (or (alist-get 'mode-line-seconds bench-counts) 0))
          (live nil))
      ;; Then the timer alone, as the user has it, with input pending
      ;; nowhere: how often it ticked and what it held the main thread.
      (setq bench-counts nil
            bench-last-tick nil)
      (let ((count-ticks (lambda (&rest _) (cl-incf ticks)))
            (gap-timer (run-at-time 0 0.01 #'bench-tick))
            (deadline (+ (float-time) 3)))
        (advice-add 'opencode--mode-line-tick :before count-ticks)
        (unwind-protect
            (while (< (float-time) deadline)
              (sit-for 0.05))
          (advice-remove 'opencode--mode-line-tick count-ticks)
          (cancel-timer gap-timer))
        (setq live `((ticks . ,ticks)
                     (max-gap-seconds . ,(alist-get 'max-gap bench-counts))
                     (mode-line-calls . ,(or (alist-get 'mode-line-calls bench-counts) 0))
                     (mode-line-seconds . ,(or (alist-get 'mode-line-seconds bench-counts) 0)))))
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (setq opencode-session-status 'idle)
          (force-mode-line-update)))
      (redisplay t)
      `((buffers . ,(length buffers))
        (windows . ,(length windows))
        (frames . ,(length (delete-dups (mapcar #'window-frame windows))))
        (tick-seconds . ,(/ tick-seconds runs))
        (redisplay-seconds . ,(/ redisplay-seconds runs))
        (mode-line-calls . ,(/ calls (float runs)))
        (mode-line-seconds . ,(/ seconds runs))
        (mode-line-seconds-per-call . ,(if (zerop calls) 0 (/ seconds calls)))
        (live-3s ,@live)))))

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
                    (push `(check ,@(bench-check)) bench-results)
                    (push `(redisplay-unforced ,@(bench-redisplay nil)) bench-results)
                    (push `(redisplay-forced ,@(bench-redisplay t)) bench-results)
                    (push `(mode-line-parts-us ,@(bench-parts)) bench-results)
                    (push `(animation-one ,@(bench-animation 1)) bench-results)
                    (push `(animation-all ,@(bench-animation most-positive-fixnum)) bench-results)
                    (push `(windows . ,(length (seq-mapcat (lambda (frame) (window-list frame 'never))
                                                           (frame-list))))
                          bench-results)
                    (push `(frames . ,(length (frame-list))) bench-results)
                    (push `(buffers . ,(length bench-buffers)) bench-results)
                    (bench-finish))))
      (bench-measure 'first-load #'bench-scene #'reconnect))))

(add-hook 'window-setup-hook (lambda () (run-at-time 0.5 nil #'bench-run)))

;; Whatever stalls the run, its results and messages are written.
(run-at-time (string-to-number (or (getenv "BENCH_DEADLINE") "560")) nil
             (lambda () (bench-finish (format "Deadline passed; busy: %S" (bench-busy-reasons)))))

;;; client.el ends here
