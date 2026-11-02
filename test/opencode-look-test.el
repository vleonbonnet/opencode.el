;;; opencode-look-test.el --- Tests of the look sessions share  -*- lexical-binding: t; -*-

;;; Commentary:

;; The icons, faces, animations and formats every display of sessions
;; uses: the mode line, the pickers, and other packages' lists.

;;; Code:

(require 'opencode-test-helpers)

(ert-deftest opencode-test-look-statuses ()
  "Every status, most urgent first, has a name, a face and a one-glyph icon."
  (should (equal opencode-session-statuses '(waiting failed done retrying working idle)))
  (dolist (status opencode-session-statuses)
    (let ((glyph (opencode-session-status-glyph status))
          (icon (opencode-session-status-icon status)))
      (should (= (length glyph) 1))
      (should (eq (opencode-session-status-face status)
                  (intern (format "opencode-session-status-%s" status))))
      (should (facep (opencode-session-status-face status)))
      ;; Font lock, which runs in lists, keeps the face.
      (should (eq (get-text-property 0 'face glyph) (opencode-session-status-face status)))
      (should (eq (get-text-property 0 'font-lock-face glyph) (opencode-session-status-face status)))
      (should (equal-including-properties (substring icon 0) (propertize glyph 'help-echo
                                                                         (opencode-session-status-label status))))))
  (should (equal (mapcar #'opencode-session-status-label opencode-session-statuses)
                 '("Needs you" "Failed" "Done" "Retrying" "Working" "Idle")))
  (should (equal (substring-no-properties (opencode-session-status-glyph nil)) "-"))
  (should (eq (opencode-session-status-face 'unknown) 'opencode-session-status-idle))
  ;; Waiting is what waits on the user, which `opencode-attention' marks.
  (should (eq (face-attribute 'opencode-session-status-waiting :inherit) 'opencode-attention)))

(ert-deftest opencode-test-look-status-icons-default ()
  "The icons are Nerd Font glyphs one column wide, and the former form still works."
  (let ((default (eval (car (get 'opencode-session-status-icons 'standard-value)) t)))
    (should (equal default
                   '((waiting . "\U000F02D7")     ; nf-md-help_circle
                     (failed . "\uf057")          ; nf-fa-times_circle
                     (done . "\U000F05E0")        ; nf-md-check_circle
                     (retrying . "\uf021")        ; nf-fa-refresh
                     (working . "\U000F051F")     ; nf-md-timer_sand
                     (idle . "\uf10c"))))         ; nf-fa-circle_o
    (dolist (entry default)
      (should (= (char-width (string-to-char (cdr entry))) 1))))
  (let ((opencode-session-status-icons '((waiting "W" some-face) (idle . "I"))))
    (should (equal (substring-no-properties (opencode-session-status-glyph 'waiting)) "W"))
    (should (eq (get-text-property 0 'face (opencode-session-status-glyph 'waiting))
                'opencode-session-status-waiting))
    (should (equal (substring-no-properties (opencode-session-status-glyph 'idle)) "I"))))

(ert-deftest opencode-test-look-animations ()
  "Working turns the phases of the moon, retrying a clock, at 0.15 s a frame."
  (let ((animations (eval (car (get 'opencode-session-status-animations 'standard-value)) t)))
    (should (= (length (alist-get 'working animations)) 8))
    (should (= (length (alist-get 'retrying animations)) 4))
    (should (equal (car (alist-get 'working animations)) "\U000F0F64"))) ; nf-md-moon_new
  (should (= (eval (car (get 'opencode-session-status-animation-interval 'standard-value)) t) 0.15))
  (let ((opencode-session-status-animations '((working "1" "" "2" nil "3") (retrying)))
        (opencode-session-status-animation-interval 0.5))
    (let ((frames (opencode-session-status-frames 'working)))
      (should (equal frames '("1" "2" "3")))
      (should (eq (get-text-property 0 'font-lock-face (car frames)) 'opencode-session-status-working)))
    (should-not (opencode-session-status-frames 'retrying))
    (should-not (opencode-session-status-frames 'idle))
    ;; Every icon shows the frame of the same instant.
    (should (= (opencode-session-status-frame-index 3 10.0) 2))
    (should (= (opencode-session-status-frame-index 3 10.6) 0))
    ;; Ticks fall in the middle of frames.
    (should (< (abs (- (opencode-session-status-frame-delay 10.1) 0.65)) 1e-9))
    (should (< (abs (- (opencode-session-status-frame-delay 10.0) 0.75)) 1e-9))
    (should (< (abs (- (opencode-session-status-frame-delay 10.49) 0.26)) 1e-9))
    (let ((opencode-session-status-animation-interval nil))
      (should-not (opencode-session-status-frames 'working)))))

(ert-deftest opencode-test-look-subagents-and-archive ()
  "Subagents at work and the archive have icons of their own, in the faces of what they tell."
  (let ((waiting (opencode-subagent-glyph 'waiting))
        (working (opencode-subagent-glyph 'working)))
    (should (equal waiting "\U000F169F"))  ; nf-md-robot_confused
    (should (equal working "\U000F06A9"))  ; nf-md-robot
    (should (eq (get-text-property 0 'face waiting) 'opencode-session-status-waiting))
    (should (eq (get-text-property 0 'font-lock-face working) 'opencode-session-status-working)))
  (let ((archived (opencode-archived-glyph)))
    (should (equal archived "\U000F003C")) ; nf-md-archive
    (should (eq (get-text-property 0 'face archived) 'opencode-session-archived))))

(ert-deftest opencode-test-look-places ()
  "A session's place is its buffer's: in the window worked in, shown, open, or none."
  (let ((opencode-session-buffers (make-hash-table :test #'equal))
        (shown (generate-new-buffer " *opencode-test-shown*"))
        (open (generate-new-buffer " *opencode-test-open*"))
        (active (generate-new-buffer " *opencode-test-active*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (puthash "ses_shown" shown opencode-session-buffers)
          (puthash "ses_open" open opencode-session-buffers)
          (puthash "ses_active" active opencode-session-buffers)
          (set-window-buffer (selected-window) active)
          (set-window-buffer (split-window) shown)
          (should (eq (opencode-session-place "ses_active") 'active))
          (should (eq (opencode-session-place "ses_shown") 'shown))
          (should (eq (opencode-session-place "ses_open") 'open))
          (should-not (opencode-session-place "ses_none"))
          (should (equal (mapcar #'opencode-session-place-face '(active shown open nil))
                         '(opencode-session-active opencode-session-shown opencode-session-open nil))))
      (mapc #'kill-buffer (list shown open active)))))

(ert-deftest opencode-test-look-numbers ()
  "Ages and token counts read short."
  (should (equal (mapcar #'opencode-format-duration '(-5 0 59.9 60 3599 3600 86399 86400 864000))
                 '("0s" "0s" "59s" "1m" "59m" "1h" "23h" "1d" "10d")))
  (should (equal (opencode-format-age 100.0 160.5) "1m"))
  (should (equal (opencode-format-age nil) ""))
  (should (equal (mapcar #'opencode-format-tokens '(950 8400 9950 182000 999499 999500 1200000 25000000))
                 '("950" "8.4k" "10k" "182k" "999k" "1M" "1.2M" "25M"))))

(ert-deftest opencode-test-look-context-description ()
  "What a context fills reads alike everywhere, with its estimate and price tier."
  (should (equal (opencode-context-description 182000 200000) "182k of 200k tokens (91%)"))
  (should (equal (opencode-context-description 182000) "182k tokens"))
  (should (equal (opencode-context-description 4200 400000 nil t)
                 "~4.2k of 400k tokens (1%)\nEstimated by the service after compaction; the next step measures it"))
  (should (equal (opencode-context-description 280000 1000000 '(over 272000 "2×"))
                 "280k of 1M tokens (28%)\nPast 272k tokens: requests cost 2×"))
  (should (equal (opencode-context-description 250000 1000000 '(near 272000 "2×") t)
                 (concat "~250k of 1M tokens (25%)\n"
                         "Estimated by the service after compaction; the next step measures it\n"
                         "Estimated near 272k tokens, past which requests cost 2×")))
  (should (equal (mapcar #'opencode-context-face '((over 1 "x") (near 1 "x") nil))
                 '(opencode-context-over opencode-context-near nil))))

(ert-deftest opencode-test-look-renamed-agent-faces ()
  "The agent faces of the mode line are now everyone's, the former name an alias."
  (should (eq (indirect-variable 'opencode-mode-line-agent-faces) 'opencode-agent-faces))
  (should (get 'opencode-mode-line-agent-faces 'byte-obsolete-variable))
  (let ((opencode-mode-line-agent-faces '(bold)))
    (should (equal opencode-agent-faces '(bold)))))

(provide 'opencode-look-test)
;;; opencode-look-test.el ends here
