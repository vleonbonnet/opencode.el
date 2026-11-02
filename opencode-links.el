;;; opencode-links.el --- Links in session transcripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann
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

;; Four kinds of references in the transcript, the agent's output and the
;; prompts sent alike, become buttons, each
;; once its closing delimiter arrives: backticked file references with an
;; optional line (`file.el:12'), bracketed Org links of any type
;; (`[[id:UUID][heading]]', opened by Org's own dispatcher, custom link
;; types included), Markdown links (`[label](url)', local paths open
;; in Emacs), and URLs written out (http, https, ftp and file), which
;; end where a URL cannot continue, or where the text ends, without the
;; punctuation closing a sentence or a bracket they do not open.  Org
;; link brackets are hidden so only the description shows, as in Org.
;; File references, Org labels and URLs get the link face over whatever
;; Markdown gave them.
;;
;; \\[opencode-open-link-at-point] follows the link at point, else offers
;; the links of the exchange around point, the way `org-open-at-point'
;; falls back to the links of the entry.

;;; Code:

(require 'button)
(require 'opencode-common)
(require 'opencode-render)
(require 'url-parse)
(require 'url-util)

(declare-function org-link-open-from-string "ol" (s &optional arg))
(defvar org-link-bracket-re)

(defvar opencode-open-link-functions nil
  "Functions tried first by `opencode-open-link-at-point', without arguments.
The first returning non-nil has handled the link at point.")

(defconst opencode--file-reference-regexp
  (rx "`"
      (group (any alnum "._~/")
             (* (any alnum "@%_./~+-")))
      (optional ":"
                (group (+ digit))
                (optional (any "-" "–") (+ digit)))
      "`")
  "Backticked file reference with an optional line number or range.")

(defconst opencode--markdown-link-regexp
  "\\[\\([^]\n]+\\)\\](\\([^) \t\n]+\\)\\(?:[^)\n]*\\))"
  "Inline Markdown link; the URL group stops before any title.")

(defun opencode-visit-file-location (location)
  "Open LOCATION, a cons (FILE . LINE), in another window."
  (find-file-other-window (car location))
  (goto-char (point-min))
  (forward-line (1- (or (cdr location) 1)))
  (recenter))

(defun opencode--buttonize-file-references (start end)
  "Turn backticked file references between START and END into buttons."
  (save-excursion
    (goto-char start)
    (while (re-search-forward opencode--file-reference-regexp end t)
      (let* ((beg (match-beginning 0))
             (finish (match-end 0))
             (file (match-string-no-properties 1))
             (line-string (match-string-no-properties 2))
             (line (if line-string (string-to-number line-string) 1)))
        (unless (button-at beg)
          (when-let* ((path (opencode-resolve-file-reference file)))
            (opencode-make-button
             beg finish
             'action #'opencode-visit-file-location
             'button-data (cons path line)
             'follow-link t
             'help-echo (if line-string
                            (format "Open %s:%d in another window" file line)
                          (format "Open %s in another window" file))
             'mouse-face 'highlight)
            (opencode-mark-fontify beg finish 'link t)))))))

(defun opencode--follow-org-link (button)
  "Open BUTTON's Org link with Org's link dispatcher."
  (require 'ol)
  (org-link-open-from-string (button-get button 'org-link)))

(defun opencode--buttonize-org-links (start end)
  "Turn bracketed Org links between START and END into buttons."
  (require 'ol)
  (save-excursion
    (goto-char start)
    (while (re-search-forward org-link-bracket-re end t)
      (let* ((beg (match-beginning 0))
             (finish (match-end 0))
             (link-beg (match-beginning 1))
             (link-end (match-end 1))
             (label-beg (or (match-beginning 2) link-beg))
             (label-end (or (match-end 2) link-end))
             (link (match-string-no-properties 0)))
        (opencode--unlink-bare-urls beg finish)
        (unless (get-text-property label-beg 'opencode-button)
          (opencode-make-button
           label-beg label-end
           'action #'opencode--follow-org-link
           'org-link link
           'org-link-target (buffer-substring-no-properties link-beg link-end)
           'follow-link t
           'help-echo (format "Open %s" link)
           'mouse-face 'highlight)
          (opencode-mark-fontify label-beg label-end 'link t)
          (opencode-mark-fontify beg label-beg 'hidden)
          (opencode-mark-fontify label-end finish 'hidden))))))

(defun opencode--follow-url (button)
  "Open BUTTON's local file in Emacs, or its external URL in a browser."
  (let* ((url (button-get button 'url))
         (parsed (url-generic-parse-url url))
         (scheme (url-type parsed)))
    (if (or (null scheme)
            (and (equal scheme "file")
                 (member (url-host parsed) '(nil "" "localhost"))))
        (let* ((file (url-unhex-string (url-filename parsed)))
               (path (opencode-resolve-file-reference file))
               (fragment (url-target parsed))
               (line (if (and fragment (string-match "\\`L\\([1-9][0-9]*\\)\\'" fragment))
                         (string-to-number (match-string 1 fragment))
                       1)))
          (unless path
            (user-error "File not found: %s" file))
          (opencode-visit-file-location (cons path line)))
      (browse-url url))))

(defun opencode--buttonize-markdown-links (start end)
  "Turn `[label](url)' Markdown links between START and END into buttons."
  (save-excursion
    (goto-char start)
    (while (re-search-forward opencode--markdown-link-regexp end t)
      (let ((label-beg (match-beginning 1))
            (url (match-string-no-properties 2)))
        (opencode--unlink-bare-urls (match-beginning 0) (match-end 0))
        (unless (get-text-property label-beg 'opencode-button)
          (opencode-make-button
           label-beg (match-end 1)
           'action #'opencode--follow-url
           'url url
           'follow-link t
           'help-echo (format "Open %s" url)
           'mouse-face 'highlight))))))

;;;; Bare URLs

(defconst opencode--bare-url-regexp
  (rx (or "https" "http" "ftp" "file") "://"
      (group (* (not (any (?\0 . ?\s) ?\177 blank ?< ?> ?\" ?`)))))
  "A URL written out in text, up to whatever cannot be part of one.
Its first group is what follows the scheme.  Trailing punctuation is
left to `opencode--bare-url-end'.")

(defconst opencode--bare-url-properties
  '(button category action url follow-link help-echo mouse-face opencode-button opencode-bare-url)
  "Text properties of a bare URL's button.")

(defun opencode--bare-url-end (beg end)
  "Return where the URL from BEG ends, its match ending at END.
Punctuation closing the sentence or quote it is in is not part of it,
nor a closing bracket it does not open, as in GitHub's Markdown."
  (let ((stop end)
        (done nil))
    (while (and (> stop beg) (not done))
      (let* ((char (char-before stop))
             (opening (cdr (assq char '((?\) . ?\() (?\] . ?\[) (?} . ?{))))))
        (if (or (memq char '(?. ?, ?: ?\; ?! ?? ?' ?* ?_ ?~))
                (and opening
                     (let ((url (buffer-substring-no-properties beg stop)))
                       (< (cl-count opening url) (cl-count char url))))
                (and (> char 127)
                     (memq (get-char-code-property char 'general-category)
                           '(Po Pe Pf Pi Ps))))
            (setq stop (1- stop))
          (setq done t))))
    stop))

(defun opencode--bare-url-skip-p (beg end)
  "Return non-nil when the URL from BEG to END must not be a button of its own.
That is when it continues a word, is or is in a link already, is the
hidden target of an Org link or the destination of a Markdown one, or is
in text marked `opencode-no-links'."
  (or (and (> beg (point-min))
           (string-match-p "[[:alnum:]]" (string (char-before beg))))
      (text-property-not-all beg end 'opencode-button nil)
      (text-property-not-all beg end 'opencode-no-links nil)
      (memq 'hidden (opencode-fontify-kinds beg))
      (save-excursion
        (goto-char beg)
        (looking-back "\\](<?" (max (point-min) (- beg 3))))))

(defun opencode--buttonize-bare-urls (start end)
  "Turn the URLs written out between START and END into buttons.
A URL ending at END may still grow, unless the output is complete
\(see `opencode-buttonize-complete'), and is left for the next pass."
  (save-excursion
    (goto-char start)
    (while (re-search-forward opencode--bare-url-regexp end t)
      (let* ((beg (match-beginning 0))
             (path-beg (match-beginning 1))
             (run-end (match-end 0))
             (finish (opencode--bare-url-end path-beg run-end)))
        (when (and (or (< run-end end) opencode-buttonize-complete)
                   (> finish path-beg)
                   (not (opencode--bare-url-skip-p beg finish)))
          (let ((url (buffer-substring-no-properties beg finish)))
            (opencode-make-button
             beg finish
             'action #'opencode--follow-url
             'url url
             'follow-link t
             'help-echo (format "Open %s" url)
             'mouse-face 'highlight
             'opencode-bare-url t)
            (opencode-mark-fontify beg finish 'link t)))))))

(defun opencode--unlink-bare-urls (start end)
  "Undo the bare URL buttons overlapping START..END.
A link whose delimiters arrived later, such as an Org link with a
description, may hold a URL that was made a button on its own."
  (let ((pos start))
    (while (and (< pos end) (setq pos (text-property-any pos end 'opencode-bare-url t)))
      (let* ((button (button-at pos))
             (beg (button-start button))
             (finish (button-end button)))
        (with-silent-modifications
          (remove-list-of-text-properties beg finish opencode--bare-url-properties)
          (let ((run beg))
            (while (< run finish)
              (let ((next (next-single-property-change run 'opencode-fontify nil finish))
                    (kinds (remq 'link (opencode-fontify-kinds run))))
                (put-text-property run next 'opencode-fontify (if (cdr kinds) kinds (car kinds)))
                (setq run next)))
            (setq run beg)
            (while (< run finish)
              (let ((next (next-single-property-change run 'face nil finish))
                    (face (get-text-property run 'face)))
                (cond ((eq face 'link) (put-text-property run next 'face nil))
                      ((and (consp face) (memq 'link face))
                       (put-text-property run next 'face (remq 'link face))))
                (setq run next)))))
        (setq pos finish)))))

(add-hook 'opencode-buttonize-functions #'opencode--buttonize-file-references)
(add-hook 'opencode-buttonize-functions #'opencode--buttonize-org-links)
(add-hook 'opencode-buttonize-functions #'opencode--buttonize-markdown-links)
;; Last, so that a URL in a link of another kind is left to it.
(add-hook 'opencode-buttonize-functions #'opencode--buttonize-bare-urls 90)

;;;; Prompts rendered without their links

(defun opencode-link-sent-prompts ()
  "Make the links of the prompts sent in this session buffer that lack them.
A prompt gets its links when it becomes sent text; earlier versions of
this package made none, and the prompts a buffer showed then keep none
until it is rendered again.  The input is left as it is."
  (when-let* ((process (get-buffer-process (current-buffer))))
    (let ((end (marker-position (process-mark process)))
          (pos (point-min)))
      (with-silent-modifications
        (while (< pos end)
          (let ((next (next-single-property-change pos 'opencode-prompt nil end)))
            (when (get-text-property pos 'opencode-prompt)
              (opencode-buttonize pos next t))
            (setq pos next)))))))

;; Loaded over an earlier version, this one links the prompts of the
;; session buffers that version rendered.
(dolist (buffer (buffer-list))
  (when (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'opencode-session-mode)
    (with-current-buffer buffer
      (opencode-link-sent-prompts))))

;;;; Opening links

(defun opencode--prompt-positions ()
  "Return the sorted positions of the Comint prompts."
  (save-excursion
    (goto-char (point-min))
    (let (prompts)
      (catch 'done
        (while t
          (comint-next-prompt 1)
          (when (or (>= (point) (point-max)) (member (point) prompts))
            (throw 'done nil))
          (push (point) prompts)))
      (nreverse prompts))))

(defun opencode--section-bounds ()
  "Return (BEG . END) of the exchange around point, delimited by prompts."
  (let ((beg (point-min))
        (end (point-max))
        (pt (point)))
    (dolist (stop (opencode--prompt-positions))
      (cond ((<= stop pt) (setq beg (max beg stop)))
            ((< stop end) (setq end stop))))
    (cons beg end)))

(defun opencode--button-target (button)
  "Return a key identifying BUTTON's destination, for merging duplicates."
  (cond
   ((button-get button 'org-link-target) (cons 'org-link (button-get button 'org-link-target)))
   ((button-get button 'url) (cons 'url (button-get button 'url)))
   ((button-get button 'button-data) (cons 'file (button-get button 'button-data)))
   (t (cons 'text (string-trim (buffer-substring-no-properties
                                (button-start button) (button-end button)))))))

(defun opencode--section-links ()
  "Return an alist (LABEL . POSITION) of the link buttons of the current exchange.
Buttons sharing a target are merged; distinct targets sharing a label
are numbered so completion keys stay unique."
  (pcase-let ((`(,beg . ,end) (opencode--section-bounds)))
    (let (links (seen (make-hash-table :test #'equal)))
      (save-excursion
        (let ((button (or (button-at beg) (next-button beg))))
          (while (and button (< (button-start button) end))
            (let ((target (opencode--button-target button)))
              (unless (gethash target seen)
                (puthash target t seen)
                (let* ((base (string-trim (buffer-substring-no-properties
                                           (button-start button) (button-end button))))
                       (key base)
                       (count 1))
                  (while (assoc key links)
                    (setq key (format "%s <%d>" base (cl-incf count))))
                  (push (cons key (button-start button)) links))))
            (setq button (next-button (button-end button))))))
      (nreverse links))))

(defun opencode-open-link-at-point ()
  "Follow the link at point, else offer the links of the exchange around point."
  (interactive)
  (cond
   ((run-hook-with-args-until-success 'opencode-open-link-functions))
   ((button-at (point)) (push-button))
   ((thing-at-point 'url t) (browse-url (thing-at-point 'url t)))
   (t (let ((links (opencode--section-links)))
        (pcase links
          ('nil (user-error "No links in this exchange"))
          (`(,only) (push-button (cdr only)))
          (_ (let ((choice (completing-read (format-prompt "Open link" (caar links))
                                            links nil t nil nil (caar links))))
               (when-let* ((position (cdr (assoc choice links))))
                 (push-button position)))))))))

(provide 'opencode-links)
;;; opencode-links.el ends here
