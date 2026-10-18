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

;; Three kinds of references in assistant output become buttons, each
;; once its closing delimiter arrives: backticked file references with an
;; optional line (`file.el:12'), bracketed Org links of any type
;; (`[[id:UUID][heading]]', opened by Org's own dispatcher, custom link
;; types included), and Markdown links (`[label](url)', local paths open
;; in Emacs).  Org link brackets are hidden so only the description shows,
;; as in Org.  File references and Org labels get the link face over
;; whatever Markdown gave them.
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
            (opencode-mark-fontify beg finish 'link)))))))

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
        (unless (get-text-property label-beg 'opencode-button)
          (opencode-make-button
           label-beg label-end
           'action #'opencode--follow-org-link
           'org-link link
           'org-link-target (buffer-substring-no-properties link-beg link-end)
           'follow-link t
           'help-echo (format "Open %s" link)
           'mouse-face 'highlight)
          (opencode-mark-fontify label-beg label-end 'link)
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
        (unless (get-text-property label-beg 'opencode-button)
          (opencode-make-button
           label-beg (match-end 1)
           'action #'opencode--follow-url
           'url url
           'follow-link t
           'help-echo (format "Open %s" url)
           'mouse-face 'highlight))))))

(add-hook 'opencode-buttonize-functions #'opencode--buttonize-file-references)
(add-hook 'opencode-buttonize-functions #'opencode--buttonize-org-links)
(add-hook 'opencode-buttonize-functions #'opencode--buttonize-markdown-links)

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
