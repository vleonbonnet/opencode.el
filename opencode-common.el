;;; opencode-common.el --- Shared definitions for opencode.el  -*- lexical-binding: t; -*-

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

;; Customization groups, faces and small helpers shared by every module.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'project)
(require 'subr-x)

(defgroup opencode nil
  "Emacs client for the OpenCode 2 server."
  :group 'applications
  :prefix "opencode-")

(defgroup opencode-faces nil
  "Faces of the OpenCode client."
  :group 'opencode)

;;;; Faces

(defface opencode-request-margin-highlight
  '((t :inherit outline-1 :height reset))
  "Margin of user requests."
  :group 'opencode-faces)

(defface opencode-reasoning-margin-highlight
  '((t :inherit outline-2 :height reset))
  "Margin of reasoning blocks."
  :group 'opencode-faces)

(defface opencode-tool-margin-highlight
  '((t :inherit outline-5 :height reset))
  "Margin of tool call blocks."
  :group 'opencode-faces)

(defface opencode-tool-running-margin
  '((t :inherit outline-8 :height reset))
  "Margin and summary box of running tool calls."
  :group 'opencode-faces)

(defface opencode-attention
  '((t :inherit warning))
  "Anything waiting on the user: permissions, questions, withheld narration."
  :group 'opencode-faces)

(defface opencode-prompt
  '((t :inherit default))
  "Text typed at the session prompt."
  :group 'opencode-faces)

(defface opencode-notice
  '((t :inherit shadow))
  "Transcript notices such as agent switches and instruction updates."
  :group 'opencode-faces)

(defface opencode-disclosure
  '((t :family "JuliaMono"))
  "Font of the collapsible block arrows, independent of their status color.
The arrows are absent from many programming fonts; naming a font that has
them avoids an expensive fallback search on every redisplay."
  :group 'opencode-faces)

;;;; Shared state

(defvar opencode-session-buffers (make-hash-table :test #'equal)
  "Session IDs mapped to their live session buffers.")

(defvar-local opencode-session-id nil
  "ID of the session shown in this buffer.")

(defvar opencode-alerted-sessions nil
  "Session infos that went idle while not displayed, newest first.")

(defvar opencode-last-session-buffer nil
  "Session buffer most recently selected.")

(defmacro opencode-with-session-buffer (session-id &rest body)
  "Run BODY in the live session buffer of SESSION-ID, if any."
  (declare (indent 1) (debug t))
  (let ((buffer (make-symbol "buffer")))
    `(when-let* ((,buffer (gethash ,session-id opencode-session-buffers))
                 ((buffer-live-p ,buffer)))
       (with-current-buffer ,buffer
         ,@body))))

;;;; Helpers

(defun opencode-normalize-directory (directory)
  "Return DIRECTORY as a canonical absolute directory name.
Symbolic links are resolved, so one project reached through two paths
maps to one location.  On Windows the drive letter is upcased, as the
server spells it: it keys locations by the exact string it receives."
  (let ((name (file-name-as-directory (file-truename (expand-file-name directory)))))
    (if (and (memq system-type '(windows-nt ms-dos))
             (string-match-p "\\`[a-z]:" name))
        (concat (upcase (substring name 0 1)) (substring name 1))
      name)))

(defun opencode-same-directory-p (a b)
  "Return non-nil when directory names A and B name the same directory."
  (and a b
       (string= (directory-file-name (opencode-normalize-directory a))
                (directory-file-name (opencode-normalize-directory b)))))

(defun opencode-project-directory (&optional directory)
  "Return the project root of DIRECTORY, else DIRECTORY itself."
  (let ((default-directory (or directory default-directory)))
    (opencode-normalize-directory
     (if-let* ((project (project-current)))
         (project-root project)
       default-directory))))

(defun opencode-time-seconds (milliseconds)
  "Return epoch MILLISECONDS as a float number of seconds, or nil."
  (and (numberp milliseconds) (/ milliseconds 1000.0)))

(defun opencode-time-ago (milliseconds)
  "Return the seconds elapsed since epoch MILLISECONDS, or nil."
  (when-let* ((seconds (opencode-time-seconds milliseconds)))
    (- (float-time) seconds)))

(defun opencode-format-time-ago (seconds)
  "Format SECONDS as a human readable duration, or \"-\" when nil."
  (if seconds (seconds-to-string seconds) "-"))

(defun opencode-annotated-completion (prompt candidates)
  "Read one of CANDIDATES with PROMPT and return its value.
CANDIDATES is a list of (NAME VALUE ANNOTATION SORT-KEY); ANNOTATION
and SORT-KEY are optional.  Candidates with a SORT-KEY are presented in
ascending key order."
  (if (null candidates)
      (progn (message "%sNo completion candidates" prompt) nil)
    (let* ((width (seq-max (mapcar (lambda (candidate) (string-width (car candidate)))
                                   candidates)))
           (values (make-hash-table :test #'equal))
           (sorted (seq-some #'cadddr candidates))
           (strings
            (cl-loop for (name value annotation sort-key) in candidates
                     for string = (propertize
                                   name
                                   'opencode-annotation
                                   (concat (make-string (+ 4 (- width (string-width name))) ?\s)
                                           (or annotation ""))
                                   'opencode-sort-key sort-key)
                     do (puthash string value values)
                     collect string))
           (completion-extra-properties
            `(:annotation-function
              ,(lambda (candidate) (get-text-property 0 'opencode-annotation candidate))
              :display-sort-function
              ,(when sorted
                 (lambda (strings)
                   (sort strings
                         :key (lambda (string)
                                (or (get-text-property 0 'opencode-sort-key string)
                                    most-positive-fixnum))))))))
      (gethash (completing-read prompt strings nil t) values))))

(defun opencode-buffer-active-p (buffer)
  "Return non-nil when BUFFER is shown in the selected window of a focused frame."
  (and buffer
       (eq buffer (window-buffer (selected-window)))
       (frame-focus-state (window-frame (selected-window)))))

(defun opencode-relative-path-for-display (file)
  "Return FILE relative to the current project root when inside it."
  (when file
    (let ((root (opencode-project-directory)))
      (if (and (file-name-absolute-p file) (file-in-directory-p file root))
          (file-relative-name file root)
        file))))

(defun opencode-resolve-file-reference (file)
  "Return the absolute name of existing regular FILE, else nil.
Relative names are tried against the project root and `default-directory'."
  (when (and (stringp file) (not (string-empty-p file)))
    (seq-find (lambda (candidate)
                (and candidate (file-regular-p candidate)))
              (delete-dups
               (list (and (file-name-absolute-p file) file)
                     (expand-file-name file (opencode-project-directory))
                     (expand-file-name file default-directory))))))

(defun opencode-fenced-block (text language)
  "Return TEXT in a LANGUAGE fence longer than any backtick run in TEXT."
  (let ((length 3)
        (start 0))
    (while (string-match "`+" text start)
      (setq length (max length (1+ (- (match-end 0) (match-beginning 0))))
            start (match-end 0)))
    (let ((fence (make-string length ?`)))
      (concat fence (or language "") "\n" text
              (unless (string-suffix-p "\n" text) "\n")
              fence))))

(defun opencode-content-text (content)
  "Join the text items of tool or message CONTENT, a list of content alists."
  (let ((texts (delq nil (mapcar (lambda (item)
                                   (when (equal (alist-get 'type item) "text")
                                     (alist-get 'text item)))
                                 content))))
    (when texts (string-join texts "\n"))))

(defvar opencode--id-last-time 0
  "Millisecond timestamp of the last generated identifier.")

(defvar opencode--id-counter 0
  "Identifiers generated within `opencode--id-last-time'.")

(defun opencode-generate-id (prefix)
  "Return a new ascending OpenCode identifier starting with PREFIX.
Same layout as the server's: 12 hex digits of millisecond time times
4096 plus a per-millisecond counter, then 14 random base62 characters.
Generating message IDs locally lets a client recognize the echo of its
own prompts on the event stream."
  (let ((now (truncate (* 1000 (float-time)))))
    (if (= now opencode--id-last-time)
        (cl-incf opencode--id-counter)
      (setq opencode--id-last-time now
            opencode--id-counter 1))
    (let ((chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
      (concat prefix
              (format "%012x" (+ (* now 4096) opencode--id-counter))
              (apply #'string
                     (cl-loop repeat 14 collect (aref chars (random 62))))))))

(provide 'opencode-common)
;;; opencode-common.el ends here
