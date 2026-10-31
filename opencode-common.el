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

(declare-function w32-read-registry "w32fns.c" (root key name))

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

(defun opencode--upcase-drive (name)
  "Return file NAME with a Windows drive letter upcased."
  (if (and (memq system-type '(windows-nt ms-dos))
           (string-match-p "\\`[a-z]:" name))
      (concat (upcase (substring name 0 1)) (substring name 1))
    name))

(defvar opencode--network-drives nil
  "Cons (TIME . DRIVES) of the network drives read at TIME.
See `opencode--network-drives'.")

(defun opencode--network-drives ()
  "Return the user's mapped network drives as ((SHARE . ROOT) ...).
SHARE is a share's UNC name as a directory, //HOST/SHARE/, and ROOT the
root of the drive mapped to it, such as Z:/.  Windows keeps the
mappings in the registry, read at most once a minute.  A drive is not
probed: a disconnected share would stall the request, and neither of
its names reaches it until it reconnects."
  (unless (and opencode--network-drives
               (< (- (float-time) (car opencode--network-drives)) 60))
    (setq opencode--network-drives
          (cons (float-time)
                (and (fboundp 'w32-read-registry)
                     (seq-keep (lambda (letter)
                                 (when-let* ((remote (ignore-errors
                                                       (w32-read-registry
                                                        'HKCU (format "Network\\%c" letter) "RemotePath")))
                                             ((stringp remote)))
                                   (cons (file-name-as-directory (string-replace "\\" "/" remote))
                                         (format "%c:/" letter))))
                               (number-sequence ?A ?Z))))))
  (cdr opencode--network-drives))

(defun opencode--mapped-drive-name (name)
  "Return directory NAME through the drive its network share is mapped to, if any."
  (or (and (memq system-type '(windows-nt ms-dos))
           (string-prefix-p "//" name)
           (seq-some (lambda (drive)
                       (when (string-prefix-p (car drive) name t)
                         (concat (cdr drive) (substring name (length (car drive))))))
                     (opencode--network-drives)))
      name))

(defun opencode-normalize-directory (directory)
  "Return DIRECTORY as a canonical absolute directory name.
Symbolic links are resolved, so one project reached through two paths
maps to one location.  On Windows a network share is named by the drive
mapped to it, as a link to the drive resolves, and the drive letter is
upcased, as the server spells it: it keys locations by the exact string
it receives."
  (opencode--upcase-drive
   (opencode--mapped-drive-name
    (file-name-as-directory (file-truename (expand-file-name directory))))))

(defun opencode-directory-name (directory)
  "Return DIRECTORY as the server names it: normalized, with no trailing slash.
On Windows `directory-file-name' lowercases the drive letter again, so
it is upcased after it; otherwise one directory would be two locations."
  (opencode--upcase-drive (directory-file-name (opencode-normalize-directory directory))))

(defun opencode-same-directory-p (a b)
  "Return non-nil when directory names A and B name the same directory."
  (and a b
       (string= (directory-file-name (opencode-normalize-directory a))
                (directory-file-name (opencode-normalize-directory b)))))

(defvar-local opencode--project-cache nil
  "Hash table of project roots by directory, for this buffer.
File references are resolved on every streamed chunk; walking the
parents of a directory each time to find its project was measurable.")

(defun opencode-project-directory (&optional directory)
  "Return the project root of DIRECTORY, else DIRECTORY itself."
  (let* ((directory (or directory default-directory))
         (cache (or opencode--project-cache
                    (setq opencode--project-cache (make-hash-table :test #'equal)))))
    (or (gethash directory cache)
        (puthash directory
                 (let ((default-directory directory))
                   (opencode-normalize-directory
                    (if-let* ((project (project-current)))
                        (project-root project)
                      default-directory)))
                 cache))))

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

(defun opencode-terminal-text (text)
  "Return TEXT as a terminal leaves it on screen.
CRLF line ends, which Windows programs print, become LF, and a carriage
return within a line, as in a progress display, writes over that line
from its start."
  (if (not (string-search "\r" text))
      text
    (mapconcat (lambda (line)
                 (let ((shown ""))
                   (dolist (segment (split-string line "\r"))
                     (setq shown (concat segment (when (> (length shown) (length segment))
                                                   (substring shown (length segment))))))
                   shown))
               (split-string (replace-regexp-in-string "\r+\n" "\n" text) "\n")
               "\n")))

(defun opencode-fenced-block (text language)
  "Return TEXT in a LANGUAGE fence longer than any backtick run in TEXT.
TEXT is shown as a terminal would show it; see `opencode-terminal-text'."
  (let ((text (opencode-terminal-text text))
        (length 3)
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
              ;; The server keeps the low 48 bits.
              (format "%012x" (logand (+ (* now 4096) opencode--id-counter) #xFFFFFFFFFFFF))
              (apply #'string
                     (cl-loop repeat 14 collect (aref chars (random 62))))))))

(defun opencode-identifier-time (id &optional now)
  "Return the creation time, in float seconds, of ascending identifier ID, or nil.
ID has the layout `opencode-generate-id' describes, as the service's
permission and form IDs do.  Its 48 bits wrap every 2^36 milliseconds,
about 795 days, so the time is the one closest to NOW, the current
time by default.  Session IDs descend and do not qualify."
  (when (and (stringp id)
             (string-match "\\`[a-z]+_\\([0-9a-f]\\{12\\}\\)[0-9A-Za-z]\\{14\\}\\'" id))
    (let* ((value (string-to-number (match-string 1 id) 16))
           (period (ash 1 48))
           (now (* 4096 (truncate (* 1000 (or now (float-time))))))
           (candidate (+ (* (/ now period) period) value)))
      (cond ((> (- candidate now) (/ period 2)) (cl-decf candidate period))
            ((> (- now candidate) (/ period 2)) (cl-incf candidate period)))
      (/ (/ candidate 4096) 1000.0))))

(provide 'opencode-common)
;;; opencode-common.el ends here
