;;; opencode-changes.el --- What tool calls change in files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
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

;; Agents change files through the shell as often as through the edit
;; tool: Python heredocs replacing text, `cat >', `sed -i', formatters.
;; A command alone hides what it changes, so its display shows more:
;;
;; - Its shell lines stay verbatim, and the body of each heredoc is a
;;   code block in the language of what reads it: an interpreter's,
;;   that of the file `cat' or `tee' writes, a diff for `git apply'.
;; - Before it runs, as while it waits for permission, the literal
;;   `.replace(OLD, NEW)' calls of its Python scripts, those of heredocs
;;   and of `python -c', show as edits do, the diff of OLD to NEW, under
;;   the file the script last opened.  This is a prediction from the
;;   text alone, labelled so: other edits show once the command ran.
;; - Once it ran, the files the service reports it changed, from the
;;   snapshots it takes around the call, show as edits do.  A write
;;   reports its change the same way.

;;; Code:

(require 'cl-lib)
(require 'diff)
(require 'seq)
(require 'opencode-common)
(require 'opencode-render)

;;;; Diffs

(defun opencode--format-edit-diff (old new)
  "Return the unified diff body from OLD to NEW."
  (with-temp-buffer
    (let ((old-buffer (current-buffer)))
      (insert old "\n")
      (with-temp-buffer
        (let ((new-buffer (current-buffer)))
          (insert new "\n")
          (with-temp-buffer
            (diff-no-select old-buffer new-buffer nil t (current-buffer))
            ;; The diff buffer is read-only, in Diff mode.
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (forward-line 3)
              (delete-region (point-min) (point))
              (goto-char (point-max))
              (forward-line -2)
              (delete-region (point) (point-max))
              (buffer-string))))))))

(defun opencode-changes--hunks (patch)
  "Return the hunks of PATCH, a one-file unified diff, without its headers.
Nil when it has none, as for a binary file."
  (when (and (stringp patch)
             (string-match "^@@ " patch))
    (substring patch (match-beginning 0))))

(defun opencode-changes--count (number sign)
  "Return NUMBER of changed lines after SIGN, or nil when not a number."
  (when (numberp number) (format "%s%d" sign number)))

;;;; Languages

(defun opencode-changes--mode-language (mode)
  "Return the code block language of major MODE, a symbol, or nil."
  (when (symbolp mode)
    (let ((name (replace-regexp-in-string "\\(?:-ts\\)?-mode\\(?:-maybe\\)?\\'" "" (symbol-name mode))))
      (unless (or (string-empty-p name) (equal name (symbol-name mode)))
        name))))

(defun opencode-changes-file-language (file)
  "Return the code block language of FILE's contents, from `auto-mode-alist'."
  (let ((mode (assoc-default (file-name-nondirectory file) auto-mode-alist #'string-match-p)))
    (opencode-changes--mode-language (if (consp mode) (car mode) mode))))

(defun opencode-changes--interpreter-language (program)
  "Return the code block language of scripts PROGRAM reads, or nil."
  (pcase program
    ((or "bun" "deno" "tsx" "ts-node") "typescript")
    (_ (opencode-changes--mode-language
        (assoc-default program interpreter-mode-alist
                       (lambda (regexp name)
                         (string-match-p (concat "\\`\\(?:" regexp "\\)\\'") name)))))))

;;;; Shell commands

(defun opencode-changes--words (text)
  "Split shell TEXT into words, unquoting simple quotes; a rough reading."
  (let (words)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (progn (skip-chars-forward " \t\n") (not (eobp)))
        (let ((word ""))
          (while (and (not (eobp)) (not (memq (char-after) '(?\s ?\t ?\n))))
            (pcase (char-after)
              ((and mark (or ?\' ?\"))
               (forward-char)
               (let ((start (point)))
                 (skip-chars-forward (string ?^ mark))
                 (setq word (concat word (buffer-substring-no-properties start (point))))
                 (unless (eobp) (forward-char))))
              (_ (setq word (concat word (string (char-after))))
                 (forward-char))))
          (push word words))))
    (nreverse words)))

(defun opencode-changes--program (words)
  "Return WORDS from the program they run, past assignments and wrappers."
  (while (and words
              (or (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car words))
                  (member (car words) '("sudo" "env" "time" "command" "exec" "nice" "nohup"))
                  (and (string-prefix-p "-" (car words)) (cdr words))))
    (pop words))
  (when (and (equal (car words) "uv") (equal (cadr words) "run"))
    (setq words (cddr words))
    (while (and words (string-prefix-p "-" (car words)))
      (pop words)))
  (when (member (car words) '("npx" "bunx"))
    (setq words (cdr words)))
  (when words
    (cons (file-name-nondirectory (car words)) (cdr words))))

(defun opencode-changes--written-file (words)
  "Return the file the redirection or `tee' of shell WORDS writes, or nil."
  (or (seq-some (lambda (word)
                  (and (string-match "\\`[0-9]*>>?\\([^>&].*\\)" word)
                       (not (string-prefix-p "&" (match-string 1 word)))
                       (match-string 1 word)))
                words)
      (let ((tail (member ">" words)))
        (cadr tail))
      (let ((tail (member ">>" words)))
        (cadr tail))
      (and (equal (car words) "tee")
           (seq-find (lambda (word) (not (string-prefix-p "-" word))) (cdr words)))))

(defun opencode-changes--heredoc-reader (text)
  "Return (LANGUAGE . FILE) for the heredoc of simple shell command TEXT.
LANGUAGE names the code block its body shows as, from the program
reading it; FILE is the file the command writes the body to, if any."
  (let* ((words (opencode-changes--program (opencode-changes--words text)))
         (program (car words))
         (file (opencode-changes--written-file words)))
    (cond
     ((member program '("cat" "tee"))
      (cons (or (and file (opencode-changes-file-language file)) "text") file))
     ((or (equal program "patch")
          (and (equal program "git") (member (cadr words) '("apply" "am"))))
      (cons "diff" nil))
     (t (cons (or (and program (opencode-changes--interpreter-language program)) "text") file)))))

(defconst opencode-changes--heredoc-regexp
  "\\(?:\\`\\|[^<]\\)<<\\(-?\\)[ \t]*\\(['\"]?\\)\\([A-Za-z_][A-Za-z0-9_.-]*\\)\\2"
  "Regexp matching a heredoc operator: its dash, quote and delimiter.")

(defun opencode-changes--simple-command (line position)
  "Return the simple command of shell LINE around POSITION."
  (let ((start (or (cl-position-if (lambda (char) (memq char '(?\; ?& ?| ?\( ?\))))
                                   line :end position :from-end t)
                   -1))
        (end (or (cl-position-if (lambda (char) (memq char '(?\; ?& ?| ?\))))
                                 line :start position)
                 (length line))))
    (substring line (1+ start) end)))

(defun opencode-changes--split (command)
  "Split shell COMMAND into its shell text and its heredoc bodies.
Return a list of (shell TEXT) and (heredoc LANGUAGE FILE TEXT) in order,
where LANGUAGE is that of the program reading the body and FILE the
file the command writes it to, if any."
  (let ((lines (split-string command "\n"))
        segments shell)
    (cl-flet ((flush ()
                (when shell
                  (push (list 'shell (string-join (nreverse shell) "\n")) segments)
                  (setq shell nil))))
      (while lines
        (let* ((line (pop lines))
               ;; A comment, from a # starting a word, holds no heredoc.
               (code (if (string-match "\\(?:\\`\\|[ \t;&|(]\\)#" line)
                         (substring line 0 (match-beginning 0))
                       line))
               (start 0)
               operators)
          (push line shell)
          (while (string-match opencode-changes--heredoc-regexp code start)
            (let ((delimiter (match-string 3 code))
                  (tabs (equal (match-string 1 code) "-"))
                  (position (match-beginning 3)))
              (setq start (match-end 0))
              (push (list delimiter tabs
                          (opencode-changes--heredoc-reader
                           (opencode-changes--simple-command code position)))
                    operators)))
          (dolist (operator (nreverse operators))
            (pcase-let ((`(,delimiter ,tabs (,language . ,file)) operator)
                        (body nil))
              (while (and lines
                          (not (equal (if tabs
                                          (replace-regexp-in-string "\\`\t+" "" (car lines))
                                        (car lines))
                                      delimiter)))
                (push (pop lines) body))
              (flush)
              (push (list 'heredoc language file (string-join (nreverse body) "\n")) segments)
              (when lines (push (pop lines) shell))))))
      (flush))
    (nreverse segments)))

(defun opencode-shell-command-display (command &optional notes)
  "Return shell COMMAND as an expanded block shows it, NOTES lines after.
Its shell lines are verbatim, the first after a prompt sign, and the
body of each heredoc is a code block in the language of what reads it."
  (let ((first t))
    (mapconcat
     (lambda (segment)
       (pcase segment
         (`(shell ,text)
          (prog1 (opencode-fontify-as (concat (if first "$ " "") text) 'verbatim)
            (setq first nil)))
         (`(heredoc ,language ,_file ,text)
          (opencode-fenced-block text language))))
     (append (opencode-changes--split command)
             (when notes (list (list 'shell (string-join notes "\n")))))
     "\n")))

;;;; Python scripts

(defun opencode-changes--python-escape ()
  "Return the text of the Python escape sequence after point, past it."
  (let ((char (char-after)))
    (forward-char)
    (pcase char
      (?\n "")
      (?n "\n") (?t "\t") (?r "\r") (?a "\a") (?b "\b") (?f "\f") (?v "\v")
      ((or ?\\ ?\' ?\") (string char))
      ((guard (and char (<= ?0 char ?7)))
       (let ((digits (string char)))
         (while (and (length< digits 3) (char-after) (<= ?0 (char-after) ?7))
           (setq digits (concat digits (string (char-after))))
           (forward-char))
         (string (string-to-number digits 8))))
      ((and (or ?x ?u ?U) (let size (pcase char (?x 2) (?u 4) (_ 8))))
       (let ((digits (buffer-substring-no-properties (point) (min (point-max) (+ (point) size)))))
         (if (string-match-p (format "\\`[0-9A-Fa-f]\\{%d\\}\\'" size) digits)
             (progn (forward-char size) (string (string-to-number digits 16)))
           (string ?\\ char))))
      (_ (if char (string ?\\ char) "\\")))))

(defun opencode-changes--python-string (prefix quote)
  "Read the Python string after its PREFIX and opening QUOTE, at point.
Return its text, or :unknown for a formatted or unterminated string."
  (let ((raw (string-match-p "[rR]" prefix))
        (formatted (string-match-p "[fF]" prefix))
        (single (length= quote 1))
        (text "")
        done)
    (while (not done)
      (cond
       ((eobp) (setq done 'unterminated))
       ((looking-at-p (regexp-quote quote))
        (forward-char (length quote))
        (setq done t))
       ((and single (eq (char-after) ?\n))
        (setq done 'unterminated))
       ((eq (char-after) ?\\)
        (forward-char)
        (if raw
            (let ((next (char-after)))
              (when next (forward-char))
              (setq text (concat text "\\" (if next (string next) ""))))
          (setq text (concat text (opencode-changes--python-escape)))))
       (t (setq text (concat text (string (char-after))))
          (forward-char))))
    (if (or formatted (not (eq done t))) :unknown text)))

(defun opencode-changes--python-tokens (source)
  "Return the tokens of Python SOURCE, a vector of (TYPE . VALUE).
TYPE is `string', VALUE its text or :unknown; `name' or `op', VALUE
its text; or `newline', ending a logical line.  Adjacent strings are
joined, as Python joins them."
  (let (tokens (depth 0))
    (with-temp-buffer
      (insert source)
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at "[ \t\f]+\\|\\\\\n\\|#.*") (goto-char (match-end 0)))
         ((eq (char-after) ?\n)
          (forward-char)
          (when (and (zerop depth) tokens (not (eq (car (car tokens)) 'newline)))
            (push '(newline) tokens)))
         ((looking-at "\\([rRbBuUfF]\\{0,2\\}\\)\\('''\\|\"\"\"\\|'\\|\"\\)")
          (let ((prefix (match-string 1))
                (quote (match-string 2)))
            (goto-char (match-end 0))
            (let ((text (opencode-changes--python-string prefix quote)))
              (if (eq (car (car tokens)) 'string)
                  (setcdr (car tokens) (if (and (stringp text) (stringp (cdr (car tokens))))
                                           (concat (cdr (car tokens)) text)
                                         :unknown))
                (push (cons 'string text) tokens)))))
         ((looking-at "[A-Za-z_][A-Za-z0-9_]*")
          (push (cons 'name (match-string 0)) tokens)
          (goto-char (match-end 0)))
         ((looking-at "[0-9][0-9A-Za-z_.]*")
          (push (cons 'number (match-string 0)) tokens)
          (goto-char (match-end 0)))
         ((looking-at "==\\|!=\\|<=\\|>=\\|[-+*/%&|^]=\\|\\*\\*\\|//")
          (push (cons 'op (match-string 0)) tokens)
          (goto-char (match-end 0)))
         (t
          (let ((char (char-after)))
            (cond ((memq char '(?\( ?\[ ?\{)) (setq depth (1+ depth)))
                  ((memq char '(?\) ?\] ?\})) (setq depth (max 0 (1- depth)))))
            (push (cons (if (eq char ?\;) 'newline 'op) (string char)) tokens)
            (forward-char))))))
    (vconcat (nreverse tokens))))

(defun opencode-changes--python-replacements (source)
  "Return the literal replacements Python SOURCE makes, and its regexp ones.
The value is (REPLACEMENTS . SUBSTITUTIONS): REPLACEMENTS a list of
\(FILE OLD NEW), FILE the one the script last opened or nil, OLD and
NEW strings or :unknown when not literal; SUBSTITUTIONS the number of
`re.sub' calls."
  (let* ((tokens (opencode-changes--python-tokens source))
         (count (length tokens))
         (bindings (make-hash-table :test #'equal))
         (substitutions 0)
         file replacements)
    (cl-labels ((at (index) (and (< -1 index count) (aref tokens index)))
                (is (index type &optional value)
                  (let ((token (at index)))
                    (and token (eq (car token) type) (or (null value) (equal (cdr token) value)))))
                (boundary (index) (or (< index 0) (>= index count) (is index 'newline)))
                (value (index)
                  (let ((token (at index)))
                    (pcase (car token)
                      ('string (cdr token))
                      ('name (gethash (cdr token) bindings :unknown))
                      (_ :unknown))))
                (path-call (index)
                  ;; Path(ARG) or pathlib.Path(ARG): the index of ARG, or nil.
                  (cond ((and (is index 'name "Path") (is (1+ index) 'op "(")) (+ index 2))
                        ((and (is index 'name "pathlib") (is (1+ index) 'op ".")
                              (is (+ index 2) 'name "Path") (is (+ index 3) 'op "("))
                         (+ index 4)))))
      (dotimes (index count)
        (cond
         ;; NAME = LITERAL, or NAME = Path(LITERAL), alone in its statement.
         ((and (boundary (1- index)) (is index 'name) (is (1+ index) 'op "="))
          (let* ((argument (path-call (+ index 2)))
                 (literal (cond ((and (is (+ index 2) 'string) (boundary (+ index 3))) (+ index 2))
                                ((and argument (is (1+ argument) 'op ")") (boundary (+ argument 2)))
                                 argument))))
            (puthash (cdr (at index)) (if literal (value literal) :unknown) bindings)))
         ;; open(FILE…), Path(FILE), NAME.read_text() and the like: the file at hand.
         ((and (is index 'name "open") (is (1+ index) 'op "(") (not (is (1- index) 'op ".")))
          (let ((name (value (+ index 2))))
            (when (stringp name) (setq file name))))
         ((path-call index)
          (let ((name (value (path-call index))))
            (when (stringp name) (setq file name))))
         ((and (is index 'name) (is (1+ index) 'op ".")
               (is (+ index 2) 'name)
               (member (cdr (at (+ index 2))) '("read_text" "write_text" "read_bytes" "write_bytes" "open")))
          (let ((name (value index)))
            (when (stringp name) (setq file name))))
         ;; .replace(OLD, NEW[, COUNT])
         ((and (is index 'op ".") (is (1+ index) 'name "replace") (is (+ index 2) 'op "(")
               (is (+ index 4) 'op ","))
          (let ((close (if (is (+ index 6) 'op ",") (+ index 8) (+ index 6))))
            (when (is close 'op ")")
              (push (list file (value (+ index 3)) (value (+ index 5))) replacements))))
         ((and (is index 'name "re") (is (1+ index) 'op ".")
               (is (+ index 2) 'name) (member (cdr (at (+ index 2))) '("sub" "subn")))
          (cl-incf substitutions)))))
    (cons (nreverse replacements) substitutions)))

(defun opencode-changes--python-c-scripts (command)
  "Return the scripts COMMAND passes to Python with -c, as the shell reads them."
  (let ((start 0) scripts)
    (while (string-match "\\_<python[0-9.]*[ \t]+\\(?:-[A-Za-z]+[ \t]+\\)*-c[ \t]+\\(['\"]\\)" command start)
      (let ((quote (string-to-char (match-string 1 command)))
            (position (match-end 0))
            (text ""))
        (while (and (length> command position) (not (eq (aref command position) quote)))
          (let ((char (aref command position)))
            (if (and (eq quote ?\") (eq char ?\\) (length> command (1+ position))
                     (memq (aref command (1+ position)) '(?\" ?\\ ?$ ?` ?\n)))
                (progn (unless (eq (aref command (1+ position)) ?\n)
                         (setq text (concat text (string (aref command (1+ position))))))
                       (setq position (+ position 2)))
              (setq text (concat text (string char))
                    position (1+ position)))))
        (push text scripts)
        (setq start (min (length command) (1+ position)))))
    (nreverse scripts)))

(defun opencode-changes--python-scripts (command)
  "Return the Python scripts shell COMMAND runs: heredoc bodies, -c arguments."
  (append (seq-keep (lambda (segment)
                      (pcase segment
                        (`(heredoc "python" ,_file ,text) text)))
                    (opencode-changes--split command))
          (opencode-changes--python-c-scripts command)))

(defun opencode-shell-predicted-changes (command)
  "Return the display of the changes shell COMMAND is predicted to make, or nil.
They are the literal replacements of its Python scripts, each shown as
an edit is."
  (let (replacements (substitutions 0))
    (dolist (script (opencode-changes--python-scripts command))
      (pcase-let ((`(,found . ,regexps) (opencode-changes--python-replacements script)))
        (setq replacements (append replacements found)
              substitutions (+ substitutions regexps))))
    (when replacements
      (let (shown last)
        (pcase-dolist (`(,file ,old ,new) replacements)
          (unless (and shown (equal file last))
            (push (opencode-fontify-as (format "%s:" (if file (opencode-relative-path-for-display file)
                                                       "a file the script names otherwise"))
                                       'verbatim)
                  shown))
          (setq last file)
          (push (if (and (stringp old) (stringp new))
                    (opencode-fenced-block (opencode--format-edit-diff old new) "diff")
                  (opencode-fontify-as "a replacement whose text the script computes" 'verbatim))
                shown))
        (concat (opencode-fontify-as
                 (concat "Predicted from the script's replace calls"
                         (if (> substitutions 0)
                             (format ", not its %d regexp substitution%s" substitutions
                                     (if (= substitutions 1) "" "s"))
                           "")
                         "; what it changes shows once it ran:")
                 'verbatim)
                "\n" (string-join (nreverse shown) "\n"))))))

;;;; Reported changes

(defun opencode-changes-files (metadata)
  "Return the file changes tool call METADATA reports, or nil."
  (let ((files (alist-get 'files metadata)))
    (and (listp files) (seq-every-p #'consp files) files)))

(defun opencode-changes-note (metadata &optional one-file)
  "Return the summary note of the file changes in tool call METADATA, or nil.
With ONE-FILE, for a tool changing one file, the note leaves out the count."
  (when-let* ((files (opencode-changes-files metadata)))
    (let ((count (+ (length files) (or (alist-get 'filesOmitted metadata) 0)))
          (additions (apply #'+ (mapcar (lambda (file) (or (alist-get 'additions file) 0)) files)))
          (deletions (apply #'+ (mapcar (lambda (file) (or (alist-get 'deletions file) 0)) files))))
      (concat (unless one-file (format "%d file%s " count (if (= count 1) "" "s")))
              (format "+%d -%d" additions deletions)))))

(defun opencode-reported-changes (metadata)
  "Return the display of the file changes tool call METADATA reports, or nil.
Each file shows as an edit does, the diff of its hunks."
  (when-let* ((files (opencode-changes-files metadata)))
    (let ((omitted (alist-get 'filesOmitted metadata)))
      (concat
       (mapconcat
        (lambda (file)
          (let-alist file
            (let* ((hunks (opencode-changes--hunks .patch))
                   (counts (delq nil (list (opencode-changes--count .additions "+")
                                           (opencode-changes--count .deletions "-"))))
                   (header (format "%s %s%s:"
                                   (pcase .status ("added" "created") ("deleted" "deleted") (_ "changed"))
                                   (opencode-relative-path-for-display (or .file "?"))
                                   (if counts (format " (%s)" (string-join counts " ")) ""))))
              (concat (opencode-fontify-as header 'verbatim) "\n"
                      (cond (hunks (opencode-fenced-block hunks "diff"))
                            ((and (stringp .patch) (string-empty-p .patch))
                             (opencode-fontify-as "Its diff is too large to show." 'verbatim))
                            (t (opencode-fontify-as "It has no text diff." 'verbatim)))))))
        files "\n")
       (when (and (numberp omitted) (> omitted 0))
         (opencode-fontify-as (format "\n%d more file%s changed, not shown." omitted (if (= omitted 1) "" "s"))
                              'verbatim))))))

(provide 'opencode-changes)
;;; opencode-changes.el ends here
