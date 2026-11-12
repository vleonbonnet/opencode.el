;;; opencode-changes-test.el --- Tests of command and change displays  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'opencode-test-helpers)

(defun opencode-test-segments (command)
  "Return the segments of COMMAND, without their text properties."
  (mapcar (lambda (segment)
            (mapcar (lambda (part) (if (stringp part) (substring-no-properties part) part)) segment))
          (opencode-changes--split command)))

(ert-deftest opencode-test-heredoc-languages ()
  "Heredoc bodies take the language of what reads them, shell lines stay around them."
  (should (equal (opencode-test-segments "python3 - <<'EOF'\nprint(1)\nEOF\necho done")
                 '((shell "python3 - <<'EOF'") (heredoc "python" nil "print(1)") (shell "EOF\necho done"))))
  (should (equal (opencode-test-segments "cat > src/app.ts <<EOF\nconst a = 1\nEOF")
                 '((shell "cat > src/app.ts <<EOF") (heredoc "typescript" "src/app.ts" "const a = 1")
                   (shell "EOF"))))
  (should (equal (opencode-test-segments "cd x && cat <<'EOF' >> notes.el\n(foo)\nEOF")
                 '((shell "cd x && cat <<'EOF' >> notes.el") (heredoc "emacs-lisp" "notes.el" "(foo)")
                   (shell "EOF"))))
  (should (equal (opencode-test-segments "git apply <<\"PATCH\"\n-a\n+b\nPATCH")
                 '((shell "git apply <<\"PATCH\"") (heredoc "diff" nil "-a\n+b") (shell "PATCH"))))
  (should (equal (opencode-test-segments "env A=1 uv run --quiet python <<-END\n\tx = 1\n\tEND")
                 '((shell "env A=1 uv run --quiet python <<-END") (heredoc "python" nil "\tx = 1")
                   (shell "\tEND"))))
  (should (equal (opencode-test-segments "kubectl apply -f - <<EOF\nkind: Pod\nEOF")
                 '((shell "kubectl apply -f - <<EOF") (heredoc "text" nil "kind: Pod") (shell "EOF"))))
  ;; Here-strings, shifts and comments hold no heredoc; an unterminated body runs to the end.
  (should (equal (opencode-test-segments "grep x <<< \"$a\"; echo $((1<<2)) # <<EOF")
                 '((shell "grep x <<< \"$a\"; echo $((1<<2)) # <<EOF"))))
  (should (equal (opencode-test-segments "node <<'JS'\nconsole.log(1)")
                 '((shell "node <<'JS'") (heredoc "js" nil "console.log(1)")))))

(ert-deftest opencode-test-shell-command-display ()
  "A command shows its shell lines verbatim and its heredocs fenced, notes last."
  (let ((display (opencode-shell-command-display "python3 - <<'EOF'\nprint(1)\nEOF" '("# in sub"))))
    (should (equal (substring-no-properties display)
                   "$ python3 - <<'EOF'\n```python\nprint(1)\n```\nEOF\n# in sub"))
    (should (eq (get-text-property 0 'opencode-fontify display) 'verbatim))
    (should-not (get-text-property (string-search "```" display) 'opencode-fontify display))))

(ert-deftest opencode-test-python-replacements ()
  "Literal replacements are read from bound names and literals, under the file opened last."
  (let ((script (concat "import re\n"
                        "p = 'a.txt'\n"
                        "s = open(p).read()\n"
                        "old = \"\"\"one\ntwo\"\"\"\n"
                        "new = 'uno\\n' 'dos'\n"
                        "s = s.replace(old, new)\n"
                        "s = s.replace(r'\\d', \"\\x41\\t\", 1).replace(f'{x}', 'y')\n"
                        "s = re.sub(r'a', 'b', s)\n"
                        "open(p, 'w').write(s)\n"
                        "q = pathlib.Path(\"b.md\")\n"
                        "t = q.read_text(); t = t.replace('#', '##')  # a comment .replace('no', 'pe')\n"
                        "old = compute()\n"
                        "t = t.replace(old, 'z')\n")))
    (should (equal (opencode-changes--python-replacements script)
                   '((("a.txt" "one\ntwo" "uno\ndos")
                      ("a.txt" "\\d" "A\t")
                      ("a.txt" :unknown "y")
                      ("b.md" "#" "##")
                      ("b.md" :unknown "z"))
                     . 1)))))

(ert-deftest opencode-test-python-c-scripts ()
  "Scripts given with -c are read as the shell passes them."
  (should (equal (opencode-changes--python-c-scripts
                  "python3 -c \"s='a\\\"b'; print(\\$x)\" && python -B -c 'x = \"y\"'")
                 '("s='a\"b'; print($x)" "x = \"y\""))))

(ert-deftest opencode-test-predicted-changes ()
  "Predictions show each replacement as an edit, under its file, labelled as predictions."
  (let ((display (substring-no-properties
                  (opencode-shell-predicted-changes
                   (concat "python3 - <<'EOF'\np='notes.txt'\ns=open(p).read()\n"
                           "s=s.replace('a\\nb', 'a\\nc')\ns=s.replace(x, 'y')\nEOF\n"
                           "python3 -c \"import re; re.sub('a', 'b', '')\"")))))
    (should (equal display
                   (concat "Predicted from the script's replace calls, not its 1 regexp substitution;"
                           " what it changes shows once it ran:\nnotes.txt:\n"
                           (opencode-fenced-block "@@ -1,2 +1,2 @@\n a\n-b\n+c\n" "diff")
                           "\na replacement whose text the script computes"))))
  (should-not (opencode-shell-predicted-changes "sed -i 's/a/b/' x.txt")))

(defconst opencode-test-reported-metadata
  '((status . "completed") (exit . 0)
    (files ((file . "sub/a.txt") (status . "modified") (additions . 1) (deletions . 1)
            (patch . "diff --git a/sub/a.txt b/sub/a.txt\nindex 1..2 100644\n--- a/sub/a.txt\n+++ b/sub/a.txt\n@@ -1 +1 @@\n-one\n+two\n"))
           ((file . "big.txt") (status . "added") (additions . 9000) (deletions . 0) (patch . ""))
           ((file . "logo.png") (status . "deleted") (additions . 0) (deletions . 0)
            (patch . "diff --git a/logo.png b/logo.png\nBinary files a/logo.png and /dev/null differ\n")))
    (filesOmitted . 2))
  "Metadata of a shell call reporting the files it changed.")

(ert-deftest opencode-test-reported-changes ()
  "Reported changes show each file's hunks as edits do, and count the files left out."
  (let ((default-directory temporary-file-directory))
    (should (equal (substring-no-properties (opencode-reported-changes opencode-test-reported-metadata))
                   (concat "changed sub/a.txt (+1 -1):\n"
                           (opencode-fenced-block "@@ -1 +1 @@\n-one\n+two\n" "diff")
                           "\ncreated big.txt (+9000 -0):\nIts diff is too large to show."
                           "\ndeleted logo.png (+0 -0):\nIt has no text diff."
                           "\n2 more files changed, not shown.")))
    (should (equal (opencode-changes-note opencode-test-reported-metadata) "5 files +9001 -1"))
    (should (equal (opencode-changes-note opencode-test-reported-metadata t) "+9001 -1"))
    (should-not (opencode-reported-changes '((status . "completed"))))))

(ert-deftest opencode-test-shell-call-shows-changes ()
  "A shell call shows what it reported changing once finished, its prediction before."
  (let* ((default-directory temporary-file-directory)
         (command "python3 - <<'EOF'\ns=s.replace('a', 'b')\nEOF")
         (input `((command . ,command)))
         (before (substring-no-properties (opencode-format-tool-call "shell" input '((shellID . "sh_1")))))
         (after (substring-no-properties
                 (opencode-format-tool-call "shell" input opencode-test-reported-metadata)))
         (unchanged (substring-no-properties
                     (opencode-format-tool-call "shell" input '((status . "completed") (exit . 0))))))
    (should (string-search "Predicted from the script" before))
    (should-not (string-search "changed sub/a.txt" before))
    (should (string-search "changed sub/a.txt" after))
    (should-not (string-search "Predicted" after))
    (should-not (string-search "Predicted" unchanged))
    (should (string-prefix-p "$ python3 - <<'EOF'\n```python\n" unchanged))))

(ert-deftest opencode-test-write-call-shows-its-diff ()
  "A write shows the diff it reports for a file it changed, its content otherwise."
  (let ((default-directory temporary-file-directory)
        (input '((path . "notes.py") (content . "b\n"))))
    (should (equal (opencode-format-tool-call
                    "write" input
                    '((files ((file . "notes.py") (status . "modified") (additions . 1) (deletions . 1)
                              (patch . "--- notes.py\n+++ notes.py\n@@ -1 +1 @@\n-a\n+b\n")))))
                   (concat "write notes.py:\n" (opencode-fenced-block "@@ -1 +1 @@\n-a\n+b\n" "diff"))))
    (should (equal (opencode-format-tool-call "write" input nil)
                   (concat "write notes.py:\n" (opencode-fenced-block "b\n" "python"))))))

(ert-deftest opencode-test-shell-summary-counts-changes ()
  "A finished shell call's summary counts the files it changed."
  (let ((data `(:type tool :tool "shell" :input ((command . "make fmt")) :status "completed"
                      :metadata ,opencode-test-reported-metadata)))
    (should (string-suffix-p "make fmt · 5 files +9001 -1"
                             (substring-no-properties (opencode-tool-collapsed-summary data))))))

;;; opencode-changes-test.el ends here
