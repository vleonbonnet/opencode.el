;;; opencode-service-test.el --- Tests of the installed CLI's version  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The CLI is stood for by `opencode-test-with-version-cli', which
;; answers `opencode --version' with recorded output; the real CLI is
;; never run.

;;; Code:

(require 'opencode-test-helpers)

(defvar opencode-test-version-runs nil
  "Arguments of each `call-process' the stand-in CLI answered, newest first.")

(defmacro opencode-test-with-version-cli (output &rest body)
  "Run BODY with a stand-in CLI printing OUTPUT for `opencode --version'.
OUTPUT is a string printed on standard output with exit status 0, or a
cons (STATUS . STRING).  The stand-in is an empty file, which
`executable-find' finds for `opencode-command' and `call-process'
answers for.  BODY may set `cli' to another file, and `answer' to
another OUTPUT."
  (declare (indent 1) (debug t))
  `(let* ((stand-in (make-temp-file "opencode-cli-"))
          (cli stand-in)
          (opencode-command "opencode-test-cli")
          (opencode-service--installed-version nil)
          (opencode-test-version-runs nil)
          (answer ,output))
     (unwind-protect
         (cl-letf* ((executable-find (symbol-function 'executable-find))
                    ((symbol-function 'executable-find)
                     (lambda (command &rest arguments)
                       (if (equal command opencode-command)
                           cli
                         (apply executable-find command arguments))))
                    ((symbol-function 'call-process)
                     (lambda (program _infile destination _display &rest arguments)
                       (should (equal program cli))
                       (push (list :arguments arguments :destination destination
                                   :directory default-directory)
                             opencode-test-version-runs)
                       (let ((answer (if (consp answer) answer (cons 0 answer))))
                         (insert (cdr answer))
                         (car answer)))))
           ,@body)
       (delete-file stand-in))))

(defconst opencode-test-version-outputs
  '(;; OpenCode 2, as this fork's build prints it.
    ("opencode v2.1.4-val.202611011924\n" . "2.1.4-val.202611011924")
    ("opencode v2.1.4\n" . "2.1.4")
    ;; A preview build, versioned by its channel.
    ("opencode v0.0.0-dev-202611011924\n" . "0.0.0-dev-202611011924")
    ("opencode v2.0.0-beta.3+build.7\n" . "2.0.0-beta.3+build.7")
    ;; The Node build, under another name.
    ("opencode2-node v2.1.4\n" . "2.1.4")
    ;; OpenCode 1, and a bare version with a v.
    ("1.14.2\n" . "1.14.2")
    ("v2.1.4\n" . "2.1.4")
    ;; Windows line ends, and no line end.
    ("opencode v2.1.4\r\n" . "2.1.4")
    ("2.1.4" . "2.1.4")
    ;; In bold on a terminal.
    ("\e[1mopencode\e[0m \e[2mv\e[0m\e[1m2.1.4-val.1\e[0m\n" . "2.1.4-val.1")
    ;; Lines around the version.
    ("Warning: config 3.json is deprecated\nopencode v2.1.4\n" . "2.1.4")
    ("opencode v2.1.4\nUpdate available: 2.1.5 (run opencode upgrade)\n" . "2.1.4")
    ("Bun v1.3.0 (macOS arm64)\n  opencode v2.1.4-val.2  \nnote: 9 plugins\n" . "2.1.4-val.2")
    ;; A build without a version, and nothing like one.
    ("opencode vlocal\n" . nil)
    ("" . nil)
    ("error: unknown option\n" . nil))
  "Outputs of `opencode --version' and the version each one reports.")

(ert-deftest opencode-test-installed-version-formats ()
  "The installed version is the whole version the CLI prints, whatever surrounds it."
  (dolist (case opencode-test-version-outputs)
    (opencode-test-with-version-cli (car case)
      (should (equal (cons (car case) (opencode-service-installed-version)) case))
      (should (equal (plist-get (car opencode-test-version-runs) :arguments) '("--version"))))))

(ert-deftest opencode-test-installed-version-run ()
  "The CLI is asked from the home directory, its standard error left out."
  (opencode-test-with-version-cli "opencode v2.1.4\n"
    (let ((default-directory "/ssh:example.invalid:/tmp/"))
      (should (equal (opencode-service-installed-version) "2.1.4")))
    (let ((run (car opencode-test-version-runs)))
      (should (equal (plist-get run :destination) '(t nil)))
      (should (equal (plist-get run :directory) (expand-file-name "~/"))))))

(ert-deftest opencode-test-installed-version-failure ()
  "A CLI failing or missing has no known version."
  (opencode-test-with-version-cli '(1 . "opencode v2.1.4\n")
    (should-not (opencode-service-installed-version)))
  (opencode-test-with-version-cli "opencode v2.1.4\n"
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (signal 'file-missing '("Searching for program")))))
      (should-not (opencode-service-installed-version))))
  (let ((opencode-command "opencode-test-missing-cli")
        (opencode-service--installed-version nil))
    (should-not (opencode-service-installed-version))))

(ert-deftest opencode-test-installed-version-cached ()
  "The CLI is asked again only once its executable changed or moved."
  (opencode-test-with-version-cli "opencode v2.1.4\n"
    (should (equal (opencode-service-installed-version) "2.1.4"))
    (setq answer "opencode v2.1.5\n")
    (should (equal (opencode-service-installed-version) "2.1.4"))
    (should (= (length opencode-test-version-runs) 1))
    ;; A new install.
    (set-file-times cli (time-add (current-time) 10))
    (should (equal (opencode-service-installed-version) "2.1.5"))
    (should (= (length opencode-test-version-runs) 2))
    ;; Another executable of the same time.
    (let ((other (make-temp-file "opencode-cli-")))
      (unwind-protect
          (progn
            (set-file-times other (file-attribute-modification-time (file-attributes cli)))
            (setq cli other
                  answer "opencode v2.1.6\n")
            (should (equal (opencode-service-installed-version) "2.1.6"))
            (should (= (length opencode-test-version-runs) 3)))
        (delete-file other)))))

(ert-deftest opencode-test-service-stale ()
  "A service is stale only when it runs another version than the CLI prints."
  (let ((connection '(:url "http://127.0.0.1:1" :password "secret"
                           :version "2.1.4-val.202611011924"))
        (stale nil))
    (let ((opencode-service-stale-functions
           (list (lambda (service installed) (push (list service installed) stale)))))
      (opencode-test-with-version-cli "opencode v2.1.4-val.202611011924\n"
        (should-not (opencode-service-stale-p connection))
        (opencode-service-check-version connection)
        (should-not stale))
      (opencode-test-with-version-cli "opencode v2.1.4-val.202611021000\n"
        (should (opencode-service-stale-p connection))
        (opencode-service-check-version connection)
        (should (equal stale '(("2.1.4-val.202611011924" "2.1.4-val.202611021000")))))
      ;; An unknown version is not taken for another one.
      (setq stale nil)
      (opencode-test-with-version-cli "opencode vlocal\n"
        (should-not (opencode-service-stale-p connection))
        (opencode-service-check-version connection)
        (should-not stale)))))

(ert-deftest opencode-test-installed-version-process ()
  "A real process's standard error does not reach the version."
  (skip-unless (and (not (memq system-type '(windows-nt ms-dos))) (executable-find "sh")))
  (let* ((directory (make-temp-file "opencode-cli-" t))
         (cli (expand-file-name "opencode" directory))
         (opencode-command cli)
         (opencode-service--installed-version nil))
    (unwind-protect
        (progn
          (with-temp-file cli
            (insert "#!/bin/sh\n"
                    "echo 'warning 9.9' >&2\n"
                    "[ \"$1\" = --version ] && echo 'opencode v2.1.4-val.202611011924'\n"))
          (set-file-modes cli #o755)
          (should (equal (opencode-service-installed-version) "2.1.4-val.202611011924")))
      (delete-directory directory t))))

(provide 'opencode-service-test)
;;; opencode-service-test.el ends here
