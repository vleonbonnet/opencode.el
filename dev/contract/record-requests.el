;;; record-requests.el --- Record every request the tests make  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Usage: emacs -Q --batch -L . -L test -L DEPS... -l dev/contract/record-requests.el OUT
;;
;; Runs the ERT suite with the fake service's entry points advised to
;; log each request to OUT as a JSON line: method, path, query, body and
;; the test making it.  Session tests answer `opencode-request' without
;; a connection: their query is as `opencode-request' puts it in the URL
;; (nil values dropped, values formatted with %s), their body as it
;; encodes it.  Requests that reach the fake HTTP service are logged as
;; they arrive, decoded from the wire, so that what the HTTP client
;; sends is checked too.  validate.ts then checks them against the
;; service's API document, which the fake service does not know.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defvar record-requests-file (expand-file-name (car command-line-args-left))
  "File the requests are written to.")
(setq command-line-args-left nil)

(let ((root (file-name-directory (directory-file-name (file-name-directory
                                                       (directory-file-name
                                                        (file-name-directory load-file-name)))))))
  (load (expand-file-name "test/opencode-test-helpers.el" root) nil t)
  (dolist (file (directory-files (expand-file-name "test" root) t "-test\\.el\\'"))
    (load file nil t)))

(defun record-requests--entry (method path arguments)
  "Return the JSON line for request METHOD on PATH with ARGUMENTS."
  (let* ((query (seq-filter #'cdr (plist-get arguments :query)))
         (body (plist-get arguments :body))
         (encoded (and body (condition-case err
                                (json-serialize body :null-object :null :false-object :false)
                              (error (list :encode-error (format "%S" err)))))))
    (json-serialize
     `((method . ,(upcase (symbol-name method)))
       (path . ,path)
       (query . ,(if query
                     (mapcar (lambda (pair)
                               (cons (intern (format "%s" (car pair))) (format "%s" (cdr pair))))
                             query)
                   (make-hash-table)))
       ,@(cond ((stringp encoded) `((body . ,encoded)))
               (encoded `((encodeError . ,(plist-get encoded :encode-error)))))
       (test . ,(format "%s" (and (ert-running-test) (ert-test-name (ert-running-test)))))))))

(defun record-requests--wire-entry (request)
  "Return the JSON line for REQUEST as the fake HTTP service received it."
  (pcase-let* ((`(,path ,query) (split-string (plist-get request :path) "?"))
               (body (plist-get request :body)))
    (json-serialize
     `((method . ,(plist-get request :method))
       (path . ,(url-unhex-string path))
       (query . ,(if query
                     (mapcar (lambda (pair)
                               (cons (intern (car pair))
                                     (decode-coding-string (url-unhex-string (or (cadr pair) "")) 'utf-8)))
                             (url-parse-query-string query))
                   (make-hash-table)))
       ,@(unless (string-empty-p body) `((body . ,body)))
       (wire . t)
       (test . ,(format "%s" (and (ert-running-test) (ert-test-name (ert-running-test)))))))))

(write-region "" nil record-requests-file nil 'silent)
(advice-add 'opencode-test--request :before
            (lambda (method path &rest arguments)
              (write-region (concat (record-requests--entry method path arguments) "\n")
                            nil record-requests-file t 'silent)))

(advice-add 'opencode-test--handle :before
            (lambda (_service _connection request)
              (write-region (concat (record-requests--wire-entry request) "\n")
                            nil record-requests-file t 'silent)))

(ert-run-tests-batch-and-exit)

;;; record-requests.el ends here
