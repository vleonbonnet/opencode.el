;;; opencode-search-test.el --- Tests of full-text session search  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; `opencode-search' talks to the fake service of opencode-test-server.el,
;; over sockets as it does to the real one: with the route, without it,
;; as an older service answers, paging, and cancelled in flight.

;;; Code:

(require 'opencode-test-server)

(defun opencode-test-search-page (request)
  "Answer search REQUEST with a page of two matches, or the last one after a cursor."
  (let ((query (url-parse-query-string (or (cadr (split-string (plist-get request :path) "?")) ""))))
    (cons 200
          (if (assoc "cursor" query)
              '((data . [((sessionID . "ses_b") (messageID . "msg_3") (role . "user")
                          (time (created . 3000))
                          (snippet (text . "last walrus") (matches . [((start . 5) (end . 11))])))])
                (cursor . #s(hash-table))
                (index (complete . t)))
            '((data . [((sessionID . "ses_a") (title . "Walruses") (messageID . "msg_1") (role . "user")
                        (time (created . 1500))
                        (snippet (text . "Where is the walrus?") (matches . [((start . 13) (end . 19))])))
                       ((sessionID . "ses_a") (title . "Walruses") (messageID . "msg_2") (role . "assistant")
                        (time (created . 2000))
                        (snippet (text . "The walrus\nsleeps")
                                 (matches . [((start . 4) (end . 10)) ((start . 40) (end . 45))])))])
              (cursor (next . "page-2"))
              (index (complete . :false) (processed . 250) (total . 1000)))))))

(defun opencode-test-search (query &rest arguments)
  "Run `opencode-search' for QUERY with ARGUMENTS and return how it settled.
Return (then MATCHES INFO) or (else REASON MESSAGE)."
  (let (outcome)
    (apply #'opencode-search query
           :then (lambda (matches info) (setq outcome (list 'then matches info)))
           :else (lambda (reason message) (setq outcome (list 'else reason message)))
           arguments)
    (opencode-test-wait-for (lambda () outcome))
    outcome))

(defun opencode-test-search-query (request)
  "Return the query parameters of fake service REQUEST as an alist of strings, in order."
  (mapcar (lambda (pair) (cons (car pair) (decode-coding-string (url-unhex-string (cadr pair)) 'utf-8)))
          (reverse (url-parse-query-string (or (cadr (split-string (plist-get request :path) "?")) "")))))

(ert-deftest opencode-test-search-route ()
  "Matches come as plists with highlighted one-line snippets, filters as the service names them."
  (opencode-test-with-service
    (let ((opencode-search--unsupported nil))
      (setf (opencode-test-service-routes opencode-test-service)
            (list (cons "/api/experimental/session/search" #'opencode-test-search-page)))
      (pcase-let ((`(,outcome ,matches ,info)
                   (opencode-test-search "walrus \"on the beach\"" :archived nil :project "prj_1"
                                         :directory "/tmp/" :parent 'root :limit 2)))
        (should (eq outcome 'then))
        (should (equal (mapcar (lambda (match) (plist-get match :message-id)) matches) '("msg_1" "msg_2")))
        (let ((first (car matches)))
          (should (equal (plist-get first :session-id) "ses_a"))
          (should (equal (plist-get first :title) "Walruses"))
          (should (eq (plist-get first :role) 'user))
          (should (= (plist-get first :time) 1.5))
          (should (equal (plist-get first :snippet) "Where is the walrus?"))
          (should (equal (plist-get first :matches) '((13 . 19))))
          (should (memq 'opencode-search-match
                        (ensure-list (get-text-property 13 'face (plist-get first :snippet))))))
        (let ((second (cadr matches)))
          (should (eq (plist-get second :role) 'assistant))
          ;; One line; a range past the text is dropped.
          (should (equal (plist-get second :snippet) "The walrus sleeps"))
          (should (equal (plist-get second :matches) '((4 . 10)))))
        (should (equal info '(:next "page-2" :complete nil :processed 250 :total 1000))))
      (should (equal (opencode-test-search-query (car (opencode-test-requests-on "/api/experimental/session/search")))
                     `(("q" . "walrus \"on the beach\"") ("archived" . "false") ("project" . "prj_1")
                       ("directory" . ,(opencode-directory-name "/tmp/")) ("parentID" . "null") ("limit" . "2"))))
      (opencode-test-search "walrus" :archived t :parent "ses_p")
      (should (equal (opencode-test-search-query
                      (car (last (opencode-test-requests-on "/api/experimental/session/search"))))
                     '(("q" . "walrus") ("archived" . "true") ("parentID" . "ses_p")))))))

(ert-deftest opencode-test-search-paging ()
  "A page's :next fetches the following one, which has none."
  (opencode-test-with-service
    (let ((opencode-search--unsupported nil))
      (setf (opencode-test-service-routes opencode-test-service)
            (list (cons "/api/experimental/session/search" #'opencode-test-search-page)))
      (let* ((first (opencode-test-search "walrus"))
             (next (plist-get (nth 2 first) :next))
             (second (opencode-test-search "walrus" :cursor next)))
        (should (equal next "page-2"))
        (should (eq (car second) 'then))
        (should (equal (mapcar (lambda (match) (plist-get match :message-id)) (nth 1 second)) '("msg_3")))
        (should (equal (nth 2 second) '(:next nil :complete t :processed nil :total nil)))
        (should (equal (cdr (assoc "cursor" (opencode-test-search-query
                                             (cadr (opencode-test-requests-on "/api/experimental/session/search")))))
                       "page-2"))))))

(ert-deftest opencode-test-search-route-missing ()
  "A service without the route fails as `unsupported', silently, and is not asked again."
  (opencode-test-with-service
    (let ((opencode-search--unsupported nil)
          (messages-start (with-current-buffer (messages-buffer) (point-max))))
      (let ((outcome (opencode-test-search "walrus")))
        (should (eq (car outcome) 'else))
        (should (eq (nth 1 outcome) 'unsupported))
        (should (stringp (nth 2 outcome))))
      (should (= (length (opencode-test-requests-on "/api/experimental/session/search")) 1))
      (should (equal opencode-search--unsupported '("svc_test")))
      ;; Known now: answered without a request, after returning.
      (let* ((outcome nil)
             (returned (progn (opencode-search "walrus" :else (lambda (reason _) (setq outcome reason)))
                              outcome)))
        (should-not returned)
        (opencode-test-wait-for (lambda () outcome))
        (should (eq outcome 'unsupported)))
      ;; Without :else, nothing is reported either.
      (opencode-search "walrus" :then (lambda (&rest _) (ert-fail "Succeeded")))
      (accept-process-output nil 0.1)
      (should (= (length (opencode-test-requests-on "/api/experimental/session/search")) 1))
      (should-not (string-match-p "search" (with-current-buffer (messages-buffer)
                                            (buffer-substring messages-start (point-max))))))))

(ert-deftest opencode-test-search-refused ()
  "A tagged error, such as a malformed cursor, is `invalid'; a server error `failed'."
  (opencode-test-with-service
    (let ((opencode-search--unsupported nil)
          (status 400))
      (setf (opencode-test-service-routes opencode-test-service)
            (list (cons "/api/experimental/session/search"
                        (lambda (_) (cons status '((_tag . "InvalidCursorError") (message . "Invalid cursor")))))))
      (let ((outcome (opencode-test-search "walrus" :cursor "bogus")))
        (should (equal (butlast outcome) '(else invalid)))
        (should (string-match-p "Invalid cursor" (nth 2 outcome))))
      (setq status 500)
      (should (equal (butlast (opencode-test-search "walrus")) '(else failed)))
      (should-not opencode-search--unsupported))))

(ert-deftest opencode-test-search-cancel ()
  "A cancelled search calls neither callback, and closes its request; cancelling later does nothing."
  (opencode-test-with-service
    (let ((opencode-search--unsupported nil)
          (outcome nil))
      (setf (opencode-test-service-routes opencode-test-service)
            (list (cons "/api/experimental/session/search" (lambda (_) '(hang)))
                  (cons "/api/agent" (lambda (_) '(200 . ((data . [])))))))
      ;; Connected first, so the request goes out at once.
      (opencode-request-sync 'get "/api/agent" :timeout 10)
      (let ((cancel (opencode-search "walrus"
                                     :then (lambda (&rest _) (setq outcome 'then))
                                     :else (lambda (&rest _) (setq outcome 'else)))))
        (should (functionp cancel))
        (opencode-test-wait-for (lambda () (opencode-test-requests-on "/api/experimental/session/search")))
        (should (opencode-test-requests-in-flight-p))
        (funcall cancel)
        (accept-process-output nil 0.2)
        (should-not outcome)
        (should-not (opencode-test-requests-in-flight-p))
        (funcall cancel)
        (should-not outcome))
      ;; Cancelling a search that settled changes nothing.
      (setf (opencode-test-service-routes opencode-test-service)
            (list (cons "/api/experimental/session/search" #'opencode-test-search-page)))
      (let ((cancel (opencode-search "walrus" :then (lambda (&rest _) (setq outcome 'then)))))
        (opencode-test-wait-for (lambda () outcome))
        (funcall cancel)
        (should (eq outcome 'then))))))

(provide 'opencode-search-test)
;;; opencode-search-test.el ends here
