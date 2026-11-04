;;; opencode-transport.el --- HTTP over sockets to the OpenCode service  -*- lexical-binding: t; -*-

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

;; `opencode-transport-request' speaks HTTP/1.1 to the local service
;; over sockets Emacs opens itself, with `make-network-process', rather
;; than through a curl process per request.  Starting a process stops
;; Emacs until the kernel has executed it, and in a large, long-running
;; Emacs that took up to seconds: the exec closes every close-on-exec
;; descriptor the new process got a copy of, and closing those of files
;; on a network share, which file notification holds open, waits on the
;; share.  A socket costs no process, so no request stops Emacs.
;;
;; Connections open without waiting, and answers are read as they
;; arrive, however the kernel splits them: framed by Content-Length, by
;; chunked transfer coding, or by the end of the connection.  A
;; connection whose answer completed and that the service keeps alive
;; is kept for the next request to the same service, a few at most, and
;; for less time than the service keeps it.  A request sent on a kept
;; connection that the service closed meanwhile is sent once more on a
;; new one, as long as no answer had begun.
;;
;; Exactly one of THEN and ELSE runs for each request, from the filter
;; or sentinel of its connection, from a timer, or from
;; `opencode-transport-cancel'; never before `opencode-transport-request'
;; returns.  ELSE receives an `opencode-transport-failure'.  A request
;; given a FILTER is a stream: its body goes to FILTER as it arrives.
;; Every connection's buffer dies with its connection.
;;
;; A URL relay://NAME/... goes to the relay registered as NAME
;; (`opencode-transport-register-relay'), a function carrying requests
;; to a service Emacs reaches otherwise, such as another host's,
;; through a link of its own; it answers through
;; `opencode-transport-relay-answer' and its kin, and the request
;; settles as any other.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-parse)

(defcustom opencode-transport-idle-connections 4
  "Most connections to the service kept open, idle, for later requests."
  :type 'natnum
  :group 'opencode)

(defcustom opencode-transport-connect-timeout 5
  "Default seconds allowed for connecting to the service."
  :type 'number
  :group 'opencode)

(defconst opencode-transport--keep-alive 4
  "Seconds an idle connection is kept when the service announces no timeout.
A connection is kept a second less than the service announces, so that
a request rarely meets one the service is closing.")

(defconst opencode-transport--head-limit (* 64 1024)
  "Largest size of an answer's status line and headers.")

(defvar opencode-transport--idle (make-hash-table :test #'equal)
  "Idle connections kept for later requests, by \"HOST:PORT\", the latest first.")

(defvar opencode-transport--relays (make-hash-table :test #'equal)
  "Functions relaying the requests of URLs relay://NAME, by NAME.")

(defvar opencode-transport--relayed (make-hash-table :test #'eq :weakness 'key)
  "Exchanges a relay carries, mapped to the function abandoning each, or t.")

(cl-defstruct (opencode-transport-failure (:constructor opencode-transport-failure--make)
                                          (:copier nil))
  "Why a request failed.
REASON is `http' for an answer whose STATUS is not 2xx, `refused' when
no connection could be made, `closed' when the service closed the
connection before any answer, `truncated' when it closed it during one,
`deleted' when something in Emacs deleted the connection, `timeout',
`cancelled', `protocol' for what is not an HTTP answer, and `unreadable'
when the answer could not be decoded.  HEADERS, an alist with downcased
names, and BODY, a decoded string, are those of the answer, if any.
MESSAGE describes the failure."
  reason status headers body message)

(cl-defstruct (opencode-transport-exchange (:constructor opencode-transport--make-exchange)
                                           (:copier nil))
  "A request and the state of its answer."
  method key host port bytes as then else filter
  timeout connect-timeout timer connect-timer connection
  settled sent received reused retried busy
  phase version status reason headers framing remaining body-end chunk-phase
  keep-alive keep-alive-timeout)

(defun opencode-transport-failure-describe (failure)
  "Return a one-line description of FAILURE."
  (or (opencode-transport-failure-message failure)
      (if-let* ((status (opencode-transport-failure-status failure)))
          (format "HTTP %s" status)
        (symbol-name (opencode-transport-failure-reason failure)))))

(defun opencode-transport-live-p (exchange)
  "Return non-nil while EXCHANGE has not settled."
  (and (opencode-transport-exchange-p exchange)
       (not (opencode-transport-exchange-settled exchange))))

;;;; Connections

(defun opencode-transport--close (connection)
  "Close CONNECTION for good, and kill its buffer."
  (let ((buffer (process-buffer connection)))
    (opencode-transport--unpool connection)
    (process-put connection 'opencode-transport-exchange nil)
    (when (process-live-p connection)
      (delete-process connection))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun opencode-transport--unpool (connection)
  "Remove CONNECTION from the idle connections."
  (when-let* ((key (process-get connection 'opencode-transport-key))
              ((memq connection (gethash key opencode-transport--idle))))
    (let ((rest (delq connection (gethash key opencode-transport--idle))))
      (if rest
          (puthash key rest opencode-transport--idle)
        (remhash key opencode-transport--idle)))))

(defun opencode-transport--take-idle (key)
  "Return an open idle connection to KEY's service, or nil.
Connections kept too long are closed on the way."
  (let ((now (float-time))
        found)
    (while (and (not found) (gethash key opencode-transport--idle))
      (let ((connection (car (gethash key opencode-transport--idle))))
        (opencode-transport--unpool connection)
        (if (and (eq (process-status connection) 'open)
                 (buffer-live-p (process-buffer connection))
                 (< now (or (process-get connection 'opencode-transport-idle-until) 0)))
            (setq found connection)
          (opencode-transport--close connection))))
    found))

(defun opencode-transport--release (connection exchange)
  "Keep CONNECTION, whose answer to EXCHANGE completed, for a later request.
It is closed instead when enough are kept already."
  (let* ((key (process-get connection 'opencode-transport-key))
         (idle (gethash key opencode-transport--idle))
         (announced (opencode-transport-exchange-keep-alive-timeout exchange))
         (limit (if announced (max 0 (1- announced)) opencode-transport--keep-alive)))
    (if (or (not (eq (process-status connection) 'open))
            (<= limit 0)
            (>= (length idle) opencode-transport-idle-connections))
        (opencode-transport--close connection)
      (process-put connection 'opencode-transport-exchange nil)
      (process-put connection 'opencode-transport-idle-until (+ (float-time) limit))
      (puthash key (cons connection idle) opencode-transport--idle))))

(defun opencode-transport-close-idle ()
  "Close every idle connection."
  (let (connections)
    (maphash (lambda (_key idle) (setq connections (append idle connections)))
             opencode-transport--idle)
    (mapc #'opencode-transport--close connections)
    (clrhash opencode-transport--idle)))

(defun opencode-transport-idle-count ()
  "Return the number of idle connections kept."
  (let ((count 0))
    (maphash (lambda (_key idle) (cl-incf count (length idle))) opencode-transport--idle)
    count))

;;;; Settling

(defun opencode-transport--fail (exchange reason message &rest slots)
  "Settle EXCHANGE with a failure of REASON and MESSAGE.
SLOTS are further `opencode-transport-failure' slots."
  (opencode-transport--settle
   exchange nil
   (apply #'opencode-transport-failure--make :reason reason :message message slots)))

(defun opencode-transport--settle (exchange success value)
  "Settle EXCHANGE once: call its THEN with VALUE when SUCCESS, else its ELSE."
  (unless (opencode-transport-exchange-settled exchange)
    (setf (opencode-transport-exchange-settled exchange) t)
    (dolist (timer (list (opencode-transport-exchange-timer exchange)
                         (opencode-transport-exchange-connect-timer exchange)))
      (when (timerp timer) (cancel-timer timer)))
    (funcall (if success
                 (opencode-transport-exchange-then exchange)
               (opencode-transport-exchange-else exchange))
             value)))

(defun opencode-transport--abandon (exchange)
  "Detach EXCHANGE from its connection, and close it: its answer cannot finish.
A relayed exchange is abandoned by its relay."
  (when-let* ((cancel (gethash exchange opencode-transport--relayed)))
    (remhash exchange opencode-transport--relayed)
    (when (functionp cancel)
      (condition-case err
          (funcall cancel)
        (error (message "OpenCode: a relay failed to abandon a request: %s"
                        (error-message-string err))))))
  (when-let* ((connection (opencode-transport-exchange-connection exchange)))
    (setf (opencode-transport-exchange-connection exchange) nil)
    (when (eq (process-get connection 'opencode-transport-exchange) exchange)
      (opencode-transport--close connection))))

(defun opencode-transport-cancel (exchange)
  "Abandon EXCHANGE, unless it settled already.
Its connection closes, and its ELSE receives a `cancelled' failure
before this returns."
  (when (opencode-transport-live-p exchange)
    (opencode-transport--abandon exchange)
    (opencode-transport--fail exchange 'cancelled "request cancelled")))

(defun opencode-transport--expire (exchange connecting)
  "Fail EXCHANGE for taking too long; CONNECTING when it is still connecting."
  (when (and (opencode-transport-live-p exchange)
             (or (not connecting) (not (opencode-transport-exchange-sent exchange))))
    (opencode-transport--abandon exchange)
    (opencode-transport--fail
     exchange 'timeout
     (if connecting
         (format "no connection to %s:%s within %s s"
                 (opencode-transport-exchange-host exchange)
                 (opencode-transport-exchange-port exchange)
                 (opencode-transport-exchange-connect-timeout exchange))
       (format "no complete answer within %s s" (opencode-transport-exchange-timeout exchange))))))

;;;; Sending

(defun opencode-transport--reset (exchange)
  "Forget what EXCHANGE read of an answer, to read one anew."
  (setf (opencode-transport-exchange-sent exchange) nil
        (opencode-transport-exchange-received exchange) nil
        (opencode-transport-exchange-phase exchange) 'head
        (opencode-transport-exchange-status exchange) nil
        (opencode-transport-exchange-headers exchange) nil
        (opencode-transport-exchange-framing exchange) nil
        (opencode-transport-exchange-body-end exchange) nil))

(defun opencode-transport--dispatch (exchange fresh)
  "Send EXCHANGE on an idle connection to its service, or on a new one.
FRESH non-nil sends it on a new one."
  (if-let* (((not fresh))
            (connection (opencode-transport--take-idle (opencode-transport-exchange-key exchange))))
      (progn
        (opencode-transport--reset exchange)
        (setf (opencode-transport-exchange-connection exchange) connection
              (opencode-transport-exchange-reused exchange) t)
        (process-put connection 'opencode-transport-exchange exchange)
        (opencode-transport--send exchange connection))
    (opencode-transport--connect exchange)))

(defun opencode-transport--connect (exchange)
  "Open a new connection for EXCHANGE, which is sent once it is open."
  (opencode-transport--reset exchange)
  (setf (opencode-transport-exchange-reused exchange) nil)
  (let ((buffer (generate-new-buffer " *opencode-transport*" t)))
    (with-current-buffer buffer
      (set-buffer-multibyte nil))
    (condition-case err
        (let ((connection (make-network-process
                           :name "opencode-transport" :buffer buffer
                           :host (opencode-transport-exchange-host exchange)
                           :service (opencode-transport-exchange-port exchange)
                           :nowait t :noquery t :coding 'binary :nodelay t
                           :filter #'opencode-transport--filter
                           :sentinel #'opencode-transport--sentinel)))
          (process-put connection 'opencode-transport-key (opencode-transport-exchange-key exchange))
          (process-put connection 'opencode-transport-exchange exchange)
          (setf (opencode-transport-exchange-connection exchange) connection)
          (when-let* ((seconds (opencode-transport-exchange-connect-timeout exchange)))
            (setf (opencode-transport-exchange-connect-timer exchange)
                  (run-at-time seconds nil #'opencode-transport--expire exchange t))))
      (error
       (kill-buffer buffer)
       ;; Its callers expect it to settle later, never during their call.
       (run-at-time 0 nil #'opencode-transport--fail exchange 'refused
                    (format "cannot connect to %s:%s: %s"
                            (opencode-transport-exchange-host exchange)
                            (opencode-transport-exchange-port exchange)
                            (error-message-string err)))))))

(defun opencode-transport--send (exchange connection)
  "Send EXCHANGE's request on CONNECTION, which is open."
  (let ((timer (opencode-transport-exchange-connect-timer exchange)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (opencode-transport-exchange-sent exchange) t)
  (condition-case err
      (process-send-string connection (opencode-transport-exchange-bytes exchange))
    (error (opencode-transport--lost exchange connection (error-message-string err)))))

;;;; Reading

(defun opencode-transport--filter (connection output)
  "Read OUTPUT of CONNECTION as part of the answer it waits for."
  (let ((exchange (process-get connection 'opencode-transport-exchange))
        (buffer (process-buffer connection)))
    (cond
     ;; Nothing should arrive on an idle connection.
     ((null exchange) (opencode-transport--close connection))
     ((not (buffer-live-p buffer))
      (opencode-transport--lost exchange connection "connection buffer killed"))
     (t
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert output))
      (setf (opencode-transport-exchange-received exchange) t)
      (opencode-transport--advance exchange connection nil)))))

(defun opencode-transport--sentinel (connection event)
  "Send the request once CONNECTION opens; settle its exchange when it ends.
EVENT says what happened."
  (let ((exchange (process-get connection 'opencode-transport-exchange)))
    (cond
     ((eq (process-status connection) 'open)
      (when (and exchange (not (opencode-transport-exchange-sent exchange)))
        (opencode-transport--send exchange connection)))
     ((process-live-p connection))
     ((null exchange)
      (opencode-transport--close connection))
     ;; Reading the answer called a stream's filter, which waited for output.
     ((opencode-transport-exchange-busy exchange)
      (run-at-time 0 nil #'opencode-transport--sentinel connection event))
     ((not (opencode-transport-exchange-sent exchange))
      (opencode-transport--close connection)
      (setf (opencode-transport-exchange-connection exchange) nil)
      (opencode-transport--fail exchange 'refused
                                (format "cannot connect to %s:%s (%s)"
                                        (opencode-transport-exchange-host exchange)
                                        (opencode-transport-exchange-port exchange)
                                        (string-trim event))))
     (t
      ;; The end of the connection ends an answer framed by it.
      (when (buffer-live-p (process-buffer connection))
        (opencode-transport--advance exchange connection t))
      (when (opencode-transport-live-p exchange)
        (if (string-prefix-p "deleted" event)
            ;; Something in Emacs deleted it: sending again would be its undoing.
            (progn
              (setf (opencode-transport-exchange-connection exchange) nil)
              (opencode-transport--fail exchange 'deleted
                                        (if (opencode-transport-exchange-received exchange)
                                            "connection deleted during the answer"
                                          "connection deleted without an answer")))
          (opencode-transport--lost exchange connection "connection closed")))
      (opencode-transport--close connection)))))

(defun opencode-transport--lost (exchange connection text)
  "Handle CONNECTION ending, as TEXT says, while EXCHANGE waited on it.
A request sent on a kept connection that ended before any answer is
sent once more on a new one: the service closed it as idle."
  (when (opencode-transport-live-p exchange)
    (setf (opencode-transport-exchange-connection exchange) nil)
    (opencode-transport--close connection)
    (cond
     ((and (opencode-transport-exchange-reused exchange)
           (not (opencode-transport-exchange-received exchange))
           (not (opencode-transport-exchange-retried exchange)))
      (setf (opencode-transport-exchange-retried exchange) t)
      (opencode-transport--connect exchange))
     ((not (opencode-transport-exchange-received exchange))
      (opencode-transport--fail exchange 'closed (format "%s without an answer" text)))
     (t
      (opencode-transport--fail exchange 'truncated (format "%s during the answer" text))))))

(defun opencode-transport--advance (exchange connection ended)
  "Read what CONNECTION's buffer holds of EXCHANGE's answer.
ENDED is non-nil once the connection ended.  The exchange settles when
its answer is complete or malformed.  A stream's filter waiting for
output calls this again from within: the outer call reads what arrived."
  (unless (opencode-transport-exchange-busy exchange)
    (setf (opencode-transport-exchange-busy exchange) t)
    (pcase (unwind-protect
               (condition-case err
                   (with-current-buffer (process-buffer connection)
                     (opencode-transport--parse exchange ended))
                 (opencode-transport-protocol (list 'protocol (cadr err)))
                 (error (list 'protocol (error-message-string err))))
             (setf (opencode-transport-exchange-busy exchange) nil))
      ('done (opencode-transport--complete exchange connection))
      (`(protocol ,text)
       (when (opencode-transport-live-p exchange)
         (opencode-transport--abandon exchange)
         (opencode-transport--fail exchange 'protocol (format "malformed answer: %s" text)))))))

(define-error 'opencode-transport-protocol "Malformed HTTP answer")

(defun opencode-transport--header (exchange name)
  "Return the value of EXCHANGE's answer header NAME, lower case, or nil."
  (cdr (assoc name (opencode-transport-exchange-headers exchange))))

(defun opencode-transport--streaming-p (exchange)
  "Return non-nil when EXCHANGE's body goes to its filter as it arrives."
  (and (opencode-transport-exchange-filter exchange)
       (<= 200 (opencode-transport-exchange-status exchange) 299)))

(defun opencode-transport--parse-head (exchange end)
  "Read EXCHANGE's status line and headers, which end at END in the current buffer."
  (let* ((lines (split-string (buffer-substring-no-properties (point-min) end) "\r\n"))
         (start (car lines))
         headers)
    (unless (string-match "\\`HTTP/\\([0-9]\\)\\.\\([0-9]\\) +\\([0-9][0-9][0-9]\\)\\(?: +\\(.*\\)\\)?\\'"
                          start)
      (signal 'opencode-transport-protocol (list (format "status line %S" (string-limit start 80)))))
    (setf (opencode-transport-exchange-version exchange)
          (+ (* 10 (string-to-number (match-string 1 start))) (string-to-number (match-string 2 start)))
          (opencode-transport-exchange-status exchange) (string-to-number (match-string 3 start))
          (opencode-transport-exchange-reason exchange) (match-string 4 start))
    (dolist (line (cdr lines))
      (cond
       ((string-match "\\`\\([^: \t]+\\)[ \t]*:[ \t]*\\(.*?\\)[ \t]*\\'" line)
        (push (cons (downcase (match-string 1 line)) (match-string 2 line)) headers))
       ;; A folded line continues the previous header.
       ((and headers (string-match "\\`[ \t]+\\(.*\\)\\'" line))
        (setcdr (car headers) (concat (cdar headers) " " (match-string 1 line))))
       (t (signal 'opencode-transport-protocol (list (format "header %S" (string-limit line 80)))))))
    (setf (opencode-transport-exchange-headers exchange) (nreverse headers))))

(defun opencode-transport--frame (exchange)
  "Set how EXCHANGE's answer body is framed, and whether its connection stays open."
  (let* ((status (opencode-transport-exchange-status exchange))
         (encoding (opencode-transport--header exchange "transfer-encoding"))
         (lengths (delete-dups (mapcar #'cdr (seq-filter (lambda (header) (equal (car header) "content-length"))
                                                          (opencode-transport-exchange-headers exchange)))))
         (connection (downcase (or (opencode-transport--header exchange "connection") "")))
         (framing
          (cond
           ((or (eq (opencode-transport-exchange-method exchange) 'head)
                (memql status '(204 304)))
            'none)
           (encoding
            (if (string-match-p "chunked[ \t]*\\'" (downcase encoding)) 'chunked 'close))
           ((cdr lengths)
            (signal 'opencode-transport-protocol (list "conflicting Content-Length headers")))
           (lengths
            (unless (string-match-p "\\`[0-9]+\\'" (car lengths))
              (signal 'opencode-transport-protocol (list (format "Content-Length %S" (car lengths)))))
            (setf (opencode-transport-exchange-remaining exchange) (string-to-number (car lengths)))
            'length)
           (t 'close))))
    (setf (opencode-transport-exchange-framing exchange) framing
          (opencode-transport-exchange-chunk-phase exchange) 'size
          (opencode-transport-exchange-body-end exchange) (point-min)
          (opencode-transport-exchange-keep-alive exchange)
          (and (not (eq framing 'close))
               (if (>= (opencode-transport-exchange-version exchange) 11)
                   (not (string-match-p "\\<close\\>" connection))
                 (string-match-p "\\<keep-alive\\>" connection)))
          (opencode-transport-exchange-keep-alive-timeout exchange)
          (when-let* ((keep-alive (opencode-transport--header exchange "keep-alive"))
                      ((string-match "\\<timeout=\\([0-9]+\\)" keep-alive)))
            (string-to-number (match-string 1 keep-alive))))))

(defun opencode-transport--take (exchange count)
  "Count COUNT bytes from the body's end in the current buffer as EXCHANGE's body.
A stream's are passed to its filter and deleted; others are kept."
  (let ((start (opencode-transport-exchange-body-end exchange)))
    (if (not (opencode-transport--streaming-p exchange))
        (setf (opencode-transport-exchange-body-end exchange) (+ start count))
      (let ((chunk (buffer-substring-no-properties start (+ start count))))
        (delete-region start (+ start count))
        (condition-case err
            (funcall (opencode-transport-exchange-filter exchange) exchange chunk)
          (error (message "OpenCode: stream filter failed: %s" (error-message-string err))))
        ;; The filter may have cancelled the stream.
        (unless (opencode-transport-live-p exchange)
          (throw 'opencode-transport--parsed nil))))))

(defun opencode-transport--parse-chunks (exchange)
  "Read the chunks of EXCHANGE's body; return non-nil once the last one was read."
  (let ((done nil)
        (more nil))
    (while (not (or done more))
      (let ((start (opencode-transport-exchange-body-end exchange)))
        (pcase (opencode-transport-exchange-chunk-phase exchange)
          ('size
           (goto-char start)
           (if (not (search-forward "\r\n" nil t))
               (progn
                 (when (> (- (point-max) start) 1024)
                   (signal 'opencode-transport-protocol (list "chunk size line too long")))
                 (setq more t))
             (let ((line (buffer-substring-no-properties start (- (point) 2))))
               (unless (string-match "\\`[ \t]*\\([0-9a-fA-F]+\\)[ \t]*\\(?:;.*\\)?\\'" line)
                 (signal 'opencode-transport-protocol (list (format "chunk size %S" (string-limit line 40)))))
               (let ((size (string-to-number (match-string 1 line) 16)))
                 (delete-region start (point))
                 (setf (opencode-transport-exchange-remaining exchange) size
                       (opencode-transport-exchange-chunk-phase exchange)
                       (if (zerop size) 'trailer 'data))))))
          ('data
           (let ((count (min (- (point-max) start) (opencode-transport-exchange-remaining exchange))))
             (cond
              ((> count 0)
               (cl-decf (opencode-transport-exchange-remaining exchange) count)
               (opencode-transport--take exchange count))
              ((> (opencode-transport-exchange-remaining exchange) 0) (setq more t))
              (t (setf (opencode-transport-exchange-chunk-phase exchange) 'end)))))
          ('end
           (cond
            ((< (- (point-max) start) 2) (setq more t))
            ((equal (buffer-substring-no-properties start (+ start 2)) "\r\n")
             (delete-region start (+ start 2))
             (setf (opencode-transport-exchange-chunk-phase exchange) 'size))
            (t (signal 'opencode-transport-protocol (list "chunk not followed by CRLF")))))
          ('trailer
           (goto-char start)
           (cond
            ((not (search-forward "\r\n" nil t))
             (when (> (- (point-max) start) opencode-transport--head-limit)
               (signal 'opencode-transport-protocol (list "trailer too long")))
             (setq more t))
            ((= (point) (+ start 2))
             (delete-region start (point))
             (setq done t))
            (t (delete-region start (point))))))))
    done))

(defun opencode-transport--parse (exchange ended)
  "Read EXCHANGE's answer from the current buffer, its connection's.
ENDED is non-nil once the connection ended.  Return `done' once the
answer is complete, nil while it is not."
  (catch 'opencode-transport--parsed
    (while t
      (pcase (opencode-transport-exchange-phase exchange)
        ('head
         (goto-char (point-min))
         (if (not (search-forward "\r\n\r\n" nil t))
             (progn
               (when (> (buffer-size) opencode-transport--head-limit)
                 (signal 'opencode-transport-protocol (list "headers too long")))
               (throw 'opencode-transport--parsed nil))
           (let ((end (point)))
             (opencode-transport--parse-head exchange (- end 4))
             (delete-region (point-min) end)
             (let ((status (opencode-transport-exchange-status exchange)))
               (cond
                ;; An interim answer precedes the answer.
                ((and (<= 100 status 199) (/= status 101)))
                ((= status 101)
                 (signal 'opencode-transport-protocol (list "unexpected protocol switch")))
                (t
                 (opencode-transport--frame exchange)
                 (setf (opencode-transport-exchange-phase exchange) 'body)))))))
        ('body
         (let ((available (- (point-max) (opencode-transport-exchange-body-end exchange))))
           (pcase (opencode-transport-exchange-framing exchange)
             ('none (throw 'opencode-transport--parsed 'done))
             ('length
              (let ((count (min available (opencode-transport-exchange-remaining exchange))))
                (cond
                 ((> count 0)
                  (cl-decf (opencode-transport-exchange-remaining exchange) count)
                  (opencode-transport--take exchange count))
                 ((zerop (opencode-transport-exchange-remaining exchange))
                  (throw 'opencode-transport--parsed 'done))
                 (t (throw 'opencode-transport--parsed nil)))))
             ('close
              (if (> available 0)
                  (opencode-transport--take exchange available)
                (throw 'opencode-transport--parsed (and ended 'done))))
             ('chunked
              (throw 'opencode-transport--parsed (and (opencode-transport--parse-chunks exchange) 'done))))))
        (_ (throw 'opencode-transport--parsed nil))))))

;;;; Answering

(defun opencode-transport--coding (exchange)
  "Return the coding system of EXCHANGE's answer body, UTF-8 unless it says so."
  (or (when-let* ((type (opencode-transport--header exchange "content-type"))
                  ((string-match "charset=\"?\\([^\";[:space:]]+\\)" type))
                  (coding (intern (downcase (match-string 1 type))))
                  ((coding-system-p coding)))
        coding)
      'utf-8))

(defun opencode-transport--decompress (exchange body)
  "Return BODY of EXCHANGE without its content coding."
  (pcase (downcase (or (opencode-transport--header exchange "content-encoding") "identity"))
    ("identity" body)
    ((or "gzip" "x-gzip" "deflate")
     (unless (and (fboundp 'zlib-available-p) (zlib-available-p))
       (error "Compressed answer, and Emacs has no zlib"))
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (insert body)
       (unless (zlib-decompress-region (point-min) (point-max))
         (error "Cannot decompress the answer"))
       (buffer-string)))
    (coding (error "Unknown content coding %s" coding))))

(defun opencode-transport--complete (exchange connection)
  "Settle EXCHANGE with its answer, complete in CONNECTION's buffer.
The connection is kept for another request when it can be."
  (when (opencode-transport-live-p exchange)
    (let* ((buffer (process-buffer connection))
           (end (opencode-transport-exchange-body-end exchange))
           (body (with-current-buffer buffer (buffer-substring-no-properties (point-min) end)))
           (leftover (with-current-buffer buffer (> (point-max) end))))
      (with-current-buffer buffer (erase-buffer))
      (setf (opencode-transport-exchange-connection exchange) nil)
      (if (and (opencode-transport-exchange-keep-alive exchange) (not leftover))
          (opencode-transport--release connection exchange)
        (opencode-transport--close connection))
      (opencode-transport--answer exchange body))))

(defun opencode-transport--answer (exchange body)
  "Settle EXCHANGE with its complete answer, whose body is BODY, still encoded."
  (let ((status (opencode-transport-exchange-status exchange))
        (as (opencode-transport-exchange-as exchange)))
    (pcase-let ((`(,ok . ,value)
                 (condition-case err
                     (let ((text (decode-coding-string (opencode-transport--decompress exchange body)
                                                       (opencode-transport--coding exchange) t)))
                       (cond
                        ((not (<= 200 status 299)) (cons 'http text))
                        ((opencode-transport--streaming-p exchange) (cons t nil))
                        ((functionp as)
                         (cons t (with-temp-buffer
                                   (insert text)
                                   (goto-char (point-min))
                                   (funcall as))))
                        (t (cons t text))))
                   (error (cons nil (error-message-string err))))))
      (pcase ok
        ('t (opencode-transport--settle exchange t value))
        ('http (opencode-transport--fail exchange 'http (opencode-transport-exchange-reason exchange)
                                         :status status :body value
                                         :headers (opencode-transport-exchange-headers exchange)))
        (_ (opencode-transport--fail exchange 'unreadable (format "unreadable response: %s" value)
                                     :status status
                                     :headers (opencode-transport-exchange-headers exchange)))))))

;;;; Requests

(defun opencode-transport--request-bytes (method host port path headers body)
  "Return the bytes of a request of METHOD for PATH on HOST and PORT.
HEADERS is an alist of names and values; BODY, a string, or nil."
  (let ((body (cond ((null body) nil)
                    ((multibyte-string-p body) (encode-coding-string body 'utf-8))
                    (t body)))
        (names (mapcar (lambda (header) (downcase (car header))) headers)))
    (dolist (header headers)
      (when (string-match-p "[\r\n]" (concat (car header) (cdr header)))
        (error "Header %s holds a line break" (car header))))
    (concat
     (encode-coding-string
      (concat (upcase (symbol-name method)) " " path " HTTP/1.1\r\n"
              "Host: " (if (string-search ":" host) (format "[%s]" host) host)
              ":" (number-to-string port) "\r\n"
              (mapconcat (lambda (header) (concat (car header) ": " (cdr header) "\r\n")) headers "")
              (unless (member "accept-encoding" names) "Accept-Encoding: identity\r\n")
              (when (or body (memq method '(post put patch)))
                (format "Content-Length: %d\r\n" (length body)))
              "\r\n")
      'utf-8)
     body)))

(defun opencode-transport-register-relay (name function)
  "Have FUNCTION carry the requests of URLs relay://NAME/....
FUNCTION is called with the exchange, the method, a symbol, the path
with its query, the headers, an alist, and the body, bytes or nil; it
returns a function of no argument abandoning the request, or nil.  It
settles the exchange once, later, with `opencode-transport-relay-answer'
or `opencode-transport-relay-fail'; a stream, an exchange given a
filter, gets its body through `opencode-transport-relay-chunk' first
and ends with `opencode-transport-relay-end'.  Registering NAME again
replaces its function."
  (puthash name function opencode-transport--relays))

(defun opencode-transport-unregister-relay (name)
  "Forget the relay NAME: requests of its URLs fail, refused."
  (remhash name opencode-transport--relays))

(defun opencode-transport--relay (exchange name method path headers body)
  "Hand EXCHANGE, METHOD on PATH with HEADERS and BODY, to the relay NAME."
  (if-let* ((relay (gethash name opencode-transport--relays)))
      (progn
        (puthash exchange t opencode-transport--relayed)
        (setf (opencode-transport-exchange-sent exchange) t)
        (condition-case err
            (let ((cancel (funcall relay exchange method path headers body)))
              (when (and (functionp cancel) (gethash exchange opencode-transport--relayed))
                (puthash exchange cancel opencode-transport--relayed)))
          (error
           (remhash exchange opencode-transport--relayed)
           (run-at-time 0 nil #'opencode-transport--fail exchange 'refused
                        (format "relay %s failed: %s" name (error-message-string err))))))
    (run-at-time 0 nil #'opencode-transport--fail exchange 'refused (format "no relay %s" name))))

(defun opencode-transport-relay-answer (exchange status body &optional headers)
  "Settle relayed EXCHANGE with its answer: STATUS, BODY, a string, and HEADERS.
HEADERS is an alist of downcased names and values.  A 2xx answer goes
to its THEN, through its AS when it has one, others to its ELSE as an
`http' failure, as for any request."
  (when (opencode-transport-live-p exchange)
    (remhash exchange opencode-transport--relayed)
    (let ((as (opencode-transport-exchange-as exchange))
          (text (or body "")))
      (pcase-let ((`(,ok . ,value)
                   (condition-case err
                       (cond
                        ((not (and (integerp status) (<= 200 status 299))) (cons 'http text))
                        ((opencode-transport-exchange-filter exchange) (cons t nil))
                        ((functionp as)
                         (cons t (with-temp-buffer
                                   (insert text)
                                   (goto-char (point-min))
                                   (funcall as))))
                        (t (cons t text)))
                     (error (cons nil (error-message-string err))))))
        (pcase ok
          ('t (opencode-transport--settle exchange t value))
          ('http (opencode-transport--fail exchange 'http (format "HTTP %s" status)
                                           :status status :body value :headers headers))
          (_ (opencode-transport--fail exchange 'unreadable (format "unreadable response: %s" value)
                                       :status status :headers headers)))))))

(defun opencode-transport-relay-chunk (exchange bytes)
  "Give relayed stream EXCHANGE the next piece of its body, BYTES."
  (when-let* (((opencode-transport-live-p exchange))
              (filter (opencode-transport-exchange-filter exchange)))
    (setf (opencode-transport-exchange-received exchange) t)
    (funcall filter exchange bytes)))

(defun opencode-transport-relay-end (exchange)
  "End relayed stream EXCHANGE: its body is whole."
  (when (opencode-transport-live-p exchange)
    (remhash exchange opencode-transport--relayed)
    (opencode-transport--settle exchange t nil)))

(defun opencode-transport-relay-fail (exchange reason message &optional status body)
  "Fail relayed EXCHANGE for REASON, a symbol as `opencode-transport-failure' has.
MESSAGE tells why; STATUS and BODY are those of an answer, if any."
  (when (opencode-transport-live-p exchange)
    (remhash exchange opencode-transport--relayed)
    (opencode-transport--fail exchange reason message :status status :body body)))

(cl-defun opencode-transport-request (method url &key headers body as (then #'ignore) (else #'ignore)
                                             timeout (connect-timeout opencode-transport-connect-timeout)
                                             filter fresh)
  "Request METHOD of URL asynchronously, and return the exchange.
METHOD is a symbol such as `get'.  HEADERS is an alist of names and
values, and BODY a string, encoded as UTF-8 when multibyte.  The
service answers over a connection Emacs opens, or one kept from an
earlier request.

Exactly one of THEN and ELSE runs, after this returns.  On a 2xx answer,
THEN receives the body decoded as the answer's charset says, UTF-8
otherwise, or, when AS is a function, what AS returns, called without
argument in a buffer holding that body, at its start.  ELSE receives an
`opencode-transport-failure', for other answers too.  TIMEOUT is how
many seconds the whole request may take, nil for no limit, and
CONNECT-TIMEOUT how many the connection may take to open.  FRESH
non-nil sends the request on a new connection, never a kept one.

With FILTER, the request is a stream: FILTER is called with the
exchange and each piece of a 2xx answer's body, as bytes, when it
arrives, and THEN receives nil once the body ends.

`opencode-transport-cancel' abandons the request."
  (let* ((parsed (url-generic-parse-url url))
         (host (string-remove-suffix "]" (string-remove-prefix "[" (or (url-host parsed) ""))))
         (port (url-port parsed))
         (path (let ((file (url-filename parsed))) (if (string-empty-p file) "/" file)))
         (exchange (opencode-transport--make-exchange
                    :method method :key (format "%s:%s" host port) :host host :port port
                    :as as :then then :else else :filter filter
                    :timeout timeout :connect-timeout connect-timeout))
         (relay (and (equal (url-type parsed) "relay") host))
         (problem
          (cond
           (relay nil)
           ((not (equal (url-type parsed) "http")) (format "unsupported URL %s" url))
           ((string-empty-p host) (format "no host in %s" url))
           (t (condition-case err
                  (progn
                    (setf (opencode-transport-exchange-bytes exchange)
                          (opencode-transport--request-bytes method host port path headers body))
                    nil)
                (error (error-message-string err)))))))
    (cond
     (problem
      (run-at-time 0 nil #'opencode-transport--fail exchange 'protocol problem))
     (t
      (when timeout
        (setf (opencode-transport-exchange-timer exchange)
              (run-at-time timeout nil #'opencode-transport--expire exchange nil)))
      (if relay
          (opencode-transport--relay exchange relay method path headers
                                     (cond ((null body) nil)
                                           ((multibyte-string-p body) (encode-coding-string body 'utf-8))
                                           (t body)))
        (opencode-transport--dispatch exchange fresh))))
    exchange))

(defun opencode-transport-request-sync (method url &rest arguments)
  "Request METHOD of URL and return what THEN would receive, waiting for it.
ARGUMENTS are `opencode-transport-request' keywords except THEN, ELSE
and FILTER; TIMEOUT defaults to 60 seconds.  A failure signals an
`error' describing it.  Quitting cancels the request."
  (let* ((result 'opencode-transport--pending)
         (failure nil)
         (arguments (cl-loop for (key value) on arguments by #'cddr
                             unless (memq key '(:then :else :filter)) append (list key value)))
         (exchange (apply #'opencode-transport-request method url
                          :timeout (or (plist-get arguments :timeout) 60)
                          :then (lambda (value) (setq result value))
                          :else (lambda (value) (setq failure value))
                          arguments)))
    (unwind-protect
        (while (and (eq result 'opencode-transport--pending) (not failure))
          (accept-process-output nil 0.05))
      (opencode-transport-cancel exchange))
    (when failure
      (error "%s %s failed: %s" (upcase (symbol-name method)) url
             (opencode-transport-failure-describe failure)))
    result))

(provide 'opencode-transport)
;;; opencode-transport.el ends here
