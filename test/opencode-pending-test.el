;;; opencode-pending-test.el --- Pending prompts and their withdrawal  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A prompt the service took but has not delivered shows between the
;; transcript and the input until its delivery moves it into the
;; transcript; withdrawing it cancels it on the service and puts it
;; back in the input.  Session tests answer requests from routes; the
;; last ones talk to the fake service over sockets.

;;; Code:

(require 'opencode-test-helpers)
(require 'opencode-test-server)

(defvar opencode-test-messages nil
  "Messages shown under `opencode-test-capturing-messages', newest first.")

(defmacro opencode-test-capturing-messages (&rest body)
  "Run BODY with the messages shown collected in `opencode-test-messages'."
  (declare (indent 0) (debug t))
  `(let ((opencode-test-messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest arguments)
                  (when format-string
                    (push (apply #'format-message format-string arguments) opencode-test-messages)))))
       ,@body)))

(defun opencode-test-said-p (regexp)
  "Return non-nil when a message collected matches REGEXP."
  (seq-some (lambda (message) (string-match-p regexp message)) opencode-test-messages))

(defun opencode-test-area ()
  "Return what the pending area of the current buffer shows, or \"\".
The margin's markers, shown in the margin, are left out."
  (replace-regexp-in-string "^>" "" (or (opencode-inbox-area-string) "")))

(defun opencode-test-enqueued (session id text &optional delivery payload)
  "Return the event admitting user prompt ID of SESSION with TEXT.
DELIVERY defaults to steer; PAYLOAD adds to the item's payload."
  `((type . "session.inbox.enqueued")
    (data (sessionID . ,session) (inboxID . ,id)
          (item (type . "user") (delivery . ,(or delivery "steer"))
                (payload (text . ,text) ,@payload)))))

(defun opencode-test-delivered (session id)
  "Return the event delivering inbox item ID of SESSION."
  `((type . "session.inbox.delivered") (data (sessionID . ,session) (inboxID . ,id))))

(defun opencode-test-cancelled (session id)
  "Return the event cancelling inbox item ID of SESSION."
  `((type . "session.inbox.cancelled") (data (sessionID . ,session) (inboxID . ,id))))

(defun opencode-test-busy-buffer (session)
  "Open SESSION with an empty transcript, a turn running in it."
  (let ((buffer (opencode-test-open-empty session)))
    (with-current-buffer buffer
      (opencode-test-turn session "started"))
    buffer))

(defun opencode-test-type-and-send (text &optional queue)
  "Type TEXT at the input and send it, queued after the turn with QUEUE."
  (goto-char (point-max))
  (insert text)
  (if queue (opencode-queue-input) (opencode-send-input))
  (opencode-test-sent-id))

(defun opencode-test-cancel-routes (session &rest options)
  "Return the default routes with withdrawal answered as OPTIONS say.
:cancel is what `DELETE .../inbox/ID' does: `event' dispatches the
cancellation event before answering, `quiet' answers without one,
`delivered' delivers the prompt instead, `missing' answers an untagged
404 as a service without the route does.  :message is what `GET
.../message/ID' answers: `missing' a 404, else the message."
  (append
   (list (list 'delete "\\`/api/session/ses_[A-Za-z0-9]+/inbox/\\(msg_[A-Za-z0-9_]+\\)\\'"
               (lambda (request)
                 (let ((id (plist-get request :match)))
                   (pcase (plist-get options :cancel)
                     ('event (opencode-test-dispatch (list (opencode-test-cancelled session id))) nil)
                     ('delivered (opencode-test-dispatch (list (opencode-test-delivered session id))) nil)
                     ('missing '(:error 404 "Not Found"))
                     (_ nil)))))
         (list 'get "\\`/api/session/ses_[A-Za-z0-9]+/message/\\(msg_[A-Za-z0-9_]+\\)\\'"
               (lambda (request)
                 (if (eq (plist-get options :message) 'missing)
                     `(:error 404 ,(format "Message not found: %s" (plist-get request :match))
                              "MessageNotFoundError")
                   `((data (id . ,(plist-get request :match)) (type . "user")))))))
   (opencode-test-default-routes)))

;;;; Showing

(ert-deftest opencode-test-pending-tells-input-sent ()
  "A prompt sent while a turn runs is told sent, with no start in the buffer."
  (opencode-test-with-server
   (let ((sent nil))
     (with-current-buffer (opencode-test-busy-buffer (opencode-test-scenario "text"))
       (let ((opencode-input-sent-functions
              (list (lambda (kind text start) (push (list kind text start) sent)))))
         (opencode-test-type-and-send "look closer"))
       (should (equal sent '((prompt "look closer" nil))))))))

(ert-deftest opencode-test-pending-shows-until-delivered ()
  "A prompt sent while a turn runs shows as pending at once, then once in the transcript."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes
            ;; The service answers later than the prompt is sent.
            (cons (list 'post "/prompt\\'" (lambda (_) (cons :defer (lambda () '((data)))))) opencode-test-routes)))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let ((id (opencode-test-type-and-send "look closer")))
          ;; Pending before the service even answered, out of the transcript and the input.
          (should (string-match-p "Pending: joins the turn at its next step\nlook closer" (opencode-test-area)))
          (should-not (string-search "look closer" (buffer-string)))
          (should (equal (opencode-test-input (current-buffer)) ""))
          (should (= (overlay-start opencode--inbox-overlay) (opencode-pmark)))
          (should (opencode-test-release))
          (opencode-test-dispatch (list (opencode-test-enqueued session id "look closer")))
          (should (string-search "look closer" (opencode-test-area)))
          ;; Output streams before it, and the input typed meanwhile stays after it.
          (goto-char (point-max))
          (insert "draft")
          (opencode-test-answer session "msg_step" "Still working.")
          (should (= (overlay-start opencode--inbox-overlay) (opencode-pmark)))
          (should (= (point) (point-max)))
          (should (equal (opencode-test-input (current-buffer)) "draft"))
          ;; Delivered: in the transcript once, with its ID, and pending no more.
          (opencode-test-dispatch (list (opencode-test-delivered session id)))
          (should (equal (opencode-test-area) ""))
          (should-not opencode--inbox-overlay)
          (should (= (how-many "look closer" (point-min) (point-max)) 1))
          (should (equal (opencode-message-at (opencode-test-find "look closer")) id))
          (should (< (opencode-test-find "Still working.") (opencode-test-find "look closer")))
          (should (equal (opencode-test-input (current-buffer)) "draft"))
          (should (zerop (hash-table-count opencode--own-inputs))))))))

(ert-deftest opencode-test-pending-idle-prompt-shows-at-once ()
  "A prompt sent to an idle session with nothing pending shows in the transcript at once."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open-empty session)
        (let ((id (opencode-test-type-and-send "start")))
          (should (string-search "start" (buffer-string)))
          (opencode-test-dispatch (list (opencode-test-enqueued session id "start")))
          (should (equal (opencode-test-area) ""))
          (opencode-test-dispatch (list (opencode-test-delivered session id)))
          (should (= (how-many "start" (point-min) (point-max)) 1)))))))

(ert-deftest opencode-test-pending-two-in-order ()
  "Pending prompts show in the order the service will deliver them, each with its delivery."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let* ((first (opencode-test-type-and-send "first"))
               (second (opencode-test-type-and-send "second" t)))
          (should (string-match-p "joins the turn at its next step\nfirst\\(.\\|\n\\)*starts after the turn\nsecond"
                                  (opencode-test-area)))
          ;; Another client's prompt admitted between them comes between them.
          (opencode-test-dispatch (list (opencode-test-enqueued session first "first")
                                        (opencode-test-enqueued session "msg_tui" "from the TUI")
                                        (opencode-test-enqueued session second "second" "queue")))
          (should (string-match-p "first\\(.\\|\n\\)*from the TUI\\(.\\|\n\\)*second" (opencode-test-area)))
          ;; The service may steer a queued prompt.
          (opencode-test-dispatch `(((type . "session.inbox.delivery.changed")
                                     (data (sessionID . ,session) (inboxID . ,second) (delivery . "steer")))))
          (should-not (string-search "starts after the turn" (opencode-test-area)))
          (opencode-test-dispatch (list (opencode-test-delivered session first)
                                        (opencode-test-delivered session "msg_tui")))
          (should (string-match-p "\\`[^\n]*\n*[^\n]*Pending[^\n]*\nsecond" (opencode-test-area)))
          (should (< (opencode-test-find "first") (opencode-test-find "from the TUI")))
          (opencode-test-dispatch (list (opencode-test-delivered session second)))
          (should (< (opencode-test-find "from the TUI") (opencode-test-find "second")))
          (should (equal (opencode-test-area) "")))))))

(ert-deftest opencode-test-pending-other-client ()
  "Another client's prompt shows as pending too, its attachments named, until delivered."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-busy-buffer session)
        (opencode-test-dispatch
         (list (opencode-test-enqueued
                session "msg_tui" "read @README.md" "queue"
                '((files ((data . "UkVBRE1F") (mime . "text/plain") (name . "README.md")
                          (source (type . "uri") (uri . "file:///tmp/README.md"))
                          (mention (start . 5) (end . 15) (text . "@README.md"))))
                  (agents ((name . "explore")))))))
        (should (string-match-p "starts after the turn\nread @README.md @explore" (opencode-test-area)))
        (opencode-test-dispatch (list (opencode-test-delivered session "msg_tui")))
        (should (equal (opencode-test-area) ""))
        (should (= (how-many "read @README.md" (point-min) (point-max)) 1))))))

(ert-deftest opencode-test-pending-drover-send ()
  "A prompt sent past the input while a turn runs shows as pending at once."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let ((id (opencode-session-send "from drover" :show t)))
          (should (string-search "from drover" (opencode-test-area)))
          (opencode-test-dispatch (opencode-test-inbox-events session id "from drover"))
          (should (equal (opencode-test-area) ""))
          (should (= (how-many "from drover" (point-min) (point-max)) 1)))))))

(ert-deftest opencode-test-pending-at-idle-prompt ()
  "With the session idle, the pending area shows before the input prompt."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open-empty session)
        (should (opencode-prompt-shown-p))
        (opencode-test-dispatch (list (opencode-test-enqueued session "msg_later" "later" "queue")))
        (should (= (overlay-start opencode--inbox-overlay) (marker-position opencode--prompt-start)))
        ;; A prompt typed while another waits waits behind it.
        (let ((id (opencode-test-type-and-send "after that")))
          (should (string-match-p "later\\(.\\|\n\\)*after that" (opencode-test-area)))
          (should-not (string-search "> after that" (buffer-string)))
          (opencode-test-dispatch (list (opencode-test-enqueued session id "after that")
                                        (opencode-test-delivered session "msg_later")
                                        (opencode-test-delivered session id)))
          (should (< (opencode-test-find "later") (opencode-test-find "after that")))
          (should (equal (opencode-test-area) "")))))))

;;;; Withdrawing

(ert-deftest opencode-test-pending-withdrawn-with-attachments ()
  "Withdrawing a pending prompt cancels it and puts it back in the input with its attachments."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes (opencode-test-cancel-routes session :cancel 'event)))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let* ((uri (opencode-test-type-attachments))
               (typed (string-trim (opencode-test-input (current-buffer))))
               (id (progn (opencode-send-input) (opencode-test-sent-id))))
          (should (string-search typed (opencode-test-area)))
          (should (null opencode--attachments))
          (opencode-test-dispatch (list (opencode-test-enqueued session id typed)))
          (opencode-test-capturing-messages
            (opencode-edit-pending-prompt)
            (should (opencode-test-said-p "withdrawn")))
          (should (equal (opencode-test-requests-to 'delete (concat "/inbox/" id)) '(t)))
          (should (equal (opencode-test-area) ""))
          (should (equal (opencode-test-input (current-buffer)) typed))
          (should (= (length (opencode-test-attachment-overlays)) 2))
          (should (equal (sort (mapcar (lambda (attachment)
                                         (or (plist-get attachment :agent)
                                             (alist-get 'uri (plist-get attachment :file))))
                                       opencode--attachments)
                               #'string<)
                         (sort (list uri "explore") #'string<)))
          (should-not (string-search "cancelled" (buffer-string)))
          ;; Sent again, it carries them.
          (opencode-send-input)
          (let ((body (opencode-test-sent-prompt)))
            (should (equal (alist-get 'uri (aref (alist-get 'files body) 0)) uri))
            (should (equal (alist-get 'name (aref (alist-get 'agents body) 0)) "explore"))))))))

(ert-deftest opencode-test-pending-withdrawn-without-event ()
  "A withdrawal the service answers before its event is told by the message lookup."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes (opencode-test-cancel-routes session :cancel 'quiet :message 'missing)))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let ((id (opencode-test-type-and-send "never mind")))
          (opencode-test-dispatch (list (opencode-test-enqueued session id "never mind")))
          (goto-char (point-max))
          (insert "a draft")
          (opencode-edit-pending-prompt)
          (should (opencode-test-requests-to 'get (concat "/message/" id)))
          (should (equal (opencode-test-area) ""))
          ;; The draft stays, the prompt after it.
          (should (equal (opencode-test-input (current-buffer)) "a draft\n\nnever mind"))
          ;; The event, late, changes nothing.
          (opencode-test-dispatch (list (opencode-test-cancelled session id)))
          (should (equal (opencode-test-input (current-buffer)) "a draft\n\nnever mind"))
          (should-not (string-search "cancelled" (buffer-string))))))))

(ert-deftest opencode-test-pending-withdrawal-refused ()
  "A prompt delivered before its withdrawal stays sent, and is said so."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes (opencode-test-cancel-routes session :cancel 'quiet)))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let ((id (opencode-test-type-and-send "too late")))
          (opencode-test-dispatch (list (opencode-test-enqueued session id "too late")))
          (opencode-test-capturing-messages
            (opencode-edit-pending-prompt)
            (should (opencode-test-said-p "delivered before it could be withdrawn")))
          ;; Still pending until its delivery is reported, then in the transcript.
          (should (string-search "too late" (opencode-test-area)))
          (should (equal (opencode-test-input (current-buffer)) ""))
          (opencode-test-dispatch (list (opencode-test-delivered session id)))
          (should (equal (opencode-test-area) ""))
          (should (= (how-many "too late" (point-min) (point-max)) 1)))))))

(ert-deftest opencode-test-pending-withdrawal-overtaken ()
  "A delivery reported while the withdrawal is asked wins: the prompt is in the transcript."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes (opencode-test-cancel-routes session :cancel 'delivered)))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let ((id (opencode-test-type-and-send "raced")))
          (opencode-test-dispatch (list (opencode-test-enqueued session id "raced")))
          (opencode-test-capturing-messages
            (opencode-edit-pending-prompt)
            (should (opencode-test-said-p "delivered before it could be withdrawn")))
          (should-not (opencode-test-requests-to 'get (concat "/message/" id)))
          (should (equal (opencode-test-input (current-buffer)) ""))
          (should (= (how-many "raced" (point-min) (point-max)) 1)))))))

(ert-deftest opencode-test-pending-withdrawal-unsupported ()
  "A service without the route keeps the prompt pending, and says it cannot withdraw it."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes (opencode-test-cancel-routes session :cancel 'missing)))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let ((id (opencode-test-type-and-send "stuck")))
          (opencode-test-dispatch (list (opencode-test-enqueued session id "stuck")))
          (opencode-test-capturing-messages
            (opencode-edit-pending-prompt)
            (should (opencode-test-said-p "cannot withdraw")))
          (should (string-match-p "joins the turn at its next step\nstuck" (opencode-test-area)))
          (should (equal (opencode-test-input (current-buffer)) "")))))))

(ert-deftest opencode-test-pending-withdrawn-before-taken ()
  "A prompt withdrawn before the service took it is cancelled once taken."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes
            (cons (list 'post "/prompt\\'" (lambda (_) (cons :defer (lambda () '((data))))))
                  (opencode-test-cancel-routes session :cancel 'event))))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let ((id (opencode-test-type-and-send "wait, no")))
          (opencode-edit-pending-prompt)
          (should (string-search "withdrawn once the service has it" (opencode-test-area)))
          (should-not (opencode-test-requests-to 'delete (concat "/inbox/" id)))
          (opencode-test-release)
          (should (opencode-test-requests-to 'delete (concat "/inbox/" id)))
          (should (equal (opencode-test-input (current-buffer)) "wait, no"))
          (should (equal (opencode-test-area) "")))))))

(ert-deftest opencode-test-pending-withdraw-other-client ()
  "Another client's pending prompt is withdrawn with its attachments, from its payload."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes (opencode-test-cancel-routes session :cancel 'event)))
      (with-current-buffer (opencode-test-busy-buffer session)
        (opencode-test-dispatch
         (list (opencode-test-enqueued
                session "msg_tui" "see @notes.txt 😀 and the image"
                "steer"
                '((files ((data . "bm90ZXM=") (mime . "text/plain") (name . "notes.txt")
                          (source (type . "uri") (uri . "file:///tmp/notes.txt"))
                          (mention (start . 4) (end . 14) (text . "@notes.txt")))
                         ((data . "iVBORw==") (mime . "image/png") (name . "shot.png")
                          (source (type . "inline"))))
                  (skills ((id . "skl_review") (name . "review")))))))
        (opencode-edit-pending-prompt)
        (should (equal (opencode-test-input (current-buffer))
                       "see `@notes.txt` 😀 and the image `shot.png` `review`"))
        (should (equal (sort (mapcar (lambda (attachment)
                                       (or (plist-get attachment :skill)
                                           (alist-get 'uri (plist-get attachment :file))))
                                     opencode--attachments)
                             #'string<)
                       '("data:image/png;base64,iVBORw==" "file:///tmp/notes.txt" "skl_review")))
        (opencode-send-input)
        (should (equal (alist-get 'skills (opencode-test-sent-prompt)) [((id . "skl_review"))]))))))

(ert-deftest opencode-test-pending-counted-and-withdrawn-by-session ()
  "The pending prompts of a session are counted, and withdrawn, from any buffer.
Its buffer keeps them, unshown."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes (opencode-test-cancel-routes session :cancel 'quiet :message 'missing))
           (buffer (opencode-test-busy-buffer session))
           (first (with-current-buffer buffer (opencode-test-type-and-send "first")))
           (second (with-current-buffer buffer (opencode-test-type-and-send "second"))))
      (with-current-buffer buffer
        (opencode-test-dispatch (list (opencode-test-enqueued session first "first")
                                      (opencode-test-enqueued session second "second"))))
      (with-temp-buffer
        (should (= (opencode-pending-prompts session) 2))
        (should (= (opencode-pending-prompts "ses_without_buffer") 0))
        (should-error (opencode-edit-pending-prompt nil "ses_without_buffer") :type 'user-error)
        (opencode-edit-pending-prompt first session)
        (should (= (opencode-pending-prompts session) 1))
        (should (opencode-test-requests-to 'delete (concat "/inbox/" first))))
      (with-current-buffer buffer
        (should (equal (opencode-test-input (current-buffer)) "first"))
        (opencode-test-dispatch (list (opencode-test-delivered session second)))
        (should (= (opencode-pending-prompts session) 0))))))

(ert-deftest opencode-test-send-attachments-of-old-input ()
  "A prompt sent with the attachments its labels carry sends them again."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-open-empty session)
        (let* ((text (opencode-inbox-payload-text
                      '((text . "see @notes.txt")
                        (files ((data . "bm90ZXM=") (mime . "text/plain") (name . "notes.txt")
                                (source (type . "uri") (uri . "file:///tmp/notes.txt"))
                                (mention (start . 4) (end . 14) (text . "@notes.txt"))))
                        (agents ((name . "explore")))
                        (skills ((id . "skl_review") (name . "review"))))))
               (attachments (delete-dups
                             (delq nil (mapcar (lambda (index)
                                                 (get-text-property index 'opencode-sent-attachment text))
                                               (number-sequence 0 (1- (length text))))))))
          (opencode-session-send (substring-no-properties text) :show t :attachments attachments
                                 :files [((uri . "file:///tmp/other.txt") (name . "other.txt"))])
          (let ((body (opencode-test-sent-prompt)))
            (should (equal (alist-get 'text body) "see @notes.txt @explore `review`"))
            (should (equal (mapcar (lambda (file) (alist-get 'uri file)) (alist-get 'files body))
                           '("file:///tmp/other.txt" "file:///tmp/notes.txt")))
            (should (equal (alist-get 'agents body) [((name . "explore"))]))
            (should (equal (alist-get 'skills body) [((id . "skl_review"))]))))))))

(ert-deftest opencode-test-pending-click-withdraws ()
  "A click on a pending prompt withdraws it."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes (opencode-test-cancel-routes session :cancel 'event)))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let ((id (opencode-test-type-and-send "clicked")))
          (opencode-test-dispatch (list (opencode-test-enqueued session id "clicked")))
          (let* ((string (overlay-get opencode--inbox-overlay 'before-string))
                 (position (string-search "clicked" string)))
            (should (eq (lookup-key (get-text-property position 'keymap string) [mouse-1])
                        #'opencode-pending-prompt-click))
            (save-window-excursion
              (set-window-buffer (selected-window) (current-buffer))
              (opencode-pending-prompt-click
               `(mouse-1 (,(selected-window) ,(point) (0 . 0) 0 (,string . ,position)
                          nil nil nil nil nil)))))
          (should (equal (opencode-test-input (current-buffer)) "clicked")))))))

(ert-deftest opencode-test-pending-several-asks-which ()
  "With several pending prompts, the one withdrawn is read, the newest by default."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes (opencode-test-cancel-routes session :cancel 'event))
           (offered nil))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let ((first (opencode-test-type-and-send "same"))
              (second (opencode-test-type-and-send "same")))
          (opencode-test-dispatch (list (opencode-test-enqueued session first "same")
                                        (opencode-test-enqueued session second "same")))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest arguments)
                       (setq offered collection)
                       (nth 4 arguments))))
            (opencode-edit-pending-prompt))
          (should (equal offered '("same" "same <2>")))
          (should (equal (opencode-test-requests-to 'delete (concat "/inbox/" second)) '(t)))
          (should (string-search "same" (opencode-test-area))))))))

(ert-deftest opencode-test-pending-refused-prompt-back-in-input ()
  "A prompt sent while a turn runs that the service refuses goes back to the input."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes (cons (list 'post "/prompt\\'" (lambda (_) '(:error 500 "boom")))
                                       opencode-test-routes)))
      (with-current-buffer (opencode-test-busy-buffer session)
        (opencode-test-type-attachments)
        (let ((typed (string-trim (opencode-test-input (current-buffer)))))
          (opencode-send-input)
          (should (equal (opencode-test-area) ""))
          (should (string-match-p "boom; the prompt is back in the input" (buffer-string)))
          (should (equal (opencode-test-input (current-buffer)) typed)))
        (should (= (length opencode--attachments) 2))))))

(ert-deftest opencode-test-pending-cancelled-elsewhere-back-in-input ()
  "A prompt typed while a turn runs and cancelled by another client goes back to the input."
  (opencode-test-with-server
    (let ((session (opencode-test-scenario "text")))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let ((id (opencode-test-type-and-send "lost elsewhere")))
          (opencode-test-dispatch (list (opencode-test-enqueued session id "lost elsewhere")
                                        (opencode-test-cancelled session id)))
          (should (equal (opencode-test-area) ""))
          (should (string-search "A pending prompt was cancelled; it is back in the input" (buffer-string)))
          (should (equal (opencode-test-input (current-buffer)) "lost elsewhere")))))))

;;;; Rebuilding

(ert-deftest opencode-test-pending-reconnect-rebuilds ()
  "A reconnection rebuilds the pending prompts from the service's inbox."
  (opencode-test-with-transcript (list (opencode-test-prompt "msg_start" "start"))
    (let ((session (opencode-test-session)))
      (with-current-buffer (opencode-test-open session)
        (opencode-test-turn session "started")
        (let ((own (opencode-test-type-and-send "mine")))
          (opencode-test-dispatch (list (opencode-test-enqueued session own "mine")
                                        (opencode-test-enqueued session "msg_gone" "gone")))
          ;; While the stream is down: one is delivered, another client adds one.
          (setq opencode-test-inbox (assoc-delete-all "msg_gone" opencode-test-inbox))
          (setq opencode-test-transcript
                (append opencode-test-transcript (list (opencode-test-prompt "msg_gone" "gone"))))
          (opencode-test-inbox-follow (opencode-test-enqueued session "msg_new" "new one" "queue"))
          (setq opencode-test-routes
                (cons (list 'get "\\`/api/session/active\\'" (lambda (_) `((data (,(intern session)))))) opencode-test-routes))
          (opencode-test-reconnect)
          (should (string-match-p "mine\\(.\\|\n\\)*starts after the turn\nnew one" (opencode-test-area)))
          (should-not (string-search "gone" (opencode-test-area)))
          (should (= (how-many "gone" (point-min) (point-max)) 1))
          ;; What it kept of its own prompt still withdraws it.
          (should (eq (car (opencode-inbox-entry own)) own))
          (should (plist-get (cdr (opencode-inbox-entry own)) :typed))
          (opencode-test-dispatch (list (opencode-test-delivered session own)
                                        (opencode-test-delivered session "msg_new")))
          (should (equal (opencode-test-area) ""))
          (should (= (how-many "mine" (point-min) (point-max)) 1))
          (should (= (how-many "new one" (point-min) (point-max)) 1)))))))

(ert-deftest opencode-test-pending-reload-rebuilds ()
  "Reverting a session buffer shows its pending prompts again, and its own once delivered."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes
            (cons (list 'get "/message\\'" (lambda (_) '((data) (cursor)))) opencode-test-routes)))
      (with-current-buffer (opencode-test-busy-buffer session)
        (let ((id (opencode-test-type-and-send "kept")))
          (opencode-test-dispatch (list (opencode-test-enqueued session id "kept")))
          (revert-buffer)
          (should (string-search "kept" (opencode-test-area)))
          (should-not (string-search "kept" (buffer-string)))
          (opencode-test-dispatch (list (opencode-test-delivered session id)))
          (should (= (how-many "kept" (point-min) (point-max)) 1))
          (should (equal (opencode-test-area) "")))))))

(ert-deftest opencode-test-pending-idle-prompt-erased-shows-pending ()
  "A prompt typed at an idle session whose input a reload erased shows as pending until delivered."
  (opencode-test-with-server
    (let* ((session (opencode-test-scenario "text"))
           (opencode-test-routes
            (cons (list 'get "/message\\'" (lambda (_) '((data) (cursor)))) opencode-test-routes)))
      (with-current-buffer (opencode-test-open-empty session)
        (let ((id (opencode-test-type-and-send "typed idle")))
          (opencode-test-dispatch (list (opencode-test-enqueued session id "typed idle")))
          (should (equal (opencode-test-area) ""))
          (opencode-session-reload)
          (should (string-search "typed idle" (opencode-test-area)))
          (opencode-test-dispatch (list (opencode-test-delivered session id)))
          (should (= (how-many "typed idle" (point-min) (point-max)) 1))
          (should (equal (opencode-test-area) "")))))))

;;;; Over sockets

(ert-deftest opencode-test-pending-over-the-wire ()
  "Against the fake service: pending while the turn runs, withdrawn, sent again, delivered."
  (opencode-test-with-lifecycle-session buffer
    (let* ((session opencode-test-lifecycle-session)
           (inbox (concat "/api/session/" session "/inbox")))
      (setf (opencode-test-service-routes opencode-test-service)
            (append
             (list (cons inbox (lambda (_) '(200 . ((data . [])))))
                   ;; The cancellation answers before its event, and a cancelled prompt has no message.
                   (cons (concat inbox "/msg_wire") (lambda (_) '(204 . "")))
                   (cons (concat "/api/session/" session "/message/msg_wire")
                         (lambda (_) '(404 . ((_tag . "MessageNotFoundError") (message . "Message not found"))))))
             (opencode-test-service-routes opencode-test-service)))
      (opencode-test-send-event `((type . "session.execution.started") (data (sessionID . ,session))))
      (opencode-test-wait-for (lambda () (eq (buffer-local-value 'opencode-session-status buffer) 'busy)))
      (cl-letf (((symbol-function 'opencode-generate-id) (lambda (_) "msg_wire")))
        (opencode-test-type-prompt buffer "over the wire"))
      (with-current-buffer buffer
        (should (string-search "over the wire" (opencode-test-area)))
        (should-not (string-search "over the wire" (buffer-string))))
      (opencode-test-wait-for (lambda () (opencode-test-requests-on (concat "/api/session/" session "/prompt"))))
      (opencode-test-send-event
       `((type . "session.inbox.enqueued")
         (data (sessionID . ,session) (inboxID . "msg_wire")
               (item (type . "user") (delivery . "steer") (payload (text . "over the wire"))))))
      (opencode-test-wait-for (lambda () (plist-get (cdr (with-current-buffer buffer (opencode-inbox-entry "msg_wire")))
                                                    :placed)))
      (with-current-buffer buffer
        (opencode-edit-pending-prompt))
      (opencode-test-wait-for (lambda () (equal (opencode-test-input buffer) "over the wire")))
      (should (equal (plist-get (car (opencode-test-requests-on (concat inbox "/msg_wire"))) :method) "DELETE"))
      (with-current-buffer buffer
        (should (equal (opencode-test-area) ""))
        (goto-char (point-max))
        (insert ", again")
        (cl-letf (((symbol-function 'opencode-generate-id) (lambda (_) "msg_wire2")))
          (opencode-send-input))
        (should (string-search "over the wire, again" (opencode-test-area))))
      (opencode-test-send-event
       `((type . "session.inbox.enqueued")
         (data (sessionID . ,session) (inboxID . "msg_wire2")
               (item (type . "user") (delivery . "steer") (payload (text . "over the wire, again"))))))
      (opencode-test-send-event `((type . "session.inbox.delivered") (data (sessionID . ,session) (inboxID . "msg_wire2"))))
      (opencode-test-wait-for (lambda () (opencode-test-buffer-shows-p buffer "over the wire, again")))
      (with-current-buffer buffer
        (should (equal (opencode-test-area) ""))
        (should (= (how-many "over the wire, again" (point-min) (point-max)) 1))))))

(provide 'opencode-pending-test)
;;; opencode-pending-test.el ends here
