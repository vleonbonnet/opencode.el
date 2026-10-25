;;; opencode-form.el --- Questions and other forms  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Arthur Heymans
;; Copyright (C) 2026  Valentin Leon

;; Author: Arthur Heymans <arthur@aheymans.xyz>
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

;; The server asks the user through forms.  The question tool's form has
;; one field per question, q0, q1 and so on, each a string or, for
;; questions allowing several answers, a multiselect, with the options'
;; labels as values and custom answers allowed.  Its block stays expanded
;; with every option and its key while pending, and collapses to the
;; answers once the call completes.  Other forms, such as MCP
;; elicitations, are answered field by field in the minibuffer.
;;
;; Each queued form keeps its own progress, the current question and one
;; saved selection per question, so deferring or suspending never ties up
;; the transient.  In a batch, M-p returns to the previous question with
;; its saved answer and M-n keeps the current one and moves on; choosing
;; an option of a single-choice question continues to the next.  A reply
;; that fails returns the form to the queue; a 404 means the server
;; already resolved it.  Forms live only in the server's memory: after a
;; reconnect the queue is reconciled with the server's list, and a form
;; the server lost is reported in the transcript rather than dropped.

;;; Code:

(require 'opencode-blocks)
(require 'opencode-common)
(require 'opencode-events)
(require 'opencode-http)
(require 'opencode-requests)
(require 'opencode-session)
(require 'transient)

(defvar-local opencode-pending-forms nil
  "Forms of this session awaiting an answer, oldest first.
Each is a plist (:id :form :call-id :index :answers :submitted); :answers
holds one saved selection (MARKED-VECTOR . CUSTOM-TEXT) per field.")

(defvar-local opencode--resolved-forms nil
  "IDs of forms recently resolved, never to be queued again.")

(defvar opencode--active-form nil
  "Plist (:buffer :entry) of the form shown or suspended in the transient.")

;;;; Forms

(defun opencode-form-question-p (form)
  "Return non-nil when FORM asks the question tool's questions."
  (equal (alist-get 'kind (alist-get 'metadata form)) "question"))

(defun opencode-form-fields (form)
  "Return the answerable fields of FORM."
  (alist-get 'fields form))

(defun opencode--form-entry (form)
  "Return a queue entry for FORM."
  (list :id (alist-get 'id form) :form form
        :call-id (alist-get 'id (alist-get 'tool (alist-get 'metadata form)))
        :index 0 :answers (make-vector (length (opencode-form-fields form)) nil)
        :submitted nil))

(defun opencode--form-find (key value)
  "Return this buffer's form entry whose KEY equals VALUE."
  (and value (seq-find (lambda (entry) (equal (plist-get entry key) value))
                       opencode-pending-forms)))

(defun opencode-open-forms ()
  "Return this buffer's forms awaiting an answer."
  (seq-remove (lambda (entry) (plist-get entry :submitted)) opencode-pending-forms))

(defun opencode--field-options (field)
  "Return the option labels of FIELD."
  (mapcar (lambda (option) (alist-get 'label option)) (alist-get 'options field)))

(defun opencode--entry-answer (entry)
  "Return ENTRY's saved selections as the answer object the server expects."
  (let ((answer nil))
    (cl-loop for field in (opencode-form-fields (plist-get entry :form))
             for saved across (plist-get entry :answers)
             for options = (alist-get 'options field)
             for chosen = (append
                           (cl-loop for option in options
                                    for marked across (or (car saved) [])
                                    when marked collect (alist-get 'value option))
                           (and (cdr saved) (list (cdr saved))))
             when chosen
             do (push (cons (intern (alist-get 'key field))
                            (if (equal (alist-get 'type field) "multiselect")
                                (vconcat chosen)
                              (car chosen)))
                      answer))
    (nreverse answer)))

;;;; Question blocks

(defun opencode--form-block (entry)
  "Return the live block of the tool call that asked ENTRY."
  (when-let* ((id (plist-get entry :call-id)))
    (opencode-block id)))

(defun opencode--set-form-waiting (overlay waiting &optional title)
  "Mark tool block OVERLAY as WAITING for a form's answer.
A question block is expanded meanwhile, since it lists its questions;
another tool's block stays collapsed, its summary naming the form by
TITLE."
  (let* ((data (opencode-block-data overlay))
         (question (equal (plist-get data :tool) "question")))
    (unless (eq (and (eq (plist-get data :waiting-kind) 'question) t) (and waiting t))
      (opencode-block-update overlay :waiting (and waiting t)
                             :waiting-kind (and waiting 'question)
                             :waiting-description (and waiting (not question) title))
      (overlay-put overlay 'help-echo
                   (and waiting (substitute-command-keys "Question pending.  \\[opencode-respond] answers it.")))
      (when question
        (opencode-call-preserving-viewport
         (overlay-buffer overlay)
         (lambda ()
           (opencode--refresh-expanded overlay)
           (when waiting (opencode-expand-block overlay)))))
      (force-mode-line-update))))

(defun opencode--form-attach (overlay)
  "Mark newly inserted tool block OVERLAY when a form it asked is queued."
  (let ((data (opencode-block-data overlay)))
    (when (and (eq (plist-get data :type) 'tool)
               (not (member (plist-get data :status) '("completed" "error"))))
      (when-let* ((entry (opencode--form-find :call-id (plist-get data :key))))
        (unless (plist-get entry :submitted)
          (opencode--set-form-waiting overlay t (alist-get 'title (plist-get entry :form))))))))

(defun opencode--form-block-finished (overlay)
  "Release the form tool block OVERLAY asked, once the call ended.
A question block, expanded while it waited, collapses."
  (let ((data (opencode-block-data overlay)))
    (when (equal (plist-get data :tool) "question")
      (opencode--set-form-waiting overlay nil)
      (opencode-call-preserving-viewport (overlay-buffer overlay)
                                         (lambda () (opencode-collapse-block overlay))))
    (when-let* ((entry (opencode--form-find :call-id (plist-get data :key))))
      (opencode--form-resolved (plist-get entry :id)))))

(defun opencode-format-questions (questions &optional answers)
  "Return QUESTIONS, the question tool's input, with their options.
ANSWERS, the chosen labels per question, marks each chosen option and
shows typed answers in place of the option keys."
  (string-join
   (cl-loop for question in questions
            for index from 0
            for chosen = (and answers (nth index answers))
            for labels = (mapcar (lambda (option) (alist-get 'label option))
                                 (alist-get 'options question))
            for typed = (seq-remove (lambda (answer)
                                      (seq-some (lambda (label) (string-equal-ignore-case label answer))
                                                labels))
                                    chosen)
            collect
            (concat
             "❓ " (alist-get 'question question) "\n"
             (string-join
              (cl-loop for option in (alist-get 'options question)
                       for key from 0
                       for label = (alist-get 'label option)
                       for description = (alist-get 'description option)
                       for picked = (seq-some (lambda (answer) (string-equal-ignore-case answer label)) chosen)
                       collect
                       (concat
                        (cond
                         ((not answers) (format "  %s  %s" (opencode--option-key key) label))
                         (picked (propertize (concat "  ✓  " label) 'font-lock-face 'success))
                         (t (propertize (concat "     " label) 'font-lock-face 'shadow)))
                        (when (and description (not (string-empty-p description))
                                   (not (equal description label)))
                          (propertize (concat "\n      " (string-replace "\n" "\n      " description))
                                      'font-lock-face (if (and answers (not picked)) 'shadow 'default)))))
              "\n")
             (cond
              ((not answers) "\n  !  Type your own answer")
              (typed (mapconcat (lambda (answer)
                                  (concat "\n" (propertize (concat "  ✎  " answer) 'font-lock-face 'success)))
                                typed ""))
              ((null chosen) (concat "\n" (propertize "  (unanswered)" 'font-lock-face 'shadow)))
              (t ""))))
   "\n\n"))

(defun opencode--question-block-content (data)
  "Return the expansion of question tool block DATA, or nil for other blocks."
  (when (and (eq (plist-get data :type) 'tool) (equal (plist-get data :tool) "question"))
    (opencode-fontify-as
     (opencode-format-questions
      (alist-get 'questions (plist-get data :input))
      (cond
       ((eq (plist-get data :waiting-kind) 'question) nil)
       ((plist-get data :answers))
       ((plist-get data :error)
        (make-list (length (alist-get 'questions (plist-get data :input))) nil))))
     'verbatim)))

(add-hook 'opencode-block-content-functions #'opencode--question-block-content)
(add-hook 'opencode-blocks-attach-functions #'opencode--form-attach)
(add-hook 'opencode-blocks-finished-functions #'opencode--form-block-finished)

;;;; Queue

(defun opencode-form-queue (form)
  "Queue FORM in this session buffer, unless known.  Return its entry."
  (let ((id (alist-get 'id form)))
    (unless (or (opencode--form-find :id id) (member id opencode--resolved-forms))
      (let ((entry (opencode--form-entry form)))
        (setq opencode-pending-forms (append opencode-pending-forms (list entry)))
        (if-let* ((overlay (opencode--form-block entry)))
            (opencode--set-form-waiting overlay t (alist-get 'title form))
          (opencode-output-notice
           (format "%s %s" (if (opencode-form-question-p form) "Question:" "Form:")
                   (or (alist-get 'title form) id))
           'opencode-attention))
        (force-mode-line-update)
        (opencode-request-state-changed)
        (if (opencode-buffer-active-p (current-buffer))
            (message (substitute-command-keys "OpenCode question pending; \\[opencode-respond] answers it"))
          (opencode-notify-session
           "OpenCode question"
           (or (alist-get 'description (car (opencode-form-fields form))) (alist-get 'title form) "")
           'question id)
          (opencode-alert-session))
        entry))))

(defun opencode--form-resolved (id)
  "Forget form ID of this buffer once answered or cancelled, by any client."
  (opencode-dismiss-notifications 'question id)
  (unless (member id opencode--resolved-forms)
    (push id opencode--resolved-forms)
    (when (nthcdr 50 opencode--resolved-forms)
      (setcdr (nthcdr 49 opencode--resolved-forms) nil)))
  (when-let* ((entry (opencode--form-find :id id)))
    (setq opencode-pending-forms (delq entry opencode-pending-forms))
    (when (eq entry (plist-get opencode--active-form :entry))
      (opencode--form-close-transient))
    (when-let* ((overlay (opencode--form-block entry)))
      (opencode--set-form-waiting overlay nil))
    (force-mode-line-update)
    (opencode-request-state-changed)))

(defun opencode-form-send (entry &optional cancel)
  "Reply to form ENTRY with its saved answers, or CANCEL it."
  (let ((id (plist-get entry :id))
        (buffer (current-buffer)))
    (opencode-dismiss-notifications 'question id)
    (plist-put entry :submitted t)
    (force-mode-line-update)
    (opencode-request-state-changed)
    (cl-flet ((failed (error)
                (if (eql (opencode-error-status error) 404)
                    (progn (opencode--form-resolved id)
                           (message "The OpenCode question is no longer pending"))
                  (with-current-buffer buffer
                    (plist-put entry :submitted nil)
                    (force-mode-line-update)
                    (opencode-request-state-changed))
                  (message "OpenCode %s failed (%s); %s retries"
                           (if cancel "dismissal" "reply") (opencode-error-describe error)
                           (substitute-command-keys "\\[opencode-respond]")))))
      (if cancel
          (opencode-request 'delete (format "/api/session/%s/form/%s" opencode-session-id id)
                            :query '((message . "dismissed"))
                            :then (lambda (_) (opencode--form-resolved id)) :else #'failed)
        (opencode-request 'post (format "/api/session/%s/form/%s/reply" opencode-session-id id)
                          :body `((answer . ,(or (opencode--entry-answer entry) (opencode-empty-object))))
                          :then (lambda (_) (opencode--form-resolved id)) :else #'failed)))))

;;;; Question transient

(defvar opencode-question--marked nil "Marked options of the shown question.")
(defvar opencode-question--labels nil "Option labels of the shown question.")
(defvar opencode-question--descriptions nil "Option descriptions of the shown question.")
(defvar opencode-question--multiple nil "Non-nil when the shown question allows several answers.")
(defvar opencode-question--custom-text nil "Custom answer typed for the shown question.")
(defvar opencode-question--heading nil "Heading of the shown question.")
(defvar opencode-question--handled nil "Non-nil once the shown transient was answered.")

(defun opencode--option-key (index)
  "Return the transient key of option INDEX."
  (cond ((< index 9) (string (+ ?1 index)))
        ((< index 35) (string (+ ?a (- index 9))))
        (t (format "M-%d" (- index 35)))))

(defun opencode--active-entry ()
  "Return the form entry the question transient answers."
  (plist-get opencode--active-form :entry))

(defun opencode--question-count ()
  "Return the number of questions of the active form."
  (length (opencode-form-fields (plist-get (opencode--active-entry) :form))))

(defun opencode--question-index ()
  "Return the index of the question shown."
  (plist-get (opencode--active-entry) :index))

(defun opencode--question-final-p ()
  "Return non-nil when the question shown is the last one."
  (= (1+ (opencode--question-index)) (opencode--question-count)))

(defun opencode--question-selection ()
  "Return the selected labels and custom answer of the shown question."
  (append (cl-loop for index below (length opencode-question--labels)
                   when (aref opencode-question--marked index)
                   collect (aref opencode-question--labels index))
          (and opencode-question--custom-text (list opencode-question--custom-text))))

(defun opencode--question-save ()
  "Store the shown question's selection in the active entry."
  (when-let* ((entry (opencode--active-entry)))
    (aset (plist-get entry :answers) (opencode--question-index)
          (when (opencode--question-selection)
            (cons (copy-sequence opencode-question--marked) opencode-question--custom-text)))))

(defun opencode--question-heading (field)
  "Return the transient heading of question FIELD, with its position in a batch."
  (let ((count (opencode--question-count))
        (title (alist-get 'title field)))
    (if (< count 2)
        (alist-get 'description field)
      (concat (format "Question %d/%d" (1+ (opencode--question-index)) count)
              (when (and title (not (string-empty-p title))) (concat " · " title))
              (propertize "   M-p previous · M-n next" 'face 'shadow)
              "\n" (alist-get 'description field)))))

(defun opencode--question-show ()
  "Show the active entry's current question in the transient."
  (when-let* ((entry (opencode--active-entry)))
    (let* ((field (nth (plist-get entry :index) (opencode-form-fields (plist-get entry :form))))
           (options (alist-get 'options field))
           (saved (aref (plist-get entry :answers) (plist-get entry :index))))
      (setq opencode-question--labels (vconcat (mapcar (lambda (option) (alist-get 'label option)) options))
            opencode-question--descriptions (vconcat (mapcar (lambda (option) (alist-get 'description option)) options))
            opencode-question--marked (if (and saved (= (length (car saved)) (length options)))
                                          (copy-sequence (car saved))
                                        (make-vector (length options) nil))
            opencode-question--custom-text (cdr saved)
            opencode-question--multiple (equal (alist-get 'type field) "multiselect")
            opencode-question--heading (opencode--question-heading field)
            opencode-question--handled nil)
      (add-hook 'transient-exit-hook #'opencode--question-on-exit)
      (call-interactively #'opencode-question-transient))))

(defun opencode--question-goto (index)
  "Keep the current selection, leave the transient and show question INDEX."
  (opencode--question-save)
  (setq opencode-question--handled t)
  (plist-put (opencode--active-entry) :index index)
  (run-at-time 0 nil #'opencode--question-show))

(defun opencode--question-submit (&optional cancel)
  "Send the active entry's answers, or CANCEL it, and release the transient."
  (let* ((active opencode--active-form)
         (buffer (plist-get active :buffer)))
    (setq opencode-question--handled t
          opencode--active-form nil)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (opencode-form-send (plist-get active :entry) cancel)))))

(defun opencode--question-advance ()
  "Keep the current answer, then show the next question or submit them all."
  (if (opencode--question-final-p)
      (progn (opencode--question-save) (opencode--question-submit))
    (opencode--question-goto (1+ (opencode--question-index)))))

(defclass opencode-question-option (transient-suffix)
  ((option-index :initarg :option-index :initform 0)
   (transient :initform t))
  "An option of the shown question.")

(cl-defmethod transient-format-description ((object opencode-question-option))
  "Format option OBJECT as a checkbox line."
  (let ((index (oref object option-index)))
    (concat (if (aref opencode-question--marked index) "[x]" "[ ]") " "
            (aref opencode-question--labels index)
            (when-let* ((description (aref opencode-question--descriptions index)))
              (unless (equal description (aref opencode-question--labels index))
                (concat "  " (propertize description 'face 'shadow)))))))

(defclass opencode-question-custom (transient-suffix)
  ((transient :initform t))
  "The custom answer of the shown question.")

(cl-defmethod transient-format-description ((_object opencode-question-custom))
  "Format the custom answer line."
  (concat (if opencode-question--custom-text "[x]" "[ ]") " Type your own answer"
          (when opencode-question--custom-text
            (concat "  " (propertize opencode-question--custom-text 'face 'font-lock-string-face)))))

(defun opencode--question-stay-if-multiple ()
  "Stay in a multiple-choice transient, leave a single-choice one."
  (if opencode-question--multiple (transient--do-stay) (transient--do-exit)))

(transient-define-suffix opencode-question-toggle ()
  "Toggle the option; in a single-choice question, choose it and continue."
  :class 'opencode-question-option
  :transient #'opencode--question-stay-if-multiple
  (interactive)
  (let ((index (oref (transient-suffix-object) option-index)))
    (if opencode-question--multiple
        (aset opencode-question--marked index (not (aref opencode-question--marked index)))
      (fillarray opencode-question--marked nil)
      (aset opencode-question--marked index t)
      (setq opencode-question--custom-text nil)
      (opencode--question-advance))))

(transient-define-suffix opencode-question-custom-answer ()
  "Type a custom answer."
  :class 'opencode-question-custom
  :transient #'opencode--question-stay-if-multiple
  (interactive)
  (let ((text (read-string "Custom answer: " opencode-question--custom-text)))
    (setq opencode-question--custom-text (unless (string-empty-p text) text))
    (unless opencode-question--multiple
      (if opencode-question--custom-text
          (progn (fillarray opencode-question--marked nil) (opencode--question-advance))
        (opencode--question-goto (opencode--question-index))))))

(defun opencode--question-confirm-transient ()
  "Leave the transient only once something is selected."
  (if (opencode--question-selection) transient--exit transient--stay))

(transient-define-suffix opencode-question-confirm ()
  "Confirm the selection of a multiple-choice question."
  :transient #'opencode--question-confirm-transient
  (interactive)
  (if (opencode--question-selection)
      (opencode--question-advance)
    (message "Nothing selected")))

(transient-define-suffix opencode-question-dismiss ()
  "Dismiss the question."
  :transient nil
  (interactive)
  (opencode--question-submit t))

(defun opencode--question-previous-transient ()
  "Leave the transient only when a previous question exists."
  (if (> (opencode--question-index) 0) transient--exit transient--stay))

(defun opencode--question-next-transient ()
  "Leave the transient only for an answered question that is not the last."
  (if (and (opencode--question-selection) (not (opencode--question-final-p)))
      transient--exit
    transient--stay))

(transient-define-suffix opencode-question-previous ()
  "Show the previous question with its saved answer."
  :transient #'opencode--question-previous-transient
  (interactive)
  (if (> (opencode--question-index) 0)
      (opencode--question-goto (1- (opencode--question-index)))
    (message "This is the first question")))

(transient-define-suffix opencode-question-next ()
  "Keep the current answer and show the next question."
  :transient #'opencode--question-next-transient
  (interactive)
  (cond
   ((not (opencode--question-selection)) (message "Answer this question first"))
   ((opencode--question-final-p) (message "Last question; confirm an answer to submit"))
   (t (opencode--question-advance))))

(defun opencode--question-setup-options (_children)
  "Return the option suffixes of the shown question."
  (transient-parse-suffixes
   'opencode-question-transient
   (append
    (cl-loop for index below (length opencode-question--labels)
             collect `(,(opencode--option-key index) opencode-question-toggle :option-index ,index))
    '(("!" opencode-question-custom-answer)))))

(transient-define-prefix opencode-question-transient ()
  "Answer an OpenCode question."
  [:class transient-column
   :description (lambda () (or opencode-question--heading "Question"))
   :setup-children opencode--question-setup-options]
  [["Actions"
    ("C-c C-c" "Confirm" opencode-question-confirm :if (lambda () opencode-question--multiple))
    ("q" "Dismiss" opencode-question-dismiss)]])

(opencode-install-hidden-navigation
 'opencode-question-transient
 (append '(("M-p" "Previous question" opencode-question-previous)
           ("M-n" "Next question" opencode-question-next))
         opencode-transient-scroll-suffixes))

(defun opencode--question-on-exit ()
  "Keep an unanswered question and its selection pending after the transient exits."
  (remove-hook 'transient-exit-hook #'opencode--question-on-exit)
  (unless opencode-question--handled
    (opencode--question-save)
    (if (opencode-transient-suspended-p 'opencode-question-transient)
        (message (substitute-command-keys "OpenCode question suspended; \\[opencode-respond] resumes it"))
      (setq opencode--active-form nil)
      (message (substitute-command-keys "OpenCode question deferred; \\[opencode-respond] resumes it")))))

(defun opencode--form-close-transient ()
  "Close the transient of the active form, resolved elsewhere."
  (setq opencode-question--handled t
        opencode--active-form nil)
  (cond
   ((opencode-transient-displayed-p 'opencode-question-transient)
    (transient--emergency-exit)
    (message "The OpenCode question was answered elsewhere"))
   ((opencode-transient-suspended-p 'opencode-question-transient)
    (pop transient--stack))))

;;;; Other forms

(defun opencode--read-field (field)
  "Read a value for FIELD of a form in the minibuffer, or nil to skip it."
  (let-alist field
    (let ((prompt (concat (or .title .description .key) ": ")))
      (pcase .type
        ("string" (if .options
                      (completing-read prompt (opencode--field-options field) nil (not .custom) nil nil .default)
                    (read-string prompt .default)))
        ((or "number" "integer") (read-number prompt .default))
        ("boolean" (if (y-or-n-p (concat (or .title .description .key) "? ")) t :false))
        ("multiselect" (vconcat (completing-read-multiple prompt (opencode--field-options field))))
        ("external" (when (y-or-n-p (format "Open %s? " .url)) (browse-url .url)) nil)))))

(defun opencode--answer-generic (entry)
  "Answer form ENTRY field by field in the minibuffer."
  (let ((answer nil))
    (dolist (field (opencode-form-fields (plist-get entry :form)))
      (let ((value (opencode--read-field field)))
        (when value (push (cons (intern (alist-get 'key field)) value) answer))))
    (plist-put entry :submitted t)
    (opencode-request 'post (format "/api/session/%s/form/%s/reply" opencode-session-id (plist-get entry :id))
                      :body `((answer . ,(or (nreverse answer) (opencode-empty-object))))
                      :then (lambda (_) (opencode--form-resolved (plist-get entry :id))))))

;;;; Answering

(defun opencode-respond-form (&optional entry)
  "Answer form ENTRY, else the oldest open form of this session.
A question transient suspended from this buffer is resumed instead."
  (interactive)
  (let ((entry (or entry (car (opencode-open-forms)))))
    (cond
     ((and entry (eq entry (opencode--active-entry))
           (opencode-transient-suspended-p 'opencode-question-transient))
      (add-hook 'transient-exit-hook #'opencode--question-on-exit)
      (transient-resume))
     ((bound-and-true-p transient--prefix)
      (user-error "Finish or suspend the active transient first"))
     ((null entry) (user-error "No pending OpenCode question"))
     ((not (opencode-form-question-p (plist-get entry :form)))
      (opencode--answer-generic entry))
     (t
      (when (opencode-transient-suspended-p 'opencode-question-transient)
        (pop transient--stack))
      (setq opencode--active-form (list :buffer (current-buffer) :entry entry))
      (opencode--question-show)))))

(defun opencode--form-at-point ()
  "Return the open form whose question block is at point."
  (when-let* ((overlay (opencode-block-at (point)))
              (data (opencode-block-data overlay))
              ((equal (plist-get data :tool) "question"))
              (entry (opencode--form-find :call-id (plist-get data :key))))
    (unless (plist-get entry :submitted) entry)))

(defun opencode--form-provider ()
  "Describe this buffer's pending forms for `opencode-respond'."
  (list :pending (opencode-open-forms)
        :at-point (when-let* ((entry (opencode--form-at-point)))
                    (lambda () (opencode-respond-form entry)))
        :respond #'opencode-respond-form))

(add-to-list 'opencode-request-providers #'opencode--form-provider t)

;;;; Events and synchronization

(defun opencode--form-created (event)
  "Queue the form of EVENT in its session's buffer, opening one in the background."
  (let* ((form (alist-get 'form (alist-get 'data event)))
         (session (alist-get 'sessionID form)))
    (unless (equal session "global")
      (if-let* ((buffer (gethash session opencode-session-buffers))
                ((buffer-live-p buffer)))
          (with-current-buffer buffer (opencode-form-queue form))
        (opencode-session-open-by-id session :no-display t
                                     :callback (lambda (_info) (opencode-form-queue form)))))))

(defun opencode--form-settled (event)
  "Forget the form EVENT says was answered or cancelled, by any client."
  (let ((data (alist-get 'data event)))
    (opencode-with-session-buffer (alist-get 'sessionID data)
      (opencode--form-resolved (alist-get 'id data)))))

(defun opencode-form-sync (&optional _info)
  "Reconcile this buffer's form queue with the server's pending forms.
Only forms queued before the request started are forgotten, so one asked
or answered meanwhile is not misfiled; a forgotten form whose block never
completed was lost by the server and is reported."
  (when opencode-session-id
    (let ((known (opencode-open-forms)))
      (opencode-request
       'get (format "/api/session/%s/form" opencode-session-id)
       :then (lambda (response)
               (let* ((forms (opencode-request-data response))
                      (ids (mapcar (lambda (form) (alist-get 'id form)) forms)))
                 (dolist (entry known)
                   (when (and (memq entry opencode-pending-forms)
                              (not (plist-get entry :submitted))
                              (not (member (plist-get entry :id) ids)))
                     (let ((overlay (opencode--form-block entry)))
                       (opencode--form-resolved (plist-get entry :id))
                       (unless (and overlay (member (plist-get (opencode-block-data overlay) :status)
                                                    '("completed" "error")))
                         (opencode-output-notice
                          (format "The OpenCode service no longer has the question “%s”; answer it in a normal message."
                                  (or (alist-get 'title (plist-get entry :form)) (plist-get entry :id)))
                          'opencode-attention)))))
                 (dolist (form forms)
                   (opencode-form-queue form))))
       :else (lambda (error) (message "OpenCode question sync failed: %s"
                                      (opencode-error-describe error)))))))

(opencode-events-register "form.created" #'opencode--form-created)
(opencode-events-register "form.replied" #'opencode--form-settled)
(opencode-events-register "form.cancelled" #'opencode--form-settled)
(add-hook 'opencode-session-resync-functions #'opencode-form-sync)
(add-hook 'opencode-session-replayed-hook #'opencode-form-sync)

(provide 'opencode-form)
;;; opencode-form.el ends here
