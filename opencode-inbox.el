;;; opencode-inbox.el --- Prompts waiting for their delivery  -*- lexical-binding: t; -*-

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

;; The service admits a prompt to its session's inbox and delivers it
;; later: a steering prompt at the running turn's next step, a queued
;; one once the turn ends.  Only then is it part of the conversation.
;; Meanwhile a session buffer lists it as pending, between the
;; transcript and the input, in the order the service will deliver it,
;; from the moment it is sent; the delivery moves it into the
;; transcript, in one change, where replay places it.  The inbox's
;; events (`session.inbox.enqueued', `delivered', `cancelled' and
;; `delivery.changed') keep the list, and loading or catching up
;; rebuilds it from `GET /api/session/:id/inbox', so prompts of other
;; clients show too.
;;
;; The area is an overlay string, not text: the transcript ends at the
;; process mark, where streamed output arrives, and the input starts
;; there, so text between them would have to move with every chunk.
;; It never holds point and is never part of the input.
;;
;; `opencode-edit-pending-prompt', or a click on a pending prompt,
;; withdraws it: the service cancels it, and its text, with its
;; attachments, goes back to the input to be edited and sent again.
;; The service cannot edit a pending prompt, only cancel it, and
;; cancels nothing already delivered: a prompt delivered first stays
;; sent, and is said so.

;;; Code:

(require 'cl-lib)
(require 'opencode-common)
(require 'opencode-http)
(require 'opencode-look)
(require 'opencode-render)

(declare-function opencode--input-shown-p "opencode-session" (id))
(declare-function opencode--prompt-buffer "opencode-session" ())
(declare-function opencode--restore-withdrawn "opencode-session" (id text))
(defvar opencode--holding)

(defcustom opencode-pending-prompt-lines 5
  "Lines of a pending prompt shown, at most; the rest is elided.
The whole prompt shows in its tooltip."
  :type 'natnum
  :group 'opencode)

;;;; The inbox

(defvar-local opencode--inbox nil
  "Items of this buffer's session that its inbox holds, in delivery order.
Each is (ID . PLIST), mutated in place: :item, the inbox item as the
service reports it; :delivery, \"steer\" or \"queue\"; :placed, non-nil
once an event or a listing told its place, the entries placed coming
first; :admitted, non-nil once the service took it; :text, for a prompt
sent from here, its text as `opencode-old-input' returns one, the
labels of its attachments carrying them; :typed, non-nil for a prompt
typed at this buffer's input; :withdraw, `waiting' while a withdrawal
waits for the service to take the prompt, `asked' once asked; and
:outcome, `delivered' or `cancelled', once it left the inbox.")

(defun opencode-inbox-entry (id)
  "Return the entry of inbox item ID, or nil."
  (assoc id opencode--inbox))

(defun opencode-inbox--set (entry &rest pairs)
  "Set the properties PAIRS of ENTRY, in place."
  (while pairs
    (setcdr entry (plist-put (cdr entry) (pop pairs) (pop pairs)))))

(defun opencode-inbox--place (entry)
  "Put ENTRY after the entries already placed, as the service placed it last."
  (let* ((rest (delq entry opencode--inbox))
         (placed (seq-take-while (lambda (other) (plist-get (cdr other) :placed)) rest)))
    (opencode-inbox--set entry :placed t)
    (setq opencode--inbox (append placed (list entry) (nthcdr (length placed) rest)))))

(defun opencode-inbox-add-local (id text delivery &optional typed)
  "Show prompt ID, with TEXT and DELIVERY, as pending before the service takes it.
TYPED means it was typed at this buffer's input, which a failure gives
it back to."
  (unless (opencode-inbox-entry id)
    (setq opencode--inbox
          (append opencode--inbox (list (list id :text text :delivery delivery :typed typed))))
    (opencode-inbox-update)))

(defun opencode-inbox-enqueued (id item)
  "Note that the inbox admitted ITEM as ID, after the items it holds."
  (let ((entry (or (opencode-inbox-entry id) (list id))))
    (unless (plist-get (cdr entry) :outcome)
      (opencode-inbox--set entry :item item :delivery (alist-get 'delivery item) :admitted t)
      (opencode-inbox--place entry)
      (opencode-inbox--withdraw-if-waiting entry)
      (opencode-inbox-update))))

(defun opencode-inbox-admitted (id item)
  "Note that the service answered that it took prompt ID as ITEM.
The answer may come before or after the event admitting it, which
alone tells its place."
  (when-let* ((entry (opencode-inbox-entry id)))
    (opencode-inbox--set entry :admitted t)
    (when (and item (not (plist-get (cdr entry) :item)))
      (opencode-inbox--set entry :item item))
    (opencode-inbox--withdraw-if-waiting entry)))

(defun opencode-inbox-delivery-changed (id delivery)
  "Note that inbox item ID is now delivered as DELIVERY."
  (when-let* ((entry (opencode-inbox-entry id)))
    (opencode-inbox--set entry :delivery delivery)
    (opencode-inbox-update)))

(defun opencode-inbox--settle (entry outcome)
  "Note that ENTRY left the inbox with OUTCOME; return its plist.
A prompt being withdrawn goes back to the input when cancelled, and is
said delivered otherwise.  Only the first outcome counts."
  (unless (plist-get (cdr entry) :outcome)
    (setq opencode--inbox (delq entry opencode--inbox))
    (opencode-inbox--set entry :outcome outcome)
    (when (plist-get (cdr entry) :withdraw)
      (if (eq outcome 'cancelled)
          (opencode--restore-withdrawn (car entry) (opencode-inbox--text entry))
        (message "The prompt was delivered before it could be withdrawn")))
    (opencode-inbox-update))
  (cdr entry))

(defun opencode-inbox-leave (id outcome)
  "Note that inbox item ID left the inbox with OUTCOME; return its plist, or nil.
OUTCOME is `delivered' or `cancelled'."
  (when-let* ((entry (opencode-inbox-entry id)))
    (opencode-inbox--settle entry outcome)))

(defun opencode-inbox-drop (id)
  "Forget prompt ID, which the service did not take; return its plist, or nil."
  (when-let* ((entry (opencode-inbox-entry id)))
    (setq opencode--inbox (delq entry opencode--inbox))
    (opencode-inbox-update)
    (cdr entry)))

(defun opencode-inbox-reset (items)
  "Rebuild the inbox from ITEMS, the service's listing of it, oldest first.
Entries listed keep what this buffer knows of them, such as the text a
prompt sent from here was typed with.  Those the listing lacks left the
inbox, delivered or cancelled, except prompts whose request has not
returned yet, which stay after them.  A withdrawal waiting for the
service to take its prompt is asked once the prompt is listed."
  (let ((old opencode--inbox)
        (new nil))
    (dolist (item items)
      (let* ((id (alist-get 'id item))
             (entry (or (assoc id old) (list id))))
        (opencode-inbox--set entry :item item :delivery (alist-get 'delivery item)
                             :placed t :admitted t)
        (push entry new)))
    (dolist (entry old)
      (unless (or (assoc (car entry) new)
                  (plist-get (cdr entry) :placed)
                  (plist-get (cdr entry) :admitted))
        (push entry new)))
    (setq opencode--inbox (nreverse new))
    (mapc #'opencode-inbox--withdraw-if-waiting opencode--inbox)
    (opencode-inbox-update)))

(defun opencode-inbox-fetch (then)
  "Rebuild this buffer's inbox from the service's, then call THEN.
A failure keeps the inbox as it was: its events correct it."
  (opencode-request
   'get (format "/api/session/%s/inbox" opencode-session-id)
   :then (lambda (response)
           (opencode-inbox-reset (opencode-request-data response))
           (funcall then))
   :else (lambda (_) (funcall then))))

;;;; Text of a pending prompt

(defun opencode-inbox--utf16-index (text offset)
  "Return the index in TEXT of OFFSET, counted in UTF-16 code units, or nil."
  (let ((units 0)
        (index 0))
    (while (and (< units offset) (< index (length text)))
      (setq units (+ units (if (> (aref text index) #xFFFF) 2 1))
            index (1+ index)))
    (and (= units offset) index)))

(defun opencode-inbox--attachment-label (attachment)
  "Return the label shown for ATTACHMENT, as typed labels show it."
  (pcase attachment
    (`(:agent ,name . ,_) (concat "@" name))
    (`(:skill ,id . ,rest) (format "`%s`" (or (plist-get rest :name) id)))
    (_ (format "`%s`" (or (alist-get 'name (plist-get attachment :file)) "file")))))

(defun opencode-inbox-attachments-text (text attachments)
  "Return TEXT with ATTACHMENTS labeled, as `opencode-old-input' returns a prompt.
ATTACHMENTS is a list of (ATTACHMENT . MENTION): ATTACHMENT is a plist
such as (:file URI-ALIST), (:agent NAME) or (:skill ID :name NAME), and
MENTION, when non-nil, the range (START . END) of TEXT naming it, which
becomes its label; the others' labels follow TEXT.  Each label carries
its attachment as its `opencode-sent-attachment' property."
  (let ((text (copy-sequence text))
        (tail nil))
    (pcase-dolist (`(,attachment . ,mention) attachments)
      (if mention
          (put-text-property (car mention) (cdr mention) 'opencode-sent-attachment attachment text)
        (push (propertize (opencode-inbox--attachment-label attachment)
                          'opencode-sent-attachment attachment)
              tail)))
    (string-join (cons text (nreverse tail)) " ")))

(defun opencode-inbox--mention (text mention)
  "Return the range of TEXT that MENTION, a `Prompt.Mention', names, or nil."
  (when-let* ((start (opencode-inbox--utf16-index text (or (alist-get 'start mention) -1)))
              (end (opencode-inbox--utf16-index text (or (alist-get 'end mention) -1)))
              ((< start end))
              ((equal (substring text start end) (alist-get 'text mention))))
    (cons start end)))

(defun opencode-inbox-payload-text (payload)
  "Return the prompt of user inbox PAYLOAD as `opencode-old-input' returns one.
Its files, subagents and skills label themselves where the prompt
mentions them, else after it.  A file read from a URI comes back as
that URI, read again when sent; one sent inline as its data."
  (let ((text (or (alist-get 'text payload) ""))
        (attachments nil))
    (dolist (file (alist-get 'files payload))
      (let* ((name (or (alist-get 'name file) "file"))
             (source (alist-get 'source file))
             (uri (if (equal (alist-get 'type source) "uri")
                      (alist-get 'uri source)
                    (format "data:%s;base64,%s" (or (alist-get 'mime file) "application/octet-stream")
                            (alist-get 'data file)))))
        (push (cons (list :file `((uri . ,uri) (name . ,name)))
                    (opencode-inbox--mention text (alist-get 'mention file)))
              attachments)))
    (dolist (agent (alist-get 'agents payload))
      (push (cons (list :agent (alist-get 'name agent))
                  (opencode-inbox--mention text (alist-get 'mention agent)))
            attachments))
    (dolist (skill (alist-get 'skills payload))
      (push (cons (list :skill (alist-get 'id skill) :name (or (alist-get 'name skill) (alist-get 'id skill)))
                  (opencode-inbox--mention text (alist-get 'mention skill)))
            attachments))
    (opencode-inbox-attachments-text text (nreverse attachments))))

(defun opencode-inbox--text (entry)
  "Return the prompt ENTRY stands for, as `opencode-old-input' returns one."
  (or (plist-get (cdr entry) :text)
      (opencode-inbox-payload-text (alist-get 'payload (plist-get (cdr entry) :item)))))

;;;; The pending area

(defvar-local opencode--inbox-overlay nil
  "Overlay showing the pending prompts as its string, or nil.")

(defvar-keymap opencode-pending-prompt-map
  :doc "Keys of a pending prompt, which a click withdraws to edit."
  "<mouse-1>" #'opencode-pending-prompt-click
  "<mouse-2>" #'opencode-pending-prompt-click)

(defun opencode-inbox--shown ()
  "Return the entries the pending area shows, in delivery order.
Those are the prompts not delivered yet that the transcript does not
show already, as one typed here at an idle session does: other
clients' prompts and commands', and those sent here while a turn ran."
  (seq-filter (lambda (entry)
                (let ((plist (cdr entry)))
                  (and (not (plist-get plist :outcome))
                       (or (plist-get plist :text)
                           (equal (alist-get 'type (plist-get plist :item)) "user"))
                       (not (opencode--input-shown-p (car entry))))))
              opencode--inbox))

(defun opencode-inbox-shown-p ()
  "Return non-nil when this buffer shows a pending prompt."
  (and (opencode-inbox--shown) t))

(defun opencode-pending-prompts (id)
  "Return how many pending prompts of session ID can be withdrawn, maybe 0.
Those are the prompts the buffer of session ID shows as pending, but
for those being withdrawn already, which `opencode-edit-pending-prompt'
withdraws.  The buffer keeps the inbox: a session without one counts
none.  It costs little, for menus telling whether they apply."
  (if-let* ((buffer (gethash id opencode-session-buffers))
            ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (seq-count (lambda (entry) (not (plist-get (cdr entry) :withdraw)))
                   (opencode-inbox--shown)))
    0))

(defun opencode-inbox--label (entry)
  "Return the line heading pending ENTRY."
  (let ((plist (cdr entry)))
    (concat (if (string-empty-p opencode-pending-prompt-icon) "" (concat opencode-pending-prompt-icon " "))
            "Pending: "
            (if (equal (plist-get plist :delivery) "queue")
                "starts after the turn"
              "joins the turn at its next step")
            (pcase (plist-get plist :withdraw)
              ('waiting ", withdrawn once the service has it")
              ('asked ", being withdrawn")))))

(defun opencode-inbox--entry-string (entry)
  "Return the lines showing pending ENTRY, each ending with a newline."
  (let* ((text (string-trim (opencode-inbox--text entry)))
         (lines (split-string text "\n"))
         (shown (seq-take lines (max 1 opencode-pending-prompt-lines)))
         (margin (opencode-margin 'opencode-pending-margin))
         (body (mapconcat
                (lambda (line)
                  (let ((line (copy-sequence line)))
                    (add-face-text-property 0 (length line) 'opencode-pending-prompt t line)
                    (let ((pos 0))
                      (while (setq pos (text-property-not-all pos (length line) 'opencode-sent-attachment nil line))
                        (let ((next (or (next-single-property-change pos 'opencode-sent-attachment line)
                                        (length line))))
                          (add-face-text-property pos next 'opencode-pending-attachment nil line)
                          (setq pos next))))
                    (concat margin line "\n")))
                shown ""))
         (string (concat margin (propertize (opencode-inbox--label entry) 'face 'opencode-pending-label) "\n"
                         body
                         (when (> (length lines) (length shown))
                           (concat margin (propertize "…" 'face 'opencode-pending-prompt) "\n")))))
    (add-text-properties 0 (length string)
                         `(opencode-inbox-id ,(car entry)
                           keymap ,opencode-pending-prompt-map
                           mouse-face highlight
                           pointer hand
                           help-echo ,(concat (substring-no-properties text)
                                              "\n\nmouse-1: withdraw this prompt to edit it"))
                         string)
    string))

(defun opencode-inbox--body (entries)
  "Return the string showing pending ENTRIES, a blank line after each."
  (mapconcat (lambda (entry) (concat (opencode-inbox--entry-string entry) "\n")) entries ""))

(defun opencode-inbox--anchor (&rest _)
  "Keep the pending area between the transcript and the input.
That is where points following output start: at the idle prompt, else
at the process mark.  Output inserted there and the idle prompt moving
change it, so this runs after every change and before redisplay."
  (when-let* ((overlay opencode--inbox-overlay)
              ((overlay-buffer overlay))
              ((get-buffer-process (current-buffer))))
    (save-restriction
      (widen)
      (let* ((at (opencode--follow-start))
             ;; The area starts after a blank line, whatever ends the transcript.
             (lead (cond ((= at (point-min)) "")
                         ((/= (char-before at) ?\n) "\n\n")
                         ((or (= (1- at) (point-min)) (/= (char-before (1- at)) ?\n)) "\n")
                         (t ""))))
        (unless (= (overlay-start overlay) at)
          (move-overlay overlay at at))
        (unless (equal lead (overlay-get overlay 'opencode-inbox-lead))
          (overlay-put overlay 'opencode-inbox-lead lead)
          (overlay-put overlay 'before-string
                       (concat lead (overlay-get overlay 'opencode-inbox-body))))))))

(defun opencode-inbox--anchor-window (_window)
  "Keep the pending area in place before redisplay, as `pre-redisplay-functions' run it."
  (opencode-inbox--anchor))

(defun opencode-inbox-update ()
  "Show the pending prompts as the inbox holds them.
While the buffer withholds its session's events, as when it loads or
catches up, the area stays as it was, and is shown anew once they are
applied: the transcript being written may already hold prompts the
withheld deliveries take out of it."
  (when (and (get-buffer-process (current-buffer)) (not opencode--holding))
    (let ((entries (opencode-inbox--shown)))
      (if (null entries)
          (when (overlayp opencode--inbox-overlay)
            (delete-overlay opencode--inbox-overlay)
            (setq opencode--inbox-overlay nil))
        (unless (and (overlayp opencode--inbox-overlay) (overlay-buffer opencode--inbox-overlay))
          (let ((at (save-restriction (widen) (opencode--follow-start))))
            (setq opencode--inbox-overlay (make-overlay at at nil t t))
            (overlay-put opencode--inbox-overlay 'opencode-inbox t)))
        (overlay-put opencode--inbox-overlay 'opencode-inbox-body (opencode-inbox--body entries))
        (overlay-put opencode--inbox-overlay 'opencode-inbox-lead nil)
        (opencode-inbox--anchor)))))

(defun opencode-inbox-setup ()
  "Keep this session buffer's pending area in place."
  (add-hook 'after-change-functions #'opencode-inbox--anchor nil t)
  (add-hook 'pre-redisplay-functions #'opencode-inbox--anchor-window nil t))

(defun opencode-inbox-area-string ()
  "Return the text the pending area shows, or nil when it shows nothing."
  (when (and (overlayp opencode--inbox-overlay) (overlay-buffer opencode--inbox-overlay))
    (substring-no-properties (overlay-get opencode--inbox-overlay 'before-string))))

;;;; Withdrawing

(defcustom opencode-withdrawn-prompt-function nil
  "Function given a pending prompt withdrawn to be edited, or nil.
It is called in the session buffer with the prompt's text, as
`opencode-old-input' returns one, the labels of its attachments
carrying them, and that buffer.  Nil stands for `opencode-put-old-input',
which puts it in the session's input, its attachments with it.  The
echo area says the prompt was withdrawn before it is called, so that a
function of your own may tell where it went instead."
  :type '(choice (const :tag "The session's input" nil) function)
  :group 'opencode)

(defun opencode-inbox--withdraw-if-waiting (entry)
  "Ask the service to cancel ENTRY when a withdrawal waits for it to be taken."
  (when (and (eq (plist-get (cdr entry) :withdraw) 'waiting)
             (not (plist-get (cdr entry) :outcome)))
    (opencode-inbox--cancel entry)))

(defun opencode-inbox--refused (entry reason)
  "Keep ENTRY pending, its withdrawal refused for REASON, which is told."
  (opencode-inbox--set entry :withdraw nil)
  (opencode-inbox-update)
  (message "%s" reason))

(defun opencode-inbox--cancel (entry)
  "Ask the service to cancel pending ENTRY, then find out whether it did.
The service answers alike whether it cancelled the prompt or found it
delivered already, and its event may come after the answer, so unless
an event told meanwhile, the message is looked up: it exists once
delivered, and never once cancelled."
  (let ((id (car entry))
        (session opencode-session-id))
    (opencode-inbox--set entry :withdraw 'asked)
    (opencode-inbox-update)
    (opencode-request
     'delete (format "/api/session/%s/inbox/%s" session id)
     :then
     (lambda (_)
       (unless (plist-get (cdr entry) :outcome)
         (opencode-request
          'get (format "/api/session/%s/message/%s" session id)
          :then (lambda (_)
                  (unless (plist-get (cdr entry) :outcome)
                    (opencode-inbox--refused
                     entry "The prompt was delivered before it could be withdrawn")))
          :else (lambda (error)
                  (unless (plist-get (cdr entry) :outcome)
                    (if (eql (opencode-error-status error) 404)
                        (opencode-inbox--settle entry 'cancelled)
                      (opencode-inbox--refused
                       entry (format "Could not tell whether the prompt was withdrawn: %s"
                                     (opencode-error-describe error)))))))))
     :else
     (lambda (error)
       (opencode-inbox--refused
        entry
        (if (and (eql (opencode-error-status error) 404) (null (opencode-error-tag error)))
            "This OpenCode service cannot withdraw a pending prompt: it stays pending"
          (format "Could not withdraw the prompt: %s" (opencode-error-describe error))))))))

(defun opencode-inbox--withdraw (entry)
  "Withdraw pending ENTRY to edit it."
  (cond
   ((plist-get (cdr entry) :withdraw)
    (message "The prompt is being withdrawn already"))
   ((or (plist-get (cdr entry) :placed) (plist-get (cdr entry) :admitted))
    (opencode-inbox--cancel entry))
   (t
    ;; Cancelling a prompt the service has not taken yet would do nothing.
    (opencode-inbox--set entry :withdraw 'waiting)
    (opencode-inbox-update))))

(defun opencode-inbox--choice-label (entry)
  "Return the line naming pending ENTRY among others."
  (truncate-string-to-width
   (car (split-string (string-trim (substring-no-properties (opencode-inbox--text entry))) "\n"))
   80 nil nil "…"))

(defun opencode-edit-pending-prompt (&optional id session)
  "Withdraw a pending prompt of this session and put it back in the input.
A pending prompt is one the service took but has not delivered to the
session yet; the session buffer shows them between the transcript and
the input.  With several, the one to withdraw is read, the newest by
default; ID names it.  The service cancels it, and its text and
attachments go to `opencode-withdrawn-prompt-function', the input by
default, to be edited and sent again.  A prompt delivered meanwhile
cannot be withdrawn, which is said.  The session is SESSION, an ID,
whose buffer keeps its pending prompts and need not be shown; else this
buffer's, else the last selected one's (see `opencode-pending-prompts')."
  (interactive)
  (with-current-buffer (if session
                           (let ((buffer (gethash session opencode-session-buffers)))
                             (if (buffer-live-p buffer)
                                 buffer
                               (user-error "No prompt of this session is pending")))
                         (opencode--prompt-buffer))
    (let* ((entries (opencode-inbox--shown))
           (entry (cond
                   (id (or (assoc id entries) (user-error "That prompt is no longer pending")))
                   ((null entries) (user-error "No prompt of this session is pending"))
                   ((null (cdr entries)) (car entries))
                   (t (let* ((choices (let ((seen (make-hash-table :test #'equal)))
                                        (mapcar (lambda (entry)
                                                  (let* ((label (opencode-inbox--choice-label entry))
                                                         (count (cl-incf (gethash label seen 0))))
                                                    (cons (if (> count 1) (format "%s <%d>" label count) label)
                                                          entry)))
                                                entries)))
                             (choice (completing-read "Withdraw pending prompt: "
                                                      (mapcar #'car choices) nil t nil nil
                                                      (car (car (last choices))))))
                        (cdr (assoc choice choices)))))))
      (opencode-inbox--withdraw entry))))

(defun opencode-pending-prompt-click (event)
  "Withdraw the pending prompt clicked to edit it, as `opencode-edit-pending-prompt'.
EVENT is the click."
  (interactive "e")
  (let* ((end (event-end event))
         (window (posn-window end))
         (object (posn-string end))
         (id (and (consp object)
                  (get-text-property (or (cdr object) 0) 'opencode-inbox-id (car object)))))
    (when (and id (windowp window))
      (with-current-buffer (window-buffer window)
        (opencode-edit-pending-prompt id)))))

(provide 'opencode-inbox)
;;; opencode-inbox.el ends here
