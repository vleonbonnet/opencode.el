;;; opencode-session.el --- Session buffers  -*- lexical-binding: t; -*-

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

;; One buffer per session renders its transcript and takes its input.
;; Opening a session replays the server's projected messages; from then
;; on the event stream drives it: a step is one assistant message, text
;; and reasoning stream as started/delta/ended fragments numbered by
;; ordinal within their step, and each tool call moves through input,
;; called, progress and success or failure.  A turn runs from the
;; execution starting until it succeeds, fails or is interrupted.
;;
;; The server keeps each session's agent and model; Emacs shows and
;; switches them.  Prompts carry an ID generated here, so the echo of
;; Emacs's own prompt on the stream is recognized while prompts sent by
;; other clients are rendered.  A prompt sent while a turn runs steers
;; it by default (`opencode-prompt-delivery'), and shows as pending until
;; the service delivers it (see opencode-inbox.el).
;;
;; A shell command or subagent run in the background ends its tool call
;; at once.  The server posts the job's completion later as a synthetic
;; message, which is drawn as a block of the same tool, colored by how
;; the job ended.
;;
;; Nothing is replayed after the stream reconnects, so every open buffer
;; catches up with the server then: it fetches the messages after the
;; last one it knows complete and writes only those again, and renders
;; its whole transcript again only when that message is gone, as after
;; a revert.

;;; Code:

(require 'comint)
(require 'opencode-blocks)
(require 'opencode-common)
(require 'opencode-events)
(require 'opencode-http)
(require 'opencode-inbox)
(require 'opencode-links)
(require 'opencode-notify)
(require 'opencode-render)
(require 'opencode-state)
(require 'savehist)
(require 'yank-media)

(defcustom opencode-prompt-delivery 'steer
  "How a prompt sent while a turn runs is delivered.
`steer' joins the running turn at its next step; `queue' waits for the
turn to finish and starts the next one.  An idle session starts a turn
either way."
  :type '(choice (const steer) (const queue))
  :group 'opencode)

(defcustom opencode-show-switch-notices t
  "When non-nil, show agent and model switches in the transcript."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-session-deleted-functions nil
  "Abnormal hook run with a session ID when that session is deleted."
  :type 'hook
  :group 'opencode)

(defcustom opencode-session-idle-functions nil
  "Abnormal hook run in a session buffer when its turn ends.
Each function receives the outcome: `succeeded', `failed' or `interrupted'."
  :type 'hook
  :group 'opencode)

(defcustom opencode-before-send-functions nil
  "Abnormal hook run before a prompt or command is sent.
Each function receives a plist (:kind :text :name) and may return
`cancel' to keep the input in the prompt instead."
  :type 'hook
  :group 'opencode)

(defcustom opencode-old-input-function nil
  "Function given what RET picks in the transcript, or nil to put it in the input.
\\<opencode-session-mode-map>\\[opencode-send-input] in the transcript of a session buffer, on an earlier
prompt, the agent's text, a block's summary or any other line, calls it,
in that buffer, point and the region as the user left them, with the
text picked and the session buffer; nothing is sent.  The text is the
active region's, else the line at point, as `opencode-transcript-text'
returns it: what overlays hide, such as the blocks of a collapsed
group, is left out.  The labels of the attachments that can go with a
prompt again, subagent mentions and files still on disk, carry the
attachment as their `opencode-sent-attachment' property (see
`opencode-old-input').  Links and other text with keys of their own
keep their RET.  Nil stands for `opencode-put-old-input', which puts the
text in the session's own input; \\[opencode-copy-old-input] puts a whole earlier prompt
there whatever this says.  Packages that quote text elsewhere, such as
drover, set it when it is nil, so that a function of the user's own
stays."
  :type '(choice (const :tag "Into the session's input" nil) function)
  :group 'opencode)

(defvar opencode-step-ended-functions nil
  "Functions called in the session buffer with each `session.step.ended' data.")

(defvar opencode-compaction-ended-functions nil
  "Functions called in the session buffer with each `session.compaction.ended' data.")

(defvar opencode-turn-ended-functions nil
  "Functions called in the session buffer with the outcome of each turn that ends.
They run before `opencode-session-idle-functions'.")

(defvar opencode-step-failed-functions nil
  "Functions called with `session.step.failed' data; non-nil handles its display.")

(defvar opencode-session-replayed-hook nil
  "Hook run in a session buffer after its transcript was replayed.")

(defvar opencode-session-resync-functions nil
  "Functions called in each session buffer after the stream reconnects.
Each receives the session info fetched from the server.")

(defvar opencode-last-model nil
  "Model reference (alist id, providerID, variant) chosen last for a session.
A model is chosen by switching a session to it (`opencode-switch-model',
which switching to an agent pinning a model does too) or by creating a
session on it (`opencode-session-create' given a MODEL).  Sending a
prompt chooses nothing: a session keeps its model, and its prompts,
however many, must not make that model the one new sessions start on.
See `opencode-new-session-model'.")
(add-to-list 'savehist-additional-variables 'opencode-last-model)

(defvar-local opencode-session-info nil
  "Session info alist as last fetched from the server.")

(defvar-local opencode-session-status 'idle
  "`idle', or `busy' while a turn runs.")

(defvar-local opencode-session-agent nil
  "Agent ID the session runs with.")

(defvar-local opencode-session-model nil
  "Model reference (alist id, providerID, variant) the session runs with.")

(defvar-local opencode-session-tokens nil
  "Context tokens of the session, or nil when unknown.
They are what the last step reporting usage reported, or, once a
compaction completed after it, the service's estimate of the next
request, which `opencode-session-tokens-estimated' then marks.")

(defvar-local opencode-session-tokens-estimated nil
  "Non-nil when `opencode-session-tokens' is the service's estimate.
A completed compaction reports one; the next step reporting usage
replaces it with the measured size.")

(defvar-local opencode-session-retry nil
  "Data of the retry the current step is waiting for, or nil.")

(defvar-local opencode-session-revert nil
  "Staged revert of this session, or nil.")

(defvar-local opencode--own-inputs nil
  "Hash table of inbox IDs sent from this buffer.")

(defvar-local opencode--sent-inputs nil
  "Hash table of the input regions waiting for the message ID they sent.
Keys are message IDs; a command's key is its correlation key until the
prompt it makes is admitted.  Values are plists (:start MARKER :end
MARKER), with :command for a command, :answered for one whose request
returned at that time, and :lost once a reload erased the region.
Read it with `opencode--sent-input-table'.")

(defun opencode--sent-input-table ()
  "Return `opencode--sent-inputs', made on first use.
A buffer made before this table existed, such as one open while
opencode.el was reloaded, has none until then."
  (or opencode--sent-inputs
      (setq opencode--sent-inputs (make-hash-table :test #'equal))))

(defvar-local opencode--delivery-points nil
  "Hash table of the message IDs of input typed here to (MARKER . COUNT).
MARKER is where the transcript ended when the service delivered the
message, which is where the conversation places it, and COUNT orders
messages delivered there; see `opencode--conversation-position'.")

(defvar-local opencode--delivery-count 0
  "Number of messages of input typed here that were delivered.")

(defvar-local opencode--text-blocks nil
  "Hash table of the text block keys of open messages to plists (:text STRING).
:text is what is shown of the block; :done marks one that ended.  A
message's keys go once it is finished, so the table stays small.")

(defvar-local opencode--current-assistant nil
  "ID of the assistant message streaming in this buffer.")

(defvar-local opencode--reasoning-native nil
  "Hash table of reasoning keys to the native data their start event carried.")

(defvar-local opencode--attachments nil
  "Files and agents to send with the next prompt, newest first.
Each is a plist (:file URI-ALIST) or (:agent NAME).")

(defvar-local opencode--temp-files nil
  "Temporary files to delete once sent or when the buffer is killed.")

(defvar-local opencode--replaying nil
  "Non-nil while the transcript is being replayed.
Nothing replayed is live: it never runs the hooks of live events, such
as those processing the files an edit changed.")

(defvar-local opencode--render-generation 0
  "Count of the times the transcript was erased or cut.
A replay sliced over timers stops when it changes.")

(defvar-local opencode--turn-errors nil
  "Error messages already shown during the current turn.")

(defvar-local opencode--location-generation 0
  "Count of live moves, to detect when one overtakes a session fetch.")

(defun opencode-empty-object ()
  "Return a value `json-serialize' encodes as an empty object."
  (make-hash-table))

;;;; Catalogs

;; Displays read a location's catalogs at redisplay and never wait, so
;; fetching them is left to timers, a few at a time.  A reconnection
;; most likely changed no catalog, yet the service may have restarted
;; with another configuration: every cached catalog is then marked
;; stale rather than forgotten, displays keep showing it, and the first
;; display asking for one again fetches it anew.  Only a catalog that
;; arrives different counts as changed, and only the mode lines of its
;; location's session buffers are updated then, once per tick.

(defcustom opencode-catalog-requests 4
  "Number of catalogs fetched at once for displays, at most.
See `opencode-catalog-ensure'."
  :type 'natnum
  :group 'opencode)

(opencode-defvar-service opencode--catalogs (make-hash-table :test #'equal)
  "Location catalogs, keyed by (DIRECTORY . KIND).
Stale ones, which `opencode--catalog-stale' holds, are included.")

(opencode-defvar-service opencode--catalog-stale (make-hash-table :test #'equal)
  "Keys of the cached catalogs to fetch again before trusting them, mapped to t.")

(opencode-defvar-service opencode--catalog-waiters (make-hash-table :test #'equal)
  "Callbacks waiting for a catalog fetch in flight, keyed like `opencode--catalogs'.
Each is (CALLBACK . ELSE); a key is present exactly while its fetch runs.")

(opencode-defvar-service opencode--catalog-scheduled (make-hash-table :test #'equal)
  "Keys, like those of `opencode--catalogs', queued in `opencode--catalog-queue'.")

(opencode-defvar-service opencode--catalog-queue nil
  "Keys of the catalogs displays asked for that wait to be fetched, oldest first.")

(opencode-defvar-service opencode--catalog-timer nil
  "Timer starting the queued catalog fetches.")

(opencode-defvar-service opencode--catalog-failed (make-hash-table :test #'equal)
  "Keys whose fetch failed since the stream opened, mapped to t.
Displays ask at every redisplay, so they do not fetch these again until
the stream reopens or an event says the catalog changed.")

(opencode-defvar-service opencode--catalog-refetch (make-hash-table :test #'equal)
  "Keys an event said changed while their fetch ran, mapped to t.
That fetch may answer from before the change, so another follows it.")

(opencode-defvar-service opencode--missing-locations (make-hash-table :test #'equal)
  "Normalized directories the service said do not exist.
Each maps to t, or to `recheck' once the stream reopened, when displays
ask for its catalogs once more.  Their catalogs are not asked for again
until then, or until an event of theirs says a catalog changed; displays
show identifiers meanwhile.")

(opencode-defvar-service opencode--missing-reported (make-hash-table :test #'equal)
  "Normalized directories whose absence was logged, mapped to t, so it is once.")

(defun opencode-location-missing-p (&optional directory)
  "Return non-nil when the service said DIRECTORY does not exist.
DIRECTORY defaults to the current location (see `opencode-location').
Displays reading its catalogs show identifiers instead; see
`opencode-catalog-ensure'."
  (gethash (opencode-normalize-directory (or directory (opencode-location) "/"))
           (opencode-sv opencode--missing-locations)))

(defun opencode-location-not-found-p (error)
  "Return non-nil when request ERROR says its location does not exist.
The service answers so a request scoped to a directory that is gone,
such as a session's worktree deleted since."
  (and (opencode-error-p error)
       (or (equal (opencode-error-tag error) "LocationNotFoundError")
           (and (eql (opencode-error-status error) 404)
                (string-prefix-p "Location not found" (or (opencode-error-message error) ""))))))

(opencode-defvar-service opencode--catalog-epoch 0
  "Count of stream openings.
A catalog fetched by a request begun before the latest one stays stale.")

(opencode-defvar-service opencode--catalog-generations (make-hash-table :test #'equal)
  "Normalized directories mapped to the count of their catalogs' changes.")

(defvar opencode-catalog-generation 0
  "Count of catalog changes, for caches of values derived from catalogs.
`opencode-catalog-location-generation' counts those of one location.")

(defcustom opencode-catalog-changed-functions nil
  "Abnormal hook run when a location's catalog changed.
Each function receives the catalog's kind, `agent', `model', `provider'
or `command', and the location's normalized directory.  It runs when a
catalog arrives for the first time, and when one fetched again, after
an event said it changed or the event stream reopened, arrives
different; a catalog fetched again unchanged runs nothing.  What was
computed from the catalog, such as `opencode-model-display', may differ
afterwards."
  :type 'hook
  :group 'opencode)

(defconst opencode--catalog-paths
  '((agent . "/api/agent") (model . "/api/model") (provider . "/api/provider")
    (command . "/api/command"))
  "Catalog kinds and their routes.")

(defun opencode--catalog-key (kind directory)
  "Return the key of the KIND catalog of DIRECTORY, by default the current location.
See `opencode-location'; a remote service's catalogs without a location
are those of its root."
  (cons (opencode-normalize-directory (or directory (opencode-location) "/")) kind))

(defun opencode-catalog (kind &optional directory)
  "Return the cached KIND catalog of DIRECTORY, or nil when not fetched.
A catalog the stream's reopening left stale is returned too, being most
likely still current; see `opencode-catalog-ensure'."
  (gethash (opencode--catalog-key kind directory) (opencode-sv opencode--catalogs)))

(defun opencode-catalog-cached-p (kind &optional directory)
  "Return non-nil when the KIND catalog of DIRECTORY is cached, even empty or stale."
  (not (eq (gethash (opencode--catalog-key kind directory) (opencode-sv opencode--catalogs) 'missing)
           'missing)))

(defun opencode-catalog-location-generation (&optional directory)
  "Return the count of changes of the catalogs of DIRECTORY.
It grows whenever `opencode-catalog-changed-functions' run for
DIRECTORY, so that values derived from one location's catalogs need not
be computed again when another location's change."
  (gethash (opencode-normalize-directory (or directory (opencode-location) "/"))
           (opencode-sv opencode--catalog-generations) 0))

(defun opencode--catalog-missing-p (key)
  "Return non-nil when catalog KEY is not cached."
  (eq (gethash key (opencode-sv opencode--catalogs) 'missing) 'missing))

(defun opencode--catalog-wanted-p (key)
  "Return non-nil when catalog KEY should be fetched for displays.
It should when it is missing or stale, and neither fetched, queued, nor
failed on this stream, and its location exists."
  (and (or (opencode--catalog-missing-p key) (gethash key (opencode-sv opencode--catalog-stale)))
       (not (gethash key (opencode-sv opencode--catalog-failed)))
       (not (gethash key (opencode-sv opencode--catalog-waiters)))
       (not (gethash key (opencode-sv opencode--catalog-scheduled)))
       (not (eq (gethash (car key) (opencode-sv opencode--missing-locations)) t))))

(opencode-defvar-service opencode--mode-line-directories nil
  "Directories of the session buffers whose mode lines wait to be updated, or t for all.")

(opencode-defvar-service opencode--mode-line-timer nil
  "Timer updating the mode lines `opencode--mode-line-directories' names.")

(defun opencode--update-mode-lines (&optional directory)
  "Update the mode lines of DIRECTORY's session buffers, else of every one.
Only those shown are redrawn, once however many calls a tick makes:
forcing every mode line of every frame instead costs tens of
milliseconds with many frames, and so did each catalog arriving."
  (cond ((null directory) (setf (opencode-sv opencode--mode-line-directories) t))
        ((listp (opencode-sv opencode--mode-line-directories))
         (cl-pushnew (opencode-normalize-directory directory) (opencode-sv opencode--mode-line-directories)
                     :test #'equal)))
  (unless (timerp (opencode-sv opencode--mode-line-timer))
    (setf (opencode-sv opencode--mode-line-timer)
          (apply #'run-at-time 0 nil #'opencode--update-mode-lines-now (opencode--timer-arguments)))))

(defun opencode--update-mode-lines-now (&optional service)
  "Update the mode lines `opencode--update-mode-lines' was asked for.
They are those of SERVICE, nil for the local one."
  (opencode-with-service service
    (opencode--update-mode-lines-1)))

(defun opencode--update-mode-lines-1 ()
  "Update the current service's mode lines, as `opencode--update-mode-lines-now'."
  (let ((directories (opencode-sv opencode--mode-line-directories)))
    (setf (opencode-sv opencode--mode-line-timer) nil
          (opencode-sv opencode--mode-line-directories) nil)
    (maphash (lambda (_id buffer)
               (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
                 (with-current-buffer buffer
                   (when (or (eq directories t)
                             (member (opencode-normalize-directory (opencode-location)) directories))
                     (force-mode-line-update)))))
             (opencode-sv opencode-session-buffers))))

(defun opencode--catalog-store (key data epoch)
  "Cache DATA as catalog KEY, fetched since stream opening EPOCH.
Return non-nil when it changed, after updating the mode lines that may
show it.  DATA stays stale when the stream reopened since the fetch
began, as the service may have changed meanwhile."
  (let ((changed (not (equal (gethash key (opencode-sv opencode--catalogs) 'missing) data))))
    (puthash key data (opencode-sv opencode--catalogs))
    (remhash key (opencode-sv opencode--catalog-failed))
    (if (= epoch (opencode-sv opencode--catalog-epoch))
        (remhash key (opencode-sv opencode--catalog-stale))
      (puthash key t (opencode-sv opencode--catalog-stale)))
    (when changed
      (cl-incf opencode-catalog-generation)
      (cl-incf (gethash (car key) (opencode-sv opencode--catalog-generations) 0))
      (opencode--update-mode-lines (car key)))
    changed))

(defun opencode--location-missing (directory error)
  "Remember that DIRECTORY, normalized, does not exist, as ERROR said.
Return non-nil when it was not known.  Its absence is logged once,
without showing in the echo area, and the displays of its catalogs,
which show identifiers now, are redrawn."
  (let ((newly (null (gethash directory (opencode-sv opencode--missing-locations)))))
    (puthash directory t (opencode-sv opencode--missing-locations))
    (when newly
      (unless (gethash directory (opencode-sv opencode--missing-reported))
        (puthash directory t (opencode-sv opencode--missing-reported))
        (let ((inhibit-message t))
          (message "OpenCode: %s does not exist; its sessions show identifiers (%s)"
                   directory (opencode-error-describe error))))
      (cl-incf opencode-catalog-generation)
      (cl-incf (gethash directory (opencode-sv opencode--catalog-generations) 0))
      (opencode--update-mode-lines directory))
    newly))

(defun opencode--location-back (directory)
  "Forget that DIRECTORY, normalized, does not exist, as it answered.
The displays of its catalogs, which showed identifiers, are redrawn."
  (when (gethash directory (opencode-sv opencode--missing-locations))
    (remhash directory (opencode-sv opencode--missing-locations))
    (cl-incf opencode-catalog-generation)
    (cl-incf (gethash directory (opencode-sv opencode--catalog-generations) 0))
    (opencode--update-mode-lines directory)))

(defun opencode--catalog-start (key)
  "Fetch catalog KEY, then settle the callbacks waiting for it.
A location's catalogs fill in shortly after its first request, so an
empty answer is asked again before being cached.  A location the
service says does not exist is remembered (see
`opencode-location-missing-p'): displays waiting are not told, and no
display asks for its catalogs again until the stream reopens."
  (let ((epoch (opencode-sv opencode--catalog-epoch))
        (directory (car key))
        (kind (cdr key))
        (service (opencode--remote-service)))
    (cl-labels ((settle (function)
                  (let ((waiters (nreverse (gethash key (opencode-sv opencode--catalog-waiters)))))
                    (remhash key (opencode-sv opencode--catalog-waiters))
                    ;; Its slot is free for the fetches queued.
                    (opencode--catalog-schedule)
                    (funcall function waiters)))
                (fetch (retries)
                  (opencode-request
                   'get (alist-get kind opencode--catalog-paths)
                   :directory directory :buffer nil
                   :then (lambda (response)
                           (let ((data (opencode-request-data response)))
                             (if (and (null data) (> retries 0))
                                 (run-at-time 0.5 nil (lambda ()
                                                        (opencode-with-service service
                                                          (fetch (1- retries)))))
                               (opencode--location-back directory)
                               (let ((changed (opencode--catalog-store key data epoch)))
                                 (when (gethash key (opencode-sv opencode--catalog-refetch))
                                   (remhash key (opencode-sv opencode--catalog-refetch))
                                   (puthash key t (opencode-sv opencode--catalog-stale))
                                   (opencode--catalog-enqueue key))
                                 (settle (lambda (waiters)
                                           (dolist (waiter waiters)
                                             (funcall (car waiter) data))))
                                 (when changed
                                   (opencode-state--run-hook 'opencode-catalog-changed-functions
                                                             kind directory))))))
                   :else (lambda (error)
                           (let* ((gone (opencode-location-not-found-p error))
                                  (newly (and gone (opencode--location-missing directory error))))
                             (remhash key (opencode-sv opencode--catalog-refetch))
                             (puthash key t (opencode-sv opencode--catalog-failed))
                             (settle (lambda (waiters)
                                       ;; Displays (`ignore') show identifiers for a location gone.
                                       (when (seq-some (lambda (waiter)
                                                         (and (null (cdr waiter))
                                                              (not (and gone (eq (car waiter) #'ignore)))))
                                                       waiters)
                                         (opencode-report-error (opencode-error-describe error)))
                                       (dolist (waiter waiters)
                                         (when (cdr waiter) (funcall (cdr waiter) error)))))
                             (when newly
                               (opencode-state--run-hook 'opencode-catalog-changed-functions
                                                         kind directory)))))))
      (fetch 2))))

(defun opencode--catalog-join (key callback else)
  "Have the fetch of catalog KEY call CALLBACK, or ELSE when it fails.
The fetch starts unless one runs."
  (let ((running (gethash key (opencode-sv opencode--catalog-waiters))))
    (push (cons callback else) (gethash key (opencode-sv opencode--catalog-waiters)))
    (unless running
      (opencode--catalog-start key))))

(defun opencode-catalog-fetch (kind callback &optional directory else)
  "Call CALLBACK with the KIND catalog of DIRECTORY, fetching it when needed.
ELSE, when given, receives the `opencode-error' of a failed fetch, which
is otherwise reported.  A stale catalog is fetched again first, so that
commands act on the service's current one."
  (let* ((key (opencode--catalog-key kind directory))
         (cached (gethash key (opencode-sv opencode--catalogs) 'missing)))
    (if (or (eq cached 'missing) (gethash key (opencode-sv opencode--catalog-stale)))
        (opencode--catalog-join key callback else)
      (funcall callback cached))))

(defun opencode-catalog-wait (kind &optional directory)
  "Return the KIND catalog of DIRECTORY, waiting for it when not cached.
For commands that prompt with it; see `opencode-wait'."
  (opencode-wait (lambda (then else) (opencode-catalog-fetch kind then directory else))))

(defun opencode--catalog-enqueue (key)
  "Queue the fetch of catalog KEY for displays."
  (unless (gethash key (opencode-sv opencode--catalog-scheduled))
    (puthash key t (opencode-sv opencode--catalog-scheduled))
    (setf (opencode-sv opencode--catalog-queue) (nconc (opencode-sv opencode--catalog-queue) (list key))))
  (opencode--catalog-schedule))

(defun opencode--catalog-schedule ()
  "Start the queued catalog fetches from a timer.
Displays ask at redisplay, and a timer also lets one redisplay's asks
queue up before any starts."
  (when (and (opencode-sv opencode--catalog-queue) (not (timerp (opencode-sv opencode--catalog-timer))))
    (setf (opencode-sv opencode--catalog-timer)
          (apply #'run-at-time 0 nil #'opencode--catalog-pump (opencode--timer-arguments)))))

(defun opencode--catalog-pump (&optional service)
  "Start queued catalog fetches while fewer than `opencode-catalog-requests' run.
Those of catalogs not cached go first: displays show identifiers until
they arrive, and a stale catalog meanwhile.  They are SERVICE's, nil
for the local one."
  (opencode-with-service service
    (opencode--catalog-pump-1)))

(defun opencode--catalog-pump-1 ()
  "Start the current service's queued catalog fetches, as `opencode--catalog-pump'."
  (setf (opencode-sv opencode--catalog-timer) nil)
  (setf (opencode-sv opencode--catalog-queue)
        (append (seq-filter #'opencode--catalog-missing-p (opencode-sv opencode--catalog-queue))
                (seq-remove #'opencode--catalog-missing-p (opencode-sv opencode--catalog-queue))))
  (while (and (opencode-sv opencode--catalog-queue)
              (< (hash-table-count (opencode-sv opencode--catalog-waiters)) (max 1 opencode-catalog-requests)))
    (let* ((key (pop (opencode-sv opencode--catalog-queue)))
           (missing (opencode--catalog-missing-p key)))
      (remhash key (opencode-sv opencode--catalog-scheduled))
      (when (opencode--catalog-wanted-p key)
        ;; Displays have the stale catalog meanwhile, so failing to
        ;; renew it is not worth a report.
        (opencode--catalog-join key #'ignore (unless missing #'ignore))))))

(defun opencode-catalog-ensure (kind &optional directory)
  "Return the cached KIND catalog of DIRECTORY, else fetch it in the background.
Return nil while it is not cached.  A stale catalog is returned, and
fetched again in the background.  Callers run at redisplay, so fetches
start from a timer, at most `opencode-catalog-requests' at a time;
once a catalog arrives changed, the mode lines of its location's
session buffers are updated and `opencode-catalog-changed-functions'
run.  A failed fetch is not tried again before the stream reopens or
an event says the catalog changed."
  (let* ((key (opencode--catalog-key kind directory))
         (cached (gethash key (opencode-sv opencode--catalogs) 'missing)))
    (when (opencode--catalog-wanted-p key)
      (opencode--catalog-enqueue key))
    (unless (eq cached 'missing) cached)))

(defun opencode--invalidate-catalog (event)
  "Fetch again the catalogs EVENT says changed in its location.
Displays keep the cached ones until the new ones arrive, and a fetch
running may answer from before the change, so another follows it."
  (when-let* ((directory (opencode-event-directory event))
              (kinds (pcase (alist-get 'type event)
                       ("agent.updated" '(agent))
                       ((or "model.updated" "provider.updated" "integration.updated") '(model provider))
                       ("command.updated" '(command)))))
    (dolist (kind kinds)
      (let ((key (cons directory kind)))
        (remhash key (opencode-sv opencode--catalog-failed))
        (opencode--location-back directory)
        (when (gethash key (opencode-sv opencode--catalog-waiters))
          (puthash key t (opencode-sv opencode--catalog-refetch)))
        (unless (opencode--catalog-missing-p key)
          (puthash key t (opencode-sv opencode--catalog-stale))
          (unless (gethash key (opencode-sv opencode--catalog-waiters))
            (opencode--catalog-enqueue key)))))))

(defun opencode--catalogs-connected (_event)
  "Mark every cached catalog stale when a stream opens: the service may have changed.
Displays keep showing them, and the first asking for one again fetches
it, so that a reconnection costs only the requests of the catalogs in
use, and redraws only for those that changed.  A location said not to
exist is asked once again, its absence no longer logged."
  (cl-incf (opencode-sv opencode--catalog-epoch))
  (clrhash (opencode-sv opencode--catalog-failed))
  (maphash (lambda (directory _) (puthash directory 'recheck (opencode-sv opencode--missing-locations)))
           (opencode-sv opencode--missing-locations))
  (maphash (lambda (key _) (puthash key t (opencode-sv opencode--catalog-stale))) (opencode-sv opencode--catalogs)))

(dolist (type '("agent.updated" "model.updated" "provider.updated" "integration.updated"
                "command.updated"))
  (opencode-events-register type #'opencode--invalidate-catalog))
(opencode-events-register "server.connected" #'opencode--catalogs-connected)

(defun opencode-agent-info (agent &optional directory)
  "Return the catalog entry of AGENT in DIRECTORY, if fetched."
  (seq-find (lambda (entry) (equal (alist-get 'id entry) agent))
            (opencode-catalog 'agent directory)))

(defun opencode-model-info (model &optional directory)
  "Return the catalog entry of MODEL, a model reference, in DIRECTORY."
  (let-alist model
    (seq-find (lambda (entry)
                (and (equal (alist-get 'id entry) .id)
                     (equal (alist-get 'providerID entry) .providerID)))
              (opencode-catalog 'model directory))))

(defun opencode-model-unavailable-p (model &optional directory)
  "Return non-nil when the model catalog of DIRECTORY proves MODEL unavailable.
MODEL is a model reference.  The catalog proves it once fetched, not
empty, and without MODEL, as when its provider is down or no longer
configured; a catalog not fetched or empty proves nothing."
  (and (opencode-catalog 'model directory)
       (not (opencode-model-info model directory))))

(defun opencode-provider-info (provider &optional directory)
  "Return the catalog entry of PROVIDER, a provider ID, in DIRECTORY, if fetched."
  (seq-find (lambda (entry) (equal (alist-get 'id entry) provider))
            (opencode-catalog 'provider directory)))

(defun opencode-agent-selectable-p (agent)
  "Return non-nil when catalog entry AGENT can run a session.
The service lets a session run any agent that is neither hidden nor
only a subagent, so agents of mode `all' qualify."
  (and (not (equal (alist-get 'mode agent) "subagent"))
       (not (alist-get 'hidden agent))))

(defun opencode-primary-agents (&optional directory)
  "Return the agents of DIRECTORY's cached catalog a session can run."
  (seq-filter #'opencode-agent-selectable-p (opencode-catalog 'agent directory)))

(defun opencode-default-agent (&optional directory)
  "Return the agent DIRECTORY's sessions run when none is chosen, or nil.
The service lists that agent first, having resolved the configured
`default_agent' as it does for a session without one, so this is the
first selectable entry of the cached catalog."
  (alist-get 'id (car (opencode-primary-agents directory))))

;;;; Mode

(defvar-keymap opencode-session-mode-map
  :parent comint-mode-map
  "RET" #'opencode-send-input
  "M-p" #'opencode-previous-input-or-button
  "M-n" #'opencode-next-input-or-button
  "TAB" #'opencode-tab-dwim
  "<backtab>" #'backward-button
  ;; Any key sending input goes through the checks, the archive and the
  ;; attachments, as RET does.
  "<remap> <comint-send-input>" #'opencode-send-input
  ;; Earlier prompts take no clicks: Comint's would put one at the prompt.
  "<remap> <comint-insert-input>" #'opencode-run-global-click
  "<remap> <comint-copy-old-input>" #'opencode-copy-old-input
  ;; C-c C-o opens links, as in Org, instead of deleting the last output.
  "C-c C-o" #'opencode-open-link-at-point
  "/" #'opencode-electric-slash
  "@" #'opencode-add-subagent
  ;; The pipe runs no subprocess: signals, end of file and erasing output
  ;; either have an OpenCode meaning or are refused.
  "<remap> <comint-interrupt-subjob>" #'opencode-interrupt
  "<remap> <comint-delchar-or-maybe-eof>" #'delete-char
  "<remap> <comint-send-eof>" #'opencode-comint-unsupported
  "<remap> <comint-stop-subjob>" #'opencode-comint-unsupported
  "<remap> <comint-continue-subjob>" #'opencode-comint-unsupported
  "<remap> <comint-quit-subjob>" #'opencode-comint-unsupported
  "<remap> <comint-kill-subjob>" #'opencode-comint-unsupported
  "<remap> <comint-delete-output>" #'opencode-comint-unsupported
  "<remap> <comint-clear-buffer>" #'opencode-comint-unsupported)

(defconst opencode--comint-refusals
  '((comint-send-eof
     "An OpenCode session takes no end of file: kill the buffer to close it")
    (comint-stop-subjob
     "An OpenCode turn cannot be suspended: %s stops it" opencode-interrupt)
    (comint-continue-subjob
     "An OpenCode turn is never suspended, so there is nothing to continue")
    (comint-quit-subjob
     "An OpenCode session takes no quit signal: %s stops the turn" opencode-interrupt)
    (comint-kill-subjob
     "An OpenCode session has no process to kill: %s stops the turn" opencode-interrupt)
    (comint-delete-output
     "The transcript is the server's and cannot be deleted: %s renders it again" revert-buffer)
    (comint-clear-buffer
     "The transcript is the server's and cannot be cleared: %s renders it again" revert-buffer))
  "Comint commands that session buffers refuse, as (COMMAND REASON HINTED).
REASON is the message given, its %s the keys of command HINTED, if any.")

(defun opencode--comint-refusal (command)
  "Return why session buffers refuse Comint COMMAND, naming the keys that help."
  (pcase (alist-get command opencode--comint-refusals)
    (`(,reason ,hinted) (format reason (opencode-command-keys hinted)))
    (`(,reason) reason)
    (_ (format "`%s' does not apply to OpenCode sessions" command))))

(defun opencode-comint-unsupported ()
  "Refuse the Comint command remapped to this one, saying why.
Session buffers run no subprocess: Comint's signals and end of file
would act on the input instead, sending it or marking it as sent without
sending it, and erasing output would desynchronize the transcript from
the server."
  (interactive)
  (user-error "%s" (opencode--comint-refusal this-original-command)))

(defun opencode--session-cleanup ()
  "Release this buffer's process, registration and temporary files.
For `kill-buffer-hook': whatever kills it, its own service's registration."
  (opencode-with-service opencode--buffer-service
    (opencode--session-cleanup-1)))

(defun opencode--session-cleanup-1 ()
  "Release this buffer's process, registration and files, as `opencode--session-cleanup'."
  (opencode-dismiss-notifications)
  (when (and opencode-session-id
             (eq (gethash opencode-session-id (opencode-sv opencode-session-buffers)) (current-buffer)))
    (remhash opencode-session-id (opencode-sv opencode-session-buffers))
    (run-hook-with-args 'opencode-session-buffer-functions opencode-session-id))
  (opencode--resync-forget (current-buffer))
  (when-let* ((process (get-buffer-process (current-buffer))))
    (delete-process process))
  (mapc (lambda (file) (ignore-errors (delete-file file))) opencode--temp-files)
  (setq opencode--temp-files nil))

(defun opencode--comint-completion-at-point ()
  "Complete history and file names as Comint does, yielding other words.
Comint reads any word at point as a file name: a word that names no
file would fail to complete without trying the functions after this
one, such as the global ones, so its file names complete
non-exclusively."
  (let ((data (comint-completion-at-point)))
    (if (and (consp data) (not (functionp data))
             (not (plist-member (nthcdr 3 data) :exclusive)))
        (append data '(:exclusive no))
      data)))

(define-derived-mode opencode-session-mode comint-mode "OpenCode"
  "Major mode of an OpenCode session buffer.
\\<opencode-session-mode-map>
The transcript is the server's: it is read-only, typing or deleting
there does nothing, and killing copies.  Point moves anywhere, and
\\[opencode-send-input] in the transcript quotes the region, else the line at point,
through `opencode-old-input-function', but on a link or other text with
keys of its own; in the input at the bottom, it sends.  In the
transcript, TAB expands or collapses the block or group at point, as a
click on its summary's box does (see `opencode-tab-dwim').

Only standard keys are bound.  Comint commands meant for a subprocess
interrupt the turn or are refused (`opencode-comint-unsupported'), and
`revert-buffer' renders the transcript again.
\\{opencode-session-mode-map}"
  (setq-local comint-use-prompt-regexp nil
              comint-input-sender #'opencode--send-input
              comint-highlight-input nil
              revert-buffer-function #'opencode--session-revert
              left-margin-width (1+ left-margin-width))
  ;; Output never prompts for a password.
  (setq-local comint-output-filter-functions
              (remove #'comint-watch-for-password-prompt comint-output-filter-functions))
  (remove-hook 'completion-at-point-functions #'comint-completion-at-point t)
  (add-hook 'completion-at-point-functions #'opencode--comint-completion-at-point nil t)
  ;; Prompts are prose: complete words as text modes do.
  (when (eq text-mode-ispell-word-completion 'completion-at-point)
    (add-hook 'completion-at-point-functions #'ispell-completion-at-point 10 t))
  (opencode-render-setup)
  (opencode-blocks-setup)
  (setq opencode--own-inputs (make-hash-table :test #'equal)
        opencode--text-blocks (make-hash-table :test #'equal))
  (add-hook 'kill-buffer-hook #'opencode--session-cleanup nil t)
  (add-hook 'comint-input-filter-functions #'opencode--note-input-sent nil t)
  (opencode-inbox-setup)
  (add-hook 'fill-nobreak-predicate #'opencode--in-attachment-p nil t)
  (visual-line-mode)
  (cursor-intangible-mode)
  (yank-media-handler "\\`image/" #'opencode--yank-image))

(defvar-local opencode--block-kind-before-input nil
  "The kind of the last block before Comint took the input last sent.
A prompt deferred until its delivery leaves the transcript as it was.")

(defun opencode--note-input-sent (&rest _)
  "Note that the input Comint takes is the last block written."
  (setq opencode--block-kind-before-input opencode--last-block-kind
        opencode--last-block-kind 'user))

(defun opencode--make-process (buffer)
  "Create BUFFER's pipe process.
Input goes over HTTP, so neither a PTY nor notifications of the window
size are needed; on Windows a PTY would run a synchronous helper on every
resize."
  (let ((process (make-pipe-process :name "opencode-session" :buffer buffer :noquery t)))
    (process-put process 'adjust-window-size-function #'ignore)
    process))

;;;; Opening sessions

(defun opencode-session-buffer-name (info)
  "Return the buffer name of session INFO, of the current service.
A remote service's is named after it, as \"*OpenCode@core: Title*\"."
  (format "*OpenCode%s: %s*"
          (if-let* ((label (opencode-service-label))) (concat "@" label) "")
          (or (alist-get 'title info) (alist-get 'id info))))

(defun opencode--session-directory (info)
  "Return the directory of session INFO."
  (opencode-normalize-directory (alist-get 'directory (alist-get 'location info))))

(defun opencode--adopt-location (info)
  "Adopt the confirmed location in session INFO or a move event's data.
A remote service's session keeps it in `opencode-session-location': the
buffer stays in this host's home, the directory being the other host's."
  (if (opencode--remote-service)
      (setq opencode-session-location (opencode--session-directory info)
            default-directory (expand-file-name "~/")
            list-buffers-directory opencode-session-location)
    (setq default-directory (opencode--session-directory info)
          list-buffers-directory default-directory))
  (dolist (field '(location projectID subpath))
    (setf (alist-get field opencode-session-info nil t) (alist-get field info))))

(defun opencode--cd (original directory)
  "Ask the server to move this session when ORIGINAL `cd' selects DIRECTORY.
A session of a remote service is not moved: DIRECTORY is this host's."
  (when (and (derived-mode-p 'opencode-session-mode) opencode-session-id opencode--buffer-service)
    (user-error "This session runs on %s: its directory is that host's" (opencode-service-label)))
  (if (not (and (derived-mode-p 'opencode-session-mode) opencode-session-id))
      (funcall original directory)
    (let ((destination
           (let ((default-directory default-directory)
                 (list-buffers-directory list-buffers-directory))
             (funcall original directory)
             (opencode-directory-name default-directory))))
      (opencode-request
       'post (format "/api/session/%s/move" opencode-session-id)
       :body `((directory . ,destination))
       :else (lambda (error) (opencode-report-error (opencode-error-describe error)))))))

(advice-add 'cd :around #'opencode--cd)

(defun opencode--display (buffer no-display other-window)
  "Show BUFFER unless NO-DISPLAY: in another window when OTHER-WINDOW."
  (unless no-display
    (if other-window (pop-to-buffer buffer) (switch-to-buffer buffer)))
  buffer)

(defun opencode--adopt-info (info &optional location-generation)
  "Record session INFO in this buffer.
LOCATION-GENERATION is the live move count when INFO was requested;
keep the newer location if a move overtook that request."
  (when (and location-generation (/= location-generation opencode--location-generation))
    (setq info (copy-alist info))
    (dolist (field '(location projectID subpath))
      (setf (alist-get field info nil t) (alist-get field opencode-session-info))))
  (setq opencode-session-info info
        opencode-session-agent (or (alist-get 'agent info) opencode-session-agent)
        opencode-session-model (or (alist-get 'model info) opencode-session-model)
        opencode-session-revert (alist-get 'revert info))
  (opencode--adopt-location info)
  (force-mode-line-update))

(cl-defun opencode-session-open (info &key buffer no-display (other-window t) callback else)
  "Show the buffer of session INFO, creating and filling it when needed.
INFO is a session info alist.  BUFFER reuses an existing placeholder
buffer.  NO-DISPLAY leaves every window alone.  OTHER-WINDOW pops the
buffer instead of switching to it.  CALLBACK runs in the buffer with
INFO once the transcript is loaded; ELSE runs there instead with the
`opencode-error' of a failed request.  The event stream is opened if it
is not, since a session buffer follows its session through it.  Return
the buffer."
  (opencode-events-connect)
  (let* ((id (alist-get 'id info))
         (service (opencode--remote-service))
         (existing (gethash id (opencode-sv opencode-session-buffers))))
    (if (buffer-live-p existing)
        (progn
          (when callback (with-current-buffer existing (funcall callback info)))
          (opencode--display existing no-display other-window))
      (let ((buffer (or buffer (generate-new-buffer (opencode-session-buffer-name info)))))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer))
          (setq buffer-read-only nil)
          ;; Before the mode, whose hooks run for its service.
          (setq opencode--buffer-service service)
          (when service
            (setq default-directory (expand-file-name "~/")))
          (opencode-session-mode)
          ;; A window takes the insertion type of its point, which keeps
          ;; it after output, and its margins when shown the buffer: one
          ;; showing a placeholder, as a restored desktop does, took them
          ;; from another mode.
          (dolist (window (get-buffer-window-list buffer nil t))
            (set-window-buffer window buffer))
          (setq opencode-session-id id)
          (opencode--adopt-info info)
          ;; A remote session's directory is not here.
          (unless service
            (hack-dir-local-variables-non-file-buffer))
          (puthash id buffer (opencode-sv opencode-session-buffers))
          (opencode--make-process buffer)
          (run-hook-with-args 'opencode-session-buffer-functions id)
          (opencode-session-load
           (lambda ()
             (when callback (funcall callback info)))
           else))
        (opencode--display buffer no-display other-window)))))

(defun opencode-session-fetch (id callback &optional directory else)
  "Call CALLBACK with the info of session ID; DIRECTORY scopes the request.
ELSE, when given, receives the `opencode-error' of a failed request."
  (opencode-request 'get (concat "/api/session/" id)
                    :directory directory :buffer nil :else else
                    :then (lambda (response) (funcall callback (opencode-request-data response)))))

(cl-defun opencode-session-open-by-id (id &key no-display (other-window t) callback else)
  "Open session ID like `opencode-session-open' with NO-DISPLAY, OTHER-WINDOW, CALLBACK.
ELSE receives the `opencode-error' of a failed request."
  (if-let* ((buffer (gethash id (opencode-sv opencode-session-buffers)))
            ((buffer-live-p buffer)))
      (progn
        (when callback (with-current-buffer buffer (funcall callback opencode-session-info)))
        (opencode--display buffer no-display other-window))
    (opencode-session-fetch
     id (lambda (info)
          (opencode-session-open info :no-display no-display :other-window other-window
                                 :callback callback :else else))
     nil else)))

(defun opencode-new-session-model (agent &optional directory)
  "Return the model reference a new session of AGENT in DIRECTORY starts on.
AGENT nil stands for DIRECTORY's default agent.  The model the agent
pins comes first, as switching to that agent would select it, then
`opencode-last-model', the model chosen last, unless the model catalog
proves it unavailable (`opencode-model-unavailable-p'), as while its
provider is down: it is kept, to apply again once the provider is back.
Nil leaves the choice to the service.  Agents and models are read from
the cached catalogs."
  (or (alist-get 'model (opencode-agent-info (or agent (opencode-default-agent directory))
                                             directory))
      (and opencode-last-model
           (not (opencode-model-unavailable-p opencode-last-model directory))
           opencode-last-model)))

(cl-defun opencode-session-create (&key (directory default-directory) title agent model
                                        callback else no-display)
  "Create a session in DIRECTORY and open it.
TITLE, AGENT and MODEL (a model reference) are optional; MODEL defaults
to `opencode-new-session-model'.  A MODEL given is chosen: once the
session exists, it becomes `opencode-last-model'.  CALLBACK runs in the
new buffer with its info; ELSE receives the `opencode-error' of a failed
request, which is otherwise reported.  NO-DISPLAY leaves every window
alone."
  (let ((directory (opencode-normalize-directory directory))
        (else (or else (lambda (error) (opencode-report-error (opencode-error-describe error))))))
    (cl-flet ((create ()
                (let ((runs (or model (opencode-new-session-model agent directory))))
                  (opencode-request
                   'post "/api/session" :directory directory :buffer nil
                   :body `((location . ((directory . ,(opencode-directory-name directory))))
                           ,@(when title `((title . ,title)))
                           ,@(when agent `((agent . ,agent)))
                           ,@(when runs `((model . ,runs))))
                   :then (lambda (response)
                           (when model
                             (setq opencode-last-model model))
                           (opencode-session-open (opencode-request-data response)
                                                  :no-display no-display
                                                  :callback callback :else else))
                   :else else))))
      (if model
          (create)
        ;; The pins live in the agent catalog, what is available in the
        ;; model catalog; without them, the last model still applies.
        (opencode-catalog-fetch
         'agent
         (lambda (_)
           (opencode-catalog-fetch 'model (lambda (_) (create)) directory
                                   (lambda (_) (create))))
         directory
         (lambda (_) (create)))))))

(defun opencode-new-session (&optional title)
  "Create a session in the current project, prompting for TITLE with a prefix."
  (interactive (list (when current-prefix-arg (read-string "Title: "))))
  (opencode-session-create :directory (opencode-project-directory) :title title))

;;;; What the transcript holds

;; The service names every message it appends after the event appending
;; it, or after the inbox item delivered, so the live events tell which
;; messages a buffer renders as well as the transcript does.  A message
;; is open while it may still change, as a step running, a shell
;; command or a compaction in progress; once none is open, everything
;; up to the newest message is rendered for good, and the end of the
;; transcript is where the buffer is in sync, `opencode--synced'.  A
;; reconnection then fetches only the messages after that one and
;; writes them again from there.
;;
;; Answers and events race: an event arriving while the transcript is
;; fetched or written may be in the answer already.  So the buffer
;; withholds its session's events meanwhile and applies them once the
;; answer is written, and a message or part already rendered is never
;; rendered again: events appending a known message, or updating a
;; finished one, change nothing.  Neither does the start of a finished
;; step: the service projects it as its message's creation time, and
;; only a step started again, as after a retry, starts later.

(defvar-local opencode--messages nil
  "Hash table of the IDs of the server messages rendered here.
Each maps to `open' while the message may still change, else `final'.")

(defvar-local opencode--open-messages nil
  "IDs of the rendered messages that are open, newest first.")

(defvar-local opencode--message-starts nil
  "Hash table of the IDs of the assistant messages rendered here to their steps' start.
That is the `started' time of the `session.step.started' event of the
step rendered, which the service projects as the message's creation
time; a step started again, as after a retry, starts later.")

(defvar-local opencode--open-shells nil
  "Alist of the shell IDs of running shell messages to their message IDs.")

(defvar-local opencode--open-compaction nil
  "ID of the compaction message in progress, or nil.")

(defvar-local opencode--awaited-parts nil
  "Hash table of the text and reasoning keys a fetched transcript had in progress.
Their earlier deltas were missed, so they are shown whole once they end.")

(defvar-local opencode--newest-message nil
  "ID of the newest server message rendered here, or nil.")

(defvar-local opencode--synced nil
  "Where this buffer is in sync with the service, or nil when unknown.
A plist: :id, the newest message such that it and every one before it
are rendered and finished; :kind, `opencode--last-block-kind' after it;
:errors, `opencode--turn-errors' then; :response, a marker at
`opencode-last-response-start' then.  `opencode-synced-end' marks
where what follows that message is rendered.")

(defvar-local opencode--holding nil
  "Non-nil while this buffer withholds its session's events.")

(defvar-local opencode--loading nil
  "Non-nil while this buffer's transcript is being fetched and written whole.")

(defvar-local opencode--pinned nil
  "Non-nil while this buffer's windows show its start, as its transcript is written whole.
Its windows and point go to its end once it is written.")

(defvar-local opencode--resync-wanted nil
  "Non-nil when this buffer must catch up once its transcript is loaded.")

(defvar-local opencode--held-events nil
  "Events withheld while the transcript is fetched, newest first.
Each is (EVENT . FUNCTION), FUNCTION applying EVENT in this buffer.")

(defvar opencode--releasing nil
  "Non-nil while withheld events are applied.")

(defun opencode--message-table ()
  "Return `opencode--messages', made on first use."
  (or opencode--messages
      (setq opencode--messages (make-hash-table :test #'equal))))

(defun opencode--message-known-p (id)
  "Return non-nil when message ID is rendered here."
  (and id (gethash id (opencode--message-table))))

(defun opencode--message-final-p (id)
  "Return non-nil when message ID is rendered here and finished."
  (and id (eq (gethash id (opencode--message-table)) 'final)))

(defun opencode--event-message-id (event)
  "Return the ID of the message EVENT appends, as the service names it, or nil."
  (when-let* ((id (alist-get 'id event)))
    (if (string-prefix-p "evt_" id) (concat "msg_" (substring id 4)) id)))

(defun opencode--settle ()
  "Note that the transcript is in sync up to its end, unless a message is open."
  (when (and opencode--newest-message (null opencode--open-messages)
             (get-buffer-process (current-buffer)))
    ;; Before the idle prompt, which is rewritten after any new message.
    (let ((end (if (opencode-prompt-shown-p) (marker-position opencode--prompt-start) (opencode-pmark)))
          (response (plist-get opencode--synced :response)))
      (if (markerp opencode-synced-end)
          (set-marker opencode-synced-end end)
        (setq opencode-synced-end (copy-marker end)))
      (when (markerp response) (set-marker response nil))
      (setq opencode--synced
            (list :id opencode--newest-message
                  :kind opencode--last-block-kind
                  :errors (copy-sequence opencode--turn-errors)
                  :response (and (markerp opencode-last-response-start)
                                 (marker-buffer opencode-last-response-start)
                                 (copy-marker opencode-last-response-start)))))))

(defun opencode--note-message-start (id started)
  "Note that assistant message ID is rendered from its step started at STARTED."
  (when (and id started)
    (puthash id started (or opencode--message-starts
                            (setq opencode--message-starts (make-hash-table :test #'equal))))))

(defun opencode--rendered-step-p (id started)
  "Return non-nil when message ID is rendered from its step started at STARTED."
  (and started (hash-table-p opencode--message-starts)
       (equal (gethash id opencode--message-starts) started)))

(defun opencode--note-message (id &optional open)
  "Note that message ID is rendered, OPEN while it may still change."
  (when id
    (puthash id (if open 'open 'final) (opencode--message-table))
    (setq opencode--newest-message id)
    (if open
        (cl-pushnew id opencode--open-messages :test #'equal)
      (setq opencode--open-messages (delete id opencode--open-messages)))
    (opencode--settle)))

(defun opencode--forget-parts (id)
  "Forget the text parts of message ID, which no event changes any longer."
  (when (hash-table-p opencode--text-blocks)
    (let ((prefix (concat id "/"))
          (keys nil))
      (maphash (lambda (key _) (when (string-prefix-p prefix key) (push key keys)))
               opencode--text-blocks)
      (dolist (key keys)
        (remhash key opencode--text-blocks)))))

(defun opencode--finish-message (id)
  "Note that open message ID is finished."
  (when (and id (eq (gethash id (opencode--message-table)) 'open))
    (puthash id 'final opencode--messages)
    (setq opencode--open-messages (delete id opencode--open-messages))
    (opencode--forget-parts id)
    (opencode--settle)))

(defun opencode--forget-messages ()
  "Forget which messages are rendered, as the transcript is erased."
  (when (hash-table-p opencode--messages) (clrhash opencode--messages))
  (when (hash-table-p opencode--message-starts) (clrhash opencode--message-starts))
  (when (hash-table-p opencode--awaited-parts) (clrhash opencode--awaited-parts))
  (when (markerp (plist-get opencode--synced :response))
    (set-marker (plist-get opencode--synced :response) nil))
  (setq opencode--open-messages nil
        opencode--open-shells nil
        opencode--open-compaction nil
        opencode--newest-message nil
        opencode--synced nil))

(defun opencode--part-awaited-p (key)
  "Return non-nil when part KEY is shown only once it ends."
  (and (hash-table-p opencode--awaited-parts) (gethash key opencode--awaited-parts)))

(defun opencode--await-part (key)
  "Show part KEY only once it ends: a fetched transcript had it in progress."
  (unless (hash-table-p opencode--awaited-parts)
    (setq opencode--awaited-parts (make-hash-table :test #'equal)))
  (puthash key t opencode--awaited-parts))

(defun opencode--projected-open-p (message)
  "Return non-nil when projected MESSAGE may still change."
  (pcase (alist-get 'type message)
    ("assistant" (not (alist-get 'completed (alist-get 'time message))))
    ((or "shell" "compaction") (equal (alist-get 'status message) "running"))))

;;;; Withholding events

(defun opencode--hold ()
  "Withhold this buffer's session events until `opencode--release'."
  (setq opencode--holding t))

(defun opencode--when-live (event function)
  "Call FUNCTION, applying EVENT in this buffer, now or once events are released."
  (if opencode--holding
      (push (cons event function) opencode--held-events)
    (funcall function)))

(defun opencode--release ()
  "Apply the events withheld, oldest first, and follow events again.
Those the transcript fetched meanwhile already holds change nothing.
An event that has the buffer withhold events again, as a reload does,
leaves the rest withheld."
  (let ((buffer (current-buffer))
        (events (nreverse opencode--held-events))
        (opencode--releasing t))
    (setq opencode--holding nil
          opencode--held-events nil)
    (while (and events (buffer-live-p buffer))
      (with-current-buffer buffer
        (if opencode--holding
            (setq opencode--held-events (append opencode--held-events (reverse events))
                  events nil)
          (let ((entry (pop events)))
            (condition-case err
                (funcall (cdr entry))
              (error
               (message "OpenCode: %s handler failed: %s"
                        (alist-get 'type (car entry)) (error-message-string err))))))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (opencode-inbox-update)))))

(defun opencode--held-status ()
  "Return the status before the first withheld turn event, or nil without one.
Applying the withheld events then leaves the status they lead to."
  (seq-some (lambda (entry)
              (let ((event (car entry)))
                (pcase (alist-get 'type event)
                  ("session.execution.started" 'idle)
                  ((or "session.execution.succeeded" "session.execution.failed") 'busy)
                  ("session.execution.interrupted"
                   (unless (equal (alist-get 'reason (alist-get 'data event)) "shutdown")
                     'busy)))))
            (reverse opencode--held-events)))

;;;; Loading the transcript

(defun opencode--fetch-messages (id callback &optional else cursor accumulated)
  "Call CALLBACK with every projected message of session ID, oldest first.
ELSE, when given, receives the `opencode-error' of a failed request.
CURSOR and ACCUMULATED page through the list; a cursor carries the
order of the first page, and the service refuses both together."
  (opencode-request
   'get (format "/api/session/%s/message" id)
   :query `((limit . 200) ,@(if cursor `((cursor . ,cursor)) '((order . "asc"))))
   :else else
   :then (lambda (response)
           (let ((data (opencode-request-data response))
                 (next (alist-get 'next (alist-get 'cursor response))))
             (if (and data next)
                 (opencode--fetch-messages id callback else next (nconc accumulated data))
               (funcall callback (nconc accumulated data)))))))

(defun opencode--show-status (active)
  "Adopt the status ACTIVE gives, non-nil while a turn runs, with its idle prompt."
  (setq opencode-session-status (if active 'busy 'idle))
  (if active
      (opencode-lift-prompt)
    (unless (opencode-prompt-shown-p)
      (opencode-call-preserving-viewport
       (current-buffer)
       (lambda ()
         (opencode-open-block 'user)
         (opencode-show-prompt)))))
  (force-mode-line-update))

(defun opencode--status-after-fetch (active)
  "Return non-nil when a turn runs, from ACTIVE, what a request asked meanwhile said.
The withheld events say better: the status before the first turn
event among them, so that applying them leaves the status they lead to."
  (pcase (opencode--held-status)
    ('busy t)
    ('idle nil)
    (_ active)))

(defun opencode--unpin ()
  "Show the end of the transcript written whole, where its windows follow output."
  (when opencode--pinned
    (setq opencode--pinned nil)
    (opencode--pin-viewport t)))

(defun opencode--loaded ()
  "Follow events again once the transcript is loaded, catching up when asked to."
  (opencode--unpin)
  (setq opencode--loading nil)
  (opencode--release)
  (when opencode--resync-wanted
    (setq opencode--resync-wanted nil)
    (opencode--resync-enqueue (current-buffer))))

(defun opencode--load-failure (else)
  "Return the handler of a failed load: follow events again, then call ELSE.
Without ELSE, the failure is reported."
  (lambda (error)
    (opencode--loaded)
    (if else
        (funcall else error)
      (opencode-report-error (opencode-error-describe error)))))

(defun opencode-session-load (&optional callback else)
  "Replay this session's transcript, then adopt its status and run CALLBACK.
ELSE, when given, receives the `opencode-error' of a failed request.
The session's events are withheld meanwhile, then applied."
  (let ((id opencode-session-id)
        (else (opencode--load-failure else)))
    (setq opencode--loading t
          opencode--pinned t)
    (opencode--hold)
    ;; The inbox first: a prompt delivered after it is in the messages.
    (opencode-inbox-fetch
     (lambda ()
       (opencode--fetch-messages
        id
        (lambda (messages)
          (opencode-replay-messages
           messages
           (lambda ()
             (opencode-request
              'get "/api/session/active"
              :else else
              :then (lambda (response)
                      (opencode--unpin)
                      (opencode--show-status
                       (opencode--status-after-fetch
                        (assq (intern id) (opencode-request-data response))))
                      (run-hooks 'opencode-session-replayed-hook)
                      (opencode--loaded)
                      (when callback (funcall callback)))))))
        else)))))

(defun opencode--session-revert (&rest _)
  "Reload the transcript, as `revert-buffer' does in a session buffer."
  (opencode-session-reload))

(defun opencode--erase-transcript ()
  "Erase the transcript and the input, and forget what they showed."
  (cl-incf opencode--render-generation)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (opencode-delete-overlays (point-min) (point-max))
    (erase-buffer))
  ;; Parsers an earlier version of this package left behind.
  (opencode-prune-parsers-later)
  ;; The prompt's records point into the text erased.
  (when (listp buffer-undo-list)
    (setq buffer-undo-list nil))
  (set-marker (process-mark (get-buffer-process (current-buffer))) (point-min))
  (opencode--sent-inputs-erased)
  (clrhash opencode--text-blocks)
  (when (hash-table-p opencode--blocks) (clrhash opencode--blocks))
  (when (hash-table-p opencode--reasoning-native) (clrhash opencode--reasoning-native))
  (setq opencode--last-block-kind nil
        opencode--current-assistant nil
        opencode--turn-errors nil)
  ;; Replaying the transcript finds the size again, if any remains.
  (setq opencode-session-tokens nil opencode-session-tokens-estimated nil)
  (opencode--forget-messages))

(defun opencode--render-again (&optional then)
  "Render this session's transcript again from the server, then call THEN.
The input and the viewport are kept as `opencode-session-reload' says;
the input keeps its attachments, and comes back too when the transcript
cannot be loaded, the failure being reported."
  (let* ((viewport (opencode-save-viewport))
         (input (opencode--take-input)))
    (opencode--erase-transcript)
    (opencode-session-load
     (lambda ()
       (let ((buffer-undo-list t))
         (opencode--put-back-input input))
       (opencode-restore-viewport viewport)
       (when then (funcall then)))
     (lambda (error)
       (let ((buffer-undo-list t))
         (opencode--put-back-input input))
       (opencode-report-error (opencode-error-describe error))))))

(defun opencode-session-reload ()
  "Render this session's transcript again from the server.
Input typed at the prompt is kept.  Points at the end of the
transcript or in the input stay there, others at their line, as
`opencode-save-viewport' keeps them.  `revert-buffer' does this too."
  (interactive)
  (unless opencode-session-id (user-error "Not in an OpenCode session"))
  (let ((location-generation opencode--location-generation))
    (opencode-session-fetch
     opencode-session-id
     (lambda (info)
       (opencode-with-session-buffer (alist-get 'id info)
         (opencode--adopt-info info location-generation)
         (opencode--render-again))))))

(defun opencode-session-prune-parsers (&optional all)
  "Delete the tree-sitter parsers of this session buffer that parse no text.
With ALL, interactively a prefix argument, or outside a session buffer,
prune every session buffer.  Return how many parsers were deleted.
Earlier versions of this package left such parsers behind, which every
change of the buffer kept editing; a session buffer prunes them by
itself when first fontified, and again whenever it catches up or
reloads."
  (interactive "P")
  (let ((count 0))
    (dolist (buffer (if (and opencode-session-id (not all))
                        (list (current-buffer))
                      (hash-table-values (opencode-sv opencode-session-buffers))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (cl-incf count (opencode-prune-parsers)))))
    (when (called-interactively-p 'interactive)
      (message "OpenCode: deleted %d parser%s" count (if (= count 1) "" "s")))
    count))

(defun opencode--render-user-prompt (text &optional message-id note)
  "Render a user prompt with TEXT for MESSAGE-ID, followed by NOTE when given.
Input still being typed is lifted out and put back afterwards."
  (opencode-without-undo
    (let* ((process (get-buffer-process (current-buffer)))
           (inhibit-read-only t)
           (pending (opencode--take-input)))
      (unless (opencode-prompt-shown-p)
        (opencode-open-block 'user)
        (opencode-show-prompt))
      (goto-char (point-max))
      (insert text)
      (let ((comint-input-sender #'opencode-highlight-input)
            (comint-input-filter-functions nil))
        (comint-send-input nil t))
      (when message-id
        (with-silent-modifications
          (put-text-property comint-last-input-start comint-last-input-end
                             'opencode-message message-id)))
      (opencode-group-earlier-turn comint-last-input-start)
      (when note
        (opencode-output (opencode-fontify-as (propertize (concat note "\n") 'font-lock-face 'opencode-notice)
                                              'verbatim)))
      (setq opencode--last-block-kind 'user)
      (set-marker (process-mark process) (opencode--put-back-input pending)))))

(defun opencode--user-message-text (message)
  "Return the prompt text of user MESSAGE with its attachments named.
The label of a file read from a file URI, and of a subagent mention,
carries its attachment as its `opencode-sent-attachment' property,
for the prompt to be edited again with it (see `opencode-old-input');
a file sent inline carries none, as its data would stay in the buffer."
  (let-alist message
    (string-join
     (delq nil (append (list .text)
                       (mapcar (lambda (file)
                                 (let ((label (format "`%s`" (or (alist-get 'name file) "file")))
                                       (uri (and (equal (map-nested-elt file '(source type)) "uri")
                                                 (map-nested-elt file '(source uri)))))
                                   (if (and (stringp uri) (string-prefix-p "file:" uri))
                                       (propertize label 'opencode-sent-attachment
                                                   (list :file `((uri . ,uri)
                                                                 (name . ,(or (alist-get 'name file) "file")))))
                                     label)))
                               .files)
                       (mapcar (lambda (agent)
                                 (let ((name (alist-get 'name agent)))
                                   (propertize (concat "@" name)
                                               'opencode-sent-attachment (list :agent name))))
                               .agents)))
     " ")))

(defun opencode--render-assistant-content (message &optional open)
  "Render the content of projected assistant MESSAGE.
OPEN means the step still runs: events may follow for its parts, so
those ended are noted as shown whole, and those still empty, whose
deltas were missed, are shown once they end."
  (let ((id (alist-get 'id message))
        (text-ordinal 0)
        (reasoning-ordinal 0))
    (dolist (item (alist-get 'content message))
      (pcase (alist-get 'type item)
        ("text"
         (let ((text (string-trim (or (alist-get 'text item) "")))
               (key (format "%s/t/%d" id text-ordinal)))
           (if (string-empty-p text)
               (when open (opencode--await-part key))
             (opencode-open-block 'text)
             (opencode-set-last-response-start (opencode-pmark))
             (setq opencode--table-scan-marker (copy-marker (opencode-pmark)))
             (opencode--output-message text id)
             (opencode-buttonize-output-end)
             (opencode-align-finished-tables t)
             (when open
               (puthash key (list :text text :done t) opencode--text-blocks))))
         (cl-incf text-ordinal))
        ("reasoning"
         (let ((text (alist-get 'text item))
               (native (alist-get 'native item))
               (key (opencode-reasoning-key id reasoning-ordinal)))
           (cond
            ((opencode-narration-p text native)
             (opencode-open-block 'text)
             (opencode--output-message (string-trim text) id)
             (opencode-buttonize-output-end)
             (when open
               (puthash key (list :text text :done t) opencode--text-blocks)))
            ((or (not (string-empty-p (or text "")))
                 (opencode-reasoning-hidden-kind text native))
             (opencode-open-block 'reasoning)
             (opencode-reasoning-end key text native))
            (open (opencode--await-part key))))
         (cl-incf reasoning-ordinal))
        ("tool"
         (opencode-open-block 'tool)
         (let ((overlay (opencode-tool-start (alist-get 'id item) (alist-get 'name item))))
           (apply #'opencode-block-update overlay :message id
                  (opencode-tool-data-from-content item))))))
    (opencode--note-tokens (alist-get 'tokens message))
    (when-let* ((error (alist-get 'error message))
                ;; The turn's idle marker says it was interrupted.
                ((not (equal (alist-get 'type error) "aborted"))))
      (opencode--output-error error))))

(defun opencode--output-message (text message-id)
  "Write TEXT for MESSAGE-ID, tagging it so commands find its message."
  (let ((start (opencode-pmark)))
    (opencode-output text)
    (with-silent-modifications
      (put-text-property start (opencode-pmark) 'opencode-message message-id))))

(defun opencode--output-error (error)
  "Write session ERROR, an alist with `type' and `message', in the error face.
An error already shown during the current turn is not repeated."
  (let ((text (or (alist-get 'message error) (format "%S" error))))
    (unless (member text opencode--turn-errors)
      (push text opencode--turn-errors)
      (opencode-open-block 'text)
      (opencode-output (opencode-fontify-as (propertize text 'font-lock-face 'error) 'verbatim))
      (opencode-output "\n"))))

(defun opencode--note-tokens (tokens)
  "Record TOKENS as the context use, unless they report no usage."
  (when-let* ((size (opencode-context-size tokens)))
    (setq opencode-session-tokens size
          opencode-session-tokens-estimated nil)
    (force-mode-line-update)))

(defun opencode--note-compaction (context)
  "Record the context use a completed compaction left, CONTEXT tokens or nil.
CONTEXT is the service's estimate of the next request.  Its own usage
is that of the compaction request, which read the conversation it
replaced, so without an estimate, from a native compaction or an older
service, the size is unknown until the next step measures it."
  (setq opencode-session-tokens (and (numberp context) context)
        opencode-session-tokens-estimated (numberp context))
  (force-mode-line-update))

(defun opencode--unwrap-notification (text tag attributes)
  "Return TEXT without its TAG element wrapper, or TEXT when it has none.
ATTRIBUTES are the element's (NAME . VALUE) pairs, in the order the
service writes them, unescaped as it writes them."
  (let ((prefix (format "<%s %s>\n" tag
                        (mapconcat (lambda (attribute) (format "%s=\"%s\"" (car attribute) (cdr attribute)))
                                   attributes " ")))
        (suffix (format "\n</%s>" tag)))
    (if (and (>= (length text) (+ (length prefix) (length suffix)))
             (string-prefix-p prefix text)
             (string-suffix-p suffix text))
        (substring text (length prefix) (- (length suffix)))
      text)))

(defun opencode--background-completion (description text metadata)
  "Return the tool block of a background job's completion, or nil.
DESCRIPTION, TEXT and METADATA are those of the synthetic message the
service posts once a shell command or subagent run in the background
ends.  The value is a plist of the block's `:tool', `:input', `:text'
\(TEXT unwrapped) and `:session-id'.  A user's shell command has no
DESCRIPTION: its own shell block shows it already."
  (let-alist metadata
    (when (and description (member .state '("completed" "error" "cancelled")))
      (pcase .source
        ((and "shell" (guard (or .jobID .shellID)))
         (list :tool "shell"
               :input `((command . ,description) (background . t))
               :text (opencode--unwrap-notification
                      text "shell" `((id . ,(or .jobID .shellID)) (state . ,.state) (command . ,description)))))
        ((and "subagent" (guard .childID))
         (list :tool "subagent"
               :input `((agent . ,.agent) (description . ,description) (background . t))
               :session-id .childID
               :text (opencode--unwrap-notification
                      text "subagent"
                      `((sessionID . ,.childID) (state . ,.state) (description . ,description)))))))))

(defun opencode--render-synthetic (id description text metadata)
  "Render synthetic message ID with DESCRIPTION, TEXT and METADATA.
A background job's completion is a block of the job's tool, in the
error face unless the job completed; any other message is a notice."
  (opencode-open-block 'tool)
  (if-let* ((completion (opencode--background-completion description text metadata)))
      (let* ((state (alist-get 'state metadata))
             (completed (equal state "completed"))
             (overlay (opencode-tool-start id (plist-get completion :tool) (plist-get completion :input)
                                           (if completed "completed" "error"))))
        (opencode-block-update
         overlay
         :background-state state
         :metadata metadata
         :session-id (plist-get completion :session-id)
         :output (and completed (plist-get completion :text))
         :error (unless completed
                  `((type . ,(if (equal state "cancelled") "aborted" ""))
                    (message . ,(string-trim (plist-get completion :text)))))))
    (opencode-notice-block (or description "synthetic message") text id)))

(defun opencode-replay-message (message)
  "Render projected MESSAGE of the transcript."
  (let ((open (opencode--projected-open-p message))
        (id (alist-get 'id message)))
    (pcase (alist-get 'type message)
      ("user"
       (opencode--render-user-prompt (opencode--user-message-text message) id))
      ("assistant"
       (opencode--render-assistant-content message open)
       (opencode--note-message-start id (alist-get 'created (alist-get 'time message)))
       (when open (setq opencode--current-assistant id)))
      ("synthetic"
       (opencode--render-synthetic id (alist-get 'description message)
                                   (alist-get 'text message) (alist-get 'metadata message)))
      ("system"
       (opencode-open-block 'tool)
       (opencode-notice-block (or (alist-get 'description message) "Instructions updated")
                              (alist-get 'text message) id))
      ("skill"
       (opencode-open-block 'tool)
       (opencode-notice-block (format "Skill %s activated" (alist-get 'name message))
                              (alist-get 'text message) id))
      ("shell"
       (opencode-open-block 'tool)
       (let ((overlay (opencode-tool-start (alist-get 'shellID message) "shell"
                                           `((command . ,(alist-get 'command message)))
                                           (if open "running" "completed"))))
         (opencode-block-update overlay :output (opencode--shell-output (alist-get 'output message))))
       (when open
         (push (cons (alist-get 'shellID message) id) opencode--open-shells)))
      ("compaction"
       (when (equal (alist-get 'status message) "completed")
         (opencode--note-compaction (alist-get 'context message)))
       (opencode-open-block 'tool)
       (let ((overlay (opencode-notice-block (pcase (alist-get 'status message)
                                               ("failed" "Compaction failed")
                                               ("running" "Compacting…")
                                               (_ "Conversation compacted"))
                                             (or (alist-get 'summary message)
                                                 (alist-get 'message (alist-get 'error message)))
                                             id)))
         (when open
           ;; The events ending it find it as they find a live one.
           (puthash (concat opencode-session-id "/compaction") overlay opencode--blocks)
           (setq opencode--open-compaction id))))
      ("agent-switched"
       (when opencode-show-switch-notices
         (opencode-output-notice (format "Switched to the %s agent" (alist-get 'agent message)))))
      ("model-switched"
       (when opencode-show-switch-notices
         (opencode-output-notice (format "Switched to %s" (opencode-model-label (alist-get 'model message))))))
      ("location-switched"
       (opencode-output-notice (format "Moved to %s" (opencode--session-directory message))))
      ("idle"
       (pcase (alist-get 'outcome message)
         ("failed" (when-let* ((error (alist-get 'error message))) (opencode--output-error error)))
         ("interrupted" (opencode-output-notice "Interrupted")))
       ;; As at the end of a live turn: the next one shows its errors anew.
       (setq opencode--turn-errors nil)))
    (opencode--note-message id open)))

(defun opencode--shell-output (output)
  "Return the text of shell OUTPUT, a string or an alist with `text'."
  (cond ((stringp output) output)
        ((consp output) (or (alist-get 'text output) (alist-get 'output output)))))

(defcustom opencode-replay-slice 0.03
  "Seconds a transcript is written at a time, at most, or nil for no limit.
A long transcript written at once would hold Emacs for seconds, the
more so as a reconnection has every open session buffer catch up at
once: it is written a slice at a time instead, from timers, with other
work running in between."
  :type '(choice (const :tag "No limit" nil) number)
  :group 'opencode)

(defun opencode--replay-slice (messages deadline)
  "Render MESSAGES, at least one, until DEADLINE passes; return those left.
DEADLINE nil renders them all."
  (let ((inhibit-redisplay t)
        (opencode--replaying t)
        (comint-output-filter-functions
         (remove #'comint-postoutput-scroll-to-bottom comint-output-filter-functions)))
    (opencode-without-undo
      (while (and messages
                  (progn
                    (let ((message (pop messages)))
                      (condition-case err
                          (opencode-replay-message message)
                        (error (message "OpenCode: cannot render message %s: %s"
                                        (alist-get 'id message) (error-message-string err)))))
                    (or (null deadline) (< (float-time) deadline))))))
    messages))

(defun opencode-replay-messages (messages &optional then)
  "Render MESSAGES, the projected transcript or its end, oldest first; call THEN.
They are rendered `opencode-replay-slice' seconds at a time, the rest
from a timer.  Erasing or cutting the transcript meanwhile stops them,
and THEN is not called.  While a transcript is written whole (see
`opencode--pinned'), its windows show its start between slices."
  (let ((buffer (current-buffer))
        (generation opencode--render-generation))
    (cl-labels ((slice (messages)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (when (= generation opencode--render-generation)
                        (let ((left (opencode--replay-slice
                                     messages
                                     (and opencode-replay-slice
                                          (+ (float-time) opencode-replay-slice)))))
                          (if (not left)
                              (when then (funcall then))
                            (when opencode--pinned (opencode--pin-viewport))
                            (run-at-time 0 nil #'slice left))))))))
      (slice messages))))

(defun opencode--pin-viewport (&optional end)
  "Show the start of this buffer, or its END, in its windows and at its point.
Redisplay between the slices of a transcript written whole then shows
text it fontified already, rather than fontify the newest text of
every slice: tens of milliseconds a slice for a window of Markdown."
  (let ((position (if end (point-max) (point-min))))
    (goto-char position)
    (dolist (window (get-buffer-window-list nil nil t))
      (set-window-point window position)
      (unless end
        (set-window-start window position t)))))

(declare-function opencode-model-label "opencode-display")

;;;; Live events

(defmacro opencode--on-session-event (event &rest body)
  "Run BODY in the buffer of EVENT's session, with `data' bound to its data.
While the buffer withholds events, BODY runs once they are released."
  (declare (indent 1))
  (let ((held (make-symbol "event")))
    `(let* ((,held ,event)
            (data (alist-get 'data ,held)))
       (opencode-with-session-buffer (alist-get 'sessionID data)
         (opencode--when-live ,held (lambda () ,@body))))))

(defun opencode--text-key (data)
  "Return the text block key of fragment event DATA."
  (format "%s/t/%d" (alist-get 'assistantMessageID data) (alist-get 'ordinal data)))

(defun opencode--event-step-started (event)
  "Handle `session.step.started' EVENT."
  (opencode--on-session-event event
    (let ((id (alist-get 'assistantMessageID data))
          (started (alist-get 'started data))
          (previous opencode--current-assistant))
      (setq opencode--current-assistant id
            opencode-session-retry nil
            opencode-session-agent (or (alist-get 'agent data) opencode-session-agent)
            opencode-session-model (or (alist-get 'model data) opencode-session-model))
      (pcase (gethash id (opencode--message-table))
        ;; The step a fetched transcript holds whole, its start withheld
        ;; meanwhile: its events change nothing.
        ((and 'final (guard (opencode--rendered-step-p id started))))
        ('final
         ;; Restarted, as after a retry: where it starts is not known,
         ;; so the next reconnection renders the transcript whole.
         (setq opencode--synced nil)
         (opencode--note-message id t))
        ('open nil)
        (_
         ;; The service finishes the step that ran before.
         (unless (equal previous id)
           (opencode--finish-message previous))
         (opencode--note-message id t)))
      (opencode--note-message-start id started)
      (force-mode-line-update))))

(defun opencode--live-part-p (data)
  "Return non-nil when the events of the part DATA names still change it.
A finished message's parts are rendered whole already."
  (not (opencode--message-final-p (alist-get 'assistantMessageID data))))

(defun opencode--text-start (key)
  "Start text block KEY at the end of the transcript, unless it is known."
  (unless (gethash key opencode--text-blocks)
    (opencode-align-finished-tables t)
    (opencode-open-block 'text)
    (opencode-set-last-response-start (opencode-pmark))
    (setq opencode--table-scan-marker (copy-marker (opencode-pmark)))
    (puthash key (list :text "") opencode--text-blocks)))

(defun opencode--event-text-started (event)
  "Handle `session.text.started' EVENT."
  (opencode--on-session-event event
    (let ((key (opencode--text-key data)))
      (when (and (opencode--live-part-p data) (not (opencode--part-awaited-p key)))
        (opencode--text-start key)))))

(defun opencode--text-append (data delta)
  "Write DELTA of the text block of fragment DATA, when it is the open block."
  (when-let* ((block (gethash (opencode--text-key data) opencode--text-blocks))
              ((not (plist-get block :done))))
    (let* ((text (plist-get block :text))
           (delta (if (string-empty-p text) (string-trim-left delta) delta)))
      (unless (string-empty-p delta)
        (opencode--output-message delta (alist-get 'assistantMessageID data))
        (plist-put block :text (concat text delta))
        (when (string-search "\n" delta)
          (opencode-align-finished-tables))))))

(defun opencode--event-text-delta (event)
  "Handle `session.text.delta' EVENT."
  (opencode--on-session-event event
    (when (and (opencode--live-part-p data) (not (opencode--part-awaited-p (opencode--text-key data))))
      (opencode--text-append data (alist-get 'delta data)))))

(defun opencode--event-text-ended (event)
  "Handle `session.text.ended' EVENT: complete text missing from the deltas.
A block whose earlier deltas were missed is written whole now."
  (opencode--on-session-event event
    (let ((key (opencode--text-key data)))
      (when (and (opencode--live-part-p data)
                 (not (plist-get (gethash key opencode--text-blocks) :done)))
        (when (hash-table-p opencode--awaited-parts)
          (remhash key opencode--awaited-parts))
        (opencode--text-start key)
        (let* ((block (gethash key opencode--text-blocks))
               (shown (plist-get block :text))
               (full (string-trim-left (or (alist-get 'text data) ""))))
          (cond
           ((string= shown full))
           ((string-prefix-p shown full)
            (opencode--text-append data (substring full (length shown))))
           (t (message "OpenCode: streamed text diverged from the final text; reloading")
              (opencode-session-reload)))
          (plist-put block :done t)
          (opencode-buttonize-output-end)
          (opencode-align-finished-tables t))))))

(defun opencode--reasoning-ended-p (overlay)
  "Return non-nil when reasoning block OVERLAY is shown whole."
  (and overlay
       (or (overlay-get overlay 'opencode-narration)
           (plist-get (opencode-block-data overlay) :ended))))

(defun opencode--event-reasoning (event)
  "Handle the `session.reasoning.*' EVENT.
A block is written once it has content: providers that keep their
reasoning encrypted stream empty blocks, which replay omits too.  One
whose earlier deltas were missed is written whole once it ends."
  (opencode--on-session-event event
    (let ((key (opencode-reasoning-key (alist-get 'assistantMessageID data)
                                       (alist-get 'ordinal data))))
      (unless (hash-table-p opencode--reasoning-native)
        (setq opencode--reasoning-native (make-hash-table :test #'equal)))
      (when (and (opencode--live-part-p data)
                 (not (plist-get (gethash key opencode--text-blocks) :done)))
        (pcase (alist-get 'type event)
          ("session.reasoning.started"
           (unless (opencode--part-awaited-p key)
             (puthash key (alist-get 'native data) opencode--reasoning-native)))
          ("session.reasoning.delta"
           (unless (or (opencode--part-awaited-p key) (opencode--reasoning-ended-p (opencode-block key)))
             (unless (opencode-block key)
               (opencode-align-finished-tables t)
               (opencode-open-block 'reasoning)
               (opencode-reasoning-start key "" (gethash key opencode--reasoning-native)))
             (opencode-reasoning-append key (alist-get 'delta data))))
          ("session.reasoning.ended"
           (let ((text (alist-get 'text data))
                 (native (alist-get 'native data)))
             (remhash key opencode--reasoning-native)
             (when (hash-table-p opencode--awaited-parts)
               (remhash key opencode--awaited-parts))
             (when (or (opencode-block key)
                       (not (string-empty-p (or text "")))
                       (opencode-reasoning-hidden-kind text native))
               (unless (opencode-block key)
                 (opencode-align-finished-tables t)
                 (opencode-open-block (if (opencode-narration-p text native) 'text 'reasoning)))
               (opencode-reasoning-end key text native)))))))))

(defun opencode--event-tool (event)
  "Handle the `session.tool.*' EVENT.
An event for a call shown finished, or of a finished step, changes
nothing: the call is shown as it ended.  Withheld, its end still runs
the hooks of a finished call, as the call did end live."
  (opencode--on-session-event event
    (let* ((id (alist-get 'id data))
           (message (alist-get 'assistantMessageID data))
           (overlay (opencode-block id))
           (done (and overlay (member (plist-get (opencode-block-data overlay) :status)
                                      '("completed" "error"))))
           (live (not (opencode--message-final-p message))))
      (pcase (alist-get 'type event)
        ("session.tool.input.started"
         (when (and live (not overlay))
           (opencode-align-finished-tables t)
           (opencode-open-block 'tool)
           (opencode-block-update (opencode-tool-start id (alist-get 'name data)) :message message)))
        ("session.tool.called"
         (when (and live (not done))
           (unless overlay
             (opencode-open-block 'tool)
             (opencode-tool-start id (or (alist-get 'name data) "tool")))
           (opencode-tool-update id :input (alist-get 'input data) :status "running" :message message)))
        ("session.tool.progress"
         (when (and overlay (not done))
           (let* ((metadata (alist-get 'metadata data))
                  (known (plist-get (opencode-block-data overlay) :session-id))
                  (session (or (alist-get 'sessionID metadata) known)))
             (opencode-block-update overlay :metadata metadata :session-id session)
             ;; A subagent's session is first known here.
             (unless (equal session known)
               (run-hook-with-args 'opencode-blocks-attach-functions overlay)))))
        ((or "session.tool.success" "session.tool.failed")
         (cond
          (done
           (when opencode--releasing
             (run-hook-with-args 'opencode-blocks-finished-functions overlay)))
          ((not live))
          ((equal (alist-get 'type event) "session.tool.success")
           (let ((metadata (alist-get 'metadata data)))
             (opencode-tool-update id :status "completed"
                                   :output (opencode-content-text (alist-get 'content data))
                                   :metadata metadata
                                   :answers (alist-get 'answers metadata)
                                   :session-id (or (alist-get 'sessionID metadata)
                                                   (and overlay
                                                        (plist-get (opencode-block-data overlay) :session-id))))))
          (t
           (opencode-tool-update id :status "error" :error (alist-get 'error data)
                                 :output (opencode-content-text (alist-get 'content data))
                                 :metadata (alist-get 'metadata data)))))))))

(defun opencode--event-step-ended (event)
  "Handle `session.step.ended' EVENT."
  (opencode--on-session-event event
    (let ((id (alist-get 'assistantMessageID data)))
      (unless (opencode--message-final-p id)
        (opencode-align-finished-tables t)
        (opencode--note-tokens (alist-get 'tokens data))
        (run-hook-with-args 'opencode-step-ended-functions data)
        (opencode--finish-message id)))))

(defun opencode--event-step-failed (event)
  "Handle `session.step.failed' EVENT."
  (opencode--on-session-event event
    (let ((id (alist-get 'assistantMessageID data)))
      (unless (opencode--message-final-p id)
        (opencode-align-finished-tables t)
        ;; A step failing after the provider answered records that usage on
        ;; its message, which replay reads.
        (opencode--note-tokens (alist-get 'tokens data))
        (unless (or (run-hook-with-args-until-success 'opencode-step-failed-functions data)
                    ;; The execution's interruption notice says it.
                    (equal (alist-get 'type (alist-get 'error data)) "aborted"))
          (opencode--output-error (alist-get 'error data)))
        (opencode--finish-message id)))))

(defun opencode--event-execution (event)
  "Handle the `session.execution.*' EVENT: a turn starts or ends."
  (opencode--on-session-event event
    (pcase (alist-get 'type event)
      ("session.execution.started"
       (setq opencode-session-status 'busy
             opencode--turn-errors nil)
       ;; A turn another client started follows the idle prompt.
       (opencode-lift-prompt)
       (force-mode-line-update))
      (type
       (let* ((outcome (pcase type
                         ("session.execution.succeeded" 'succeeded)
                         ("session.execution.failed" 'failed)
                         (_ 'interrupted)))
              ;; A shutdown is resumed by the restarted service.
              (shutdown (equal (alist-get 'reason data) "shutdown"))
              (idle (unless shutdown (opencode--event-message-id event)))
              (shown (opencode--message-known-p idle)))
         (opencode-align-finished-tables t)
         (unless shown
           (pcase outcome
             ('failed (opencode--output-error (alist-get 'error data)))
             ('interrupted
              (unless shutdown
                (opencode-output-notice "Interrupted")))))
         (unless shutdown
           (opencode--finish-message opencode--current-assistant)
           (setq opencode-session-status 'idle
                 opencode-session-retry nil
                 opencode--current-assistant nil
                 opencode--turn-errors nil)
           (opencode-call-preserving-viewport
            (current-buffer)
            (lambda ()
              ;; Shown already when the transcript fetched held the end.
              (unless (opencode-prompt-shown-p)
                (opencode-open-block 'user)
                (opencode-show-prompt))))
           (opencode--note-message idle)
           (force-mode-line-update)
           (run-hook-with-args 'opencode-turn-ended-functions outcome)
           (opencode-notify-turn-ended outcome)
           (run-hook-with-args 'opencode-session-idle-functions outcome)))))))

(defun opencode--render-inbox-item (id item)
  "Render inbox ITEM of inbox ID where it entered the conversation."
  (let ((payload (alist-get 'payload item)))
    (pcase (alist-get 'type item)
      ("user"
       (opencode--render-user-prompt (opencode--user-message-text payload) id))
      ("synthetic"
       (opencode-before-prompt
         (opencode--render-synthetic id (alist-get 'description payload)
                                     (alist-get 'text payload) (alist-get 'metadata payload)))))))

(defun opencode--event-inbox (event)
  "Handle the `session.inbox.*' EVENT.
A prompt typed here at an idle session is already shown, and gets its
message ID once delivered.  Other items, such as prompts from other
clients, commands' prompts, prompts sent here while a turn ran and
background job notifications, are shown once delivered: that is where
the transcript places them.  Until then, prompts show as pending (see
`opencode-inbox-update'), and the delivery moves them into the
transcript."
  (opencode--on-session-event event
    (let ((id (alist-get 'inboxID data)))
      (pcase (alist-get 'type event)
        ("session.inbox.enqueued"
         (let ((item (alist-get 'item data)))
           (when (equal (alist-get 'type item) "user")
             (opencode-dismiss-turn-notifications)
             (opencode--claim-command-prompt id item))
           (opencode-inbox-enqueued id item)))
        ("session.inbox.delivery.changed"
         (opencode-inbox-delivery-changed id (alist-get 'delivery data)))
        ("session.inbox.delivered"
         (let* ((own (gethash id opencode--own-inputs))
                (shown (and own (opencode--input-shown-p id)))
                (entry (opencode-inbox-leave id 'delivered))
                (item (plist-get entry :item))
                ;; Prompts and notes become messages named after the item.
                (appended (if item (member (alist-get 'type item) '("user" "synthetic")) own)))
           (opencode--stamp-sent-input id)
           (remhash id opencode--own-inputs)
           (when (and item (not shown) (not (opencode--message-known-p id)))
             (opencode-call-preserving-viewport
              (current-buffer)
              (lambda () (opencode--render-inbox-item id item))))
           (when appended
             (opencode--note-message id))))
        ("session.inbox.cancelled"
         (let ((entry (opencode-inbox-leave id 'cancelled)))
           (opencode--release-sent-input id)
           (when (gethash id opencode--own-inputs)
             (remhash id opencode--own-inputs)
             (cond
              ((plist-get entry :withdraw))
              ;; Shown nowhere else, it goes back to be sent again.
              ((plist-get entry :typed)
               (opencode-put-old-input (plist-get entry :text) (current-buffer))
               (opencode-output-notice "A pending prompt was cancelled; it is back in the input"))
              (t (opencode-output-notice "A queued prompt was cancelled"))))))))))

(defun opencode--input-shown-p (id)
  "Return non-nil when the transcript shows the input sent here as prompt ID.
A prompt typed at an idle session shows where it was typed until a
reload erases it; one typed while a turn ran shows once delivered."
  (when-let* (((gethash id opencode--own-inputs))
              (entry (gethash id (opencode--sent-input-table)))
              (start (plist-get entry :start)))
    (and (markerp start) (eq (marker-buffer start) (current-buffer))
         (not (plist-get entry :command)))))

(defun opencode--restore-withdrawn (id text)
  "Give TEXT, the prompt ID withdrawn from the inbox, back to be edited.
It goes to `opencode-withdrawn-prompt-function', the input by default.
The echo area tells it first, so that the function may tell where the
prompt went instead."
  (remhash id opencode--own-inputs)
  (opencode--release-sent-input id)
  (message "The pending prompt was withdrawn: edit it and send it again")
  (funcall (or opencode-withdrawn-prompt-function #'opencode-put-old-input) text (current-buffer)))

(defun opencode--event-notice (event)
  "Render the notice EVENT reports: a synthetic message, new instructions or a skill."
  (opencode--on-session-event event
    (let* ((id (opencode--event-message-id event))
           (type (alist-get 'type event))
           ;; New instructions without their text make no message.
           (appended (and id (or (not (equal type "session.instructions.updated"))
                                 (alist-get 'text data)))))
      (unless (opencode--message-known-p id)
        (opencode-before-prompt
          (pcase type
            ("session.synthetic"
             (opencode--render-synthetic id (alist-get 'description data)
                                         (alist-get 'text data) (alist-get 'metadata data)))
            ("session.instructions.updated"
             (when-let* ((text (alist-get 'text data)))
               (opencode-open-block 'tool)
               (opencode-notice-block
                (format "Instructions updated: %s"
                        (mapconcat (lambda (entry) (symbol-name (car entry)))
                                   (alist-get 'delta data) ", "))
                text id)))
            ("session.skill.activated"
             (opencode-open-block 'tool)
             (opencode-notice-block (format "Skill %s activated" (alist-get 'name data))
                                    (alist-get 'text data) id)))))
      (when appended
        (opencode--note-message id)))))

(defun opencode--event-shell (event)
  "Handle `session.shell.started' and `session.shell.ended' EVENT."
  (opencode--on-session-event event
    (let* ((shell (alist-get 'shell data))
           (id (alist-get 'id shell)))
      (pcase (alist-get 'type event)
        ("session.shell.started"
         (let ((message (opencode--event-message-id event)))
           (unless (opencode--message-known-p message)
             (opencode-before-prompt
               (opencode-open-block 'tool)
               (opencode-tool-start id "shell" `((command . ,(alist-get 'command shell))) "running")))
           (when message
             (unless (assoc id opencode--open-shells)
               (push (cons id message) opencode--open-shells))
             (opencode--note-message message t))))
        ("session.shell.ended"
         (opencode-tool-update id :status "completed"
                               :output (opencode--shell-output (alist-get 'output data)))
         (when-let* ((message (cdr (assoc id opencode--open-shells))))
           (setq opencode--open-shells (assoc-delete-all id opencode--open-shells))
           (opencode--finish-message message)))))))

(defun opencode--event-compaction (event)
  "Handle the `session.compaction.*' EVENT.
Withheld while no compaction is open here, its end is the one of a
compaction the transcript fetched meanwhile shows finished already."
  (opencode--on-session-event event
    (let ((key (concat (alist-get 'sessionID data) "/compaction"))
          (type (alist-get 'type event)))
      (pcase type
        ("session.compaction.started"
         (let ((message (or (alist-get 'inputID data) (opencode--event-message-id event))))
           (unless (opencode--message-known-p message)
             (when (opencode-block key) (remhash key opencode--blocks))
             (opencode-before-prompt
               (opencode-open-block 'tool)
               (opencode-notice-block "Compacting…" "" key)))
           (when message
             (setq opencode--open-compaction message)
             (opencode--note-message message t))))
        ("session.compaction.delta"
         (when-let* ((overlay (opencode-block key)))
           (opencode-block-update overlay :text (concat (plist-get (opencode-block-data overlay) :text)
                                                        (alist-get 'text data)))))
        ((guard (and opencode--releasing (null opencode--open-compaction))))
        (_
         (when-let* ((overlay (opencode-block key)))
           (if (equal type "session.compaction.ended")
               (progn
                 (opencode-replace-summary
                  overlay (opencode-fontify-as (propertize "Conversation compacted" 'font-lock-face 'opencode-notice)
                                               'verbatim))
                 (opencode-block-update overlay :text (alist-get 'text data)))
             (opencode-block-update overlay :error (alist-get 'error data)))
           ;; Finished: later deltas are no longer its.
           (remhash key opencode--blocks))
         (when (equal type "session.compaction.ended")
           (opencode--note-compaction (alist-get 'context data))
           (run-hook-with-args 'opencode-compaction-ended-functions data))
         (when opencode--open-compaction
           (opencode--finish-message (prog1 opencode--open-compaction
                                       (setq opencode--open-compaction nil)))))))))

(defun opencode--event-session (event)
  "Handle session lifecycle EVENT: renames, selections, reverts, deletion."
  (let* ((data (alist-get 'data event))
         (id (alist-get 'sessionID data)))
    (pcase (alist-get 'type event)
      ("session.deleted"
       (run-hook-with-args 'opencode-session-deleted-functions id)
       (opencode-with-session-buffer id
         (opencode-dismiss-notifications)
         (opencode-output-notice "This session was deleted" 'error)
         (remhash id (opencode-sv opencode-session-buffers))
         (run-hook-with-args 'opencode-session-buffer-functions id)))
      ("session.renamed"
       (opencode-with-session-buffer id
         (setf (alist-get 'title opencode-session-info) (alist-get 'title data))
         (rename-buffer (opencode-session-buffer-name opencode-session-info) t)
         (force-mode-line-update)))
      ("session.archived"
       (opencode--note-archived id (or (alist-get 'created event) (truncate (* 1000 (float-time))))))
      ("session.unarchived"
       (opencode--note-archived id nil))
      (type
       (opencode-with-session-buffer id
         (opencode--when-live
          event
          (lambda ()
            (let* ((message (opencode--event-message-id event))
                   (shown (opencode--message-known-p message)))
              (pcase type
                ("session.agent.selected"
                 (setq opencode-session-agent (alist-get 'agent data))
                 (when (and opencode-show-switch-notices (not shown))
                   (opencode-output-notice (format "Switched to the %s agent" (alist-get 'agent data))))
                 (opencode--note-message message))
                ("session.model.selected"
                 (setq opencode-session-model (alist-get 'model data))
                 (when (and opencode-show-switch-notices (not shown))
                   (opencode-output-notice (format "Switched to %s" (opencode-model-label (alist-get 'model data)))))
                 (opencode--note-message message))
                ("session.usage.updated"
                 (setf (alist-get 'cost opencode-session-info) (alist-get 'cost data)
                       (alist-get 'tokens opencode-session-info) (alist-get 'tokens data)))
                ("session.retry.scheduled"
                 (setq opencode-session-retry data)
                 (opencode-notify-retry data))
                ("session.revert.staged"
                 (setq opencode-session-revert (alist-get 'revert data))
                 (opencode-output-notice
                  (format "A revert is staged: the next prompt drops everything after it, %s cancels it"
                          (opencode-command-keys 'opencode-revert-clear))))
                ("session.revert.cleared"
                 (setq opencode-session-revert nil)
                 (opencode-output-notice "The staged revert was cancelled"))
                ("session.revert.committed"
                 (setq opencode-session-revert nil)
                 (opencode-session-reload))
                ("session.moved"
                 (cl-incf opencode--location-generation)
                 (opencode--adopt-location data)
                 (unless shown
                   (opencode-output-notice (format "Moved to %s" (opencode-location))))
                 (opencode--note-message message))))
            (force-mode-line-update))))))))

(dolist (handler '(("session.step.started" . opencode--event-step-started)
                   ("session.step.ended" . opencode--event-step-ended)
                   ("session.step.failed" . opencode--event-step-failed)
                   ("session.text.started" . opencode--event-text-started)
                   ("session.text.delta" . opencode--event-text-delta)
                   ("session.text.ended" . opencode--event-text-ended)
                   ("session.reasoning.started" . opencode--event-reasoning)
                   ("session.reasoning.delta" . opencode--event-reasoning)
                   ("session.reasoning.ended" . opencode--event-reasoning)
                   ("session.tool.input.started" . opencode--event-tool)
                   ("session.tool.called" . opencode--event-tool)
                   ("session.tool.progress" . opencode--event-tool)
                   ("session.tool.success" . opencode--event-tool)
                   ("session.tool.failed" . opencode--event-tool)
                   ("session.execution.started" . opencode--event-execution)
                   ("session.execution.succeeded" . opencode--event-execution)
                   ("session.execution.failed" . opencode--event-execution)
                   ("session.execution.interrupted" . opencode--event-execution)
                   ("session.inbox.enqueued" . opencode--event-inbox)
                   ("session.inbox.delivered" . opencode--event-inbox)
                   ("session.inbox.cancelled" . opencode--event-inbox)
                   ("session.inbox.delivery.changed" . opencode--event-inbox)
                   ("session.synthetic" . opencode--event-notice)
                   ("session.instructions.updated" . opencode--event-notice)
                   ("session.skill.activated" . opencode--event-notice)
                   ("session.shell.started" . opencode--event-shell)
                   ("session.shell.ended" . opencode--event-shell)
                   ("session.compaction.started" . opencode--event-compaction)
                   ("session.compaction.delta" . opencode--event-compaction)
                   ("session.compaction.ended" . opencode--event-compaction)
                   ("session.compaction.failed" . opencode--event-compaction)
                   ("session.deleted" . opencode--event-session)
                   ("session.renamed" . opencode--event-session)
                   ("session.archived" . opencode--event-session)
                   ("session.unarchived" . opencode--event-session)
                   ("session.agent.selected" . opencode--event-session)
                   ("session.model.selected" . opencode--event-session)
                   ("session.usage.updated" . opencode--event-session)
                   ("session.retry.scheduled" . opencode--event-session)
                   ("session.revert.staged" . opencode--event-session)
                   ("session.revert.cleared" . opencode--event-session)
                   ("session.revert.committed" . opencode--event-session)
                   ("session.moved" . opencode--event-session)))
  (opencode-events-register (car handler) (cdr handler)))

;;;; Reconnection

;; Every open buffer catches up once the stream reopens: it withholds
;; its events, fetches its session and its newest messages, a page at a
;; time from the newest, until the one it is in sync with, and writes
;; those after it again (see `opencode--synced').  A buffer that missed
;; nothing writes nothing.  One whose message is gone, as after a
;; revert, renders its transcript whole.  Shown buffers go first, a
;; few at a time, each written a slice at a time (see
;; `opencode-replay-slice'), so that no buffer holds Emacs long.

(defcustom opencode-resync-requests 4
  "Number of session buffers catching up with the service at once, at most.
After a reconnection, every open session buffer fetches what it missed
and writes it, the buffers shown in a window first."
  :type 'natnum
  :group 'opencode)

(defconst opencode--resync-pages '(5 25 100 200)
  "Sizes of the pages of newest messages fetched to find where a buffer is in sync.
Most buffers missed nothing or a turn, which the small first page finds.")

(opencode-defvar-service opencode--resync-epoch 0
  "Count of the reconnections that had open buffers catch up.
A catch-up begun before the latest one is abandoned.")

(opencode-defvar-service opencode--resync-queue nil
  "Session buffers waiting to catch up, in order.")

(opencode-defvar-service opencode--resync-running nil
  "Session buffers catching up.")

(opencode-defvar-service opencode--resync-active nil
  "(EPOCH . DATA): the running sessions as asked once for reconnection EPOCH.
DATA is the `/api/session/active' data, or `:unknown' when that failed.")

(defun opencode--fetch-newer (id anchor callback else)
  "Call CALLBACK with the messages of session ID after message ANCHOR, oldest first.
CALLBACK receives `:missing' instead when ANCHOR is nil or the session
no longer has it.  Pages are fetched from the newest one, growing as
`opencode--resync-pages' says, until one holds ANCHOR.  When the first
page lacks it, the service is asked whether it still has it at all,
rather than paging back through a transcript a revert cut.  ELSE
receives the `opencode-error' of a failed request."
  (if (null anchor)
      (funcall callback :missing)
    (cl-labels ((page (cursor sizes newer checked)
                  (opencode-request
                   'get (format "/api/session/%s/message" id)
                   ;; A cursor carries the order of the first page.
                   :query `((limit . ,(car sizes))
                            ,@(if cursor `((cursor . ,cursor)) '((order . "desc"))))
                   :else else
                   :then (lambda (response)
                           (let* ((data (opencode-request-data response))
                                  (next (alist-get 'next (alist-get 'cursor response)))
                                  (tail (seq-take-while
                                         (lambda (message) (not (equal (alist-get 'id message) anchor)))
                                         data))
                                  (newer (nconc newer tail))
                                  (sizes (or (cdr sizes) sizes)))
                             (cond
                              ((< (length tail) (length data)) (funcall callback (nreverse newer)))
                              ((not (and data next)) (funcall callback :missing))
                              (checked (page next sizes newer t))
                              (t (check next sizes newer)))))))
                (check (cursor sizes newer)
                  (opencode-request
                   'get (format "/api/session/%s/message/%s" id anchor)
                   :then (lambda (_) (page cursor sizes newer t))
                   :else (lambda (error)
                           (if (eql (opencode-error-status error) 404)
                               (funcall callback :missing)
                             (funcall else error))))))
      (page nil opencode--resync-pages nil nil))))

(defun opencode--resync-turn (epoch callback)
  "Call CALLBACK with non-nil when this buffer's session runs a turn.
The answer asked once for reconnection EPOCH serves, else it is asked
now.  CALLBACK receives `:unknown' when the request fails."
  (let ((id opencode-session-id))
    (cl-flet ((answer (data)
                (funcall callback (if (eq data :unknown) :unknown (assq (intern id) data)))))
      (if (eql (car (opencode-sv opencode--resync-active)) epoch)
          (answer (cdr (opencode-sv opencode--resync-active)))
        (opencode-request 'get "/api/session/active" :directory nil
                          :then (lambda (response) (answer (opencode-request-data response)))
                          :else (lambda (_) (answer :unknown)))))))

(defun opencode--cut-to-synced ()
  "Delete what is written after the point where this buffer is in sync.
The input stays.  Messages written there are written again from the
service's, from the state the transcript had at that point."
  (let ((start (marker-position opencode-synced-end))
        (process (get-buffer-process (current-buffer)))
        (response (plist-get opencode--synced :response))
        (inhibit-read-only t))
    (cl-incf opencode--render-generation)
    (opencode-without-undo
      (opencode-keeping-followers
        (let ((end (opencode-pmark)))
          (when (and (markerp opencode--prompt-start) (marker-buffer opencode--prompt-start)
                     (>= opencode--prompt-start start))
            (set-marker opencode--prompt-start nil)
            (setq opencode--prompt-start nil))
          (opencode-delete-overlays start end)
          (delete-region start end)
          (set-marker (process-mark process) start))))
    ;; Orphans left by earlier versions of this package go too.
    (opencode-prune-parsers-later)
    (when (hash-table-p opencode--blocks)
      (let ((gone nil))
        (maphash (lambda (key overlay) (unless (overlay-buffer overlay) (push key gone))) opencode--blocks)
        (dolist (key gone) (remhash key opencode--blocks))))
    (clrhash opencode--text-blocks)
    (when (hash-table-p opencode--reasoning-native) (clrhash opencode--reasoning-native))
    (when (hash-table-p opencode--awaited-parts) (clrhash opencode--awaited-parts))
    (opencode--sent-inputs-erased start)
    (when (markerp opencode--table-scan-marker)
      (set-marker opencode--table-scan-marker nil)
      (setq opencode--table-scan-marker nil))
    (dolist (id opencode--open-messages)
      (remhash id opencode--messages))
    (when (markerp opencode-last-response-start)
      (set-marker opencode-last-response-start nil))
    (setq opencode--open-messages nil
          opencode--open-shells nil
          opencode--open-compaction nil
          opencode--current-assistant nil
          opencode--newest-message (plist-get opencode--synced :id)
          opencode--last-block-kind (plist-get opencode--synced :kind)
          opencode--turn-errors (copy-sequence (plist-get opencode--synced :errors))
          opencode-last-response-start (and (markerp response) (marker-buffer response)
                                            (copy-marker response)))))

(defun opencode--resync-buffer (&optional then)
  "Catch this buffer up with the service after the stream reopened, then call THEN.
Its events are withheld meanwhile.  Its session info is adopted, and
the messages after the one it is in sync with are written again from
there; when that one is gone, the whole transcript is.  A catch-up
begun before another reconnection gives up, without calling THEN."
  (let ((buffer (current-buffer))
        (id opencode-session-id)
        (epoch (opencode-sv opencode--resync-epoch))
        (location-generation opencode--location-generation)
        (anchor (plist-get opencode--synced :id)))
    (opencode--hold)
    (cl-labels ((current-p ()
                  (and (buffer-live-p buffer) (= epoch (opencode-sv opencode--resync-epoch))))
                (finish ()
                  (when then (funcall then)))
                (settle (active rendered)
                  (opencode--show-status
                   (opencode--status-after-fetch
                    (if (eq active :unknown) (eq opencode-session-status 'busy) active)))
                  (if rendered
                      (run-hooks 'opencode-session-replayed-hook)
                    (run-hook-with-args 'opencode-session-resync-functions opencode-session-info))
                  (opencode--release)
                  (finish))
                (fail (error)
                  (when (current-p)
                    (with-current-buffer buffer
                      (opencode--release)
                      ;; The next reconnection tries again.
                      (let ((inhibit-message t))
                        (message "OpenCode: %s could not catch up: %s"
                                 (buffer-name) (opencode-error-describe error))))
                    (finish)))
                (catch-up (newer active)
                  (if (and (null newer) (null opencode--open-messages))
                      (settle active nil)
                    (opencode--cut-to-synced)
                    (opencode-replay-messages newer (lambda () (settle active t))))))
      (opencode-session-fetch
       id
       (lambda (info)
         (when (current-p)
           (with-current-buffer buffer
             (opencode--adopt-info info location-generation)
             ;; The inbox first: a prompt delivered after it is in the messages.
             (opencode-inbox-fetch
              (lambda ()
                (when (current-p)
                  (opencode--fetch-newer
                   id anchor
                   (lambda (newer)
                     (when (current-p)
                       (with-current-buffer buffer
                         (if (eq newer :missing)
                             (opencode--render-again #'finish)
                           (opencode--resync-turn
                            epoch (lambda (active)
                                    (when (current-p)
                                      (with-current-buffer buffer
                                        (catch-up newer active)))))))))
                   #'fail)))))))
       nil #'fail))))

(defun opencode--resync-pump ()
  "Start the queued catch-ups while fewer than `opencode-resync-requests' run."
  (while (and (opencode-sv opencode--resync-queue)
              (< (length (opencode-sv opencode--resync-running)) (max 1 opencode-resync-requests)))
    (let ((buffer (pop (opencode-sv opencode--resync-queue)))
          (epoch (opencode-sv opencode--resync-epoch)))
      (when (buffer-live-p buffer)
        (push buffer (opencode-sv opencode--resync-running))
        (with-current-buffer buffer
          (opencode--resync-buffer
           (lambda ()
             (when (= epoch (opencode-sv opencode--resync-epoch))
               (setf (opencode-sv opencode--resync-running) (delq buffer (opencode-sv opencode--resync-running)))
               (opencode--resync-pump)))))))))

(defun opencode--resync-enqueue (buffer)
  "Have session BUFFER catch up, after those queued."
  (with-current-buffer buffer
    (opencode--hold))
  (unless (or (memq buffer (opencode-sv opencode--resync-queue)) (memq buffer (opencode-sv opencode--resync-running)))
    (setf (opencode-sv opencode--resync-queue) (append (opencode-sv opencode--resync-queue) (list buffer))))
  (opencode--resync-pump))

(defun opencode--resync-forget (buffer)
  "Forget killed BUFFER's catch-up, letting a queued one start."
  (setf (opencode-sv opencode--resync-queue) (delq buffer (opencode-sv opencode--resync-queue)))
  (when (memq buffer (opencode-sv opencode--resync-running))
    (setf (opencode-sv opencode--resync-running) (delq buffer (opencode-sv opencode--resync-running)))
    (opencode--resync-pump)))

(defun opencode--resync-all ()
  "Have every open session buffer catch up with the reconnected service.
Catch-ups of an earlier reconnection are abandoned and begun again.
Buffers shown in a window go first; a buffer loading its transcript
catches up once it is loaded, as it may have been fetched before."
  (cl-incf (opencode-sv opencode--resync-epoch))
  ;; A catch-up loading a whole transcript finishes loading, then
  ;; catches up again; any other stops writing.
  (dolist (buffer (opencode-sv opencode--resync-running))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless opencode--loading
          (cl-incf opencode--render-generation)))))
  (setf (opencode-sv opencode--resync-running) nil
        (opencode-sv opencode--resync-queue) nil
        (opencode-sv opencode--resync-active) nil)
  (let ((epoch (opencode-sv opencode--resync-epoch))
        (shown nil)
        (hidden nil))
    (maphash (lambda (_id buffer)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (opencode--hold)
                   (cond (opencode--loading (setq opencode--resync-wanted t))
                         ((get-buffer-window buffer t) (push buffer shown))
                         (t (push buffer hidden))))))
             (opencode-sv opencode-session-buffers))
    (setf (opencode-sv opencode--resync-queue) (nconc (nreverse shown) (nreverse hidden)))
    (when (opencode-sv opencode--resync-queue)
      ;; Asked once for every buffer; the withheld events correct it.
      (opencode-request
       'get "/api/session/active" :directory nil :buffer nil
       :then (lambda (response)
               (when (= epoch (opencode-sv opencode--resync-epoch))
                 (setf (opencode-sv opencode--resync-active) (cons epoch (opencode-request-data response)))
                 (opencode--resync-pump)))
       :else (lambda (_)
               (when (= epoch (opencode-sv opencode--resync-epoch))
                 (setf (opencode-sv opencode--resync-active) (cons epoch :unknown))
                 (opencode--resync-pump)))))))

(opencode-defvar-service opencode--stream-seen nil
  "Non-nil once a stream opened in this Emacs; later openings resynchronize.")

(defun opencode--on-connected ()
  "Resynchronize open sessions when a stream reopens."
  (if (opencode-sv opencode--stream-seen)
      (opencode--resync-all)
    (setf (opencode-sv opencode--stream-seen) t)))

(add-hook 'opencode-events-connected-hook #'opencode--on-connected)

(defun opencode--session-buffers-open-p ()
  "Return non-nil while a session buffer is open, which follows its session on the stream."
  (seq-some #'buffer-live-p (hash-table-values (opencode-sv opencode-session-buffers))))

(add-hook 'opencode-events-wanted-functions #'opencode--session-buffers-open-p)

;;;; Message IDs of sent input

;; A prompt's message exists once the service delivers it, which a
;; steering or queued prompt awaits, so its input gets the ID only then:
;; reverting to or forking at a message not yet delivered fails.

(defconst opencode--command-key 'emacsCommandID
  "Metadata key of the correlation key a command sent from Emacs carries.")

(defconst opencode--command-wait 60
  "Seconds a command's input waits for its prompt once its request returned.
The service admits the prompt before it answers, though the stream may
report it a little later; a command run by a subagent prompts no
message of this session at all.")

(defun opencode--track-sent-input (key region &optional command)
  "Remember REGION, a cons (START . END), as the input sent under KEY.
COMMAND means KEY is a command's correlation key.  Commands that waited
too long for their prompt are forgotten first."
  (opencode--forget-stale-commands)
  ;; Text inserted at either end, such as output, stays outside.
  (puthash key (list :start (copy-marker (car region) t) :end (copy-marker (cdr region))
                     :command command)
           (opencode--sent-input-table)))

(defun opencode--release-sent-input (key)
  "Forget the input sent under KEY and return its entry, or nil."
  (when-let* ((entry (gethash key (opencode--sent-input-table))))
    (remhash key (opencode--sent-input-table))
    (dolist (marker (list (plist-get entry :start) (plist-get entry :end)))
      (when (markerp marker) (set-marker marker nil)))
    entry))

(defun opencode--forget-stale-commands ()
  "Forget the commands whose prompt did not arrive in `opencode--command-wait'."
  (let ((now (float-time))
        (stale nil))
    (maphash (lambda (key entry)
               (when-let* ((answered (plist-get entry :answered)))
                 (when (> (- now answered) opencode--command-wait)
                   (push key stale))))
             (opencode--sent-input-table))
    (mapc #'opencode--release-sent-input stale)))

(defun opencode--claim-command-prompt (id item)
  "Let the command that made ITEM, admitted as ID, wait for its delivery."
  (when-let* ((key (alist-get opencode--command-key
                              (alist-get 'metadata (alist-get 'payload item))))
              (entry (gethash key (opencode--sent-input-table))))
    (remhash key (opencode--sent-input-table))
    (puthash id (list :start (plist-get entry :start) :end (plist-get entry :end) :command t)
             (opencode--sent-input-table))))

(defun opencode--stamp-sent-input (id)
  "Give the input sent as message ID, now delivered, that ID; return its entry.
Return nil when no input of this buffer waits for ID."
  (when-let* ((entry (gethash id (opencode--sent-input-table))))
    (let ((start (plist-get entry :start))
          (end (plist-get entry :end)))
      (when (and (markerp start) (eq (marker-buffer start) (current-buffer)) (< start end))
        (with-silent-modifications
          (put-text-property start end 'opencode-message id))
        ;; Only output rendered after the input makes the orders differ.
        (when (opencode--message-rendered-p end (opencode-pmark))
          (unless (hash-table-p opencode--delivery-points)
            (setq opencode--delivery-points (make-hash-table :test #'equal)))
          (puthash id (cons (copy-marker (opencode-pmark)) (cl-incf opencode--delivery-count))
                   opencode--delivery-points))))
    (opencode--release-sent-input id)))

(defun opencode--message-rendered-p (start end)
  "Return non-nil when part of a message is rendered between START and END."
  (or (text-property-not-all start end 'opencode-message nil)
      (seq-some (lambda (overlay) (and (opencode-block-data overlay) (opencode--block-message overlay)))
                (overlays-in start end))))

(defun opencode--sent-inputs-erased (&optional from)
  "Note that the transcript was erased from FROM with inputs waiting for their ID.
FROM nil means the whole transcript.  A prompt stays known, marked
lost, so it is rendered once delivered; a command's prompt is rendered
anyway.  Rendered again, the transcript is in the conversation's order,
so delivery points there are forgotten."
  (cl-flet ((erased-p (marker)
              (and (markerp marker) (or (null from) (>= marker from)))))
    (when (hash-table-p opencode--delivery-points)
      (let ((gone nil))
        (maphash (lambda (id point)
                   (when (erased-p (car point))
                     (set-marker (car point) nil)
                     (push id gone)))
                 opencode--delivery-points)
        (dolist (id gone) (remhash id opencode--delivery-points))))
    (let ((commands nil))
      (maphash (lambda (key entry)
                 (when (erased-p (plist-get entry :start))
                   (if (plist-get entry :command)
                       (push key commands)
                     (dolist (marker (list (plist-get entry :start) (plist-get entry :end)))
                       (when (markerp marker) (set-marker marker nil)))
                     (puthash key (list :lost t) (opencode--sent-input-table)))))
               (opencode--sent-input-table))
      (mapc #'opencode--release-sent-input commands))))

;;;; Sending

(defun opencode-session-ready-p ()
  "Return non-nil when this session buffer is loaded and can send input."
  (and opencode-session-id (get-buffer-process (current-buffer))
       (opencode-events-connected-p)))

(defun opencode--prompt-files ()
  "Return the file attachments of the next prompt, as a vector."
  (vconcat (delq nil (mapcar (lambda (attachment) (plist-get attachment :file))
                             (reverse opencode--attachments)))))

(defun opencode--prompt-agents ()
  "Return the agent mentions of the next prompt, as a vector."
  (vconcat (delq nil (mapcar (lambda (attachment)
                               (when-let* ((name (plist-get attachment :agent)))
                                 `((name . ,name))))
                             (reverse opencode--attachments)))))

(defun opencode--prompt-skills ()
  "Return the skills attached to the next prompt, as a vector."
  (vconcat (delq nil (mapcar (lambda (attachment)
                               (when-let* ((id (plist-get attachment :skill)))
                                 `((id . ,id))))
                             (reverse opencode--attachments)))))

(defun opencode--defer-prompt-p ()
  "Return non-nil when a prompt sent now shows as pending until delivered.
While a turn runs, the service delivers a prompt at the next step, or
once the turn ends, and the transcript places it there; so does it
while other prompts wait, which it delivers first.  An idle session
with nothing pending starts a turn with it at once."
  (or (not (eq opencode-session-status 'idle))
      (opencode-inbox-shown-p)))

(defun opencode-thinking-metadata (metadata &optional accept)
  "Return prompt METADATA with its stale-thinking choice.
Thinking from earlier turns goes stale when what preceded it changed.
Unless METADATA already chooses, ask the server to refuse a turn that
would replay stale thinking, so Emacs can ask before it is lost, rather
than let the provider drop it unseen; with ACCEPT, consent to replacing
those blocks with their text.  Services without my fork ignore the key."
  (if (assq 'thinkingLoss metadata)
      metadata
    (cons `(thinkingLoss . ,(if accept "accept" "ask")) metadata)))

;; Input sent to an archived session restores it from the archive first.
;; Whether it is archived is known locally, so other sessions send as
;; before, without a request more.

(opencode-defvar-service opencode--restores (make-hash-table :test #'equal)
  "Sessions being restored from the archive for input sent to them.
Each ID maps to the sends waiting for the restore, newest first, as
\(BUFFER THEN ELSE); see `opencode--after-restore'.")

(defvar-local opencode--input-awaits-restore nil
  "Non-nil while the input at the prompt waits for its session to leave the archive.")

(defun opencode-session-archived-p (&optional id)
  "Return non-nil when session ID is archived, as Emacs knows it without asking.
ID defaults to this buffer's session.  The registry tells when it holds
the session; otherwise the info of the session's buffer does, as for a
buffer opened on an archived session the registry has not fetched yet."
  (let ((id (or id opencode-session-id)))
    (if-let* ((entry (opencode-state-get id)))
        (and (plist-get entry :archived) t)
      (when-let* ((buffer (seq-find (lambda (buffer)
                                      (and (buffer-live-p buffer)
                                           (equal (buffer-local-value 'opencode-session-id buffer) id)))
                                    (list (gethash id (opencode-sv opencode-session-buffers)) (current-buffer)))))
        (and (alist-get 'archived (alist-get 'time (buffer-local-value 'opencode-session-info buffer)))
             t)))))

(defun opencode--after-restore (then else)
  "Call THEN once this buffer's session is out of the archive, or ELSE.
A session that is not archived calls THEN at once.  Otherwise the
service is asked to restore it, once however many sends wait for that,
and once it did, the waiting sends call their THEN in the order they
were made, each in its buffer; were it refused, they call ELSE with the
`opencode-error' of the refusal.  A send whose buffer was killed
meanwhile is dropped."
  (let ((id opencode-session-id)
        (waiter (list (current-buffer) then else)))
    (cond
     ((gethash id (opencode-sv opencode--restores))
      (push waiter (gethash id (opencode-sv opencode--restores))))
     ((not (opencode-session-archived-p id))
      (funcall then))
     (t
      (puthash id (list waiter) (opencode-sv opencode--restores))
      (cl-flet ((settle (restored &rest arguments)
                  (let ((waiters (reverse (gethash id (opencode-sv opencode--restores)))))
                    (remhash id (opencode-sv opencode--restores))
                    (pcase-dolist (`(,buffer ,then ,else) waiters)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (with-demoted-errors "OpenCode send: %S"
                            (apply (if restored then else) arguments))))))))
        (condition-case error
            (opencode-session-archive id t
                                      (lambda (_info) (settle t))
                                      (lambda (error) (settle nil error)))
          (error (remhash id (opencode-sv opencode--restores))
                 (signal (car error) (cdr error)))))))))

(defun opencode--restore-failure (error)
  "Return the one-line notice that input was not sent, as restoring failed with ERROR."
  (truncate-string-to-width
   (concat "Not sent: restoring the session from the archive failed: "
           (opencode-state--one-line (opencode-error-describe error)))
   300 nil nil "…"))

(cl-defun opencode-session-send (text &key files agents skills attachments delivery metadata
                                      accept-thinking-loss show region input then else)
  "Send TEXT as a prompt of this session.
FILES, AGENTS and SKILLS are vectors of attachments; ATTACHMENTS, a
list of attachments as the labels of `opencode-old-input' carry them,
\(:file URI-ALIST), (:agent NAME) or (:skill ID ...), go after them, so
that a prompt withdrawn or sent before goes again whole from elsewhere,
as from drover's prompt.  DELIVERY overrides
`opencode-prompt-delivery'; METADATA is an alist attached to the
prompt.  ACCEPT-THINKING-LOSS lets the server replace earlier thinking
that no longer matches the conversation, which it otherwise refuses to
send (see `opencode-thinking-metadata').

Where the prompt shows: REGION, a cons (START . END) of the text showing
it as typed, gets the prompt's message ID as its `opencode-message'
property once the server delivers it, the message existing only then.
INPUT, the prompt as typed at this buffer's input, as `opencode-old-input'
returns a prompt, shows it as pending until delivered, then in the
transcript, where it goes back to the input should the server refuse
it.  SHOW renders it once delivered, like a prompt of another client,
and as pending until then while a turn runs or other prompts wait (see
`opencode--defer-prompt-p').  Pending, it can be withdrawn to be edited
again (see `opencode-edit-pending-prompt').

THEN runs with the admitted inbox item, ELSE with the error; a prompt
that fails keeps no ID.

An archived session (see `opencode-session-archived-p') is restored from
the archive first, and the prompt sent once the service restored it;
prompts sent meanwhile wait for that same restore, and go in the order
they were sent.  Were it refused, nothing is sent, and ELSE receives
the error of the restore's request, a PATCH of the session.

No send check runs here (see `opencode-check-send'): a prompt sent while
the session waits on a permission request or a question reaches the
agent once that is answered or dismissed, as the agent's step waits for
it."
  (let ((id (opencode-generate-id "msg_"))
        (metadata (opencode-thinking-metadata metadata accept-thinking-loss))
        (delivery (symbol-name (or delivery opencode-prompt-delivery))))
    (when attachments
      (setq files (vconcat files (delq nil (mapcar (lambda (attachment) (plist-get attachment :file))
                                                   attachments)))
            agents (vconcat agents (delq nil (mapcar (lambda (attachment)
                                                       (when-let* ((name (plist-get attachment :agent)))
                                                         `((name . ,name))))
                                                     attachments)))
            skills (vconcat skills (delq nil (mapcar (lambda (attachment)
                                                       (when-let* ((skill (plist-get attachment :skill)))
                                                         `((id . ,skill))))
                                                     attachments)))))
    (opencode-dismiss-turn-notifications)
    (unless show
      (puthash id t opencode--own-inputs)
      (when region
        (opencode--track-sent-input id region)))
    (when (and (not region) (or input (opencode--defer-prompt-p)))
      (opencode-inbox-add-local
       id (or input
              (opencode-inbox-attachments-text
               text (append (mapcar (lambda (file) (list (list :file file))) files)
                            (mapcar (lambda (agent) (list (list :agent (alist-get 'name agent)))) agents)
                            (mapcar (lambda (skill) (list (list :skill (alist-get 'id skill)))) skills))))
       delivery (and input t)))
    ;; The turn shows only through the stream.
    (opencode-events-reopen)
    (let ((path (format "/api/session/%s/prompt" opencode-session-id))
          (body `((id . ,id) (text . ,text)
                  ,@(when (and files (> (length files) 0)) `((files . ,files)))
                  ,@(when (and agents (> (length agents) 0)) `((agents . ,agents)))
                  ,@(when (and skills (> (length skills) 0)) `((skills . ,skills)))
                  ,@(when metadata `((metadata . ,metadata)))
                  (delivery . ,delivery))))
      (cl-flet ((fail (error describe)
                  (remhash id opencode--own-inputs)
                  (opencode--release-sent-input id)
                  (let ((pending (opencode-inbox-drop id)))
                    (cond
                     (else (funcall else error))
                     ((plist-get pending :typed)
                      ;; Shown nowhere else, it goes back to be sent again.
                      (opencode-put-old-input (plist-get pending :text) (current-buffer))
                      (opencode-output-notice
                       (concat (funcall describe error) "; the prompt is back in the input")
                       'error))
                     (t (opencode-output-notice (funcall describe error) 'error))))))
        (opencode--after-restore
         (lambda ()
           (opencode-request
            'post path :body body
            :then (lambda (response)
                    (let ((item (opencode-request-data response)))
                      (opencode-inbox-admitted id (and (consp item) (alist-get 'type item) item))
                      (when then (funcall then item))))
            :else (lambda (error) (fail error #'opencode-error-describe))))
         (lambda (error) (fail error #'opencode--restore-failure)))))
    id))

(cl-defun opencode-session-command (name arguments &key metadata accept-thinking-loss region then else)
  "Run slash command NAME with ARGUMENTS text in this session.
METADATA and ACCEPT-THINKING-LOSS are attached to the command's prompt,
THEN and ELSE run, as for `opencode-session-send'.  The service gives
the prompt a command makes an ID of its own and expands its text, so
it is rendered once delivered, like a prompt of another client; REGION,
a cons (START . END) of the text showing the command, gets that ID too.
The command's prompt is recognized by a correlation key Emacs adds to
METADATA, which the service passes on to it.  An archived session is
restored from the archive first, as for `opencode-session-send'."
  (let* ((key (and region (opencode-generate-id "cmd_")))
         (metadata (opencode-thinking-metadata
                    (if key (append metadata `((,opencode--command-key . ,key))) metadata)
                    accept-thinking-loss)))
    (opencode-dismiss-turn-notifications)
    (when key
      (opencode--track-sent-input key region t))
    (opencode-events-reopen)
    (let ((path (format "/api/session/%s/command" opencode-session-id))
          (body `((name . ,name) (text . ,arguments)
                  (metadata . ,metadata)
                  (delivery . ,(symbol-name opencode-prompt-delivery)))))
      (cl-flet ((fail (error describe)
                  (when key (opencode--release-sent-input key))
                  (if else (funcall else error)
                    (opencode-output-notice (funcall describe error) 'error))))
        (opencode--after-restore
         (lambda ()
           (opencode-request
            'post path :body body
            :then (lambda (_)
                    (when-let* ((entry (and key (gethash key (opencode--sent-input-table)))))
                      (plist-put entry :answered (float-time)))
                    (when then (funcall then nil)))
            :else (lambda (error) (fail error #'opencode-error-describe))))
         (lambda (error) (fail error #'opencode--restore-failure)))))))

(defun opencode--session-shell (command)
  "Run shell COMMAND in this session, restoring it from the archive first."
  (opencode-events-reopen)
  (let ((path (format "/api/session/%s/shell" opencode-session-id)))
    (opencode--after-restore
     (lambda () (opencode-request 'post path :body `((command . ,command))))
     (lambda (error) (opencode-output-notice (opencode--restore-failure error) 'error)))))

(defvar opencode-send-checks nil
  "Functions checking input before it is sent, in order.
Each is called in the session buffer with the plist `opencode-input-request'
returns and returns nil to send it, `cancel' to keep it in the prompt, or
an alist of metadata to attach to it.")

(defvar opencode--send-decision nil
  "Outcome of the checks of the input being sent: t, or metadata to attach.")

(defvar opencode--sending-input nil
  "The session buffer whose input Comint is taking to send it, or nil.
Comint deletes the input and inserts it again as sent text, which must
not drop the attachments whose labels it holds: they go with the prompt.
Bound by `opencode--comint-send', whatever command runs, rather than
read from `this-command', which names RET's command, not Comint's; it
ends once Comint hands the input over to `opencode--send-input'.")

(defun opencode--comint-send ()
  "Have Comint send the input at point, keeping its attachments for the prompt."
  (let ((opencode--sending-input (current-buffer)))
    (comint-send-input)))

(defvar opencode--input-sent nil
  "The input Comint is taking to send, as `opencode--copy-input' returns it.
A send check that keeps input Comint took already puts it back from
this, with its attachment labels.  Nil when Comint sends an earlier
input rather than the one at the prompt.")

(defvar opencode--routing-input nil
  "Non-nil while `comint-send-input' called in a session buffer runs as RET.")

(defun opencode--comint-send-input (send &rest arguments)
  "Run Comint's SEND, `comint-send-input', with ARGUMENTS as session buffers send.
In a session buffer, input sent otherwise than by `opencode--comint-send',
such as with `execute-extended-command', goes through
`opencode-send-input' as RET's does: its checks, the archive, its
attachments.  Input Comint then takes keeps the attachments its labels
stand for.  Elsewhere, and for text a session buffer writes through
Comint with another sender, SEND runs as it is."
  (cond
   ((not (and (derived-mode-p 'opencode-session-mode)
              (eq comint-input-sender #'opencode--send-input)))
    (apply send arguments))
   ((and (not (seq-some #'identity arguments))
         (not (eq opencode--sending-input (current-buffer)))
         (not opencode--routing-input))
    (let ((opencode--routing-input t))
      (opencode-send-input)))
   (t
    (let ((opencode--sending-input (current-buffer))
          (opencode--input-sent (and (get-buffer-process (current-buffer))
                                     (>= (point) (opencode-pmark))
                                     (opencode--copy-input))))
      (apply send arguments)))))

(advice-add 'comint-send-input :around #'opencode--comint-send-input)

(defun opencode-input-request (string)
  "Return what sending input STRING requests, as a plist.
:kind is `prompt', `command' or `shell'; :text is STRING; a command also
has :name and :arguments.  A caller that switches the agent or model
before sending adds :agent and :model, a model reference, so the checks
consider the turn the switch leads to."
  (let ((string (string-trim-right string)))
    (cond
     ((string-prefix-p "/" string)
      (let ((space (string-search " " string)))
        (list :kind 'command :text string :name (substring string 1 space)
              :arguments (if space (substring string (1+ space)) ""))))
     ((string-prefix-p "!" string) (list :kind 'shell :text string))
     (t (list :kind 'prompt :text string)))))

(defun opencode-check-send (request)
  "Run the send checks on REQUEST; return `cancel', or the metadata to attach.
REQUEST is a plist like the one `opencode-input-request' returns.  The
checks may ask questions, so callers run them from a command, before
any asynchronous step.  `opencode-before-send-functions' run first."
  (if (eq 'cancel (run-hook-with-args-until-success 'opencode-before-send-functions
                                                    (list :kind (plist-get request :kind)
                                                          :text (plist-get request :text)
                                                          :name (plist-get request :name))))
      'cancel
    (let ((metadata nil)
          (checks opencode-send-checks))
      (while (and checks (not (eq metadata 'cancel)))
        (let ((result (funcall (pop checks) request)))
          (setq metadata (if (eq result 'cancel) 'cancel (append metadata result)))))
      metadata)))

(defvar opencode--send-cancel-quietly nil
  "Non-nil when a send check that kept the input told the user why itself.")

(defun opencode-send-input ()
  "Send the input at the prompt, unless a check keeps it there.
Checks run before Comint takes the input, so declining leaves it as typed.
An archived session is restored from the archive once the checks passed,
and the input sent once the service restored it; were the restore
refused, the input stays at the prompt.  In the transcript, nothing is
sent: the region, else the line at point, goes to
`opencode-old-input-function', quoted elsewhere or put in the input."
  (interactive)
  (cond
   ((not (and opencode-session-id (get-buffer-process (current-buffer))))
    (opencode--comint-send))
   ((< (point) (opencode-pmark))
    (opencode--reuse-old-input))
   (opencode--input-awaits-restore
    (message "The session is being restored from the archive; the input is sent then"))
   (t
    (let ((input (buffer-substring-no-properties (opencode-pmark) (point-max))))
      (if (string-blank-p input)
          ;; An empty prompt would start a turn about nothing.
          (message "Nothing to send")
        (let* ((opencode--send-cancel-quietly nil)
               (decision (opencode-check-send (opencode-input-request input))))
          (if (eq decision 'cancel)
              (unless opencode--send-cancel-quietly
                (message "Not sent"))
            (opencode--send-checked-input (or decision t)))))))))

(defun opencode--send-checked-input (decision)
  "Have Comint send the input at the prompt, checked with DECISION.
DECISION is t, or the metadata the checks attach.  An archived session
is restored first: the input waits at the prompt, where it can still
be edited, and is sent as if RET were pressed again there; were the
restore refused, it stays."
  (let ((delivery opencode-prompt-delivery)
        (settled nil))
    (opencode--after-restore
     (lambda ()
       (setq settled t
             opencode--input-awaits-restore nil)
       (goto-char (max (point) (opencode-pmark)))
       (if (string-blank-p (buffer-substring-no-properties (opencode-pmark) (point-max)))
           (message "Nothing to send")
         (let ((opencode--send-decision decision)
               (opencode-prompt-delivery delivery))
           (opencode--comint-send))))
     (lambda (error)
       (setq settled t
             opencode--input-awaits-restore nil)
       (opencode-output-notice
        (concat (opencode--restore-failure error) "; the input is still at the prompt")
        'error)))
    (unless settled
      (setq opencode--input-awaits-restore t))))

;;;; Earlier prompts

(defun opencode--prompt-bounds (position)
  "Return (START . END) of the earlier prompt at POSITION, or nil.
POSITION may be at the end of the prompt, where its last line ends."
  (when-let* ((process (get-buffer-process (current-buffer)))
              (pmark (marker-position (process-mark process)))
              ((< position pmark))
              (at (cond ((get-text-property position 'opencode-prompt) position)
                        ((and (> position (point-min))
                              (get-text-property (1- position) 'opencode-prompt))
                         (1- position)))))
    (cons (or (previous-single-property-change (1+ at) 'opencode-prompt) (point-min))
          (next-single-property-change at 'opencode-prompt nil pmark))))

(defun opencode--uri-file (uri)
  "Return the local file name of file URI, or nil for another URI."
  (let ((parsed (url-generic-parse-url uri)))
    (when (and (equal (url-type parsed) "file")
               (member (url-host parsed) '(nil "" "localhost")))
      (let ((file (url-unhex-string (car (url-path-and-query parsed)))))
        ;; A drive letter follows the slash of the path.
        (if (string-match-p "\\`/[A-Za-z]:" file) (substring file 1) file)))))

(defun opencode--reattachable-p (attachment)
  "Return non-nil when sent ATTACHMENT can go with a prompt again.
A subagent mention can; a file can while it is on disk."
  (or (plist-get attachment :agent)
      (when-let* ((uri (alist-get 'uri (plist-get attachment :file)))
                  (file (opencode--uri-file uri)))
        (file-exists-p file))))

(defun opencode--attachment-text (start end)
  "Return the text from START to END without properties but on reattachable labels.
The labels of the attachments that can go with a prompt again carry
them as their `opencode-sent-attachment' property."
  (let ((text (buffer-substring-no-properties start end))
        (pos start))
    (while (< pos end)
      (let ((attachment (get-text-property pos 'opencode-sent-attachment))
            (next (next-single-property-change pos 'opencode-sent-attachment nil end)))
        (when (and attachment (opencode--reattachable-p attachment))
          (put-text-property (- pos start) (- next start) 'opencode-sent-attachment attachment text))
        (setq pos next)))
    text))

(defun opencode--old-input-text (start end)
  "Return the text of an earlier prompt from START to END, as `opencode-old-input' does."
  (string-trim-right (opencode--attachment-text start end)))

(defun opencode--hidden-p (position)
  "Return non-nil when an overlay hides the character at POSITION.
Those are the blocks of a collapsed group and the prompt string before
each prompt; text that markup hides, such as a link's target, shows."
  (seq-some (lambda (overlay)
              (when-let* ((invisible (overlay-get overlay 'invisible)))
                (invisible-p invisible)))
            (overlays-at position)))

(defun opencode-transcript-text (start end)
  "Return the transcript's text from START to END as RET quotes it.
What overlays hide is left out, the blocks of a collapsed group and
the prompt string before each prompt, and so is the trailing
whitespace; the Markdown stays as written, markup and link targets
included.  The text has no properties but on the labels of the
attachments that can go with a prompt again, which carry them, as in
`opencode-old-input'."
  (let ((parts nil)
        (pos start))
    (while (< pos end)
      (let ((next (min end (next-overlay-change pos))))
        (unless (opencode--hidden-p pos)
          (push (opencode--attachment-text pos next) parts))
        (setq pos next)))
    (string-trim-right (apply #'concat (nreverse parts)))))

(defun opencode-old-input (&optional position)
  "Return the text of the earlier prompt at POSITION, or nil when none.
POSITION defaults to point, and may be at the end of the prompt.  The
text is the whole prompt, all its lines, without text properties but
on the labels of the attachments it can take along again: subagent
mentions, and files still on disk, whose label carries the attachment,
\(:file URI-ALIST) or (:agent NAME), as its `opencode-sent-attachment'
property.  The labels of other attachments, such as a buffer or region
sent inline, are text."
  (when-let* ((bounds (opencode--prompt-bounds (or position (point)))))
    (opencode--old-input-text (car bounds) (cdr bounds))))

(defun opencode-old-input-at-point ()
  "Return what RET picks in the transcript at point, or nil in the input.
That is the active region's text, else the line at point, up to the
input: an earlier prompt's, the agent's, a block's summary or any
other.  The text is as `opencode-transcript-text' returns it, without
what overlays hide; the labels of attachments that can go again carry
them, as in `opencode-old-input'."
  (when-let* ((process (get-buffer-process (current-buffer)))
              (pmark (marker-position (process-mark process)))
              ((< (point) pmark)))
    (if (use-region-p)
        (opencode-transcript-text (region-beginning) (region-end))
      (opencode-transcript-text (pos-bol) (min (pos-eol) pmark)))))

(defun opencode--reuse-old-input ()
  "Give what RET picks in the transcript at point to `opencode-old-input-function'.
Return non-nil when point is in the transcript.  A blank line gives
nothing, and the echo area says so."
  (when-let* ((text (opencode-old-input-at-point)))
    (if (string-blank-p text)
        (message "Nothing to quote here")
      (funcall (or opencode-old-input-function #'opencode-put-old-input) text (current-buffer)))
    t))

(defun opencode--insert-old-input (text)
  "Insert TEXT, as `opencode-old-input' returns it, at point in the input.
Its labels carrying an attachment stand for it again, as typed ones do."
  (let ((pos 0)
        (length (length text)))
    (while (< pos length)
      (let ((attachment (get-text-property pos 'opencode-sent-attachment text))
            (next (or (next-single-property-change pos 'opencode-sent-attachment text) length)))
        (if attachment
            (opencode--insert-attachment
             (string-remove-prefix "`" (string-remove-suffix "`" (substring-no-properties text pos next)))
             ;; Each label drops its own attachment.
             (copy-tree attachment))
          (insert (substring-no-properties text pos next)))
        (setq pos next)))))

(defun opencode-put-old-input (text buffer)
  "Put TEXT, an earlier prompt or other text of session BUFFER, in BUFFER's input.
TEXT is as `opencode-old-input' returns it: its labels carrying an
attachment stand for it again.  A blank input is replaced; input typed
already stays, TEXT following it after a blank line, which the echo
area tells.  Point goes to the end of TEXT.  Nothing is sent.  This is
what nil stands for in `opencode-old-input-function'."
  (with-current-buffer buffer
    (let* ((pmark (opencode-pmark))
           (draft (not (string-blank-p (buffer-substring-no-properties pmark (point-max))))))
      (if draft
          (progn
            (goto-char (point-max))
            (insert (cond ((string-suffix-p "\n\n" (buffer-substring-no-properties pmark (point))) "")
                          ((eq (char-before) ?\n) "\n")
                          (t "\n\n"))))
        (delete-region pmark (point-max))
        (goto-char pmark))
      (opencode--insert-old-input text)
      (when draft
        (message "The text follows the input already typed")))))

(defun opencode-copy-old-input ()
  "Put the earlier prompt at point, whole, in the input, to be edited.
This is `opencode-put-old-input' whatever `opencode-old-input-function'
says.  Elsewhere in the transcript, Comint's `comint-copy-old-input'
copies the line at point."
  (interactive)
  (if-let* ((text (opencode-old-input)))
      (opencode-put-old-input text (current-buffer))
    (comint-copy-old-input)))

(defun opencode-queue-input ()
  "Send the input at the prompt to start after the running turn, not steer it."
  (interactive)
  (let ((opencode-prompt-delivery 'queue))
    (opencode-send-input)))

(defun opencode-previous-input-or-button (&optional count)
  "Recall the COUNTth previous input at the prompt, else move to a previous button."
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-previous-input count)
    (backward-button count)))

(defun opencode-next-input-or-button (&optional count)
  "Recall the COUNTth next input at the prompt, else move to a next button."
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-next-input count)
    (forward-button count)))

(defun opencode--restore-input (text)
  "Put TEXT back at the prompt after a send was cancelled.
The input Comint took is put back whole, with its attachment labels,
when known (see `opencode--input-sent'); else TEXT, an earlier input
Comint sent."
  (if opencode--input-sent
      (opencode--put-back-input opencode--input-sent)
    (goto-char (point-max))
    (insert text)))

(defun opencode--send-input (_process string)
  "Send STRING typed at the prompt: a prompt, a /command or a !shell command.
Input that did not go through `opencode-send-input' is checked here.  The
input of a prompt or a command gets the ID of the message it makes once
the service delivers it, which reverting and forking at point need; a
shell command makes no prompt, only a shell message and its result,
which like other messages between prompts never start a cut.  A prompt
sent while a turn runs, or while other prompts wait, leaves the
transcript: it shows as pending until delivered, then where the
conversation places it (see `opencode--defer-prompt-p').  The prompt's
attachments are those whose labels the input held; Comint leaves their
overlays emptied, which go, the labels' text keeping the attachments
that can go with a prompt again (see `opencode--mark-sent-attachments')."
  (let* ((opencode--sending-input nil)
         (request (opencode-input-request string))
         (string (plist-get request :text))
         (decision (or opencode--send-decision (opencode-check-send request)))
         (metadata (and (consp decision) decision))
         (deferred (and (not (eq decision 'cancel))
                        (eq (plist-get request :kind) 'prompt)
                        (opencode--defer-prompt-p)))
         (input (and deferred (opencode--sent-input-text string)))
         (files (opencode--prompt-files))
         (agents (opencode--prompt-agents))
         (skills (opencode--prompt-skills))
         (temp-files opencode--temp-files)
         (region nil))
    (if deferred
        (opencode--unsend-input)
      (opencode-highlight-input)
      (opencode-output "\n")
      (opencode--forget-attachment-labels comint-last-input-start comint-last-input-end)
      (opencode--mark-sent-attachments comint-last-input-start comint-last-input-end)
      (setq region (cons comint-last-input-start comint-last-input-end)))
    (when (and region (not (eq decision 'cancel)) (memq (plist-get request :kind) '(prompt command)))
      (opencode-group-earlier-turn (car region)))
    (if (eq decision 'cancel)
        (opencode--restore-input string)
      (setq opencode--attachments nil
            opencode--temp-files nil)
      (pcase (plist-get request :kind)
        ('command
         (opencode-session-command (plist-get request :name) (plist-get request :arguments)
                                   :metadata metadata :region region
                                   :then (lambda (_) (mapc #'delete-file temp-files))))
        ('shell
         (opencode--session-shell (substring string 1)))
        ('prompt
         (opencode-session-send string :files files :agents agents :skills skills :metadata metadata
                                :region region :input input
                                :then (lambda (_) (mapc (lambda (file) (ignore-errors (delete-file file)))
                                                        temp-files))))))))

(defun opencode--sent-input-text (string)
  "Return STRING, the prompt Comint took, as `opencode-old-input' returns one.
Each attachment label carries its attachment, inline ones included, so
that the prompt withdrawn or refused goes back to the input whole.  The
labels are those of `opencode--input-sent', the input before Comint
took it; without it, labels of the prompt's attachments follow STRING."
  (let ((input (plist-get opencode--input-sent :text)))
    (if (and input (string= (string-trim-right (substring-no-properties input)) string))
        (let ((text (copy-sequence string)))
          (set-text-properties 0 (length text) nil text)
          (pcase-dolist (`(,from ,to ,attachment ,_display) (plist-get opencode--input-sent :labels))
            (when (<= to (length text))
              (put-text-property from to 'opencode-sent-attachment attachment text)))
          text)
      (opencode-inbox-attachments-text
       string (mapcar #'list (reverse opencode--attachments))))))

(defun opencode--unsend-input ()
  "Take the input Comint just sent out of the transcript, as it was before.
A deferred prompt shows as pending until delivered, then where the
conversation places it.  Its labels' attachments stay with the prompt."
  (let ((start (marker-position comint-last-input-start))
        (end (opencode-pmark))
        (inhibit-read-only t))
    (opencode--forget-attachment-labels start end)
    (opencode-delete-parsers start end)
    (let ((opencode--sending-input (current-buffer)))
      (delete-region start end))
    (set-marker (process-mark (get-buffer-process (current-buffer))) start)
    (setq opencode--last-block-kind opencode--block-kind-before-input)))

;;;; Attachments

(defun opencode--forget-attachment-labels (start end)
  "Delete the overlays of the attachment labels between START and END.
Comint taking the input to send it leaves them there, emptied."
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'opencode-attachment)
      (delete-overlay overlay))))

(defun opencode--mark-sent-attachments (start end)
  "Mark the labels of the input sent from START to END with their attachments.
The labels are those of `opencode--input-sent', the input as it was
before Comint took it.  A subagent mention, or a file read from a file
URI, goes on its label's text as its `opencode-sent-attachment'
property, for the prompt to be edited again with it, as the transcript
rendered from the server gives it (see `opencode--user-message-text');
a file sent inline does not, as its data would stay in the buffer."
  (when-let* ((input (plist-get opencode--input-sent :text))
              ((<= (+ start (length input)) end))
              ((string= (substring-no-properties input)
                        (buffer-substring-no-properties start (+ start (length input))))))
    (with-silent-modifications
      (pcase-dolist (`(,from ,to ,attachment ,_display) (plist-get opencode--input-sent :labels))
        (when (or (plist-get attachment :agent)
                  (when-let* ((uri (alist-get 'uri (plist-get attachment :file))))
                    (opencode--uri-file uri)))
          (put-text-property (+ start from) (+ start to) 'opencode-sent-attachment attachment))))))

(defun opencode--in-attachment-p ()
  "Return non-nil when point is inside an attachment label."
  (cl-loop for overlay in (overlays-at (point))
           thereis (overlay-get overlay 'opencode-attachment)))

(defun opencode--remove-attachment (overlay after _beg _end &optional _length)
  "Drop OVERLAY's attachment once its label is deleted, AFTER the change.
Comint taking the input to send it deletes the labels too, which drops
nothing (see `opencode--sending-input')."
  (when (and after (not (eq opencode--sending-input (current-buffer))))
    (let ((attachment (overlay-get overlay 'opencode-attachment))
          (start (overlay-start overlay))
          (end (overlay-end overlay)))
      (setq opencode--attachments (delq attachment opencode--attachments))
      (delete-overlay overlay)
      ;; The change hooks that would delete them are off.
      (opencode-delete-parsers start end)
      (let ((inhibit-modification-hooks t))
        (delete-region start end)))))

(defun opencode--insert-attachment (label attachment)
  "Insert an intangible LABEL standing for ATTACHMENT at point."
  (push attachment opencode--attachments)
  (let ((start (point)))
    (insert "`" (propertize label 'cursor-intangible t)
            (propertize "`" 'rear-nonsticky t))
    (opencode--label-attachment start (point) attachment (propertize label 'face 'markdown-ts-code))))

(defun opencode--label-attachment (start end attachment display)
  "Make the text from START to END the label of ATTACHMENT, shown as DISPLAY.
Deleting any of it drops the attachment.  Return the label's overlay."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'opencode-attachment attachment)
    (overlay-put overlay 'display display)
    (overlay-put overlay 'modification-hooks '(opencode--remove-attachment))
    overlay))

;; Rendering lifts the input out and puts it back: another client's
;; prompt goes before it, and a transcript rendered again erases it.  Its
;; text alone would lose the attachments its labels stand for, so every
;; path lifting it goes through this pair.

(defun opencode--copy-input (&optional take)
  "Return the input typed at the prompt, its attachment labels included.
The value, for `opencode--put-back-input', is a plist: :text, the input
with its text properties; :labels, its attachment labels, as (START END
ATTACHMENT DISPLAY) with START and END offsets in :text; :attachments,
the attachments it takes along.  With TAKE, the input is removed, and
the next prompt's attachments go with it, out of `opencode--attachments'
until put back: the labels' overlays go first, so that removing their
text drops nothing.  Without TAKE, the input and the attachments stay,
and :attachments is nil.  Return nil without a prompt."
  (when (get-buffer-process (current-buffer))
    (let* ((start (opencode-pmark))
           (end (point-max))
           (overlays (seq-filter (lambda (overlay) (overlay-get overlay 'opencode-attachment))
                                 (overlays-in start end)))
           (input (list :text (buffer-substring start end)
                        :labels (mapcar (lambda (overlay)
                                          (list (- (overlay-start overlay) start)
                                                (- (overlay-end overlay) start)
                                                (overlay-get overlay 'opencode-attachment)
                                                (overlay-get overlay 'display)))
                                        (seq-filter (lambda (overlay)
                                                      (< (overlay-start overlay) (overlay-end overlay)))
                                                    overlays))
                        :attachments (and take opencode--attachments))))
      (when take
        (mapc #'delete-overlay overlays)
        (setq opencode--attachments nil)
        (let ((inhibit-read-only t))
          (delete-region start end)))
      input)))

(defun opencode--take-input ()
  "Remove the input typed at the prompt; return it for `opencode--put-back-input'.
See `opencode--copy-input'."
  (opencode--copy-input t))

(defun opencode--put-back-input (input)
  "Insert INPUT, which `opencode--copy-input' returned, at the end of the buffer.
Its labels stand for their attachments again, deleting one dropping
it, and the attachments it took along are the next prompt's again.
Point ends after it.  Return where it starts."
  (goto-char (point-max))
  (let ((start (point)))
    (when input
      (insert (plist-get input :text))
      (pcase-dolist (`(,from ,to ,attachment ,display) (plist-get input :labels))
        (opencode--label-attachment (+ start from) (+ start to) attachment display))
      (setq opencode--attachments (append opencode--attachments (plist-get input :attachments))))
    start))

(defun opencode--prompt-buffer ()
  "Return the session buffer whose input prompt commands edit.
That is this buffer when it is a session buffer, so commands run in it
from elsewhere act on its own session; otherwise the last selected one."
  (cond ((and opencode-session-id (get-buffer-process (current-buffer))) (current-buffer))
        ((buffer-live-p opencode-last-session-buffer) opencode-last-session-buffer)
        (t (user-error "Open an OpenCode session first"))))

(defmacro opencode--with-prompt (buffer &rest body)
  "Run BODY in session BUFFER, with point in its input.
Point stays where it is when already in the input, else moves to its end."
  (declare (indent 1) (debug (form body)))
  `(with-current-buffer ,buffer
     (unless (comint-after-pmark-p) (goto-char (point-max)))
     ,@body))

(defun opencode--file-uri (file)
  "Return the file URI of FILE."
  (concat "file://" (if (string-prefix-p "/" (expand-file-name file)) "" "/")
          (url-hexify-string (expand-file-name file) url-path-allowed-chars)))

(defun opencode--data-uri (text)
  "Return a data URI holding TEXT as UTF-8 plain text."
  (concat "data:text/plain;base64," (base64-encode-string (encode-coding-string text 'utf-8) t)))

(defun opencode--attach-file (file &optional name)
  "Attach FILE to the next prompt, shown as NAME."
  (let ((name (or name (opencode-relative-path-for-display file))))
    (opencode--insert-attachment
     name (list :file `((uri . ,(opencode--file-uri file)) (name . ,name))))
    (insert " ")))

(defun opencode--attach-buffer (buffer target)
  "Attach the text of BUFFER to the next prompt of session buffer TARGET."
  (let ((name (buffer-name buffer))
        (text (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max)))))
    (opencode--with-prompt target
      (opencode--insert-attachment name (list :file `((uri . ,(opencode--data-uri text)) (name . ,name))))
      (insert " "))))

(defun opencode-add-file (&optional file directories)
  "Attach FILE, or a file of the current project, to the next prompt.
With prefix argument DIRECTORIES, offer project directories.  The prompt
is this session buffer's, else the last selected session's."
  (interactive (list nil current-prefix-arg))
  (let* ((target (opencode--prompt-buffer))
         (project (project-current t))
         (root (project-root project))
         (files (project-files project))
         (file (or file
                   (project--read-file-name
                    project "Attach"
                    (if directories
                        (delete root (delete-dups (cons (concat root "./")
                                                        (mapcar #'file-name-directory files))))
                      files)
                    nil 'file-name-history))))
    (opencode--with-prompt target (opencode--attach-file (expand-file-name file root)))))

(defun opencode-add-buffer-dwim ()
  "Attach a buffer's text to the next prompt.
From a session buffer prompt for the buffer, elsewhere attach the current
one to the last selected session."
  (interactive)
  (let ((target (opencode--prompt-buffer)))
    (opencode--attach-buffer (if (eq target (current-buffer))
                                 (get-buffer (read-buffer "Attach buffer: " nil t))
                               (current-buffer))
                             target)))

(defun opencode-add-region ()
  "Attach the active region to the next prompt.
The prompt is this session buffer's, else the last selected session's."
  (interactive)
  (unless (use-region-p) (user-error "No active region"))
  (let* ((target (opencode--prompt-buffer))
         (text (buffer-substring-no-properties (region-beginning) (region-end)))
         (name (format "region of %s" (buffer-name))))
    (opencode--with-prompt target
      (opencode--insert-attachment (format "region: %s" (truncate-string-to-width text 24 nil nil "…"))
                                   (list :file `((uri . ,(opencode--data-uri text)) (name . ,name))))
      (insert " "))))

(defconst opencode--attach-buffer-prefix "buffer:"
  "Prefix of the buffers `opencode-attach' offers, telling them from files.")

(defun opencode--attachment-group (candidate transform)
  "Return the group of `opencode-attach' CANDIDATE, or with TRANSFORM its display."
  (let ((buffer (string-prefix-p opencode--attach-buffer-prefix candidate)))
    (cond ((not transform) (if buffer "Buffers" "Files"))
          (buffer (substring candidate (length opencode--attach-buffer-prefix)))
          (t candidate))))

(defun opencode--attachment-candidates (exclude)
  "Return a hash table of what `opencode-attach' offers, and its keys in order.
Keys are file names relative to the current project's root, or to
`default-directory' outside a project, whose files only are offered
then, and live buffer names after `opencode--attach-buffer-prefix';
values are (file . ABSOLUTE-NAME) or (buffer . BUFFER).  Buffers come
first, most recently used first.  Internal buffers and EXCLUDE are left
out."
  (let* ((project (project-current))
         (root (if project (project-root project) default-directory))
         (table (make-hash-table :test #'equal))
         (keys nil))
    (dolist (buffer (buffer-list))
      (unless (or (eq buffer exclude) (string-prefix-p " " (buffer-name buffer)))
        (let ((key (concat opencode--attach-buffer-prefix (buffer-name buffer))))
          (puthash key (cons 'buffer buffer) table)
          (push key keys))))
    (dolist (file (if project
                      (project-files project)
                    (seq-filter #'file-regular-p
                                (directory-files root t directory-files-no-dot-files-regexp))))
      (let* ((file (expand-file-name file root))
             (key (file-relative-name file root)))
        ;; A file name that reads as a buffer gets the prefix of a relative name.
        (when (string-prefix-p opencode--attach-buffer-prefix key)
          (setq key (concat "./" key)))
        (puthash key (cons 'file file) table)
        (push key keys)))
    (list table (nreverse keys))))

(defun opencode--read-attachment (exclude)
  "Read a file of the current project or a live buffer other than EXCLUDE.
Return (file . ABSOLUTE-NAME), (buffer . BUFFER), or nil for no choice."
  (pcase-let* ((`(,table ,keys) (opencode--attachment-candidates exclude))
               (annotate (lambda (key)
                           (pcase (gethash key table)
                             (`(buffer . ,buffer)
                              (concat "  " (symbol-name (buffer-local-value 'major-mode buffer)))))))
               (collection (lambda (string predicate action)
                             (if (eq action 'metadata)
                                 `(metadata (category . opencode-attachment)
                                            (group-function . opencode--attachment-group)
                                            (annotation-function . ,annotate))
                               (complete-with-action action keys string predicate)))))
    (gethash (completing-read "Attach: " collection nil t) table)))

(defun opencode-attach ()
  "Attach the active region, else a file or a buffer, to the next prompt.
Without a region, the files of the current project and the live buffers
are offered together, in two groups; buffer names start with
\"buffer:\".  A file is attached by reference, a buffer by its text, as
`opencode-add-file' and `opencode-add-buffer-dwim' do; `opencode-add-file'
also attaches directories.  The prompt is this session buffer's, else the
last selected session's."
  (interactive)
  (if (use-region-p)
      (opencode-add-region)
    (let ((target (opencode--prompt-buffer)))
      (pcase (opencode--read-attachment target)
        (`(file . ,file) (opencode--with-prompt target (opencode--attach-file file)))
        (`(buffer . ,buffer) (opencode--attach-buffer buffer target))))))

(defun opencode--yank-image (type data)
  "Attach pasted image DATA of MIME TYPE to the next prompt."
  (unless (comint-after-pmark-p)
    (user-error "Images can only be pasted at the prompt"))
  (let* ((mime (symbol-name type))
         (name (format "pasted %s" (or (cadr (split-string mime "[/;+]" t)) "image")))
         (uri (concat "data:" mime ";base64," (base64-encode-string data t))))
    (opencode--insert-attachment name (list :file `((uri . ,uri) (name . ,name))))
    (insert " ")))

(defun opencode--read-subagent ()
  "Read a subagent of this session's directory; nil when it has none.
Subagents are the agents of mode `subagent' or `all' that are not hidden."
  (when-let* ((subagents (seq-filter (lambda (agent)
                                       (and (member (alist-get 'mode agent) '("subagent" "all"))
                                            (not (alist-get 'hidden agent))))
                                     (opencode-catalog-wait 'agent))))
    (opencode-annotated-completion
     "Subagent: "
     (mapcar (lambda (agent)
               (list (alist-get 'id agent) (alist-get 'id agent) (alist-get 'description agent)))
             subagents))))

(defun opencode--insert-mention (name)
  "Insert a mention of subagent NAME at point, sent with the next prompt."
  (opencode--insert-attachment (concat "@" name) (list :agent name)))

(defun opencode-add-subagent ()
  "Insert @, then offer the subagents when at the input.
The chosen subagent's mention replaces the @; without a choice it stays.
This is what @ does in a session buffer; `opencode-mention-subagent'
works from any key."
  (interactive)
  (call-interactively #'self-insert-command)
  (when (comint-after-pmark-p)
    (when-let* ((name (opencode--read-subagent)))
      (delete-char -1)
      (opencode--insert-mention name))))

(defun opencode-mention-subagent ()
  "Choose a subagent and mention it in the next prompt.
The mention goes at point when it is in the input, else at the input's
end.  The prompt is this session buffer's, else the last selected
session's."
  (interactive)
  (opencode--with-prompt (opencode--prompt-buffer)
    (opencode--insert-mention (or (opencode--read-subagent)
                                  (user-error "This session has no subagent to mention")))))

(defun opencode--read-command ()
  "Read a slash command of this session's directory; nil when it has none."
  (opencode-annotated-completion
   "Command: "
   (mapcar (lambda (command)
             (list (alist-get 'name command) (alist-get 'name command) (alist-get 'description command)))
           (opencode-catalog-wait 'command))))

(defun opencode-insert-slash-command ()
  "Choose a slash command and put it at the start of the input.
Text already typed becomes the command's arguments, except a command
the input starts with already, which the chosen one replaces.  Point
ends after the command.  The prompt is this session buffer's, else the
last selected session's."
  (interactive)
  (opencode--with-prompt (opencode--prompt-buffer)
    (when-let* ((name (opencode--read-command)))
      (goto-char (opencode-pmark))
      (when (looking-at "/[^ \t\n]*[ \t]?")
        (delete-region (match-beginning 0) (match-end 0)))
      (insert "/" name " "))))

(defun opencode-electric-slash ()
  "Choose a slash command when typed at the start of the input, else insert /.
At the start of the input this is `opencode-insert-slash-command'."
  (interactive)
  (if (and (comint-after-pmark-p) (= (point) (opencode-pmark)))
      (opencode-insert-slash-command)
    (call-interactively #'self-insert-command)))

;;;; Agent and model

(defun opencode--require-session ()
  "Signal an error outside a session buffer."
  (unless opencode-session-id (user-error "Not in an OpenCode session")))

(declare-function opencode-read-project-session "opencode-sessions" (prompt &optional running))

(defun opencode--session-here (prompt &optional running)
  "Return the info of this buffer's session, else of a picked one.
Outside a session buffer, the session is read with PROMPT among those
`opencode' offers, only those running a turn when RUNNING is non-nil.
Signal a `user-error' when there is none or none is chosen."
  (if opencode-session-id
      (or opencode-session-info `((id . ,opencode-session-id)))
    ;; The pickers' module requires this one, so it cannot be required at load time.
    (require 'opencode-sessions)
    (opencode-read-project-session prompt running)))

(defun opencode--switch-failed (else)
  "Return the failure handler of a switch: ELSE, else one reporting the error."
  (or else (lambda (error) (opencode-report-error (opencode-error-describe error)))))

(cl-defun opencode-switch-agent (agent &key model then else)
  "Switch this session to AGENT, an agent its directory can run.
The session keeps its model unless AGENT pins one, which it then
switches to, as the TUI does; MODEL, a model reference, is selected
instead when given.  THEN runs in the buffer once the service took
every switch; ELSE receives the `opencode-error' of a refused one, or a
message when AGENT is not an agent the session can run, since the
service would select any name.  Without ELSE, failures are reported."
  (opencode--require-session)
  (let ((buffer (current-buffer))
        (else (opencode--switch-failed else)))
    (opencode-catalog-fetch
     'agent
     (lambda (_)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let* ((info (seq-find (lambda (entry) (equal (alist-get 'id entry) agent))
                                  (opencode-primary-agents)))
                  (model (or model (alist-get 'model info))))
             (cl-flet ((follow ()
                         (if model
                             (opencode-switch-model model :then then :else else)
                           (when then (funcall then)))))
               (cond
                ((null info)
                 (funcall else (format "%s cannot run as agent %s" (buffer-name) agent)))
                ((equal agent opencode-session-agent) (follow))
                (t
                 (opencode-request
                  'post (format "/api/session/%s/agent" opencode-session-id)
                  :body `((agent . ,agent))
                  :else else
                  :then (lambda (_)
                          (setq opencode-session-agent agent)
                          (force-mode-line-update)
                          (follow))))))))))
     nil else)))

(cl-defun opencode-switch-model (model &key then else)
  "Switch this session to MODEL, a model reference, and make it the last pick.
THEN runs in the buffer once the service took the switch; ELSE receives
the `opencode-error' of a refused one, which is otherwise reported.  The
buffer records MODEL at once, as the selection event may trail the reply."
  (opencode--require-session)
  (setq opencode-last-model model)
  (if (opencode-same-model-p model opencode-session-model)
      (when then (funcall then))
    (opencode-request 'post (format "/api/session/%s/model" opencode-session-id)
                      :body `((model . ,model))
                      :else (opencode--switch-failed else)
                      :then (lambda (_)
                              (setq opencode-session-model model)
                              (force-mode-line-update)
                              (when then (funcall then))))))

(defun opencode-same-model-p (a b)
  "Return non-nil when model references A and B select the same model and variant.
A missing variant is the model's default, as the service compares them."
  (and a b
       (equal (alist-get 'providerID a) (alist-get 'providerID b))
       (equal (alist-get 'id a) (alist-get 'id b))
       (equal (or (alist-get 'variant a) "default") (or (alist-get 'variant b) "default"))))

(defun opencode-cycle-agent ()
  "Switch this session to the next primary agent."
  (interactive)
  (opencode--require-session)
  (let ((buffer (current-buffer)))
    (opencode-catalog-fetch
     'agent
     (lambda (_)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let* ((agents (mapcar (lambda (agent) (alist-get 'id agent)) (opencode-primary-agents)))
                  (next (or (cadr (member opencode-session-agent agents)) (car agents))))
             (if next
                 (opencode-switch-agent next)
               (message "No agent to switch to")))))))))

(defun opencode-select-agent ()
  "Choose this session's primary agent.
With exactly two agents to choose from, switch to the other one at once."
  (interactive)
  (opencode--require-session)
  (opencode-catalog-wait 'agent)
  (let* ((agents (opencode-primary-agents))
         (ids (mapcar (lambda (agent) (alist-get 'id agent)) agents))
         ;; A session that never chose runs the default agent.
         (current (or opencode-session-agent (opencode-default-agent))))
    (if (and (= (length ids) 2) (member current ids))
        (opencode-switch-agent (car (remove current ids)))
      (when-let* ((agent (opencode-annotated-completion
                          "Agent: "
                          (mapcar (lambda (agent)
                                    (list (alist-get 'id agent) (alist-get 'id agent)
                                          (alist-get 'description agent)))
                                  agents))))
        (opencode-switch-agent agent)))))

(defun opencode-select-model ()
  "Choose this session's model."
  (interactive)
  (opencode--require-session)
  (when-let* ((model (opencode-annotated-completion
                      "Model: "
                      (mapcar (lambda (entry)
                                (list (format "%s/%s" (alist-get 'providerID entry)
                                              (alist-get 'id entry))
                                      entry (alist-get 'name entry)))
                              (opencode-catalog-wait 'model)))))
    (let* ((variant (alist-get 'variant opencode-session-model))
           (offered (mapcar (lambda (v) (alist-get 'id v)) (alist-get 'variants model))))
      (opencode-switch-model
       `((id . ,(alist-get 'id model)) (providerID . ,(alist-get 'providerID model))
         ,@(when (member variant offered) `((variant . ,variant))))))))

(defun opencode-select-variant ()
  "Choose the variant, such as the reasoning effort, of this session's model."
  (interactive)
  (opencode--require-session)
  (opencode-catalog-wait 'model)
  (let* ((info (opencode-model-info opencode-session-model))
         (variants (alist-get 'variants info)))
    (if (null variants)
        (message "This model has no variants")
      (let ((variant (completing-read
                      "Variant: "
                      (cons "default" (mapcar (lambda (v) (alist-get 'id v)) variants))
                      nil t)))
        (opencode-switch-model
         `((id . ,(alist-get 'id opencode-session-model))
           (providerID . ,(alist-get 'providerID opencode-session-model))
           ,@(unless (equal variant "default") `((variant . ,variant)))))))))

;;;; Session commands

(defun opencode-session-interrupt (id &optional then else)
  "Interrupt the running turn of session ID, whether or not it has a buffer.
THEN, when given, is called once the service answered, with non-nil
when it interrupted a turn and nil when none ran, which is otherwise
told in the echo area; ELSE with the `opencode-error' of a refusal,
which is otherwise reported.  Either is called once, in whatever buffer
is current then."
  (opencode-request 'post (format "/api/session/%s/interrupt" id)
                    :directory (opencode-state--directory id) :buffer nil
                    :then (lambda (response)
                            (let ((interrupted (alist-get 'interrupted response)))
                              (cond (then (funcall then interrupted))
                                    ((not interrupted) (message "Nothing to interrupt")))))
                    :else else))

(defun opencode-interrupt ()
  "Interrupt the running turn of this session.
Outside a session buffer, the session is picked among the running ones
of the current project."
  (interactive)
  (opencode-session-interrupt (alist-get 'id (opencode--session-here "Interrupt session: " t))))

(defun opencode-session-archive (id &optional restore then else)
  "Archive session ID, or with RESTORE restore it from the archive.
Archived sessions stay whole; pickers and `opencode-state-sessions'
leave them out.  THEN, when given, is called with the session's info as
the service returns it once the service archived or restored it; ELSE
with the `opencode-error' of a refusal, which is otherwise reported.
Either is called once, in whatever buffer is current then, so that a
caller can act on the outcome, such as close the session's buffer only
once the archive took.  The registry and the session's buffer know of
the outcome before THEN runs, without waiting for its event."
  (opencode-request 'patch (concat "/api/session/" id)
                    :directory (opencode-state--directory id) :buffer nil
                    :body `((archived . ,(if restore :false t)))
                    :then (lambda (response)
                            (let* ((info (opencode-request-data response))
                                   (archived (unless restore
                                               (or (alist-get 'archived (alist-get 'time info))
                                                   (truncate (* 1000 (float-time)))))))
                              (opencode-state--note-archived id (opencode-time-seconds archived))
                              (opencode--note-archived id archived)
                              (when then (funcall then info))))
                    :else else))

(defun opencode--note-archived (id archived)
  "Record in session ID's buffer that it was archived at ARCHIVED, or restored.
ARCHIVED is in epoch milliseconds, as the service reports times, or nil
for a session restored from the archive.  The buffer's session info
then tells it, for sessions the registry does not hold."
  (opencode-with-session-buffer id
    (unless (equal archived (alist-get 'archived (alist-get 'time opencode-session-info)))
      (let ((time (copy-alist (alist-get 'time opencode-session-info))))
        (setf (alist-get 'archived time nil t) archived)
        (setf (alist-get 'time opencode-session-info) time))
      (force-mode-line-update))))

(defun opencode--reply-text (message)
  "Return the text of assistant MESSAGE, or nil when it has none."
  (when-let* ((text (opencode-content-text (alist-get 'content message)))
              (text (string-trim text))
              ((not (string-empty-p text))))
    text))

(defun opencode--newest-assistant (id then else)
  "Call THEN with the newest assistant messages of session ID.
THEN receives NEWEST and REPLY: the newest assistant message, and the
newest one with text, which may be older, as steps that only called
tools have none; either is nil when the session has none.  Pages of
messages are fetched until REPLY is found.  ELSE receives the
`opencode-error' of a failed request."
  (let ((newest nil))
    (cl-labels ((page (cursor)
                  (opencode-request
                   'get (format "/api/session/%s/message" id)
                   :directory (opencode-state--directory id) :buffer nil
                   ;; A cursor carries its order, and the type must follow it.
                   :query `((limit . 20) (type . "assistant")
                            ,@(if cursor `((cursor . ,cursor)) '((order . "desc"))))
                   :else else
                   :then (lambda (response)
                           (let ((messages (opencode-request-data response))
                                 (next (alist-get 'next (alist-get 'cursor response))))
                             (setq newest (or newest (car messages)))
                             (if-let* ((reply (seq-find #'opencode--reply-text messages)))
                                 (funcall then newest reply)
                               (if (and messages next)
                                   (page next)
                                 (funcall then newest nil))))))))
      (page nil))))

(defun opencode-session-last-reply (id callback &optional max-length)
  "Call CALLBACK with the text of session ID's last assistant reply, or nil.
The reply is the newest assistant message with text; steps that only
called tools are skipped.  Its text is cut to MAX-LENGTH characters,
2000 by default, the last one an ellipsis.  CALLBACK receives nil when
there is no reply or the request fails."
  (let ((max-length (or max-length 2000)))
    (opencode--newest-assistant
     id
     (lambda (_newest reply)
       (let ((text (and reply (opencode--reply-text reply))))
         (funcall callback (if (and text (> (length text) max-length))
                               (concat (substring text 0 (max 0 (1- max-length))) "…")
                             text))))
     (lambda (_) (funcall callback nil)))))

(defun opencode-session-last-turn (id callback)
  "Call CALLBACK with a recap of the last turn of session ID, as a plist.
The turn is that of the newest user message.  The plist holds:

 :prompt   The text of that message.
 :reply    The text of the newest assistant message with text since
           it, whole: what the agent said last.
 :error    The message of the error the turn's newest assistant
           message ended with.
 :changes  The files the turn changed, as (FILE ADDITIONS DELETIONS),
           FILE as the service names it, relative to the session's
           working tree.

Each is nil when the turn has none.  The prompt, the replies and the
files changed are asked for at once; a request failing leaves its
members nil, and when every request fails the plist is (:failure
MESSAGE).  CALLBACK is called once, possibly before this returns, in
whatever buffer is current then."
  (let ((pending 3)
        (failures nil)
        prompt newest reply changes)
    (cl-labels ((done ()
                  (when (zerop (cl-decf pending))
                    (funcall callback (opencode--last-turn prompt newest reply changes failures))))
                (fail (error)
                  (push (opencode-error-describe error) failures)
                  (done)))
      (let ((directory (opencode-state--directory id)))
        (opencode-request
         'get (format "/api/session/%s/message" id)
         :directory directory :buffer nil
         :query '((limit . 1) (type . "user") (order . "desc"))
         :else #'fail
         :then (lambda (response)
                 (setq prompt (car (opencode-request-data response)))
                 (done)))
        (opencode--newest-assistant
         id (lambda (message text-message)
              (setq newest message reply text-message)
              (done))
         #'fail)
        (opencode-request
         'get (format "/api/session/%s/diff" id)
         :directory directory :buffer nil
         ;; Hunks without context: only the counts are read.
         :query '((context . "0"))
         :else #'fail
         :then (lambda (response)
                 (setq changes (opencode-request-data response))
                 (done)))))))

(defun opencode--last-turn (prompt newest reply changes failures)
  "Return the recap of `opencode-session-last-turn'.
PROMPT is the newest user message, NEWEST the newest assistant
message, REPLY the newest one with text, CHANGES the files the turn
changed, each nil when unknown; FAILURES the descriptions of the
requests that failed, newest first."
  (if (= (length failures) 3)
      (list :failure (car (last failures)))
    (let ((since (alist-get 'created (alist-get 'time prompt))))
      (cl-flet ((in-turn (message)
                  ;; Before the prompt, a message is a former turn's.
                  (and message
                       (or (not (numberp since))
                           (let ((created (alist-get 'created (alist-get 'time message))))
                             (or (not (numberp created)) (>= created since))))
                       message)))
        (let ((text (alist-get 'text prompt))
              (error (alist-get 'error (in-turn newest))))
          (list :prompt (and (stringp text) (not (string-blank-p text)) (string-trim text))
                :reply (opencode--reply-text (in-turn reply))
                :error (let ((message (alist-get 'message error)))
                         (and (stringp message) (not (string-empty-p message)) message))
                :changes (mapcar (lambda (change)
                                   (let-alist change
                                     (list .file
                                           (and (numberp .additions) .additions)
                                           (and (numberp .deletions) .deletions))))
                                 changes)))))))

(defun opencode--session-title (id)
  "Return the title of session ID as the registry or its buffer knows it, or nil."
  (or (plist-get (opencode-state-get id) :title)
      (when-let* ((buffer (gethash id (opencode-sv opencode-session-buffers)))
                  ((buffer-live-p buffer)))
        (alist-get 'title (buffer-local-value 'opencode-session-info buffer)))))

(defun opencode-rename-session (&optional id title)
  "Rename session ID to TITLE.
ID defaults to this buffer's session; outside a session buffer, it is
picked among the current project's.  TITLE is read when nil, starting
from the current title.  The session needs no buffer."
  (interactive)
  (let* ((info (unless id (opencode--session-here "Rename session: ")))
         (id (or id (alist-get 'id info)))
         (title (or title (read-string "Title: " (or (opencode--session-title id)
                                                     (alist-get 'title info))))))
    (opencode-request 'patch (concat "/api/session/" id)
                      :directory (opencode-state--directory id) :buffer nil
                      :body `((title . ,title)))))

(defun opencode-session-delete (id &optional then else)
  "Delete session ID for good, without confirmation, and kill its buffer.
The session needs no buffer.  THEN, when given, is called with no
argument once the service deleted the session and its buffer is killed;
ELSE with the `opencode-error' of a refusal, which is otherwise
reported.  Either is called once, in whatever buffer is current then."
  (let ((buffer (gethash id (opencode-sv opencode-session-buffers))))
    (opencode-request 'delete (concat "/api/session/" id)
                      :directory (opencode-state--directory id) :buffer nil
                      :then (lambda (_)
                              ;; The deletion event may have unregistered the buffer already.
                              (dolist (buffer (list buffer (gethash id (opencode-sv opencode-session-buffers))))
                                (when (buffer-live-p buffer) (kill-buffer buffer)))
                              (when then (funcall then)))
                      :else else)))

(defun opencode-delete-session (&optional id)
  "Delete session ID after confirmation.
ID defaults to this buffer's session; outside a session buffer, it is
picked among the current project's.  See `opencode-session-delete'."
  (interactive)
  (let* ((info (unless id (opencode--session-here "Delete session: ")))
         (id (or id (alist-get 'id info))))
    (when (yes-or-no-p (format "Delete the OpenCode session %s for good? "
                               (or (opencode--session-title id) (alist-get 'title info) id)))
      (opencode-session-delete id))))

(defun opencode-archive-session (&optional id)
  "Archive session ID after confirmation.
ID defaults to this buffer's session; outside a session buffer, it is
picked among the current project's.  See `opencode-session-archive'."
  (interactive)
  (let* ((info (unless id (opencode--session-here "Archive session: ")))
         (id (or id (alist-get 'id info))))
    (when (y-or-n-p (format "Archive the OpenCode session %s? "
                            (or (opencode--session-title id) (alist-get 'title info) id)))
      (opencode-session-archive id))))

(defun opencode-session-compact (id)
  "Compact the conversation of session ID; the session needs no buffer.
The service picks the model, the session's own unless the compaction
agent pins one, and steers a running turn: compaction runs at its next
step."
  (opencode-request 'post (format "/api/session/%s/compact" id)
                    :directory (opencode-state--directory id) :buffer nil
                    :body (opencode-empty-object)))

(defun opencode-compact-session ()
  "Compact this session's conversation."
  (interactive)
  (opencode--require-session)
  (opencode-session-compact opencode-session-id))

(defun opencode--block-message (overlay)
  "Return the ID of the message block OVERLAY belongs to, or nil.
Tool blocks record it; reasoning keys and notice keys name it."
  (let ((data (opencode-block-data overlay)))
    (or (plist-get data :message)
        (let ((key (plist-get data :key)))
          (and (stringp key) (string-match "\\`\\(msg_[^/]+\\)" key) (match-string 1 key))))))

(defun opencode-message-at (position)
  "Return the ID of the message rendered at POSITION, or nil."
  (or (get-text-property position 'opencode-message)
      (when-let* ((overlay (opencode-block-at position)))
        (opencode--block-message overlay))))

(defun opencode--conversation-position (position message)
  "Return where POSITION, showing MESSAGE, stands in the conversation's order.
The value is a list (PLACE RANK) that `value<' orders as the
conversation does.  Input typed here stays where it was typed, which a
turn streaming meanwhile may have left before output that comes first
in the conversation; the conversation places it where the transcript
ended when the service delivered it, after what was rendered before and
before what is rendered there since, so its RANK is its delivery's
count there.  Anything else is in place, ranked last at its position."
  (let ((delivered (and message (hash-table-p opencode--delivery-points)
                        (gethash message opencode--delivery-points))))
    (if (and delivered (eq (marker-buffer (car delivered)) (current-buffer))
             (< position (car delivered)))
        (list (marker-position (car delivered)) (cdr delivered))
      (list position most-positive-fixnum))))

(defun opencode--message-parts ()
  "Return (ORDER . MESSAGE-ID) of every rendered message part, in order.
Text carries its message as a property, blocks in their data.  ORDER is
the part's place in the conversation (see `opencode--conversation-position')."
  (let ((parts nil)
        (position (point-min))
        (end (opencode-pmark)))
    (while (< position end)
      (when-let* ((message (get-text-property position 'opencode-message)))
        (push (cons (opencode--conversation-position position message) message) parts))
      (setq position (next-single-property-change position 'opencode-message nil end)))
    (when (hash-table-p opencode--blocks)
      (maphash (lambda (_key overlay)
                 (when-let* (((overlay-buffer overlay))
                             (message (opencode--block-message overlay)))
                   (push (cons (opencode--conversation-position (overlay-start overlay) message) message)
                         parts)))
               opencode--blocks))
    (sort parts :key #'car)))

(defun opencode--prompt-parts ()
  "Return (ORDER . MESSAGE-ID) of every rendered prompt with a message, in order.
ORDER is as for `opencode--message-parts'."
  (let ((parts nil)
        (position (point-min))
        (end (opencode-pmark)))
    (while (< position end)
      (when-let* (((get-text-property position 'opencode-prompt))
                  (message (opencode-message-at position)))
        (push (cons (opencode--conversation-position position message) message) parts))
      (setq position (min (next-single-property-change position 'opencode-prompt nil end)
                          (next-single-property-change position 'opencode-message nil end))))
    (sort parts :key #'car)))

(defun opencode--message-boundary (position)
  "Return the message after which a fork or revert at POSITION cuts, or nil.
In a prompt the cut keeps the prompt and drops its answer, tool calls
included; in an answer it keeps the answer; at the live prompt it keeps
everything.  What follows is taken in the conversation's order, which
for input typed while a turn streamed is not the buffer's."
  (let* ((here (get-text-property position 'opencode-message))
         (prompt (get-text-property position 'opencode-prompt))
         (from (opencode--conversation-position position here)))
    (cond
     ((>= position (opencode-pmark)) nil)
     ((and here prompt)
      ;; The first message after the prompt starts its answer.
      (cdr (seq-find (lambda (part) (and (not (value< (car part) from)) (not (equal (cdr part) here))))
                     (opencode--message-parts))))
     (t
      ;; The next prompt starts the part to drop.
      (cdr (seq-find (lambda (part) (value< from (car part))) (opencode--prompt-parts)))))))

(defun opencode-session-fork (id &optional message-id)
  "Fork session ID into a new session and show it in the selected window.
The fork holds the whole conversation, or what precedes MESSAGE-ID.
Session ID needs no buffer."
  (opencode-request 'post (format "/api/session/%s/fork" id)
                    :directory (opencode-state--directory id) :buffer nil
                    :body (if message-id `((before . ,message-id)) (opencode-empty-object))
                    :then (lambda (response)
                            (opencode-session-open (opencode-request-data response)
                                                   :other-window nil))))

(defun opencode-fork-session ()
  "Fork this session through the content at point into a new session.
At the input, the fork holds the whole conversation."
  (interactive)
  (opencode--require-session)
  (opencode-session-fork opencode-session-id (opencode--message-boundary (point))))

(defun opencode-revert-to-point ()
  "Stage a revert dropping everything after the content at point.
The files changed since are restored; the next prompt commits the revert."
  (interactive)
  (opencode--require-session)
  (let ((boundary (or (opencode--message-boundary (point))
                      (user-error "Nothing to revert after point"))))
    (opencode-request 'post (format "/api/session/%s/revert/stage" opencode-session-id)
                      :body `((messageID . ,boundary) (files . t)))))

(defun opencode-revert-clear ()
  "Cancel the staged revert of this session."
  (interactive)
  (opencode--require-session)
  (opencode-request 'delete (format "/api/session/%s/revert" opencode-session-id)))

(defun opencode-open-parent ()
  "Open the parent of this session."
  (interactive)
  (opencode--require-session)
  (if-let* ((parent (alist-get 'parentID opencode-session-info)))
      (opencode-session-open-by-id parent)
    (user-error "This session has no parent")))

(defun opencode-scroll-to-last-response-start ()
  "Scroll to the start of the latest assistant text."
  (interactive)
  (unless opencode-last-response-start (user-error "No response yet"))
  (goto-char opencode-last-response-start)
  (recenter-top-bottom 0))

(defun opencode-yank-code-block ()
  "Copy the code of the fenced block at point."
  (interactive)
  (let* ((node (treesit-parent-until (treesit-node-at (point) 'markdown) "fenced_code_block" t))
         (content (and node (treesit-search-subtree node "code_fence_content"))))
    (unless node (user-error "Not in a fenced code block"))
    (if content
        (copy-region-as-kill (treesit-node-start content) (treesit-node-end content))
      (kill-new ""))))

(defun opencode--view-selected-session (&rest _)
  "Mark the session shown in the selected window of a focused frame viewed.
The read marker is the service's, so every client sees it read."
  (let ((buffer (window-buffer (selected-window))))
    (when-let* ((id (buffer-local-value 'opencode-session-id buffer))
                ((opencode-buffer-active-p buffer)))
      (opencode-with-service (buffer-local-value 'opencode--buffer-service buffer)
        (opencode-session-view id)))))

(defun opencode--selection-changed (&optional _frame)
  "Remember the selected session buffer and forget its idle alert.
The session shown in the selected window is marked viewed."
  (when opencode-session-id
    (setq opencode-alerted-sessions
          (seq-remove (lambda (info) (equal (alist-get 'id info) opencode-session-id))
                      opencode-alerted-sessions)
          opencode-last-session-buffer (current-buffer)))
  (opencode--view-selected-session))

(defun opencode--view-finished-turn (id _old new)
  "Mark session ID viewed when its turn ended, NEW, while its buffer is active."
  (when-let* (((memq new '(done failed)))
              (buffer (gethash id (opencode-sv opencode-session-buffers)))
              ((buffer-live-p buffer))
              ((opencode-buffer-active-p buffer)))
    (opencode-session-view id)))

(add-hook 'window-selection-change-functions #'opencode--selection-changed)
;; A session buffer also becomes active by being shown in the selected
;; window, or by its frame gaining focus.
(add-hook 'window-buffer-change-functions #'opencode--view-selected-session)
(add-function :after after-focus-change-function #'opencode--view-selected-session)
(add-hook 'opencode-state-change-functions #'opencode--view-finished-turn)

;;;; Navigating sessions

(defun opencode-visit-last-idle ()
  "Open the session that most recently finished while not displayed."
  (interactive)
  (if opencode-alerted-sessions
      (opencode-session-open (pop opencode-alerted-sessions) :other-window nil)
    (message "No idle sessions")))

(declare-function consult-line-multi "consult")

(defun opencode-consult-sessions ()
  "Search every open session buffer with `consult-line-multi'."
  (interactive)
  (unless (require 'consult nil t) (user-error "This command needs consult"))
  (consult-line-multi (list :predicate (lambda (buffer)
                                         (buffer-local-value 'opencode-session-id buffer)))))

;;;; Buffers rendered by earlier versions

(defun opencode--unhighlight-sent-prompts ()
  "Take from the prompts of this buffer what Comint gave them for the mouse.
Earlier versions left its highlight and its help on them; their links
keep their own."
  (when-let* ((process (get-buffer-process (current-buffer))))
    (let ((end (marker-position (process-mark process)))
          (pos (point-min)))
      (with-silent-modifications
        (while (< pos end)
          (let ((next (min (next-single-property-change pos 'opencode-prompt nil end)
                           (next-single-property-change pos 'button nil end))))
            (when (and (get-text-property pos 'opencode-prompt) (not (get-text-property pos 'button)))
              (remove-list-of-text-properties pos next '(mouse-face help-echo)))
            (setq pos next)))))))

;; Loaded over an earlier version, this one gives the session buffers
;; that version rendered its look.
(dolist (buffer (buffer-list))
  (when (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'opencode-session-mode)
    (with-current-buffer buffer
      (with-demoted-errors "OpenCode: cannot update the look of a session buffer: %S"
        (opencode--unhighlight-sent-prompts)
        (opencode-blocks-upgrade)
        (opencode-read-only-upgrade)))))

(provide 'opencode-session)
;;; opencode-session.el ends here
