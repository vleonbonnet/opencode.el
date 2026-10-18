;;; opencode.el --- Emacs client for OpenCode 2  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann
;; Copyright (C) 2026  Valentin Leon

;; Author: Scott Zimmermann <sczi@disroot.org>
;;         Valentin Leon <valentin@leon.click>
;; Maintainer: Valentin Leon <valentin@leon.click>
;; Keywords: tools, llm, opencode
;; Version: 2.0.0
;; Package-Requires: ((emacs "31.1") (plz "0.9") (transient "0.7"))
;; URL: https://github.com/vleonbonnet/opencode.el

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

;; A Comint-based client for the OpenCode 2 server: one buffer per
;; session, with collapsible reasoning and tool blocks, deferred and
;; resumable permission requests and questions, a prompt-cache guard,
;; and post-edit processing of the files the agent changed.  It connects
;; to the shared background service the TUI and desktop app use, starting
;; it when needed, and never stops it.
;;
;; Derived from sczi's opencode.el for OpenCode 1.

;;; Code:

(require 'opencode-common)
(require 'opencode-service)
(require 'opencode-http)
(require 'opencode-events)
(require 'opencode-render)
(require 'opencode-links)
(require 'opencode-blocks)
(require 'opencode-notify)
(require 'opencode-session)
(require 'opencode-requests)
(require 'opencode-permission)
(require 'opencode-form)
(require 'opencode-modeline)
(require 'opencode-sessions)
(require 'opencode-edits)
(require 'opencode-desktop)
(require 'opencode-convert)

;; Entry points load this file, so that every module is in place.

;;;###autoload
(autoload 'opencode "opencode" "List the sessions of the current project." t)
;;;###autoload
(autoload 'opencode-list-all-sessions "opencode" "List the sessions of every project." t)
;;;###autoload
(autoload 'opencode-select-project "opencode" "List the sessions of a project." t)
;;;###autoload
(autoload 'opencode-select-session "opencode" "Open a session of the current project." t)
;;;###autoload
(autoload 'opencode-new-session "opencode" "Create a session in the current project." t)
;;;###autoload
(autoload 'opencode-new-worktree "opencode" "Create a worktree and a session in it." t)
;;;###autoload
(autoload 'opencode-select-open-session "opencode" "Switch to an open session buffer." t)
;;;###autoload
(autoload 'opencode-visit-last-idle "opencode" "Open the session that last finished unseen." t)
;;;###autoload
(autoload 'opencode-select-idle "opencode" "Open a session that finished unseen." t)
;;;###autoload
(autoload 'opencode-consult-sessions "opencode" "Search every open session buffer." t)
;;;###autoload
(autoload 'opencode-desktop-restore "opencode")

;;;###autoload
(with-eval-after-load 'desktop
  (defvar desktop-buffer-mode-handlers)
  (dolist (mode '(opencode-session-mode opencode-desktop-pending-mode))
    (setf (alist-get mode desktop-buffer-mode-handlers) #'opencode-desktop-restore)))

(provide 'opencode)
;;; opencode.el ends here
