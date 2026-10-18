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
(require 'opencode-session)

(provide 'opencode)
;;; opencode.el ends here
