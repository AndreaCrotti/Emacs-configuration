;;; bookmark+.el --- Bookmark+: extensions to standard library `bookmark.el'.
;;
;; Filename: bookmark+.el
;; Description: Bookmark+: extensions to standard library `bookmark.el'.
;; Author: Drew Adams, Thierry Volpiatto
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2000-2010, Drew Adams, all rights reserved.
;; Copyright (C) 2009, Thierry Volpiatto, all rights reserved.
;; Created: Fri Sep 15 07:58:41 2000
;; Last-Updated: Sat Jul 17 10:21:11 2010 (-0700)
;;           By: dradams
;;     Update #: 14939
;; URL: http://www.emacswiki.org/cgi-bin/wiki/bookmark+.el
;; Keywords: bookmarks, bookmark+, placeholders, annotations, search, info, url, w3m, gnus
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `bookmark', `bookmark+', `bookmark+-1', `bookmark+-bmu',
;;   `bookmark+-lit', `ffap', `pp'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Bookmark+: extensions to standard library `bookmark.el'.
;;
;;    The Bookmark+ libraries are these:
;;
;;    `bookmark+.el'     - main (driver) library (this file)
;;    `bookmark+-bmu.el' - code for the `*Bookmark List*' (bmenu)
;;    `bookmark+-1.el'   - other required code (non-bmenu)
;;    `bookmark+-lit.el' - (optional) code for highlighting bookmarks
;;    `bookmark+-doc.el' - documentation (comment-only file)
;;    `bookmark+-chg.el' - change log (comment-only file)
;;
;;    The documentation (in `bookmark+-doc.el') includes how to
;;    byte-compile and install Bookmark+.  The documentation is also
;;    available in these ways:
;;
;;    1. From the bookmark list (`C-x r l'):
;;       Use `?' to show the current bookmark-list status and general
;;       help, then click link `Doc in Commentary' or link `Doc on the
;;       Web'.
;;
;;    2. From the Emacs-Wiki Web site:
;;       http://www.emacswiki.org/cgi-bin/wiki/BookmarkPlus.
;;    
;;    3. From the Bookmark+ group customization buffer:
;;       `M-x customize-group bookmark-plus', then click link
;;       `Commentary'.
;;
;;    (The commentary links in #1 and #3 work only if you have library
;;    `bookmark+-doc.el' in your `load-path'.)
;;
;;
;;    ****** NOTE ******
;;
;;      On 2010-06-18, I changed the prefix used by package Bookmark+
;;      from `bookmarkp-' to `bmkp-'.  THIS IS AN INCOMPATIBLE CHANGE.
;;      I apologize for the inconvenience, but the new prefix is
;;      preferable for a number of reasons, including easier
;;      distinction from standard `bookmark.el' names.
;;
;;      This change means that YOU MUST MANUALLY REPLACE ALL
;;      OCCURRENCES of `bookmarkp-' by `bmkp-' in the following
;;      places, if you used Bookmark+ prior to this change:
;;
;;      1. In your init file (`~/.emacs') or your `custom-file', if
;;         you have one.  This is needed if you customized any
;;         Bookmark+ features.
;;
;;      2. In your default bookmark file, `bookmark-default-file'
;;         (`.emacs.bmk'), and in any other bookmark files you might
;;         have.
;;
;;      3. In your `*Bookmark List*' state file,
;;         `bmkp-bmenu-state-file' (`~/.emacs-bmk-bmenu-state.el').
;;
;;      4. In your `*Bookmark List*' commands file,
;;         `bmkp-bmenu-commands-file' (`~/.emacs-bmk-bmenu-commands.el'),
;;         if you have one.
;;
;;      Again, sorry for this inconvenience.
;;
;;
;;  Commands defined here:
;;
;;    `bmkp-version'.
;;
;;  Internal variables defined here:
;;
;;    `bmkp-version-number'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;

(require 'bookmark)                     ; Vanilla Emacs.
(require 'bookmark+-lit nil t)          ; Optional (soft require) - no error if not found.  If you do
                                        ; not want to use `bookmark+-lit.el' then simply do not put
                                        ; it in your `load-path'.
(require 'bookmark+-bmu)                ; `*Bookmark List*' stuff.
(require 'bookmark+-1)                  ; Rest of Bookmark+ required stuff.

(provide 'bookmark+)                    ; Ensure this library is loaded before we compile it.
(require 'bookmark+)                    ; So be sure to put this library in your `load-path' before
                                        ; trying to byte-compile it.

;;;;;;;;;;;;;;;;;;;;;;;

(defconst bmkp-version-number "3.1.1")

(defun bmkp-version ()
  "Show version number of library `bookmark+.el'."
  (interactive)
  (message "Bookmark+, version %s" bmkp-version-number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmark+.el ends here
