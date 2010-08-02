;;; ob-keys.el --- key bindings for org-babel

;; Copyright (C) 2009, 2010  Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.01trans

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add org-babel keybindings to the org-mode keymap for exposing
;; org-babel functions.  These will all share a common prefix.  See
;; the value of `org-babel-key-bindings' for a list of interactive
;; functions and their associated keys.

;;; Code:
(require 'ob)

(defvar org-babel-key-prefix "\C-c\C-v"
  "The key prefix for Babel interactive key-bindings.
See `org-babel-key-bindings' for the list of interactive babel
functions which are assigned key bindings, and see
`org-babel-map' for the actual babel keymap.")

(defvar org-babel-map (make-sparse-keymap)
  "The keymap for interactive Babel functions.")

;;;###autoload
(defun org-babel-describe-bindings ()
  "Describe all keybindings behind `org-babel-key-prefix'."
  (interactive)
  (describe-bindings org-babel-key-prefix))

(defvar org-babel-key-bindings
  '(("p" . org-babel-previous-src-block)
    ("\C-p" . org-babel-previous-src-block)
    ("n" . org-babel-next-src-block)
    ("\C-n" . org-babel-next-src-block)
    ("e" . org-babel-execute-src-block)
    ("\C-e" . org-babel-execute-src-block)
    ("o" . org-babel-open-src-block-result)
    ("\C-o" . org-babel-open-src-block-result)
    ("\C-v" . org-babel-expand-src-block)
    ("v" . org-babel-expand-src-block)
    ("g" . org-babel-goto-named-src-block)
    ("r" . org-babel-goto-named-result)
    ("\C-r" . org-babel-goto-named-result)
    ("\C-b" . org-babel-execute-buffer)
    ("b" . org-babel-execute-buffer)
    ("\C-s" . org-babel-execute-subtree)
    ("s" . org-babel-execute-subtree)
    ("\C-t" . org-babel-tangle)
    ("t" . org-babel-tangle)
    ("\C-f" . org-babel-tangle-file)
    ("f" . org-babel-tangle-file)
    ("\C-l" . org-babel-load-in-session)
    ("l" . org-babel-load-in-session)
    ("\C-i" . org-babel-lob-ingest)
    ("i" . org-babel-lob-ingest)
    ("\C-z" . org-babel-switch-to-session)
    ("z" . org-babel-switch-to-session)
    ("\C-a" . org-babel-sha1-hash)
    ("a" . org-babel-sha1-hash)
    ("h" . org-babel-describe-bindings))
  "Alist of key bindings and interactive Babel functions.
This list associates interactive Babel functions
with keys.  Each element of this list will add an entry to the
`org-babel-map' using the letter key which is the `car' of the
a-list placed behind the generic `org-babel-key-prefix'.")

(provide 'ob-keys)

;; arch-tag: 01e348ee-4906-46fa-839a-6b7b6f989048

;;; ob-keys.el ends here
