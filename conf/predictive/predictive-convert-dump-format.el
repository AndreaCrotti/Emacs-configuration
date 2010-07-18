
;;; predictive-convert-dump-format.el --- convert dictionaries to new foramt


;; Copyright (C) 2009 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: predictive, completion
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is part of the Emacs Predictive Completion package.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


(defun predictive-convert-dump-format ()
  "Convert dictionary dump format from version 20 (and earlier)
of the predictive package to version 21 (i.e. from version < 0.17.x
and earlier of the \"predictive.el\" library to version 0.18)."
  (interactive)
  ;; convert format line by line
  (goto-char (point-min))
  (while (not (eobp))
    (unless (eolp) (forward-sexp))
    ;; convert null weight to 0
    (cond
     ((eolp)) ; (insert " 0"))
     ((looking-at "[[:space:]]+[[:digit:]]+\\([[:space:]]\\|$\\)")
      (forward-sexp))
     ((looking-at "[[:space:]]+\\(nil\\)\\([[:space:]]\\|$\\)")
      (replace-match "0" nil nil nil 1)))
    ;; convert prefix list format
    (unless (or (eolp)
		(looking-at "[[:space:]]+(:prefixes[[:space:]]")
		(null (search-forward "(" (line-end-position) t)))
      (backward-char)
      (insert "(:prefixes ")
      (goto-char (line-end-position))
      (insert ")"))
    (forward-line))
  ;; save converted file
  (save-buffer))
