;;; ecomplete.el --- electric completion of addresses and the like

;; Copyright (C) 2006, 2007, 2008, 2009, 2010  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: mail

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

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-when-compile
  (unless (fboundp 'with-no-warnings)
    (defmacro with-no-warnings (&rest body)
      `(progn ,@body))))

(defgroup ecomplete nil
  "Electric completion of email addresses and the like."
  :group 'mail)

(defcustom ecomplete-database-file "~/.ecompleterc"
  "*The name of the file to store the ecomplete data."
  :group 'ecomplete
  :type 'file)

(defcustom ecomplete-database-file-coding-system 'iso-2022-7bit
  "Coding system used for writing the ecomplete database file."
  :type '(symbol :tag "Coding system")
  :group 'ecomplete)

;;; Internal variables.

(defvar ecomplete-database nil)

;;;###autoload
(defun ecomplete-setup ()
  (when (file-exists-p ecomplete-database-file)
    (with-temp-buffer
      (let ((coding-system-for-read ecomplete-database-file-coding-system))
	(insert-file-contents ecomplete-database-file)
	(setq ecomplete-database (read (current-buffer)))))))

(defun ecomplete-add-item (type key text)
  (let ((elems (assq type ecomplete-database))
	(now (string-to-number
	      (format "%.0f" (if (and (fboundp 'float-time)
				      (subrp (symbol-function 'float-time)))
				 (float-time)
			       (with-no-warnings
				 (time-to-seconds (current-time)))))))
	entry)
    (unless elems
      (push (setq elems (list type)) ecomplete-database))
    (if (setq entry (assoc key (cdr elems)))
	(setcdr entry (list (1+ (cadr entry)) now text))
      (nconc elems (list (list key 1 now text))))))

(defun ecomplete-get-item (type key)
  (assoc key (cdr (assq type ecomplete-database))))

(defun ecomplete-save ()
  (with-temp-buffer
    (let ((coding-system-for-write ecomplete-database-file-coding-system))
      (insert "(")
      (loop for (type . elems) in ecomplete-database
	    do
	    (insert (format "(%s\n" type))
	    (dolist (entry elems)
	      (prin1 entry (current-buffer))
	      (insert "\n"))
	    (insert ")\n"))
      (insert ")")
      (write-region (point-min) (point-max)
		    ecomplete-database-file nil 'silent))))

(defun ecomplete-get-matches (type match)
  (let* ((elems (cdr (assq type ecomplete-database)))
	 (match (regexp-quote match))
	 (candidates
	  (sort 
	   (loop for (key count time text) in elems
		 when (string-match match text)
		 collect (list count time text))
	   (lambda (l1 l2)
	     (> (car l1) (car l2))))))
    (when (> (length candidates) 10)
      (setcdr (nthcdr 10 candidates) nil))
    (unless (zerop (length candidates))
      (with-temp-buffer
	(dolist (candidate candidates)
	  (insert (caddr candidate) "\n"))
	(goto-char (point-min))
	(put-text-property (point) (1+ (point)) 'ecomplete t)
	(while (re-search-forward match nil t)
	  (put-text-property (match-beginning 0) (match-end 0)
			     'face 'isearch))
	(buffer-string)))))

(defun ecomplete-display-matches (type word &optional choose)
  (let* ((matches (ecomplete-get-matches type word))
	 (line 0)
	 (max-lines (when matches (- (length (split-string matches "\n")) 2)))
	 (message-log-max nil)
	 command highlight)
    (if (not matches)
	(progn
	  (message "No ecomplete matches")
	  nil)
      (if (not choose)
	  (progn
	    (message "%s" matches)
	    nil)
	(setq highlight (ecomplete-highlight-match-line matches line))
	(while (not (memq (setq command (read-event highlight)) '(? return)))
	  (cond
	   ((eq command ?\M-n)
	    (setq line (min (1+ line) max-lines)))
	   ((eq command ?\M-p)
	    (setq line (max (1- line) 0))))
	  (setq highlight (ecomplete-highlight-match-line matches line)))
	(when (eq command 'return)
	  (nth line (split-string matches "\n")))))))

(defun ecomplete-highlight-match-line (matches line)
  (with-temp-buffer
    (insert matches)
    (goto-char (point-min))
    (forward-line line)
    (save-restriction
      (narrow-to-region (point) (point-at-eol))
      (while (not (eobp))
	;; Put the 'region face on any charactes on this line that
	;; aren't already highlighted.
	(unless (get-text-property (point) 'face)
	  (put-text-property (point) (1+ (point)) 'face 'highlight))
	(forward-char 1)))
    (buffer-string)))

(provide 'ecomplete)

;; arch-tag: 34622935-bb81-4711-a600-57b89c2ece72
;;; ecomplete.el ends here
