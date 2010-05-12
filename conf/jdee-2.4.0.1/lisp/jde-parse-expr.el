;;; jde-parse-expr.el -- Beanshell JDEE integration.
;; $Id: jde.el 127 2009-08-12 08:22:57Z paullandes $

;; Author: Paul Landes <landes <at> mailc dot net>
;; Maintainer: Paul Landes
;; Keywords: java, tools, parse expression

;; Copyright (C) 2009 by Paul Landes

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This library has beanshell specific functionality.  Most of it was taken
;; from jde.el and placed here in an attempt to make jde.el a little skinner
;; and make a home for beanshell specific code.

;;; Code:

(require 'thingatpt)

(defun jde-beginning-of-expression ()
  (c-beginning-of-statement))
(put 'java-expression 'beginning-op 'jde-beginning-of-expression)

(defun jde-end-of-expression ()
  (c-end-of-statement))
(put 'java-expression 'end-op 'jde-end-of-expression)

;;;###autoload
(defun jde-hungarian-to-reverse-camel-notation (start end local-replacement-p)
  "\
Converts Hungarian (i.e. m_sMyApp) to reverse camel notation (i.e. myApp).
Hungarian notation is used, for example, in most Microsoft visual C++ code
whereas reverse camel notation is used as the Sun Java standard style.

START the start of the buffer region
END is the end of the buffer region
LOCAL-REPLACEMENT-P, if non-nil, replace expecting a local variable
replacement.  This adds a `this.' to each replacment."
  (interactive
   (if (not mark-active)
       (error "No region selected")
     (list (region-beginning) (region-end) current-prefix-arg)))
  (let ((case-fold-search nil)
	(reg "\\(?:oa\\|[onbfs]\\)\\([A-Z]\\)\\([a-zA-Z0-9]+\\)"))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(save-match-data
	  (goto-char (point-min))
	  (while (re-search-forward (concat "m_" reg) nil t)
	    (replace-match (concat (and local-replacement-p "this.")
				   (downcase (match-string 1))
				   (match-string 2))))
	  (goto-char (point-min))
	  (while (re-search-forward (concat "\\<" reg "\\>") nil t)
	    (replace-match (concat (downcase (match-string 1))
				   (match-string 2)))))))))

(defun jde-split-by-camel-notation (to-parse)
  "Parse tokens based on \(reverse) camel notation.
TO-PARSE is the string to parse."
  (if (or nil (= 0 (length to-parse)))
      nil
    (let ((last-cap 0)
	  toks)
      (setq to-parse (append (vconcat to-parse) nil))
      (flet ((upperp
	      (pos)
	      (if (<= pos 0)
		  t
		(let ((char (elt to-parse pos)))
		  (and (eq char (upcase char))))))
	     (handle
	      (pos)
	      (prog1
		  (apply 'string (subseq to-parse last-cap pos))
		(setq last-cap pos))))
	(do ((pos 0 (incf pos)))
	    ((> pos (1- (length to-parse))))
	  (if (and (upperp pos)
		   ;;(> (- pos last-cap) 2))
		   (> (- pos last-cap) 0))
	      (setq toks (append toks (cons (handle pos) nil)))))
	(append toks (cons (handle (length to-parse)) nil))))))

(defconst jde-camel-tok-skip-chars " \t\n().'\""
  "Characters used top a traveral of a reverse camel notation string." )

(defun jde-beginning-of-camel-tok ()
  "Go to the beginning of a reverese camel case token."
  (interactive)
  (let ((space-regex (format "[%s]" jde-camel-tok-skip-chars))
	(n-space-regex (format "[^%s]" jde-camel-tok-skip-chars))
	start end reg toks)
    (save-excursion
      (save-match-data
	(if (and (> (point) (- 2 (point-min)))
		 (save-excursion
		   (forward-char -1)
		   (looking-at space-regex)))
	    (re-search-backward n-space-regex nil t))
	(setq end (point))
	(setq start (or (when (re-search-backward space-regex nil t)
			  (forward-char 1)
			  (point))
			(point-min)))))
    (setq reg (buffer-substring start end)
	  toks (jde-split-by-camel-notation reg))
    (goto-char (- end (length (car (last toks)))))))

(defun jde-end-of-camel-tok ()
  "Go to the beginning of a reverese camel case token."
  (interactive)
  (let ((space-regex (format "[%s]" jde-camel-tok-skip-chars))
	(n-space-regex (format "[^%s]" jde-camel-tok-skip-chars)))
    (if (looking-at space-regex) (re-search-forward n-space-regex))
    (let ((start (point))
	  (end (or (save-excursion
		     (save-match-data
		       (re-search-forward space-regex nil t)))
		   (point-max)))
	  reg toks)
      (setq reg (buffer-substring start end)
	    toks (jde-split-by-camel-notation reg))
      (goto-char (+ start (length (car toks)))))))

(defun jde-forward-camel-tok (arg)
  "Go forward a reverse camel case token."
  (interactive "p")
  (dotimes (i arg) (jde-end-of-camel-tok)))

(defun jde-backward-camel-tok (arg)
  "Go back a reverse camel case token."
  (interactive "p")
  (dotimes (i arg) (jde-beginning-of-camel-tok)))

(defun jde-kill-camel-tok ()
  "Kill a reverse camel case token."
  (interactive)
  (delete-region (point) (save-excursion (jde-end-of-camel-tok))))

(defun jde-backward-kill-camel-tok ()
  "Go back a reverse camel case token and kill."
  (interactive)
  (delete-region (save-excursion (jde-beginning-of-camel-tok)) (point)))

(defun jde-map-camel-notation-token (start end iter-fn delimiter)
  "Helper method to remove and add back tokesn with a delimiter.
Operates on the current buffer.

START the beginning of the region.
END the end of the region.

ITER-FN the function used to map over to create the string to add
back after the deletion.  This is usually something like
`upcaes'.  The function takes the camel token.

DELIMITER the delimiter used to place between each camel token."
  (let ((toks (jde-split-by-camel-notation
	       (buffer-substring start end))))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert (mapconcat iter-fn toks delimiter)))))

(defun jde-parse-expr-get-region-or-thing (thing)
  (if mark-active
      (list (region-beginning) (region-end))
    (let ((cell (bounds-of-thing-at-point thing)))
      (list (car cell) (cdr cell)))))

;;;###autoload
(defun jde-camel-to-sql (start end)
  "Convert from reverse camel notation to the SQL underscore convention."
  (interactive (jde-parse-expr-get-region-or-thing 'word))
  (jde-map-camel-notation-token start end 'downcase "_"))

;;;###autoload
(defun jde-camel-to-c-const (start end)
  "Convert from reverse camel notation to the C constant convention."
  (interactive (jde-parse-expr-get-region-or-thing 'word))
  (jde-map-camel-notation-token start end 'upcase "_"))

;;;###autoload
(defun jde-camel-to-lisp (start end)
  "Convert from reverse camel notation to the lisp symbol convention."
  (interactive (jde-parse-expr-get-region-or-thing 'word))
  (jde-map-camel-notation-token start end 'downcase "-"))

(defun jde-camel-to-java-property (start end)
  "Convert from reverse camel notation to the Java properties convention."
  (interactive (jde-parse-expr-get-region-or-thing 'symbol))
  (let ((str (buffer-substring start end)))
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert (downcase (replace-regexp-in-string "_" "." str nil t))))))

;;;###autoload
(defun jde-member-to-attribute (member-name getterp)
  "Convert Java member to a getter or setter syntax.

MEMBER-NAME is member to convret.  This is taken as the current word at point
when called interactively.

GETTERP, if non-nil, make it a getter, otherwise make it a setter.  If
\\[universal-argument] is used while calling interactively, then make it a
setter, otherwise, make a getter."
  (interactive (list (thing-at-point 'word) (not current-prefix-arg)))
  (let* ((toks (jde-split-by-camel-notation member-name))
	 (attr (apply 'concat
		      (append (list (if getterp "get" "set")
				    (capitalize (substring (car toks) 0 1))
				    (substring (car toks) 1))
			      (cdr toks)))))
    (if (interactive-p)
	(let ((bounds (bounds-of-thing-at-point 'word)))
	  (delete-region (car bounds) (cdr bounds))
	  (insert (concat attr "(" (if getterp ")")))))
    attr))

(eval-after-load
    "jde"
  '(progn
     ;; wait until jde-mode-map is defined in jde.el

     ;; clobbers `kill-word'
     (define-key jde-mode-map "\M-d" 'jde-kill-camel-tok)
     ;; clobbers `backward-kill-word'
     (define-key jde-mode-map [M-backspace] 'jde-backward-kill-camel-tok)))

(provide 'jde-parse-expr)

;; End of jde-parse-expr.el
