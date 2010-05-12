;;; semantic-html.el --- Semantic details for html files

;;; Copyright (C) 2004, 2005, 2007, 2008, 2009, 2010 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; X-RCS: $Id: semantic-html.el,v 1.14 2010/02/20 20:52:11 zappo Exp $

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Parse HTML files and organize them in a nice way.
;; Pay attention to anchors, including them in the tag list.
;;
;; Copied from the original semantic-texi.el.
;;
;; ToDo: Find <script> tags, and parse the contents in other
;; parsers, such as javascript, php, shtml, or others.

(require 'semantic)
(require 'semantic-format)
(condition-case nil
    ;; This is not installed in all versions of Emacs.
    (require 'sgml-mode) ;; html-mode is in here.
  (error
   (require 'psgml-mode) ;; XEmacs uses psgml, and html-mode is in here.
   ))

;;; Code:
(eval-when-compile
  (require 'semantic-ctxt)
  (require 'semantic-imenu)
  (require 'senator))

(defvar semantic-html-super-regex
  "<\\(h[1-9]\\|title\\|script\\|body\\|a +href\\)\\>"
  "Regular expression used to find special sections in an HTML file.")

(defvar semantic-html-section-list
  '(("title" 1)
    ("script" 1)
    ("body" 1)
    ("a" 11)
    ("h1" 2)
    ("h2" 3)
    ("h3" 4)
    ("h4" 5)
    ("h5" 6)
    ("h6" 7)
    ("h7" 8)
    ("h8" 9)
    ("h9" 10)
    )
  "Alist of sectioning commands and their relative level.")

(define-mode-local-override semantic-parse-region
  html-mode (&rest ignore)
  "Parse the current html buffer for semantic tags.
INGNORE any arguments.  Always parse the whole buffer.
Each tag returned is of the form:
 (\"NAME\" section (:members CHILDREN))
or
 (\"NAME\" anchor)"
  (mapcar 'semantic-html-expand-tag
	  (semantic-html-parse-headings)))

(define-mode-local-override semantic-parse-changes
  html-mode ()
  "We can't parse changes for HTML mode right now."
  (semantic-parse-tree-set-needs-rebuild))

(defun semantic-html-expand-tag (tag)
  "Expand the HTML tag TAG."
  (let ((chil (semantic-html-components tag)))
    (if chil
        (semantic-tag-put-attribute
         tag :members (mapcar 'semantic-html-expand-tag chil)))
    (car (semantic--tag-expand tag))))

(defun semantic-html-components (tag)
  "Return components belonging to TAG."
  (semantic-tag-get-attribute tag :members))

(defun semantic-html-parse-headings ()
  "Parse the current html buffer for all semantic tags."
  (let ((pass1 nil))
    ;; First search and snarf.
    (save-excursion
      (goto-char (point-min))
      (working-status-forms (file-name-nondirectory buffer-file-name) "done"
	(while (re-search-forward semantic-html-super-regex nil t)
	  (setq pass1 (cons (match-beginning 0) pass1))
	  (working-status)
	  )
	(working-status t)))
    (setq pass1 (nreverse pass1))
    ;; Now, make some tags while creating a set of children.
    (car (semantic-html-recursive-combobulate-list pass1 0))
    ))

(defun semantic-html-set-endpoint (metataglist pnt)
  "Set the end point of the first section tag in METATAGLIST to PNT.
METATAGLIST is a list of tags in the intermediate tag format used by the
html parser.  PNT is the new point to set."
  (let ((metatag nil))
    (while (and metataglist
		(not (eq (semantic-tag-class (car metataglist)) 'section)))
      (setq metataglist (cdr metataglist)))
    (setq metatag (car metataglist))
    (when metatag
      (setcar (nthcdr (1- (length metatag)) metatag) pnt)
      metatag)))

(defsubst semantic-html-new-section-tag (name members level start end)
  "Create a semantic tag of class section.
NAME is the name of this section.
MEMBERS is a list of semantic tags representing the elements that make
up this section.
LEVEL is the levelling level.
START and END define the location of data described by the tag."
  (let ((anchorp (eq level 11)))
    (append (semantic-tag name
			  (cond (anchorp 'anchor)
				(t 'section))
			  :members members)
	    (list start (if anchorp (point) end)) )))

(defun semantic-html-extract-section-name ()
  "Extract a section name from the current buffer and point.
Assume the cursor is in the tag representing the section we
need the name from."
  (save-excursion
    ; Skip over the HTML tag.
    (forward-sexp -1)
    (forward-char -1)
    (forward-sexp 1)
    (skip-chars-forward "\n\t ")
    (while (looking-at "<")
      (forward-sexp 1)
      (skip-chars-forward "\n\t ")
      )
    (let ((start (point))
	  (end nil))
      (if (re-search-forward "</" nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (skip-chars-backward " \n\t")
	    (setq end (point))
	    (buffer-substring-no-properties start end))
	""))
    ))

(defun semantic-html-recursive-combobulate-list (sectionlist level)
  "Rearrange SECTIONLIST to be a hierarchical tag list starting at LEVEL.
Return the rearranged new list, with all remaining tags from
SECTIONLIST starting at ELT 2.  Sections not are not dealt with as soon as a
tag with greater section value than LEVEL is found."
  (let ((newl nil)
	(oldl sectionlist)
	(case-fold-search t)
        tag
	)
    (save-excursion
      (catch 'level-jump
	(while oldl
	  (goto-char (car oldl))
	  (if (looking-at "<\\(\\w+\\)")
	      (let* ((word (match-string 1))
		     (levelmatch (assoc-ignore-case
                                  word semantic-html-section-list))
		     text begin tmp
		     )
		(when (not levelmatch)
		  (error "Tag %s matched in regexp but is not in list"
			 word))
		;; Set begin to the right location
		(setq begin (point))
		;; Get out of here if there if we made it that far.
		(if (and levelmatch (<= (car (cdr levelmatch)) level))
		    (progn
		      (when newl
			(semantic-html-set-endpoint newl begin))
		      (throw 'level-jump t)))
		;; When there is a match, the descriptive text
		;; consists of the rest of the line.
		(goto-char (match-end 1))
		(skip-chars-forward " \t")
		(setq text (semantic-html-extract-section-name))
		;; Next, recurse into the body to find the end.
		(setq tmp (semantic-html-recursive-combobulate-list
			   (cdr oldl) (car (cdr levelmatch))))
		;; Build a tag
		(setq tag (semantic-html-new-section-tag
			   text (car tmp) (car (cdr levelmatch)) begin (point-max)))
		;; Before appending the newtag, update the previous tag
		;; if it is a section tag.
		(when newl
		  (semantic-html-set-endpoint newl begin))
		;; Append new tag to our master list.
		(setq newl (cons tag newl))
		;; continue
		(setq oldl (cdr tmp))
		)
	    (error "Problem finding section in semantic/html parser"))
	  ;; (setq oldl (cdr oldl))
	  )))
    ;; Return the list
    (cons (nreverse newl) oldl)))

(define-mode-local-override semantic-sb-tag-children-to-expand
  html-mode (tag)
  "The children TAG expands to."
  (semantic-html-components tag))

;;;###autoload
(defun semantic-default-html-setup ()
  "Set up a buffer for parsing of HTML files."
  ;; This will use our parser.
  (setq semantic-parser-name "HTML"
        semantic--parse-table t
        imenu-create-index-function 'semantic-create-imenu-index
	semantic-command-separation-character ">"
	semantic-type-relation-separator-character '(":")
	semantic-symbol->name-assoc-list '((section . "Section")
					   )
	semantic-imenu-expandable-tag-classes '(section)
	semantic-imenu-bucketize-file nil
	semantic-imenu-bucketize-type-members nil
	senator-step-at-start-end-tag-classes '(section)
	senator-step-at-tag-classes '(section)
	semantic-stickyfunc-sticky-classes '(section)
	)
  (semantic-install-function-overrides
   '((tag-components . semantic-html-components)
     )
   t)
  )

;;;###autoload
(add-hook 'html-mode-hook 'semantic-default-html-setup)

(define-child-mode html-helper-mode html-mode
  "`html-helper-mode' needs the same semantic support as `html-mode'.")

(provide 'semantic-html)

;;; semantic-html.el ends here
