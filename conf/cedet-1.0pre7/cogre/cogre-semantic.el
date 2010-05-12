;;; cogre-semantic.el --- Convert Semantic TAGS to COGRE nodes.
;;
;; Copyright (C) 2009 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: cogre-semantic.el,v 1.9 2009/04/23 03:28:09 zappo Exp $
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Define a set of Semantic COGRE element peers based on Semantic objects.
;;
;; Convert tags into COGRE nodes, or update existing COGRE nodes
;; with data from TAGS.
;;
;; Used for cut/paste, or updating graphs linked to code from changes
;; made in the code.
;;
;; To convert nodes into tags, see `cogre-convert.el'.

(require 'semantic)
(require 'cogre-srecode)
(require 'cogre)

;;; Code:

;;; GRAPH PEERS
;;
;; The graph peer for Semantic will contain information needed for
;; code generation, such as files and what-not.
(defclass cogre-peer-project-semantic (cogre-element-peer)
  (
   )
  "Peer for graph objects intended for use with Semantic element peers.")

;;; TAG PEERS
;;
;; The peers can represent a Semantic tag, and keep it up to date.

(defclass cogre-peer-semantic (cogre-element-peer)
  ((tag :initarg :tag
	:initform nil
	:type (or null semantic-tag)
	:documentation
	"The Semantic Tag managed by this peer.")
   )
  "A peer containing a Semantic class.")

(defmethod cogre-peer-source-file ((peer cogre-peer-semantic))
  "Does this peer have a source file?"
  (with-slots (tag) peer
    (semantic-tag-file-name tag)))

(defclass cogre-peer-semantic-class (cogre-peer-semantic)
  (
   )
  "A peer containing a Semantic class.")

(defun cogre-refresh-tag (tag)
  "Refresh TAG from sources.
Goes to the original location of TAG, and tries to re-find that tag."
  (save-excursion
    (semantic-go-to-tag tag)
    ;; Force a refresh if needed.
    (semantic-fetch-tags)
    ;; Make sure we find the original.
    (let ((newtag (semantic-current-tag))
	  (replace nil))
      ;; Are they basically the same?
      (if (semantic-tag-similar-p tag newtag)
	  (setq replace t)
	;; Maybe we missed because the code moved around?
	(let ((tagsearch (semantic-deep-find-tags-by-name (semantic-tag-name tag))))
	  (setq tagsearch
		(semantic--find-tags-by-function
		 (lambda (T) (semantic-tag-similar-p T tag))
		 tagsearch))
	  (setq newtag (car tagsearch)))
	(when (and newtag (semantic-tag-similar-p tag newtag))
	  (setq replace t)))
      ;; Make a copy of this new tag, and return it.
      (when newtag
	(semantic-tag-copy newtag nil t)))))

(defmethod cogre-peer-update-from-source ((peer cogre-peer-semantic-class) node)
  "Update the PEER object, and NODE from environment."
  (let* ((tag (oref peer tag))
	 (newtag (cogre-refresh-tag tag))
	 )

    (when newtag
      (oset peer :tag (semantic-tag-copy newtag nil t))
      (setq tag (oref peer :tag)))

    ;; Update node based one what we learned.
    (let ((slots (semantic-tag-type-members tag))
	  (extmeth (semantic-tag-external-member-children tag t))
	  attrib method)
      ;; Bin them up
      (while slots
	(cond
	 ;; A plain string, a simple language, just do attributes.
	 ((stringp (car slots))
	  (setq attrib (cons (list (car slots) 'variable nil)
			     attrib))
	  )
	 ;; Variable decl is an attribute
	 ((eq (semantic-tag-class (car slots)) 'variable)
	  (setq attrib (cons (semantic-tag-copy (car slots) nil t) attrib)))
	 ;; A function decle is a method.
	 ((eq (semantic-tag-class (car slots)) 'function)
	  (setq method (cons (semantic-tag-copy (car slots) nil t) method)))
	 )
	(setq slots (cdr slots)))

      ;; Add in all those extra methods	
      (when (semanticdb-find-results-p extmeth)
	(setq extmeth (semanticdb-strip-find-results extmeth t)))
      (while extmeth
	(when (eq (semantic-tag-class (car extmeth)) 'function)
	  (setq method (cons (semantic-tag-copy (car extmeth) nil t) method)))
	(setq extmeth (cdr extmeth)))

      ;; Put them into the class.
      (oset node object-name (semantic-tag-name tag))
      (oset node attributes (nreverse attrib))
      (oset node methods (nreverse method))
      ;; Tada!
      )))

(defmethod cogre-peer-update-from-element ((peer cogre-peer-semantic-class) element)
  "Update the PEER object, from the ELEMENT data, changing the environment."
  (message "Cannot update source from graph yet.")
   nil)

;;; NEW NODE FROM SOURCE
;;
;; Handle conversion from a Semantic Tag (source) to a new node.
;;
;;;###autoload
(defun cogre-semantic-tag-to-node (tag)
  "Convert the Semantic tag TAG into a COGRE node.
Only handles data types nodes.
To convert function/variables into methods or attributes in
an existing COGRE node, see @TODO - do that."
  (cond
   ((and tag (semantic-tag-of-class-p tag 'type)
	 (or (string= (semantic-tag-type tag) "class")
	     (string= (semantic-tag-type tag) "struct")))
    ;; A type from a typed language, make the peer and the class,
    ;; then perform the update.
    (let* ((peer (cogre-peer-semantic-class
		  (semantic-tag-name tag)
		  :tag (semantic-tag-copy tag nil t)))
	   (node (cogre-class (semantic-tag-name tag)
			      :peer peer)))
      (cogre-peer-update-from-source peer node)
      node))

   ((semantic-tag-of-class-p tag 'node)
    ;; A node from a dot file.
    (error "COGRE can't import dot files yet")
    )
   (t
    (error "COGRE can only convert language datatypes into class nodes"))))

;;; SEMANTIC TAG GENERATOR
;;
;; Convert a graph into a set of Semantic Tags.   Different utilties
;; will be needed to convert these tags into source code.
(defun cogre-export-semantic (&optional m-mode)
  "Export the current COGRE graph to a set of Semantic Tags.
This command will just display the generated tags.
Use different utilities to convert these tags to into code.
Optional argument M-MODE specifies the mode to have active
while creating the tags."
  (interactive)
  (when (not (eieio-object-p cogre-graph)) (error "No graph to export"))

  (let* ((g cogre-graph)
	 (tags nil)
	 )

    (setq tags
	  (if m-mode
	      (with-mode-local-symbol m-mode
		(cogre-export-semantic-method g))
	    ;; Else, no mode, use whatever...
	    (cogre-export-semantic-method g)))

    (if (interactive-p)
	;; Dump the output.
	(data-debug-show-stuff tags "CogreSemanticTagExport")
      tags)))

(defmethod cogre-export-semantic-method ((g cogre-base-graph))
  "Convert G into Semantic Tag for a typed language."
  (save-excursion
    (set-buffer (oref g buffer))
    ;; Sort the graph into a nested a tree.
    (let ((tree (cogre-uml-sort-for-lineage g))
	  (out nil))
      ;; Each entry in TREE is:
      ;; ( CLASS SUPER1 SUPER2 ... )
      (dolist (E tree) 
	(let* ((node (car E))
	       (name (oref node :object-name))
	       (parents (mapcar (lambda (P)
				  (oref P :object-name))
				(cdr E)))
	       (attrib (oref node attributes))
	       (method (oref node methods))
	       )
	  (let ((tag (semantic-tag-new-type
		      name "class"
		      (append method attrib)
		      (list parents))))
	    (push tag out))
	  ))
      (nreverse out))))

;;; CODE GENERATOR
;;
;; Generate code from a graph.  Convert into Semantic tags, then
;; output into a source file.
;;
;;;###autoload
(defun cogre-export-code (file)
  "Export the current graph into source-code in FILE.
Uses `cogre-export-semantic' to convert into Semantic tags.
Uses `cogre-srecode-setup' to setup SRecode for code generation."
  (interactive "FOutput File: ")
  (let* ((newfilebuff (find-file-noselect file))
	 (mode (save-excursion (set-buffer newfilebuff)
			       major-mode))
	 (tags (cogre-export-semantic mode))
	 )
    ;; Load our tables.
    (cogre-srecode-load-tables)
    (srecode-load-tables-for-mode mode)

    ;; Switch to this new buffer.
    (switch-to-buffer newfilebuff)

    (when (= (point-min) (point-max))
      ;; Start it out.
      (srecode-insert "file:empty"))

    ;; Insert all the tags.
    (dolist (T tags)
      (let ((me (srecode-semantic-insert-tag T)))
	(goto-char me))
      )
    
    ))

;;; USER-UTILITY
;; 
;; Previous version of cogre-uml-quick-class used a different class
;; hierarchy.  This implementation uses the new peer back-end to various
;; nodes to build up the tree.
;;
;; Also, use new Semantic analyzer features to identify the
;; classes we want to build.

(defvar cogre-class-history nil
  "History for inputting class names.")

(defun cogre-read-class-name ()
  "Read in a class name to be used by a cogre node."
  (let ((finddefaultlist (semantic-find-tag-by-overlay))
	class prompt stream
	)
    ;; Assume the top most item is the all encompassing class.
    (if finddefaultlist
	(setq class (car finddefaultlist)))
    ;; Make sure our class is really a class
    (if (not (and
	      class
	      (eq (semantic-tag-class class) 'type)
	      (string= (semantic-tag-type class) "class")))
	(setq class nil)
      (setq class (semantic-tag-name class)))
    ;; Create a prompt
    (setq prompt (if class (concat "Class (default " class "): ") "Class: "))
    ;; Get the stream used for completion.
    (let ((types (semanticdb-strip-find-results
		  (semanticdb-brute-find-tags-by-class 'type)
		  ;; Don't find-file-match.  Just need names.
		  )))
      (setq stream (semantic-find-tags-by-type "class" types)))
    ;; Do the query
    (completing-read prompt stream
		     nil nil nil 'cogre-class-history
		     class)
    ))

;;;###autoload
(defun cogre-uml-quick-class (class)
  "Create a new UML diagram based on CLASS showing only immediate lineage.
The parent to CLASS, CLASS, and all of CLASSes children will be shown."
  (interactive (list (cogre-read-class-name)))
  
  (message "Building UML diagram for %S" class)

  (let* ((brute (semanticdb-brute-deep-find-tags-by-name class (current-buffer)))
	 (byclass (when brute (semanticdb-find-tags-by-class 'type brute)))
	 (stripped (when byclass (semanticdb-strip-find-results byclass t)))
	 (classes (when stripped (semantic-find-tags-by-type "class" stripped)))
	 (class-tok (car classes))
	 (parent (semantic-tag-type-superclasses class-tok))
	 (ptags nil)

	 (children
	  (semanticdb-strip-find-results
	   (semanticdb-find-tags-subclasses-of-type
	    (semantic-tag-name class-tok) (current-buffer))))
	 )

    (unless class-tok
      (error "Could not find class %S" class))

    (save-excursion
      ;; Go to our token, and then look up the parents.
      (semantic-go-to-tag class-tok)
      (let ((scope (semantic-calculate-scope)))
	(dolist (P parent)
	  (push (semantic-analyze-find-tag P 'type scope)
		ptags))
	))

    ;; Create a graph.
    (cogre (semantic-tag-name class-tok))

    ;; Create this class
    (let ((CT (cogre-semantic-tag-to-node class-tok)))

      ;; Create all the parent nodes in the graph, then connect
      ;; them to C.
      (dolist (P ptags)
	(when P
	  (let ((pn (cogre-semantic-tag-to-node P)))
	    (make-instance 'cogre-inherit :start CT :end pn))))
      
      ;; Create all the children nodes, and align them.
      (dolist (C children)
	(let ((cn (cogre-semantic-tag-to-node C)))
	(make-instance 'cogre-inherit :start cn :end CT))))
      
    ;; Run the layout engine.
    (condition-case nil
	(cogre-layout)
      (error
       (message "Layout engine failed. You need to install Graphviz.")
       ))
    ))

(provide 'cogre-semantic)
;;; cogre-semantic.el ends here
