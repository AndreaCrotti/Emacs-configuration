;;; u-vm-color.el --- Font-lock support for VM.

;;  Copyright (C) 2001-2007 by Ulf Jasper

;;  Emacs Lisp Archive Entry
;;  Author:     Ulf Jasper <ulf.jasper@web.de>
;;  Filename:   u-vm-color.el
;;  Created:    January 19 2001
;;  Keywords:   VM, Customization
;;  Time-stamp: "23. Februar 2008, 21:28:20 (ulf)"
;;  CVS-Version: $Id: u-vm-color.el,v 2.19 2008-02-23 20:28:57 ulf Exp $

(defconst u-vm-color-version "2.10" "Version number of u-vm-color.")

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; ======================================================================
;;; Commentary:

;;  This package provides a simple way for configuring faces for VM.
;;  All faces are customizable.

;;  For the VM summary buffer this is done using `font-lock', for the
;;  message buffer by a "proprietary" fontifier.

;;  For vm-summary-mode font-lock-keywords are created from the value of
;;  `vm-summary-format'.  All vm format-specifiers are understood (as of VM
;;  6.88), as well as the user-defined specifier `%UB', provided by BBDB.

;;  To install and use place this file somewhere in your load-path and put
;;  the following in your VM startup file (~/.emacs or ~/.vm)

;;  (require 'u-vm-color)
;;  (add-hook 'vm-summary-mode-hook 'u-vm-color-summary-mode)
;;  (add-hook 'vm-select-message-hook 'u-vm-color-fontify-buffer)

;;  It may be necessary to add the following, which probably comes from
;;  a bug in my code...
;;  (defadvice vm-decode-mime-message (after u-vm-color activate)
;;    (u-vm-color-fontify-buffer-even-more))

;;  If you are using auto-fill, ie when the variable
;;  `vm-fill-paragraphs-containing-long-lines' is not nil, you should
;;  also add this:
;;  (defadvice vm-fill-paragraphs-containing-long-lines
;;              (after u-vm-color activate)
;;              (u-vm-color-fontify-buffer))

;;  It will make sure that buffers/messages, which have been re-filled
;;  are fontified properly.

;;  It is possible to use the face definitions from Gnus by adding
;;  (setq u-vm-color-use-gnus-faces t)
;;  However, this is irreversible.  At least for that Emacs session.

;;  All faces are customizable: Just say
;;  M-x customize-group <ret> u-vm-color

;;  In order to prevent Emacs from locking I strongly recommend to use
;;  lazy-lock or jit-mode.

;;  Disclaimer: `u-vm-color' may show unexpected results, or even fail, if
;;  vm-summary-format is sufficiently complex=strange.

;;  XEmacs users might want to turn off `vm-use-lucid-highlighting', if
;;  this package works...

;; ======================================================================
;;; History:

;;  2.10: (2008-02-23)
;;        Bugfixes -- thanks to Martin Schwenke
;;  2.9:  (2007-12-19)
;;        Handle PGP signatures -- thanks to Frederik Axelsson.
;;        Other minor changes.
;;  2.8.1: (2005-10-22)
;;        Added autoload cookies.
;;        Silence compiler warnings.
;;  2.8:  (2005-04-05)
;;        Fixed problems with non-graphical chars in summary buffers.
;;        Fixed font-lock-problems with "older" Emacsen which were
;;        introduced with version 2.7.
;;  2.7:  (2005-02-26)
;;        Fixed font-lock-problems with recent CVS Emacs.
;;  2.6:  Fixed problems with summary mode in recent CVS Emacs.
;;        Added u-vm-color-spamassassin.
;;  2.5:  Bugfix(?): require gnus-cite for gnus-faces. Thanks to
;;                   Richard Walker for pointing out.
;;        Tested with Emacs 21.2.2/VM 7.08
;;  2.4:  Bugfix: re-activated font-lock-keywords-only. If this is not set,
;;                font-lock tries to fontify strings and will screw up the
;;                summary buffer if it finds double-quotes.
;;                Thanks to Stefan Kamphausen for testing.
;;        Recognize lengths of *strings* in the vm-summary-format, like in
;;        "%-10.10F %s". In this case sender/recipient and subject will
;;        always be correctly fontified. (The font-lock regexp will now be
;;        ".......... .*" instead of ".* .*".) Note that it is still not
;;        possible to distinguish two arbitrary-length adjacent strings,
;;        like in "%F %s".

;;        Tested with Emacs 21.2.2
;;  2.3:  Bugfix: Removed (setq font-lock-keywords-only t) in
;;                u-vm-color-summary-mode, which confused font-lock in XEmacs
;;                21.4 when vm-use-toolbar was non-nil -- ???!
;;        Tested with Emacs 21.2.1/VM 7.07 and XEmacs 21.4.6/VM 7.03.
;;  2.2:  Bugfixes: Recipient- and author face were interchanged in message.
;;                  Now setting buffer-modified-p to its original value after
;;                  fontifying message buffer.
;;  2.0:  Fontification in message buffers now done "by hand" -- no
;;        font-lock here any more. Apparently font-lock removes all
;;        face-properties when it is started. So, inlined html messages and
;;        such looked quite boring.

;;        No limitation on header lengths anymore. Doesn't remove faces for
;;        inlined html messages and such.

;;        Tested with emacs 21.1.
;;  1.11: Added faces for dark backgrounds.
;;        Introduced u-vm-color-use-gnus-faces.
;;  1.9   Colons belong to header-keywords.
;;  1.7   Forgot VM's B attribute.
;;  1.6:  Limited headers and signatures to 5 lines to avoid regexp stack
;;        overflow.
;;        Citations now supercite-compliant.
;;  1.5:  Minor bug fixes.
;;  1.1:  Introduced minor modes.
;;        Should work for xemacs as well.
;;  1.0:  Initial version.

;; ======================================================================
;;; Code:
(require 'font-lock)

;; Silence compiler warnings
(defvar vm-summary-format)

(defgroup u-vm-color nil
  "Font-lock support for vm."
  :group 'vm)

(defcustom u-vm-color-use-gnus-faces nil
  "Use corresponding face definitions from Gnus."
  :type 'boolean
  :group 'u-vm-color)

(defface u-vm-color-signature-face
  '((((class color) (background dark))
     (:bold nil :italic t :foreground "misty rose"))
    (((class color) (background light))
     (:bold nil :italic t :foreground "Sienna")))
  "Face for Signatures."
  :group 'u-vm-color)

(defface u-vm-color-header-face
  '((((class color) (background dark))
     (:bold t :italic nil :foreground "white"))
    (((class color) (background light))
     (:bold t :italic nil :foreground "black")))
  "General Face for header keywords."
  :group 'u-vm-color)

(defface u-vm-color-author-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "cornflower blue"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "midnight blue")))
  "Face for sender names."
  :group 'u-vm-color)

(defface u-vm-color-recipient-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "green"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "DarkGreen")))
  "Face for recipient names."
  :group 'u-vm-color)

(defface u-vm-color-subject-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "sky blue"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "medium blue")))
  "Face for subjects."
  :group 'u-vm-color)

(defface u-vm-color-default-face
  '((t (:italic t)))
  "Default face."
  :group 'u-vm-color)

(defface u-vm-color-time-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "pink"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "maroon")))
  "Face for message time."
  :group 'u-vm-color)

(defface u-vm-color-attribute-face
  '((((class color) (background dark))
     (:bold t :italic nil :foreground "orange red"))
    (((class color) (background light))
     (:bold t :italic nil :foreground "red")))
  "Face for vm attributes."
  :group 'u-vm-color)

(defface u-vm-color-date-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "pink"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "maroon")))
  "Face for message date."
  :group 'u-vm-color)

(defface u-vm-color-id-face
  '((t (:bold nil :italic t)))
  "Face for message id."
  :group 'u-vm-color)

(defface u-vm-color-label-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "orange red"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "red")))
  "Face for vm labels."
  :group 'u-vm-color)

(defface u-vm-color-length-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "white"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "black")))
  "Face for message length."
  :group 'u-vm-color)

(defface u-vm-color-number-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "white"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "black")))
  "Face for message number."
  :group 'u-vm-color)

(defface u-vm-color-user-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "light sea green"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "forest green")))
  "Face for user defined summary elements."
  :group 'u-vm-color)

(defface u-vm-color-citation-1-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "orange red"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "orange red")))
  "Face for citations."
  :group 'u-vm-color)

(defface u-vm-color-citation-2-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "SkyBlue1"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "SlateBlue")))
  "Face for citation."
  :group 'u-vm-color)

(defface u-vm-color-citation-3-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "cyan"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "DarkGreen")))
  "Face for citation."
  :group 'u-vm-color)

(defface u-vm-color-citation-4-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "magenta"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "BlueViolet")))
  "Face for citation."
  :group 'u-vm-color)

(defface u-vm-color-citation-5-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "firebrick1"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "Firebrick")))
  "Face for citation."
  :group 'u-vm-color)

(defface u-vm-color-spamassassin-face
  '((((class color) (background dark))
     (:bold nil :italic nil :foreground "firebrick1"))
    (((class color) (background light))
     (:bold nil :italic nil :foreground "Firebrick")))
  "Face for spamassassin preview block."
  :group 'u-vm-color)

(defface u-vm-color-pgp-inline-signature-face
  '((((class color) (background dark))
     (:bold nil :italic t :foreground "blue"))
    (((class color) (background light))
     (:bold nil :italic t :foreground "blue")))
  "Face for pgp inline signatures."
  :group 'u-vm-color)

(defun u-vm-color-copy-gnus-faces ()
  "Set up u-vm-color faces by copying from corresponding Gnus faces."
  ;; make sure we have the Gnus faces
  (require 'gnus-art)
  (require 'gnus-cite)
  (require 'message)
  ;;
  (message "u-vm-color: copying Gnus faces...")
  (when (facep 'gnus-signature-face)
    (copy-face 'gnus-signature-face 'u-vm-color-signature-face))
  (when (facep 'gnus-header-from-face)
    (copy-face 'gnus-header-from-face 'u-vm-color-author-face))
  (when (facep 'gnus-header-subject-face)
    (copy-face 'gnus-header-subject-face 'u-vm-color-subject-face))
  (when (facep 'gnus-header-content-face)
    (copy-face 'gnus-header-content-face 'u-vm-color-default-face))
  (when (facep 'gnus-header-name-face)
    (copy-face 'gnus-header-name-face 'u-vm-color-header-face))
  (when (facep 'gnus-cite-face-1)
    (copy-face 'gnus-cite-face-1 'u-vm-color-citation-1-face))
  (when (facep 'gnus-cite-face-2)
    (copy-face 'gnus-cite-face-2 'u-vm-color-citation-2-face))
  (when (facep 'gnus-cite-face-3)
    (copy-face 'gnus-cite-face-3 'u-vm-color-citation-3-face))
  (when (facep 'gnus-cite-face-4)
    (copy-face 'gnus-cite-face-4 'u-vm-color-citation-4-face))
  (when (facep 'gnus-cite-face-5)
    (copy-face 'gnus-cite-face-5 'u-vm-color-citation-5-face))
  (message "u-vm-color: copying Gnus faces... done"))

(defun u-vm-color-make-specific-length-regexp (regexp m-length length
						      &optional prefix)
  "Create a regular expression.
Argument REGEXP a regexp .
Argument M-LENGTH the minimal LENGTH.
Optional argument PREFIX the maximal length."
(let ((i 0)
	(result "\\("))
    (if prefix
	(setq result (concat result prefix)))
    ;;(message "input: %s %d %d" regexp m-length length)
    (cond ((and length (> length 0))
	   (when m-length
	     (while (and (< i m-length) (< i length))
	       (setq result (concat result regexp))
	       (setq i (1+ i))))
	   (while (< i length)
	     (setq result (concat result regexp "?"))
	     (setq i (1+ i))))
	  (t
	   (setq result (concat result regexp "*"))))
    ;;(message "result: --%s--" result)
    (concat result "\\)")))


(defun u-vm-color-make-summary-keywords ()
  "Parse `vm-summary-format' and return a font-lock keyword list.
List consists of one big regexp and lots of face instructions for
subexpressions."
  (let ((search-start 0)
	(length 0)   ; (maximum) length
	(m-length 0) ; minimum length
	(rest "")
	(f-element "")
	(m-element "")
	(value "")
	(u-format "^..")
	(u-match nil)
	(count 1)
	(t-vm-summary-format vm-summary-format)
	(u-vm-color-xemacs-workaround
	 (string-match "XEmacs\\|Lucid" emacs-version)))
    ;; pick up all elements in the vm-summary-format
    (while (string-match
	    (concat "%-?\\([0-9]+\\.\\)?-?\\([0-9]+\\)?"
		    "\\([aAcdfFhHiIlLmMnstTwyz*]\\|U.\\)\\([^%\n]*\\)")
	    t-vm-summary-format search-start)
      (setq search-start (match-end 0))
      (if (match-beginning 1)
	  (setq m-length (string-to-number
			  (substring t-vm-summary-format (match-beginning 1)
				     (1- (match-end 1)))))
	(setq m-length 0))
      (if (match-beginning 2)
	  (setq length (string-to-number
			(substring t-vm-summary-format (match-beginning 2)
				   (match-end 2))))
	(setq length 0))
      (if (match-beginning 3)
	  (setq value (substring t-vm-summary-format (match-beginning 3)
				 (match-end 3)))
	(setq value ""))
      (if (match-beginning 4)
	  (setq rest (substring t-vm-summary-format (match-beginning 4)
				(match-end 4)))
	(setq rest ""))
      (setq rest (regexp-quote rest))

      ;;(message "--> %s, %s, %s" length m-length value)
      ;; Should use the length and m-length values for things like %5d
      ;; instead of doing [0-9 ]+ for numerics...
      ;; No!
      (cond ((string-equal value "a") ;; attributes -- make sure that all
				      ;; possible letters are given!
	     (setq f-element "\\([DNU ][FW ][RZB ][E ]\\)")
	     (setq m-element (list count (quote 'u-vm-color-attribute-face)
				   nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "A") ;; attributes -- long
	     (setq f-element "\\([DNU ][r ][z ][b ][f ][w ][e ]\\)")
	     (setq m-element (list count (quote 'u-vm-color-attribute-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "c") ;; number of characters
	     (setq f-element "\\( *[0-9]+ *\\)")
	     (setq m-element (list count (quote 'u-vm-color-length-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "d") ;; day -- numeric
	     (setq f-element "\\( *[0-9]+ *\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "f") ;; authors / recipients address
	     ;;(setq f-element "\\(To: [^ \n]+\\)?\\([^ \n]+\\)?")
	     (setq f-element (concat
			      "\\("
			      (u-vm-color-make-specific-length-regexp
			       ;;"[ [:graph:]]"
                               "." (- m-length 4) (- length 4) "To: ")
			      "\\|"
			      (u-vm-color-make-specific-length-regexp
			       ;;"[ [:graph:]]"
                               "." m-length length)
			      "\\)"))
	     (setq count (+ 1 count))
	     (setq m-element (list count
				   (quote 'u-vm-color-recipient-face) t t))
	     (setq count (+ 1 count))
	     (setq u-match (append u-match (list m-element)))
	     (setq m-element (list count (quote 'u-vm-color-author-face) t t)))
	    ((or (string-equal value "F")
		 (string-equal value "UA")  ;; IS THIS CORRECT!????????
		 (string-equal value "UB")) ;; authors / recipients full names
	     ;;(setq f-element "\\(To:.+\\)?\\([^:\n]+\\)?")
	     (setq f-element (concat
			      "\\("
			      (u-vm-color-make-specific-length-regexp
			       ;;"[ [:graph:]]"
                               "." (- m-length 4) (- length 4) "To: ")
			      "\\|"
			      (u-vm-color-make-specific-length-regexp
			       ;;"[ [:graph:]]"
                               "." m-length length)
			      "\\)"))
	     (setq count (+ 1 count))
	     (setq m-element (list count
				   (quote 'u-vm-color-recipient-face) t t))
	     (setq count (+ 1 count))
	     (setq u-match (append u-match (list m-element)))
	     (setq m-element (list count (quote 'u-vm-color-author-face) t t)))
	    ((string-equal value "h") ;; time
	     (setq f-element "\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)")
	     (setq m-element (list count (quote 'u-vm-color-time-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "H") ;; time -- short
	     (setq f-element "\\([0-9][0-9]:[0-9][0-9]\\)")
	     (setq m-element (list count (quote 'u-vm-color-time-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "i") ;; id
	     (setq f-element "\\(<[^ \n]+>\\)")
	     (setq m-element (list count (quote 'u-vm-color-id-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "I") ;; indentation
	     (setq f-element " *")
	     (setq m-element nil))
	    ((string-equal value "l") ;; number of lines
	     (setq f-element "\\( *[0-9]+ *\\)")
	     (setq m-element (list count (quote 'u-vm-color-length-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "L") ;; label
	     (setq f-element (u-vm-color-make-specific-length-regexp
			      ;;"[ [:graph:]]"
                              "." m-length length))
	     (setq m-element (list count (quote 'u-vm-color-label-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "m") ;; month
	     (setq f-element "\\([A-Za-z]+\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "M") ;; month -- numeric
	     (setq f-element "\\( *[0-9]+ *\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "n") ;; message number
	     (setq f-element "\\( *[0-9]+ *\\)")
	     (setq m-element  (list count (quote 'u-vm-color-number-face))))
	    ((string-equal value "s") ;; subject
	     (setq f-element (u-vm-color-make-specific-length-regexp
			      ;;"[ [:graph:]]"
                              "." m-length length))
	     (setq m-element (list count (quote 'u-vm-color-subject-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "t") ;; recipient addresses
	     (setq f-element "\\([^ \n]+\\)")
	     (setq m-element (list count (quote 'u-vm-color-recipient-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "T") ;; recipient full names
	     (setq f-element "\\(.+\\)")
	     (setq m-element (list count (quote 'u-vm-color-recipient-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "w") ;; week day (is missing in some mails!)
	     (setq f-element "\\([A-Za-z ]+\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "y") ;; year
	     (setq f-element "\\([0-9]+\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "z") ;; timezone
	     (setq f-element "\\(.+\\)")
	     (setq m-element (list count (quote 'u-vm-color-date-face)
				    nil u-vm-color-xemacs-workaround)))
	    ((string-equal value "*") ;; mark-marker
	     (setq f-element "\\(\\*\\| \\)")
	     (setq m-element (list count (quote 'u-vm-color-attribute-face)
				    nil u-vm-color-xemacs-workaround)))
	    (t ;; user defined and everything else
	     (setq f-element ".*")
	     (setq m-element nil)))
      (setq u-format (concat u-format f-element rest))
      (if m-element
	  (progn
	    (setq count (+ 1 count))
	    (setq u-match (append u-match (list m-element))))))
    (setq u-format (concat u-format "$"))
    (append (list u-format) u-match)))

(defvar u-vm-color-summary-mode nil)
(make-variable-buffer-local 'u-vm-color-summary-mode)
(add-to-list 'minor-mode-alist '(u-vm-color-summary-mode nil))

(defvar u-vm-color-summary-keywords nil)

;; FIXME: u-vm-color-summary-mode cannot be turned off
;;;###autoload
(defun u-vm-color-summary-mode (&optional arg)
  "Configure `font-lock-keywords' and add some hooks for vm-buffers.
Optional argument ARG is not used!"
  (interactive "P")
  (setq u-vm-color-summary-mode
	(not (or (and (null arg) u-vm-color-summary-mode)
		 (<= (prefix-numeric-value arg) 0))))

  (if u-vm-color-use-gnus-faces (u-vm-color-copy-gnus-faces))

  ;; apparently emacs expects this statement here...
  (font-lock-mode 1)
  (cond ((string-match "XEmacs\\|Lucid" emacs-version)
	 ;; XEmacs
	 (setq u-vm-color-summary-keywords
	       (list (u-vm-color-make-summary-keywords)))
	 (put 'vm-summary-mode 'font-lock-defaults
	      '(
		'u-vm-color-summary-keywords
		t	; keywords-only
		t	; case-fold
		nil	; syntax-alist
		nil  ; syntax-begin
		))
	 (setq font-lock-keywords (list (u-vm-color-make-summary-keywords)))
	 (font-lock-fontify-buffer))
   	(t
	 ;; GNU Emacs
	 (setq u-vm-color-summary-keywords
	       (list (u-vm-color-make-summary-keywords)))
 	 (set (make-local-variable 'font-lock-defaults)
 	       (list 'u-vm-color-summary-keywords ;; keywords
 		     t				  ;; keywords-only
 		     t				  ;; case-fold
 		     nil			  ;; syntax-alist
 		     nil))			  ;; syntax-begin

         ;; With the CVS version of GNU Emacs as of Feb. 2005 one must
	 ;; not set font-lock-keywords explicitly as a global variable.
	 ;; It is sufficient to set font-lock-defaults.
         ;; For older GNU Emacs versions up to 21.3 it is necessary to
         ;; set font-lock-keywords.
         ;; Setting font-lock-keywords as a local variable works with all
	 ;; GNU Emacs versions.
	 ;; 2005-04-05
         (set (make-local-variable 'font-lock-keywords)
              u-vm-color-summary-keywords)
	 (set (make-local-variable 'font-lock-keywords-only) t)
 	 (font-lock-mode 1))))

(defun u-vm-color-fontify-regexp (start end regexp how)
  "Search the buffer for an expression and fontify it.
Search starts at START and ends at END.  If REGEXP is found, it
is fontified according to the argument HOW, which is a list of
the form '((index face)...)."
;;(message "Searching from %d to %d for %s" start end regexp)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and start (< start end))
	(setq start (re-search-forward regexp end t))
	(when start
	  ;;(message "match found!")
	  (mapc (lambda (what)
                  (let ((index (nth 0 what)) (face (nth 1 what)))
                    (when (match-beginning index)
                      ;;(message "Adding face %s for match %d" face index)
                      (put-text-property (match-beginning index)
                                         (match-end index)
                                         'face face))))
                how))))))

(defun u-vm-color-fontify-signature (start end)
  "Search and fontify the signature.
Search is restricted to the region between START and END."
(let ((inhibit-read-only t))
    (save-excursion
      (goto-char end)
      (setq start (re-search-backward "^\\(- \\)?-- ?$" start t))
	(when start
	  (put-text-property start end 'face 'u-vm-color-signature-face)))))

(defun u-vm-color-fontify-pgp-signature (start end)
  "Search and fontify inline PGP signatures."
  (let ((inhibit-read-only t)
	(pgp-end-regex "-----END PGP SIGNATURE-----")
	(pgp-start-regex "-----BEGIN PGP SIGNATURE-----")
	(pgp-sign-regex "-----BEGIN PGP SIGNED MESSAGE-----")
	(pgp-hash-regex "^Hash: .*")
	re-end-pos)
    (save-excursion
      (goto-char end)
      (when (re-search-backward pgp-end-regex start t)
	(setq re-end-pos (match-end 0))
	(when (re-search-backward pgp-start-regex start t)
	  (put-text-property (point) re-end-pos
			     'face 'u-vm-color-pgp-inline-signature-face)))
      (when (re-search-backward pgp-hash-regex start t)
      	(setq re-end-pos (match-end 0))
	(when (re-search-backward pgp-sign-regex start t)
	  (put-text-property (point) re-end-pos
			     'face ' 'u-vm-color-pgp-inline-signature-face)))
      )))

;;;###autoload
(defun u-vm-color-fontify-buffer ()
  "Fontifies mail-buffers."
  (interactive)
  ;;(message "u-vm-color-fontify-buffer")
  (let ((continued-header-contents "\\(.*\\(\n[ \t]+.*\\)*\\)")
	(pmin (point-min))
	(buffer-has-been-modified-before (buffer-modified-p))
	(header-end (or
		     (save-excursion
		       (goto-char (point-min))
		       (re-search-forward "^[ \t]*$" (point-max) t))
		     (point-min))))
    (u-vm-color-fontify-regexp pmin header-end
			       (concat "^\\([A-Z][-A-Za-z0-9]+:\\) "
				       continued-header-contents)
			       '((1 u-vm-color-header-face)
				 (2 u-vm-color-default-face)))
    (u-vm-color-fontify-regexp pmin header-end
			       (concat "^Date: " continued-header-contents)
			       '((1 u-vm-color-date-face)))
    (u-vm-color-fontify-regexp pmin header-end
			       (concat "^Subject: "
				       continued-header-contents)
			       '((1 u-vm-color-subject-face)))
    (u-vm-color-fontify-regexp pmin header-end
			       (concat "^\\(From\\|Sender\\): "
				       continued-header-contents)
			       '((2 u-vm-color-author-face)))
    (u-vm-color-fontify-regexp pmin header-end
			       (concat "^\\(To\\|Cc\\|Bcc\\|Fcc\\): "
				       continued-header-contents)
			       '((2 u-vm-color-recipient-face)))
    ;; signature
    (u-vm-color-fontify-signature header-end (point-max))
    ;; PGP-signatures
    (u-vm-color-fontify-pgp-signature header-end (point-max))
    ;; citations
    (u-vm-color-fontify-regexp header-end (point-max)
			       "^ *[-A-Za-z0-9]*> *.*$"
			       '((0 u-vm-color-citation-1-face)))
    (u-vm-color-fontify-regexp header-end (point-max)
			       "^ *[-A-Za-z0-9]*> *\\([-A-Za-z0-9]*> *.*\\)$"
			       '((1 u-vm-color-citation-2-face)))
    (u-vm-color-fontify-regexp header-end (point-max)
			       (concat "^ *[-A-Za-z0-9]*> *[-A-Za-z0-9]*> *"
				       "\\([-A-Za-z0-9]*> *.*\\)$")
			       '((1 u-vm-color-citation-3-face)))
    (u-vm-color-fontify-regexp header-end (point-max)
			       (concat "^ *[-A-Za-z0-9]*> *[-A-Za-z0-9]*> *"
				       "[-A-Za-z0-9]*> *\\([-A-Za-z0-9]*> *"
				       ".*\\)$")
			       '((1 u-vm-color-citation-4-face)))
    (u-vm-color-fontify-regexp header-end (point-max)
			       (concat "^ *[-A-Za-z0-9]*> *[-A-Za-z0-9]*> *"
				       "[-A-Za-z0-9]*> *[-A-Za-z0-9]*> *"
				       "\\([-A-Za-z0-9]*> *.*\\)$")
			       '((1 u-vm-color-citation-5-face)))
    ;; Spamassassin preview block
    (u-vm-color-fontify-regexp header-end (point-max)
			       (concat "^Content preview:"
				       "\\([^\n]*\n\\( +[^\n]*\n\\)*\\)")
			       '((1 u-vm-color-spamassassin-face)))
    (set-buffer-modified-p buffer-has-been-modified-before)))

;;;###autoload
(defun u-vm-color-fontify-buffer-even-more ()
  "Temporarily widen buffer and call `u-vm-color-fontify-buffer'."
(save-restriction
    (widen)
    ;;(message "u-vm-color-fontify-even-more: %d %d" (point-min) (point-max))
    (u-vm-color-fontify-buffer)))

(provide 'u-vm-color)
;;; u-vm-color.el ends here
