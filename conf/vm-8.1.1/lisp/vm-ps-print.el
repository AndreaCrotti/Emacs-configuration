;;; vm-ps-print.el --- PS-printing functions for VM
;;
;; Copyright (C) 1999 Robert Fenk
;;
;; Author:	Robert Fenk
;; Status:	Tested with XEmacs 21.4.15 & VM 7.18
;; Keywords:	extensions, vm, ps-print
;; X-URL:       http://www.robf.de/Hacking/elisp
;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; 
;; There are three new user functions for generating postscript output:
;;   vm-ps-print-message
;;   vm-ps-print-each-message
;;   vm-ps-print-message-preview
;; The first one prints like vm-ps-print, but multiple messages are
;; concatenated to one printout.  In contrast to this the second
;; function creates one print job for each message.  Finally the the
;; third one prints the current message as displayed in the
;; presentation buffer -- the other two functions do their own MIME
;; decoding therefore messages are always display in their default
;; appearance.
;;
;; To use these functions you should put this file into your load-path
;; and add the following lines to your .vm file:
;;
;; (require 'vm-ps-print)
;;
;; To redefine the default VM settings for the tool bar and menu add
;; the following line.  The default is to use  `vm-ps-print-message',
;; but if you use an optional non nil argument you will get
;; `vm-ps-print-each-message' as print function.
;; 
;; (vm-ps-print-message-infect-vm)
;;
;; This will refine the default VM settings and from now on you should
;; be able to print to your postscript printer by using the usual VM
;; commands.
;; Of course you still have to set `lpr-command' and `lpr-switches' or
;; `ps-lpr-command' and `ps-lpr-switches' to reasonable values!
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(eval-when-compile
  (require 'vm-version)
  (require 'vm-message)
  (require 'vm-macro)
  (require 'vm-vars))

(require 'vm-save)
(require 'ps-print)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup vm nil
  "VM"
  :group 'mail)

(defgroup vm-psprint nil
  "The VM ps-print lib"
  :group 'vm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defcustom vm-ps-print-message-function  'ps-print-buffer-with-faces
  "*This should point to the function which is used for ps-printing.
The function should accept one optional argument which is a filename."
  :group 'vm-psprint
  :type 'function)

;;;###autoload
(defcustom vm-ps-print-message-separater  "\n"
  "*The separator between messages when printing multiple messages."
  :group 'vm-psprint
  :type 'string)

;;;###autoload
(defcustom vm-ps-print-message-font-size  10
  "*The font size for the PS-output of the message text."
  :group 'vm-psprint
  :type 'integer)

;;----------------------------------------------------------------------------

;;;###autoload
(defcustom vm-ps-print-message-header-lines  2
  "*See `ps-header-lines'."
  :group 'vm-psprint
  :type 'integer)

;;;###autoload
(defcustom vm-ps-print-message-left-header
  '(list (format "(Folder `%s')" folder-name)
	 (format "(%d message%s printed)" mcount (if (= mcount 1) "" "s")))
  "*This variable should contain a command returning a valid `ps-left-header'."
  :group 'vm-psprint
  :type 'sexp)

;;;###autoload
(defcustom vm-ps-print-message-right-header
  '(list"/pagenumberstring load" 'dd-mon-yyyy)
  "*This variable should contain a command returning a valid `ps-right-header'.
The defaults to the number of pages and the date of the printout."
  :group 'vm-psprint
  :type 'sexp)

;;;###autoload
(defcustom vm-ps-print-message-summary-format
  (concat "******************************************************************************\n"
	  (if (boundp 'vm-summary-format)
              vm-summary-format
            "%n %*%a %-17.17F %-3.3m %2d %4l/%-5c %I\"%s\"\n")
          "******************************************************************************\n")
  "*The summary line before a message.
See `vm-summary-format' for a description of the conversion specifiers."
  :group 'vm-psprint
  :type 'string)

;;----------------------------------------------------------------------------
;;;###autoload
(defcustom vm-ps-print-each-message-header-lines 2
  "*See `ps-header-lines'."
  :group 'vm-psprint
  :type 'integer)

;;;###autoload
(defcustom vm-ps-print-each-message-left-header
  '(list (format "(Folder `%s')" folder-name)
	 (format "(%s)" (vm-ps-print-tokenized-summary msg (vm-summary-sprintf vm-ps-print-each-message-summary-format msg t))))
  "*This command should return a valid `ps-left-header'.
The default is to have the folder name and a summary according to the
variable `vm-ps-print-each-message-summary-format' in the left header."
  :group 'vm-psprint
  :type 'sexp)

;;;###autoload
(defcustom vm-ps-print-each-message-right-header
  '(list  "/pagenumberstring load" 'dd-mon-yyyy)
  "*This variable should contain a command returning a valid `ps-right-header'.
The defaults to the number of pages and the date of the printout."
  :group 'vm-psprint
  :type 'sexp)

;;;###autoload
(defcustom vm-ps-print-each-message-summary-format
  "Message# %n, Lines %l, Characters %c"
  "*The summary line for the postscript header.
See `vm-summary-format' for a description of the conversion specifiers."
  :group 'vm-psprint
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vm-ps-print-message-internal (filename each folder-name mcount msg)
  "This function does the actual call to the ps-printing function.
This is not a function to call interactively!

If the customization of headers is insufficient, then you may want
to modify this function.  If FILENAME is a string, then the output is
written to that file.  If EACH is t then create a new johb for each
message.  FOLDER-NAME specifies the folder name which is displayed in
the header line and MCOUNT is the number of messages to print, while
MSG is a VM message pointer.

See:	`vm-ps-print-message-function'"
  (let* ((dd-mon-yyyy (format-time-string "%d %b %Y   %T" (current-time)))
	 (ps-left-header (if each (eval vm-ps-print-each-message-left-header)
			   (eval vm-ps-print-message-left-header)))
	 (ps-right-header (if each (eval vm-ps-print-each-message-right-header)
			    (eval vm-ps-print-message-right-header)))
	 (ps-header-lines  (if each vm-ps-print-each-message-header-lines
			     vm-ps-print-each-message-header-lines))
	 (ps-print-header-frame t)
	 (ps-font-size vm-ps-print-message-font-size))
;    (setq filename (expand-file-name "~/mail.ps"))
    (funcall vm-ps-print-message-function filename)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vm-ps-print-tokenized-summary (message tokens)
  "Return the summary string for MESSAGE according to the format in TOKENS.
Like `vm-tokenized-summary-insert'."
  (if (stringp tokens)
      tokens
    (let (token summary)
      (while tokens
	(setq token (car tokens))
	(cond ((stringp token)
	       (if vm-display-using-mime
		   (setq summary (concat summary
					 (vm-decode-mime-encoded-words-in-string token)))
		 (setq summary (concat summary token))))
	      ((eq token 'number)
	       (setq summary (concat summary (vm-padded-number-of message))))
	      ((eq token 'mark)
	       (setq summary (concat summary (vm-su-mark message))))
	      ((eq token 'thread-indent)
	       (if (and vm-summary-show-threads
			(natnump vm-summary-thread-indent-level))
		   (setq summary (concat summary
					 ?\ (* vm-summary-thread-indent-level
					       (vm-th-thread-indentation message)))))))
	(setq tokens (cdr tokens)))
      summary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vm-ps-print-message-folder-name ()
  "Return a nice folder name, without complete path."
  (let* ((folder-name (or (buffer-file-name) (buffer-name)))
	 (folder-name
	  (if (and vm-folder-directory
		   (string-match (concat (regexp-quote (expand-file-name
							vm-folder-directory))
					 "/?\\(.+\\)")
				 folder-name))
	      (substring folder-name (match-beginning 1) (match-end 2))
	    folder-name)))
    folder-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-ps-print-message (&optional count filename each)
  "PS-Print the current message.

A positive COUNT arg N means print the current message and the next
N-1 messages and a negative one print the current message and the
previous N-1 messages.

If FILENAME is specified then write PS into that file.

When printing a single message it acts like `vm-ps-print-each-message'.
When printing multiple messages it will insert a summary line according
to the variable `vm-ps-print-message-summary-format' and a separator
according to the variable `vm-ps-print-message-separater' between
messages.  You might force the printing of one job per message, by
giving a t EACH argument.

See: `vm-ps-print-message-function'
     `vm-ps-print-message-font-size'
     `vm-ps-print-message-summary-format'
     `vm-ps-print-message-separater'
     `vm-ps-print-message-left-header'
     `vm-ps-print-message-right-header'
for customization of the output."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (or count (setq count 1))

  (let* ((vm-summary-faces-mode nil)
         (folder-name (vm-ps-print-message-folder-name))
         (mstart nil)
	 (m nil)
	 (mlist (vm-select-marked-or-prefixed-messages count))
	 (mcount (length mlist))
	 (tmpbuf (get-buffer-create "*vm-ps-print*")))

    (set-buffer tmpbuf)
    (setq major-mode 'vm-mode)
    (erase-buffer)
    (if (= mcount 1) (setq each 1))
    
    (while mlist
      (setq m (vm-real-message-of (car mlist)))
      (if (not each)
	  (vm-tokenized-summary-insert
	   m (vm-summary-sprintf vm-ps-print-message-summary-format m t)))
      (setq mstart (point-max))
      (vm-insert-region-from-buffer
       (vm-buffer-of m) (vm-vheaders-of m) (vm-end-of m))
      (vm-reorder-message-headers nil
				  vm-visible-headers
				  vm-invisible-header-regexp)
      (vm-decode-mime-encoded-words)
      (goto-char mstart)
      (re-search-forward "\n\n") ;; skip headers
      (if (not (vm-mime-plain-message-p m))
	  (progn (vm-decode-mime-layout (vm-mm-layout m))
		 (delete-region (point) (point-max))))
      (narrow-to-region mstart (point-max))
      (vm-energize-urls)
      (vm-highlight-headers)
      (widen)
      (end-of-buffer)
      (if each
	  (progn (save-excursion
		   (vm-ps-print-message-internal filename t folder-name
						 mcount m))
		 (set-buffer tmpbuf)
		 (erase-buffer))
	(if (> (length mlist) 1) (insert vm-ps-print-message-separater)))
      (setq mlist (cdr mlist)))

    (if (not each)
	(vm-ps-print-message-internal filename nil folder-name mcount nil))
    (kill-buffer tmpbuf)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-ps-print-each-message (&optional count filename)
  "PS-Print the current message.
A positive COUNT arg N means print the current message and the next
N-1 messages and a negative one print the current message and the
previous N-1 messages.

If FILENAME is specified then write PS into that file.

This function acts like `vm-ps-print-message', but it will generate a
separate print job for each message and it does not generate the
summary lines between messages.

See: `vm-ps-print-message-function'
     `vm-ps-print-message-font-size'
     `vm-ps-print-each-message-separater'
     `vm-ps-print-each-message-left-header'
     `vm-ps-print-each-message-right-header'
     `vm-ps-print-each-message-summary-format'
for customization of the output."
  (interactive "p")
  (vm-ps-print-message count filename t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-ps-print-message-presentation (&optional filename)
  "PS-Print the currently presented message.
When called with a numeric prefix argument, prompts the user for the
name of a file to save the PostScript image in, instead of sending it
to the printer.

More specifically, the FILENAME argument is treated as follows: if it
is nil, send the image to the printer.  If FILENAME is a string, save
the PostScript image in a file with that name.  If FILENAME is a
number, prompt the user for the name of the file to save in.

See: `vm-ps-print-message-function'
     `vm-ps-print-message-font-size'
     `vm-ps-print-each-message-separater'
     `vm-ps-print-each-message-left-header'
     `vm-ps-print-each-message-right-header'
     `vm-ps-print-each-message-summary-format'
for customization of the output."
    (interactive (list (ps-print-preprint current-prefix-arg)))
    (save-excursion
      (vm-follow-summary-cursor)
      (vm-select-folder-buffer)
      (vm-check-for-killed-summary)
      (vm-error-if-folder-empty)
      
      (let ((folder-name (vm-ps-print-message-folder-name))
	    (mcount 1)
	    (msg (car vm-message-pointer)))
	
	(if (and (boundp 'vm-mail-buffer) (symbol-value 'vm-mail-buffer))
	    (set-buffer (symbol-value 'vm-mail-buffer)))
	(if vm-presentation-buffer
	    (set-buffer vm-presentation-buffer))
	(vm-ps-print-message-internal filename t folder-name mcount msg)
	)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-ps-print-message-fix-menu (menu each)
  "Fix VM-menu MENU.
If EACH it t, then replace `vm-print-message' by
'vm-ps-print-each-message', otherwise by `vm-ps-print-message'."
  (let ((tmpbuf (get-buffer-create "*vm-ps-print*")))
    (save-excursion
      (set-buffer tmpbuf)
      (erase-buffer)
      (insert (format "(setq %s '%S)" (symbol-name menu) (symbol-value menu)))
      (if (re-search-backward "vm-\\(ps-\\)?print-\\(each-\\)?message"
			      (point-min) t)
	  (if each (replace-match "vm-print-each-message")
	    (replace-match "vm-ps-print-message")))
      (eval-buffer)
      (kill-buffer tmpbuf)
      )))

;;;###autoload
(defun vm-ps-print-message-infect-vm (&optional each)
  "Call this function to hook the ps-printing functions into VM.
Arranges that the usual VM printing commands in menus and the
toolbar use `vm-ps-print-message' or `vm-ps-print-each-message'
(when EACH is t) instead of `vm-print-message'."
  (interactive)
  (if each (fset 'vm-toolbar-print-command 'vm-ps-print-each-message)
    (fset 'vm-toolbar-print-command 'vm-ps-print-message))
  (require 'vm-version)
  (require 'vm-menu)
  (vm-ps-print-message-fix-menu 'vm-menu-dispose-menu each)
  (vm-ps-print-message-fix-menu 'vm-menu-vm-menu each)
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; From: "Jeffrey J. Kosowsky" <jeff.kosowsky_ATsign_verizon_DOTsymbol_net>
;;;###autoload
(defun vm-ps-print-marked (&optional filename seperate nup color)
  "Postscript print all marked emails in mail Summary. If no messages marked,
print just the current message.
Optionally write postscript output to FILENAME (default is to spool
to printer). 
Optionally force SEPERATE printing of each message by setting to 't'. 
Optionally also print NUP pages per sheet.
Optionally also print in COLOR by setting to non-nil.

Note when run interactively setting a positive prefix number prints
NUP pages per sheet to  the printer, while negative number prints NUP
pages per sheet to queried FILENAME. No prefix prints 1 page per sheet
to printer while prefix without numerical argument simply queries for
filename and formats 1 page per sheet. (JJK)"  
  (interactive
   (if (and (integerp current-prefix-arg) (plusp current-prefix-arg))
       nil
     (list (ps-print-preprint current-prefix-arg))))
  (let ((last-command)
        (ps-print-color-p color)
        (ps-n-up-printing
         (cond
          (nup nup)
          ((integerp current-prefix-arg) (abs current-prefix-arg))
          (t 1)))                       ; default 1 page per sheet
        )
    (and (vm-marked-messages)
         (setq last-command 'vm-next-command-uses-marks))
    (vm-ps-print-message nil filename seperate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vm-ps-print)

;;; vm-ps-print.el ends here
