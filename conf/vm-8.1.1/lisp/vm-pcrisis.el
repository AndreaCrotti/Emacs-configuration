;;; vm-pcrisis.el --- wide-ranging auto-setup for personalities in VM
;;
;; Copyright (C) 1999 Rob Hodges,
;;               2006 Robert Widhopf, Robert P. Goldman
;;
;; Package: Personality Crisis for VM
;; Author: Rob Hodges
;;
;; Maintainer: Robert Widhopf-Fenk <hack@robf.de>
;; X-URL:       http://www.robf.de/Hacking/elisp
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;; DOCUMENTATION:
;; -------------
;;
;; Documentation is now in Texinfo and HTML formats.  You should have
;; downloaded one or the other along with this package at the URL
;; above.

;;; Code:

(eval-when-compile
  (require 'vm-version)
  (require 'vm-message)
  (require 'vm-macro)
  (require 'vm-reply)
  ;; get the macros we need.
  (require 'cl)
  (require 'advice)
  (condition-case e
      (progn 
        (require 'regexp-opt)
        (require 'bbdb)
        (require 'bbdb-com))
    (error
     (message "%S" e)
     (message "Could not load bbdb.el.  Related functions may not work correctly!")
     (sit-for 5))))

;; Dummy declarations for variables that are defined in bbdb

(defvar bbdb-records)
(defvar bbdb-file)
(defvar bbdb-records)

;; -------------------------------------------------------------------
;; Variables:
;; -------------------------------------------------------------------
(defconst vmpc-version "0.9.1"
  "Version of pcrisis.")

(defgroup vmpc nil
  "Manage personalities and more in VM."
  :group  'vm)

(defcustom vmpc-conditions ()
  "*List of conditions which will be checked by pcrisis."
  :group 'vmpc)

(defcustom vmpc-actions ()
  "*List of actions.
Actions are associated with conditions from `vmpc-conditions' by one of
`vmpc-actions-alist', `vmpc-reply-alist', `', `vmpc-forward-alist',
`vmpc-resend-alist',  `vmpc-newmail-alist' or `vmpc-automorph-alist'.

These are also the actions from which you can choose when using the newmail
features of Personality Crisis, or the `vmpc-prompt-for-profile' action.

You may also define an action without associated commands, e.g. \"none\"."
  :type '(repeat (list (string :tag "Action name")
                       (sexp :tag "Commands")))
  :group 'vmpc)

(defun vmpc-alist-set (symbol value)
  "Used as :set for vmpc-*-alist variables.
Checks if the condition and all the actions exist."
  (while value
    (let ((condition (caar value))
          (actions   (cdar value)))
      (if (and condition (not (assoc condition vmpc-conditions)))
          (error "Condition '%s' does not exist!" condition))
      (while actions 
        (if (not (assoc (car actions) vmpc-actions))
            (error "Action '%s' does not exist!" (car actions)))
        (setq actions (cdr actions))))
    (setq value (cdr value)))
  (set symbol value))

(defun vmpc-defcustom-alist-type ()
  "Generate :type for vmpc-*-alist variables."
  (list 'repeat
        (list 'list 
              (append '(choice :tag "Condition")
                      (mapcar (lambda (c) (list 'const (car c))) vmpc-conditions)
                      '((string)))
              (list 'repeat :tag "Actions to run"
                    (append '(choice :tag "Action")
                            (mapcar (lambda (a) (list 'const (car a))) vmpc-actions)
                            '(string))))))

(defcustom vmpc-actions-alist ()
  "*An alist associating conditions with actions from `vmpc-actions'.
If you do not want to map actions for each state, e.g. for replying, forwarding,
resending, composing or automorphing, then set this one."
  :type (vmpc-defcustom-alist-type)
;  :set 'vmpc-alist-set
  :group 'vmpc)

(defcustom vmpc-reply-alist ()
  "*An alist associating conditions with actions from `vmpc-actions' when replying."
  :type (vmpc-defcustom-alist-type)
;  :set 'vmpc-alist-set
  :group 'vmpc)

(defcustom vmpc-forward-alist ()
  "*An alist associating conditions with actions from `vmpc-actions' when forwarding."
  :type (vmpc-defcustom-alist-type)
;  :set 'vmpc-alist-set
  :group 'vmpc)

(defcustom vmpc-automorph-alist ()
  "*An alist associating conditions with actions from `vmpc-actions' when automorphing."
  :type (vmpc-defcustom-alist-type)
;  :set 'vmpc-alist-set
  :group 'vmpc)

(defcustom vmpc-newmail-alist ()
  "*An alist associating conditions with actions from `vmpc-actions' when composing."
  :type (vmpc-defcustom-alist-type)
;  :set 'vmpc-alist-set
  :group 'vmpc)

(defcustom vmpc-resend-alist ()
  "*An alist associating conditions with actions from `vmpc-actions' when resending."
  :type (vmpc-defcustom-alist-type)
;  :set 'vmpc-alist-set
  :group 'vmpc)

(defcustom vmpc-default-profile "default"
  "*The default profile to select if no profile was found."
  :type '(choice (const :tag "None" nil)
                 (string))
  :group 'vmpc)

(defcustom vmpc-auto-profiles-file "~/.vmpc-auto-profiles"
  "*File in which to save information used by `vmpc-prompt-for-profile'.
When set to the symbol 'BBDB, profiles will be stored there."
  :type '(choice (file)
                 (const BBDB))
  :group 'vmpc)

(defcustom vmpc-auto-profiles-expunge-days 100
  "*Number of days after which to expunge old address-profile associations.
Performance may suffer noticeably if this file becomes enormous, but in other
respects it is preferable for this value to be fairly high.  The value that is
right for you will depend on how often you send email to new addresses using
`vmpc-prompt-for-profile'."
  :type 'integer
  :group 'vmpc)

(defvar vmpc-current-state nil
  "The current state of pcrisis.
It is one of 'reply, 'forward, 'resend, 'automorph or 'newmail.
It controls which actions/functions can/will be run.")

(defvar vmpc-current-buffer nil
  "The current buffer, i.e. 'none or 'composition.
It is 'none before running an adviced VM function and 'composition afterward,
i.e. when within the composition buffer.")

(defvar vmpc-saved-headers-alist nil
  "Alist of headers from the original message saved for later use.")

(defvar vmpc-actions-to-run nil
  "The actions to run.")

(defvar vmpc-true-conditions nil
  "The true conditions.")

(defvar vmpc-auto-profiles nil
  "The auto profiles as stored in `vmpc-auto-profiles-file'.")

;; An "exerlay" is an overlay in FSF Emacs and an extent in XEmacs.
;; It's not a real type; it's just the way I'm dealing with the damn
;; things to produce containers for the signature and pre-signature
;; which can be highlighted etc. and work on both platforms.

(defvar vmpc-pre-sig-exerlay ()
  "Don't mess with this.")

(make-variable-buffer-local 'vmpc-pre-sig-exerlay)

(defvar vmpc-sig-exerlay ()
  "Don't mess with this.")

(make-variable-buffer-local 'vmpc-sig-exerlay)

(defvar vmpc-pre-sig-face (progn (make-face 'vmpc-pre-sig-face
	    "Face used for highlighting the pre-signature.")
				 (set-face-foreground
				  'vmpc-pre-sig-face "forestgreen")
				 'vmpc-pre-sig-face)
  "Face used for highlighting the pre-signature.")

(defvar vmpc-sig-face (progn (make-face 'vmpc-sig-face
		"Face used for highlighting the signature.")
			     (set-face-foreground 'vmpc-sig-face
						  "steelblue")
			     'vmpc-sig-face)
  "Face used for highlighting the signature.")

(defvar vmpc-intangible-pre-sig 'nil
  "Whether to forbid the cursor from entering the pre-signature.")

(defvar vmpc-intangible-sig 'nil
  "Whether to forbid the cursor from entering the signature.")

(defvar vmpc-expect-default-signature 'nil
  "*Set this to 't if you have a signature-inserting function.
It will ensure that pcrisis correctly handles the signature .")


;; -------------------------------------------------------------------
;; Some easter-egg functionality:
;; -------------------------------------------------------------------

(defun vmpc-my-identities (&rest identities)
  "Setup pcrisis with the given IDENTITIES."
  (setq vmpc-conditions    '(("always true" t))
        vmpc-actions-alist '(("always true" "prompt for a profile"))
        vmpc-actions       '(("prompt for a profile" (vmpc-prompt-for-profile t t))))
  (setq vmpc-actions
        (append (mapcar
                 (lambda (i)
                   (list i (list 'vmpc-substitute-header "From" i)))
                 identities)
                vmpc-actions)))

(defun vmpc-header-field-for-point ()
  "*Return a string indicating the mail header field point is in.
If point is not in a header field, returns nil."
  (save-excursion
    (unless (save-excursion
	      (re-search-backward (regexp-quote mail-header-separator)
				  (point-min) t))
      (re-search-backward "^\\([^ \t\n:]+\\):")
      (match-string 1))))

(defun vmpc-tab-header-or-tab-stop (&optional backward)
  "*If in a mail header field, moves to next useful header or body.
When moving to the message body, calls the `vmpc-automorph' function.
If within the message body, runs `tab-to-tab-stop'.
If BACKWARD is specified and non-nil, moves to previous useful header
field, whether point is in the body or the headers.
\"Useful header fields\" are currently, in order, \"To\" and
\"Subject\"."
  (interactive)
  (let ((curfield) (nextfield) (useful-headers '("To" "Subject")))
    (if (or (setq curfield (vmpc-header-field-for-point))
	    backward)
	(progn
	  (setq nextfield
		(- (length useful-headers)
		   (length (member curfield useful-headers))))
	  (if backward
	      (setq nextfield (nth (1- nextfield) useful-headers))
	    (setq nextfield (nth (1+ nextfield) useful-headers)))
	  (if nextfield
	      (mail-position-on-field nextfield)
	    (mail-text)
	    (vmpc-automorph))
	  )
      (tab-to-tab-stop)
      )))

(defun vmpc-backward-tab-header-or-tab-stop ()
  "*Wrapper for `vmpc-tab-header-or-tab-stop' with BACKWARD set."
  (interactive)
  (vmpc-tab-header-or-tab-stop t))


;; -------------------------------------------------------------------
;; Stuff for dealing with exerlays:
;; -------------------------------------------------------------------

(defun vmpc-set-overlay-insertion-types (overlay start end)
  "Set insertion types for OVERLAY from START to END.
In fact a new copy of OVERLAY with different insertion types at START and END
is created and returned.

START and END should be nil or t -- the marker insertion types at the start
and end.  This seems to be the only way you of changing the insertion types
for an overlay -- save the overlay properties that we care about, create a new
overlay with the new insertion types, set its properties to the saved ones.
Overlays suck.  Extents rule.  XEmacs got this right."
  (let* ((useful-props (list 'face 'intangible 'evaporate)) (saved-props)
	 (i 0) (len (length useful-props)) (startpos) (endpos) (new-ovl))
    (while (< i len)
      (setq saved-props (append saved-props (cons
		       (overlay-get overlay (nth i useful-props)) ())))
      (setq i (1+ i)))
    (setq startpos (overlay-start overlay))
    (setq endpos (overlay-end overlay))
    (delete-overlay overlay)
    (if (and startpos endpos)
	(setq new-ovl (make-overlay startpos endpos (current-buffer)
				    start end))
      (setq new-ovl (make-overlay 1 1 (current-buffer) start end))
      (vmpc-forcefully-detach-exerlay new-ovl))
    (setq i 0)
    (while (< i len)
      (overlay-put new-ovl (nth i useful-props) (nth i saved-props))
      (setq i (1+ i)))
    new-ovl))


(defun vmpc-set-extent-insertion-types (extent start end)
  "Set the insertion types of EXTENT from START to END.
START and END should be either nil or t, indicating the desired value
of the 'start-open and 'end-closed properties of the extent
respectively.
This is the XEmacs version of `vmpc-set-overlay-insertion-types'."
  ;; pretty simple huh?
  (set-extent-property extent 'start-open start)
  (set-extent-property extent 'end-closed end))


(defun vmpc-set-exerlay-insertion-types (exerlay start end)
  "Set the insertion types for EXERLAY from START to END.
In other words, EXERLAY is the name of the overlay or extent with a quote in
front.  START and END are the equivalent of the marker insertion types for the
start and end of the overlay/extent."
  (if vm-xemacs-p
      (vmpc-set-extent-insertion-types (symbol-value exerlay) start end)
    (set exerlay (vmpc-set-overlay-insertion-types (symbol-value exerlay)
						   start end))))


(defun vmpc-exerlay-start (exerlay)
  "Return buffer position of the start of EXERLAY."
  (if vm-xemacs-p
      (extent-start-position exerlay)
    (overlay-start exerlay)))


(defun vmpc-exerlay-end (exerlay)
  "Return buffer position of the end of EXERLAY."
  (if vm-xemacs-p
      (extent-end-position exerlay)
    (overlay-end exerlay)))


(defun vmpc-move-exerlay (exerlay new-start new-end)
  "Change EXERLAY to cover region from NEW-START to NEW-END."
  (if vm-xemacs-p
      (set-extent-endpoints exerlay new-start new-end (current-buffer))
    (move-overlay exerlay new-start new-end (current-buffer))))


(defun vmpc-set-exerlay-detachable-property (exerlay newval)
  "Set the 'detachable or 'evaporate property for EXERLAY to NEWVAL."
  (if vm-xemacs-p
      (set-extent-property exerlay 'detachable newval)
    (overlay-put exerlay 'evaporate newval)))


(defun vmpc-set-exerlay-intangible-property (exerlay newval)
  "Set the 'intangible or 'atomic property for EXERLAY to NEWVAL."
  (if vm-xemacs-p
      (progn
	(require 'atomic-extents)
	(set-extent-property exerlay 'atomic newval))
    (overlay-put exerlay 'intangible newval)))


(defun vmpc-set-exerlay-face (exerlay newface)
  "Set the face used by EXERLAY to NEWFACE."
  (if vm-xemacs-p
      (set-extent-face exerlay newface)
    (overlay-put exerlay 'face newface)))


(defun vmpc-forcefully-detach-exerlay (exerlay)
  "Leave EXERLAY in memory but detaches it from the buffer."
  (if vm-xemacs-p
      (detach-extent exerlay)
    (delete-overlay exerlay)))


(defun vmpc-make-exerlay (startpos endpos)
  "Create a new exerlay spanning from STARTPOS to ENDPOS."
  (if vm-xemacs-p
      (make-extent startpos endpos (current-buffer))
    (make-overlay startpos endpos (current-buffer))))


(defun vmpc-create-sig-and-pre-sig-exerlays ()
  "Create the extents in which the pre-sig and sig can reside.
Or overlays, in the case of GNU Emacs.  Thus, exerlays."
  (setq vmpc-pre-sig-exerlay (vmpc-make-exerlay 1 2))
  (setq vmpc-sig-exerlay (vmpc-make-exerlay 3 4))

  (vmpc-set-exerlay-detachable-property vmpc-pre-sig-exerlay t)
  (vmpc-set-exerlay-detachable-property vmpc-sig-exerlay t)
  (vmpc-forcefully-detach-exerlay vmpc-pre-sig-exerlay)
  (vmpc-forcefully-detach-exerlay vmpc-sig-exerlay)

  (vmpc-set-exerlay-face vmpc-pre-sig-exerlay 'vmpc-pre-sig-face)
  (vmpc-set-exerlay-face vmpc-sig-exerlay 'vmpc-sig-face)

  (vmpc-set-exerlay-intangible-property vmpc-pre-sig-exerlay
					vmpc-intangible-pre-sig)
  (vmpc-set-exerlay-intangible-property vmpc-sig-exerlay
					vmpc-intangible-sig)
  
  (vmpc-set-exerlay-insertion-types 'vmpc-pre-sig-exerlay t nil)
  (vmpc-set-exerlay-insertion-types 'vmpc-sig-exerlay t nil)

  ;; deal with signatures inserted by other things than vm-pcrisis:
  (if vmpc-expect-default-signature
      (save-excursion
	(let ((p-max (point-max))
	      (body-start (save-excursion (mail-text) (point)))
	      (sig-start nil))
	  (goto-char p-max)
	  (setq sig-start (re-search-backward "\n-- \n" body-start t))
	  (if sig-start
	      (vmpc-move-exerlay vmpc-sig-exerlay sig-start p-max))))))
  

;; -------------------------------------------------------------------
;; Functions for vmpc-actions:
;; -------------------------------------------------------------------

(defmacro vmpc-composition-buffer (&rest form)
  "Evaluate FORM if in the composition buffer.
That is to say, evaluates the form if you are really in a composition
buffer.  This function should not be called directly, only from within
the `vmpc-actions' list."
  (list 'if '(eq vmpc-current-buffer 'composition)
        (list 'eval (cons 'progn form))))

(put 'vmpc-composition-buffer 'lisp-indent-hook 'defun)

(defmacro vmpc-pre-function (&rest form)
  "Evaluate FORM if in pre-function state.
That is to say, evaluates the FORM before VM does its thing, whether
that be creating a new mail or a reply.  This function should not be
called directly, only from within the `vmpc-actions' list."
  (list 'if '(and (eq vmpc-current-buffer 'none)
                  (not (eq vmpc-current-state 'automorph)))
        (list 'eval (cons 'progn form))))

(put 'vmpc-pre-function 'lisp-indent-hook 'defun)

(defun vmpc-delete-header (hdrfield &optional entire)
  "Delete the contents of a HDRFIELD in the current mail message.
If ENTIRE is specified and non-nil, deletes the header field as well."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
	(let ((start) (end))
	  (mail-position-on-field hdrfield)
	  (if entire
	      (setq end (+ (point) 1))
	    (setq end (point)))
	  (re-search-backward ": ")
	  (if entire
	      (setq start (progn (beginning-of-line) (point)))
	    (setq start (+ (point) 2)))
	  (delete-region start end)))))


(defun vmpc-insert-header (hdrfield content)
  "Insert to HDRFIELD the new CONTENT.
Both arguments are strings.  The field can either be present or not,
but if present, HDRCONT will be appended to the current header
contents."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
	(mail-position-on-field hdrfield)
	(insert content))))

(defun vmpc-substitute-header (hdrfield content)
  "Substitute HDRFIELD with new CONTENT.
Both arguments are strings.  The field can either be present or not.
If the header field is present and already contains something, the
contents will be replaced, otherwise a new header is created."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
	(vmpc-delete-header hdrfield)
	(vmpc-insert-header hdrfield content))))

(defun vmpc-add-header (hdrfield content)
  "Add HDRFIELD with CONTENT if it is not present already.
Both arguments are strings.  
If a header field with the same CONTENT is present already nothing will be
done, otherwise  a new field with the same name and the new CONTENT will be
added to the message.

This is suitable for FCC, which can be specified multiple times."
  (unless (eq vmpc-current-buffer 'composition)
    (error "attempting to insert a header into a non-composition buffer."))
  (let ((prev-contents (vmpc-get-header-contents hdrfield "\n")))
    (setq prev-contents (vmpc-split prev-contents "\n"))
    ;; don't add this new header if it's already there
    (unless (member content prev-contents)
      (save-excursion
	(or (mail-position-on-field hdrfield t)	; Put new field after existing one
	    (mail-position-on-field "to"))
	(unless (eq (aref hdrfield (1- (length hdrfield))) ?:)
	  (setq hdrfield (concat hdrfield ":")))
	(insert "\n" hdrfield " ")
	(insert content)))))

(defun vmpc-get-current-header-contents (hdrfield &optional clump-sep)
  "Return the contents of HDRFIELD in the current mail message.
Returns an empty string if the header doesn't exist.  HDRFIELD should
be a string.  If the string CLUMP-SEP is specified, it means to return
the contents of all headers matching the regexp HDRFIELD, separated by
CLUMP-SEP."
  ;; This code is based heavily on vm-get-header-contents and vm-match-header.
  ;; Thanks Kyle :)
  (if (eq vmpc-current-state 'automorph)
      (save-excursion
	(let ((contents nil) (header-name-regexp "\\([^ \t\n:]+\\):")
	      (case-fold-search t) (temp-contents) (end-of-headers) (regexp))
          (if (not (listp hdrfield))
              (setq hdrfield (list hdrfield)))
	  ;; find the end of the headers:
	  (goto-char (point-min))
	  (or (re-search-forward
               (concat "^\\(" (regexp-quote mail-header-separator) "\\)$")
               nil t)
              (error "Cannot find mail-header-separator %S in buffer %S"
                     mail-header-separator (current-buffer)))
	  (setq end-of-headers (match-beginning 0))
	  ;; now rip through finding all the ones we want:
          (while hdrfield
            (setq regexp (concat "^\\(" (car hdrfield) "\\)"))
            (goto-char (point-min))
            (while (and (or (null contents) clump-sep)
                        (re-search-forward regexp end-of-headers t)
                        (save-excursion
                          (goto-char (match-beginning 0))
                          (let (header-cont-start header-cont-end)
                            (if (if (not clump-sep)
                                    (and (looking-at (car hdrfield))
                                         (looking-at header-name-regexp))
                                  (looking-at header-name-regexp))
                                (save-excursion
                                  (goto-char (match-end 0))
                                  ;; skip leading whitespace
                                  (skip-chars-forward " \t")
                                  (setq header-cont-start (point))
                                  (forward-line 1)
                                  (while (looking-at "[ \t]")
                                    (forward-line 1))
                                  ;; drop the trailing newline
                                  (setq header-cont-end (1- (point)))))
                            (setq temp-contents
                                  (buffer-substring header-cont-start
                                                    header-cont-end)))))
              (if contents
                  (setq contents
                        (concat contents clump-sep temp-contents))
                (setq contents temp-contents)))
            (setq hdrfield (cdr hdrfield)))

	  (if (null contents)
	      (setq contents ""))
	  contents ))))

(defun vmpc-get-current-body-text ()
  "Return the body text of the mail message in the current buffer."
  (if (eq vmpc-current-state 'automorph)
      (save-excursion
	(goto-char (point-min))
	(let ((start (re-search-forward
		      (concat "^" (regexp-quote mail-header-separator) "$")))
	      (end (point-max)))
	  (buffer-substring start end)))))


(defun vmpc-get-replied-header-contents (hdrfield &optional clump-sep)
  "Return the contents of HDRFIELD in the message being replied to.
If that header does not exist, returns an empty string.  If the string
CLUMP-SEP is specified, treat HDRFIELD as a regular expression and
return the contents of all header fields which match that regexp,
separated from each other by CLUMP-SEP."
  (if (and (eq vmpc-current-buffer 'none)
	   (memq vmpc-current-state '(reply forward resend)))
      (let ((mp (car (vm-select-marked-or-prefixed-messages 1)))
            content c)
        (if (not (listp hdrfield))
           (setq hdrfield (list hdrfield)))
        (while hdrfield
          (setq c (vm-get-header-contents mp (car hdrfield) clump-sep))
          (if c (setq content (cons c content)))
          (setq hdrfield (cdr hdrfield)))
        (or (mapconcat 'identity content "\n") ""))))

(defun vmpc-get-header-contents (hdrfield &optional clump-sep)
 "Return the contents of HDRFIELD."
 (cond ((and (eq vmpc-current-buffer 'none)
             (memq vmpc-current-state '(reply forward resend)))
        (vmpc-get-replied-header-contents hdrfield clump-sep))
       ((eq vmpc-current-state 'automorph)
        (vmpc-get-current-header-contents hdrfield clump-sep))))

(defun vmpc-get-replied-body-text ()
  "Return the body text of the message being replied to."
  (if (and (eq vmpc-current-buffer 'none)
	   (memq vmpc-current-state '(reply forward resend)))
      (save-excursion
	(let* ((mp (car (vm-select-marked-or-prefixed-messages 1)))
	       (message (vm-real-message-of mp))
	       start end)
	  (set-buffer (vm-buffer-of message))
	  (save-restriction
	    (widen)
	    (setq start (vm-text-of message))
	    (setq end (vm-end-of message))
	    (buffer-substring start end))))))

(defun vmpc-save-replied-header (hdrfield)
  "Save the contents of HDRFIELD in `vmpc-saved-headers-alist'.
Does nothing if that header doesn't exist."
  (let ((hdrcont (vmpc-get-replied-header-contents hdrfield)))
  (if (and (eq vmpc-current-buffer 'none)
	   (memq vmpc-current-state '(reply forward resend))
	   (not (equal hdrcont "")))
      (add-to-list 'vmpc-saved-headers-alist (cons hdrfield hdrcont)))))

(defun vmpc-get-saved-header (hdrfield)
  "Return the contents of HDRFIELD from `vmpc-saved-headers-alist'.
The alist in question is created by `vmpc-save-replied-header'."
  (if (and (eq vmpc-current-buffer 'composition)
	   (memq vmpc-current-state '(reply forward resend)))
      (cdr (assoc hdrfield vmpc-saved-headers-alist))))

(defun vmpc-substitute-replied-header (dest src)
  "Substitute header DEST with content from SRC.
For example, if the address you want to send your reply to is the same
as the contents of the \"From\" header in the message you are replying
to, use (vmpc-substitute-replied-header \"To\" \"From\"."
  (if (memq vmpc-current-state '(reply forward resend))
      (progn
	(if (eq vmpc-current-buffer 'none)
	    (vmpc-save-replied-header src))
	(if (eq vmpc-current-buffer 'composition)
	    (vmpc-substitute-header dest (vmpc-get-saved-header src))))))

(defun vmpc-get-header-extents (hdrfield)
  "Return buffer positions (START . END) for the contents of HDRFIELD.
If HDRFIELD does not exist, return nil."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
        (let ((header-name-regexp "^\\([^ \t\n:]+\\):") (start) (end))
          (setq end
                (if (mail-position-on-field hdrfield t)
                    (point)
                  nil))
          (setq start
                (if (re-search-backward header-name-regexp (point-min) t)
                    (match-end 0)
                  nil))
          (and start end (<= start end) (cons start end))))))

(defun vmpc-substitute-within-header
  (hdrfield regexp to-string &optional append-if-no-match sep)
  "Replace in HDRFIELD strings matched by  REGEXP with TO-STRING.
HDRFIELD need not exist.  TO-STRING may contain references to groups
within REGEXP, in the same manner as `replace-regexp'.  If REGEXP is
not found in the header contents, and APPEND-IF-NO-MATCH is t,
TO-STRING will be appended to the header contents (with HDRFIELD being
created if it does not exist).  In this case, if the string SEP is
specified, it will be used to separate the previous header contents
from TO-STRING, unless HDRFIELD has just been created or was
previously empty."
  (if (eq vmpc-current-buffer 'composition)
      (save-excursion
        (let ((se (vmpc-get-header-extents hdrfield)) (found))
          (if se
              ;; HDRFIELD exists
              (save-restriction
                (narrow-to-region (car se) (cdr se))
                (goto-char (point-min))
                (while (re-search-forward regexp nil t)
                  (setq found t)
                  (replace-match to-string))
                (if (and (not found) append-if-no-match)
                    (progn
                      (goto-char (cdr se))
                      (if (and sep (not (equal (car se) (cdr se))))
                          (insert sep))
                      (insert to-string))))
            ;; HDRFIELD does not exist
            (if append-if-no-match
                (progn
                  (mail-position-on-field hdrfield)
                  (insert to-string))))))))


(defun vmpc-replace-or-add-in-header (hdrfield regexp hdrcont &optional sep)
  "Replace in HDRFIELD the match of REGEXP with HDRCONT.
All arguments are strings.  The field can either be present or not.
If the header field is present and already contains something, HDRCONT
will be appended and if SEP is none nil it will be used as separator.

I use this function to modify recipients in the TO-header.
e.g.
 (vmpc-replace-or-add-in-header \"To\" \"[Rr]obert Fenk[^,]*\"
                                     \"Robert Fenk\" \", \"))"
  (if (eq vmpc-current-buffer 'composition)
      (let ((hdr (vmpc-get-current-header-contents hdrfield))
            (old-point (point)))
        (if hdr
            (progn
              (vmpc-delete-header hdrfield)
              (if (string-match regexp hdr)
                  (setq hdr (vm-replace-in-string hdr regexp hdrcont))
                (setq hdr (if sep (concat hdr sep hdrcont)
                            (concat hdr hdrcont))))
              (vmpc-insert-header hdrfield hdr)
              (goto-char old-point))
          ))))

(defun vmpc-insert-signature (sig &optional pos)
  "Insert SIG at the end of `vmpc-sig-exerlay'.
SIG is a string.  If it is the name of a file, its contents is inserted --
otherwise the string itself is inserted.  Optional parameter POS means insert
the signature at POS if `vmpc-sig-exerlay' is detached."
  (if (eq vmpc-current-buffer 'composition)
      (progn
	(let ((end (or (vmpc-exerlay-end vmpc-sig-exerlay) pos)))
	  (save-excursion
	    (vmpc-set-exerlay-insertion-types 'vmpc-sig-exerlay nil t)
	    (vmpc-set-exerlay-detachable-property vmpc-sig-exerlay nil)
	    (vmpc-set-exerlay-intangible-property vmpc-sig-exerlay nil)
	    (unless end
	      (setq end (point-max))
	      (vmpc-move-exerlay vmpc-sig-exerlay end end))
	    (if (and pos (not (vmpc-exerlay-end vmpc-sig-exerlay)))
		(vmpc-move-exerlay vmpc-sig-exerlay pos pos))
	    (goto-char end)
	    (insert "\n-- \n")
	    (if (and (file-exists-p sig)
		     (file-readable-p sig)
		     (not (equal sig "")))
		(insert-file-contents sig)
	      (insert sig)))
	  (vmpc-set-exerlay-intangible-property vmpc-sig-exerlay
						vmpc-intangible-sig)
	  (vmpc-set-exerlay-detachable-property vmpc-sig-exerlay t)
	  (vmpc-set-exerlay-insertion-types 'vmpc-sig-exerlay t nil)))))
    

(defun vmpc-delete-signature ()
  "Deletes the contents of `vmpc-sig-exerlay'."
  (when (and (eq vmpc-current-buffer 'composition)
             ;; make sure it's not detached first:
             (vmpc-exerlay-start vmpc-sig-exerlay))
    (delete-region (vmpc-exerlay-start vmpc-sig-exerlay)
                   (vmpc-exerlay-end vmpc-sig-exerlay))
    (vmpc-forcefully-detach-exerlay vmpc-sig-exerlay)))


(defun vmpc-signature (sig)
  "Remove a current signature if present, and replace it with SIG.
If the string SIG is the name of a readable file, its contents are
inserted as the signature; otherwise SIG is inserted literally.  If
SIG is the empty string (\"\"), the current signature is deleted if
present, and that's all."
  (if (eq vmpc-current-buffer 'composition)
      (let ((pos (vmpc-exerlay-start vmpc-sig-exerlay)))
	(save-excursion
	  (vmpc-delete-signature)
	  (if (not (equal sig ""))
	      (vmpc-insert-signature sig pos))))))
  

(defun vmpc-insert-pre-signature (pre-sig &optional pos)
  "Insert PRE-SIG at the end of `vmpc-pre-sig-exerlay'.
PRE-SIG is a string.  If it's the name of a file, the file's contents
are inserted; otherwise the string itself is inserted.  Optional
parameter POS means insert the pre-signature at position POS if
`vmpc-pre-sig-exerlay' is detached."
  (if (eq vmpc-current-buffer 'composition)
      (progn
	(let ((end (or (vmpc-exerlay-end vmpc-pre-sig-exerlay) pos))
	      (sigstart (vmpc-exerlay-start vmpc-sig-exerlay)))
	  (save-excursion
	    (vmpc-set-exerlay-insertion-types 'vmpc-pre-sig-exerlay nil t)
	    (vmpc-set-exerlay-detachable-property vmpc-pre-sig-exerlay nil)
	    (vmpc-set-exerlay-intangible-property vmpc-pre-sig-exerlay nil)
	    (unless end
	      (if sigstart
		  (setq end sigstart)
		(setq end (point-max)))
	      (vmpc-move-exerlay vmpc-pre-sig-exerlay end end))
	    (if (and pos (not (vmpc-exerlay-end vmpc-pre-sig-exerlay)))
		(vmpc-move-exerlay vmpc-pre-sig-exerlay pos pos))
	    (goto-char end)
	    (insert "\n")
	    (if (and (file-exists-p pre-sig)
		     (file-readable-p pre-sig)
		     (not (equal pre-sig "")))
		(insert-file-contents pre-sig)
	      (insert pre-sig))))
	(vmpc-set-exerlay-intangible-property vmpc-pre-sig-exerlay
					      vmpc-intangible-pre-sig)
	(vmpc-set-exerlay-detachable-property vmpc-pre-sig-exerlay t)
	(vmpc-set-exerlay-insertion-types 'vmpc-pre-sig-exerlay t nil))))


(defun vmpc-delete-pre-signature ()
  "Deletes the contents of `vmpc-pre-sig-exerlay'."
  ;; make sure it's not detached first:
  (if (eq vmpc-current-buffer 'composition)
      (if (vmpc-exerlay-start vmpc-pre-sig-exerlay)
	  (progn
	    (delete-region (vmpc-exerlay-start vmpc-pre-sig-exerlay)
			   (vmpc-exerlay-end vmpc-pre-sig-exerlay))
	    (vmpc-forcefully-detach-exerlay vmpc-pre-sig-exerlay)))))


(defun vmpc-pre-signature (pre-sig)
  "Insert PRE-SIG at the end of `vmpc-pre-sig-exerlay' removing last pre-sig."
  (if (eq vmpc-current-buffer 'composition)
      (let ((pos (vmpc-exerlay-start vmpc-pre-sig-exerlay)))
	(save-excursion
	  (vmpc-delete-pre-signature)
	  (if (not (equal pre-sig ""))
	      (vmpc-insert-pre-signature pre-sig pos))))))


(defun vmpc-gregorian-days ()
  "Return the number of days elapsed since December 31, 1 B.C."
  ;; this code stolen from gnus-util.el :)
  (let ((tim (decode-time (current-time))))
    (timezone-absolute-from-gregorian
     (nth 4 tim) (nth 3 tim) (nth 5 tim))))


(defun vmpc-load-auto-profiles ()
  "Initialise `vmpc-auto-profiles' from `vmpc-auto-profiles-file'."
  (interactive)
  (setq vmpc-auto-profiles nil)
  (if (eq vmpc-auto-profiles-file 'BBDB)
      (let ((records (bbdb-with-db-buffer bbdb-records))
            profile rec nets)
        (while records
          (setq rec (car records)
                profile (bbdb-get-field rec 'vmpc-profile))
          (when (and profile (> (length profile) 0))
            (setq nets (bbdb-record-net rec))
            (while nets
              (setq vmpc-auto-profiles (cons (cons (car nets) (read profile))
                                             vmpc-auto-profiles)
                    nets (cdr nets))))
          (setq records (cdr records)))
        (setq vmpc-auto-profiles (reverse vmpc-auto-profiles)))
    (when (and (file-exists-p vmpc-auto-profiles-file) ;
               (file-readable-p vmpc-auto-profiles-file))
      (save-excursion
	(set-buffer (get-buffer-create "*pcrisis-temp*"))
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(insert-file-contents vmpc-auto-profiles-file)
	(goto-char (point-min))
	(setq vmpc-auto-profiles (read (current-buffer)))
	(kill-buffer (current-buffer))))))


(defun vmpc-save-auto-profiles ()
  "Save `vmpc-auto-profiles' to `vmpc-auto-profiles-file'."
  (when (not (eq vmpc-auto-profiles-file 'BBDB))
    (if (not (file-writable-p vmpc-auto-profiles-file))
        ;; if file is not writable, signal an error:
        (error "Error: P-Crisis could not write to file %s"
               vmpc-auto-profiles-file))
    (save-excursion
      (set-buffer (get-buffer-create "*pcrisis-temp*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (goto-char (point-min))
;	(prin1 vmpc-auto-profiles (current-buffer))
      (pp vmpc-auto-profiles (current-buffer))
      (write-region (point-min) (point-max)
                    vmpc-auto-profiles-file nil 'quietly)
      (kill-buffer (current-buffer)))))
    
(defun vmpc-fix-auto-profiles-file ()
  "Change `vmpc-auto-profiles-file' to the format used by v0.82+."
  (interactive)
  (vmpc-load-auto-profiles)
  (let ((len (length vmpc-auto-profiles)) (i 0) (day))
    (while (< i len)
      (setq day (cddr (nth i vmpc-auto-profiles)))
      (if (consp day)
	  (setcdr (cdr (nth i vmpc-auto-profiles)) (car day)))
      (setq i (1+ i))))
  (vmpc-save-auto-profiles)
  (setq vmpc-auto-profiles ()))


(defun vmpc-migrate-profiles-to-BBDB ()
  "Migrate the profiles stored in `vmpc-auto-profiles-file' to the BBDB.

This will automatically create records if they do not exist and add the new
field `vmpc-profile' to the records which is a sexp not meant to be edited."
  (interactive)
  (if (eq vmpc-auto-profiles-file 'BBDB)
      (error "`vmpc-auto-profiles-file' has been migrated already."))
  (unless vmpc-auto-profiles
    (vmpc-load-auto-profiles))
  ;; create a BBDB backup
  (bbdb-save-db)
  (copy-file (expand-file-name bbdb-file)
             (concat (expand-file-name bbdb-file) "-vmpc-profile-migration-backup"))
  ;; now migrate the profiles 
  (let ((profiles vmpc-auto-profiles)
        (records (bbdb-with-db-buffer bbdb-records))
        p addr rec)
    (while profiles
      (setq p (car profiles)
            addr (car p)
            rec (car (bbdb-search records nil nil addr)))
      (when (not rec)
        (setq rec (bbdb-create-internal "?" nil addr nil nil nil)))
      (bbdb-record-putprop rec 'vmpc-profile (format "%S" (cdr p)))
      (setq profiles (cdr profiles))))
  ;; move old profiles file out of the way
  (rename-file vmpc-auto-profiles-file
               (concat vmpc-auto-profiles-file "-migrated-to-BBDB"))
  ;; switch to BBDB mode
  (customize-save-variable 'vmpc-auto-profiles-file 'BBDB)
  (message "`vmpc-auto-profiles-file' has been set to 'BBDB"))

(defun vmpc-get-profile-for-address (addr)
  "Return profile for ADDR."
  (unless vmpc-auto-profiles
    (vmpc-load-auto-profiles))
  ;; TODO: BBDB "normalizes" email addresses, i.e. before we had a one-to-one
  ;; mapping of address=>actions, now multiple actions may point to the same
  ;; list of actions.  So either we should update vmpc-auto-profiles upon
  ;; storing a new profile or directly search BBDB for it, which might be
  ;; slower!
  (let ((prof (cadr (assoc addr vmpc-auto-profiles))))
    (when prof
      ;; we found a profile for this address and we are still
      ;; using it -- so "touch" the record to ensure it stays
      ;; newer than vmpc-auto-profiles-expunge-days
      (setcdr (cdr (assoc addr vmpc-auto-profiles)) (vmpc-gregorian-days))
      (vmpc-save-auto-profiles))
    prof))


(defun vmpc-save-profile-for-address (addr actions)
  "Save the association ADDR => ACTIONS."
  (let ((today (vmpc-gregorian-days))
        (old-association (assoc addr vmpc-auto-profiles))
        profile)

    ;; we store the actions list and the durrent date
    (setq profile (append (list addr actions) today))

    ;; remove old profile
    (when old-association
      ;; now possibly delete it from the BBDB
      (setq vmpc-auto-profiles (delete old-association vmpc-auto-profiles))
      (when (and (eq vmpc-auto-profiles-file 'BBDB) (not actions))
        (let ((records (bbdb-with-db-buffer bbdb-records)) rec)
          (setq rec (bbdb-search records nil nil addr))
          (when rec
            (bbdb-record-putprop (car rec) 'vmpc-profile nil)))))

    ;; add new profile
    (when actions 
      (setq vmpc-auto-profiles (cons profile vmpc-auto-profiles))
      ;; now possibly add it to the BBDB
      (when (eq vmpc-auto-profiles-file 'BBDB)
        (let ((records (bbdb-with-db-buffer bbdb-records)) rec)
          (setq rec (car (bbdb-search records nil nil addr)))
          (when (not rec)
            (setq rec (bbdb-create-internal "?" nil addr nil nil nil)))
          (bbdb-record-putprop rec 'vmpc-profile (format "%S" (cdr profile))))))

    ;; expunge old stuff from the list:
    (when vmpc-auto-profiles-expunge-days
      (setq vmpc-auto-profiles
            (mapcar (lambda (p)
                      (if (> (- today (cddr p)) vmpc-auto-profiles-expunge-days)
                          nil
                        p))
                    vmpc-auto-profiles))
      (setq vmpc-auto-profiles (delete nil vmpc-auto-profiles)))

    ;; save the file 
    (vmpc-save-auto-profiles)))


(defun vmpc-string-extract-address (str)
  "Find the first email address in the string STR and return it.
If no email address in found in STR, returns nil."
  (if (string-match "[^ \t,<]+@[^ \t,>]+" str)
      (match-string 0 str)))

(defun vmpc-split (string separators)
  "Return a list by splitting STRING at SEPARATORS and trimming all whitespace."
  (let (result
        (not-separators (concat "^" separators)))
    (save-excursion
      (set-buffer (get-buffer-create " *split*"))
      (erase-buffer)
      (insert string)
      (goto-char (point-min))
      (while (progn
               (skip-chars-forward separators)
               (skip-chars-forward " \t\n\r")
               (not (eobp)))
        (let ((begin (point))
              p)
          (skip-chars-forward not-separators)
          (setq p (point))
          (skip-chars-backward " \t\n\r")
          (setq result (cons (buffer-substring begin (point)) result))
          (goto-char p)))
      (erase-buffer))
    (nreverse result)))

(defun vmpc-read-actions (prompt &optional default)
  "Read a list of actions to run and store it in `vmpc-actions-to-run'.
The special action \"none\" will result in an empty action list."
  (interactive (list "VMPC actions%s: "))
  (let ((actions ()) (read-count 0) a)
    (setq actions (vm-read-string 
                   (format prompt (if default (format " %s" default) ""))
                   (append '(("none")) vmpc-actions)
                   t))
    (if (string= actions "none")
        (setq actions nil)
      (if (string= actions "")
          (setq actions default)
        (setq actions (vmpc-split actions " "))
        (setq actions (reverse actions))))
    (when (interactive-p)
      (setq vmpc-actions-to-run actions)
      (message "VMPC actions to run: %S" actions))
    actions))

(defcustom vmpc-prompt-for-profile-headers
  '((composition ("To" "CC" "BCC"))
    (default     ("From" "Sender" "Reply-To" "From" "Resent-From")))
  "*List of headers to check for email addresses.

`vmpc-prompt-for-profile' will scan the given headers in the given order."
  :type '(repeat (list (choice (const default)
                               (const composition)
                               (const reply)
                               (const forward)
                               (const resent)
                               (const newmail))
                       (repeat (string :tag "Header"))))
  :group 'vmpc)

(defvar vmpc-profiles-history nil
  "History of profiles prompted for.")

(defun vmpc-read-profile (&optional require-match initial-contents default)
  "Read a profile and return it."
  (unless default
    (setq default (car vmpc-profiles-history)))
  (completing-read (format "VMPC profile%s: "
                           (if vmpc-profiles-history
                               (concat " (" default ")")
                             ""))
                   vmpc-auto-profiles
                   nil
                   require-match
                   initial-contents
                   'vmpc-profiles-history
                   default))

(defun vmpc-prompt-for-profile (&optional remember prompt)
  "Find a profile or prompt for it and add its actions to the list of actions.

A profile is an association between a recipient address and a set of the
actions named in `vmpc-actions'.  When entering the list of actions, one has
to press ENTER after each action and finish adding action by pressing ENTER
without an action.

The association is stored in `vmpc-auto-profiles-file' and in the future the
stored actions will automatically run for messages to that address.

REMEMBER can be set to t or 'prompt.  When set to 'prompt you will be asked if
you want to store the association.  When set to t a new profile will be stored
without asking.

Set PROMPT to t and you will be prompted each time, i.e. not only for unknown
profiles.  If you want to change the profile only explicitly, then omit the
PROMPT argument and call this function interactively in the composition buffer."
  (interactive (progn (setq vmpc-current-state 'automorph)
                      (list 'prompt t)))
    
  (if (or (and (eq vmpc-current-buffer 'none)
	       (not (eq vmpc-current-state 'automorph)))
	  (eq vmpc-current-state 'automorph))
      (let ((headers (or (assoc vmpc-current-buffer vmpc-prompt-for-profile-headers)
                         (assoc vmpc-current-state vmpc-prompt-for-profile-headers)
                         (assoc 'default vmpc-prompt-for-profile-headers)))
            addrs a old-actions actions dest)
        (setq headers (car (cdr headers)))
        ;; search also other headers for known addresses 
        (while (and headers (not actions))
          (setq addrs (vmpc-get-header-contents (car headers)))
          (if addrs (setq addrs (vmpc-split addrs  ",")))
          (while addrs
            (setq a (vmpc-string-extract-address (car addrs)))
            (if (vm-ignored-reply-to a)
                (setq a nil))
            (setq actions (append (vmpc-get-profile-for-address a) actions))
            (if (not dest) (setq dest a))
            (setq addrs (cdr addrs)))
          (setq headers (cdr headers)))

        (setq dest (or dest vmpc-default-profile (if prompt (vmpc-read-profile))))
        
        (unless actions 
          (setq actions (vmpc-get-profile-for-address dest)))

        ;; save action to detect a change
        (setq old-actions actions)
        
        (when dest
          ;; figure out which actions to run
          (when (or prompt (not actions))
            (setq actions (vmpc-read-actions
                           (format "Actions for \"%s\"%%s: " dest)
                           actions)))

          ;; fixed old style format where there was only a single action
          (unless (listp actions)
            (setq remember t)
            (setq actions (list actions)))

          ;; save the association of this profile with these actions if applicable
          (if (and (not (equal old-actions actions))
                   (or (eq remember t)
                       (and (eq remember 'prompt)
                            (if actions 
                                (y-or-n-p (format "Always run %s for \"%s\"? "
                                                  actions dest))
                              (if (vmpc-get-profile-for-address dest)
                                  (yes-or-no-p (format "Delete profile for \"%s\"? "
                                                       dest)))))))
              (vmpc-save-profile-for-address dest actions))
          
          ;; TODO: understand when vmpc-prompt-for-profile has to run actions 
          ;; if we are in automorph (actually being called from within an action)
          (if (eq vmpc-current-state 'automorph)
              (let ((vmpc-actions-to-run actions))
                (vmpc-run-actions))
            ;; otherwise add the actions to the end of the list as a side effect 
            (setq vmpc-actions-to-run (append vmpc-actions-to-run actions)))
	
          ;; return the actions, which makes the condition true if a profile exists 
          actions))))

;; -------------------------------------------------------------------
;; Functions for vmpc-conditions:
;; -------------------------------------------------------------------

(defun vmpc-none-true-yet (&optional &rest exceptions)
  "True if none of the previous evaluated conditions was true.
This is a condition that can appear in `vmpc-conditions'.  If EXCEPTIONS are
specified, it means none were true except those.  For example, if you wanted
to check whether no conditions had yet matched with the exception of the two
conditions named \"default\" and \"blah\", you would make the call like this:
  (vmpc-none-true-yet \"default\" \"blah\")
Then it will return true regardless of whether \"default\" and \"blah\" had
matched."
  (let ((lenex (length exceptions)) (lentc (length vmpc-true-conditions)))
    (cond
     ((> lentc lenex)
      'nil)
     ((<= lentc lenex)
      (let ((i 0) (j 0) (k 0))
	(while (< i lenex)
	  (setq k 0)
	  (while (< k lentc)
	    (if (equal (nth i exceptions) (nth k vmpc-true-conditions))
		(setq j (1+ j)))
	    (setq k (1+ k)))
	  (setq i (1+ i)))
	(if (equal j lentc)
	    't
	  'nil))))))

(defun vmpc-other-cond (condition)
  "Return true if the specified CONDITION in `vmpc-conditions' matched.
CONDITION can only be the name of a condition specified earlier in
`vmpc-conditions' -- that is to say, any conditions which follow the one
containing `vmpc-other-cond' will show up as not having matched, because they
haven't yet been checked when this one is checked."
  (member condition vmpc-true-conditions))

(defun vmpc-folder-match (regexp)
  "Return true if the current folder name matches REGEXP."
  (string-match regexp (buffer-name)))

(defun vmpc-header-match (hdrfield regexp &optional clump-sep num)
  "Return true if the contents of specified header HDRFIELD match REGEXP.
For automorph, this means the header in your message, when replying it means
the header in the message being replied to.

CLUMP-SEP is specified, treat HDRFIELD as a regular expression and
return the contents of all header fields which match that regexp,
separated from each other by CLUMP-SEP.

If NUM is specified return the match string NUM."
  (cond ((memq vmpc-current-state '(reply forward resend))
         (let ((hdr (vmpc-get-replied-header-contents hdrfield clump-sep)))
           (and hdr (string-match regexp hdr)
                (if num (match-string num hdr) t))))
        ((eq vmpc-current-state 'automorph)
         (let ((hdr (vmpc-get-current-header-contents hdrfield clump-sep)))
           (and (string-match regexp hdr)
                (if num (match-string num hdr) t))))))

(defun vmpc-only-from-match (hdrfield regexp &optional clump-sep)
  "Return non-nil if all emails from the given HDRFIELD are matched by REGEXP."
  (let* ((content (vmpc-get-header-contents hdrfield clump-sep))
         (case-fold-search t)
         (pos 0)
         (len (length content))
         (only-from (not (null content))))
    (while (and only-from (< pos len)
                (setq pos (string-match "[a-z0-9._-]+@[a-z0-9._-]+" content pos)))
      (if (not (string-match regexp (match-string 0 content)))
          (setq only-from nil))
      (setq pos (1+ pos)))
    only-from))

(defun vmpc-body-match (regexp)
  "Return non-nil if the contents of the message body match REGEXP.
For automorph, this means the body of your message; when replying it means the
body of the message being replied to."
  (cond ((and (memq vmpc-current-state '(reply forward resend))
	      (eq vmpc-current-buffer 'none))
	 (string-match regexp (vmpc-get-replied-body-text)))
	((eq vmpc-current-state 'automorph)
	 (string-match regexp (vmpc-get-current-body-text)))))


(defun vmpc-xor (&rest args)
  "Return true if one and only one argument in ARGS is true."
  (= 1 (length (delete nil args))))

;; -------------------------------------------------------------------
;; Support functions for the advices:
;; -------------------------------------------------------------------

(defun vmpc-true-conditions ()
  "Return a list of all true conditions.
Run this function in order to test/check your conditions."
  (interactive)
  (let (vmpc-true-conditions
        vmpc-current-state
        vmpc-current-buffer)
    (if (eq major-mode 'vm-mail-mode)
        (setq vmpc-current-state 'automorph
              vmpc-current-buffer 'composition)
      (setq vmpc-current-state (intern (completing-read
                                        "VMPC state (default is 'reply): "
                                        '(("reply") ("forward") ("resend")
                                          ("newmail") ("automorph"))
                                        nil t nil nil "reply"))
            vmpc-current-buffer 'none))
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (vmpc-build-true-conditions-list)
    (message "VMPC true conditions: %S" vmpc-true-conditions)
    vmpc-true-conditions))

(defun vmpc-build-true-conditions-list ()
  "Built list of true conditions and store it in variable `vmpc-true-conditions'."
  (setq vmpc-true-conditions nil)
  (mapcar (lambda (c)
            (if (save-excursion (eval (cons 'progn (cdr c))))
                (setq vmpc-true-conditions (cons (car c) vmpc-true-conditions))))
          vmpc-conditions)
  (setq vmpc-true-conditions (reverse vmpc-true-conditions)))

(defun vmpc-build-actions-to-run-list ()
  "Built a list of the actions to run.
These are the true conditions mapped to actions.  Duplicates will be
eliminated.  You may run it in a composition buffer in order to see what
actions will be run."
  (interactive)
  (if (and (interactive-p) (not (member major-mode '(vm-mail-mode mail-mode))))
      (error "Run `vmpc-build-actions-to-run-list' in a composition buffer!"))
  (let ((alist (or (symbol-value (intern (format "vmpc-%s-alist"
                                                 vmpc-current-state)))
                   vmpc-actions-alist))
        (old-vmpc-actions-to-run vmpc-actions-to-run)
        actions)
    (setq vmpc-actions-to-run nil)
    (mapcar (lambda (c)
              (setq actions (cdr (assoc c alist)))
              ;; TODO: warn about unbound conditions?
              (while actions
                (if (not (member (car actions) vmpc-actions-to-run))
                    (setq vmpc-actions-to-run (cons (car actions) vmpc-actions-to-run)))
                (setq actions (cdr actions))))
            vmpc-true-conditions)
    (setq vmpc-actions-to-run (reverse vmpc-actions-to-run))
    (setq vmpc-actions-to-run (append vmpc-actions-to-run old-vmpc-actions-to-run)))
  (if (interactive-p)
      (message "VMPC actions to run: %S" vmpc-actions-to-run))
  vmpc-actions-to-run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vmpc-run-action (&optional action-regexp)
  "Run all actions with names matching the ACTION-REGEXP.
If called interactivly it promts for the regexp.  You may also use
completion."
  (interactive)
  (let ((action-names (mapcar '(lambda (a)
                                 (list (regexp-quote (car a)) 1))
                              vmpc-actions)))
    (if (not action-regexp)
        (setq action-regexp (completing-read "VMPC action-regexp: "
                                             action-names)))
    (mapcar '(lambda (action)
               (if (string-match action-regexp (car action))
                   (mapcar '(lambda (action-command)
                              (eval action-command))
                           (cdr action))))
            vmpc-actions)))


(defun vmpc-run-actions (&optional actions verbose)
  "Run the argument actions, or the actions stored in `vmpc-actions-to-run'.
If verbose is supplied, it should be a STRING, indicating the name of a
buffer to which to write diagnostic output."
  (interactive)
  
  (if (and (not vmpc-actions-to-run) (not actions) (interactive-p))
      (setq vmpc-actions-to-run (vmpc-read-actions)))

  (let ((actions (or actions vmpc-actions-to-run)) form)
    (while actions
      (setq form (or (assoc (car actions) vmpc-actions)
                     (error "Action %S does not exist!" (car actions)))
            actions (cdr actions))
      (let ((form (cons 'progn (cdr form)))
	    (results (eval (cons 'progn (cdr form)))))
	(when verbose
	  (save-excursion
	    (set-buffer verbose)
	    (insert (format "Action form is:\n%S\nResults are:\n%S\n"
			    form results))))))))

;; ------------------------------------------------------------------------
;; The main functions and advices -- these are the entry points to pcrisis:
;; ------------------------------------------------------------------------
(defun vmpc-init-vars (&optional state buffer)
  "Initialize pcrisis variables and optionally set STATE and BUFFER."
  (setq vmpc-saved-headers-alist nil
        vmpc-actions-to-run nil
        vmpc-true-conditions nil
        vmpc-current-state state
        vmpc-current-buffer (or buffer 'none)))

(defun vmpc-make-vars-local ()
  "Make the pcrisis vars buffer local.

When the vars are first set they cannot be made buffer local as we are not in
the composition buffer then.

Unfortunately making them buffer local while they are bound by a `let' does
not work, see the info for `make-local-variable'.  So we are using the global
ones and make them buffer local when in the composition buffer.  At least for
`saved-headers-alist' this should fix the bug that another composition
overwrites the stored headers for subsequent morphs.

The current solution is not reentrant save, but there also should be no
recursion nor concurrent calls."
  ;; make the variables buffer local
  (let ((tc vmpc-true-conditions)
        (sha vmpc-saved-headers-alist)
        (atr vmpc-actions-to-run)
        (cs vmpc-current-state))
    (make-local-variable 'vmpc-true-conditions)
    (make-local-variable 'vmpc-saved-headers-alist)
    (make-local-variable 'vmpc-actions-to-run)
    (make-local-variable 'vmpc-current-state)
    (make-local-variable 'vmpc-current-buffer)
    ;; now set them again to make sure the contain the right value
    (setq vmpc-true-conditions tc)
    (setq vmpc-saved-headers-alist sha)
    (setq vmpc-actions-to-run atr)
    (setq vmpc-current-state cs))
    ;; mark, that we are in the composition buffer now
    (setq vmpc-current-buffer      'composition)
  ;; BUGME why is the global value resurrected after making the variable
  ;; buffer local?  Is this related to defadvice?  I have no idea what is
  ;; going on here!  Thus we clear it afterwards now!
  (save-excursion
    (set-buffer (get-buffer-create " *vmpc-cleanup*"))
    (vmpc-init-vars)
    (setq vmpc-current-buffer nil)))

(defadvice vm-do-reply (around vmpc-reply activate)
  "*Reply to a message with pcrisis voodoo."
  (vmpc-init-vars 'reply)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (vmpc-make-vars-local)
  (vmpc-run-actions))

(defadvice vm-mail (around vmpc-newmail activate)
  "*Start a new message with pcrisis voodoo."
  (vmpc-init-vars 'newmail)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (vmpc-make-vars-local)
  (vmpc-run-actions))

(defadvice vm-compose-mail (around vmpc-compose-newmail activate)
  "*Start a new message with pcrisis voodoo."
  (vmpc-init-vars 'newmail)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (vmpc-make-vars-local)
  (vmpc-run-actions))

(defadvice vm-forward-message (around vmpc-forward activate)
  "*Forward a message with pcrisis voodoo."
  ;; this stuff is already done when replying, but not here:
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  ;;  the rest is almost exactly the same as replying:
  (vmpc-init-vars 'forward)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (vmpc-make-vars-local)
  (vmpc-run-actions))

(defadvice vm-resend-message (around vmpc-resend activate)
  "*Resent a message with pcrisis voodoo."
  ;; this stuff is already done when replying, but not here:
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  ;; the rest is almost exactly the same as replying:
  (vmpc-init-vars 'resend)
  (vmpc-build-true-conditions-list)
  (vmpc-build-actions-to-run-list)
  (vmpc-run-actions)
  ad-do-it
  (vmpc-create-sig-and-pre-sig-exerlays)
  (vmpc-make-vars-local)
  (vmpc-run-actions))

(defvar vmpc-no-automorph nil
  "When true automorphing will be disabled.")

(make-variable-buffer-local 'vmpc-no-automorph)

;;;###autoload
(defun vmpc-toggle-no-automorph ()
  "Disable automorph for the current buffer.
When automorph is not doing the right thing and you want to disable it for the
current composition, then call this function."
  (interactive)
  (setq vmpc-no-automorph (not vmpc-no-automorph))
  (message (if vmpc-no-automorph
               "Automorphing has been enabled"
             "Automorphing has been disabled")))

;;;###autoload
(defun vmpc-automorph ()
  "*Change contents of the current mail message based on its own headers.
Unless `vmpc-current-state' is 'no-automorph, headers and signatures can be
changed; pre-signatures added; functions called.

Call `vmpc-no-automorph' to disable it for the current buffer."
  (interactive)
  (unless vmpc-no-automorph
    (vmpc-make-vars-local)
    (vmpc-init-vars 'automorph 'composition)
    (vmpc-build-true-conditions-list)
    (vmpc-build-actions-to-run-list)
    (vmpc-run-actions)))

(provide 'vm-pcrisis)

;;; vm-pcrisis.el ends here
