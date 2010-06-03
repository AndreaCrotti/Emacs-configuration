;;; vm-pine.el --- draft handling and other neat functions for VM
;; 
;; Copyright (C) 1998-2006 Robert Fenk
;;
;; Author:      Robert Fenk
;; Status:      Tested with XEmacs 21.4.19 & VM 7.19
;; Keywords:    vm draft handling
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
;;
;;; Commentary:
;; 
;; This package provides the following new features for VM:
;;
;;  A Pine-like postpone message function and folder.  There are two new
;;  functions. `vm-postpone-message' bound to [C-c C-d] in
;;  the `vm-mail-mode' and the function `vm-continue-postponed-message'
;;  is bound to [C] in a folder buffer.
;;
;;  Typical usage: If you are writing a mail message, and you wish to
;;  postpone it for a while, hit C-c C-d.  The message will be saved in
;;  a folder called "postponed" by default.  Later, when you wish to
;;  resume editing that file, visit the "postponed" folder, find the
;;  message you wish to continue editing, and then hit C to resume
;;  editing.
;;
;;  Furthermore, this facility can be configured, using
;;  `vm-continue-what-message' to imitate Pine's message composing.
;;  You can set `vm-mode-map' in the following way to get Pine-like
;;  behaviour:
;;
;;  (define-key vm-mode-map "m" 'vm-continue-what-message)
;;  (setq vm-zero-drafts-start-compose t)
;;
;;  If you have postponed messages you will be asked if you want to continue
;;  composing them, if you say "yes" you will visit the `vm-postponed-folder'
;;  and you can select the message you would like to continue and press "m"
;;  again!  However be aware this works currently only if you expunge all
;;  messages marked for deletion and save the postponed folder.
;;
;;  You can also bind it to "C-x m" in order to check for postponed messages
;;  when composing a message without starting VM.
;;
;;  (autoload 'vm-continue-what-message-other-window "vm-pine" "" t)
;;  (global-set-key "\C-xm" 'vm-continue-what-message-other-window)
;;
;;
;;  Three new mail header insertion functions make life easier. The
;;  bindings and names are:
;;     "\C-c\C-f\C-a"  vm-mail-return-receipt-to
;;     "\C-c\C-f\C-p"  vm-mail-priority
;;     "\C-c\C-f\C-f"  vm-mail-fcc
;;  The variables `vm-mail-return-receipt-to' and `vm-mail-priority'
;;  can be used to configure the inserted headers.
;;  `vm-mail-fcc' can be configured by setting the variable
;;  `vm-mail-folder-alist' which has the same syntax and default
;;  value as `vm-auto-folder-alist'.
;;  You may also add `vm-mail-auto-fcc' to `vm-reply-hook' in order to
;;  automatically setup the FCC header according to the variable
;;  `vm-mail-folder-alist'.
;;  There is another fcc-function `vm-mail-to-fcc' which set the FCC
;;  according to the recipients email-address.
;;
;;; Bug reports and feature requests:
;; Please send a backtrace and the version number of vm-pine.el!
;; Feature requests are welcome!

;;; Code:

;; Attempt to handle older/other emacs.
(eval-and-compile
  (require 'vm-version)
  (require 'vm-message)
  (require 'vm-macro)
  (require 'vm-vars))

(eval-when-compile 
  (require 'cl))

(if (not (boundp 'user-mail-address))
    (if (functionp 'user-mail-address)
        (setq user-mail-address (user-mail-address))
      (setq user-mail-address "unknown")
      (message "Please set the variable `user-mail-address'!!!")
      (sit-for 2)))
      
(defgroup vm nil
  "VM"
  :group 'mail)

(defgroup vm-pine nil
  "Pine inspired extensions to VM."
  :group  'vm)

;;-----------------------------------------------------------------------------
;;;###autoload
(defun vm-summary-function-f (m)
  "Return the recipient or newsgroup for uninteresting senders.
If the \"From:\" header contains the user login or full name then
this function returns the \"To:\" or \"Newsgroups:\" header field with a
\"To:\" as prefix.

For example the outgoing message box will now list to whom you sent the
messages.  Use `vm-fix-summary!!!' to update the summary of a folder! With
loaded BBDB it uses `vm-summary-function-B' to obtain the full name of the
sender.  The only difference to VMs default behavior is the honoring of
messages sent to news groups. ;c)

See also:    `vm-summary-uninteresting-senders'"
  (interactive)
  (let ((case-fold-search t)
        (headers '(("From:" "") 
                   ("Newsgroups:" "News:")
                   "To:" "CC:" "BCC:"
                   "Resent-To:" "Resent-CC:" "Resent-BCC:"
                   ("Sender:" "")  ("Resent-From:" "Resent:")))
        header-name arrow
        addresses
        address
        first)

    (while (and (not address) headers)
      (if (listp (car headers))
          (setq header-name (caar headers) arrow (cadar headers))
        (setq header-name (car headers) arrow (concat header-name " ")))
      (setq addresses (vm-get-header-contents m header-name))
      (if addresses
          (setq addresses (vm-decode-mime-encoded-words-in-string addresses)
                addresses
                (or (if (functionp 'bbdb-extract-address-components)
                        (bbdb-extract-address-components addresses t))
                    (list (mail-extract-address-components addresses))
                    addresses)))
      (if (not first) (setq first (car addresses)))
      (while addresses
        (if (or (not vm-summary-uninteresting-senders)
                (and vm-summary-uninteresting-senders
                     (not (string-match vm-summary-uninteresting-senders
                                        (format "%s" (car addresses))))))
            (setq address (car addresses) addresses nil))
        (setq addresses (cdr addresses)))
      (setq headers (cdr headers)))

    (if (and (null address) (null first))
        ""
      (if (and (null address) first)
          (setq address first))
      (concat arrow
              (cond ((functionp 'bbdb/vm-alternate-full-name)
                     (or (bbdb/vm-alternate-full-name (cadr address))
                         (car address)
                         (cadr address)))
                    (t (or (car address) (cadr address))))))))

;;-----------------------------------------------------------------------------
;;;###autoload
(defcustom vm-postponed-header "X-VM-postponed-data: "
  "Additional header which is inserted to postponed messages.
It is used for internal things and should not be modified. 
It is a lisp list which currently contains the following items:
 <date of the postponing>
 <reply references list>
 <forward references list>
 <redistribute references list>
while the last three are set by `vm-get-persistent-message-ids-for'."
  :type 'string
  :group 'vm-pine)

;;-----------------------------------------------------------------------------
;; A Pine-like postponed folder handling
;;;###autoload
(defcustom vm-postponed-folder "postponed"
  "The name of the folder where postponed messages are saved."
  :type 'string
  :group 'vm-pine)

;;;###autoload
(defcustom vm-postponed-message-headers '("From:" "Organization:"
                                          "Reply-To:"
                                          "To:" "Newsgroups:"
                                          "CC:" "BCC:" "FCC:"
                                          "In-Reply-To:"
                                          "References:"
                                          "Subject:"
                                          "X-Priority:" "Priority:")
  "Similar to `vm-forwarded-headers'.
A list of headers that should be kept, when continuing a postponed message.

The following mime headers should not be kept, since this breaks things:
Mime-Version, Content-Type, Content-Transfer-Encoding."
  :type '(repeat (string))
  :group 'vm-pine)

;;;###autoload
(defcustom vm-postponed-message-discard-header-regexp nil
  "Similar to `vm-unforwarded-header-regexp'.
A regular expression matching all headers that should be discard when
when continuing a postponed message."
  :type 'regexp
  :group 'vm-pine)

;;;###autoload
(defcustom vm-continue-postponed-message-hook nil
  "List of hook functions to be run after continuing a postponed message."
  :type 'hook
  :group 'vm-pine)

;;;###autoload
(defcustom vm-postpone-message-hook nil
  "List of hook functions to be run before postponing a message."
  :type 'hook
  :group 'vm-pine)

(defvar vm-postponed-message-folder-buffer nil
  "Buffer of source folder.
This is only for internal use of vm-pine.el!!!")

;;-----------------------------------------------------------------------------
(define-key vm-mode-map "C"      'vm-continue-what-message)

;;-----------------------------------------------------------------------------
(defun vm-get-persistent-message-ids-for (mlist)
  "Return a list of message id and folder name of all messages in MLIST."
  (let (mp midlist folder mid f)
    (while mlist
      (setq mp (car mlist)
            folder (buffer-file-name (vm-buffer-of (vm-real-message-of mp)))
            mid (vm-message-id-of mp)
            f (assoc folder midlist))
      (if mid
          (if f
              (setcdr f (cons mid (cdr f)))
            (add-to-list 'midlist (list folder mid))))
      (setq mlist (cdr mlist)))
    midlist))
  
(defun vm-get-message-pointers-for (msgidlist)
  "Return the message pointers belonging to the messages listed in MSGIDLIST.
MSGIDLIST is a list as returned by `vm-get-persistent-message-ids-for'."
  (let (folder vm-message-pointers)
    (while msgidlist
      (setq folder (caar msgidlist))
      (save-excursion
        (when (cond ((get-buffer folder)
                     (set-buffer (get-buffer folder)))
                    ((get-file-buffer folder)
                     (set-buffer (get-file-buffer folder)))
                    ((file-exists-p folder)
                     (vm-visit-folder folder))
                    (t
                     (message "The folder '%s' does not exist anymore.  Maybe it was virtual or closed before postponing." folder)
                     nil))
          (vm-select-folder-buffer)
          (save-restriction
            (widen)
            (goto-char (point-min))
            (let ((msgid-regexp (concat "^Message-Id:\\s-*"
                                        (regexp-opt (cdar msgidlist))))
                  (point-max (point-max))
                    (case-fold-search t))
              
              (while (re-search-forward msgid-regexp point-max t)
                (let ((point (point))
                      (mp vm-message-list))
                  (while mp
                    (if (and (>= point (vm-start-of (car mp)))
                             (<= point (vm-end-of (car mp))))
                        (setq vm-message-pointers (cons (car mp)
                                                        vm-message-pointers)
                              mp nil)
                      (setq mp (cdr mp)))))))))
        (setq msgidlist (cdr msgidlist))))
    vm-message-pointers))

;;-----------------------------------------------------------------------------
;;;###autoload
(defun vm-continue-postponed-message (&optional silent)
  "Continue composing of the currently selected message.
Before continuing the composition you may decode the presentation as
you like, by pressing [D] and viewing part of the message!
Then current message is copied to a new buffer and the vm-mail-mode is
entered.  When every thing is finished the hook functions in
`vm-mail-mode-hook' and `vm-continue-postponed-message-hook' are
executed.  When called with a prefix argument it will not switch to
the composition buffer, this may be used for automatic editing of
messages.

The variables `vm-postponed-message-headers' and
`vm-postponed-message-discard-header-regexp' control which
headers are copied to the composition buffer.

In `vm-mail-mode' this is bound to [C].
If optional argument SILENT is positive then act in background (no frame
creation)."
  (interactive "P")

  (vm-session-initialization)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-check-for-killed-presentation)
  (vm-error-if-folder-empty)

  (if (eq vm-system-state 'previewing)
      (vm-show-current-message))
  
  (save-restriction
    (widen)
    (let* ((folder-buffer (current-buffer))
           (presentation-buffer vm-presentation-buffer)
           (vmp vm-message-pointer)
           (is-decoded vm-mime-decoded)
           (hstart (vm-headers-of (car vmp)))
           (tstart (vm-text-of    (car vmp)))
           (tend   (- (vm-end-of     (car vmp)) 1))
           (to (format "mail to %s" (vm-get-header-contents (car vmp)
                                                            "To:" ",")))
           (vm-pp-data (vm-get-header-contents (car vmp)
                                               vm-postponed-header)))
      
      ;; Prepare the composition buffer
      (if (and to (string-match "[^,\n<(]+" to))
          (setq to (match-string 0 to)))
      
      (if (not silent)
          (let ((vm-mail-hook nil)
                (vm-mail-mode-hook nil)
                (this-command 'vm-mail))
            (vm-mail-internal to))
        (set-buffer (generate-new-buffer to))
        (setq default-directory (expand-file-name
                                 (or vm-folder-directory "~/")))
        (auto-save-mode (if auto-save-default 1 -1))
        (let ((mail-mode-hook nil)
              (mail-setup-hook nil))
          (mail-mode))
        (setq vm-mail-buffer folder-buffer))
      
      (make-local-variable 'vm-postponed-message-folder-buffer)
      (setq vm-postponed-message-folder-buffer
            (vm-buffer-of (vm-real-message-of (car vmp))))
      (make-local-variable 'vm-message-pointer)
      (setq vm-message-pointer vmp)
      (make-local-hook 'mail-send-hook)
      (add-hook 'mail-send-hook 'vm-delete-postponed-message t t)
      (erase-buffer)

      ;; set the VM variables for setting source message attributes
      (when vm-pp-data
        (make-local-variable 'vm-reply-list)
        (make-local-variable 'vm-forward-list)
        (make-local-variable 'vm-redistribute-list)
        (setq vm-pp-data (read vm-pp-data)
              vm-reply-list 
              (and (nth 1 vm-pp-data) (vm-get-message-pointers-for (nth 1 vm-pp-data)))
              vm-forward-list
              (and (nth 2 vm-pp-data) (vm-get-message-pointers-for (nth 2 vm-pp-data)))
              vm-redistribute-list
              (and (nth 3 vm-pp-data) (vm-get-message-pointers-for (nth 3 vm-pp-data))))
        (if vm-reply-list (setq vm-system-state 'replying))
        (if vm-forward-list (setq vm-system-state 'forwarding))
        (if vm-redistribute-list (setq vm-system-state 'redistributing)))
      
      ;; Prepare headers
      (insert-buffer-substring folder-buffer hstart tstart)
      (goto-char (point-min))
      (cond ((or (vm-mime-plain-message-p (car vmp)) is-decoded)
             (vm-reorder-message-headers nil
                 vm-postponed-message-headers
                 vm-postponed-message-discard-header-regexp))
            (t ; copy undecoded messages with mime headers
             (vm-reorder-message-headers nil
                  (append '("MIME-Version:" "Content-type:")
                      vm-postponed-message-headers)
                  vm-postponed-message-discard-header-regexp)))
      (vm-decode-mime-encoded-words)
      (search-forward-regexp "\n\n")
      (replace-match (concat "\n" mail-header-separator "\n") t t)

      ;; Add message body as previewed
      (goto-char (point-max))
      (if presentation-buffer
          ;; when using presentation buffer we have to
          (save-excursion
            (set-buffer presentation-buffer)
            (goto-char (point-min))
            (search-forward-regexp "\n\n")
            (setq tstart (match-end 0)
                  tend (point-max)))
        (setq presentation-buffer folder-buffer))
            
      (insert-buffer-substring presentation-buffer tstart tend)
      ;; in order to show headers hidden by vm-shrunken-headers 
      (put-text-property (point-min) (point-max) 'invisible nil)
      
      ;; and add the buttons for attachments
      (vm-decode-postponed-mime-message)))

  (when (not silent)
    (run-hooks 'mail-setup-hook)
    (run-hooks 'vm-mail-hook)
    (run-hooks 'vm-mail-mode-hook))
  
  (run-hooks 'vm-continue-postponed-message-hook))

;;-----------------------------------------------------------------------------
;;;###autoload
(defun vm-reply-by-continue-postponed-message ()
  "Like `vm-reply' but preserves attachments."
  (interactive)
  (let ((vm-continue-postponed-message-hook)
        (vm-reply-hook nil)
        (vm-mail-mode-hook nil)
        (mail-setup-hook nil)
        (mail-signature nil)
        reply-buffer
        start end)
    (vm-reply 1)
    (save-excursion
      (vm-continue-postponed-message t)
      (goto-char (point-min))
      (re-search-forward (regexp-quote mail-header-separator) (point-max))
      (forward-char 1)
      (setq reply-buffer (current-buffer)
            start (point)
            end (point-max)))
    (goto-char (point-max))
    (insert-buffer-substring reply-buffer start end)
    (vm-add-reply-subject-prefix (car vm-message-pointer)))
  (run-hooks 'mail-setup-hook)
  (run-hooks 'vm-mail-hook)
  (run-hooks 'vm-mail-mode-hook)
  (run-hooks 'vm-reply-hook))

;;-----------------------------------------------------------------------------
(defun vm-delete-postponed-message ()
  "Delete the source message belonging to the continued composition."
  (interactive)
  (if vm-message-pointer
      (condition-case nil
          (let* ((msg (car vm-message-pointer))
                (buffer (vm-buffer-of msg)))
            ;; only delete messages which have been postponed by us before
            (when (vm-get-header-contents msg vm-postponed-header)
              (vm-set-deleted-flag msg t)
              (vm-update-summary-and-mode-line))
            ;; in the postponded folder expunge them right now 
            (when (string= (buffer-name buffer)
                           (file-name-nondirectory vm-postponed-folder))
              (if (frames-of-buffer buffer t)
                  (iconify-frame (car (frames-of-buffer buffer))))
              (save-excursion
                (switch-to-buffer buffer)
                (vm-expunge-folder)
                (vm-save-folder)
                (when (not vm-message-list)
                  (let ((this-command 'vm-quit))
                    (vm-quit))))))
        (error "Folder buffer closed before deletion of source message."))))

;;-----------------------------------------------------------------------------
;;;###autoload
(defun vm-decode-postponed-mime-message ()
  "Replace the mime buttons by attachment buttons."
  (interactive)
  (cond (vm-xemacs-p
         (let ((e-list (extent-list nil (point-min) (point-max))))
           ;; First collect the extents
           (setq e-list
                 (sort (vm-delete
                        (function (lambda (e)
                                    (extent-property e 'vm-mime-layout)))
                        e-list t)
                       (function (lambda (e1 e2)
                                   (< (extent-end-position e1)
                                      (extent-end-position e2))))))
           ;; Then replace the buttons, because doing it at once will result in
           ;; problems since the new buttons are from the same extent.
           (while e-list
             (vm-decode-postponed-mime-button (car e-list))
             (setq e-list (cdr e-list)))))
        (vm-fsfemacs-p
         (let ((o-list (vm-pine-fake-attachment-overlays (point-min)
                                                         (point-max))))
           (setq o-list (sort (vm-delete
                               (function (lambda (o)
                                           (overlay-get o 'vm-mime-layout)))
                               o-list t)
                              (function
                               (lambda (e1 e2)
                                 (< (overlay-end e1)
                                    (overlay-end e2))))))
           (while o-list
             (vm-decode-postponed-mime-button (car o-list))
             (setq o-list (cdr o-list)))))
        (t
         (error "don't know how to MIME dencode composition for %s"
                (emacs-version)))))

(defun vm-pine-fake-attachment-overlays (start end)
  (let ((o-list nil)
	(done nil)
	(pos start)
	object props o)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(while (not done)
	  (setq object (get-text-property pos 'vm-mime-layout))
	  (setq pos (next-single-property-change pos 'vm-mime-layout))
	  (or pos (setq pos (point-max) done t))
	  (if object
	      (progn
		(setq o (make-overlay start pos))
		(overlay-put o 'insert-in-front-hooks
			     '(vm-disallow-overlay-endpoint-insertion))
		(overlay-put o 'insert-behind-hooks
			     '(vm-disallow-overlay-endpoint-insertion))
		(setq props (append (list 'vm-mime-object t)
				    (text-properties-at start)))
		(while props
		  (overlay-put o (car props) (car (cdr props)))
		  (setq props (cdr (cdr props))))
		(setq o-list (cons o o-list))))
	  (setq start pos))
	o-list ))))

;;-----------------------------------------------------------------------------
(defun vm-decode-postponed-mime-button (x)
  "Replace the mime button specified by X."

  (save-excursion
    (let (layout
          xstart
          xend)

      (if vm-fsfemacs-p
          (setq layout (overlay-get x 'vm-mime-layout)
                xstart (overlay-start x)
                xend   (overlay-end x))
        (setq layout (extent-property x 'vm-mime-layout)
              xstart (extent-start-position x)
              xend   (extent-end-position x)))
               
      (let* ((start  (vm-mm-layout-header-start layout))
             (end    (vm-mm-layout-body-end   layout))
             (b      (marker-buffer start))
             (desc   (or (vm-mm-layout-description layout)
                         "message body text"))
             (disp   (or (vm-mm-layout-disposition layout)
                         '("inline")))
             (file   (vm-mime-get-disposition-parameter layout "filename"))
             filename
             (type   (vm-mm-layout-type layout)))

        (if (and type
                 (string= (car type) "message/external-body")
                 (string= (cadr type) "access-type=local-file"))
          (save-excursion
            (setq filename (substring (caddr type) 5))
            (vm-select-folder-buffer)
            (save-restriction
              (let ((start (vm-mm-layout-body-start layout))
                    (end   (vm-mm-layout-body-end layout)))
                (set-buffer (marker-buffer (vm-mm-layout-body-start layout)))
                (widen)
                (goto-char start)
                (if (not (re-search-forward
                          "Content-Type: \"?\\([^ ;\" \n\t]+\\)\"?;?"
                          end t))
                    (error "No `Content-Type' header found in: %s"
                           (buffer-substring start end))
                  (setq type (list (match-string 1))))))))
        
        ;; delete the mime-button
        (goto-char xstart)
        (delete-region xstart xend)
        ;; and insert an attached-object-button
        (if filename
            (vm-mime-attach-file filename (car type))
          (if file
              (vm-mime-attach-object (list b start end disp file)
                                     (car type) nil desc t)
            (vm-mime-attach-object (list b start end disp)
                                   (car type) nil desc t)))))))

;;-----------------------------------------------------------------------------
(define-key vm-mail-mode-map "\C-c\C-d" 'vm-postpone-message)

(defvar vm-postpone-message-modes-to-disable
  '(font-lock-mode ispell-minor-mode filladapt-mode auto-fill-mode)
  "A list of modes to disable before postponing a message.")

;;-----------------------------------------------------------------------------
;;;###autoload
(defun vm-postpone-message (&optional folder dont-kill no-postpone-header)
  "Save the current composition as a draft.
Before saving the composition the `vm-postpone-message-hook' functions
are executed and it is written into the FOLDER `vm-postponed-folder'.
When called with a prefix argument you will be asked for
the folder.
Optional argument DONT-KILL is positive, then do not kill source message."
  (interactive "P")
  
  (let ((message-buffer (current-buffer))
        folder-buffer
        target-type)

    (let (m (modes vm-postpone-message-modes-to-disable))
      (while modes
        (setq m (car modes) modes (cdr modes))
        (if (and (boundp m) (symbol-value m))
            (funcall m 0))))

    (if (and folder (not (stringp folder)))
        (setq folder (vm-read-file-name
                      (format "Postpone to folder (%s): " vm-postponed-folder)
                      (or vm-folder-directory default-directory)
                      vm-postponed-folder nil nil
                      'vm-folder-history)))
    
    ;; there is no explicit folder given ...
    (if (not folder)
        (if vm-postponed-message-folder-buffer
            (setq folder (buffer-file-name vm-postponed-message-folder-buffer))
          (setq folder (expand-file-name vm-postponed-folder
                                         (or vm-folder-directory
                                             default-directory)))))

    (if (not folder)
        (error "I could not find a folder for postponing messages!"))

    ;; if it is no absolute folder path then prepend the folder directory
    (if (not (file-name-absolute-p folder))
        (setq folder (expand-file-name folder
                                       (or vm-folder-directory
                                           default-directory))))

    ;; Now add possibly missing headers
    (goto-char (point-min))
    (vm-mail-mode-show-headers)
    (if (not (vm-mail-mode-get-header-contents "From:"))
        (let* ((login user-mail-address)
               (fullname (user-full-name)))
          (cond ((and (eq mail-from-style 'angles) login fullname)
                 (insert (format "From: %s <%s>\n" fullname login)))
                ((and (eq mail-from-style 'parens) login fullname)
                 (insert (format "From: %s (%s)\n" login fullname)))
                (t
                 (insert (format "From: %s\n" login))))))
    
    ;; mime-encode the message if necessary 
    (let ((vm-do-fcc-before-mime-encode nil))
      (condition-case nil (vm-mime-encode-composition) (error t)))

    ;; add the current date 
    (if (not (vm-mail-mode-get-header-contents "Date:"))
        (insert "Date: "
                (format-time-string "%a, %d %b %Y %H:%M:%S %Z"
                                    (current-time))
                "\n"))
    ;; add the postponed header
    (vm-mail-mode-remove-header vm-postponed-header)

    (if no-postpone-header nil
      (insert vm-postponed-header " "
              (format
               "(\"%s\" %S %S %S)\n"
               (format-time-string "%a, %d %b %Y %T %Z" (current-time))
               (vm-get-persistent-message-ids-for vm-reply-list)
               (vm-get-persistent-message-ids-for vm-forward-list)
               (vm-get-persistent-message-ids-for vm-redistribute-list))))

    ;; ensure that the message ends with an empty line!
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (delete-region (point) (point-max))
    (insert "\n\n\n")
    
    ;; run the hooks 
    (run-hooks 'vm-postpone-message-hook)

    ;; delete mail header separator
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote mail-header-separator) nil t)
        (delete-region (match-beginning 0) (match-end 0)))


    (setq folder-buffer (vm-get-file-buffer folder))
    (if folder-buffer
        ;; o.k. the folder is already opened
        (save-excursion
          (set-buffer folder-buffer)
          (vm-error-if-folder-read-only)
          (let ((buffer-read-only nil))
            (vm-save-restriction
             (widen)
             (goto-char (point-max))
             (vm-write-string (current-buffer) (vm-leading-message-separator))
             (insert-buffer-substring message-buffer)
             (vm-write-string (current-buffer) (vm-trailing-message-separator))

             (cond ((eq major-mode 'vm-mode)
                    (vm-increment vm-messages-not-on-disk)
                    (vm-clear-modification-flag-undos)))
             
             (vm-check-for-killed-summary)
             (vm-assimilate-new-messages)
             (vm-update-summary-and-mode-line))))
      ;; well the folder is not visited, so we write to the file
      (setq target-type (or (vm-get-folder-type folder)
                            vm-default-folder-type))
      
      (if (eq target-type 'unknown)
          (error "Folder `%s' type is unrecognized" folder))
      
      (vm-write-string folder (vm-leading-message-separator target-type))
      (write-region (point-min) (point-max) folder t 'quiet)
      (vm-write-string folder (vm-trailing-message-separator target-type)))
    
    ;; delete source message
    (vm-delete-postponed-message)

    ;; mess arounf with the window configuration 
    (let ((b (current-buffer))
          (this-command 'vm-mail-send-and-exit))
      (cond ((null (buffer-name b));; dead buffer
             ;; This improves window configuration behavior in
             ;; XEmacs.  It avoids taking the folder buffer from
             ;; one frame and attaching it to the selected frame.
             (set-buffer (window-buffer (selected-window)))
             (vm-display nil nil '(vm-mail-send-and-exit)
                         '(vm-mail-send-and-exit
                           reading-message
                           startup)))
            (t
             (vm-display b nil '(vm-mail-send-and-exit)
                         '(vm-mail-send-and-exit reading-message startup)))))
    
    ;; and kill this buffer?
    (if dont-kill
        (insert (concat "FCC: " folder "\n" mail-header-separator))
      (kill-this-buffer))

    (if (interactive-p)
        (message "Message postponed to folder `%s'" folder))))

;;-----------------------------------------------------------------------------
(defun vm-buffer-in-vm-mode ()
  (member major-mode '(vm-mode vm-virtual-mode
                               vm-presentation-mode
                               vm-summary-mode
                               vm-mail-mode)))

(defcustom vm-continue-what-message 'ask
  "Whether to never continue, ask or always continue postponed messages."
  :type '(choice (const :tag "never" nil)
                 (const ask)
                 (const continue))
  :group 'vm-pine)

(defcustom vm-zero-drafts-start-compose nil
  "When t and there are no drafts, `vm-continue-what-message' call `vm-mail'."
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "start new message" t))
  :group 'vm-pine)

(defun vm-continue-what-message-composing ()
  "Decide whether to compose a new message or continue a draft.
This checks if the postponed folder contains drafts.
Drafts in other folders are not recognized!"
  (save-excursion
    (vm-session-initialization)
    
    (let* ((ppfolder (and vm-postponed-folder
                          (expand-file-name vm-postponed-folder
                                            (or vm-folder-directory
                                                default-directory))))
           action
           buffer)
      
      (when current-prefix-arg
        (setq action 'force-continue))
      
      (when (vm-find-composition-buffer)
        (setq action 'continue))
      
      ;; postponed message in current folder
      (when (vm-buffer-in-vm-mode)
        (vm-check-for-killed-folder)
        (vm-select-folder-buffer)
                  
        (if (and vm-message-pointer
                 (vm-get-header-contents (vm-real-message-of
                                          (car vm-message-pointer))
                                         (regexp-quote vm-postponed-header))
                 (not (vm-deleted-flag (car vm-message-pointer))))
            (setq action 'continue)))

      ;; postponed message in postponed folder
      (when (and (not action) (setq buffer (vm-get-file-buffer ppfolder)))
        (if (and (get-buffer-window-list buffer nil 0))
            (when (save-excursion
                    (set-buffer buffer)
                    (not (vm-deleted-flag (car vm-message-pointer))))
              (message "Please select a draft!")
              (select-window (car (get-buffer-window-list buffer nil 0)))
              (if (frames-of-buffer buffer)
                  (deiconify-frame (car (frames-of-buffer buffer))))
              (setq action 'none))
          (setq action 'visit)))

      ;; visit postponed folder 
      (when (and (not action) (file-exists-p ppfolder)
                 (> (nth 7 (file-attributes ppfolder)) 0))
        (setq action 'visit))

      (if (not action) (setq action 'new))

      ;; decide what to do
      (setq action 
            (cond ((eq vm-continue-what-message nil)
                   'new)
                  ((eq vm-continue-what-message 'ask)
                   (if (equal action 'visit)
                       (if (y-or-n-p
                            "Continue composition of postponed messages? ")
                           'visit
                         'new)
                     action))
                  ((eq vm-continue-what-message 'continue)
                   action)
                  (t
                   action))))))
              
;;;###autoload
(defun vm-continue-what-message (&optional where)
  "Continue compositions or postponed messages if there are some.

With a prefix arg, call `vm-continue-postponed-message', i.e. continue the
currently selected message.

See `vm-continue-what-message' and `vm-zero-drafts-start-compose' for
configuration."
  (interactive)
  (if where (setq where (concat "-" where)))
  (let ((action (vm-continue-what-message-composing))
        (visit  (intern (concat "vm-visit-folder" (or where ""))))
        (mail   (intern (concat "vm-mail" (or where "")))))
    (cond ((equal action 'force-continue)
           (vm-continue-postponed-message))
          ((equal action 'continue)
           (if (vm-find-composition-buffer)
               (vm-continue-composing-message)
             (vm-continue-postponed-message)))
          ((equal action 'visit)
           (funcall visit vm-postponed-folder)
           (vm-select-folder-buffer)
           (make-local-hook 'vm-quit-hook)
           (add-hook 'vm-quit-hook 'vm-expunge-folder nil t)
           (vm-expunge-folder)
           (cond ((= (length vm-message-list) 0)
                  (let ((this-command 'vm-quit))
                    (vm-quit))
                  (let ((this-command mail))
                    (funcall mail)))
                 ((= (length vm-message-list) 1)
                  (vm-continue-postponed-message))))
          ((and vm-zero-drafts-start-compose (equal action 'new))
           (let ((this-command mail))
             (funcall mail)))
          (t
           (message "There are no known drafts.")))))

;;;###autoload
(defun vm-continue-what-message-other-window ()
    "Ask for continuing of postponed messages if there are some."
    (interactive)
    (vm-continue-what-message "other-window"))

;;;###autoload
(defun vm-continue-what-message-other-frame ()
  "Ask for continuing of postponed messages if there are some."
  (interactive)
  (vm-continue-what-message "other-frame"))

;;-----------------------------------------------------------------------------
;; And now do some cool stuff when killing a mail buffer
;; This was inspired by Uwe Brauer
(defcustom vm-save-killed-message
  'ask
  "How `vm-save-killed-message-hook' handles saving of a mail as a draft.
If set to 'ask it will ask whether to save the mail as draft or not.
If set to 'always it will save without asking.
If set to nil it will never save them nor it will ask."
  :type '(choice (const ask)
                 (const always)
                 (const :tag "never" nil))
  :group 'vm-pine)

(defcustom vm-save-killed-messages-folder
  vm-postponed-folder
  "The name of the folder where killed messages are saved."
  :type 'string
  :group 'vm-pine)

(defun vm-add-save-killed-message-hook ()
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'vm-save-killed-message-hook nil t))

(defun vm-remove-save-killed-message-hook ()
  (remove-hook 'kill-buffer-hook 'vm-save-killed-message-hook t))

(defun vm-save-killed-message-hook ()
  (if (or (and (equal vm-save-killed-message 'ask)
               (y-or-n-p (format "Save `%s' as draft in folder `%s'? "
                                 (buffer-name)
                                 vm-save-killed-messages-folder)))
          (equal vm-save-killed-message 'always))
      (vm-postpone-message vm-save-killed-messages-folder t)
    (message "`%s' is gone forever!" (buffer-name))))

(add-hook 'vm-mail-mode-hook 'vm-add-save-killed-message-hook)
(add-hook 'mail-send-hook 'vm-remove-save-killed-message-hook)
(add-hook 'vm-postpone-message-hook 'vm-remove-save-killed-message-hook)

;;-----------------------------------------------------------------------------
;; New header fields
(define-key vm-mail-mode-map "\C-c\C-f\C-a" 'vm-mail-return-receipt-to)
(define-key vm-mail-mode-map "\C-c\C-f\C-p" 'vm-mail-priority)
(define-key vm-mail-mode-map "\C-c\C-f\C-f" 'vm-mail-fcc)
(define-key vm-mail-mode-map "\C-c\C-f\C-n" 'vm-mail-notice-requested-upon-delivery-to)

;;;###autoload
(defcustom vm-mail-return-receipt-to
  (concat (user-full-name) " <" user-mail-address ">")
  "The address where return receipts should be sent to."
  :type 'string
  :group 'vm-pine)

;;;###autoload
(defun vm-mail-return-receipt-to ()
  "Insert the \"Return-Receipt-To\" header into a `vm-mail-mode' buffer.
See the variable `vm-mail-return-receipt-to'."
  (interactive)
  (expand-abbrev)
  (save-excursion
    (or (mail-position-on-field "Return-Receipt-To" t)
        (progn (mail-position-on-field "Subject")
               (insert "\nReturn-Receipt-To: " vm-mail-return-receipt-to
                       "\nRead-Receipt-To: " vm-mail-return-receipt-to
                       "\nDelivery-Receipt-To: " vm-mail-return-receipt-to))))
  (message "Remove those headers you do not require!"))

;;;###autoload
(defun vm-mail-notice-requested-upon-delivery-to ()
  "Notice-Requested-Upon-Delivery-To:"
  (interactive)
  (expand-abbrev)
  (save-excursion
    (or (mail-position-on-field "Notice-Requested-Upon-Delivery-To" t)
        (progn (mail-position-on-field "Subject")
               (insert "\nNotice-Requested-Upon-Delivery-To: "
                       (let ((to (vm-mail-get-header-contents
                                  "\\(.*-\\)?To:")))
                         (if to to "")))))))

;;;###autoload
(defcustom vm-mail-priority
  "Priority: urgent\nImportance: High\nX-Priority: 1"
  "The priority headers."
  :type 'string
  :group 'vm-pine)

;;;###autoload
(defun vm-mail-priority ()
  "Insert priority headers into a `vm-mail-mode' buffer.
See the variable `vm-mail-priority'."
  (interactive)
  (expand-abbrev)
  (save-excursion
    (or (mail-position-on-field "Priority" t)
        (progn (mail-position-on-field "Subject")
               (insert "\n" vm-mail-priority)))))

;;-----------------------------------------------------------------------------
(if (not vm-xemacs-p)
    (defun user-home-directory ()
      (getenv "HOME")))

(defun vm-mail-fcc-file-join (dir file)
  "Returns a nice path to a folder."
  (let* ((path (expand-file-name file dir)))
    (if path
	(if vm-xemacs-p
	    (abbreviate-file-name path t)
	  (abbreviate-file-name path))
      dir)))

;;;###autoload
(defcustom vm-mail-folder-alist (if (boundp 'vm-auto-folder-alist)
                                    vm-auto-folder-alist)
  "Like `vm-auto-folder-alist' but for outgoing messages.
It should be fed to `vm-mail-select-folder'!"
  :type 'list
  :group 'vm-pine)

;;;###autoload
(defcustom vm-mail-fcc-default
  '(or (vm-mail-select-folder vm-mail-folder-alist)
       (vm-mail-to-fcc nil t)
       mail-archive-file-name)
  "A list which is evaluated to return a folder name.
By reordering the elements of this list or adding own functions you
can control the behavior of vm-mail-fcc and `vm-mail-auto-fcc'.
You may allow a sophisticated decision for the right folder for your
outgoing message."
  :type 'list
  :group 'vm-pine)

;;;###autoload
(defun vm-mail-fcc (&optional arg)
  "Insert the FCC-header into a `vm-mail-mode' buffer.
Like `mail-fcc', but honors VM variables and offers a default folder
according to `vm-mail-folder-alist'.
Called with prefix ARG it just removes the FCC-header."
  (interactive "P")
  (expand-abbrev)

  (let ((dir (or vm-folder-directory default-directory))
        (fcc nil)
        (folder (vm-mail-mode-get-header-contents "FCC:"))
        (prompt nil))
    
    (if arg (progn (vm-mail-mode-remove-header "FCC:")
                   (message "FCC header removed!"))
      (save-excursion
        (setq fcc (eval vm-mail-fcc-default))

        ;; cleanup the name 
        (setq fcc (if fcc (vm-mail-fcc-file-join dir fcc)))

        (setq prompt (if fcc
                         (format "FCC to folder (%s): " fcc)
                       "FCC to folder: "))

        (setq folder (if (and folder (not (file-directory-p folder)))
                         (file-relative-name folder dir)))

        ;; we got the name so insert it 
        (vm-mail-mode-remove-header "FCC:")
        (setq fcc (vm-read-file-name prompt
                                        dir fcc
                                        nil folder
                                        'vm-folder-history))
        (setq fcc (vm-mail-fcc-file-join dir fcc))
        (if (file-directory-p fcc)
            (error "Folder `%s' in no file, but a directory!" fcc)
          (mail-position-on-field "FCC")
          (insert fcc))))))

;;;###autoload
(defun vm-mail-auto-fcc ()
  "Add a new FCC field, with file name guessed by `vm-mail-folder-alist'.
You likely want to add it to `vm-reply-hook' by
   (add-hook 'vm-reply-hook 'vm-mail-auto-fcc)
or if sure about what you are doing you can add it to mail-send-hook!"
  (interactive "")
  (expand-abbrev)
  (save-excursion
    (let ((dir (or vm-folder-directory default-directory))
          (fcc nil))
      
      (vm-mail-mode-remove-header "FCC:")
      (setq fcc (eval vm-mail-fcc-default))
      (if fcc
          (if (file-directory-p fcc)
              (error "Folder `%s' in no file, but a directory!" fcc)
            (progn (mail-position-on-field "FCC")
                   (insert (vm-mail-fcc-file-join dir fcc))))))))

;;;###autoload
(defun vm-mail-get-header-contents (header-name-regexp &optional clump-sep)
  "Return the contents of the header(s) matching HEADER-NAME-REGEXP.
This function is a slightly changed version of `vm-get-header-contents'.
Optional argument CLUMP-SEP usually a \",\"."
  (let ((contents nil)
        (text-of-message 0)
        (regexp (concat "^\\(" header-name-regexp "\\)")))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (regexp-quote mail-header-separator)
                             (point-max) t)
          (setq text-of-message (match-end 0))
        (error "No mail header separator found!"))

      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (and (or (null contents) clump-sep)
                    (re-search-forward regexp text-of-message t)
                    (save-excursion (goto-char (match-beginning 0))
                                    (vm-match-header)))
          (if contents
              (setq contents
                    (concat contents clump-sep (vm-matched-header-contents)))
            (setq contents (vm-matched-header-contents)))))
      contents)))

;;;###autoload
(defun vm-mail-select-folder (folder-alist)
  "Return a folder according to FOLDER-ALIST for the current message.
This function is a slightly changed version of `vm-auto-select-folder'."
  (interactive)
  (condition-case error-data
      (catch 'match
        (let (header tuple-list)
          (while folder-alist
            (setq header (vm-mail-get-header-contents
                          (car (car folder-alist)) ", "))
            (if (null header)
                ()
              (setq tuple-list (cdr (car folder-alist)))
              (while tuple-list
                (if (let ((case-fold-search vm-auto-folder-case-fold-search))
                      (string-match (car (car tuple-list)) header))
                    ;; Don't waste time eval'ing an atom.
                    (if (stringp (cdr (car tuple-list)))
                        (throw 'match (cdr (car tuple-list)))
                      (let* ((match-data (vm-match-data))
                             ;; allow this buffer to live forever
                             (buf (get-buffer-create " *vm-auto-folder*"))
                             (result))
                        ;; Set up a buffer that matches our cached
                        ;; match data.
                        (save-excursion
                          (set-buffer buf)
                          (if vm-fsfemacs-mule-p
                              (set-buffer-multibyte nil))
                          (widen)
                          (erase-buffer)
                          (insert header)
                          ;; It appears that get-buffer-create clobbers the
                          ;; match-data.
                          ;;
                          ;; The match data is off by one because we matched
                          ;; a string and Emacs indexes strings from 0 and
                          ;; buffers from 1.
                          ;;
                          ;; Also store-match-data only accepts MARKERS!!
                          ;; AUGHGHGH!!
                          (store-match-data
                           (mapcar
                            (function (lambda (n) (and n (vm-marker n))))
                            (mapcar
                             (function (lambda (n) (and n (1+ n))))
                             match-data)))
                          (setq result (eval (cdr (car tuple-list))))
                          (while (consp result)
                            (setq result (vm-mail-select-folder result)))
                          (if result
                              (throw 'match result))))))
                (setq tuple-list (cdr tuple-list))))
            (setq folder-alist (cdr folder-alist)))
          nil ))
    (error "Error processing folder-alist: %s"
           (prin1-to-string error-data))))

;;;###autoload
(defcustom vm-mail-to-regexp "\\([^<\t\n ]+\\)@"
  "A regexp matching the part of an email address to use as FCC-folder.
The string enclosed in \"\\\\(\\\\)\" is used as folder name."
  :type 'regexp
  :group 'vm-pine)

;;;###autoload
(defcustom vm-mail-to-headers '("To:" "CC:" "BCC:")
  "A list of headers for finding the email address to use as FCC-folder."
  :type '(repeat (string))
  :group 'vm-pine)

;;;###autoload
(defun vm-mail-to-fcc (&optional arg return-only)
  "Insert a FCC-header into a `vm-mail-mode' buffer.
Like `mail-fcc', but honors VM variables and inserts the first email
address (or the like matched by `vm-mail-to-regexp') found in the headers
listed in `vm-mail-to-headers'.
Called with prefix ARG it just removes the FCC-header.
If optional argument RETURN-ONLY is t just returns FCC."
  (interactive "P")
  (expand-abbrev)
  (let ((fcc nil)
        (headers vm-mail-to-headers))
    (if arg (progn (vm-mail-mode-remove-header "FCC:")
                   (message "FCC header removed!"))
      (progn
        (while (and (not fcc) headers)
          (setq fcc (vm-mail-get-header-contents (car headers)))
          (if (and fcc (string-match vm-mail-to-regexp fcc))
              (setq fcc (match-string 1 fcc))
            (setq fcc nil))
          (setq headers (cdr headers)))
        (setq fcc (or fcc mail-archive-file-name))
        (if return-only
            fcc
          (if fcc
              (if (file-directory-p fcc)
                  (error "Folder `%s' in no file, but a directory!" fcc)
                (vm-mail-mode-remove-header "FCC:")
                (mail-position-on-field "FCC")
                (insert (vm-mail-fcc-file-join (or vm-folder-directory
                                                   default-directory)
                                               fcc)))))))))

;;-----------------------------------------------------------------------------
(provide 'vm-pine)
 
;;; vm-pine.el ends here
