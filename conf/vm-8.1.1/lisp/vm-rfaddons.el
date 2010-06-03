;;; vm-rfaddons.el --- a collections of various useful VM helper functions
;; 
;; Copyright (C) 1999-2006 Robert Widhopf-Fenk
;;
;; Author:      Robert Widhopf-Fenk
;; Status:      Integrated into View Mail (aka VM), 8.0.x
;; Keywords:    VM helpers
;; X-URL:       http://bazaar.launchpad.net/viewmail

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
;; Some of the functions should be unbundled into separate packages,
;; but well I'm a lazy guy.  And some of them are not tested well. 
;;
;; In order to use this package add the following lines to the _end_ of your
;; .vm file.  It should be the _end_ in order to ensure that variable you had
;; been setting are honored!
;;
;;      (require 'vm-rfaddons)
;;      (vm-rfaddons-infect-vm)
;;
;; If you want to use only a subset of the functions you should have a
;; look at the documentation of `vm-rfaddons-infect-vm' and modify
;; its call as desired.  
;; 
;; Additional packages you may need are:
;;
;; * Package: Personality Crisis for VM
;;   is a really cool package if you want to do automatic header rewriting,
;;   e.g.  if you have various mail accounts and always want to use the right
;;   from header, then check it out! 
;;
;; * Package: BBDB
;;   Homepage: http://bbdb.sourceforge.net
;;
;; All other packages should be included within standard (X)Emacs
;; distributions.
;;
;; As I am no active GNU Emacs user, I would be thankful for any patches to
;; make things work with GNU Emacs!
;;
;;; Code:

(defgroup vm nil
  "VM"
  :group 'mail)

(defgroup vm-rfaddons nil
  "Customize vm-rfaddons.el"
  :group 'vm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile
  (require 'vm-version)
  (require 'vm-message)
  (require 'vm-macro)
  (require 'vm-vars)
  (require 'cl)
  (require 'advice)
  (vm-load-features '(regexp-opt bbdb bbdb-vm gnus-group)))

(require 'sendmail)

(if vm-xemacs-p (require 'overlay))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vm-rfaddons-check-option (option option-list &rest body)
  "Evaluate body if option is in OPTION-LIST or OPTION-LIST is nil."
  (list 'if (list 'member option option-list)
        (cons 'progn
              (cons (list 'setq option-list (list 'delq option option-list))
                    (cons (list 'message "Adding vm-rfaddons-option `%s'."
                                option)
                          body)))))

;;;###autoload
(defun vm-rfaddons-infect-vm (&optional sit-for
                                        option-list exclude-option-list)
  "This function will setup the key bindings, advices and hooks
necessary to use all the function of vm-rfaddons.el!

SIT-FOR specifies the number of seconds to display the infection message!
The OPTION-LIST can be use to select individual option.
The EXCLUDE-OPTION-LIST can be use to exclude individual option.

The following options are possible.

`general' options:
 - rf-faces: change some faces

`vm-mail-mode' options:
 - attach-save-files: bind [C-c C-a] to `vm-mime-attach-files-in-directory' 
 - check-recipients: add `vm-mail-check-recipients' to `mail-send-hook' in
   order to check if the recipients headers are correct.
 - encode-headers: add `vm-mime-encode-headers' to `mail-send-hook' in
   order to encode the headers before sending.
 - fake-date: if enabled allows you to fake the date of an outgoing message.

`vm-mode' options:
 - shrunken-headers: enable shrunken-headers by advising several functions 

Other EXPERIMENTAL options:
 - auto-save-all-attachments: add `vm-mime-auto-save-all-attachments' to
   `vm-select-new-message-hook' for automatic saving of attachments and define
   an advice for `vm-set-deleted-flag-of' in order to automatically delete
   the files corresponding to MIME objects of type message/external-body when
   deleting the message.
 - return-receipt-to

If you want to use only a subset of the options then call
`vm-rfaddons-infect-vm' like this:
        (vm-rfaddons-infect-vm 2 '(general vm-mail-mode shrunken-headers)
                                 '(fake-date))
This will enable all `general' and `vm-mail-mode' options plus the
`shrunken-headers' option, but it will exclude the `fake-date' option of the
`vm-mail-mode' options.

or do the binding and advising on your own."
  (interactive "")

  (if (eq option-list 'all)
      (setq option-list (list 'general 'vm-mail-mode 'vm-mode
                              'auto-save-all-attachments
                              'auto-delete-message-external-body))
    (if (eq option-list t)
        (setq option-list (list 'vm-mail-mode 'vm-mode))))
  
  (when (member 'general option-list)
    (setq option-list (append '(rf-faces)
                              option-list))
    (setq option-list (delq 'general option-list)))
  
  (when (member 'vm-mail-mode option-list)
    (setq option-list (append '(attach-save-files
                                check-recipients
                                check-for-empty-subject
                                encode-headers
                                clean-subject
                                fake-date
                                open-line)
                              option-list))
    (setq option-list (delq 'vm-mail-mode option-list)))
  
  (when (member 'vm-mode option-list)
    (setq option-list (append '(
                                ;; save-all-attachments
                                shrunken-headers
                                take-action-on-attachment
				)
                              option-list))
    (setq option-list (delq 'vm-mode option-list)))
    
  (while exclude-option-list
    (if (member (car exclude-option-list) option-list)
        (setq option-list (delq (car exclude-option-list) option-list))
      (message "VM-RFADDONS: The option `%s' was not excluded, maybe it is unknown!"
               (car exclude-option-list))
      (ding)
      (sit-for 3))
    (setq exclude-option-list (cdr exclude-option-list)))
  
  ;; general ----------------------------------------------------------------
  ;; install my choice of faces 
  (vm-rfaddons-check-option
   'rf-faces option-list
   (vm-install-rf-faces))
  
  ;; vm-mail-mode -----------------------------------------------------------
  (vm-rfaddons-check-option
   'attach-save-files option-list
   (define-key vm-mail-mode-map "\C-c\C-a" 'vm-mime-attach-files-in-directory))
  
  ;; check recipients headers for errors before sending
  (vm-rfaddons-check-option
   'check-recipients option-list
   (add-hook 'mail-send-hook 'vm-mail-check-recipients))

  ;; check if the subjectline is empty
  (vm-rfaddons-check-option
   'check-for-empty-subject option-list
   (add-hook 'vm-mail-send-hook 'vm-mail-check-for-empty-subject))
  
  ;; encode headers before sending
  (vm-rfaddons-check-option
   'encode-headers option-list
   (add-hook 'mail-send-hook 'vm-mime-encode-headers))

  ;; This allows us to fake a date by advising vm-mail-mode-insert-date-maybe
  (vm-rfaddons-check-option
   'fake-date option-list
   (defadvice vm-mail-mode-insert-date-maybe (around vm-fake-date activate)
     "Do not change an existing date if `vm-mail-mode-fake-date-p' is t."
     (if (not (and vm-mail-mode-fake-date-p
                   (vm-mail-mode-get-header-contents "Date:")))
         ad-do-it)))
  
  (vm-rfaddons-check-option
   'open-line option-list
   (add-hook 'vm-mail-mode-hook 'vm-mail-mode-install-open-line))

  (vm-rfaddons-check-option
   'clean-subject option-list
   (add-hook 'vm-mail-mode-hook 'vm-mail-subject-cleanup))

  ;; vm-mode -----------------------------------------------------------

  ;; Shrunken header handlers
  (vm-rfaddons-check-option
   'shrunken-headers option-list
   (if (not (boundp 'vm-always-use-presentation-buffer))
       (message "Shrunken-headers do NOT work in standard VM!")
     ;; We would corrupt the folder buffer for messages which are
     ;; not displayed by a presentation buffer, thus we must ensure
     ;; that a presentation buffer is used.  The visibility-widget
     ;; would cause "*"s to be inserted into the folder buffer.
     (setq vm-always-use-presentation-buffer t)
     (defadvice vm-preview-current-message
       (after vm-shrunken-headers-pcm activate)
       "Shrink headers when previewing a message."
       (vm-shrunken-headers))
     (defadvice vm-expose-hidden-headers
       (after vm-shrunken-headers-ehh activate)
       "Shrink headers when viewing hidden headers."
       (vm-shrunken-headers))
     (define-key vm-mode-map "T" 'vm-shrunken-headers-toggle)))

;; This is not needed any more because VM has $ commands to take
;; action on attachments.  But we keep it for compatibility.

  ;; take action on attachment binding
  (vm-rfaddons-check-option
   'take-action-on-attachment option-list
   (define-key vm-mode-map "."  'vm-mime-take-action-on-attachment))
  
;; This is not needed any more becaue it is in the core  
;;   (vm-rfaddons-check-option
;;    'save-all-attachments option-list
;;    (define-key vm-mode-map "\C-c\C-s" 'vm-mime-save-all-attachments))

  ;; other experimental options ---------------------------------------------
  ;; Now take care of automatic saving of attachments
  (vm-rfaddons-check-option
   'auto-save-all-attachments option-list
   ;; In order to reflect MIME type changes when `vm-mime-delete-after-saving'
   ;; is t we preview the message again.
   (defadvice vm-mime-send-body-to-file
     (after vm-do-preview-again activate)
     (if vm-mime-delete-after-saving
         (vm-preview-current-message)))
   (add-hook 'vm-select-new-message-hook 'vm-mime-auto-save-all-attachments))
   
   (vm-rfaddons-check-option
    'auto-delete-message-external-body option-list
   ;; and their deletion when deleting a unfiled message,
   ;; this is probably a problem, since actually we should delete it
   ;; only if there remains no reference to it!!!!
   (defadvice vm-set-deleted-flag-of
     (before vm-mime-auto-save-all-attachments activate)
     (if (and (eq (ad-get-arg 1) 'expunged)
              (not (vm-filed-flag (ad-get-arg 0))))
         (vm-mime-auto-save-all-attachments-delete-external (ad-get-arg 0)))))

   (vm-rfaddons-check-option
    'return-receipt-to option-list
    (add-hook 'vm-select-message-hook 'vm-handle-return-receipt))

   (when option-list
    (message "VM-RFADDONS: The following options are unknown: %s" option-list)
    (ding)
    (sit-for 3))
  
  (message "VM-RFADDONS: VM is now infected.")
  (sit-for (or sit-for 2)))

(defun rf-vm-su-labels (m)
  "This version does some sanity checking."
  (let ((labels (vm-label-string-of m)))
    (if (and labels (stringp labels))
        labels
      (setq labels (vm-labels-of m))
      (if (and labels (listp labels))
          (vm-set-label-string-of
           m
           (setq labels (mapconcat 'identity labels ",")))
        (vm-set-label-string-of m "")
        (setq labels "")))
    labels))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vm-reply-include-presentation nil)

;;;###autoload
(defun vm-reply-include-presentation (count &optional to-all)
  "Include presentation instead of text.
This does only work with my modified VM, i.e. a hacked `vm-yank-message'."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if (null vm-presentation-buffer)
      (if to-all
          (vm-followup-include-text count)
        (vm-reply-include-text count))
    (let ((vm-include-text-from-presentation t)
	  (vm-reply-include-presentation t)) ; is this variable necessary?
      (vm-do-reply to-all t count))))

;;;###autoload
(defun vm-followup-include-presentation (count)
  "Include presentation instead of text.
This does not work when replying to multiple messages."
  (interactive "p")
  (vm-reply-include-presentation count t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-do-fcc-before-mime-encode ()
  "The name says it all.
Sometimes you may want to save a message unencoded, specifically not to waste
storage for attachments which are stored on disk anyway."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (regexp-quote mail-header-separator) (point-max))
    (delete-region (match-beginning 0) (match-end 0))
    (let ((header-end (point-marker)))
      (mail-do-fcc header-end)
      (goto-char header-end)
      (insert mail-header-separator))))

(defcustom vm-do-fcc-before-mime-encode nil
  "*Non-nil means to FCC before encoding."
  :type 'boolean
  :group 'vm-rfaddons)
  
(defadvice vm-mime-encode-composition
  (before do-fcc-before-mime-encode activate)
  "FCC before encoding attachments if `vm-do-fcc-before-mime-encode' is t."
  (if vm-do-fcc-before-mime-encode
      (vm-do-fcc-before-mime-encode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This has been moved to the VM core.  USR, 2010-03-11
;;;;;###autoload
;; (defun vm-fill-paragraphs-by-longlines (width start end)
;;   "Uses longlines.el for filling.
;; To use it, advice `vm-fill-paragraphs-containing-long-lines' and call this
;; function instead."
;;   (if (eq width 'window-width)
;;       (setq width (- (window-width (get-buffer-window (current-buffer))) 1)))
;;   ;; prepare for longlines.el in XEmacs
;;   (require 'overlay)
;;   (require 'longlines)
;;   (defvar fill-nobreak-predicate nil)
;;   (defvar undo-in-progress nil)
;;   (defvar longlines-mode-hook nil)
;;   (defvar longlines-mode-on-hook nil)
;;   (defvar longlines-mode-off-hook nil)
;;   (unless (functionp 'replace-regexp-in-string)
;;     (defun replace-regexp-in-string (regexp rep string
;;                                             &optional fixedcase literal)
;;       (vm-replace-in-string string regexp rep literal)))
;;   (unless (functionp 'line-end-position)
;;     (defun line-end-position ()
;;       (save-excursion (end-of-line) (point))))
;;   (unless (functionp 'line-beginning-position)
;;     (defun line-beginning-position (&optional n)
;;       (save-excursion
;;         (if n (forward-line n))
;;         (beginning-of-line)
;;         (point)))
;;     (unless (functionp 'replace-regexp-in-string)
;;       (defun replace-regexp-in-string (regexp rep string
;;                                               &optional fixedcase literal)
;;         (vm-replace-in-string string regexp rep literal))))
;;   ;; now do the filling
;;   (let ((buffer-read-only nil)
;;         (fill-column width))
;;     (save-excursion
;;       (vm-save-restriction
;;        ;; longlines-wrap-region contains a (forward-line -1) which is causing
;;        ;; wrapping of headers which is wrong, so we restrict it here!
;;        (narrow-to-region start end)
;;        (longlines-decode-region start end) ; make linebreaks hard
;;        (longlines-wrap-region start end)  ; wrap, adding soft linebreaks
;;        (widen)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-spamassassin-strip-report "spamassassin -d"
  "*Shell command used to strip spamassassin-reports from a message."
  :type 'string
  :group 'vm-rfaddons)

(defun vm-strip-spamassassin-report ()
  "Strips spamassassin-reports from a message."
  (interactive)
  (save-window-excursion
    (let ((vm-frame-per-edit nil))
      (vm-edit-message)
      (shell-command-on-region (point-min) (point-max)
                               vm-spamassassin-strip-report
                               (current-buffer)
                               t)
      (vm-edit-message-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vm-switch-to-folder-history nil)

;;;###autoload
(defun vm-switch-to-folder (folder-name)
"Switch to another opened VM folder and rearrange windows as with a scroll."
  (interactive (list
                (let ((fl (vm-folder-list))
                      (f vm-switch-to-folder-history) d)
                  (if (member major-mode
                              '(vm-mode vm-presentation-mode
                                        vm-summary-mode))
                      (save-excursion
                        (vm-select-folder-buffer)
                        (setq fl (delete (buffer-name) fl))))
                  (while f
                    (setq d (car f) f (cdr f))
                    (if (member d fl)
                        (setq f nil)))
                  (completing-read
                   (format "Foldername%s: " (if d (format " (%s)" d) ""))
                   (mapcar (lambda (f) (list f)) (vm-folder-list))
                   nil t nil
                   'vm-switch-to-folder-history
                   d))))

  (switch-to-buffer folder-name)
  (vm-select-folder-buffer)
  (vm-summarize)
  (let ((this-command 'vm-scroll-backward))
    (vm-display nil nil '(vm-scroll-forward vm-scroll-backward)
                (list this-command 'reading-message))
    (vm-update-summary-and-mode-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-rmail-mode nil
  "*Non-nil means up/down move to the next/previous message instead.
Otherwise normal cursor movement is done.  Specifically only modes
listed in `vm-rmail-mode-list' are affected.
Use `vm-rmail-toggle' to switch between normal and this mode."
  :type 'boolean
  :group 'vm-rfaddons)

(defcustom vm-rmail-mode-list '(vm-summary-mode)
  "*Mode to activate `vm-rmail-mode' in."
  :type '(set (const vm-mode)
              (const vm-presentation-mode)
              (const vm-virtual-mode)
              (const vm-summary-mode))
  :group 'vm-rfaddons)
  
(defun vm-rmail-toggle (&optional arg)
  (interactive)
  (cond ((eq nil arg)
         (setq vm-rmail-mode (not vm-rmail-mode)))
        ((=  1 arg)
         (setq vm-rmail-mode t))
        ((= -1 arg)
         (setq vm-rmail-mode nil))
        (t
         (setq vm-rmail-mode (not vm-rmail-mode))))
  (message (if vm-rmail-mode "Rmail cursor mode" "VM cursor mode")))
  
(defun vm-rmail-up ()
  (interactive)
  (cond ((and vm-rmail-mode (member major-mode vm-rmail-mode-list))
         (vm-next-message -1)
         (vm-display nil nil '(rf-vm-rmail-up vm-previous-message)
                     (list this-command)))
        (t 
         (next-line -1))))

(defun vm-rmail-down ()
  (interactive)
  (cond ((and vm-rmail-mode (member major-mode vm-rmail-mode-list))
         (vm-next-message 1)
         (vm-display nil nil '(rf-vm-rmail-up vm-next-message)
                     (list this-command)))
        (t 
         (next-line 1))))

(defun vm-do-with-message (count function vm-display)
  (vm-follow-summary-cursor)
  (save-excursion
    (vm-select-folder-buffer)
    (let ((mlist (vm-select-marked-or-prefixed-messages count)))
      (while mlist
        (funcall function (car mlist))
        (vm-mark-for-summary-update (car mlist) t)
        (setq mlist (cdr mlist))))
    (vm-display nil nil (append vm-display '(vm-do-with-message))
                (list this-command))
    (vm-update-summary-and-mode-line)))
  
(defun vm-toggle-mark (count &optional m)
  (interactive "p")
  (vm-do-with-message
   count
   (lambda (m) (vm-set-mark-of m (not (vm-mark-of m))))
   '(vm-toggle-mark vm-mark-message marking-message)))

(defun vm-toggle-deleted (count &optional m)
  (interactive "p")
  (vm-do-with-message
   count
   (lambda (m) (vm-set-deleted-flag m (not (vm-deleted-flag m))))
   '(vm-toggle-deleted vm-delete-message vm-delete-message-backward)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-mail-subject-prefix-replacements
  '(("\\(\\(re\\|aw\\|antw\\)\\(\\[[0-9]+\\]\\)?:[ \t]*\\)+" . "Re: ")
    ("\\(\\(fo\\|wg\\)\\(\\[[0-9]+\\]\\)?:[ \t]*\\)+" . "Fo: "))
  "*List of subject prefixes which should be replaced.
Matching will be done case insentivily."
  :group 'vm-rfaddons
  :type '(repeat (cons (regexp :tag "Regexp")
                       (string :tag "Replacement"))))

(defcustom vm-mail-subject-number-reply nil
  "*Non-nil means, add a number [N] after the reply prefix.
The number reflects the number of references."
  :group 'vm-rfaddons
  :type '(choice
          (const :tag "on" t)
          (const :tag "off" nil)))

(defun vm-mail-subject-cleanup ()
  "Do some subject line clean up.
- Replace subject prefixes according to `vm-replace-subject-prefixes'.
- Add a number after replies is `vm-mail-subject-number-reply' is t.

You might add this function to `vm-mail-mode-hook' in order to clean up the
Subject header."
  (interactive)
  (save-excursion
    ;; cleanup
    (goto-char (point-min))
    (re-search-forward (regexp-quote mail-header-separator) (point-max))
    (let ((case-fold-search t)
          (rpl vm-mail-subject-prefix-replacements))
      (while rpl
        (if (re-search-backward (concat "^Subject:[ \t]*" (caar rpl))
                                (point-min) t)
            (replace-match (concat "Subject: " (cdar rpl))))
        (setq rpl (cdr rpl))))

    ;; add number to replys
    (let (refs (start 0) end (count 0))
      (when (and vm-mail-subject-number-reply vm-reply-list
                 (setq refs  (vm-mail-mode-get-header-contents "References:")))
        (while (string-match "<[^<>]+>" refs start)
          (setq count (1+ count)
                start (match-end 0)))
        (when (> count 1)
          (mail-position-on-field "Subject" t)
          (setq end (point))
          (if (re-search-backward "^Subject:" (point-min) t)
              (setq start (point))
            (error "Could not find end of Subject header start!"))
          (goto-char start)
          (if (not (re-search-forward (regexp-quote vm-reply-subject-prefix)
                                      end t))
              (error "Cound not find vm-reply-subject-prefix `%s' in header!"
                     vm-reply-subject-prefix)
            (goto-char (match-end 0))
            (skip-chars-backward ": \t")
            (insert (format "[%d]" count))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vm-mime-set-8bit-composition-charset (charset &optional buffer-local)
  "*Set `vm-mime-8bit-composition-charset' to CHARSET.
With the optional BUFFER-LOCAL prefix arg, this only affects the current
buffer."
  (interactive (list (completing-read "Composition charset: "
				      vm-mime-charset-completion-alist
				      nil t)
		     current-prefix-arg))
  (if (or vm-xemacs-mule-p vm-fsfemacs-p)
      (error "vm-mime-8bit-composition-charset has no effect in XEmacs/MULE"))
  (if buffer-local
      (set (make-local-variable 'vm-mime-8bit-composition-charset) charset)
    (setq vm-mime-8bit-composition-charset charset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bbdb/vm-set-virtual-folder-alist ()
  "Create a `vm-virtual-folder-alist' according to the records in the bbdb.
For each record that has a 'vm-virtual' attribute, add or modify the
corresponding BBDB-VM-VIRTUAL element of the `vm-virtual-folder-alist'.

  (BBDB-VM-VIRTUAL ((vm-primary-inbox)
                    (author-or-recipient BBDB-RECORD-NET-REGEXP)))

The element gets added to the 'element-name' sublist of the
`vm-virtual-folder-alist'."
  (interactive)
  (let (notes-field  email-regexp folder selector)
    (dolist (record (bbdb-records))
      (setq notes-field (bbdb-record-raw-notes record))
      (when (and (listp notes-field)
                 (setq folder (cdr (assq 'vm-virtual notes-field))))
        (setq email-regexp (mapconcat '(lambda (addr)
                                         (regexp-quote addr))
                                      (bbdb-record-net record) "\\|"))
        (unless (zerop (length email-regexp))
          (setq folder (or (assoc folder vm-virtual-folder-alist)
                           (car
                            (setq vm-virtual-folder-alist
                                  (nconc (list (list folder
                                                     (list (list vm-primary-inbox)
                                                           (list 'author-or-recipient))))
                                               vm-virtual-folder-alist))))
                folder (cadr folder)
                selector (assoc 'author-or-recipient folder))

          (if (cdr selector)
              (if (not (string-match (regexp-quote email-regexp)
                                     (cadr selector)))
                  (setcdr selector (list (concat (cadr selector) "\\|"
                                                 email-regexp))))
            (nconc selector (list email-regexp)))))
      )
    ))

(defun vm-virtual-find-selector (selector-spec type)
  "Return the first selector of TYPE in SELECTOR-SPEC."
  (let ((s (assoc type selector-spec)))
    (unless s
      (while (and (not s) selector-spec)
        (setq s (and (listp (car selector-spec))
                     (vm-virtual-find-selector (car selector-spec) type))
              selector-spec (cdr selector-spec))))
    s))

(defcustom bbdb/vm-virtual-folder-alist-by-mail-alias-alist nil
  "*A list of (ALIAS . FOLDER-NAME) pairs, which map an alias to a folder."
  :group 'vm-rfaddons
  :type '(repeat (cons :tag "Mapping Definition"
                       (regexp :tag "Alias")
                       (string :tag "Folder Name"))))

(defun bbdb/vm-set-virtual-folder-alist-by-mail-alias ()
  "Create a `vm-virtual-folder-alist' according to the records in the bbdb.
For each record check wheather its alias is in the variable 
`bbdb/vm-virtual-folder-alist-by-mail-alias-alist' and then
add/modify the corresponding VM-VIRTUAL element of the
`vm-virtual-folder-alist'. 

  (BBDB-VM-VIRTUAL ((vm-primary-inbox)
                    (author-or-recipient BBDB-RECORD-NET-REGEXP)))

The element gets added to the 'element-name' sublist of the
`vm-virtual-folder-alist'."
  (interactive)
  (let (notes-field email-regexp mail-aliases folder selector)
    (dolist (record (bbdb-records))
      (setq notes-field (bbdb-record-raw-notes record))
      (when (and (listp notes-field)
                 (setq mail-aliases (cdr (assq 'mail-alias notes-field)))
                 (setq mail-aliases (bbdb-split mail-aliases ",")))
        (setq folder nil)
        (while mail-aliases
          (setq folder
                (assoc (car mail-aliases)
                       bbdb/vm-virtual-folder-alist-by-mail-alias-alist))
          
          (when (and folder
                     (setq folder (cdr folder)
                           email-regexp (mapconcat '(lambda (addr)
                                                      (regexp-quote addr))
                                                   (bbdb-record-net record)
                                                   "\\|"))
                     (> (length email-regexp) 0))
            (setq folder (or (assoc folder vm-virtual-folder-alist)
                             (car
                              (setq vm-virtual-folder-alist
                                    (nconc
                                     (list
                                      (list folder
                                            (list (list vm-primary-inbox)
                                                  (list 'author-or-recipient))
                                            ))
                                     vm-virtual-folder-alist))))
                  folder (cadr folder)
                  selector (vm-virtual-find-selector folder
                                                     'author-or-recipient))
            (unless selector
              (nconc (cdr folder) (list (list 'author-or-recipient))))
            (if (cdr selector)
                (if (not (string-match (regexp-quote email-regexp)
                                       (cadr selector)))
                    (setcdr selector (list (concat (cadr selector) "\\|"
                                                   email-regexp))))
              (nconc selector (list email-regexp))))
          (setq mail-aliases (cdr mail-aliases)))
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-handle-return-receipt-mode 'edit
  "*Tells `vm-handle-return-receipt' how to handle return receipts.
One can choose between 'ask, 'auto, 'edit or an expression which is evaluated
and which should return t if the return receipts should be sent."
  :group 'vm-rfaddons
  :type '(choice (const :tag "Edit" edit)
                 (const :tag "Ask" ask)
                 (const :tag "Auto" auto)))

(defcustom vm-handle-return-receipt-peek 500
  "*Number of characters from the original message body to be returned."
  :group 'vm-rfaddons
  :type '(integer))

(defun vm-handle-return-receipt ()
  "Generate a reply to the current message if it requests a return receipt
and has not been replied so far!
See the variable `vm-handle-return-receipt-mode' for customization."
  (interactive)
  (save-excursion
    (vm-select-folder-buffer)
    (let* ((msg (car vm-message-pointer))
           (sender (vm-get-header-contents msg  "Return-Receipt-To:"))
           (mail-signature nil)
           (mode (and sender
                      (cond ((equal 'ask vm-handle-return-receipt-mode)
                             (y-or-n-p "Send a return receipt? "))
                            ((symbolp vm-handle-return-receipt-mode)
                             vm-handle-return-receipt-mode)
                            (t
                             (eval vm-handle-return-receipt-mode)))))
           (vm-mutable-frames (if (eq mode 'edit) vm-mutable-frames nil))
           (vm-mail-mode-hook nil)
           (vm-mode-hook nil)
           message)
      (when (and mode (not (vm-replied-flag msg)))
        (vm-reply 1)
        (vm-mail-mode-remove-header "Return-Receipt-To:")
        (vm-mail-mode-remove-header "To:")
        (goto-char (point-min))
        (insert "To: " sender "\n")
        (mail-text)
        (delete-region (point) (point-max))
        (insert 
         (format 
          "Your mail has been received on %s."
          (current-time-string)))
        (save-restriction
          (save-excursion
          (set-buffer (vm-buffer-of msg))
          (widen)
          (setq message
              (buffer-substring
               (vm-vheaders-of msg)
               (let ((tp (+ vm-handle-return-receipt-peek
                            (marker-position
                             (vm-text-of msg))))
                     (ep (marker-position
                          (vm-end-of msg))))
                 (if (< tp ep) tp ep))
               ))))
        (insert "\n-----------------------------------------------------------------------------\n"
                message)
        (if (re-search-backward "^\\s-+.*" (point-min) t)
            (replace-match ""))
        (insert "[...]\n")
        (if (not (eq mode 'edit))
            (vm-mail-send-and-exit nil))
        )
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vm-mime-find-type-of-message/external-body (layout)
  (save-excursion
    (vm-select-folder-buffer)
    (save-restriction
      (set-buffer (marker-buffer (vm-mm-layout-body-start layout)))
      (widen)
      (goto-char (vm-mm-layout-body-start layout))
      (if (not (re-search-forward "Content-Type: \"?\\([^ ;\" \n\t]+\\)\"?;?"
                                  (vm-mm-layout-body-end layout)
                                  t))
          (error "No `Content-Type' header found in: %s"
                 (buffer-substring (vm-mm-layout-body-start layout)
                                   (vm-mm-layout-body-end layout)))
        (match-string 1)))))

;; This is a hack in order to get the right MIME button 
;(defadvice vm-mime-set-extent-glyph-for-type
;  (around vm-message/external-body-glyph activate)
;  (if (and (boundp 'real-mime-type)
;          (string= (ad-get-arg 1) "message/external-body"))
;      (ad-set-arg 1 real-mime-type))
;  ad-do-it)
      
;;;###autoload
(defun vm-mime-display-button-message/external-body (layout)
  "Return a button usable for viewing message/external-body MIME parts.
When you apply `vm-mime-send-body-to-file' with `vm-mime-delete-after-saving'
set to t one will get theses message/external-body parts which point
to the external file.
In order to view these we search for the right viewer hopefully listed
in `vm-mime-external-content-types-alist' and invoke it as it would
have happened before saving.  Otherwise we display the contents as text/plain.
Probably we should be more clever here in order to fake a layout if internal
displaying is possible ...

But nevertheless this allows for keeping folders smaller without
loosing basic functionality when using `vm-mime-auto-save-all-attachments'." 
  (let ((buffer-read-only nil)
        (real-mime-type (vm-mime-find-type-of-message/external-body layout)))
    (vm-mime-insert-button
     (vm-replace-in-string
      (format " external: %s %s"
              (if (vm-mime-get-parameter layout "name")
                  (file-name-nondirectory (vm-mime-get-parameter layout "name"))
                "")
              (let ((tmplayout (copy-tree layout t))
                    format)
                (aset tmplayout 0 (list real-mime-type))
                (setq format (vm-mime-find-format-for-layout tmplayout))
                (setq format (vm-replace-in-string format "^%-[0-9]+.[0-9]+"
                                                "%-15.15" t))
                (vm-mime-sprintf format tmplayout)))
      "save to a file\\]"
      "display as text]")
     (function
      (lambda (xlayout)
        (setq layout (if vm-xemacs-p
                         (vm-extent-property xlayout 'vm-mime-layout)
                       (overlay-get xlayout 'vm-mime-layout)))
        (let* ((type (vm-mime-find-type-of-message/external-body layout))
               (viewer (vm-mime-find-external-viewer type))
               (filename (vm-mime-get-parameter layout "name")))
          (if (car viewer)
              (progn
                (message "Viewing %s with %s" filename (car viewer))
                (start-process (format "Viewing %s" filename)
                               nil
                               (car viewer)
                               filename))
            (let ((buffer-read-only nil)
                  (converter (assoc type vm-mime-type-converter-alist)))
              (if vm-xemacs-p
                  (delete-region (extent-start-position xlayout)
                                 (extent-end-position xlayout))
                (delete-region (overlay-start xlayout) (overlay-end xlayout)))
              
              (if converter
                  (shell-command (concat (caddr converter) " < '" filename "'")
                                 1)
                (message "Could not find viewer for type %s!" type)
                (insert-file-contents filename))))
          )))
     layout
      nil)))

;;;###autoload
;(defun vm-mime-display-internal-message/external-body (layout)
;  "Display the text of the message/external-body MIME part."
;  (vm-mime-display-internal-text/plain layout))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vm-mime-attach-files-in-directory-regexps-history nil
  "Regexp history for matching files.")

(defcustom vm-mime-attach-files-in-directory-default-type nil
  "*The default MIME-type for attached files.
If set to nil you will be asked for the type if it cannot be guessed.
For guessing mime-types we use `vm-mime-attachment-auto-type-alist'."
  :group 'vm-rfaddons
  :type '(choice (const :tag "Ask" nil)
                 (string "application/octet-stream")))

(defcustom vm-mime-attach-files-in-directory-default-charset 'guess
  "*The default charset used for attached files of type `text'.
If set to nil you will be asked for the charset.
If set to 'guess it will be determined by `vm-determine-proper-charset', but
this may take some time, since the file needs to be visited."
  :group 'vm-rfaddons
  :type '(choice (const :tag "Ask" nil)
                 (const :tag "Guess" guess)))

(defun vm-mime-is-type-valid (type types-alist type-exceptions)
  (catch 'done
    (let ((list type-exceptions)
          (matched nil))
      (while list
        (if (vm-mime-types-match (car list) type)
            (throw 'done nil)
          (setq list (cdr list))))
      (setq list types-alist)
      (while (and list (not matched))
        (if (vm-mime-types-match (car list) type)
            (setq matched t)
          (setq list (cdr list))))
      matched )))

;;;###autoload
(defun vm-mime-attach-files-in-directory (directory &optional regexp)
  "Attach all files in DIRECTORY matching REGEXP.
The optional argument MATCH might specify a regexp matching all files
which should be attached, when empty all files will be attached.

When called with a prefix arg it will do a literal match instead of a regexp
match."
  (interactive
   (flet ((substitute-in-file-name (file) file))
     (let ((file (vm-read-file-name
                  "Attach files matching regexp: "
                  (or vm-mime-all-attachments-directory
                      vm-mime-attachment-save-directory
                      default-directory)
                  (or vm-mime-all-attachments-directory
                      vm-mime-attachment-save-directory
                      default-directory)
                  nil nil
                  vm-mime-attach-files-in-directory-regexps-history)))
       (list (file-name-directory file)
             (file-name-nondirectory file)))))

  (setq vm-mime-all-attachments-directory directory)

  (message "Attaching files matching `%s' from directory %s " regexp directory)
  
  (if current-prefix-arg
      (setq regexp (concat "^" (regexp-quote regexp) "$")))
  
  (let ((files (directory-files directory t regexp nil))
        file type charset)
    (if (null files)
        (error "No matching files!")
      (while files
        (setq file (car files))
        (if (file-directory-p file)
            nil ;; should we add recursion here?
          (setq type (or (vm-mime-default-type-from-filename file)
                         vm-mime-attach-files-in-directory-default-type))
          (message "Attaching file %s with type %s ..." file type)
          (if (null type)
              (let ((default-type (or (vm-mime-default-type-from-filename file)
                                      "application/octet-stream")))
                (setq type (completing-read
                            (format "Content type for %s (default %s): "
                                    (file-name-nondirectory file)
                                    default-type)
                            vm-mime-type-completion-alist)
                      type (if (> (length type) 0) type default-type))))
          (if (not (vm-mime-types-match "text" type)) nil
            (setq charset vm-mime-attach-files-in-directory-default-charset)
            (cond ((eq 'guess charset)
                   (save-excursion
                     (let ((b (get-file-buffer file)))
                       (set-buffer (or b (find-file-noselect file t t)))
                       (setq charset (vm-determine-proper-charset (point-min)
                                                                  (point-max)))
                       (if (null b) (kill-buffer (current-buffer))))))
                  ((null charset)
                   (setq charset
                         (completing-read
                          (format "Character set for %s (default US-ASCII): "
                                  file)
                          vm-mime-charset-completion-alist)
                         charset (if (> (length charset) 0) charset)))))
          (vm-mime-attach-file file type charset))
        (setq files (cdr files))))))

(defcustom vm-mime-auto-save-all-attachments-subdir
  nil
  "*Subdirectory where to save the attachments of a message.
This variable might be set to a string, a function or anything which evaluates
to a string.  If set to nil we use a concatenation of the from, subject and
date header as subdir for the attachments."
  :group 'vm-rfaddons
  :type '(choice (directory :tag "Directory")
                 (string :tag "No Subdir" "")
                 (function :tag "Function")
                 (sexp :tag "sexp")))

(defun vm-mime-auto-save-all-attachments-subdir (msg)
  "Return a subdir for the attachments of MSG.
This will be done according to `vm-mime-auto-save-all-attachments-subdir'."
  (setq msg (vm-real-message-of msg))
  (when (not (string-match (regexp-quote (vm-su-full-name msg))
                           (vm-get-header-contents msg "From:")))
    (backtrace)
    (if (y-or-n-p (format "Is this wrong? %s <> %s "
                         (vm-su-full-name msg)
                         (vm-get-header-contents msg "From:")))
        (error "Yes it is wrong!")))
    
  (cond ((functionp vm-mime-auto-save-all-attachments-subdir)
         (funcall vm-mime-auto-save-all-attachments-subdir msg))
        ((stringp vm-mime-auto-save-all-attachments-subdir)
         (vm-summary-sprintf vm-mime-auto-save-all-attachments-subdir msg))
        ((null vm-mime-auto-save-all-attachments-subdir)
         (let (;; for the folder
               (basedir (buffer-file-name (vm-buffer-of msg)))
               ;; for the message
               (subdir (concat 
                        "/"
                        (format "%04s.%02s.%02s-%s"
                                (vm-su-year msg)
                                (vm-su-month-number msg)
                                (vm-su-monthday msg)
                                (vm-su-hour msg))
                        "--"
                        (vm-decode-mime-encoded-words-in-string
                         (or (vm-su-full-name msg)
                             "unknown"))
                        "--"
                        (vm-decode-mime-encoded-words-in-string
                         (vm-su-subject msg)))))
               
           (if (and basedir vm-folder-directory
                    (string-match
                     (concat "^" (expand-file-name vm-folder-directory))
                     basedir))
               (setq basedir (replace-match "" nil nil basedir)))
           
           (setq subdir (vm-replace-in-string subdir "\\s-\\s-+" " " t))
           (setq subdir (vm-replace-in-string subdir "[^A-Za-z0-9\241-_-]+" "_" t))
           (setq subdir (vm-replace-in-string subdir "?_-?_" "-" nil))
           (setq subdir (vm-replace-in-string subdir "^_+" "" t))
           (setq subdir (vm-replace-in-string subdir "_+$" "" t))
           (concat basedir "/" subdir)))
        (t
         (eval vm-mime-auto-save-all-attachments-subdir))))

(defun vm-mime-auto-save-all-attachments-path (msg)
  "Create a path for storing the attachments of MSG."
  (let ((subdir (vm-mime-auto-save-all-attachments-subdir
                 (vm-real-message-of msg))))
    (if (not vm-mime-attachment-save-directory)
        (error "Set `vm-mime-attachment-save-directory' for autosaving of attachments!")
      (if subdir
          (if (string-match "/$" vm-mime-attachment-save-directory)
              (concat vm-mime-attachment-save-directory subdir)
            (concat vm-mime-attachment-save-directory "/" subdir))
        vm-mime-attachment-save-directory))))

;;;###autoload
(defun vm-mime-auto-save-all-attachments (&optional count)
  "Save all attachments to a subdirectory.
Root directory for saving is `vm-mime-attachment-save-directory'.

You might add this to `vm-select-new-message-hook' in order to automatically
save attachments.

    (add-hook 'vm-select-new-message-hook 'vm-mime-auto-save-all-attachments)
"
  (interactive "P")

  (if vm-mime-auto-save-all-attachments-avoid-recursion
      nil
    (let ((vm-mime-auto-save-all-attachments-avoid-recursion t))
      (vm-check-for-killed-folder)
      (vm-select-folder-buffer)
      (vm-check-for-killed-summary)
      
      (vm-mime-save-all-attachments
       count
       'vm-mime-auto-save-all-attachments-path)

      (when (interactive-p)
        (vm-discard-cached-data)
        (vm-preview-current-message)))))

;;;###autoload
(defun vm-mime-auto-save-all-attachments-delete-external (msg)
  "Deletes the external attachments created by `vm-mime-save-all-attachments'.
You may want to use this function in order to get rid of the external files
when deleting a message.

See the advice in `vm-rfaddons-infect-vm'."
  (interactive "")
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (setq msg (or msg (car vm-message-pointer)))
  (if msg 
      (let ((o (vm-mm-layout msg))
            (no 0)
            parts layout file type)

        (if (eq 'none o)
            nil;; this is no mime message
          (setq type (car (vm-mm-layout-type o)))
      
          (cond ((or (vm-mime-types-match "multipart/alternative" type)
                     (vm-mime-types-match "multipart/mixed" type))
                 (setq parts (copy-sequence (vm-mm-layout-parts o))))
                (t (setq parts (list o))))
        
          (while parts
            (if (vm-mime-composite-type-p
                 (car (vm-mm-layout-type (car parts))))
                (setq parts (nconc (copy-sequence
                                    (vm-mm-layout-parts
                                     (car parts)))
                                   (cdr parts))))
      
            (setq layout (car parts))
            (if layout
                (setq type (car (vm-mm-layout-type layout))))

            (if (not (string= type "message/external-body"))
                nil
              (setq file (vm-mime-get-parameter layout "name"))
              (if (and file (file-exists-p file))
                  (progn (delete-file file)
                         (setq no (+ 1 no)))))
            (setq parts (cdr parts))))

        (if (> no 0)
            (message "%s file%s deleted."
                     (if (= no 1) "One" no)
                     (if (= no 1) "" "s")))

        (if (and file
                 (file-name-directory file)
                 (file-exists-p (file-name-directory file))
                 ;; is the directory empty?
                 (let ((files (directory-files (file-name-directory file))))
                   (and files (= 2 (length files)))))
            (delete-directory (file-name-directory file))))))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-mail-check-recipients ()
  "Check if the recipients are specified correctly.
Actually it checks only if there are any missing commas or the like in the
headers."
  (interactive)
  (let ((header-list '("To:" "CC:" "BCC:"
                       "Resent-To:" "Resent-CC:" "Resent-BCC:"))
        (contents nil)
        (errors nil))
    (while header-list
      (setq contents (vm-mail-mode-get-header-contents (car header-list)))
      (if (and contents (string-match "@[^,\"]*@" contents))
          (setq errors (vm-replace-in-string
                        (format "Missing separator in %s \"%s\"!  "
                                (car header-list)
                                (match-string 0 contents))
                        "[\n\t ]+" " ")))
      (setq header-list (cdr header-list)))
    (if errors
        (error errors))))


(defcustom vm-mail-prompt-if-subject-empty t
  "*Prompt for a subject when empty."
  :group 'vm-rfaddons
  :type '(boolean))

;;;###autoload
(defun vm-mail-check-for-empty-subject ()
  "Check if the subject line is empty and issue an error if so."
  (interactive)
  (let (subject)
    (setq subject (vm-mail-mode-get-header-contents "Subject:"))
    (if (or (not subject) (string-match "^[ \t]*$" subject))
        (if (not vm-mail-prompt-if-subject-empty)
            (error "Empty subject")
          (mail-position-on-field "Subject")
          (insert (read-string "Subject: "))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface vm-shrunken-headers-face 
  '((t (:background "gray")))
  "Used for marking shrunken headers."
  :group 'vm-faces)

(defvar vm-shrunken-headers-keymap
  (let ((map (if vm-xemacs-p (make-keymap) (copy-keymap vm-mode-map))))
    (define-key map [(return)]   'vm-shrunken-headers-toggle-this)
    (if vm-xemacs-p
        (define-key map [(button2)]  'vm-shrunken-headers-toggle-this-mouse)
      (define-key map [(mouse-2)]  'vm-shrunken-headers-toggle-this-mouse))
    map)
  "*Keymap used for shrunken-headers glyphs.")

;;;###autoload
(defun vm-shrunken-headers-toggle ()
  "Toggle display of shrunken headers."
  (interactive)
  (vm-shrunken-headers 'toggle))

;;;###autoload
(defun vm-shrunken-headers-toggle-this-mouse (&optional event)
  "Toggle display of shrunken headers!"
  (interactive "e")
  (mouse-set-point event)
  (end-of-line)
  (vm-shrunken-headers-toggle-this))

;;;###autoload
(defun vm-shrunken-headers-toggle-this-widget (widget &rest event)
  (goto-char (widget-get widget :to))
  (end-of-line)
  (vm-shrunken-headers-toggle-this))

;;;###autoload
(defun vm-shrunken-headers-toggle-this ()
  "Toggle display of shrunken headers!"
  (interactive)
  
  (save-excursion
    (if (and (boundp 'vm-mail-buffer) (symbol-value 'vm-mail-buffer))
        (set-buffer (symbol-value 'vm-mail-buffer)))
    (if vm-presentation-buffer
        (set-buffer vm-presentation-buffer))
    (let ((o (or (car (vm-shrunken-headers-get-overlays (point)))
                 (car (vm-shrunken-headers-get-overlays
                       (save-excursion (end-of-line)
                                       (forward-char 1)
                                       (point)))))))
      (save-restriction
        (narrow-to-region (- (overlay-start o) 7) (overlay-end o))
        (vm-shrunken-headers 'toggle)
        (widen)))))

(defun vm-shrunken-headers-get-overlays (start &optional end)
  (let ((o-list (if end
                    (overlays-in start end)
                  (overlays-at start))))
    (setq o-list (mapcar (lambda (o)
                           (if (overlay-get o 'vm-shrunken-headers)
                               o
                             nil))
                         o-list)
          o-list (delete nil o-list))))

;;;###autoload
(defun vm-shrunken-headers (&optional toggle)
  "Hide or show headers which occupy more than one line.
Well, one might do it more precisely with only some headers,
but it is sufficient for me!

If the optional argument TOGGLE, then hiding is toggled.

The face used for the visible hidden regions is `vm-shrunken-headers-face' and
the keymap used within that region is `vm-shrunken-headers-keymap'."
  (interactive "P")
  
  (save-excursion 
    (let (headers-start headers-end start end o shrunken)
      (if (equal major-mode 'vm-summary-mode)
          (if (and (boundp 'vm-mail-buffer) (symbol-value 'vm-mail-buffer))
              (set-buffer (symbol-value 'vm-mail-buffer))))
      (if (equal major-mode 'vm-mode)
          (if vm-presentation-buffer
              (set-buffer vm-presentation-buffer)))

      ;; We cannot use the default functions (vm-headers-of, ...) since
      ;; we might also work within a presentation buffer.
      (goto-char (point-min))
      (setq headers-start (point-min)
            headers-end (or (re-search-forward "\n\n" (point-max) t)
                            (point-max)))

      (cond (toggle
             (setq shrunken (vm-shrunken-headers-get-overlays
                             headers-start headers-end))
             (while shrunken
               (setq o (car shrunken))
               (let ((w (overlay-get o 'vm-shrunken-headers-widget)))
                 (widget-toggle-action w))
	       (overlay-put o 'invisible (not (overlay-get o 'invisible)))
	       (setq shrunken (cdr shrunken))))
            (t
             (goto-char headers-start)
             (while (re-search-forward "^\\(\\s-+.*\n\\)+" headers-end t)
               (setq start (match-beginning 0) end (match-end 0))
               (setq o (vm-shrunken-headers-get-overlays start end))
               (if o
                   (setq o (car o))
                 (setq o (make-overlay (1- start) end))
                 (overlay-put o 'face 'vm-shrunken-headers-face)
                 (overlay-put o 'mouse-face 'highlight)
                 (overlay-put o 'local-map vm-shrunken-headers-keymap)
                 (overlay-put o 'priority 10000)
                 ;; make a new overlay for the invisibility, the other one we
                 ;; made before is just for highlighting and key-bindings ...
                 (setq o (make-overlay start end))
                 (overlay-put o 'vm-shrunken-headers t)
		 (goto-char (1- start))
		 (overlay-put o 'start-closed nil)
		 (overlay-put o 'vm-shrunken-headers-widget
			      (widget-create 'visibility
					     :action
                                      'vm-shrunken-headers-toggle-this-widget))
		 (overlay-put o 'invisible t)))))
      (goto-char (point-min)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-assimilate-html-command "striptags"
  "*Command/function which should be called for stripping tags.

When this is a string, then it is a command which is fed with the
html and which should return the text.
Otherwise it should be a Lisp function which performs the stripping of
the tags.

I prefer to use lynx for this job:

#!/bin/tcsh

tmpfile=/tmp/$USER-stripttags.html
cat > $tmpfile
lynx -force_html -dump $tmpfile
rm $tmpfile"
  :group 'vm-rfaddons
  :type '(string))

(defcustom vm-assimilate-html-mixed t
  "*Non-nil values cause messages to be assimilated as text/mixed.
Otherwise they will be assimilated into a text/alternative message."
  :group 'vm-rfaddons
  :type '(boolean))

;;;###autoload
(defun vm-assimilate-html-message (&optional plain)
  "Try to assimilate a message which is only in html format.
When called with a prefix argument then it will replace the message
with the PLAIN text version otherwise it will create a text/mixed or
text/alternative message depending on the value of the variable
`vm-assimilate-html-mixed'."
  (interactive "P")

  (let ((vm-frame-per-edit nil)
        (boundary (concat (vm-mime-make-multipart-boundary)))
        (case-fold-search t)
        (qp-encoded nil)
        body start end charset)
    
    (vm-edit-message)
    (goto-char (point-min))
    (goto-char (re-search-forward "\n\n"))

    (if (re-search-backward "^Content-Type:\\s-*\\(text/html\\)\\(.*\n?\\(^\\s-.*\\)*\\)$"
                            (point-min) t)
        (progn (setq charset (buffer-substring (match-beginning 2)
                                               (match-end 2)))
               (if plain
                   (progn (delete-region (match-beginning 1) (match-end 1))
                          (goto-char (match-beginning 1))
                          (insert "text/plain"))
                 (progn (delete-region (match-beginning 1) (match-end 2))
                        (goto-char (match-beginning 1))
                        (insert "multipart/"
                                (if vm-assimilate-html-mixed "mixed"
                                  "alternative") ";\n"
                                  "  boundary=\"" boundary "\""))))
      (progn
        (kill-this-buffer)
        (error "This message seems to be no HTML only message!")))

    (goto-char (point-min))
    (goto-char (re-search-forward "\n\n"))
    (setq qp-encoded (re-search-backward "^Content-Transfer-Encoding: quoted-printable"
                                         (point-min) t))
    
    (goto-char (re-search-forward "\n\n"))
    (if plain
        (progn (setq body (point)
                     start (point))
               (goto-char (point-max))
               (setq end (point)))
      (progn (insert "--" boundary "\n"
                     "Content-Type: text/plain" charset "\n"
                     "Content-Transfer-Encoding: 8bit\n\n")
             (setq body (point))
             
             (insert "\n--" boundary "\n"
                     "Content-Type: text/html" charset "\n"
                     "Content-Transfer-Encoding: 8bit\n\n")
               (setq start (point-marker))
               (goto-char (point-max))
               (setq end (point-marker))
               (insert "--" boundary "--\n")))

    (if qp-encoded (vm-mime-qp-decode-region start end))
    
    (goto-char body)
    (if (stringp vm-assimilate-html-command)
        (call-process-region start end vm-assimilate-html-command
                             plain t)
      (funcall vm-assimilate-html-command start end plain))
    (vm-edit-message-end)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Original Authors:  Edwin Huffstutler & John Reynolds

(defcustom vm-mail-mode-citation-kill-regexp-alist
  (list
   ;; empty lines multi quoted 
   (cons (concat "^\\(" vm-included-text-prefix "[|{}>:;][^\n]*\n\\)+")
         "[...]\n")
   ;; empty quoted starting/ending lines
   (cons (concat "^\\([^|{}>:;]+.*\\)\n"
                 vm-included-text-prefix "[|{}>:;]*$")
         "\\1")
   (cons (concat "^" vm-included-text-prefix "[|{}>:;]*\n"
                 "\\([^|{}>:;]\\)")
         "\\1")
   ;; empty quoted multi lines 
   (cons (concat "^" vm-included-text-prefix "[|{}>:;]*\\s-*\n\\("
                 vm-included-text-prefix "[|{}>:;]*\\s-*\n\\)+")
         (concat vm-included-text-prefix "\n"))
   ;; empty lines
   (cons "\n\n\n+"
         "\n\n")
   ;; signature & -----Ursprüngliche Nachricht-----
   (cons (concat "^" vm-included-text-prefix "--[^\n]*\n"
                 "\\(" vm-included-text-prefix "[^\n]*\n\\)+")
         "\n")
   (cons (concat "^" vm-included-text-prefix "________[^\n]*\n"
                 "\\(" vm-included-text-prefix "[^\n]*\n\\)+")
         "\n")
   )
  "*Regexp replacement pairs for cleaning of replies."
  :group 'vm-rfaddons
  :type '(repeat (cons :tag "Kill Definition"
                       (regexp :tag "Regexp")
                       (string :tag "Replacement"))))
   
(defun vm-mail-mode-citation-clean-up ()
  "Remove doubly-cited text and extra lines in a mail message."
  (interactive)
  (save-excursion
    (mail-text)
    (let ((re-alist vm-mail-mode-citation-kill-regexp-alist)
          (pmin (point))
          re subst)

      (while re-alist
        (goto-char pmin)
        (setq re (caar re-alist)
              subst (cdar re-alist))
        (while (re-search-forward re (point-max) t)
          (replace-match subst))
        (setq re-alist (cdr re-alist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-summary-attachment-label "$"
  "*Label added to messages containing an attachments."
  :group 'vm-rfaddons
  :type '(choice (string) (const :tag "No Label" nil)))

;;;###autoload
(defun vm-summary-attachment-label (msg)
  "Indicate if there are attachments in a message.
The summary displays a `vm-summary-attachment-indicator', which is a '$' by
default.  In order to get this working, add a \"%1UA\" to your
`vm-summary-format' and call `vm-fix-my-summary!!!'.

As a sideeffect a label can be added to new messages.  Setting 
`vm-summary-attachment-label' to a string (the label) enables this.
If you just want the label, then set `vm-summary-attachment-indicator' to nil
and add an \"%0UA\" to your `vm-summary-format'." 
  (let ((attachments 0))
    (setq msg (vm-real-message-of msg))
    (vm-mime-action-on-all-attachments
     nil
     (lambda (msg layout type file)
       (setq attachments (1+ attachments)))
     vm-summary-attachment-mime-types
     vm-summary-attachment-mime-type-exceptions
     (list msg)
     t)
                                       
    (when (and (> attachments 0 )
               (vm-new-flag msg)
               (or (not (vm-labels-of msg))
                   (not (member vm-summary-attachment-label
                                (vm-labels-of msg)))))
      (vm-set-labels msg (append (list vm-summary-attachment-label)
                                 (vm-labels-of msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-delete-quit ()
  "Delete mails and quit.  Expunge only if it's not the primary inbox!"
  (interactive)
  (save-excursion
    (vm-select-folder-buffer)
    (if (and buffer-file-name
             (string-match (regexp-quote vm-primary-inbox) buffer-file-name))
        (message "No auto-expunge for folder `%s'!" buffer-file-name)
      (condition-case nil
          (vm-expunge-folder)
        (error nil)))
    (vm-quit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-mail-mode-install-open-line ()
  "Install the open-line hooks for `vm-mail-mode'.
Add this to `vm-mail-mode-hook'."
  ;; these are not local even when using add-hook, so we make them local
  (make-local-hook 'before-change-functions)
  (make-local-hook 'after-change-functions)
  (add-hook 'before-change-functions 'vm-mail-mode-open-line nil t)
  (add-hook 'after-change-functions 'vm-mail-mode-open-line nil t))

(defvar vm-mail-mode-open-line nil
  "Flag used by `vm-mail-mode-open-line'.")

(defcustom vm-mail-mode-open-line-regexp "[ \t]*>"
  "Regexp matching prefix of quoted text at line start.")

(defun vm-mail-mode-open-line (start end &optional length)
  "Opens a line when inserting into the region of a reply.

Insert newlines before and after an insert where necessary and does a cleanup
of empty lines which have been quoted." 
  (if (= start end)
      (save-excursion
        (beginning-of-line)
        (setq vm-mail-mode-open-line
              (if (and (eq this-command 'self-insert-command)
                       (looking-at (concat "^"
                                           vm-mail-mode-open-line-regexp)))
                  (if (< (point) start) (point) start))))
    (if (and length (= length 0) vm-mail-mode-open-line)
        (let (start-mark end-mark)
          (save-excursion 
            (if (< vm-mail-mode-open-line start)
                (progn
                  (insert "\n\n" vm-included-text-prefix)
                  (setq end-mark (point-marker))
                  (goto-char start)
                  (setq start-mark (point-marker))
                  (insert "\n\n"))
              (if (looking-at (concat "\\("
                                      vm-mail-mode-open-line-regexp
                                      "\\)+[ \t]*\n"))
                  (replace-match ""))
              (insert "\n\n")
              (setq end-mark (point-marker))
              (goto-char start)
              (setq start-mark (point-marker))
              (insert "\n"))

            ;; clean leading and trailing garbage 
            (let ((iq (concat "^" vm-mail-mode-open-line-regexp
                              "[> \t]*\n")))
              (save-excursion
                (goto-char start-mark)
                (beginning-of-line)
                (while (looking-at "^$") (forward-line -1))
;                (message "1%s<" (buffer-substring (point) (save-excursion (end-of-line) (point))))
                (while (looking-at iq)
                  (replace-match "")
                  (forward-line -1))
                (goto-char end-mark)
                (beginning-of-line)
                (while (looking-at "^$") (forward-line 1))
;                (message "3%s<" (buffer-substring (point) (save-excursion (end-of-line) (point))))
                (while (looking-at iq)
                  (replace-match "")))))
      
          (setq vm-mail-mode-open-line nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-mail-mode-elide-reply-region "[...]\n"
  "*String which is used as replacement for elided text."
  :group 'vm-rfaddons
  :type '(string))

;;;###autoload
(defun vm-mail-mode-elide-reply-region (b e)
  "Replace marked region or current line with `vm-mail-elide-reply-region'.
B and E are the beginning and end of the marked region or the current line."
  (interactive (if (mark)
                   (if (< (mark) (point))
                       (list (mark) (point))
                     (list (point) (mark)))
                 (list (save-excursion (beginning-of-line) (point))
                       (save-excursion (end-of-line) (point)))))
  (if (eobp) (insert "\n"))
  (if (mark) (delete-region b e) (delete-region b (+ 1 e)))
  (insert vm-mail-mode-elide-reply-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-save-everything ()
  "Save all VM folder buffers, BBDB and newsrc if GNUS is started."
  (interactive)
  (save-excursion
    (let ((folders (vm-folder-list)))
      (while folders
        (set-buffer (car folders))
        (message "Saving <%S>" (car folders))
        (vm-save-folder)
        (setq folders (cdr folders))))
    (if (fboundp 'bbdb-save-db)
        (bbdb-save-db)))
  (if (fboundp 'gnus-group-save-newsrc)
      (gnus-group-save-newsrc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-get-all-new-mail ()
  "Get mail for all opened VM folders."
  (interactive)
  (save-excursion
    (let ((buffers (buffer-list)))
      (while buffers
        (set-buffer (car buffers))
        (if (eq major-mode 'vm-mode)
            (vm-get-new-mail))
        (setq buffers (cdr buffers))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-save-message-preview (file)
  "Save preview of a message in FILE.
It saves the decoded message and not the raw message like `vm-save-message'!"
  (interactive
   ;; protect value of last-command
   (let ((last-command last-command)
         (this-command this-command)
         filename)
     (vm-follow-summary-cursor)
     (vm-select-folder-buffer)
     (setq filename
      (vm-read-file-name
       (if vm-last-written-file
           (format "Write text to file: (default %s) "
                   vm-last-written-file)
         "Write text to file: ")
       nil vm-last-written-file nil))
     (if (and (file-exists-p filename)
              (not (yes-or-no-p (format "Overwrite '%s'? " filename))))
         (error "Aborting `vm-save-message-preview'."))
     (list filename)))
    (save-excursion
      (vm-follow-summary-cursor)
      (vm-select-folder-buffer)
      (vm-check-for-killed-summary)
      (vm-error-if-folder-empty)
      
      (if (and (boundp 'vm-mail-buffer) (symbol-value 'vm-mail-buffer))
          (set-buffer (symbol-value 'vm-mail-buffer))
        (if vm-presentation-buffer
            (set-buffer vm-presentation-buffer)))
      (write-region (point-min) (point-max) file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This code is now obsolete.  VM has built-in facilities for taking
;; actions on attachments.  USR, 2010-01-05
;; Subject: Re: How to Delete an attachment?
;; Newsgroups: gnu.emacs.vm.info
;; Date: 05 Oct 1999 11:09:19 -0400
;; Organization: Road Runner
;; From: Dave Bakhash
(defun vm-mime-take-action-on-attachment (action)
  "Do something with the MIME attachment at point."
  (interactive
   (list (vm-read-string "action: "
                         '("save-to-file"
                           "delete"
                           "display-as-ascii"
                           "pipe-to-command")
                         nil)))
  (vm-mime-run-display-function-at-point
   (cond ((string= action "save-to-file")
          'vm-mime-send-body-to-file)
         ((string= action "display-as-ascii")
          'vm-mime-display-body-as-text)
         ((string= action "delete")
          (vm-delete-mime-object))
         ((string= action "pipe-to-command")
          'vm-mime-pipe-body-to-queried-command-discard-output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subject: RE: How to configure for more obvious 'auto decode' attachement.
;; Newsgroups: gnu.emacs.vm.info
;; Date: Mon, 20 Sep 1999 21:48:37 GMT
;; Organization: Deja.com - Share what you know. Learn what you don't.
;; From: rmirani
(defcustom vm-mime-display-internal-multipart/mixed-separater
  "\n----------------------------------------------------------------------\n"
  "*The separator which is inserted between the parts of a multipart message."
  :group 'vm-rfaddons
  :type '(choice (string :tag "Separator")
                 (const :tag "No Separator" nil)))

;;;###autoload
(defun vm-mime-display-internal-multipart/mixed (layout)
  "A replacement for VMs default function adding separators.
LAYOUT specifies the layout."
  
  (let ((part-list (vm-mm-layout-parts layout)))
    (while part-list
      (let ((cur (car part-list)))
        (vm-decode-mime-layout cur)
        (setq part-list (cdr part-list))
        (cond
         ((and part-list
               (not (vm-mime-should-display-button cur nil))
               (vm-mime-should-display-button (car part-list) nil))
	  ;; do nothing 
	  )
         ((and part-list
               (not (vm-mime-should-display-button cur nil))
               (not (vm-mime-should-display-button (car part-list) nil))
               vm-mime-display-internal-multipart/mixed-separater)
          (insert vm-mime-display-internal-multipart/mixed-separater)))))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun vm-assimilate-outlook-message ()
  "Assimilate a message which has been forwarded by MS Outlook.
You will need vm-pine.el in order to get this work."
  (interactive)
  (vm-continue-postponed-message t)
  (let ((pm (point-max)))
    (goto-char (point-min))
    (if (re-search-forward "^.*\\(-----Urspr[u]ngliche Nachricht-----\\|-----Original Message-----\\)\n" pm)
        (delete-region 1 (match-end 0)))
    ;; remove the quotes from the forwarded message 
    (while (re-search-forward "^> ?" pm t)
      (replace-match ""))
    (goto-char (point-min))
    ;; rewrite headers 
    (while (re-search-forward "^\\(Von\\|From\\):[ \t]*\\(.+\\) *\\[\\(SMTP\\|mailto\\):\\(.+\\)\\].*" pm t)
      (replace-match "From: \\2 <\\4>"))
    (while (re-search-forward "^\\(Gesendet[^:]*\\|Sent\\):[ \t]*\\(...\\).*, \\([0-9]+\\)\\. \\(...\\)[a-z]+[ \t]*\\(.*\\)" pm t)
      (replace-match "Date: \\3 \\4 \\5"))
    (while (re-search-forward "^\\(An\\|To\\):[ \t]*\\(.*\\)$" pm t)
      (replace-match "To: \\2"))
    (while (re-search-forward "^\\(Betreff\\|Subject\\):[ \t]*\\(.*\\)$" pm t)
      (replace-match "Subject: \\2"))
    (goto-char (point-min))
    ;; insert mail header separator 
    (re-search-forward "^$" pm)
    (goto-char (match-end 0))
    (insert mail-header-separator "\n")
    ;; and put it back into the source folder
    (vm-postpone-message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting faces
;;;###autoload
(defun vm-install-rf-faces ()
  (make-face 'message-url)
  
  (custom-set-faces
   '(message-url
     ((t (:foreground "blue" :bold t))))
   '(message-headers
     ((t (:foreground "blue" :bold t))))
   '(message-cited-text
     ((t (:foreground "red3"))))
   '(message-header-contents
     ((((type x)) (:foreground "green3"))))
   '(message-highlighted-header-contents
     ((((type x)) (:bold t))
       (t (:bold t)))))
  
  (setq vm-highlight-url-face 'message-url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Well I like to have a different comment style a provided as default.
;; I'd like to have blank lines also prefixed by a comment char.
;; I overwrite the standard function by a slightly different version.
;;;###autoload
(defun vm-mail-mode-comment-region (beg end &optional arg)
  "Comment or uncomment each line in the region BEG to END.
With just a non-nil prefix ARG, uncomment each line in region.
Numeric prefix arg ARG means use ARG comment characters.
If ARG is negative, delete that many comment characters instead.
Comments are terminated on each line, even for syntax in which newline does
not end the comment.  Blank lines do not get comments."
  ;; if someone wants it to only put a comment-start at the beginning and
  ;; comment-end at the end then typing it, C-x C-x, closing it, C-x C-x
  ;; is easy enough.  No option is made here for other than commenting
  ;; every line.
  (interactive "r\nP")
  (or comment-start (error "No comment syntax is defined"))
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    (save-restriction
      (let ((cs comment-start) (ce comment-end)
            numarg)
        (if (consp arg) (setq numarg t)
          (setq numarg (prefix-numeric-value arg))
          ;; For positive arg > 1, replicate the comment delims now,
          ;; then insert the replicated strings just once.
          (while (> numarg 1)
            (setq cs (concat cs comment-start)
                  ce (concat ce comment-end))
            (setq numarg (1- numarg))))
        ;; Loop over all lines from BEG to END.
        (narrow-to-region beg end)
        (goto-char beg)
        (while (not (eobp))
          (if (or (eq numarg t) (< numarg 0))
              (progn
                ;; Delete comment start from beginning of line.
                (if (eq numarg t)
                    (while (looking-at (regexp-quote cs))
                      (delete-char (length cs)))
                  (let ((count numarg))
                    (while (and (> 1 (setq count (1+ count)))
                                (looking-at (regexp-quote cs)))
                      (delete-char (length cs)))))
                ;; Delete comment end from end of line.
                (if (string= "" ce)
                    nil
                  (if (eq numarg t)
                      (progn
                        (end-of-line)
                        ;; This is questionable if comment-end ends in
                        ;; whitespace.  That is pretty brain-damaged,
                        ;; though.
                        (skip-chars-backward " \t")
                        (if (and (>= (- (point) (point-min)) (length ce))
                                 (save-excursion
                                   (backward-char (length ce))
                                   (looking-at (regexp-quote ce))))
                            (delete-char (- (length ce)))))
                    (let ((count numarg))
                      (while (> 1 (setq count (1+ count)))
                        (end-of-line)
                        ;; This is questionable if comment-end ends in
                        ;; whitespace.  That is pretty brain-damaged though
                        (skip-chars-backward " \t")
                        (save-excursion
                          (backward-char (length ce))
                          (if (looking-at (regexp-quote ce))
                              (delete-char (length ce))))))))
                (forward-line 1))
            ;; Insert at beginning and at end.
            (progn
              (insert cs)
              (if (string= "" ce) ()
                (end-of-line)
                (insert ce)))
            (search-forward "\n" nil 'move)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sometimes it's handy to fake a date.
;; I overwrite the standard function by a slightly different version.
(defcustom vm-mail-mode-fake-date-p t
  "*Non-nil means `vm-mail-mode-insert-date-maybe' will not overwrite a existing date header."
  :group 'vm-rfaddons
  :type '(boolean))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vm-isearch-presentation ()
  "Switched to the presentation or preview buffer and starts isearch."
  (interactive)
  (vm-select-folder-buffer)
  (let ((target (or vm-presentation-buffer (current-buffer))))
    (if (get-buffer-window-list target)
        (select-window (car (get-buffer-window-list target)))
      (switch-to-buffer target)))
  (isearch-forward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-delete-message-action "vm-next-message"
  "Command to do after deleting a message."
  :group 'vm)

;;;###autoload
(defun vm-delete-message-action (&optional arg)
  "Delete current message and perform some action after it, e.g. move to next.
Call it with a prefix ARG to change the action."
  (interactive "P")
  (when (and (listp arg) (not (null arg)))
    (setq vm-delete-message-action
          (completing-read "After delete: "
                           '(("vm-rmail-up")
                             ("vm-rmail-down")
                             ("vm-previous-message")
                             ("vm-previous-unread-message")
                             ("vm-next-message")
                             ("vm-next-unread-message")
                             ("nothing"))))
    (message "action after delete is %S" vm-delete-message-action))
  (vm-toggle-deleted (prefix-numeric-value arg))
  (let ((fun (intern vm-delete-message-action)))
    (if (functionp fun)
        (call-interactively fun))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vm-smtp-server-online-p-cache nil
  "Alist of cached (server online-status) entries.")

(defun vm-smtp-server-online-p (&optional host port)
  "Opens SMTP connection to see if the server HOST on PORT is online.
Results are cached in `smtp-server-online-p-cache' for non interactive
calls."
  (interactive)
  (save-excursion 
    (let (online-p server hp)
      (if (null host)
          (setq server (if (functionp 'esmtpmail-via-smtp-server)
                           (esmtpmail-via-smtp-server)
                         (smtpmail-via-smtp-server))
                host   (car server)
                port   (cadr server)))
      (setq port (or port 25)
            hp (format "%s:%s" host port))

      (if (interactive-p)
          (setq vm-smtp-server-online-p-cache nil))
      
      (if (assoc hp vm-smtp-server-online-p-cache)
          ;; take cache content
          (setq online-p (cadr (assoc hp vm-smtp-server-online-p-cache))
                hp (concat hp " (cached)"))
        ;; do the check
        (let* ((n (format " *SMTP server check %s:%s *" host port))
               (buf (get-buffer n))
               (stream nil))
          (if buf (kill-buffer buf))
        
          (condition-case err
              (progn 
                (setq stream (open-network-stream n n host port))
                (setq online-p t))
            (error
             (message (cadr err))
             (if (and (get-buffer n)
                      (< 0 (length (save-excursion
				     (set-buffer (get-buffer n))
				     (buffer-substring (point-min) (point-max))))))
		 (pop-to-buffer n))))
	  (if stream (delete-process stream))
          (when (setq buf (get-buffer n))
            (set-buffer buf)
            (message "%S" (buffer-substring (point-min) (point-max)))
            (goto-char (point-min))
            (when (re-search-forward
                   "gethostbyname: Resource temporarily unavailable"
                   (point-max) t)
              (setq online-p nil))))
        
        ;; add to cache for further lookups 
        (add-to-list 'vm-smtp-server-online-p-cache (list hp online-p)))
    
      (if (interactive-p)
          (message "SMTP server %s is %s" hp
                   (if online-p "online" "offline")))
      online-p)))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vm-mail-send-or-feed-it ()
  "Sends a message if the SMTP server is online, queues it otherwise."
  (if (not (vm-smtp-server-online-p))
      (feedmail-send-it)
    (if (functionp 'esmtpmail-send-it)
        (esmtpmail-send-it)
      (smtpmail-send-it))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'vm-rfaddons)

;;; vm-rfaddons.el ends here
