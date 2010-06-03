;;; vm.el --- Entry points for VM
;;
;; Copyright (C) 1994-1998, 2003 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


;;; History:
;;
;; This file was vm-startup.el!

;;; Code:
(defvar enable-multibyte-characters)

(require 'vm-version)

;; Ensure that vm-autoloads is loaded in case the user is using VM 7.x
;; autoloads 

(eval-when (load)
  (if (not (featurep 'xemacs))
      (require 'vm-autoloads)))

;;;###autoload
(defun vm (&optional folder read-only access-method reload)
  "Read mail under Emacs.
Optional first arg FOLDER specifies the folder to visit.  It defaults
to the value of vm-primary-inbox.  The folder buffer is put into VM
mode, a major mode for reading mail.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, message additions or deletions will be allowed in the
visited folder.

Visiting the primary inbox normally causes any contents of the system
mailbox to be moved and appended to the resulting buffer.  You can
disable this automatic fetching of mail by setting
`vm-auto-get-new-mail' to nil. 

All the messages can be read by repeatedly pressing SPC.  Use `n'ext and
`p'revious to move about in the folder.  Messages are marked for
deletion with `d', and saved to another folder with `s'.  Quitting VM
with `q' saves the buffered folder to disk, but does not expunge
deleted messages.  Use `###' to expunge deleted messages.

See the documentation for vm-mode for more information."

  ;; Internally, this function may also be called with a buffer as the
  ;; FOLDER argument.  In that case, the function sets up the buffer
  ;; as a folder buffer and turns on vm-mode.

  ;; ACCESS-METHOD, if non-nil, indicates that the FOLDER is the
  ;; maildrop spec of a remote server folder.  Possible values for the
  ;; parameter are 'pop and 'imap.  Or, if FOLDER is a buffer instead
  ;; of a name, it will be set up as a folder buffer using the
  ;; specified ACCESS-METHOD.

  ;; RELOAD, if non-nil, means that the folder should be reloaded into
  ;; an existing buffer.  All initialisations must be performed but
  ;; some variables need to be preserved, e.g., vm-folder-access-data.

  ;; The functions find-name-for-spec and find-spec-for-name translate
  ;; between folder names and maildrop specs for the server folders.

  (interactive (list nil current-prefix-arg))
  (vm-session-initialization)
  ;; recursive call to vm in order to allow defadvice on its first call
  (unless (boundp 'vm-session-beginning)
    (vm folder read-only access-method reload))
  ;; set inhibit-local-variables non-nil to protect
  ;; against letter bombs.
  ;; set enable-local-variables to nil for newer Emacses
  (catch 'done
    ;; deduce the access method if none specified
    (if (null access-method)
	(let ((f (or folder vm-primary-inbox)))
	  (cond ((bufferp f)		; may be unnecessary. USR, 2010-01
		 (setq access-method vm-folder-access-method))
		((and (stringp f)
		      vm-recognize-imap-maildrops
		      (string-match vm-recognize-imap-maildrops f))
		 (setq access-method 'imap
		       folder f))
		((and (stringp f)
		      vm-recognize-pop-maildrops
		      (string-match vm-recognize-pop-maildrops f))
		 (setq access-method 'pop
		       folder f)))))
    (let ((full-startup (and (not reload) (not (bufferp folder))))
	  ;; not clear why full-startup isn't always true - USR, 2010-01-02
	  (did-read-index-file nil)
	  folder-buffer first-time totals-blurb
	  folder-name remote-spec
	  preserve-auto-save-file)
      (cond ((and full-startup (eq access-method 'pop))
	     (setq vm-last-visit-pop-folder folder)
	     (setq remote-spec (vm-pop-find-spec-for-name folder))
	     (if (null remote-spec)
		 (error "No such POP folder: %s" folder))
	     (setq folder-name folder)
	     (setq folder (vm-pop-find-cache-file-for-spec remote-spec)))
	    ((and full-startup (eq access-method 'imap))
	     (setq vm-last-visit-imap-folder folder)
	     (setq remote-spec folder
		   folder-name (or (nth 3 (vm-imap-parse-spec-to-list
					   remote-spec))
				   folder)
		   folder (vm-imap-make-filename-for-spec remote-spec))))
      (setq folder-buffer
	    (if (bufferp folder)
		folder
	      (vm-read-folder folder remote-spec)))
      (set-buffer folder-buffer)
      ;; notice the message summary file of Thunderbird 
      (let ((msf (concat (buffer-file-name) ".msf")))
        (setq vm-sync-thunderbird-status
              (or (file-exists-p msf)
                  ;TODO (re-search-forward "^X-Mozilla-Status2?:"
                  ;                   (point-max) t)
                  )))
      (when (and (stringp folder) (memq access-method '(pop imap)))
	     (if (not (equal folder-name (buffer-name)))
		 (rename-buffer folder-name t)))
      (if (and vm-fsfemacs-mule-p enable-multibyte-characters)
	  (set-buffer-multibyte nil))
      ;; for MULE
      ;;
      ;; If the file coding system is not a no-conversion variant,
      ;; make it so by encoding all the text, then setting the
      ;; file coding system and decoding it.  This situation is
      ;; only possible if a file is visited and then vm-mode is
      ;; run on it afterwards.
      ;;
      ;; There are separate code blocks for FSF Emacs and XEmacs
      ;; because the coding systems have different names.
      (defvar buffer-file-coding-system)
      (if (and (or vm-xemacs-mule-p vm-xemacs-file-coding-p)
	       (not (eq (get-coding-system buffer-file-coding-system)
			(get-coding-system 'no-conversion-unix)))
	       (not (eq (get-coding-system buffer-file-coding-system)
			(get-coding-system 'no-conversion-dos)))
	       (not (eq (get-coding-system buffer-file-coding-system)
			(get-coding-system 'no-conversion-mac)))
	       (not (eq (get-coding-system buffer-file-coding-system)
			(get-coding-system 'binary))))
	  (let ((buffer-read-only nil)
		(omodified (buffer-modified-p)))
	    (unwind-protect
		(progn
		  (encode-coding-region (point-min) (point-max)
					buffer-file-coding-system)
		  (set-buffer-file-coding-system 'no-conversion nil)
		  (decode-coding-region (point-min) (point-max)
					buffer-file-coding-system))
	      (set-buffer-modified-p omodified))))
      (if (and vm-fsfemacs-mule-p (null buffer-file-coding-system))
	  (set-buffer-file-coding-system 'raw-text nil))
      (if (and vm-fsfemacs-mule-p
	       (not (eq (coding-system-base buffer-file-coding-system)
			(coding-system-base 'raw-text-unix)))
	       (not (eq (coding-system-base buffer-file-coding-system)
			(coding-system-base 'raw-text-mac)))
	       (not (eq (coding-system-base buffer-file-coding-system)
			(coding-system-base 'raw-text-dos)))
	       (not (eq (coding-system-base buffer-file-coding-system)
			(coding-system-base 'no-conversion))))
	  (let ((buffer-read-only nil)
		(omodified (buffer-modified-p)))
	    (unwind-protect
		(progn
		  (encode-coding-region (point-min) (point-max)
					buffer-file-coding-system)
		  (set-buffer-file-coding-system 'raw-text nil)
		  (decode-coding-region (point-min) (point-max)
					buffer-file-coding-system))
	      (set-buffer-modified-p omodified))))
      (vm-check-for-killed-summary)
      (vm-check-for-killed-presentation)
      ;; If the buffer's not modified then we know that there can be no
      ;; messages in the folder that are not on disk.
      (or (buffer-modified-p) (setq vm-messages-not-on-disk 0))
      (setq first-time (not (eq major-mode 'vm-mode))
	    preserve-auto-save-file (and buffer-file-name
					  (not (buffer-modified-p))
					  (file-newer-than-file-p
					   (make-auto-save-file-name)
					   buffer-file-name)))
      (setq vm-folder-read-only (or preserve-auto-save-file read-only
				    (default-value 'vm-folder-read-only)
				    (and first-time buffer-read-only)))
      ;; If this is not a VM mode buffer then some initialization
      ;; needs to be done
      (if first-time
	  (progn
	    (buffer-disable-undo (current-buffer))
	    (abbrev-mode 0)
	    (auto-fill-mode 0)
	    ;; If an 8-bit message arrives undeclared the 8-bit
	    ;; characters in it should be displayed using the
	    ;; user's default face charset, rather than as octal
	    ;; escapes.
	    (vm-fsfemacs-nonmule-display-8bit-chars)
	    (vm-mode-internal access-method reload)
	    (if full-startup
		(cond ((eq access-method 'pop)
		       (vm-set-folder-pop-maildrop-spec remote-spec))
		      ((eq access-method 'imap)
		       (vm-set-folder-imap-maildrop-spec remote-spec))))
	    ;; If the buffer is modified we don't know if the
	    ;; folder format has been changed to be different
	    ;; from index file, so don't read the index file in
	    ;; that case.
	    (if (not (buffer-modified-p))
		(setq did-read-index-file (vm-read-index-file-maybe)))))

      ;; builds message list, reads attributes if they weren't
      ;; read from an index file.
      (vm-assimilate-new-messages nil (not did-read-index-file) nil t)

      (if (and first-time (not did-read-index-file))
	  (progn
	    (vm-gobble-visible-header-variables)
	    (vm-gobble-bookmark)
	    (vm-gobble-pop-retrieved)
	    (vm-gobble-imap-retrieved)
	    (vm-gobble-summary)
	    (vm-gobble-labels)))

      (if first-time
	  (vm-start-itimers-if-needed))

      ;; make a new frame if the user wants one.  reuse an
      ;; existing frame that is showing this folder.
      (if (and full-startup
	       ;; this so that "emacs -f vm" doesn't create a frame.
	       this-command)
	  (apply 'vm-goto-new-folder-frame-maybe
		 (if folder '(folder) '(primary-folder folder))))

      ;; raise frame if requested and apply startup window
      ;; configuration.
      (if full-startup
	  (let ((buffer-to-display (or vm-summary-buffer
				       vm-presentation-buffer
				       (current-buffer))))
	    (vm-display buffer-to-display buffer-to-display
			(list this-command)
			(list (or this-command 'vm) 'startup))
	    (if vm-raise-frame-at-startup
		(vm-raise-frame))))

      ;; say this NOW, before the non-previewers read a message,
      ;; alter the new message count and confuse themselves.
      (if full-startup
	  (progn
	    ;; save blurb so we can repeat it later as necessary.
	    (setq totals-blurb (vm-emit-totals-blurb))
	    (and buffer-file-name
		 (vm-store-folder-totals buffer-file-name (cdr vm-totals)))))

      (vm-thoughtfully-select-message)
      (vm-update-summary-and-mode-line)
      ;; need to do this after any frame creation because the
      ;; toolbar sets frame-specific height and width specifiers.
      (vm-toolbar-install-or-uninstall-toolbar)

      (and vm-use-menus (vm-menu-support-possible-p)
	   (vm-menu-install-visited-folders-menu))

      (if full-startup
	  (progn
	    (if (and (vm-should-generate-summary)
		     ;; don't generate a summary if recover-file is
		     ;; likely to happen, since recover-file does
		     ;; not work in a summary buffer.
		     (not preserve-auto-save-file))
		(vm-summarize t nil))
	    ;; raise the summary frame if the user wants frames
	    ;; raised and if there is a summary frame.
	    (if (and vm-summary-buffer
		     vm-mutable-frames
		     vm-frame-per-summary
		     vm-raise-frame-at-startup)
		(vm-raise-frame))
	    ;; if vm-mutable-windows is nil, the startup
	    ;; configuration can't be applied, so do
	    ;; something to get a VM buffer on the screen
	    (if vm-mutable-windows
		(vm-display nil nil (list this-command)
			    (list (or this-command 'vm) 'startup))
	      (save-excursion
		(switch-to-buffer (or vm-summary-buffer
				      vm-presentation-buffer
				      (current-buffer)))))))

      (if vm-message-list
	  ;; don't decode MIME if recover-file is
	  ;; likely to happen, since recover-file does
	  ;; not work in a presentation buffer.
	  (let ((vm-auto-decode-mime-messages
		 (and vm-auto-decode-mime-messages
		      (not preserve-auto-save-file))))
	    (vm-preview-current-message)))

      (run-hooks 'vm-visit-folder-hook)

      ;; Warn user about auto save file, if appropriate.
      (if preserve-auto-save-file
	  (message
	   (substitute-command-keys
	    (concat
	     "Auto save file is newer; consider \\[vm-recover-folder].  "
	     "FOLDER IS READ ONLY."))))
      ;; if we're not doing a full startup or if doing more would
      ;; trash the auto save file that we need to preserve,
      ;; stop here.
      (if (or (not full-startup) preserve-auto-save-file)
	  (throw 'done t))
      
      (message totals-blurb)

      (if (and vm-auto-get-new-mail
	       (not vm-block-new-mail)
	       (not vm-folder-read-only))
	  (progn
	    (message "Checking for new mail for %s..."
		     (or buffer-file-name (buffer-name)))
	    (if (vm-get-spooled-mail t)
		(progn
		  (setq totals-blurb (vm-emit-totals-blurb))
		  (if (vm-thoughtfully-select-message)
		      (vm-preview-current-message)
		    (vm-update-summary-and-mode-line))))
	    (message totals-blurb)))

      ;; Display copyright and copying info.
      (if (and (interactive-p) (not vm-startup-message-displayed))
	  (progn
	    (vm-display-startup-message)
	    (if (not (input-pending-p))
		(message totals-blurb)))))))

;;;###autoload
(defun vm-other-frame (&optional folder read-only)
  "Like vm, but run in a newly created frame."
  (interactive (list nil current-prefix-arg))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (if folder
	  (vm-goto-new-frame 'folder)
	(vm-goto-new-frame 'primary-folder 'folder)))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm folder read-only))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-other-window (&optional folder read-only)
  "Like vm, but run in a different window."
  (interactive (list nil current-prefix-arg))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm folder read-only)))

(put 'vm-mode 'mode-class 'special)

;;;###autoload
(defun vm-mode (&optional read-only)
  "Major mode for reading mail.

This is VM.

Use M-x vm-submit-bug-report to submit a bug report.

Commands:
\\{vm-mode-map}

Customize VM by setting variables and store them in the `vm-init-file'."
  (interactive "P")
  (vm (current-buffer) read-only)
  (vm-display nil nil '(vm-mode) '(vm-mode)))

;;;###autoload
(defun vm-visit-folder (folder &optional read-only)
  "Visit a mail file.
VM will parse and present its messages to you in the usual way.

First arg FOLDER specifies the mail file to visit.  When this
command is called interactively the file name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-file-name
	      (format "Visit%s folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil nil 'vm-folder-history)
	     current-prefix-arg))))
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (setq vm-last-visit-folder folder)
  (let ((access-method nil) foo)
    (cond ((and (stringp vm-recognize-pop-maildrops)
		(string-match vm-recognize-pop-maildrops folder)
		(setq foo (vm-pop-find-name-for-spec folder)))
	   (setq folder foo
		 access-method 'pop
		 vm-last-visit-pop-folder folder))
	  ((and (stringp vm-recognize-imap-maildrops)
		(string-match vm-recognize-imap-maildrops folder)
		;;(setq foo (vm-imap-find-name-for-spec folder))
		)
	   (setq ;; folder foo
	         access-method 'imap
		 vm-last-visit-imap-folder folder))
	  (t
	   (let ((default-directory 
		   (or vm-folder-directory default-directory)))
	     (setq folder (expand-file-name folder)
		   vm-last-visit-folder folder))))
    (vm folder read-only access-method)))

;;;###autoload
(defun vm-visit-folder-other-frame (folder &optional read-only)
  "Like vm-visit-folder, but run in a newly created frame."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-file-name
	      (format "Visit%s folder in other frame:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil nil 'vm-folder-history)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-folder folder read-only))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-visit-folder-other-window (folder &optional read-only)
  "Like vm-visit-folder, but run in a different window."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-file-name
	      (format "Visit%s folder in other window:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil nil 'vm-folder-history)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-folder folder read-only)))

;;;###autoload
(defun vm-visit-pop-folder (folder &optional read-only)
  "Visit a POP mailbox.
VM will present its messages to you in the usual way.  Messages
found in the POP mailbox will be downloaded and stored in a local
cache.  If you expunge messages from the cache, the corresponding
messages will be expunged from the POP mailbox.

First arg FOLDER specifies the name of the POP mailbox to visit.
You can only visit mailboxes that are specified in `vm-pop-folder-alist'.
When this command is called interactively the mailbox name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-pop)
     (let ((completion-list (mapcar (function (lambda (x) (nth 1 x)))
				    vm-pop-folder-alist))
	   (default vm-last-visit-pop-folder)
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-string
	      (format "Visit%s POP folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      completion-list)
	     current-prefix-arg))))
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (if (and (equal folder "") (stringp vm-last-visit-pop-folder))
      (setq folder vm-last-visit-pop-folder))
  (if (null (vm-pop-find-spec-for-name folder))
      (error "No such POP folder: %s" folder))
  (setq vm-last-visit-pop-folder folder)
  (vm folder read-only 'pop))

;;;###autoload
(defun vm-visit-pop-folder-other-frame (folder &optional read-only)
  "Like vm-visit-pop-folder, but run in a newly created frame."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-pop)
     (let ((completion-list (mapcar (function (lambda (x) (nth 1 x)))
				    vm-pop-folder-alist))
	   (default vm-last-visit-pop-folder)
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-string
	      (format "Visit%s POP folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      completion-list)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-pop-folder folder read-only))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-visit-pop-folder-other-window (folder &optional read-only)
  "Like vm-visit-pop-folder, but run in a different window."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-pop)
     (let ((completion-list (mapcar (function (lambda (x) (nth 1 x)))
				    vm-pop-folder-alist))
	   (default vm-last-visit-pop-folder)
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-string
	      (format "Visit%s POP folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      completion-list)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-pop-folder folder read-only)))

;;;###autoload
(defun vm-visit-imap-folder (folder &optional read-only)
  "Visit a IMAP mailbox.
VM will present its messages to you in the usual way.  Messages
found in the IMAP mailbox will be downloaded and stored in a local
cache.  If you expunge messages from the cache, the corresponding
messages will be expunged from the IMAP mailbox when the folder is
saved. 

When this command is called interactively, the FOLDER name will
be read from the minibuffer in the format
\"account-name:folder-name\", where account-name is the short
name of an IMAP account listed in `vm-imap-account-alist' and
folder-name is a folder in this account.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-imap)
     (let ((this-command this-command)
	   (last-command last-command))
       (if (null vm-imap-account-alist)
	   (setq vm-imap-account-alist 
		 (mapcar 
		  'reverse
		  (vm-imap-spec-list-to-host-alist vm-imap-server-list))))
       (list (vm-read-imap-folder-name
	      (format "Visit%s IMAP folder: "
		      (if current-prefix-arg " read only" ""))
	      t nil vm-last-visit-imap-folder)
	     current-prefix-arg))))
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (setq vm-last-visit-imap-folder folder)
  (vm folder read-only 'imap))

;;;###autoload
(defun vm-visit-imap-folder-other-frame (folder &optional read-only)
  "Like vm-visit-imap-folder, but run in a newly created frame."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-imap)
     (let ((this-command this-command)
	   (last-command last-command))
       (list (vm-read-imap-folder-name
	      (format "Visit%s IMAP folder: "
		      (if current-prefix-arg " read only" ""))
	      nil nil vm-last-visit-imap-folder)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-imap-folder folder read-only))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-visit-imap-folder-other-window (folder &optional read-only)
  "Like vm-visit-imap-folder, but run in a different window."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (require 'vm-imap)
     (let ((this-command this-command)
	   (last-command last-command))
       (list (vm-read-imap-folder-name
	      (format "Visit%s IMAP folder: "
		      (if current-prefix-arg " read only" ""))
	      nil nil vm-last-visit-imap-folder)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-imap-folder folder read-only)))

(put 'vm-virtual-mode 'mode-class 'special)

(defun vm-virtual-mode (&rest ignored)
  "Mode for reading multiple mail folders as one folder.

The commands available are the same commands that are found in
vm-mode, except that a few of them are not applicable to virtual
folders.

vm-virtual-mode is not a normal major mode.  If you run it, it
will not do anything.  The entry point to vm-virtual-mode is
vm-visit-virtual-folder.")

(defvar scroll-in-place)

;;;###autoload
(defun vm-visit-virtual-folder (folder-name &optional read-only bookmark)
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (vm-read-string (format "Visit%s virtual folder: "
			      (if current-prefix-arg " read only" ""))
		      vm-virtual-folder-alist)
      current-prefix-arg)))
  (vm-session-initialization)
  (require 'vm-virtual)
  (if (not (assoc folder-name vm-virtual-folder-alist))
      (error "No such virtual folder, %s" folder-name))
  (let ((buffer-name (concat "(" folder-name ")"))
	first-time blurb)
    (set-buffer (get-buffer-create buffer-name))
    (setq first-time (not (eq major-mode 'vm-virtual-mode)))
    (if first-time
	(progn
	  (if (fboundp 'buffer-disable-undo)
	      (buffer-disable-undo (current-buffer))
	    ;; obfuscation to make the v19 compiler not whine
	    ;; about obsolete functions.
	    (let ((x 'buffer-flush-undo))
	      (funcall x (current-buffer))))
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (vm-fsfemacs-nonmule-display-8bit-chars)
	  (setq mode-name "VM Virtual"
		mode-line-format vm-mode-line-format
		buffer-read-only t
		vm-folder-read-only read-only
		vm-label-obarray (make-vector 29 0)
		vm-virtual-folder-definition
		  (assoc folder-name vm-virtual-folder-alist))
	  ;; scroll in place messes with scroll-up and this loses
	  (make-local-variable 'scroll-in-place)
	  (setq scroll-in-place nil)
	  (vm-build-virtual-message-list nil)
	  (use-local-map vm-mode-map)
	  (and (vm-menu-support-possible-p)
	       (vm-menu-install-menus))
	  (add-hook 'kill-buffer-hook 'vm-garbage-collect-folder)
	  (add-hook 'kill-buffer-hook 'vm-garbage-collect-message)
	  ;; save this for last in case the user interrupts.
	  ;; an interrupt anywhere before this point will cause
	  ;; everything to be redone next revisit.
	  (setq major-mode 'vm-virtual-mode)
	  (run-hooks 'vm-virtual-mode-hook)
	  ;; must come after the setting of major-mode
	  (setq mode-popup-menu (and vm-use-menus
				     (vm-menu-support-possible-p)
				     (vm-menu-mode-menu)))
	  (setq blurb (vm-emit-totals-blurb))
	  (if vm-summary-show-threads
	      (vm-sort-messages "thread"))
	  (if bookmark
	      (let ((mp vm-message-list))
		(while mp
		  (if (eq bookmark (vm-real-message-of (car mp)))
		      (progn
			(vm-record-and-change-message-pointer
			 vm-message-pointer mp)
			(vm-preview-current-message)
			(setq mp nil))
		    (setq mp (cdr mp))))))
	  (if (null vm-message-pointer)
	      (if (vm-thoughtfully-select-message)
		  (vm-preview-current-message)
		(vm-update-summary-and-mode-line)))
	  (message blurb)))
    ;; make a new frame if the user wants one.  reuse an
    ;; existing frame that is showing this folder.
    (vm-goto-new-folder-frame-maybe 'folder)
    (if vm-raise-frame-at-startup
	(vm-raise-frame))
    (vm-display nil nil (list this-command) (list this-command 'startup))
    (vm-toolbar-install-or-uninstall-toolbar)
    (if first-time
	(progn
	  (if (vm-should-generate-summary)
	      (progn (vm-summarize t nil)
		     (message blurb)))
	  ;; raise the summary frame if the user wants frames
	  ;; raised and if there is a summary frame.
	  (if (and vm-summary-buffer
		   vm-mutable-frames
		   vm-frame-per-summary
		   vm-raise-frame-at-startup)
	      (vm-raise-frame))
	  ;; if vm-mutable-windows is nil, the startup
	  ;; configuration can't be applied, so do
	  ;; something to get a VM buffer on the screen
	  (if vm-mutable-windows
	      (vm-display nil nil (list this-command)
			  (list (or this-command 'vm) 'startup))
	    (save-excursion
	      (switch-to-buffer (or vm-summary-buffer
				    vm-presentation-buffer
				    (current-buffer)))))))

    ;; check interactive-p so as not to bog the user down if they
    ;; run this function from within another function.
    (and (interactive-p)
	 (not vm-startup-message-displayed)
	 (vm-display-startup-message)
	 (message blurb))))

;;;###autoload
(defun vm-visit-virtual-folder-other-frame (folder-name &optional read-only)
  "Like vm-visit-virtual-folder, but run in a newly created frame."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (vm-read-string (format "Visit%s virtual folder in other frame: "
			      (if current-prefix-arg " read only" ""))
		      vm-virtual-folder-alist)
      current-prefix-arg)))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-virtual-folder folder-name read-only))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-visit-virtual-folder-other-window (folder-name &optional read-only)
  "Like vm-visit-virtual-folder, but run in a different window."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (vm-read-string (format "Visit%s virtual folder in other window: "
			      (if current-prefix-arg " read only" ""))
		      vm-virtual-folder-alist)
      current-prefix-arg)))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-virtual-folder folder-name read-only)))

;;;###autoload
(defun vm-mail (&optional to subject)
  "Send a mail message from within VM, or from without.
Optional argument TO is a string that should contain a comma separated
recipient list."
  (interactive)
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (vm-mail-internal nil to subject)
  (run-hooks 'vm-mail-hook)
  (run-hooks 'vm-mail-mode-hook))

;;;###autoload
(defun vm-mail-other-frame (&optional to)
  "Like vm-mail, but run in a newly created frame.
Optional argument TO is a string that should contain a comma separated
recipient list."
  (interactive)
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-mail to))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-mail-other-window (&optional to)
  "Like vm-mail, but run in a different window.
Optional argument TO is a string that should contain a comma separated
recipient list."
  (interactive)
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-mail to)))

(fset 'vm-folders-summary-mode 'vm-mode)
(put 'vm-folders-summary-mode 'mode-class 'special)


;;;###autoload
(defun vm-folders-summarize (&optional display raise)
  "Generate a summary of the folders in your folder directories.
Set `vm-folders-summary-directories' to specify the folder directories.
Press RETURN or click mouse button 2 on an entry in the folders
summary buffer to select a folder."
  (interactive "p\np")
  (vm-session-initialization)
  (vm-check-for-killed-summary)
  (if (not (featurep 'berkeley-db))
      (error "Berkeley DB support needed to run this command"))
  (if (null vm-folders-summary-database)
      (error "'vm-folders-summary-database' must be non-nil to run this command"))
  (if (null vm-folders-summary-buffer)
      (let ((folder-buffer (and (eq major-mode 'vm-mode)
				(current-buffer))))
	(setq vm-folders-summary-buffer
	      (let ((default-enable-multibyte-characters t))
		(get-buffer-create "VM Folders Summary")))
	(save-excursion
	  (set-buffer vm-folders-summary-buffer)
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (vm-fsfemacs-nonmule-display-8bit-chars)
	  (if (fboundp 'buffer-disable-undo)
	      (buffer-disable-undo (current-buffer))
	    ;; obfuscation to make the v19 compiler not whine
	    ;; about obsolete functions.
	    (let ((x 'buffer-flush-undo))
	      (funcall x (current-buffer))))
	  (vm-folders-summary-mode-internal))
	(vm-make-folders-summary-associative-hashes)
	(vm-do-folders-summary)))
  ;; if this command was run from a VM related buffer, select
  ;; the folder buffer in the folders summary, but only if that
  ;; folder has an entry there.
  (and vm-mail-buffer
       (vm-check-for-killed-folder))
  (save-excursion
    (and vm-mail-buffer
	 (vm-select-folder-buffer))
    (vm-check-for-killed-summary)
    (let ((folder-buffer (and (eq major-mode 'vm-mode)
			      (current-buffer)))
	  fs )
      (if (or (null vm-folders-summary-hash) (null folder-buffer)
	      (null buffer-file-name))
	  nil
	(setq fs (symbol-value (intern-soft (vm-make-folders-summary-key
					     buffer-file-name)
					    vm-folders-summary-hash)))
	(if (null fs)
	    nil
	  (vm-mark-for-folders-summary-update buffer-file-name)
	  (set-buffer vm-folders-summary-buffer)
	  (setq vm-mail-buffer folder-buffer)))))
  (if display
      (save-excursion
	(vm-goto-new-folders-summary-frame-maybe)
	(vm-display vm-folders-summary-buffer t
		    '(vm-folders-summarize)
		    (list this-command) (not raise))
	;; need to do this after any frame creation because the
	;; toolbar sets frame-specific height and width specifiers.
	(set-buffer vm-folders-summary-buffer)
	(vm-toolbar-install-or-uninstall-toolbar))
    (vm-display nil nil '(vm-folders-summarize)
		(list this-command)))
  (vm-update-summary-and-mode-line))

(defvar mail-send-actions)

;;;###autoload
(defun vm-compose-mail (&optional to subject other-headers continue
		        switch-function yank-action
			send-actions)
  (interactive)
  (vm-session-initialization)
  (if continue
      (vm-continue-composing-message)
    (let ((buffer (vm-mail-internal
		   (if to
		       (format "message to %s"
			       (vm-truncate-roman-string to 20))
		     nil)
		   to subject)))
      (goto-char (point-min))
      (re-search-forward (concat "^" mail-header-separator "$"))
      (beginning-of-line)
      (while other-headers
	(insert (car (car other-headers)))
	(while (eq (char-syntax (char-before (point))) ?\ )
	  (delete-char -1))
	(while (eq (char-before (point)) ?:)
	  (delete-char -1))
	(insert ": " (cdr (car other-headers)))
	(if (not (eq (char-before (point)) ?\n))
	    (insert "\n"))
	(setq other-headers (cdr other-headers)))
      (cond ((null to)
	     (mail-position-on-field "To"))
	    ((null subject)
	     (mail-position-on-field "Subject"))
	    (t
	     (mail-text)))
      (funcall (or switch-function (function switch-to-buffer))
	       (current-buffer))
      (if yank-action
	  (save-excursion
	    (mail-text)
	    (apply (car yank-action) (cdr yank-action))
	    (push-mark (point))
	    (mail-text)
	    (cond (mail-citation-hook (run-hooks 'mail-citation-hook))
		  (mail-yank-hooks (run-hooks 'mail-yank-hooks))
		  (t (vm-mail-yank-default)))))
      (make-local-variable 'mail-send-actions)
      (setq mail-send-actions send-actions))))

;;;###autoload
(defun vm-submit-bug-report (&optional pre-hooks post-hooks)
  "Submit a bug report, with pertinent information to the VM bug list."
  (interactive)
  (require 'reporter)
  (vm-session-initialization)
  ;; Use VM to send the bug report.  Could be trouble if vm-mail
  ;; is what the user wants to complain about.  But most of the
  ;; time we'll be fine and users like to use MIME to attach
  ;; stuff to the reports.
  (let ((reporter-mailer '(vm-mail))
	(mail-user-agent 'vm-user-agent)
        varlist)
    (setq varlist (apropos-internal "^\\(vm\\|vmpc\\)-" 'user-variable-p)
          varlist (sort varlist
                        (lambda (v1 v2)
                          (string-lessp (format "%s" v1) (format "%s" v2)))))
    (let ((vars-to-delete 
	   '(vm-shrunken-headers-keymap	; big and wasteful
	     vm-auto-folder-alist	; a bit private
	     vm-mail-folder-alist	; ditto
	     ;; vm-mail-fcc-default - is this private?
	     vmpc-actions vmpc-conditions 
	     vmpc-actions-alist vmpc-reply-alist vmpc-forward-alist
	     vmpc-resend-alist vmpc-newmail-alist vmpc-automorph-alist
	     ))
	  ;; delete any passwords stored in maildrop strings
	  (vm-spool-files 
	   (if (listp (car vm-spool-files))
	       (vm-mapcar 
		(lambda (elem-xyz)
		  (vm-mapcar (function vm-maildrop-sans-password)
			     elem-xyz)))
	     (vm-mapcar (function vm-maildrop-sans-password)
			vm-spool-files)))
	  (vm-pop-folder-alist 
	   (vm-maildrop-alist-sans-password vm-pop-folder-alist))
	  (vm-imap-server-list 
	   (vm-mapcar (function vm-maildrop-sans-password) 
		      vm-imap-server-list))
	  (vm-imap-account-alist 
	   (vm-maildrop-alist-sans-password vm-imap-account-alist))
	  (vm-pop-auto-expunge-alist
	   (vm-maildrop-alist-sans-password vm-pop-auto-expunge-alist))
	  (vm-imap-auto-expunge-alist
	   (vm-maildrop-alist-sans-password vm-imap-auto-expunge-alist)))
      (while vars-to-delete
        (setq varlist (delete (car vars-to-delete) varlist)
              vars-to-delete (cdr vars-to-delete)))
      ;; see what the user had loaded
      (setq varlist (append (list 'features) varlist))
      (delete-other-windows)
      (reporter-submit-bug-report
       vm-maintainer-address
       (concat "VM " (vm-version))
       varlist
       nil
       nil
       "INSTRUCTIONS:
- Please change the Subject header to a concise bug description.

- In this report, remember to cover the basics, that is, what you
  expected to happen and what in fact did happen and how to reproduce it.

- You may attach sample messages or attachments that can be used to
  reproduce the problem.  (They will be kept confidential.)

- Please remove these instructions and other stuff which is unrelated
  to the bug from your message.
")
      (goto-char (point-min))
      (mail-position-on-field "Subject")
      (insert "VM-BUG: "))))

(defun vm-edit-init-file ()
  "Edit the `vm-init-file'."
  (interactive)
  (find-file-other-frame vm-init-file))

(defun vm-load-init-file (&optional interactive)
  (interactive "p")
  (if (or (not vm-init-file-loaded) interactive)
      (progn
	(and vm-init-file
	     (load vm-init-file (not interactive) (not interactive) t))
	(and vm-preferences-file (load vm-preferences-file t t t))))
  (setq vm-init-file-loaded t)
  (vm-display nil nil '(vm-load-init-file) '(vm-load-init-file)))

(defun vm-check-emacs-version ()
  (cond ((and vm-xemacs-p (< emacs-major-version 21))
	 (error "VM %s must be run on XEmacs 21 or a later version."
		(vm-version)))
	((and vm-fsfemacs-p (< emacs-major-version 21))
	 (error "VM %s must be run on GNU Emacs 21 or a later version."
		(vm-version)))))

(defun vm-set-debug-flags ()
  (or stack-trace-on-error
      debug-on-error
      (setq stack-trace-on-error
	    '(
	      wrong-type-argument
	      wrong-number-of-arguments
	      args-out-of-range
	      void-function
	      void-variable
	      invalid-function
	     ))))

(defvar vm-postponed-folder)

(defvar vm-drafts-exist nil)

(defvar vm-ml-draft-count ""
  "The current number of drafts in the `vm-postponed-folder'.")

(defun vm-update-draft-count ()
  "Check number of postponed messages in folder `vm-postponed-folder'."
  (let ((f (expand-file-name vm-postponed-folder vm-folder-directory)))
    (if (or (not (file-exists-p f)) (= (nth 7 (file-attributes f)) 0))
        (setq vm-drafts-exist nil)
      (let ((mtime (nth 5 (file-attributes f))))
        (when (not (equal vm-drafts-exist mtime))
          (setq vm-drafts-exist mtime)
          (setq vm-ml-draft-count (format "%d postponed"
                                          (vm-count-messages-in-file f))))))))

(defun vm-session-initialization ()
  ;;  (vm-set-debug-flags)
  ;; If this is the first time VM has been run in this Emacs session,
  ;; do some necessary preparations.
  (if (or (not (boundp 'vm-session-beginning))
	  vm-session-beginning)
      (progn
        (vm-check-emacs-version)
        (require 'vm-vars)
        (require 'vm-macro)
        (require 'vm-misc)
        (require 'vm-message)
        (require 'vm-minibuf)
        (require 'vm-motion)
        (require 'vm-page)
        (require 'vm-mouse)
        (require 'vm-summary)
        (require 'vm-undo)
        (require 'vm-mime)
        (require 'vm-folder)
        (require 'vm-toolbar)
        (require 'vm-window)
        (require 'vm-menu)
        (require 'vm-rfaddons)
	;; The default loading of vm-pgg is disabled because it is an
	;; add-on.  If and when it is integrated into VM, without advices
	;; and other add-on features, then it can be loaded by
	;; default.  USR, 2010-01-14
        ;; (if (locate-library "pgg")
        ;;     (require 'vm-pgg)
        ;;   (message "vm-pgg disabled since pgg is missing!"))
        (add-hook 'kill-emacs-hook 'vm-garbage-collect-global)
	(vm-load-init-file)
	(when vm-enable-addons
	  (vm-rfaddons-infect-vm 0 vm-enable-addons)
	  (when (or (eq t vm-enable-addons)
                    (member 'summary-faces vm-enable-addons))
	    (require 'vm-summary-faces)
	    (vm-summary-faces-mode 1)))
	(if (not vm-window-configuration-file)
	    (setq vm-window-configurations vm-default-window-configuration)
	  (or (vm-load-window-configurations vm-window-configuration-file)
	      (setq vm-window-configurations vm-default-window-configuration)))
	(setq vm-buffers-needing-display-update (make-vector 29 0))
	(setq vm-buffers-needing-undo-boundaries (make-vector 29 0))
	(add-hook 'post-command-hook 'vm-add-undo-boundaries)
	(if (if vm-xemacs-p
		(find-face 'vm-monochrome-image)
	      (facep 'vm-monochrome-image))
	    nil
	  (make-face 'vm-monochrome-image)
	  (set-face-background 'vm-monochrome-image "white")
	  (set-face-foreground 'vm-monochrome-image "black"))
	(if (or (not vm-fsfemacs-p)
		;; don't need this face under Emacs 21.
		(fboundp 'image-type-available-p)
		(facep 'vm-image-placeholder))
	    nil
	  (make-face 'vm-image-placeholder)
	  (if (fboundp 'set-face-stipple)
	      (set-face-stipple 'vm-image-placeholder
				(list 16 16
				      (concat "UU\377\377UU\377\377UU\377\377"
					      "UU\377\377UU\377\377UU\377\377"
					      "UU\377\377UU\377\377")))))
	;; default value of vm-mime-button-face is 'gui-button-face
	;; this face doesn't exist by default in FSF Emacs 19.34.
	;; Create it and initialize it to something reasonable.
	(if (and vm-fsfemacs-p (featurep 'faces)
		 (not (facep 'gui-button-face)))
	    (progn
	      (make-face 'gui-button-face)
	      (cond ((eq window-system 'x)
		     (set-face-foreground 'gui-button-face "black")
		     (set-face-background 'gui-button-face "gray75"))
		    (t
		     ;; use primary color names, since fancier
		     ;; names may not be valid.
		     (set-face-foreground 'gui-button-face "white")
		     (set-face-background 'gui-button-face "red")))))
	;; gui-button-face might not exist under XEmacs either.
	;; This can happen if XEmacs is built without window
	;; system support.  In any case, create it anyway.
	(if (and vm-xemacs-p (not (find-face 'gui-button-face)))
	    (progn
	      (make-face 'gui-button-face)
	      (set-face-foreground 'gui-button-face "black" nil '(win))
	      (set-face-background 'gui-button-face "gray75" nil '(win))
	      (set-face-foreground 'gui-button-face "white" nil '(tty))
	      (set-face-background 'gui-button-face "red" nil '(tty))))
	(and (vm-mouse-support-possible-p)
	     (vm-mouse-install-mouse))
	(and (vm-menu-support-possible-p)
	     vm-use-menus
	     (vm-menu-fsfemacs-menus-p)
	     (vm-menu-initialize-vm-mode-menu-map))
	(setq vm-session-beginning nil)))
  ;; check for postponed messages
  (vm-update-draft-count))

;;;###autoload
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent 'vm-user-agent
      (function vm-compose-mail)	; compose function
      (function vm-mail-send-and-exit)	; send function
      nil				; abort function (kill-buffer)
      nil)				; hook variable (mail-send-hook)
)

(autoload 'reporter-submit-bug-report "reporter")
(autoload 'timezone-make-date-sortable "timezone")
(autoload 'rfc822-addresses "rfc822")
(autoload 'mail-strip-quoted-names "mail-utils")
(autoload 'mail-fetch-field "mail-utils")
(autoload 'mail-position-on-field "mail-utils")
(autoload 'mail-send "sendmail")
(autoload 'mail-mode "sendmail")
(autoload 'mail-extract-address-components "mail-extr")
(autoload 'set-tapestry "tapestry")
(autoload 'tapestry "tapestry")
(autoload 'tapestry-replace-tapestry-element "tapestry")
(autoload 'tapestry-nullify-tapestry-elements "tapestry")
(autoload 'tapestry-remove-frame-parameters "tapestry")
(autoload 'vm-easy-menu-define "vm-easymenu" nil 'macro)
(autoload 'vm-easy-menu-do-define "vm-easymenu")

(provide 'vm)

;;; vm.el ends here
