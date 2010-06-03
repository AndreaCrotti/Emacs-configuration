;;; vm-save.el --- Saving and piping messages under VM
;;
;; Copyright (C) 1989, 1990, 1993, 1994 Kyle E. Jones
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

;; (match-data) returns the match data as MARKERS, often corrupting it in the
;; process due to buffer narrowing, and the fact that buffers are indexed from
;; 1 while strings are indexed from 0. :-(

;;; Code:

;;;###autoload
(defun vm-match-data ()
  (let ((n (1- (/ (length (match-data)) 2)))
        (list nil))
    (while (>= n 0)
      (setq list (cons (match-beginning n) 
                       (cons (match-end n) list))
            n (1- n)))
    list))

;;;###autoload
(defun vm-auto-select-folder (mp auto-folder-alist)
  (condition-case error-data
      (catch 'match
	(let (header alist tuple-list)
	  (setq alist auto-folder-alist)
	  (while alist
	    (setq header (vm-get-header-contents (car mp) (car (car alist))
						 ", "))
	    (if (null header)
		()
	      (setq tuple-list (cdr (car alist)))
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
			    (setq result (vm-auto-select-folder mp result)))
			  (if result
			      (throw 'match result))))))
		(setq tuple-list (cdr tuple-list))))
	    (setq alist (cdr alist)))
	  nil ))
    (error (error "error processing vm-auto-folder-alist: %s"
		  (prin1-to-string error-data)))))

;;;###autoload
(defun vm-auto-archive-messages (&optional arg)
  "Save all unfiled messages that auto-match a folder via
vm-auto-folder-alist to their appropriate folders.  Messages that
are flagged for deletion are not saved.

Prefix arg means to ask user for confirmation before saving each message.

When invoked on marked messages (via vm-next-command-uses-marks),
only marked messages are checked against vm-auto-folder-alist.

The saved messages are flagged as `filed'."
  (interactive "P")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (message "Archiving...")
  (let ((auto-folder)
	(archived 0))
    (unwind-protect
	;; Need separate (let ...) so vm-message-pointer can
	;; revert back in time for
	;; (vm-update-summary-and-mode-line).
	;; vm-last-save-folder is tucked away here since archives
	;; shouldn't affect its value.
	(let ((vm-message-pointer
	       (if (eq last-command 'vm-next-command-uses-marks)
		   (vm-select-marked-or-prefixed-messages 0)
		 vm-message-list))
	      (done nil)
	      stop-point
	      (vm-last-save-folder vm-last-save-folder)
	      (vm-move-after-deleting nil))
	  ;; mark the place where we should stop.  otherwise if any
	  ;; messages in this folder are archived to this folder
	  ;; we would file messages into this folder forever.
	  (setq stop-point (vm-last vm-message-pointer))
	  (while (not done)
	    (and (not (vm-filed-flag (car vm-message-pointer)))
		 ;; don't archive deleted messages
		 (not (vm-deleted-flag (car vm-message-pointer)))
		 (setq auto-folder (vm-auto-select-folder
				    vm-message-pointer
				    vm-auto-folder-alist))
		 ;; Don't let user archive into the same folder
		 ;; that they are visiting.
		 (not (eq (vm-get-file-buffer auto-folder)
			  (current-buffer)))
		 (or (null arg)
		     (y-or-n-p
		      (format "Save message %s in folder %s? "
			      (vm-number-of (car vm-message-pointer))
			      auto-folder)))
		 (let ((vm-delete-after-saving vm-delete-after-archiving)
		       (last-command 'vm-auto-archive-messages))
		   (vm-save-message auto-folder)
		   (vm-increment archived)
		   (message "%d archived, still working..." archived)))
	    (setq done (eq vm-message-pointer stop-point)
		  vm-message-pointer (cdr vm-message-pointer))))
      ;; fix mode line
      (intern (buffer-name) vm-buffers-needing-display-update)
      (vm-update-summary-and-mode-line))
    (if (zerop archived)
	(message "No messages were archived")
      (message "%d message%s archived"
	       archived (if (= 1 archived) "" "s")))))

;;;---------------------------------------------------------------------------
;; The following defun seems a lot less efficient than it might be,
;; but I don't have a better sense of how to access the folder buffer
;; and read its local variables. [2006/10/31:rpg]
;;---------------------------------------------------------------------------

(defun vm-imap-folder-p ()
  "Is the current folder an IMAP folder?"
  (save-excursion
    (vm-select-folder-buffer)
    (eq vm-folder-access-method 'imap)))

;;;---------------------------------------------------------------------------
;; New shell defun to handle both IMAP and local saving.
;;---------------------------------------------------------------------------
;;;###autoload
(defun vm-save-message (folder &optional count)
  "Save the current message.  This may be done either by saving it
to an IMAP folder or by saving it to a local filesystem folder.
Which is done is controlled by the type of the current vm-folder
buffer and the variable `vm-imap-save-to-server'."
  (interactive
   (if (and vm-imap-save-to-server (vm-imap-folder-p))
       ;; IMAP saving --- argument parsing taken from
       ;; vm-save-message-to-imap-folder
       (let ((this-command this-command)
	     (last-command last-command))
	 (vm-follow-summary-cursor)
	 (save-excursion
	   (vm-session-initialization)
	   (vm-select-folder-buffer)
	   (vm-check-for-killed-summary)
	   (vm-error-if-folder-empty)
	   (list (vm-read-imap-folder-name "Save to IMAP folder: " t)
		 (prefix-numeric-value current-prefix-arg))))
       ;; saving to local filesystem.  argument parsing taken from old
       ;; vm-save-message (now vm-save-message-to-local-folder)
       (list
	;; protect value of last-command
	(let ((last-command last-command)
	      (this-command this-command))
	  (vm-follow-summary-cursor)
	  (let ((default 
		  (save-excursion
		    (vm-select-folder-buffer)
		    (vm-check-for-killed-summary)
		    (vm-error-if-folder-empty)
		    (or (vm-auto-select-folder 
			 vm-message-pointer vm-auto-folder-alist)
			vm-last-save-folder)))
		(dir (or vm-folder-directory default-directory)))
	    (cond ((and default
			(let ((default-directory dir))
			  (file-directory-p default)))
		   (vm-read-file-name "Save in folder: " dir nil nil default))
		  (default
		      (vm-read-file-name
		       (format "Save in folder: (default %s) " default)
		       dir default))
		  (t
		   (vm-read-file-name "Save in folder: " dir nil)))))
	(prefix-numeric-value current-prefix-arg))))
  (if (and vm-imap-save-to-server (vm-imap-folder-p))
      (vm-save-message-to-imap-folder folder count)
    (vm-save-message-to-local-folder folder count)))
   
;;;###autoload
(defun vm-save-message-to-local-folder (folder &optional count)
  "Save the current message to a mail folder.
If the folder already exists, the message will be appended to it.

Prefix arg COUNT means save this message and the next COUNT-1
messages.  A negative COUNT means save this message and the
previous COUNT-1 messages.

When invoked on marked messages (via vm-next-command-uses-marks),
all marked messages in the current folder are saved; other messages are
ignored.

The saved messages are flagged as `filed'."
  (interactive
   (list
    ;; protect value of last-command
    (let ((last-command last-command)
	  (this-command this-command))
      (vm-follow-summary-cursor)
      (let ((default (save-excursion
		       (vm-select-folder-buffer)
		       (vm-check-for-killed-summary)
		       (vm-error-if-folder-empty)
		       (or (vm-auto-select-folder vm-message-pointer
						  vm-auto-folder-alist)
			   vm-last-save-folder)))
	    (dir (or vm-folder-directory default-directory)))
	(cond ((and default
		    (let ((default-directory dir))
		      (file-directory-p default)))
	       (vm-read-file-name "Save in folder: " dir nil nil default))
	      (default
	       (vm-read-file-name
		(format "Save in folder: (default %s) " default)
		dir default))
	      (t
	       (vm-read-file-name "Save in folder: " dir nil)))))
    (prefix-numeric-value current-prefix-arg)))
  (let (auto-folder unexpanded-folder)
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (setq unexpanded-folder folder
	  auto-folder (vm-auto-select-folder vm-message-pointer
					     vm-auto-folder-alist))
    (vm-display nil nil '(vm-save-message) '(vm-save-message))
    (or count (setq count 1))
    ;; Expand the filename, forcing relative paths to resolve
    ;; into the folder directory.
    (let ((default-directory
	    (expand-file-name (or vm-folder-directory default-directory))))
      (setq folder (expand-file-name folder)))
    ;; Confirm new folders, if the user requested this.
    (if (and vm-confirm-new-folders
	     (not (file-exists-p folder))
	     (or (not vm-visit-when-saving) (not (vm-get-file-buffer folder)))
	     (not (y-or-n-p (format "%s does not exist, save there anyway? "
				    folder))))
	(error "Save aborted"))
    ;; Check and see if we are currently visiting the folder
    ;; that the user wants to save to.
    (if (and (not vm-visit-when-saving) (vm-get-file-buffer folder))
	(error "Folder %s is being visited, cannot save." folder))
    (let ((mlist (vm-select-marked-or-prefixed-messages count))
	  (coding-system-for-write
	   (if (file-exists-p folder)
	       (vm-get-file-line-ending-coding-system folder)
	     (vm-new-folder-line-ending-coding-system)))
	  (oldmodebits (and (fboundp 'default-file-modes)
			    (default-file-modes)))
	  (m nil) (count 0) folder-buffer target-type)
      (cond ((and mlist (eq vm-visit-when-saving t))
	     (setq folder-buffer (or (vm-get-file-buffer folder)
				     ;; avoid letter bombs
				     (let ((inhibit-local-variables t)
					   (enable-local-eval nil)
					   (enable-local-variables nil))
				       (find-file-noselect folder)))))
	    ((and mlist vm-visit-when-saving)
	     (setq folder-buffer (vm-get-file-buffer folder))))
      (if (and mlist vm-check-folder-types)
	  (progn
	    (setq target-type (or (vm-get-folder-type folder)
				  vm-default-folder-type
				  (and mlist
				       (vm-message-type-of (car mlist)))))
	    (if (eq target-type 'unknown)
		(error "Folder %s's type is unrecognized" folder))))
      (unwind-protect
	  (save-excursion
	    (and oldmodebits (set-default-file-modes
			      vm-default-folder-permission-bits))
	    ;; if target folder is empty or nonexistent we need to
	    ;; write out the folder header first.
	    (if mlist
		(let ((attrs (file-attributes folder)))
		  (if (or (null attrs) (= 0 (nth 7 attrs)))
		      (if (null folder-buffer)
			  (vm-write-string folder
					   (vm-folder-header target-type))
			(vm-write-string folder-buffer
					 (vm-folder-header target-type))))))
	    (while mlist
	      (setq m (vm-real-message-of (car mlist)))
	      (set-buffer (vm-buffer-of m))
	      ;; FIXME try to load the body before saving
	      (if (vm-body-to-be-retrieved-of m)
		  (error "Message %s body has not been retrieved"
			 (vm-number-of (car mlist))))
	      (vm-save-restriction
	       (widen)
	       ;; have to stuff the attributes in all cases because
	       ;; the deleted attribute may have been stuffed
	       ;; previously and we don't want to save that attribute.
	       ;; also we don't want to save out the cached summary entry.
	       (vm-stuff-attributes m t)
	       (if (null folder-buffer)
		   (if (or (null vm-check-folder-types)
			   (eq target-type (vm-message-type-of m)))
		       (write-region (vm-start-of m)
				     (vm-end-of m)
				     folder t 'quiet)
		     (if (null vm-convert-folder-types)
			 (if (not (vm-virtual-message-p (car mlist)))
			     (error "Folder type mismatch: %s, %s"
				    (vm-message-type-of m) target-type)
			   (error "Message %s type mismatches folder %s"
				  (vm-number-of (car mlist))
				  folder
				  (vm-message-type-of m)
				  target-type))
		       (vm-write-string
			folder
			(vm-leading-message-separator target-type m t))
		       (if (eq target-type 'From_-with-Content-Length)
			   (vm-write-string
			    folder
			    (concat vm-content-length-header " "
				    (vm-su-byte-count m) "\n")))
		       (write-region (vm-headers-of m)
				     (vm-text-end-of m)
				     folder t 'quiet)
		       (vm-write-string
			folder
			(vm-trailing-message-separator target-type))))
		 (save-excursion
		   (set-buffer folder-buffer)
		   ;; if the buffer is a live VM folder
		   ;; honor vm-folder-read-only.
		   (if vm-folder-read-only
		       (signal 'folder-read-only (list (current-buffer))))
		   (let ((buffer-read-only nil))
		     (vm-save-restriction
		      (widen)
		      (save-excursion
			(goto-char (point-max))
			(if (or (null vm-check-folder-types)
				(eq target-type (vm-message-type-of m)))
			    (insert-buffer-substring
			     (vm-buffer-of m)
			     (vm-start-of m) (vm-end-of m))
			  (if (null vm-convert-folder-types)
			      (if (not (vm-virtual-message-p (car mlist)))
				  (error "Folder type mismatch: %s, %s"
					 (vm-message-type-of m) target-type)
				(error "Message %s type mismatches folder %s"
				       (vm-number-of (car mlist))
				       folder
				       (vm-message-type-of m)
				       target-type))
			    (vm-write-string
			     (current-buffer)
			     (vm-leading-message-separator target-type m t))
			    (if (eq target-type 'From_-with-Content-Length)
				(vm-write-string
				 (current-buffer)
				 (concat vm-content-length-header " "
					 (vm-su-byte-count m) "\n")))
			    (insert-buffer-substring (vm-buffer-of m)
						     (vm-headers-of m)
						     (vm-text-end-of m))
			    (vm-write-string
			     (current-buffer)
			     (vm-trailing-message-separator target-type)))))
		      ;; vars should exist and be local
		      ;; but they may have strange values,
		      ;; so check the major-mode.
		      (cond ((eq major-mode 'vm-mode)
			     (vm-increment vm-messages-not-on-disk)
			     (vm-clear-modification-flag-undos)))))))
	       (if (null (vm-filed-flag m))
		   (vm-set-filed-flag m t))
	       (vm-increment count)
	       (vm-modify-folder-totals folder 'saved 1 m)
	       (vm-update-summary-and-mode-line)
	       (setq mlist (cdr mlist)))))
	(and oldmodebits (set-default-file-modes oldmodebits)))
      (if m
	  (if folder-buffer
	      (progn
		(save-excursion
		  (set-buffer folder-buffer)
		  (if (eq major-mode 'vm-mode)
		      (progn
			(vm-check-for-killed-summary)
			(vm-assimilate-new-messages)
			(if (null vm-message-pointer)
			    (progn (setq vm-message-pointer vm-message-list
					 vm-need-summary-pointer-update t)
				   (intern (buffer-name)
					   vm-buffers-needing-display-update)
				   (vm-preview-current-message))
			  (vm-update-summary-and-mode-line)))))
		(message "%d message%s saved to buffer %s"
			 count
			 (if (/= 1 count) "s" "")
			 (buffer-name folder-buffer)))
	    (message "%d message%s saved to %s"
		     count (if (/= 1 count) "s" "") folder))))
    (if (or (null vm-last-save-folder)
	    (not (equal unexpanded-folder auto-folder)))
	(setq vm-last-save-folder unexpanded-folder))
    (if (and vm-delete-after-saving (not vm-folder-read-only))
	(vm-delete-message count))
    folder ))

;;;###autoload
(defun vm-save-message-sans-headers (file &optional count)
  "Save the current message to a file, without its header section.
If the file already exists, the message body will be appended to it.
Prefix arg COUNT means save the next COUNT message bodiess.  A
negative COUNT means save the previous COUNT bodies.

When invoked on marked messages (via vm-next-command-uses-marks),
only the next COUNT marked messages are saved; other intervening
messages are ignored.

The saved messages are flagged as `written'.

This command should NOT be used to save message to mail folders; use
vm-save-message instead (normally bound to `s')."
  (interactive
   ;; protect value of last-command
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-follow-summary-cursor)
     (vm-select-folder-buffer)
     (list
      (vm-read-file-name
       (if vm-last-written-file
	   (format "Write text to file: (default %s) "
		   vm-last-written-file)
	 "Write text to file: ")
       nil vm-last-written-file nil)
      (prefix-numeric-value current-prefix-arg))))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-display nil nil '(vm-save-message-sans-headers)
	      '(vm-save-message-sans-headers))
  (or count (setq count 1))
  (setq file (expand-file-name file))
  ;; Check and see if we are currently visiting the file
  ;; that the user wants to save to.
  (if (and (not vm-visit-when-saving) (vm-get-file-buffer file))
      (error "File %s is being visited, cannot save." file))
  (let ((mlist (vm-select-marked-or-prefixed-messages count))
	(oldmodebits (and (fboundp 'default-file-modes)
			  (default-file-modes)))
	(coding-system-for-write
	 (vm-get-file-line-ending-coding-system file))
	(m nil) file-buffer)
    (cond ((and mlist (eq vm-visit-when-saving t))
	   (setq file-buffer (or (vm-get-file-buffer file)
				 (find-file-noselect file))))
	  ((and mlist vm-visit-when-saving)
	   (setq file-buffer (vm-get-file-buffer file))))
    (if (and (not (memq (vm-get-folder-type file) '(nil unknown)))
	     (not (y-or-n-p "This file looks like a mail folder, append to it anyway? ")))
	(error "Aborted"))
    (unwind-protect
	(save-excursion
	  (and oldmodebits (set-default-file-modes
			    vm-default-folder-permission-bits))
	  (while mlist
	    (setq m (vm-real-message-of (car mlist)))
	    (set-buffer (vm-buffer-of m))
	    ;; FIXME try to load the body before saving
	    (if (vm-body-to-be-retrieved-of m)
		(error "Message %s body has not been retrieved"
		       (vm-number-of (car mlist))))
	    (vm-save-restriction
	     (widen)
	     (if (null file-buffer)
		 (write-region (vm-text-of m)
			       (vm-text-end-of m)
			       file t 'quiet)
	       (let ((start (vm-text-of m))
		     (end (vm-text-end-of m)))
		 (save-excursion
		   (set-buffer file-buffer)
		   (save-excursion
		     (let (buffer-read-only)
		       (vm-save-restriction
			(widen)
			(save-excursion
			  (goto-char (point-max))
			  (insert-buffer-substring
			   (vm-buffer-of m)
			   start end))))))))
	     (if (null (vm-written-flag m))
		 (vm-set-written-flag m t))
	     (vm-update-summary-and-mode-line)
	     (setq mlist (cdr mlist)))))
      (and oldmodebits (set-default-file-modes oldmodebits)))
    (if m
	(if file-buffer
	    (message "Message%s written to buffer %s" (if (/= 1 count) "s" "")
		     (buffer-name file-buffer))
	  (message "Message%s written to %s" (if (/= 1 count) "s" "") file)))
    (setq vm-last-written-file file)))

(defun vm-switch-to-command-output-buffer (command buffer discard-output)
  "Eventually switch to the output buffer of the command."
  (let ((output-bytes (save-excursion (set-buffer buffer) (buffer-size))))
    (if (zerop output-bytes)
	(message "Command '%s' produced no output." command)
      (if discard-output
	  (message "Command '%s' produced %d bytes of output." 
		   command output-bytes)
	(display-buffer buffer)))))

(defun vm-pipe-message-part (m arg)
  "Return (START END) bounds for piping to external command, based on ARG."
  (cond ((equal prefix-arg '(4))
	 (list (vm-text-of m) (vm-text-end-of m)))
	((equal prefix-arg '(16))
	 (list (vm-headers-of m) (vm-text-of m)))
	((equal prefix-arg '(64))
	 (list (vm-vheaders-of m) (vm-text-end-of m)))
	(t 
	 (list (vm-headers-of m) (vm-text-end-of m)))))

;;;###autoload
(defun vm-pipe-message-to-command (command &optional prefix-arg discard-output)
  "Runs a shell command with contents from the current message as input.
By default, the entire message is used.  Message separators are
included if `vm-message-includes-separators' is non-Nil.

With one \\[universal-argument] the text portion of the message is used.
With two \\[universal-argument]'s the header portion of the message is used.
With three \\[universal-argument]'s the visible header portion of the message
plus the text portion is used.

When invoked on marked messages (via vm-next-command-uses-marks),
each marked message is successively piped to the shell command,
one message per command invocation.

Output, if any, is displayed.  The message is not altered."
  (interactive
   ;; protect value of last-command
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-follow-summary-cursor)
     (vm-select-folder-buffer)
     (list (read-string "Pipe to command: " vm-last-pipe-command)
	   current-prefix-arg)))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (setq vm-last-pipe-command command)
  (let ((buffer (get-buffer-create "*Shell Command Output*"))
	m
	(pop-up-windows (and pop-up-windows (eq vm-mutable-windows t)))
	;; prefix arg doesn't have "normal" meaning here, so only call
	;; vm-select-marked-or-prefixed-messages if we're using marks.
	(mlist (if (eq last-command 'vm-next-command-uses-marks)
		   (vm-select-marked-or-prefixed-messages 0)
		 (list (car vm-message-pointer)))))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer))
    (while mlist
      (setq m (vm-real-message-of (car mlist)))
      (set-buffer (vm-buffer-of m))
      ;; FIXME try to load the body before saving
      (if (vm-body-to-be-retrieved-of m)
	  (error "Message %s body has not been retrieved"
		 (vm-number-of (car mlist))))
      (save-restriction
	(widen)
	(let ((pop-up-windows (and pop-up-windows (eq vm-mutable-windows t)))
	      ;; call-process-region calls write-region.
	      ;; don't let it do CR -> LF translation.
	      (selective-display nil)
	      (region (vm-pipe-message-part m prefix-arg)))
	  (call-process-region (nth 0 region) (nth 1 region)
			       (or shell-file-name "sh")
			       nil buffer nil shell-command-switch command)))
      (setq mlist (cdr mlist)))
    (vm-display nil nil '(vm-pipe-message-to-command)
		'(vm-pipe-message-to-command))
    (vm-switch-to-command-output-buffer command buffer discard-output)
    buffer))

(defun vm-pipe-message-to-command-to-string (command &optional prefix-arg)
  "Run a shell command with contents from the current message as input.
This function is like `vm-pipe-message-to-command', but will not display the
output of the command, but return it as a string."
  (save-excursion 
    (set-buffer (vm-pipe-message-to-command command prefix-arg t))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun vm-pipe-message-to-command-discard-output (command &optional prefix-arg)
  "Run a shell command with contents from the current message as input.
This function is like `vm-pipe-message-to-command', but will not display the
output of the command."
  (interactive
   ;; protect value of last-command
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-follow-summary-cursor)
     (vm-select-folder-buffer)
     (list (read-string "Pipe to command: " vm-last-pipe-command)
	   current-prefix-arg)))
  (vm-pipe-message-to-command command prefix-arg t))

(defun vm-pipe-command-exit-handler (process command discard-output 
					     &optional exit-handler)
"Switch to output buffer of PROCESS that ran COMMAND, if
DISCARD-OUTPUT non-nil.  
If non-nil call EXIT-HANDLER with the two arguments COMMAND and OUTPUT-BUFFER." 
  (let ((exit-code (process-exit-status process))
	(buffer (process-buffer process))
	(process-command (process-command process)))
  (if (not (zerop exit-code))
      (message "Command '%s' exit code is %d." command exit-code))
  (vm-display nil nil '(vm-pipe-message-to-command)
	      '(vm-pipe-message-to-command))
  (vm-switch-to-command-output-buffer command buffer discard-output)
  (if exit-handler
      (funcall exit-handler process-command buffer))))

(defvar vm-pipe-messages-to-command-start t
  "*The string to be used as the leading message separator by
`vm-pipe-messages-to-command' at the beginning of each message.
If set to 't', then use the leading message separator stored in the VM
folder.  If set to nil, then no leading separator is included.")

(defvar vm-pipe-messages-to-command-end t
  "*The string to be used as the trailing message separator by
`vm-pipe-messages-to-command' at the end of each message.
If set to 't', then use the trailing message separator stored in the VM
folder.  If set to nil, no trailing separator is included.")

;;;###autoload
(defun vm-pipe-messages-to-command (command &optional prefix-arg 
					    discard-output no-wait)
  "Run a shell command with contents from messages as input.

Similar to `vm-pipe-message-to-command', but it will call process
just once and pipe all messages to it.  For bulk operations this
is much faster than calling the command on each message.  This is
more like saving to a pipe.

With one \\[universal-argument] the text portion of the messages is used.
With two \\[universal-argument]'s the header portion of the messages is used.
With three \\[universal-argument]'s the visible header portion of the messages
plus the text portion is used.

Leading and trailing separators are included with each message
depending on the settings of `vm-pipe-messages-to-command-start'
and `vm-pipe-messages-to-command-end'.

Output, if any, is displayed unless DISCARD-OUTPUT is t.

If NO-WAIT is t, then do not wait for process to finish, if it is
a function then call it with the COMMAND and OUTPUT-BUFFER as
arguments after the command finished."
  (interactive
   ;; protect value of last-command
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-follow-summary-cursor)
     (vm-select-folder-buffer)
     (list (read-string "Pipe to command: " vm-last-pipe-command)
	   current-prefix-arg)))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (setq vm-last-pipe-command command)
  (let ((buffer (get-buffer-create "*Shell Command Output*"))
	(pop-up-windows (and pop-up-windows (eq vm-mutable-windows t)))
	;; prefix arg doesn't have "normal" meaning here, so only call
	;; vm-select-marked-or-prefixed-messages if we're using marks.
	(mlist (if (eq last-command 'vm-next-command-uses-marks)
		   (vm-select-marked-or-prefixed-messages 0)
		 (list (car vm-message-pointer))))
	m process)
    (save-excursion
      (set-buffer buffer)
      (erase-buffer))
    (setq process (start-process command buffer 
				 (or shell-file-name "sh")
				 shell-command-switch command))
    (set-process-sentinel 
     process 
     `(lambda (process status) 
	(setq status (process-status process))
	(if (eq 'exit status)
	    (if ,no-wait
		(vm-pipe-command-exit-handler 
		 process ,command ,discard-output 
		 (if (and ,no-wait (functionp ,no-wait))
		     ,no-wait)))
	  (message "Command '%s' changed state to %s."
		   ,command status))))
    (while mlist
      (setq m (vm-real-message-of (car mlist)))
      (set-buffer (vm-buffer-of m))
      ;; FIXME try to load the body before saving
      (if (vm-body-to-be-retrieved-of m)
	  (error "Message %s body has not been retrieved"
		 (vm-number-of (car mlist))))
     (save-restriction
	(widen)
	(cond ((eq vm-pipe-messages-to-command-start t)
	       (process-send-region process 
				    (vm-start-of m) (vm-headers-of m)))
	      (vm-pipe-messages-to-command-start
	       (process-send-string process vm-pipe-messages-to-command-start)))
	(let ((region (vm-pipe-message-part m prefix-arg)))
	  (process-send-region process (nth 0 region) (nth 1 region)))
	(cond ((eq vm-pipe-messages-to-command-end t)
	       (process-send-region process 
				    (vm-text-end-of m) (vm-end-of m)))
	      (vm-pipe-messages-to-command-end
	       (process-send-string process vm-pipe-messages-to-command-end)))
	(setq mlist (cdr mlist))))

    (process-send-eof process)

    (when (not no-wait) 
      (while (and (eq 'run (process-status process)))
	(accept-process-output process)
	(sit-for 0))
      (vm-pipe-command-exit-handler process command discard-output))
    buffer))

(defun vm-pipe-messages-to-command-to-string (command &optional prefix-arg)
  "Runs a shell command with contents from the current message as input.
This function is like `vm-pipe-messages-to-command', but will not display the
output of the command, but return it as a string."
  (interactive
   ;; protect value of last-command
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-follow-summary-cursor)
     (vm-select-folder-buffer)
     (list (read-string "Pipe to command: " vm-last-pipe-command)
	   current-prefix-arg)))
  (save-excursion 
    (set-buffer (vm-pipe-messages-to-command command prefix-arg t))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun vm-pipe-messages-to-command-discard-output (command &optional prefix-arg)
  "Runs a shell command with contents from the current message as input.
This function is like `vm-pipe-messages-to-command', but will not display the
output of the command."
  (interactive
   ;; protect value of last-command
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-follow-summary-cursor)
     (vm-select-folder-buffer)
     (list (read-string "Pipe to command: " vm-last-pipe-command)
	   current-prefix-arg)))
  (vm-pipe-messages-to-command command prefix-arg t))

;;;###autoload
(defun vm-print-message (&optional count)
  "Print the current message
Prefix arg N means print the current message and the next N - 1 messages.
Prefix arg -N means print the current message and the previous N - 1 messages.

The variable `vm-print-command' controls what command is run to
print the message, and `vm-print-command-switches' is a list of switches
to pass to the command.

When invoked on marked messages (via vm-next-command-uses-marks),
each marked message is printed, one message per vm-print-command invocation.

Output, if any, is displayed.  The message is not altered."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (or count (setq count 1))
  (let* ((buffer (get-buffer-create "*Shell Command Output*"))
	 (need-tempfile (string-match ".*-.*-\\(win95\\|nt\\)"
				      system-configuration))
	 (tempfile (if need-tempfile (vm-make-tempfile-name)))
	 (command (mapconcat (function identity)
			     (nconc (list vm-print-command)
				    (copy-sequence vm-print-command-switches)
				    (if need-tempfile
					(list tempfile)))
			     " "))
	 (m nil)
	 (pop-up-windows (and pop-up-windows (eq vm-mutable-windows t)))
	 (mlist (vm-select-marked-or-prefixed-messages count)))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer))
    (while mlist
      (setq m (vm-real-message-of (car mlist)))
      (set-buffer (vm-buffer-of m))
      ;; FIXME try to load the body before saving
      (if (vm-body-to-be-retrieved-of m)
	  (error "Message %s body has not been retrieved"
		 (vm-number-of (car mlist))))
     (if (and vm-display-using-mime (vectorp (vm-mm-layout m)))
	  (let ((work-buffer nil))
	    (unwind-protect
		(progn
		  (setq work-buffer (vm-make-work-buffer))
		  (set-buffer work-buffer)
		  (vm-insert-region-from-buffer
		   (vm-buffer-of m) (vm-vheaders-of m) (vm-text-of m))
		  (vm-decode-mime-encoded-words)
		  (goto-char (point-max))
		  (let ((vm-auto-displayed-mime-content-types
			 '("text" "multipart"))
			(vm-mime-internal-content-types
			 '("text" "multipart"))
			(vm-mime-external-content-types-alist nil))
		    (vm-decode-mime-layout (vm-mm-layout m)))
		  (let ((pop-up-windows (and pop-up-windows
					     (eq vm-mutable-windows t)))
			;; call-process-region calls write-region.
			;; don't let it do CR -> LF translation.
			(selective-display nil))
		    (if need-tempfile
			(write-region (point-min) (point-max)
				      tempfile nil 0))
		    (call-process-region (point-min) (point-max)
					 (or shell-file-name "sh")
					 nil buffer nil
					 shell-command-switch command)
		    (if need-tempfile
			(vm-error-free-call 'delete-file tempfile))))
	      (and work-buffer (kill-buffer work-buffer))))
	(save-restriction
	  (widen)
	  (narrow-to-region (vm-vheaders-of m) (vm-text-end-of m))
	  (let ((pop-up-windows (and pop-up-windows
				     (eq vm-mutable-windows t)))
		;; call-process-region calls write-region.
		;; don't let it do CR -> LF translation.
		(selective-display nil))
	    (if need-tempfile
		(write-region (point-min) (point-max)
			      tempfile nil 0))
	    (call-process-region (point-min) (point-max)
				 (or shell-file-name "sh")
				 nil buffer nil
				 shell-command-switch command)
	    (if need-tempfile
		(vm-error-free-call 'delete-file tempfile)))))
      (setq mlist (cdr mlist)))
    (vm-display nil nil '(vm-print-message) '(vm-print-message))
    (vm-switch-to-command-output-buffer command buffer nil)))

;;;###autoload
(defun vm-save-message-to-imap-folder (target-folder &optional count)
  "Save the current message to an IMAP folder.
Prefix arg COUNT means save this message and the next COUNT-1
messages.  A negative COUNT means save this message and the
previous COUNT-1 messages.

When invoked on marked messages (via vm-next-command-uses-marks),
all marked messages in the current folder are saved; other messages are
ignored.

The saved messages are flagged as `filed'."
  (interactive
   (let ((this-command this-command)
	 (last-command last-command))
     (vm-follow-summary-cursor)
     (save-excursion
       (vm-session-initialization)
       (vm-select-folder-buffer)
       (vm-check-for-killed-summary)
       (vm-error-if-folder-empty)
       (list (vm-read-imap-folder-name 
	      "Save to IMAP folder: " t nil
	      (or vm-last-save-imap-folder vm-last-visit-imap-folder))
	     (prefix-numeric-value current-prefix-arg)))))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-display nil nil '(vm-save-message-to-imap-folder)
	      '(vm-save-message-to-imap-folder))
  (or count (setq count 1))
  (let (source-spec-list
	(target-spec-list (vm-imap-parse-spec-to-list target-folder))
	(mlist (vm-select-marked-or-prefixed-messages count))
	(count 0)
	server-to-server-p mailbox m
	process
	)
    (setq mailbox (nth 3 target-spec-list))
    (unwind-protect
	(save-excursion
	  (message "Saving messages...")
	  (while mlist
	    (setq m (vm-real-message-of (car mlist)))
	    (set-buffer (vm-buffer-of m))
	    (setq source-spec-list 
		  (and (vm-imap-folder-p)
		       (vm-imap-parse-spec-to-list 
			(vm-folder-imap-maildrop-spec))))
	    (setq server-to-server-p	; copy on the same imap server
		  (and (equal (nth 1 source-spec-list) 
			      (nth 1 target-spec-list))
		       (equal (nth 5 source-spec-list) 
			      (nth 5 target-spec-list))))
	    ;; FIXME try to load the body before saving
	    (if (and (not server-to-server-p)
		     (vm-body-to-be-retrieved-of m))
		(error "Message %s body has not been retrieved"
		       (vm-number-of (car mlist))))
	    ;; Kyle Jones says:
	    ;; have to stuff the attributes in all cases because
	    ;; the deleted attribute may have been stuffed
	    ;; previously and we don't want to save that attribute.
	    ;; FIXME But stuffing attributes into the IMAP buffer is
	    ;; not easy.  USR, 2010-03-08
	    ;; (vm-stuff-attributes m t)
	    (if server-to-server-p 	; economise on upstream data traffic
		(let ((process (vm-re-establish-folder-imap-session)))
		  (vm-imap-copy-message process m mailbox))
	      (unless process
		(setq process (vm-imap-make-session target-folder)))
	      (vm-imap-save-message process m mailbox))
	    (unless (vm-filed-flag m)
	      (vm-set-filed-flag m t))
	    ;; we set the deleted flag so that the user is not
	    ;; confused if the save doesn't go through fully.
	    (when (and vm-delete-after-saving (not (vm-deleted-flag m)))
	      (vm-set-deleted-flag m t))
	    (vm-increment count)
	    (message "Saving messages... %s" count)
	    (vm-modify-folder-totals target-folder 'saved 1 m)
	    (setq mlist (cdr mlist))))
      (when process (vm-imap-end-session process)))
    (vm-update-summary-and-mode-line)
    (setq vm-last-save-imap-folder target-folder)
    (if (and vm-delete-after-saving (not vm-folder-read-only))
	(vm-delete-message count))
    (message "%d message%s saved to %s"
	     count (if (/= 1 count) "s" "")
	     (vm-safe-imapdrop-string target-folder))
    target-folder ))

(provide 'vm-save)

;;; vm-save.el ends here
