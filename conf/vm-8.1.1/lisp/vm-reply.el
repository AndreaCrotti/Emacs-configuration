;;; vm-reply.el --- Mailing, forwarding, and replying commands
;;
;; Copyright (C) 1989-2001 Kyle E. Jones
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


;;; Code:

(defun vm-add-reply-subject-prefix (message &optional start)
  (when (not start)
    (goto-char (point-min))
    (re-search-forward (regexp-quote mail-header-separator) (point-max))
    (forward-char 1)
    (setq start (point)))
  (goto-char start)
  (if (and message vm-included-text-attribution-format)
      (let ((vm-summary-uninteresting-senders nil))
        (insert (vm-summary-sprintf
                 vm-included-text-attribution-format
                 message))))
  (while (re-search-forward "^" (point-max) t)
    (insert vm-included-text-prefix)))

;;;###autoload
(defun vm-fill-long-lines-in-reply ()
  (interactive)
  (vm-fill-paragraphs-containing-long-lines
   vm-fill-long-lines-in-reply-column
   (save-excursion
     (goto-char (point-min))
     (re-search-forward (regexp-quote mail-header-separator) (point-max))
     (forward-line 1)
     (point))
   (point-max)))

;;;###autoload
(defun vm-do-reply (to-all include-text count)
  (let ((mlist (vm-select-marked-or-prefixed-messages count))
        (dir default-directory)
        (message-pointer vm-message-pointer)
        (case-fold-search t)
        to cc subject in-reply-to references
        mp tmp tmp2 newsgroups)
    (setq mp mlist)
    (while mp
      (cond ((add-to-list 'to
                          (let ((reply-to
                                 (vm-get-header-contents (car mp) "Reply-To:"
                                                         ", ")))
                            (if (vm-ignored-reply-to reply-to)
                                nil
                              reply-to ))))
            ((add-to-list 'to (vm-get-header-contents (car mp) "From:"
                                                      ", ")))
            ;; bad, but better than nothing for some
            ((add-to-list 'to (vm-grok-From_-author (car mp))))
            (t (error "No From: or Reply-To: header in message")))

      (let ((this-subject (vm-get-header-contents (car mp) "Subject:"))
            (this-reply-to (and vm-in-reply-to-format
                                (let ((vm-summary-uninteresting-senders nil))
                                  (vm-summary-sprintf vm-in-reply-to-format
                                                      (car mp))))))
        (if (and this-subject vm-reply-subject-prefix
                 (not (string-match vm-reply-subject-prefix this-subject)))
            (setq this-subject (concat vm-reply-subject-prefix
                                       this-subject)))
	  
        (unless subject
          (setq subject (concat this-subject
                                (if (cdr mlist)
                                    (format " [and %d more messages]"
                                            (length (cdr mlist)))))))
        (setq in-reply-to (if in-reply-to
                              (concat in-reply-to ",\n\t" this-reply-to)
                            this-reply-to)))
		
      (cond ((setq tmp (vm-get-header-contents (car mp) "Reply-To:" ", "))
             (if (not (vm-ignored-reply-to tmp))
                 (add-to-list 'to tmp)))
            ((setq tmp (vm-get-header-contents (car mp) "From:" ", "))
             (add-to-list 'to tmp))
            ;; bad, but better than nothing for some
            ((setq tmp (vm-grok-From_-author (car mp)))
             (add-to-list 'to tmp))
            (t (error "No From: or Reply-To: header in message")))

      (if to-all
          (progn
            (setq tmp (vm-get-header-contents (car mp) "To:" ", "))
            (setq tmp2 (vm-get-header-contents (car mp) "Cc:" ", "))
            (if tmp
                (if cc
                    (setq cc (concat cc "," tmp))
                  (setq cc tmp)))
            (if tmp2
                (if cc
                    (setq cc (concat cc "," tmp2))
                  (setq cc tmp2)))))
      (setq references
            (cons (or (vm-get-header-contents (car mp) "References:" " ")
                      (vm-get-header-contents (car mp) "In-reply-to:" " "))
                  (cons (vm-get-header-contents (car mp) "Message-ID:" " ")
                        references)))
      (setq newsgroups
            (cons (or (and to-all
                           (vm-get-header-contents (car mp)
                                                   "Followup-To:" ","))
                      (vm-get-header-contents (car mp) "Newsgroups:" ","))
                  newsgroups))
      (setq mp (cdr mp)))

    (if (null to) nil
      (setq tmp (car to))
      (setq to (cdr to))
      (while to
        (setq tmp (concat tmp ", " (car to)))
        (setq to (cdr to)))
      (setq to tmp))

    (if vm-strip-reply-headers
        (let ((mail-use-rfc822 t))
          (and to (setq to (mail-strip-quoted-names to)))
          (and cc (setq cc (mail-strip-quoted-names cc)))))
    (setq to (vm-parse-addresses to)
          cc (vm-parse-addresses cc))
    (if vm-reply-ignored-addresses
        (setq to (vm-strip-ignored-addresses to)
              cc (vm-strip-ignored-addresses cc)))
    (setq to (vm-delete-duplicates to nil t))
    (setq cc (vm-delete-duplicates
              (append (vm-delete-duplicates cc nil t)
                      to (copy-sequence to))
              t t))
    (and to (setq to (mapconcat 'identity to ",\n    ")))
    (and cc (setq cc (mapconcat 'identity cc ",\n    ")))
    (and (null to) (setq to cc cc nil))
    (setq references (delq nil references)
          references (mapconcat 'identity references " ")
          references (vm-parse references "[^<]*\\(<[^>]+>\\)")
          references (vm-delete-duplicates references)
          references (if references (mapconcat 'identity references "\n\t")))
    (setq newsgroups (delq nil newsgroups)
          newsgroups (mapconcat 'identity newsgroups ",")
          newsgroups (vm-parse newsgroups "[ \t\f\r\n,]*\\([^ \t\f\r\n,]+\\)")
          newsgroups (vm-delete-duplicates newsgroups)
          newsgroups (if newsgroups (mapconcat 'identity newsgroups ",")))
    (vm-mail-internal
     (format "reply to %s%s" (vm-su-full-name (car mlist))
             (if (cdr mlist) ", ..." ""))
     to subject in-reply-to cc references newsgroups)
    (make-local-variable 'vm-reply-list)
    (setq vm-system-state 'replying
          vm-reply-list mlist
          default-directory dir)
    (if include-text
        (save-excursion
          (goto-char (point-min))
          (let ((case-fold-search nil))
            (re-search-forward
             (concat "^" (regexp-quote mail-header-separator) "$") nil 0))
          (forward-char 1)
          (while mlist
            (save-restriction
              (narrow-to-region (point) (point))
              (vm-yank-message (car mlist))
              (goto-char (point-max)))
            (setq mlist (cdr mlist)))))
    (if vm-fill-long-lines-in-reply-column
        (vm-fill-long-lines-in-reply))
    (run-hooks 'vm-reply-hook)
    (run-hooks 'vm-mail-mode-hook)))

(defun vm-strip-ignored-addresses (addresses)
  (setq addresses (copy-sequence addresses))
  (let (re-list list addr-list)
    (setq re-list vm-reply-ignored-addresses)
    (while re-list
      (setq addr-list addresses)
      (while addr-list
	(if (string-match (car re-list) (car addr-list))
	    (setq addresses (delq (car addr-list) addresses)))
	(setq addr-list (cdr addr-list)))
      (setq re-list (cdr re-list))))
  addresses )

(defun vm-ignored-reply-to (reply-to)
  (if (and reply-to (not (string= reply-to "")))
      (let (re-list result)
	(setq re-list vm-reply-ignored-reply-tos)
	(while re-list
	  (if (string-match (car re-list) reply-to)
	      (setq result t re-list nil)
	    (setq re-list (cdr re-list))))
	result)))

;;;###autoload
(defun vm-mail-yank-default (&optional message)
  "The default message yank handler when `mail-citation-hook' is set to nil."
  (save-excursion
    (vm-reorder-message-headers nil vm-included-text-headers
				vm-included-text-discard-header-regexp)
    ;; if all the headers are gone, delete the trailing blank line, too.
    (if (eq (following-char) ?\n)
	(delete-char 1))
    (if (and message vm-included-text-attribution-format)
	(let ((vm-summary-uninteresting-senders nil))
	  (insert (vm-summary-sprintf vm-included-text-attribution-format
				      message))))
    ;; turn off zmacs-regions for Lucid Emacs 19
    ;; and get around transient-mark-mode in FSF Emacs 19
    ;; all this so that (mark) does what it did in v18, sheesh.
    (let* ((zmacs-regions nil)
	   (mark-even-if-inactive t)
	   (end (mark-marker)))
      (while (< (point) end)
	(insert vm-included-text-prefix)
	(forward-line 1)))))

;;;###autoload
(defun vm-yank-message-other-folder (folder)
  "Like vm-yank-message except the message is yanked from a folder other
than the one that spawned the current Mail mode buffer.  The name of the
folder is read from the minibuffer.

Don't call this function from a program."
  (interactive
   (list
    (let ((dir (if vm-folder-directory
		    (expand-file-name vm-folder-directory)
		  default-directory))
	  (last-command last-command)
	  (this-command this-command))
      (read-file-name "Yank from folder: " dir nil t))))
  (let ((b (current-buffer)) newbuf sumbuf default result prompt mp)
    (set-buffer (or (vm-get-file-buffer folder) (find-file-noselect folder)))
    (setq newbuf (current-buffer))
    (if (not (eq major-mode 'vm-mode))
	(vm-mode))
    (if vm-presentation-buffer-handle
	(vm-bury-buffer vm-presentation-buffer-handle))
    (if (null vm-message-pointer)
	(error "No messages in folder %s" folder))
    (setq default (vm-number-of (car vm-message-pointer)))
    (save-excursion
      (save-window-excursion
	(save-window-excursion
	  (vm-summarize))
	(vm-display vm-summary-buffer t '(vm-yank-message-other-folder)
		    '(vm-yank-message-other-folder composing-message))
	(setq sumbuf (current-buffer))
	(setq prompt (format "Yank message number: (default %s) " default)
	      result 0)
	(while (zerop result)
	  (setq result (read-string prompt))
	  (and (string= result "") default (setq result default))
	  (setq result (string-to-number result)))
	(if (null (setq mp (nthcdr (1- result) vm-message-list)))
	    (error "No such message."))))
    (set-buffer b)
    (unwind-protect
	(let ((vm-mail-buffer newbuf))
	  (vm-yank-message (car mp)))
      (vm-bury-buffer newbuf)
      (vm-bury-buffer sumbuf))))

;;;###autoload
(defun vm-yank-message (message)
  "Yank message number N into the current buffer at point.
When called interactively N is always read from the minibuffer.  When
called non-interactively the first argument is expected to be a
message struct.

This command is meant to be used in VM created Mail mode buffers; the
yanked message comes from the mail buffer containing the message you
are replying to, forwarding, or invoked VM's mail command from.

All message headers are yanked along with the text.  Point is
left before the inserted text, the mark after.  Any hook
functions bound to `mail-citation-hook' are run, after inserting
the text and setting point and mark.  For backward compatibility,
if mail-citation-hook is set to nil, `mail-yank-hooks' is run
instead.

If mail-citation-hook and mail-yank-hooks are both nil, this
default action is taken: the yanked headers are trimmed as
specified by `vm-included-text-headers' and
`vm-included-text-discard-header-regexp', and the value of
`vm-included-text-prefix' is prepended to every yanked line."
  (interactive
   (list
    ;; What we really want for the first argument is a message struct,
    ;; but if called interactively, we let the user type in a message
    ;; number instead.
    (let (mp default
             (result 0)
             prompt
             (last-command last-command)
             (this-command this-command))
      (save-excursion
	(vm-select-folder-buffer)
	(setq default (and vm-message-pointer
			   (vm-number-of (car vm-message-pointer)))
	      prompt (if default
			 (format "Yank message number: (default %s) "
				 default)
		       "Yank message number: "))
	(while (zerop result)
	  (setq result (read-string prompt))
	  (and (string= result "") default (setq result default))
	  (setq result (string-to-number result)))
	(if (null (setq mp (nthcdr (1- result) vm-message-list)))
	    (error "No such message.")))
      (car mp))))
  (if (not (bufferp vm-mail-buffer))
      (error "This is not a VM Mail mode buffer."))
  (if (null (buffer-name vm-mail-buffer))
      (error "The folder buffer containing message %d has been killed."
	     (vm-number-of message)))
  (vm-display nil nil '(vm-yank-message) '(vm-yank-message composing-message))
  (setq message (vm-real-message-of message))
  (let ((layout (vm-mm-layout message))
	(start (point))
        (end (point-marker)))
     (save-excursion
      (cond ((or (and vm-include-text-from-presentation
		      (not (vm-mime-plain-message-p message)))
		 (vm-body-to-be-retrieved-of message))
	     (vm-yank-message-presentation message)
	     (setq end (point-marker)))
	    ((null vm-included-mime-types-list)
	     (vm-yank-message-mime message layout)
	     (setq end (point-marker)))
	    (t
	     (vm-yank-message-text message layout)
	     (setq end (point-marker)))
	    )
      ;; decode MIME encoded words so supercite and other
      ;; mail-citation-hook denizens won't have to eat 'em.
      (if vm-display-using-mime
	  (save-restriction
	    (narrow-to-region start end)
	    (vm-decode-mime-encoded-words))))
    ;; get rid of read-only text properties on the text, as
    ;; they will only cause trouble.
    (let ((inhibit-read-only t))
      (remove-text-properties (point-min) (point-max)
                              '(read-only nil invisible nil)
                              (current-buffer)))
    (push-mark end)
    (save-excursion
      ;; Move point above the headers which should be at the top of the buffer by
      ;; this point, and given the push-mark above, mark should now be after the
      ;; message text. This is the invariant needed by the hook functions called
      ;; by mail-citation-hook whose doc string states "Each hook function can
      ;; find the citation between (point) and (mark t)." The upshot of that is
      ;; that if point equals mark at the end of the buffer, some citation
      ;; functions will fail with messages similar to "doesn't conform to RFC
      ;; 822." -- Brent Goodrick, 2009-01-24 
      ;; But this yanks wrongly!  
      ;; The following line reverted by Uday Reddy, 2009-12-07
      ;; (goto-char (point-min))
      (cond (mail-citation-hook (run-hooks 'mail-citation-hook))
	    (mail-yank-hooks (run-hooks 'mail-yank-hooks))
	    (t (vm-mail-yank-default message))))))

(defun vm-yank-message-presentation (message)
  ;; This function is the same as Rob's vm-insert-presentation.
  ;; It has been reported that it includes the entire mail box on
  ;; occasion.  See Bug #498477.  It should not be used until that
  ;; problem resolved.
  (let ((start (point)))
    (vm-insert-region-from-buffer
     (save-excursion
       (vm-select-folder-buffer)
       ;; ensure the current message is presented 
       (vm-show-current-message)
       (vm-select-folder-buffer)
       (if vm-presentation-buffer
	   (set-buffer vm-presentation-buffer))
       (current-buffer)))
    (save-excursion
      (goto-char start)
      (if (looking-at "From ")
          (delete-region start (1+ (line-end-position)))))))

(defun vm-yank-message-mime (message layout)
  ;; This is Rob's new code that uses vm-decode-mime-layout for
  ;; creating the yanked text
  (if (eq layout 'none)

      (vm-insert-region-from-buffer (vm-buffer-of message)
				    (vm-headers-of message)
				    (vm-text-end-of message))
    (vm-insert-region-from-buffer (vm-buffer-of message)
				  (vm-headers-of message)
				  (vm-text-of message))
    (save-excursion
      (goto-char (point-min))
      (vm-decode-mime-message-headers))
    (vm-decode-mime-layout layout)
    (if vm-mime-yank-attachments
	;; FIXME This uses a function of vm-pine.el
	(vm-decode-postponed-mime-message))))

(defun vm-yank-message-text (message layout)
  ;; This is the original code for included text
  (let (new-layout type alternatives parts res insert-start)
    (if (null (vectorp (vm-mm-layout message)))
	(let ((b (current-buffer)))
	  (set-buffer (vm-buffer-of message))
	  (save-restriction
	    (widen)
	    ;; decode MIME encoded words so supercite and other
	    ;; mail-citation-hook denizens won't have to eat 'em.
	    (append-to-buffer b (vm-headers-of message)
			      (vm-text-end-of message))
	    (set-buffer b)))
      (setq type (car (vm-mm-layout-type layout))
	    alternatives 0
	    parts (list layout))

      (vm-insert-region-from-buffer (vm-buffer-of message)
				    (vm-headers-of message)
				    (vm-text-of message))
      (while parts
	(setq layout (car parts))
	(cond ((vm-mime-text-type-layout-p layout)
	       (cond ((vm-mime-types-match
		       "text/enriched"
		       (car (vm-mm-layout-type layout)))
		      (setq res (vm-mime-display-internal-text/enriched
				 layout)))
		     ((vm-mime-types-match
		       "message/rfc822"
		       (car (vm-mm-layout-type layout)))
		      (setq res (vm-mime-display-internal-message/rfc822
				 layout)))
		     ;; no text/html for now
		     ;; ((vm-mime-types-match
		     ;;   "text/html"
		     ;;   (car (vm-mm-layout-type layout)))
		     ;;  (setq res (vm-mime-display-internal-text/html
		     ;; 	      layout)))
		     ((member (downcase (car (vm-mm-layout-type
					      layout)))
			      vm-included-mime-types-list)
		      (setq res (vm-mime-display-internal-text/plain
				 layout t)))
		     ;; convert the layout if possible
		     ((and (not (vm-mm-layout-is-converted layout))
			   (vm-mime-can-convert (car (vm-mm-layout-type
						      layout)))
			   (setq new-layout
				 (vm-mime-convert-undisplayable-layout
				  layout)))
		      (setq res (vm-decode-mime-layout new-layout))))
	       (if res
		   ;; we have found a part to insert, thus skip the
		   ;; remaining alternatives  
		   (while (> alternatives 1)
		     (setq parts (cdr parts)
			   alternatives (1- alternatives)))
			 
		 (if (not (member (downcase (car (vm-mm-layout-type
						  layout)))
				  vm-included-mime-types-list))
		     nil
		   ;; charset problems probably
		   ;; just dump the raw bits
		   (setq insert-start (point))
		   (vm-mime-insert-mime-body layout)
		   (vm-mime-transfer-decode-region layout
						   insert-start
						   (point))))
	       (setq alternatives (1- alternatives))
	       (setq parts (cdr parts)))
	      ;; burst composite types 
	      ((vm-mime-composite-type-p
		(car (vm-mm-layout-type layout)))
	       (setq alternatives (length (vm-mm-layout-parts (car parts))))
	       (setq parts (nconc (copy-sequence
				   (vm-mm-layout-parts
				    (car parts)))
				  (cdr parts))))
	      ;; skip non-text parts 
	      (t
	       (setq alternatives (1- alternatives))
	       (setq parts (cdr parts))))))))

;;;###autoload
(defun vm-mail-send-and-exit (&rest ignored)
  "Send message and maybe delete the composition buffer.
The value of `vm-keep-sent-mesages' determines whether the composition buffer
is deleted.  If the composition is a reply to a message in a currently visited
folder, that message is marked as having been replied to."  
  (interactive "P")
  (vm-check-for-killed-folder)
  (if (and (boundp 'mail-alias-file)
	   mail-alias-file
	   (not (eq (user-uid) 0)))
      (error "Must be superuser to use mail-alias-file.  Please set mail-alias-file to nil."))
  (let ((b (current-buffer)))
    (vm-mail-send)
    (cond ((null (buffer-name b)) ;; dead buffer
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
		       '(vm-mail-send-and-exit reading-message startup))
	   (vm-bury-buffer b)))))

(defun vm-keep-mail-buffer (buffer)
  (vm-keep-some-buffers buffer 'vm-kept-mail-buffers vm-keep-sent-messages))

(defun vm-help-tale ()
  (save-excursion
    (goto-char (point-min))
    (while (vm-match-header)
      (if (not (vm-match-header "To:\\|Resent-To:\\|Cc:\\|Resent-Cc:"))
	  (goto-char (vm-matched-header-end))
	(goto-char (vm-matched-header-contents-start))
	(if (re-search-forward "[^, \t][ \t]*\n[ \t\n]+[^ \t\n]"
			       (vm-matched-header-contents-end)
			       t)
	    (error "tale is an idiot, and so are you. :-)"))
	(goto-char (vm-matched-header-end))))))

(defun vm-mail-mode-insert-message-id-maybe ()
  (if (not vm-mail-header-insert-message-id)
      nil
    (save-restriction
      (save-excursion
	(let ((resent nil))
	  (if (or (vm-mail-mode-get-header-contents "Resent-To:")
		  (vm-mail-mode-get-header-contents "Resent-Cc:")
		  (vm-mail-mode-get-header-contents "Resent-Bcc:"))
	      (progn
		(vm-mail-mode-remove-header "Resent-Message-ID:")
		(setq resent t))
	    (vm-mail-mode-remove-header "Message-ID:"))
	  (widen)
	  (goto-char (point-min))
	  (insert (format "%sMessage-ID: %s\n"
			  (if resent "Resent-" "")
			  (vm-make-message-id))))))))

(defun vm-mail-mode-insert-date-maybe ()
  (if (not vm-mail-header-insert-date)
      nil
    (save-restriction
      (save-excursion
	(let* ((timezone (car (current-time-zone)))
	       (hour (/ timezone 3600))
	       (min (/ (- timezone (* hour 3600)) 60))
	       (time (current-time))
	       (resent nil))
	  (if (or (vm-mail-mode-get-header-contents "Resent-To:")
		  (vm-mail-mode-get-header-contents "Resent-Cc:")
		  (vm-mail-mode-get-header-contents "Resent-Bcc:"))
	      (progn
		(vm-mail-mode-remove-header "Resent-Date:")
		(setq resent t))
	    (vm-mail-mode-remove-header "Date:"))
	  (widen)
	  (goto-char (point-min))
	  (insert (format "%sDate: " (if resent "Resent-" ""))
		  (capitalize
		   (car (nth (string-to-number (format-time-string "%w" time))
			     vm-weekday-alist)))
		  ", "
		  ;; %e generated " 2".  Go from string to int
		  ;; to string to get rid of the blank.
		  (int-to-string
		   (string-to-number
		    (format-time-string "%e" time)))
		  " "
		  (capitalize
		   (car (nth
			 (1- (string-to-number (format-time-string "%m" time)))
			 vm-month-alist)))
		  (format-time-string " %Y %H:%M:%S" time)
		  (format " %s%02d%02d"
			  (if (< timezone 0) "-" "+")
			  (abs hour)
			  (abs min))
;; localization in Europe and elsewhere can cause %Z to return
;; 8-bit chars, which are forbidden in headers.
;;		  (format-time-string " (%Z)" time)
		  "\n"))))))

(defun vm-mail-mode-remove-message-id-maybe ()
  (if vm-mail-header-insert-message-id
      (let ((resent nil))
	(if (or (vm-mail-mode-get-header-contents "Resent-To:")
		(vm-mail-mode-get-header-contents "Resent-Cc:")
		(vm-mail-mode-get-header-contents "Resent-Bcc:"))
	    (progn
	      (vm-mail-mode-remove-header "Resent-Message-ID:")
	      (setq resent t))
	  (vm-mail-mode-remove-header "Message-ID:")))))

(defun vm-mail-mode-remove-date-maybe ()
  (if vm-mail-header-insert-date
      (let ((resent nil))
	(if (or (vm-mail-mode-get-header-contents "Resent-To:")
		(vm-mail-mode-get-header-contents "Resent-Cc:")
		(vm-mail-mode-get-header-contents "Resent-Bcc:"))
	    (progn
	      (vm-mail-mode-remove-header "Resent-Date:")
	      (setq resent t))
	  (vm-mail-mode-remove-header "Date:")))))

(defvar vm-dont-ask-coding-system-question nil)

(cond ((and vm-fsfemacs-mule-p
	    (fboundp 'select-message-coding-system)
	    (not (fboundp 'vm-old-select-message-coding-system)))
       (fset 'vm-old-select-message-coding-system
	     (symbol-function 'select-message-coding-system))
       (defun select-message-coding-system (&rest ignored)
	 (if vm-dont-ask-coding-system-question
	     nil
	   (apply 'vm-old-select-message-coding-system ignored)))))

(defvar select-safe-coding-system-function)

(defvar coding-system-for-write)

;;;###autoload
(defun vm-mail-send ()
  "Just like mail-send except that VM flags the appropriate message(s)
as replied to, forwarded, etc, if appropriate."
  (interactive)
  (if vm-tale-is-an-idiot
      (vm-help-tale))
  ;; protect value of this-command from minibuffer read
  (let ((this-command this-command))
    (if (and vm-confirm-mail-send
	     (not (y-or-n-p "Send the message? ")))
	(error "Message not sent.")))
  (vm-mail-mode-show-headers)
  (save-excursion (run-hooks 'vm-mail-send-hook))
  (vm-mail-mode-insert-date-maybe)
  (vm-mail-mode-insert-message-id-maybe)
  ;; send mail using MIME if user requests it and if the buffer
  ;; has not already been MIME encoded.
  (if (and vm-send-using-mime
	   (null (vm-mail-mode-get-header-contents "MIME-Version:")))
      (vm-mime-encode-composition))
  (if vm-mail-reorder-message-headers
      (vm-reorder-message-headers nil vm-mail-header-order 'none))
  ;; this to prevent Emacs 19 from asking whether a message that
  ;; has already been sent should be sent again.  VM renames mail
  ;; buffers after the message has been sent, so the user should
  ;; already know that the message has been sent.
  (set-buffer-modified-p t)
  (let ((composition-buffer (current-buffer))
	;; preserve these in case the composition buffer gets
	;; killed.
	(vm-reply-list vm-reply-list)
	(vm-forward-list vm-forward-list)
	(vm-redistribute-list vm-redistribute-list))
    ;; fragment message using message/partial if it is too big.
    (if (and vm-send-using-mime
	     (integerp vm-mime-max-message-size)
	     (> (buffer-size) vm-mime-max-message-size))
	(let (list)
	  (setq list (vm-mime-fragment-composition vm-mime-max-message-size))
	  (while list
	    (save-excursion
	      (set-buffer (car list))
	      (vm-mail-send)
	      (kill-buffer (car list)))
	    (setq list (cdr list)))
	  ;; what mail-send would have done
	  (set-buffer-modified-p nil))
      ;; don't want a buffer change to occur here
      ;; save-excursion to be sure.
      ;;
      ;; also protect value of this-command from minibuffer reads
      (let ((this-command this-command)
	    ;; set up coding-system-for-write so that FCC uses
	    ;; the correct coding system to save the message into
	    ;; a folder.
	    (coding-system-for-write
	     (if (stringp mail-archive-file-name)
		 (vm-get-file-line-ending-coding-system
		  mail-archive-file-name)
	       (and (boundp 'coding-system-for-write)
		    coding-system-for-write)))
	    ;; For Emacs 21.
	    (mail-send-nonascii t)
	    (sendmail-coding-system (vm-binary-coding-system))
	    (vm-dont-ask-coding-system-question t)
	    (select-safe-coding-system-function nil))
	(save-excursion
	  (mail-send))))
    ;; be careful, something could have killed the composition
    ;; buffer inside mail-send.
    (if (eq (current-buffer) composition-buffer)
	(progn
	  (cond ((eq vm-system-state 'replying)
		 (vm-mail-mark-replied))
		((eq vm-system-state 'forwarding)
		 (vm-mail-mark-forwarded))
		((eq vm-system-state 'redistributing)
		 (vm-mail-mark-redistributed)))
	  (vm-rename-current-mail-buffer)
	  (vm-keep-mail-buffer (current-buffer))))
    (vm-display nil nil '(vm-mail-send) '(vm-mail-send))))

;;;###autoload
(defun vm-mail-mode-get-header-contents (header-name-regexp)
  (let (regexp)
    (setq regexp (concat "^\\(" header-name-regexp "\\)\\|\\(^"
			 (regexp-quote mail-header-separator) "$\\)"))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let ((case-fold-search t))
	  (if (and (re-search-forward regexp nil t)
		   (match-beginning 1)
		   (progn (goto-char (match-beginning 0))
			  (vm-match-header)))
	      (vm-matched-header-contents)
	    nil ))))))

;;;###autoload
(defun vm-mail-mode-remove-header (header-name-regexp)
  (let (regexp)
    (setq regexp (concat "^\\(" header-name-regexp "\\)\\|\\(^"
			 (regexp-quote mail-header-separator) "$\\)"))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let ((case-fold-search t))
	  (if (and (re-search-forward regexp nil t)
		   (match-beginning 1)
		   (progn (goto-char (match-beginning 0))
			  (vm-match-header)))
	      (delete-region (vm-matched-header-start) (vm-matched-header-end))
	    nil ))))))

(defun vm-rename-current-mail-buffer ()
  (if vm-rename-current-buffer-function
      (funcall vm-rename-current-buffer-function)
    (let ((case-fold-search nil))
      (if (not (string-match "^sent " (buffer-name)))
	  (let (prefix name n)
	    (if (not (string-match "^mail to \\?" (buffer-name)))
		(setq prefix (format "sent %s" (buffer-name)))
	      (let (recipients)
		(cond ((not (zerop (length (setq recipients
						 (mail-fetch-field "To"))))))
		      ((not (zerop (length (setq recipients
						 (mail-fetch-field "Cc"))))))
		      ((not (zerop (length (setq recipients
						 (mail-fetch-field "Bcc"))))))
					; can't happen?!?
		      (t (setq recipients "the horse with no name")))
		(setq prefix (format "sent mail to %s" recipients))))
	    (if (> (length prefix) 44)
		(setq prefix (concat (substring prefix 0 40) " ...")))
	    (setq name prefix n 2)
	    (while (get-buffer name)
	      (setq name (format "%s<%d>" prefix n))
	      (vm-increment n))
	    (rename-buffer name))))))

(defun vm-mail-mark-replied ()
  (save-excursion
    (let ((mp vm-reply-list))
      (while mp
	(if (null (buffer-name (vm-buffer-of (car mp))))
	    ()
	  (set-buffer (vm-buffer-of (car mp)))
	  (cond ((and (memq (car mp) vm-message-list)
		      (null (vm-replied-flag (car mp))))
		 (vm-set-replied-flag (car mp) t))))
	(setq mp (cdr mp)))
      (vm-update-summary-and-mode-line))))

(defun vm-mail-mark-forwarded ()
  (save-excursion
    (let ((mp vm-forward-list))
      (while mp
	(if (null (buffer-name (vm-buffer-of (car mp))))
	    ()
	  (set-buffer (vm-buffer-of (car mp)))
	  (cond ((and (memq (car mp) vm-message-list)
		      (null (vm-forwarded-flag (car mp))))
		 (vm-set-forwarded-flag (car mp) t))))
	(setq mp (cdr mp)))
      (vm-update-summary-and-mode-line))))

(defun vm-mail-mark-redistributed ()
  (save-excursion
    (let ((mp vm-redistribute-list))
      (while mp
	(if (null (buffer-name (vm-buffer-of (car mp))))
	    ()
	  (set-buffer (vm-buffer-of (car mp)))
	  (cond ((and (memq (car mp) vm-message-list)
		      (null (vm-redistributed-flag (car mp))))
		 (vm-set-redistributed-flag (car mp) t))))
	(setq mp (cdr mp)))
      (vm-update-summary-and-mode-line))))

;;;###autoload
(defun vm-reply (count)
  "Reply to the sender of the current message.
Numeric prefix argument N means to reply to the current message plus the
next N-1 messages.  A negative N means reply to the current message and
the previous N-1 messages.

If invoked on marked messages (via vm-next-command-uses-marks),
all marked messages will be replied to.

You will be placed into a standard Emacs Mail mode buffer to compose and
send your message.  See the documentation for the function `mail' for
more info.

Note that the normal binding of C-c C-y in the reply buffer is
automatically changed to vm-yank-message during a reply.  This
allows you to yank any message from the current folder into a
reply.

Normal VM commands may be accessed in the reply buffer by prefixing them
with C-c C-v."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-do-reply nil nil count))

;;;###autoload
(defun vm-reply-include-text (count)
  "Reply to the sender (only) of the current message and include text
from the message.  See the documentation for function vm-reply for details."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-do-reply nil t count))

;;;###autoload
(defun vm-followup (count)
  "Reply to all recipients of the current message.
See the documentation for the function vm-reply for details."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-do-reply t nil count))

;;;###autoload
(defun vm-followup-include-text (count)
  "Reply to all recipients of the current message and include text from
the message.  See the documentation for the function vm-reply for details."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-do-reply t t count))

;;;###autoload
(defun vm-forward-message-all-headers ()
  "Like vm-forward-message but always forwards all the headers."
  (interactive)
  (let ((vm-forwarded-headers nil)
	(vm-unforwarded-header-regexp "only-drop-this-header")
	;; set these because vm-forward-message calls vm-send-digest
	;; if there is more than one message to be forwarded.
	(vm-rfc934-digest-headers nil)
	(vm-rfc934-digest-discard-header-regexp "only-drop-this-header")
	(vm-rfc1153-digest-headers nil)
	(vm-rfc1153-digest-discard-header-regexp "only-drop-this-header")
	(vm-mime-digest-headers nil)
	(vm-mime-digest-discard-header-regexp "only-drop-this-header"))
    (vm-forward-message)))

;;;###autoload
(defun vm-forward-message ()
  "Forward the current message to one or more recipients.
You will be placed in a Mail mode buffer as you would with a
reply, but you must fill in the To: header and perhaps the
Subject: header manually."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if (and (eq last-command 'vm-next-command-uses-marks)
	   (cdr (vm-select-marked-or-prefixed-messages 0)))
      (let ((vm-digest-send-type vm-forwarding-digest-type))
	(setq this-command 'vm-next-command-uses-marks)
	(command-execute 'vm-send-digest))
    (let ((dir default-directory)
	  (miming (and vm-send-using-mime
		       (equal vm-forwarding-digest-type "mime")))
	  reply-buffer
	  header-end
	  (mp (vm-select-marked-or-prefixed-messages 1)))
      (save-restriction
	(widen)
	(vm-mail-internal
	 (format "forward of %s's note re: %s"
		 (vm-su-full-name (car vm-message-pointer))
		 (vm-su-subject (car vm-message-pointer)))
	 nil
	 (and vm-forwarding-subject-format
	      (let ((vm-summary-uninteresting-senders nil))
		(vm-summary-sprintf vm-forwarding-subject-format
				    (car mp)))))
	(make-local-variable 'vm-forward-list)
	(setq vm-system-state 'forwarding
	      vm-forward-list (list (car mp))
	      default-directory dir)
	;; current-buffer is now the reply buffer
	(if (vm-body-to-be-retrieved-of (car mp))
	    (error "Message %s body has not been retrieved"
		   (vm-number-of (car mp))))
	(if miming
	    (progn
	      (setq reply-buffer (current-buffer))
	      (set-buffer (vm-make-work-buffer "*vm-forward-buffer*"))
	      (setq header-end (point))
	      (insert "\n"))
	  (goto-char (point-min))
	  (re-search-forward (concat "^" (regexp-quote mail-header-separator)
				     "\n"))
	  (goto-char (match-end 0))
	  (setq header-end (match-beginning 0)))
	(cond ((equal vm-forwarding-digest-type "mime")
	       (vm-mime-encapsulate-messages (list (car mp))
					     vm-forwarded-headers
					     vm-unforwarded-header-regexp
					     nil)
	       (goto-char header-end)
	       (insert "MIME-Version: 1.0\n")
	       (insert "Content-Type: message/rfc822\n")
	       (insert "Content-Transfer-Encoding: "
		       (vm-determine-proper-content-transfer-encoding
			(point)
			(point-max))
		       "\n")
	       (insert "Content-Description: forwarded message\n")
	       ;; eight bit chars will get \201 prepended if we
	       ;; don't do this.
	       (if vm-fsfemacs-mule-p
		   (set-buffer-multibyte t)))
	      ((equal vm-forwarding-digest-type "rfc934")
	       (vm-rfc934-encapsulate-messages
		vm-forward-list vm-forwarded-headers
		vm-unforwarded-header-regexp))
	      ((equal vm-forwarding-digest-type "rfc1153")
	       (vm-rfc1153-encapsulate-messages
		vm-forward-list vm-forwarded-headers
		vm-unforwarded-header-regexp))
	      ((equal vm-forwarding-digest-type nil)
	       (vm-no-frills-encapsulate-message
		(car vm-forward-list) vm-forwarded-headers
		vm-unforwarded-header-regexp)))
      (if miming
	  (let ((b (current-buffer)))
	    (set-buffer reply-buffer)
	    (mail-text)
	    (vm-mime-attach-object b "message/rfc822" nil
				   "forwarded message" t)
	    (add-hook 'kill-buffer-hook
		      (list 'lambda ()
			    (list 'if (list 'eq reply-buffer '(current-buffer))
				  (list 'kill-buffer b))))))
	(mail-position-on-field "To"))
      (run-hooks 'vm-forward-message-hook)
      (run-hooks 'vm-mail-mode-hook))))

;;;###autoload
(defun vm-resend-bounced-message ()
  "Extract the original text from a bounced message and resend it.
You will be placed in a Mail mode buffer with the extracted message and
you can change the recipient address before resending the message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((b (current-buffer)) start
	(dir default-directory)
	(layout (vm-mm-layout (car vm-message-pointer)))
	(lim (vm-text-end-of (car vm-message-pointer))))
    ;; FIXME try to load the body before saving
    (if (vm-body-to-be-retrieved-of (car vm-message-pointer))
	(error "Message %s body has not been retrieved"
	       (vm-number-of (car vm-message-pointer))))
      (save-restriction
	(widen)
	(if (or (not (vectorp layout))
		(not (setq layout (vm-mime-layout-contains-type
				   layout "message/rfc822"))))
	    (save-excursion
	      (goto-char (vm-text-of (car vm-message-pointer)))
	      (let ((case-fold-search t))
		;; What a wonderful world it would be if mailers
		;; used a single message encapsulation standard
		;; instead of all the weird variants. It is
		;; useless to try to cover them all.  This simple
		;; rule should cover the sanest of the formats
		(if (not (re-search-forward "^Received:" lim t))
		    (error "This doesn't look like a bounced message."))
		(beginning-of-line)
		(setq start (point)))))
	;; briefly nullify vm-mail-header-from to keep vm-mail-internal
	;; from inserting another From header.
	(let ((vm-mail-header-from nil))
	  (vm-mail-internal
	   (format "retry of bounce from %s"
		   (vm-su-from (car vm-message-pointer)))))
	(goto-char (point-min))
	(if (vectorp layout)
	    (progn
	      (setq start (point))
	      (vm-mime-insert-mime-body layout)
	      (vm-mime-transfer-decode-region layout start (point)))
	  (insert-buffer-substring b start lim))
	(delete-region (point) (point-max))
	(goto-char (point-min))
	;; delete all but pertinent headers
	(vm-reorder-message-headers nil nil "\\(X-VM-\\|Status:\\|Sender:\\)")
	(vm-reorder-message-headers nil vm-resend-bounced-headers
				    vm-resend-bounced-discard-header-regexp)
	(if (search-forward "\n\n" nil t)
	    (replace-match "")
	  (goto-char (point-max)))
	(insert ?\n mail-header-separator ?\n)
	(goto-char (point-min))
	(if vm-mail-header-from
	    (insert "Resent-From: " vm-mail-header-from ?\n))
	(if (vm-mail-mode-get-header-contents "Resent-To:")
	    (mail-position-on-field "Resent-To")
	  (insert "Resent-To: \n")
	  (forward-char -1))
	(setq default-directory dir)))
  (run-hooks 'vm-resend-bounced-message-hook)
  (run-hooks 'vm-mail-mode-hook))

;;;###autoload
(defun vm-resend-message ()
  "Resend the current message to someone else.
The current message will be copied to a Mail mode buffer and you
can edit the message and send it as usual.

NOTE: since you are doing a resend, a Resent-To header is provided
for you to fill in the new recipient list.  If you don't fill in
this header, what happens when you send the message is undefined.
You may also create a Resent-Cc header."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (save-restriction
    (widen)
    (let ((b (current-buffer))
	  (dir default-directory)
	  (vmp vm-message-pointer)
	  (start (vm-headers-of (car vm-message-pointer)))
	  (lim (vm-text-end-of (car vm-message-pointer))))
      ;; FIXME try to load the body before saving
      (if (vm-body-to-be-retrieved-of (car vm-message-pointer))
	  (error "Message %s body has not been retrieved"
		 (vm-number-of (car vm-message-pointer))))
      ;; briefly nullify vm-mail-header-from to keep vm-mail-internal
      ;; from inserting another From header.
      (let ((vm-mail-header-from nil))
	(vm-mail-internal
	 (format "resend of %s's note re: %s"
		 (vm-su-full-name (car vm-message-pointer))
		 (vm-su-subject (car vm-message-pointer)))))
      (goto-char (point-min))
      (insert-buffer-substring b start lim)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (if vm-mail-header-from
	  (insert "Resent-From: " vm-mail-header-from ?\n))
      (insert "Resent-To: \n")
      (if mail-self-blind
	  (insert "Bcc: "
		  (cond ((and vm-xemacs-p (fboundp 'user-mail-address))
			 (user-mail-address))
			((and (boundp 'user-mail-address)
			      (stringp user-mail-address))
			 user-mail-address)
			(t (user-login-name)))
		  ?\n))
      (if mail-archive-file-name
	  (insert "FCC: " mail-archive-file-name ?\n))
      ;; delete all but pertinent headers
      (vm-reorder-message-headers nil nil "\\(X-VM-\\|Status:\\|Sender:\\)")
      (vm-reorder-message-headers nil vm-resend-headers
				  vm-resend-discard-header-regexp)
      (if (search-forward "\n\n" nil t)
	  (replace-match ""))
      (insert ?\n mail-header-separator ?\n)
      (goto-char (point-min))
      (mail-position-on-field "Resent-To")
      (make-local-variable 'vm-redistribute-list)
      (setq vm-system-state 'redistributing
	    vm-redistribute-list (list (car vmp))
	    default-directory dir)
      (run-hooks 'vm-resend-message-hook)
      (run-hooks 'vm-mail-mode-hook))))

;;;###autoload
(defun vm-send-digest (&optional prefix)
  "Send a digest of all messages in the current folder to recipients.
The type of the digest is specified by the variable vm-digest-send-type.
You will be placed in a Mail mode buffer as is usual with replies, but you
must fill in the To: and Subject: headers manually.

Prefix arg means to insert a list of preamble lines at the beginning of
the digest.  One line is generated for each message being digestified.
The variable vm-digest-preamble-format determines the format of the
preamble lines.

If invoked on marked messages (via vm-next-command-uses-marks),
only marked messages will be put into the digest."
  (interactive "P")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((dir default-directory)
	(miming (and vm-send-using-mime (equal vm-digest-send-type "mime")))
	mp mail-buffer b
	;; prefix arg doesn't have "normal" meaning here, so only call
	;; vm-select-marked-or-prefixed-messages if we're using marks.
	(mlist (if (eq last-command 'vm-next-command-uses-marks)
		   (vm-select-marked-or-prefixed-messages 0)
		 vm-message-list))
	ms start header-end boundary)
    ;; FIXME try to load the body before saving
    (setq ms mlist)
    (while ms
      (if (vm-body-to-be-retrieved-of (car ms))
	    (error "Message %s body has not been retrieved"
		   (vm-number-of (car ms))))
      (setq ms (cdr ms)))
    (save-restriction
      (widen)
      (vm-mail-internal
       (format "digest from %s" (buffer-name))
       nil
       (and vm-forwarding-subject-format
            (let ((vm-summary-uninteresting-senders nil))
              (concat (vm-summary-sprintf vm-forwarding-subject-format (car mlist))
                      (if (cdr mlist)
                          (format " [and %d more messages]"
                                  (length (cdr mlist))))))))
      (make-local-variable 'vm-forward-list)
      (setq vm-system-state 'forwarding
	    vm-forward-list mlist
	    default-directory dir)
      (if miming
	  (progn
	    (setq mail-buffer (current-buffer))
	    (set-buffer (vm-make-work-buffer "*vm-digest-buffer*"))
	    (setq header-end (point))
	    (insert "\n")
	    (setq start (point-marker)))
	(goto-char (point-min))
	(re-search-forward (concat "^" (regexp-quote mail-header-separator)
				   "\n"))
	(goto-char (match-end 0))
	(setq start (point-marker)
	      header-end (match-beginning 0)))
      (message "Building %s digest..." vm-digest-send-type)
      (cond ((equal vm-digest-send-type "mime")
	     (setq boundary (vm-mime-encapsulate-messages
			     mlist vm-mime-digest-headers
			     vm-mime-digest-discard-header-regexp
			     t))
	     (goto-char header-end)
	     (insert "MIME-Version: 1.0\n")
	     (insert (if vm-mime-avoid-folding-content-type
			 "Content-Type: multipart/digest; boundary=\""
		       "Content-Type: multipart/digest;\n\tboundary=\"")
		     boundary "\"\n")
	     (insert "Content-Transfer-Encoding: "
		     (vm-determine-proper-content-transfer-encoding
		      (point)
		      (point-max))
		     "\n"))
	    ((equal vm-digest-send-type "rfc934")
	     (vm-rfc934-encapsulate-messages
	      mlist vm-rfc934-digest-headers
	      vm-rfc934-digest-discard-header-regexp))
	    ((equal vm-digest-send-type "rfc1153")
	     (vm-rfc1153-encapsulate-messages
	      mlist vm-rfc1153-digest-headers
	      vm-rfc1153-digest-discard-header-regexp))
            ((equal vm-digest-send-type nil)
             (while mlist
               (vm-no-frills-encapsulate-message
                (car mlist) vm-forwarded-headers
                vm-unforwarded-header-regexp)
               (setq mlist (cdr mlist)))))

      (goto-char start)
      (setq mp mlist)
      (if miming
	  (let ((b (current-buffer)))
	    (set-buffer mail-buffer)
	    (mail-text)
	    (vm-mime-attach-object b "multipart/digest"
				   (list (concat "boundary=\""
						 boundary "\"")) nil t)
	    (add-hook 'kill-buffer-hook
		      (list 'lambda ()
			    (list 'if (list 'eq mail-buffer '(current-buffer))
				  (list 'kill-buffer b))))))
      (if prefix
	  (save-excursion
	    (message "Building digest preamble...")
	    (if miming
		(progn
		  (set-buffer mail-buffer)
		  (mail-text)))
	    (while mp
	      (let ((vm-summary-uninteresting-senders nil))
		(insert (vm-summary-sprintf vm-digest-preamble-format
					    (car mp)) "\n"))
	      (if vm-digest-center-preamble
		  (progn
		    (forward-char -1)
		    (center-line)
		    (forward-char 1)))
	      (setq mp (cdr mp)))))
      (mail-position-on-field "To")
      (message "Building %s digest... done" vm-digest-send-type)))
  (run-hooks 'vm-send-digest-hook)
  (run-hooks 'vm-mail-mode-hook))

;;;###autoload
(defun vm-send-rfc934-digest (&optional preamble)
  "Like vm-send-digest but always sends an RFC 934 digest."
  (interactive "P")
  (let ((vm-digest-send-type "rfc934"))
    (vm-send-digest preamble)))

;;;###autoload
(defun vm-send-rfc1153-digest (&optional preamble)
  "Like vm-send-digest but always sends an RFC 1153 digest."
  (interactive "P")
  (let ((vm-digest-send-type "rfc1153"))
    (vm-send-digest preamble)))

;;;###autoload
(defun vm-send-mime-digest (&optional preamble)
  "Like vm-send-digest but always sends an MIME (multipart/digest) digest."
  (interactive "P")
  (let ((vm-digest-send-type "mime"))
    (vm-send-digest preamble)))

;;;###autoload
(defun vm-continue-composing-message (&optional not-picky)
  "Find and select the most recently used mail composition buffer.
If the selected buffer is already a Mail mode buffer then it is
buried before beginning the search.  Non Mail mode buffers and
unmodified Mail buffers are skipped.  Prefix arg means unmodified
Mail mode buffers are not skipped.  If no suitable buffer is
found, the current buffer remains selected."
  (interactive "P")
  (if (eq major-mode 'mail-mode)
      (vm-bury-buffer (current-buffer)))
  (let ((b (vm-find-composition-buffer not-picky)))
    (if (not (or (null b) (eq b (current-buffer))))
	(progn
	  ;; avoid having the window configuration code choose a
	  ;; different composition buffer.
	  (vm-unbury-buffer b)
	  (set-buffer b)
	  (if (and vm-mutable-frames vm-frame-per-composition
		   (vm-multiple-frames-possible-p)
		   ;; only pop up a frame if there's an undisplay
		   ;; hook in place to make the frame go away.
		   vm-undisplay-buffer-hook)
	      (let ((w (vm-get-buffer-window b)))
		(if (null w)
		    (vm-goto-new-frame 'composition)
		  (select-window w)
		  (and vm-warp-mouse-to-new-frame
		       (vm-warp-mouse-to-frame-maybe (vm-window-frame w))))
		;; need to do this here too, since XEmacs has per
		;; frame buffer lists.
		(vm-unbury-buffer b)
		(vm-set-hooks-for-frame-deletion)))
	  (vm-display b t '(vm-continue-composing-message)
		      '(vm-continue-composing-message composing-message)))
      (message "No composition buffers found"))))

;;;###autoload
(defun vm-mail-to-mailto-url (url)
  "Creates a message composition buffer to send mail to the URL.  This
command can be invoked from external agents via an emacsclient."
  (interactive "s")
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (let ((list (vm-parse url "^mailto:\\([^?]*\\)\\??\\|\\([^&]+\\)&?"
			'(1 2)))
	to subject in-reply-to cc references newsgroups body
	tem header value header-list)
    (setq to (car list)
	  to (vm-url-decode-string to)
	  list (cdr list))
    (while list
      (setq tem (vm-parse (car list) "\\([^=]+\\)=?"))
      (if (null (nth 1 tem))
	  nil
	(setq header (downcase (vm-url-decode-string (car tem)))
	      value (vm-url-decode-string (nth 1 tem)))
	(if (member header '("subject" "in-reply-to" "cc"
			     "references" "newsgroups" "body"))
	    ;; set the variable let-bound above
	    (set (intern header) value)
	  ;; we'll insert the header later
	  (setq header-list (cons header (cons value header-list)))))
      (setq list (cdr list)))
    (vm-mail-internal nil to subject in-reply-to cc references newsgroups)
    (save-excursion
      (goto-char (point-min))
      (while header-list
	(insert (car header-list) ": ")
	(capitalize-region (point) (save-excursion (beginning-of-line) (point)))
	(insert (nth 1 header-list) "\n")
	(setq header-list (nthcdr 2 header-list)))
      (if (null body)
	  nil
	(mail-text)
	(save-excursion (insert (vm-url-decode-string body) "\n"))
	;; CRLF to LF for line breaks in the body
	(while (search-forward "\r\n" nil t)
	  (replace-match "\n"))))
    (run-hooks 'vm-mail-hook)
    (run-hooks 'vm-mail-mode-hook)))

;; to quiet the v19 byte compiler
(defvar mail-mode-map)
(defvar mail-aliases)
(defvar mail-default-reply-to)
(defvar mail-signature-file)
(defvar mail-personal-alias-file)

(defun vm-sanitize-buffer-name (buffer-name)
  "Replace chars matching `vm-drop-buffer-name-chars' by an \"_\"."
  (let ((r vm-drop-buffer-name-chars))
    (when buffer-name
      (if r
          (setq buffer-name (vm-replace-in-string buffer-name r "_" t)))
      (if (>= (length buffer-name) vm-buffer-name-limit)
          (setq buffer-name 
		(concat (substring buffer-name 0 (-  vm-buffer-name-limit 4))
			"...")))))
  buffer-name)

(defvar vm-compositions-exist nil)

(defvar vm-composition-buffer-count 0
  "The current number of composition buffers.")

(defvar vm-ml-composition-buffer-count ""
  "The modeline string displayed for the current number of composition buffers.")

(defun vm-update-ml-composition-buffer-count ()
   (setq vm-ml-composition-buffer-count
         (format "%d composition%s" vm-composition-buffer-count
                 (if (= vm-composition-buffer-count 1) "" "s"))))

(defun vm-forget-composition-buffer ()
  (setq vm-composition-buffer-count (- vm-composition-buffer-count 1))
  (setq vm-compositions-exist (> vm-composition-buffer-count 0))
  (vm-update-ml-composition-buffer-count))

(defun vm-new-composition-buffer ()
  (setq vm-composition-buffer-count (+ 1 vm-composition-buffer-count))
  (setq vm-compositions-exist t)
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'vm-forget-composition-buffer nil t)
  (add-hook 'vm-mail-send-hook 'vm-forget-composition-buffer nil t)
  (vm-update-ml-composition-buffer-count))

;;;###autoload
(defun vm-mail-internal
    (&optional buffer-name to subject in-reply-to cc references newsgroups)
    "Create a message buffer and set it up according to args.
Fills in the headers as given by the arguments.
Binds the `vm-mail-mode-map' and hooks"
  (let ((folder-buffer nil))
    (if (memq major-mode '(vm-mode vm-virtual-mode))
	(setq folder-buffer (current-buffer)))
    (setq buffer-name (if buffer-name
                          (vm-decode-mime-encoded-words-in-string buffer-name)
                        "mail to ?"))
    (setq buffer-name (vm-sanitize-buffer-name buffer-name))
    (set-buffer (generate-new-buffer buffer-name))
    ;; FSF Emacs: try to prevent write-region (called to handle FCC) from
    ;; asking the user to choose a safe coding system.
    (if (and vm-fsfemacs-mule-p (fboundp 'set-buffer-file-coding-system))
	(set-buffer-file-coding-system 'raw-text))
    ;; avoid trying to write auto-save files in potentially
    ;; unwritable directories.
    (setq default-directory (or vm-folder-directory (expand-file-name "~/")))
    (auto-save-mode (if auto-save-default 1 -1))
    (mail-mode)
    ;; TM infests mail mode, uninfest it if VM's MIME stuff is in
    ;; use.
    (if vm-send-using-mime
	(vm-mail-mode-remove-tm-hooks))
    (use-local-map vm-mail-mode-map)
    ;; make mail-mode-map the parent of this vm-mail-mode-map, if we can.
    ;; do it only once.
    (if (not vm-mail-mode-map-parented)
	(cond ((fboundp 'set-keymap-parents)
	       (set-keymap-parents vm-mail-mode-map (list mail-mode-map))
	       (setq vm-mail-mode-map-parented t))
	      ((consp mail-mode-map)
	       (nconc vm-mail-mode-map mail-mode-map)
	       (setq vm-mail-mode-map-parented t))))
    (setq vm-mail-buffer folder-buffer
	  mode-popup-menu (and vm-use-menus
			       (vm-menu-support-possible-p)
			       (vm-menu-mode-menu)))
    (and vm-use-menus (vm-menu-support-possible-p)
	 (vm-menu-install-mail-mode-menu))
    (if (fboundp 'mail-aliases-setup) ; use mail-abbrevs.el if present
	(mail-aliases-setup)
      (if (eq mail-aliases t)
	  (progn
	    (setq mail-aliases nil)
	    (if (file-exists-p (or mail-personal-alias-file "~/.mailrc"))
		(build-mail-aliases)))))
    (if (stringp vm-mail-header-from)
	(insert "From: " vm-mail-header-from "\n"))
    (setq to (if to (vm-decode-mime-encoded-words-in-string to))
	  subject (if subject (vm-decode-mime-encoded-words-in-string subject))
	  cc (if cc (vm-decode-mime-encoded-words-in-string cc)))
    (insert "To: " (or to "") "\n")
    (and cc (insert "Cc: " cc "\n"))
    (insert "Subject: " (or subject "") "\n")
    (and newsgroups (insert "Newsgroups: " newsgroups "\n"))
    (and in-reply-to (insert "In-Reply-To: " in-reply-to "\n"))
    (and references (insert "References: " references "\n"))
    (insert "X-Mailer: VM " (vm-version) " under ")
    (if (boundp 'emacs-version)
	   (insert emacs-version)
      (insert "Unknown Emacs"))
    (if (functionp 'emacsw32-version)
	(insert " [" (emacsw32-version) "]"))
    (if (boundp 'system-configuration)
	(insert " (" system-configuration ")"))
    (insert "\n")
    ;; REPLYTO environmental variable support
    ;; note that in FSF Emacs v19.29 we would initialize if the
    ;; value was t.  nil is the trigger value used now.
    (and (eq mail-default-reply-to nil)
	 (setq mail-default-reply-to (getenv "REPLYTO")))
    (if mail-default-reply-to
	(insert "Reply-To: " mail-default-reply-to "\n"))
    (if mail-self-blind
	(insert "Bcc: "
		(cond ((and vm-xemacs-p (fboundp 'user-mail-address))
		       (user-mail-address))
		      ((and (boundp 'user-mail-address)
			    (stringp user-mail-address))
		       user-mail-address)
		      (t (user-login-name)))
		?\n))
    (if mail-archive-file-name
	(insert "FCC: " mail-archive-file-name "\n"))
    (if mail-default-headers
	(insert mail-default-headers))
    (if (not (= (preceding-char) ?\n))
	(insert ?\n))
    (insert mail-header-separator "\n")
    (if mail-signature
	(save-excursion
	  (save-restriction
	    (narrow-to-region (point) (point))
	    (cond ((stringp mail-signature)
		   (insert mail-signature))
		  ((eq mail-signature t)
		   (insert-file-contents (or (and (boundp 'mail-signature-file)
						  (stringp mail-signature-file)
						  mail-signature-file)
					     "~/.signature")))
		  (t
		   (let ((str (eval mail-signature)))
		     (if (stringp str)
			 (insert str)))))
	    (goto-char (point-min))
	    (if (looking-at "\n*-- \n")
		nil
	      (insert "\n-- \n"))
	    (goto-char (point-max)))))
    ;; move this buffer to the head of the buffer list so window
    ;; config stuff will select it as the composition buffer.
    (vm-unbury-buffer (current-buffer))
    ;; make a new frame if the user wants it.
    (if (and vm-mutable-frames vm-frame-per-composition
	     (vm-multiple-frames-possible-p))
	(progn
	  (vm-goto-new-frame 'composition)
	  (vm-set-hooks-for-frame-deletion)))
    ;; now do window configuration
    (vm-display (current-buffer) t
		'(vm-mail
		  vm-mail-other-frame
		  vm-mail-other-window
		  vm-reply
		  vm-reply-other-frame
		  vm-reply-include-text
		  vm-reply-include-text-other-frame
		  vm-followup
		  vm-followup-other-frame
		  vm-followup-include-text
		  vm-followup-include-text-other-frame
		  vm-send-digest
		  vm-send-digest-other-frame
		  vm-send-rfc934-digest
		  vm-send-rfc934-digest-other-frame
		  vm-send-rfc1153-digest
		  vm-send-rfc1153-digest-other-frame
		  vm-send-mime-digest
		  vm-send-mime-digest-other-frame
		  vm-forward-message
		  vm-forward-message-other-frame
		  vm-forward-message-all-headers
		  vm-forward-message-all-headers-other-frame
		  vm-resend-message
		  vm-resend-message-other-frame
		  vm-resend-bounced-message
		  vm-resend-bounced-message-other-frame)
		(list this-command 'composing-message))
    (if (null to)
	(mail-position-on-field "To"))
    (cond ((and vm-xemacs-p
		(fboundp 'start-itimer)
		(null (get-itimer "vm-rename-mail"))
	   (start-itimer "vm-rename-mail"
			 'vm-update-composition-buffer-name
			 1.5 1.5 t)))
	  ((and (fboundp 'run-with-idle-timer)
		(null vm-update-composition-buffer-name-timer))
	   (setq vm-update-composition-buffer-name-timer
		 (run-with-idle-timer 1.5 t 'vm-update-composition-buffer-name))))
    (vm-new-composition-buffer)
    (run-hooks 'mail-setup-hook)))

;;;###autoload
(defun vm-reply-other-frame (count)
  "Like vm-reply, but run in a newly created frame."
  (interactive "p")
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-reply count))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-reply-include-text-other-frame (count)
  "Like vm-reply-include-text, but run in a newly created frame."
  (interactive "p")
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-reply-include-text count))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-followup-other-frame (count)
  "Like vm-followup, but run in a newly created frame."
  (interactive "p")
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-followup count))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-followup-include-text-other-frame (count)
  "Like vm-followup-include-text, but run in a newly created frame."
  (interactive "p")
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-followup-include-text count))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-forward-message-all-headers-other-frame ()
  "Like vm-forward-message-all-headers, but run in a newly created frame."
  (interactive)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-forward-message-all-headers))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-forward-message-other-frame ()
  "Like vm-forward-message, but run in a newly created frame."
  (interactive)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-forward-message))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-resend-message-other-frame ()
  "Like vm-resend-message, but run in a newly created frame."
  (interactive)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-resend-message))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-resend-bounced-message-other-frame ()
  "Like vm-resend-bounced-message, but run in a newly created frame."
  (interactive)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-resend-bounced-message))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-send-digest-other-frame (&optional prefix)
  "Like vm-send-digest, but run in a newly created frame."
  (interactive "P")
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-send-digest prefix))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-send-rfc934-digest-other-frame (&optional prefix)
  "Like vm-send-rfc934-digest, but run in a newly created frame."
  (interactive "P")
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-send-rfc934-digest prefix))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-send-rfc1153-digest-other-frame (&optional prefix)
  "Like vm-send-rfc1153-digest, but run in a newly created frame."
  (interactive "P")
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-send-rfc1153-digest prefix))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-send-mime-digest-other-frame (&optional prefix)
  "Like vm-send-mime-digest, but run in a newly created frame."
  (interactive "P")
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-send-mime-digest prefix))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

(defvar enriched-mode)

;;;###autoload
(defun vm-preview-composition ()
  "Show how the current composition buffer might be displayed
in a MIME-aware mail reader.  VM copies and encodes the current
mail composition buffer and displays it as a mail folder.
Type `q' to quit this temp folder and return to composing your
message."
  (interactive)
  (if (not (eq major-mode 'mail-mode))
      (error "Command must be used in a VM Mail mode buffer."))
  (let ((temp-buffer nil)
	(mail-buffer (current-buffer))
	(enriched (and (boundp 'enriched-mode) enriched-mode))
	e-list)
    (unwind-protect
	(progn
	  (setq temp-buffer (generate-new-buffer "composition preview"))
	  (set-buffer temp-buffer)
	  ;; so vm-mime-xxxx-encode-composition won't complain
	  (setq major-mode 'mail-mode)
	  (set (make-local-variable 'enriched-mode) enriched)
	  (vm-insert-region-from-buffer mail-buffer)
	  (goto-char (point-min))
	  (or (vm-mail-mode-get-header-contents "From")
	      (insert "From: " (user-login-name) "\n"))
	  (or (vm-mail-mode-get-header-contents "Message-ID")
	      (insert (format "Message-ID: <fake.%d.%d@fake.fake>\n"
			      (random 1000000) (random 1000000))))
	  (or (vm-mail-mode-get-header-contents "Date")
	      (insert "Date: "
		      (format-time-string "%a, %d %b %Y %H%M%S %Z"
					  (current-time))
		      "\n"))
	  (and vm-send-using-mime
	       (null (vm-mail-mode-get-header-contents "MIME-Version:"))
	       (vm-mime-encode-composition))
          (if vm-mail-reorder-message-headers
              (vm-reorder-message-headers nil vm-mail-header-order 'none))
  	  (vm-remove-mail-mode-header-separator)
	  (vm-munge-message-separators 'mmdf (point-min) (point-max))
	  (goto-char (point-min))
	  (insert (vm-leading-message-separator 'mmdf))
	  (goto-char (point-max))
	  (if (not (eq (preceding-char) ?\n))
	      (insert ?\n))
	  (insert (vm-trailing-message-separator 'mmdf))
	  (set-buffer-modified-p nil)
	  ;; point of no return, don't kill it if the user quits
	  (setq temp-buffer nil)
	  (let ((vm-auto-decode-mime-messages t)
		(vm-auto-displayed-mime-content-types t))
	    (vm-save-buffer-excursion
	     (vm-goto-new-folder-frame-maybe 'folder)
	     (vm-mode)))
	  (message
	   (substitute-command-keys
	    "Type \\[vm-quit] to continue composing your message"))
	  ;; temp buffer, don't offer to save it.
	  (setq buffer-offer-save nil)
	  (vm-display (or vm-presentation-buffer (current-buffer)) t
		      (list this-command) '(vm-mode startup)))
      (and temp-buffer (kill-buffer temp-buffer)))))

(defun vm-update-composition-buffer-name ()
  (if (and (eq major-mode 'mail-mode)
           (save-match-data (string-match "^\\(mail\\|reply\\) to "
					  (buffer-name))))
      (let ((to (mail-fetch-field "To"))
            (cc (mail-fetch-field "Cc"))
	    (curbufname (buffer-name))
	    (deactivate-mark)
	    fmt newbufname
            (ellipsis ""))
	(cond (vm-reply-list (setq fmt "reply to %s%s"))
	      (t (setq fmt "mail to %s%s on \"%s\"")))
        (setq to (vm-parse-addresses to)
              cc (vm-parse-addresses cc))
        (if (or (cdr to)
                (and (car to) (car cc)))
            (setq ellipsis ", ..."))
        (setq newbufname (or (car to) (car cc) "foo (?)")
              newbufname (funcall vm-chop-full-name-function newbufname)
              newbufname (or (car newbufname) (car (cdr newbufname)))
              newbufname (format fmt newbufname ellipsis
                                 (mail-fetch-field "Subject")))
        (if (equal newbufname curbufname)
            nil
          (setq newbufname (vm-sanitize-buffer-name newbufname))
          (rename-buffer newbufname t)))))

;;;###autoload
(defun vm-mail-mode-remove-tm-hooks ()
  (remove-hook 'mail-setup-hook 'turn-on-mime-edit)
  (remove-hook 'mail-setup-hook 'mime/decode-message-header)
  (remove-hook 'mail-setup-hook 'mime/editor-mode)
  (remove-hook 'mail-send-hook  'mime-edit-maybe-translate)
  (remove-hook 'mail-send-hook 'mime-editor/maybe-translate))


(defun vm-mail-mode-show-headers ()
  "Display any hidden headers in a composition buffer."
  (interactive)
  (mapcar 'delete-overlay (overlays-in (point-min)
                                       (save-excursion (mail-text) (point))))
  (if (local-variable-p 'line-move-ignore-invisible (current-buffer))
      (setq line-move-ignore-invisible nil)))

(make-variable-buffer-local 'line-move-ignore-invisible)

(defun vm-mail-mode-hide-headers ()
  "Hides and protects headers listed in `vm-mail-mode-hidden-headers'.
With a prefix arg, call `vm-mail-mode-show-headers' instead."
  (interactive)
  (let ((case-fold-search t)
        (header-regexp (regexp-opt vm-mail-mode-hidden-headers))
        (header-end (save-excursion (mail-text) (point)))
        start end o)
    (setq header-regexp (concat "^" header-regexp))
    (setq line-move-ignore-invisible t)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward header-regexp header-end t)
        (setq start (match-beginning 0)
              end (1- (re-search-forward "^[^ \t]" header-end)))
        (goto-char end)
        (let ((o (or (car (overlays-at start))
                     (make-overlay start end))))
          (when (not (overlay-get o 'invisible))
            (overlay-put o 'invisible t)
            (overlay-put o 'read-only t)))))))

(defun vm-mail-mode-hide-headers-hook ()
  "Hook which handles `vm-mail-mode-hidden-headers'."
  (if vm-mail-mode-hidden-headers
      (vm-mail-mode-hide-headers)))

(add-hook 'vm-mail-mode-hook 'vm-mail-mode-hide-headers-hook)

(provide 'vm-reply)

;;; vm-reply.el ends here
