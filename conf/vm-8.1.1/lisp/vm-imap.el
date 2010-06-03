;;; vm-imap.el ---  Simple IMAP4 (RFC 2060) client for VM
;;
;; Copyright (C) 1998, 2001, 2003 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
;; Copyright (C) 2006 Robert P. Goldman
;; Copyright (C) 2008 Uday S. Reddy
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
(eval-when-compile 
  (require 'sendmail)
  (require 'vm-vars)
  (require 'vm-misc)
  (require 'vm-macro))

(defvar selectable-only)		; used with dynamic binding

;; To-Do  (USR)
;; - Need to ensure that new imap sessions get created as and when needed.


;; ------------------------------------------------------------------------
;; Utilities
;; ------------------------------------------------------------------------

;; For verification of session protocol
;; Possible values are 
;; 'active - active session present
;; 'valid - message sequence numbers are valid 
;;	validity is preserved by FETCH, STORE and SEARCH operations
;; 'inactive - session is inactive

;; (defvar vm-imap-session-type nil)  ; moved to vm-vars.el

(defun vm-imap-session-type:set (type)
  (setq vm-imap-session-type type))

(defun vm-imap-session-type:make-active ()
  (if (eq vm-imap-session-type 'inactive)
      (setq vm-imap-session-type 'active)))

(defsubst vm-imap-session-type:assert (type)
  (vm-assert (eq vm-imap-session-type type)))

(defsubst vm-imap-folder-session-type:assert (type)
  (save-excursion
    (set-buffer (process-buffer (vm-folder-imap-process)))
    (vm-assert (eq vm-imap-session-type type))))

(defsubst vm-imap-session-type:assert-active ()
  (vm-assert (or (eq vm-imap-session-type 'active) 
		 (eq vm-imap-session-type 'valid))))

(if (fboundp 'define-error)
    (progn
      (define-error 'vm-imap-protocol-error "IMAP protocol error"))
  (put 'vm-imap-protocol-error 'error-conditions
       '(vm-imap-protocol-error error))
  (put 'vm-imap-protocol-error 'error-message "IMAP protocol error"))

(defun vm-imap-capability (cap &optional process)
  (if process
      (save-excursion
	;;----------------------------
	(vm-buffer-type:enter 'process)
	;;----------------------------
	(set-buffer (process-buffer process))
	;;-------------------
	(vm-buffer-type:exit)
	;;-------------------
	(memq cap vm-imap-capabilities))
    (memq cap vm-imap-capabilities)))

(defun vm-imap-auth-method (auth)
  (memq auth vm-imap-auth-methods))

;; the maildrop spec of the imap folder
(defsubst vm-folder-imap-maildrop-spec ()
  (aref vm-folder-access-data 0))
;; current imap process of the folder
(defsubst vm-folder-imap-process ()
  (aref vm-folder-access-data 1))
;; the uid validity value of the imap folder
(defsubst vm-folder-imap-uid-validity ()
  (aref vm-folder-access-data 2))
;; the list of uid's and flags of the messages in the imap folder
;; (msg-num . uid . size . flags list)
(defsubst vm-folder-imap-uid-list ()
  (aref vm-folder-access-data 3))	
;; the number of messages in the imap folder
(defsubst vm-folder-imap-mailbox-count ()
  (aref vm-folder-access-data 4))
;; flag indicating whether the imap folder allows writing
(defsubst vm-folder-imap-read-write ()
  (aref vm-folder-access-data 5))
;; flag indicating whether the imap folder allows deleting
(defsubst vm-folder-imap-can-delete ()
  (aref vm-folder-access-data 6))
;; flag indicating whether the imap server has body-peek functionality
(defsubst vm-folder-imap-body-peek ()
  (aref vm-folder-access-data 7))
;; list of permanent flags storable on the imap server
(defsubst vm-folder-imap-permanent-flags ()
  (aref vm-folder-access-data 8))
;; obarray of uid's with message numbers as their values
(defsubst vm-folder-imap-uid-obarray ()
  (aref vm-folder-access-data 9))	; obarray(uid, msg-num)
;; obarray of uid's with flags lists as their values
(defsubst vm-folder-imap-flags-obarray ()
  (aref vm-folder-access-data 10))	; obarray(uid, (size . flags list))
					; cons-pair shared with imap-uid-list

(defsubst vm-set-folder-imap-maildrop-spec (val)
  (aset vm-folder-access-data 0 val))
(defsubst vm-set-folder-imap-process (val)
  (aset vm-folder-access-data 1 val))
(defsubst vm-set-folder-imap-uid-validity (val)
  (aset vm-folder-access-data 2 val))
(defsubst vm-set-folder-imap-uid-list (val)
  (aset vm-folder-access-data 3 val))
(defsubst vm-set-folder-imap-mailbox-count (val)
  (aset vm-folder-access-data 4 val))
(defsubst vm-set-folder-imap-read-write (val)
  (aset vm-folder-access-data 5 val))
(defsubst vm-set-folder-imap-can-delete (val)
  (aset vm-folder-access-data 6 val))
(defsubst vm-set-folder-imap-body-peek (val)
  (aset vm-folder-access-data 7 val))
(defsubst vm-set-folder-imap-permanent-flags (val)
  (aset vm-folder-access-data 8 val))
(defsubst vm-set-folder-imap-uid-obarray (val)
  (aset vm-folder-access-data 9 val))
(defsubst vm-set-folder-imap-flags-obarray (val)
  (aset vm-folder-access-data 10 val))

(defun delete-common-elements (list1 list2 pred)
  ;; Takes two lists of unique values with dummy headers and
  ;; destructively deletes all their common elements
  (rplacd list1 (sort (cdr list1) pred))
  (rplacd list2 (sort (cdr list2) pred))
  (while (and (cdr list1) (cdr list2))
    (cond ((equal (car (cdr list1)) (car (cdr list2)))
	   (rplacd list1 (cdr (cdr list1)))
	   (rplacd list2 (cdr (cdr list2))))
	  ((apply pred (car (cdr list1)) (car (cdr list2)) nil)
	   (setq list1 (cdr list1)))
	  (t
	   (setq list2 (cdr list2)))
	  )))


;; -----------------------------------------------------------------------
;; IMAP Spool
;; 
;; -- Functions that treat IMAP mailboxes as spools to get mail
;; -- into local buffers and subsequently expunge on the server.
;; -- USR thinks this is obsolete functionality that should not be
;; -- used. Use 'IMAP folders' instead.
;;
;; handler methods:
;; vm-imap-move-mail: (string & string) -> bool
;; vm-imap-check-mail: string -> void
;;
;; interactive commands:
;; vm-expunge-imap-messages: () -> void
;;
;; vm-imap-clear-invalid-retrieval-entries: ... 
;; ------------------------------------------------------------------------


(defsubst vm-imap-fetch-message (process n use-body-peek 
					   &optional headers-only)
  (vm-imap-fetch-messages process n n use-body-peek headers-only))

(defun vm-imap-fetch-messages (process beg end use-body-peek 
				       &optional headers-only) 
  (let ((fetchcmd
         (if headers-only
             (if use-body-peek "(BODY.PEEK[HEADER])" "(RFC822.HEADER)")
           (if use-body-peek "(BODY.PEEK[])" "(RFC822.PEEK)"))))
    (vm-imap-send-command process (format "FETCH %d:%d %s" beg end fetchcmd))))

;; Our goal is to drag the mail from the IMAP maildrop to the crash box.
;; just as if we were using movemail on a spool file.
;; We remember which messages we have retrieved so that we can
;; leave the message in the mailbox, and yet not retrieve the
;; same messages again and again.

;;;###autoload
(defun vm-imap-move-mail (source destination)
  "move-mail function for IMAP folders.  SOURCE is the IMAP mail box
from which mail is to be moved and DESTINATION is the VM folder."
  ;;--------------------------
  (vm-buffer-type:set 'folder)
  ;;--------------------------
  (let ((process nil)
	(m-per-session vm-imap-messages-per-session)
	(b-per-session vm-imap-bytes-per-session)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-imap-move-mail)
			(wrong-number-of-arguments
			  (find-file-name-handler source)))))
	(imapdrop (vm-safe-imapdrop-string source))
	(statblob nil)
	(msgid (list nil nil (vm-imapdrop-sans-password source) 'uid))
	(imap-retrieved-messages vm-imap-retrieved-messages)
	(did-delete nil)
	(source-nopwd (vm-imapdrop-sans-password source))
	use-body-peek auto-expunge x select source-list uid
	can-delete read-write uid-validity
	mailbox mailbox-count message-size response
	n (retrieved 0) retrieved-bytes process-buffer)
    (setq auto-expunge (cond ((setq x (assoc source
					     vm-imap-auto-expunge-alist))
			      (cdr x))
			     ((setq x (assoc (vm-imapdrop-sans-password source)
					     vm-imap-auto-expunge-alist))
			      (cdr x))
			     (t vm-imap-expunge-after-retrieving)))
    (unwind-protect
	(catch 'end-of-session
	  (if handler
	      (throw 'end-of-session
		     (funcall handler 'vm-imap-move-mail source destination)))
	  (setq process (vm-imap-make-session source))
	  (or process (throw 'end-of-session nil))
	  (setq process-buffer (process-buffer process))
	  (save-excursion
	    (set-buffer process-buffer)
	    ;;--------------------------------
	    (vm-buffer-type:enter 'process)
	    ;;--------------------------------
	    ;; find out how many messages are in the box.
	    (setq source-list (vm-parse source "\\([^:]+\\):?")
		  mailbox (nth 3 source-list))
	    (setq select (vm-imap-select-mailbox process mailbox))
	    (setq mailbox-count (nth 0 select)
		  uid-validity (nth 1 select)
		  read-write (nth 2 select)
		  can-delete (nth 3 select)
		  use-body-peek (vm-imap-capability 'IMAP4REV1))
	    ;;--------------------------------
	    (vm-imap-session-type:set 'valid)
	    ;;--------------------------------
	    ;; The session type is not really "valid" because the uid
	    ;; and flags data has not been obtained.  But since
	    ;; move-mail uses a short, bursty session, the effect is
	    ;; that of a valid session throughout.

	    ;; sweep through the retrieval list, removing entries
	    ;; that have been invalidated by the new UIDVALIDITY
	    ;; value.
	    (setq imap-retrieved-messages
		  (vm-imap-clear-invalid-retrieval-entries
		   source-nopwd
		   imap-retrieved-messages
		   uid-validity))
	    ;; loop through the maildrop retrieving and deleting
	    ;; messages as we go.
	    (setq n 1 retrieved-bytes 0)
	    (setq statblob (vm-imap-start-status-timer))
	    (vm-set-imap-stat-x-box statblob imapdrop)
	    (vm-set-imap-stat-x-maxmsg statblob mailbox-count)
	    (while (and (<= n mailbox-count)
			(or (not (natnump m-per-session))
			    (< retrieved m-per-session))
			(or (not (natnump b-per-session))
			    (< retrieved-bytes b-per-session)))
	      (catch 'skip
		(vm-set-imap-stat-x-currmsg statblob n)
		(let (list)
		  (setq list (vm-imap-get-uid-list process n n))
		  (setq uid (cdr (car list)))
		  (setcar msgid uid)
		  (setcar (cdr msgid) uid-validity)
		  (if (member msgid imap-retrieved-messages)
		      (progn
			(if vm-imap-ok-to-ask
			    (message
			     "Skipping message %d (of %d) from %s (retrieved already)..."
			     n mailbox-count imapdrop))
			(throw 'skip t))))
		(setq message-size (vm-imap-get-message-size process n))
		(vm-set-imap-stat-x-need statblob message-size)
		(if (and (integerp vm-imap-max-message-size)
			 (> message-size vm-imap-max-message-size)
			 (progn
			   (setq response
				 (if vm-imap-ok-to-ask
				     (vm-imap-ask-about-large-message
				      process message-size n)
				   'skip))
			   (not (eq response 'retrieve))))
		    (progn
		      (if (and read-write can-delete (eq response 'delete))
			  (progn
			    (message "Deleting message %d..." n)
			    (vm-imap-delete-message process n)
			    (setq did-delete t))
			(if vm-imap-ok-to-ask
			    (message "Skipping message %d..." n)
			  (message
			   "Skipping message %d in %s, too large (%d > %d)..."
			   n imapdrop message-size vm-imap-max-message-size)))
		      (throw 'skip t)))
		(message "Retrieving message %d (of %d) from %s..."
			 n mailbox-count imapdrop)
                (vm-imap-fetch-message process n
				       use-body-peek vm-load-headers-only)
                (vm-imap-retrieve-to-target process destination
					    statblob use-body-peek) 
		(vm-imap-read-ok-response process)
                (message "Retrieving message %d (of %d) from %s...done"
                         n mailbox-count imapdrop)
		(vm-increment retrieved)
		(and b-per-session
		     (setq retrieved-bytes (+ retrieved-bytes message-size)))
		(setq imap-retrieved-messages
		      (cons (copy-sequence msgid)
			    imap-retrieved-messages))
		(if auto-expunge
		  ;; The user doesn't want the messages
		  ;; kept in the mailbox.
		  ;; Delete the message now.
		  (if (and read-write can-delete)
		      (progn
			(vm-imap-delete-message process n)
			(setq did-delete t)))))
	      (vm-increment n))
	    (if did-delete
		(progn
		  ;; CLOSE forces an expunge and avoids the EXPUNGE
		  ;; responses.
		  (vm-imap-send-command process "CLOSE")
		  (vm-imap-read-ok-response process)
		  ;;----------------------------------
		  (vm-imap-session-type:set 'inactive)
		  ;;----------------------------------
		  ))
	    ;;-------------------
	    (vm-buffer-type:exit)
	    ;;-------------------
	    (not (equal retrieved 0))	; return result
	    ))
      ;; unwind-protections
      (setq vm-imap-retrieved-messages imap-retrieved-messages)
      (if (and (eq vm-flush-interval t) (not (equal retrieved 0)))
	  (vm-stuff-imap-retrieved))
      (when statblob 
	(vm-imap-stop-status-timer statblob))
      (when process
	(vm-imap-end-session process))
      )))

(defun vm-imap-check-mail (source)
  ;;--------------------------
  (vm-buffer-type:set 'folder)
  ;;--------------------------
  (let ((process nil)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-imap-check-mail)
			(wrong-number-of-arguments
			 (find-file-name-handler source)))))
	(retrieved vm-imap-retrieved-messages)
	(imapdrop (vm-imapdrop-sans-password source))
	(count 0)
	msg-count uid-validity x response select mailbox source-list
	result)
    (unwind-protect
	(prog1
	    (save-excursion
	      ;;----------------------------
	      (vm-buffer-type:enter 'process)
	      ;;----------------------------
	      (catch 'end-of-session
		(if handler
		    (throw 'end-of-session
			   (funcall handler 'vm-imap-check-mail source)))
		(setq process (vm-imap-make-session source))
		(or process (throw 'end-of-session nil))
		(set-buffer (process-buffer process))
		(setq source-list (vm-parse source "\\([^:]+\\):?")
		      mailbox (nth 3 source-list))
		(setq select (vm-imap-select-mailbox process mailbox)
		      msg-count (car select)
		      uid-validity (nth 1 select))
		(if (zerop msg-count)
		    (progn
		      (vm-store-folder-totals source '(0 0 0 0))
		      (throw 'end-of-session nil)))
		;; sweep through the retrieval list, removing entries
		;; that have been invalidated by the new UIDVALIDITY
		;; value.
		(setq retrieved
		  (vm-imap-clear-invalid-retrieval-entries imapdrop
							   retrieved
							   uid-validity))
		(setq response (vm-imap-get-uid-list process 1 msg-count))
		(if (null response)
		    nil
		  (if (null (car response))
		      ;; (nil . nil) is returned if there are no
		      ;; messages in the mailbox.
		      (progn
			(vm-store-folder-totals source '(0 0 0 0))
			(throw 'end-of-session nil))
		    (while response
		      (if (not (and (setq x (assoc (cdr (car response))
						   retrieved))
				    (equal (nth 1 x) imapdrop)
				    (eq (nth 2 x) 'uid)))
			  (vm-increment count))
		      (setq response (cdr response))))
		  (vm-store-folder-totals source (list count 0 0 0))
		  (throw 'end-of-session (not (eq count 0))))
		(not (equal 0 (car select)))))

	  (setq vm-imap-retrieved-messages retrieved))

      ;; unwind-protections
      (when process 
	(vm-imap-end-session process)
	;; (vm-imap-dump-uid-and-flags-data)
	;;-------------------
	(vm-buffer-type:exit)
	;;-------------------
	))))

(defun vm-expunge-imap-messages ()
  "Deletes all messages from IMAP mailbox that have already been retrieved
into the current folder.  VM sets the \\Deleted flag on all such messages
on all the relevant IMAP servers and then immediately expunges."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-virtual-folder)
  (let ((process nil)
	(source nil)
	(trouble nil)
	(delete-count 0)
	(vm-global-block-new-mail t)
	(vm-imap-ok-to-ask t)
	(did-delete nil)
	msg-count can-delete read-write uid-validity
	select-response source-list imapdrop uid-alist mailbox data mp match)
    (unwind-protect
	(save-excursion
	  ;;------------------------
	  (vm-buffer-type:duplicate)
	  ;;------------------------
	  (setq vm-imap-retrieved-messages
		(sort vm-imap-retrieved-messages
		      (function (lambda (a b)
				  (cond ((string-lessp (nth 2 a) (nth 2 b)) t)
					((string-lessp (nth 2 b)
						       (nth 2 a))
					 nil)
					((string-lessp (nth 1 a) (nth 1 b)) t)
					((string-lessp (nth 1 b) (nth 1 a))
					 nil)
					((string-lessp (nth 0 a) (nth 0 b)) t)
					(t nil))))))
	  (setq mp vm-imap-retrieved-messages)
	  (while mp
	    (catch 'replay
	      (condition-case error-data
		  (progn
		    (setq data (car mp))
		    (if (not (equal source (nth 2 data)))
			(progn
			  (if process
			      (progn
				(if did-delete
				    (progn
				      (vm-imap-send-command process "CLOSE")
				      (vm-imap-read-ok-response process)
				      ;;----------------------------------
				      (vm-imap-session-type:set 'inactive)
				      ;;----------------------------------
				      ;; (vm-imap-dump-uid-and-flags-data)
				      ))
				(vm-imap-end-session process)
				
				(setq process nil
				      did-delete nil)))
			  (setq source (nth 2 data))
			  (setq imapdrop (vm-safe-imapdrop-string source))
			  (condition-case error-data
			      (progn
				(message "Opening IMAP session to %s..."
					 imapdrop)
				(setq process (vm-imap-make-session source))
				(if (null process)
				    (signal 'vm-imap-protocol-error nil))
				;;--------------------------
				(vm-buffer-type:set 'process)
				;;--------------------------
				(set-buffer (process-buffer process))
				(setq source-list (vm-parse source
							    "\\([^:]+\\):?")
				      mailbox (nth 3 source-list)
				      select-response (vm-imap-select-mailbox
						       process mailbox)
				      msg-count (car select-response)
				      uid-validity (nth 1 select-response)
				      read-write (nth 2 select-response)
				      can-delete (nth 3 select-response))
				(setq mp
				      (vm-imap-clear-invalid-retrieval-entries
				       source
				       mp
				       uid-validity))
				(if (not (eq data (car mp)))
				    ;; this entry must have been
				    ;; discarded as invalid, so
				    ;; skip it and process the
				    ;; entry that is now at the
				    ;; head of the list.
				    (throw 'replay t))
				(if (not can-delete)
				    (error "Can't delete messages in mailbox %s, skipping..." mailbox))
				(if (not read-write)
				    (error "Mailbox %s is read-only, skipping..." mailbox))
				(message "Expunging messages in %s..." imapdrop))
			    (error
			     (if (cdr error-data)
				 (apply 'message (cdr error-data))
			       (message
				"Couldn't open IMAP session to %s, skipping..."
				imapdrop))
			     (setq trouble (cons imapdrop trouble))
			     (sleep-for 2)
			     (while (equal (nth 1 (car mp)) source)
			       (setq mp (cdr mp)))
			     (throw 'replay t)))
			  (if (zerop msg-count)
			      (progn
				(while (equal (nth 1 (car mp)) source)
				  (setq mp (cdr mp)))
				(throw 'replay t)))
			  (setq uid-alist
				(vm-imap-get-uid-list
				 process 1 msg-count))))
		    (if (setq match (rassoc (car data) uid-alist))
			(progn
			  (vm-imap-delete-message process (car match))
			  (setq did-delete t)
			  (vm-increment delete-count))))
		(error
		 (setq trouble (cons imapdrop trouble))
		 (message "Something signaled: %s"
			  (prin1-to-string error-data))
		 (sleep-for 2)
		 (message "Skipping rest of mailbox %s..." imapdrop)
		 (sleep-for 2)
		 (while (equal (nth 2 (car mp)) source)
		   (setq mp (cdr mp)))
		 (throw 'replay t)))
	      (setq mp (cdr mp))))
	  (if did-delete
	      (progn
		(vm-imap-send-command process "CLOSE")
		(vm-imap-read-ok-response process)
		;;----------------------------------
		(vm-imap-session-type:set 'inactive)
		;;----------------------------------
		;; (vm-imap-dump-uid-and-flags-data)
		))
	  (if trouble
	      (progn
		;;--------------------------
		(vm-buffer-type:set 'scratch)
		;;--------------------------
		(set-buffer (get-buffer-create "*IMAP Expunge Trouble*"))
		(setq buffer-read-only nil)
		(erase-buffer)
		(insert (format "%s IMAP message%s expunged.\n\n"
				(if (zerop delete-count) "No" delete-count)
				(if (= delete-count 1) "" "s")))
		(insert "VM had problems expunging messages from:\n")
		(nreverse trouble)
		(setq mp trouble)
		(while mp
		  (insert "   " (car mp) "\n")
		  (setq mp (cdr mp)))
		(setq buffer-read-only t)
		(display-buffer (current-buffer)))
	    (message "%s IMAP message%s expunged."
		     (if (zerop delete-count) "No" delete-count)
		     (if (= delete-count 1) "" "s")))
	  ;;-------------------
	  (vm-buffer-type:exit)
	  ;;-------------------
	  )
      (and process (vm-imap-end-session process)))
    (or trouble (setq vm-imap-retrieved-messages nil))))

(defun vm-imap-clear-invalid-retrieval-entries (source-nopwd retrieved
						uid-validity)
  (let ((x retrieved)
	(prev nil))
    (while x
      (if (and (equal source-nopwd (nth 2 (car x)))
	       (not (equal (nth 1 (car x)) uid-validity)))
	  (if prev
	      (setcdr prev (cdr x))
	    (setq retrieved (cdr retrieved))))
      (setq x (cdr x)))
    retrieved ))


;; --------------------------------------------------------------------
;; Server-side
;;
;; vm-establish-new-folder-imap-session: (&optional interactive) -> void
;; vm-re-establish-folder-imap-session: (&optional interactive) -> void
;;
;; -- Functions to handle the interaction with the IMAP server
;;
;; vm-imap-make-session: folder -> process
;; vm-imap-end-session: (process &optional buffer) -> void
;; vm-imap-check-connection: process -> void
;;
;; -- mailbox operations
;; vm-imap-mailbox-list: (process & bool) -> string list
;; vm-imap-create-mailbox: (process & string &optional bool) -> void
;; vm-imap-delete-mailbox: (process & string) -> void
;; vm-imap-rename-mailbox: (process & string & string) -> void
;; 
;; -- lower level I/O
;; vm-imap-send-command: (process command &optional tag no-tag) ->
;; 				void
;; vm-imap-select-mailbox: (process & mailbox &optional bool) -> 
;;				(int uid-validity bool bool (flag list))
;; vm-imap-read-capability-response: process -> ?
;; vm-imap-read-greeting: process -> ?
;; vm-imap-read-ok-response: process -> ?
;; vm-imap-read-response: process -> server-resonse
;; vm-imap-read-response-and-verify: process -> server-resopnse
;; vm-imap-read-boolean-response: process -> ?
;; vm-imap-read-object: (process &optinal bool) -> ?
;; vm-imap-response-matches: (string &rest symbol) -> bool
;; vm-imap-response-bail-if-server-says-farewell: 
;;			response -> void + 'end-of-session exception
;; vm-imap-protocol-error: *&rest
;;
;; -- message opeations
;; vm-imap-retrieve-uid-and-flags-data: () -> void
;; vm-imap-dump-uid-and-flags-data: () -> void
;; vm-imap-get-uid-list: (process & int & int) -> (int . uid) list
;; vm-imap-get-message-data-list: (process & int & int) ->
;;					(int . uid . string list) list
;; vm-imap-get-message-data: (process & vm-message) -> 
;;					(int . uid . string list)
;; vm-imap-save-message-flags: (process & int &optional bool) -> void
;; vm-imap-get-message-size: (process & int) -> int
;; vm-imap-save-message: (process & int & string?) -> void
;; vm-imap-delete-message: (process & int) -> void
;;
;; vm-imap-ask-about-large-message: (process int int) -> ?
;; vm-imap-retrieve-to-target: (process target statblob bodypeek) -> bool
;; 
;; -- to be phased out
;; vm-imap-get-message-flags: 
;;	(process & vm-message &optional norecord:bool) -> 
;; --------------------------------------------------------------------


;; The IMAP sessions work as follows:

;; Generally, sessions are created for get-new-mail, save-folder and
;; vm-imap-synchronize operations.  All these operations read the
;; uid-and-flags-data and cache it internally.  At this stage, the
;; IMAP session is said to be "valid", i.e., message numbers stored in
;; the cache are valid.  As long as FETCH and STORE operations are
;; performed, the session remains valid.

;; When other IMAP operations are performed, the server can send
;; EXPUNGE responses and invalidate the cached message sequence
;; numbers.  In this state, the IMAP session is "active", but not
;; "valid".  Only UID-based commands can be issued in this state.

;; Create a process for a new IMAP session to the account SOURCE and
;; return it.

;;;###autoload
(defun vm-imap-make-session (source)
  "Create a new IMAP session for the IMAP mail box SOURCE."
  (let ((process-to-shutdown nil)
	(folder-type vm-folder-type)
	process ooo
	(imapdrop (vm-safe-imapdrop-string source))
	(coding-system-for-read (vm-binary-coding-system))
	(coding-system-for-write (vm-binary-coding-system))
	(use-ssl nil)
	(use-ssh nil)
	(session-name "IMAP")
	(process-connection-type nil)
	greeting
	host port mailbox auth user pass source-list process-buffer
	source-nopwd-nombox)
    (unwind-protect
	(catch 'end-of-session
	  ;; parse the maildrop
	  (setq source-list (vm-parse source "\\([^:]*\\):?" 1 7)
		host (nth 1 source-list)
		port (nth 2 source-list)
;;		mailbox (nth 3 source-list)
		auth (nth 4 source-list)
		user (nth 5 source-list)
		pass (nth 6 source-list)
		source-nopwd-nombox
		(vm-imapdrop-sans-password-and-mailbox source))
	  (cond ((equal auth "preauth") t)
		((equal "imap-ssl" (car source-list))
		 (setq use-ssl t
		       session-name "IMAP over SSL")
		 (if (null vm-stunnel-program)
		     (error "vm-stunnel-program must be non-nil to use IMAP over SSL.")))
		((equal "imap-ssh" (car source-list))
		 (setq use-ssh t
		       session-name "IMAP over SSH")
		 (if (null vm-ssh-program)
		     (error "vm-ssh-program must be non-nil to use IMAP over SSH."))))
	  ;; carp if parts are missing
	  (if (null host)
	      (error "No host in IMAP maildrop specification, \"%s\""
		     source))
	  (if (null port)
	      (error "No port in IMAP maildrop specification, \"%s\""
		     source))
	  (if (string-match "^[0-9]+$" port)
	      (setq port (string-to-number port)))
	  (if (null auth)
	      (error "No authentication method in IMAP maildrop specification, \"%s\"" source))
	  (if (null user)
	      (error "No user in IMAP maildrop specification, \"%s\""
		     source))
	  (if (null pass)
	      (error "No password in IMAP maildrop specification, \"%s\""
		     source))
	  (if (and (equal pass "*")
		   (not (equal auth "preauth")))
	      (progn
		(setq pass (car (cdr (assoc source-nopwd-nombox
					    vm-imap-passwords))))
		(if (null pass)
		    (if (null vm-imap-ok-to-ask)
			(progn (message "Need password for %s" imapdrop)
			       (throw 'end-of-session nil))
		      (setq pass
			    (read-passwd
			     (format "IMAP password for %s: "
				     imapdrop)))))))
	  ;; save the password for the sake of
	  ;; vm-expunge-imap-messages, which passes password-less
	  ;; imapdrop specifications to vm-make-imap-session.
	  (if (null (assoc source-nopwd-nombox vm-imap-passwords))
	      (setq vm-imap-passwords (cons (list source-nopwd-nombox pass)
					    vm-imap-passwords)))
	  ;; get the trace buffer
	  (setq process-buffer
		(vm-make-work-buffer (format "trace of %s session to %s"
					     session-name
					     host)))
	  (save-excursion
	    ;;----------------------------
	    (vm-buffer-type:enter 'process)
	    ;;----------------------------
	    (set-buffer process-buffer)
	    (setq vm-folder-type (or folder-type vm-default-folder-type))
	    (buffer-disable-undo process-buffer)
	    (make-local-variable 'vm-imap-read-point)
	    ;; clear the trace buffer of old output
	    (erase-buffer)
	    ;; Tell MULE not to mess with the text.
	    (if (fboundp 'set-buffer-file-coding-system)
		(set-buffer-file-coding-system (vm-binary-coding-system) t))
	    (if (equal auth "preauth")
		(setq process
		      (run-hook-with-args-until-success 'vm-imap-session-preauth-hook
							host port mailbox
							user pass)))
	    (if (processp process)
		(set-process-buffer process (current-buffer))
	      (insert "starting " session-name
		      " session " (current-time-string) "\n")
	      (insert (format "connecting to %s:%s\n" host port))
	      ;; open the connection to the server
	      (cond (use-ssl
		     (vm-setup-stunnel-random-data-if-needed)
		     (setq process
			   (apply 'start-process session-name process-buffer
				  vm-stunnel-program
				  (nconc (vm-stunnel-configuration-args host
									port)
					 vm-stunnel-program-switches))))
		    (use-ssh
		     (setq process (open-network-stream
				    session-name process-buffer
				    "127.0.0.1"
				    (vm-setup-ssh-tunnel host port))))
		    (t
		     (setq process (open-network-stream session-name
							process-buffer
							host port))))
	      (and (null process) (throw 'end-of-session nil))
	      (insert-before-markers "connected\n"))
	    (setq vm-imap-read-point (point))
	    (process-kill-without-query process)
	    (if (null (setq greeting (vm-imap-read-greeting process)))
		(progn (delete-process process)
		       (throw 'end-of-session nil)))
	    (setq process-to-shutdown process)
	    (set (make-local-variable 'vm-imap-session-done) nil)
	    ;; record server capabilities
	    (vm-imap-send-command process "CAPABILITY")
	    (if (null (setq ooo (vm-imap-read-capability-response process)))
		(throw 'end-of-session nil))
	    (set (make-local-variable 'vm-imap-capabilities) (car ooo))
	    (set (make-local-variable 'vm-imap-auth-methods) (nth 1 ooo))
	    ;; authentication
	    (cond ((equal auth "login")
		   ;; LOGIN must be supported by all imap servers,
		   ;; no need to check for it in CAPABILITIES.
		   (vm-imap-send-command process
					 (format "LOGIN %s %s"
						 (vm-imap-quote-string user)
						 (vm-imap-quote-string pass)))
		   (if (null (vm-imap-read-ok-response process))
			(progn
			  (setq vm-imap-passwords
				(delete (list source-nopwd-nombox pass)
					vm-imap-passwords))
			  (message "IMAP password for %s incorrect" imapdrop)
			  ;; don't sleep unless we're running synchronously.
			  (if vm-imap-ok-to-ask
			      (sleep-for 2))
			  (throw 'end-of-session nil))
		     ;;--------------------------------
		     (vm-imap-session-type:set 'active)
		     ;;--------------------------------
		     ))
		  ((equal auth "cram-md5")
		   (if (not (vm-imap-auth-method 'CRAM-MD5))
		       (error "CRAM-MD5 authentication unsupported by this server"))
		   (let ((ipad (make-string 64 54))
			 (opad (make-string 64 92))
			 (command "AUTHENTICATE CRAM-MD5")
			 (secret (concat
				  pass
				  (make-string (max 0 (- 64 (length pass)))
					       0)))
			 response p challenge answer)
		     (vm-imap-send-command process command)
		     (setq response 
			   (vm-imap-read-response-and-verify process command))
		     (cond ((vm-imap-response-matches response '+ 'atom)
			    (setq p (cdr (nth 1 response))
				  challenge (buffer-substring
					     (nth 0 p)
					     (nth 1 p))
				  challenge (vm-mime-base64-decode-string
					     challenge)))
			   (t
			    (vm-imap-protocol-error
			     "Don't understand AUTHENTICATE response")))
		     (setq answer
			   (concat
			    user " "
			    (vm-md5-string
			     (concat
			      (vm-xor-string secret opad)
			      (vm-md5-raw-string 
			       (concat
				(vm-xor-string secret ipad) challenge)))))
			   answer (vm-mime-base64-encode-string answer))
		     (vm-imap-send-command process answer nil t)
		     (if (null (vm-imap-read-ok-response process))
			  (progn
			    (setq vm-imap-passwords
				  (delete (list source-nopwd-nombox pass)
					  vm-imap-passwords))
			    (message "IMAP password for %s incorrect" imapdrop)
			    ;; don't sleep unless we're running synchronously.
			    (if vm-imap-ok-to-ask
				(sleep-for 2))
			    (throw 'end-of-session nil))
		       ;;-------------------------------
		       (vm-imap-session-type:set 'active)
		       ;;-------------------------------
		       )))
		  ((equal auth "preauth")
		   (if (not (eq greeting 'preauth))
		       (progn
			 (message "IMAP session was not pre-authenticated")
			 ;; don't sleep unless we're running synchronously.
			 (if vm-imap-ok-to-ask
			     (sleep-for 2))
			 (throw 'end-of-session nil))
		     ;;-------------------------------
		     (vm-imap-session-type:set 'active)
		     ;;-------------------------------
		     ))
		  (t (error "Don't know how to authenticate using %s" auth)))
	    (setq process-to-shutdown nil)
	    ;;-------------------
	    (vm-buffer-type:exit)
	    ;;-------------------
	    process ))
      (if process-to-shutdown		; unwind-protection
	  (vm-imap-end-session process-to-shutdown t))
      (vm-tear-down-stunnel-random-data))))

;; Kill the IMAP session represented by PROCESS.  If the optional
;; argument KEEP-BUFFER is non-nil, the process buffer is retained,
;; otherwise it is killed as well

;;;###autoload
(defun vm-imap-end-session (process &optional keep-buffer)
  "End the IMAP session denoted by PROCESS.  Unless the optional
argument KEEP-BUFFER is non-nil, the process-buffer is deleted.  See
also `vm-imap-keep-trace-buffer'."
  (if (and (memq (process-status process) '(open run))
	   (buffer-live-p (process-buffer process)))
      (save-excursion
	;;----------------------------
	(vm-buffer-type:enter 'process)
	;;----------------------------
	(set-buffer (process-buffer process))
	;; vm-imap-end-session might have already been called on
	;; this process, so don't logout and schedule the killing
	;; the process again if it's already been done.
	(if vm-imap-session-done
	    ;;-------------------------------------
	    (vm-imap-session-type:assert 'inactive)
	    ;;-------------------------------------
	  (vm-imap-send-command process "LOGOUT")
	  (setq vm-imap-session-done t)
	  ;; we don't care about the response.
	  ;; try reading it anyway and see who complains.
	  (vm-imap-read-ok-response process)
	  ;;----------------------------------
	  (vm-imap-session-type:set 'inactive)
	  ;;----------------------------------
	  (if (and (not vm-imap-keep-trace-buffer) (not keep-buffer))
	      (kill-buffer (process-buffer process))
	    (save-excursion
	      ;;----------------------------
	      (vm-buffer-type:enter 'process)
	      ;;----------------------------
	      (set-buffer (process-buffer process))
	      (rename-buffer (concat "saved " (buffer-name)) t)
	      (vm-keep-some-buffers (current-buffer) 'vm-kept-imap-buffers
				    vm-imap-keep-failed-trace-buffers)
	      ;;-------------------
	      (vm-buffer-type:exit)
	      ;;-------------------
	      ))
	  (if (fboundp 'add-async-timeout)
	      (add-async-timeout 2 'delete-process process)
	    (run-at-time 2 nil 'delete-process process)))
	;;----------------------------------
	(vm-buffer-type:exit)
	;;----------------------------------
	)))

;; Status indicator vector
;; timer
(defun vm-imap-stat-timer (o) (aref o 0))
;; whether the current status has been reported already
(defun vm-imap-stat-did-report (o) (aref o 1))
;; mailbox specification
(defun vm-imap-stat-x-box (o) (aref o 2))
;; message number (count) of the message currently being retrieved
(defun vm-imap-stat-x-currmsg (o) (aref o 3))
;; total number of mesasges that need to be retrieved in this round
(defun vm-imap-stat-x-maxmsg (o) (aref o 4))
;; amount of the current message that has been retrieved
(defun vm-imap-stat-x-got (o) (aref o 5))
;; size of the current message
(defun vm-imap-stat-x-need (o) (aref o 6))
;; Data for the message last reported
(defun vm-imap-stat-y-box (o) (aref o 7))
(defun vm-imap-stat-y-currmsg (o) (aref o 8))
(defun vm-imap-stat-y-maxmsg (o) (aref o 9))
(defun vm-imap-stat-y-got (o) (aref o 10))
(defun vm-imap-stat-y-need (o) (aref o 11))

(defun vm-set-imap-stat-timer (o val) (aset o 0 val))
(defun vm-set-imap-stat-did-report (o val) (aset o 1 val))
(defun vm-set-imap-stat-x-box (o val) (aset o 2 val))
(defun vm-set-imap-stat-x-currmsg (o val) (aset o 3 val))
(defun vm-set-imap-stat-x-maxmsg (o val) (aset o 4 val))
(defun vm-set-imap-stat-x-got (o val) (aset o 5 val))
(defun vm-set-imap-stat-x-need (o val) (aset o 6 val))
(defun vm-set-imap-stat-y-box (o val) (aset o 7 val))
(defun vm-set-imap-stat-y-currmsg (o val) (aset o 8 val))
(defun vm-set-imap-stat-y-maxmsg (o val) (aset o 9 val))
(defun vm-set-imap-stat-y-got (o val) (aset o 10 val))
(defun vm-set-imap-stat-y-need (o val) (aset o 11 val))

(defun vm-imap-start-status-timer ()
  (let ((blob (make-vector 12 nil))
	timer)
    (setq timer (add-timeout 5 'vm-imap-report-retrieval-status blob 5))
    (vm-set-imap-stat-timer blob timer)
    blob ))

(defun vm-imap-stop-status-timer (status-blob)
  (if (vm-imap-stat-did-report status-blob)
      (message ""))
  (if (fboundp 'disable-timeout)
      (disable-timeout (vm-imap-stat-timer status-blob))
    (cancel-timer (vm-imap-stat-timer status-blob))))

(defun vm-imap-report-retrieval-status (o)
  (vm-set-imap-stat-did-report o t)
  (cond ((null (vm-imap-stat-x-got o)) t)
	;; should not be possible, but better safe...
	((not (eq (vm-imap-stat-x-box o) (vm-imap-stat-y-box o))) t)
	((not (eq (vm-imap-stat-x-currmsg o) (vm-imap-stat-y-currmsg o))) t)
	(t (message "Retrieving message %d (of %d) from %s, %s..."
		    (vm-imap-stat-x-currmsg o)
		    (vm-imap-stat-x-maxmsg o)
		    (vm-imap-stat-x-box o)
		    (if (vm-imap-stat-x-need o)
			(format "%d%s of %d%s"
				(vm-imap-stat-x-got o)
				(if (> (vm-imap-stat-x-got o)
				       (vm-imap-stat-x-need o))
				    "!"
				  "")
				(vm-imap-stat-x-need o)
				(if (eq (vm-imap-stat-x-got o)
					(vm-imap-stat-y-got o))
				    " (stalled)"
				  ""))
		      "post processing"))))
  (vm-set-imap-stat-y-box o (vm-imap-stat-x-box o))
  (vm-set-imap-stat-y-currmsg o (vm-imap-stat-x-currmsg o))
  (vm-set-imap-stat-y-maxmsg o (vm-imap-stat-x-maxmsg o))
  (vm-set-imap-stat-y-got o (vm-imap-stat-x-got o))
  (vm-set-imap-stat-y-need o (vm-imap-stat-x-need o)))

(defun vm-imap-check-connection (process)
  (cond ((not (memq (process-status process) '(open run)))
	 (vm-imap-protocol-error "IMAP connection not open: %s" process))
	((not (buffer-live-p (process-buffer process)))
	 (vm-imap-protocol-error
	  "IMAP process %s's buffer has been killed" process))))

(defun vm-imap-send-command (process command &optional tag no-tag)
  ;;------------------------------
  (vm-buffer-type:assert 'process)
  ;;------------------------------
  (vm-imap-check-connection process)
  (goto-char (point-max))
  (or no-tag (insert-before-markers (or tag "VM") " "))
  (let ((case-fold-search t))
    (if (string-match "^LOGIN" command)
	(insert-before-markers "LOGIN <parameters omitted>\r\n")
      (insert-before-markers command "\r\n")))
  (setq vm-imap-read-point (point))
  ;; previously we had a process-send-string call for each string
  ;; to avoid extra consing but that caused a lot of packet overhead.
  (if no-tag
      (process-send-string process (format "%s\r\n" command))
    (process-send-string process (format "%s %s\r\n" (or tag "VM") command))))

(defun vm-imap-select-mailbox (process mailbox &optional just-examine)
  ;; I/O function to select an IMAP mailbox
  ;;   PROCESS - the IMAP process
  ;;   MAILBOX - the name fo the mailbox to be selected
  ;;   JUST-EXAMINE - select the mailbox in a read-only (examine) mode
  ;; Returns a list containing:
  ;;   int msg-count - number of messages in the mailbox
  ;;   string uid-validity - the UID validity value of the mailbox
  ;;   bool read-write - whether the mailbox is writable
  ;;   bool can-delete - whether the mailbox allows message deletion
  ;;   server-response permanent-flags - permanent flags used in the mailbox

  ;;------------------------------
  (vm-buffer-type:assert 'process)
  ;;------------------------------

  (let ((imap-buffer (current-buffer))
	(command (if just-examine "EXAMINE" "SELECT"))
	tok response p
	(flags nil)
	(permanent-flags nil)
	(msg-count nil)
	(uid-validity nil)
	(read-write (not just-examine))
	(can-delete t)
	(need-ok t))
    (vm-imap-send-command 
     process (format "%s %s" command (vm-imap-quote-string mailbox)))
    (while need-ok
      (setq response (vm-imap-read-response-and-verify process command))
      (cond ((vm-imap-response-matches response '* 'OK 'vector)
	     (setq p (cdr (nth 2 response)))
	     (cond ((vm-imap-response-matches p 'UIDVALIDITY 'atom)
		    (setq tok (nth 1 p))
		    (setq uid-validity (buffer-substring (nth 1 tok)
							 (nth 2 tok))))
		   ((vm-imap-response-matches p 'PERMANENTFLAGS 'list)
		    (setq permanent-flags (nth 1 p)))))
	    ((vm-imap-response-matches response '* 'FLAGS 'list)
	     (setq flags (nth 2 response)))
	    ((vm-imap-response-matches response '* 'atom 'EXISTS)
	     (setq tok (nth 1 response))
	     (goto-char (nth 1 tok))
	     (setq msg-count (read imap-buffer)))
	    ((vm-imap-response-matches response 'VM 'OK '(vector READ-WRITE))
	     (setq need-ok nil read-write t))
	    ((vm-imap-response-matches response 'VM 'OK '(vector READ-ONLY))
	     (setq need-ok nil read-write t))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))))
    (if (null flags)
	(vm-imap-protocol-error "FLAGS missing from SELECT responses"))
    (if (null msg-count)
	(vm-imap-protocol-error "EXISTS missing from SELECT responses"))
    (if (null uid-validity)
	(vm-imap-protocol-error "UIDVALIDITY missing from SELECT responses"))
    (setq can-delete (vm-imap-scan-list-for-flag flags "\\Deleted"))
    (if (vm-imap-scan-list-for-flag permanent-flags "\\*")
	(if (vm-imap-scan-list-for-flag flags "\\Seen")
	    nil
	  (message "Warning: No permanent changes permitted for the mailbox"))
      (message "Warning: Only basic message flags available for the mailbox")
      )
    ;;-------------------------------
    (vm-imap-session-type:set 'active)
    ;;-------------------------------
    (list msg-count uid-validity read-write can-delete permanent-flags)))

(defun vm-imap-read-expunge-response (process)
  (let ((list nil)
	(imap-buffer (current-buffer))
	(need-ok t)
	tok msg-num response
	)
    (while need-ok
      (setq response (vm-imap-read-response-and-verify process "EXPUNGE"))
      (cond ((vm-imap-response-matches response '* 'atom 'EXPUNGE)
	     (setq tok (nth 1 response))
	     (goto-char (nth 1 tok))
	     (setq msg-num (read imap-buffer))
	     (setq list (cons msg-num list)))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))))
    ;;--------------------------------
    (vm-imap-session-type:set 'active)		; seq nums are now invalid
    ;;--------------------------------
    (nreverse list)))

(defun vm-imap-get-uid-list (process first last)
  ;; I/O function to read the uid's of a message range
  ;;   PROCESS - the IMAP process
  ;;   FIRST - message sequence number of the first message in the range
  ;;   LAST - message sequene number of the last message in the range
  ;; Returns an assoc list with pairs 
  ;;   int msg-num - message sequence number of a message
  ;;   string uid - uid of the message
  ;; or nil indicating failure
  ;; If there are no messages in the range then (nil) is returned

  (let ((list nil)
	(imap-buffer (current-buffer))
	tok msg-num uid response p
	(need-ok t))
    ;;----------------------------------
    (vm-imap-session-type:assert-active)
    ;;----------------------------------
    (vm-imap-send-command process (format "FETCH %s:%s (UID)" first last))
    (while need-ok
      (setq response (vm-imap-read-response-and-verify process "UID FETCH"))
      (cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	     (setq p (cdr (nth 3 response)))
	     (if (not (vm-imap-response-matches p 'UID 'atom))
		 (vm-imap-protocol-error
		  "expected (UID number) in FETCH response"))
	     (setq tok (nth 1 response))
	     (goto-char (nth 1 tok))
	     (setq msg-num (read imap-buffer))
	     (setq tok (nth 1 p))
	     (setq uid (buffer-substring (nth 1 tok) (nth 2 tok))
		   list (cons (cons msg-num uid) list)))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))))
      ;; returning nil means the uid fetch failed so return
      ;; something other than nil if there aren't any messages.
      (if (null list)
	  (cons nil nil)
	list )))

;; This function is not recommended, but is available to use when
;; caching uid-and-flags data might be too expensive.

(defun vm-imap-get-message-data (process m uid-validity)
  ;; I/O function to read the flags of a message
  ;;   PROCESS  - The IMAP process
  ;;   M - a vm-message
  ;;   uid-validity -  the folder's uid-validity
  ;; Returns (msg-num: int . uid: string . size: string . flags: string list)
  ;; Or gives an error if the message has an invalid uid
  (let ((imap-buffer (current-buffer))
	response tok need-ok msg-num list)
    (if (not (equal (vm-imap-uid-validity-of m) uid-validity))
	(vm-imap-protocol-error "message has invalid uid"))
    ;;----------------------------------
    (vm-imap-session-type:assert 'valid)
    ;;----------------------------------
    (vm-imap-send-command
     process (format "SEARCH UID %s" (vm-imap-uid-of m)))
    (setq need-ok t)
    (while need-ok
      (setq response (vm-imap-read-response-and-verify process "UID"))
      (cond ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))
	    ((vm-imap-response-matches response '* 'SEARCH 'atom)
	     (if (null (setq tok (nth 2 response)))
		 (vm-imap-protocol-error "Message not found on server"))
	     (goto-char (nth 1 tok))
	     (setq msg-num (read imap-buffer))
	     )))
    (setq list (vm-imap-get-message-data-list process msg-num msg-num))
    (car list)))
	

(defun vm-imap-get-message-data-list (process first last)
  ;; I/O function to read the flags of a message range
  ;;   PROCESS - the IMAP process
  ;;   FIRST - message sequence number of the first message in the range
  ;;   LAST - message sequene number of the last message in the range
  ;; Returns an assoc list with entries
  ;;   int msg-num - message sequence number of a message
  ;;   string uid - uid of the message
  ;;   string size - message size
  ;;   (string list) flags - list of flags for the message
  ;; or nil indicating failure
  ;; If there are no messages in the range then (nil) is returned

  (let ((list nil)
	(imap-buffer (current-buffer))
	tok msg-num uid size flag flags response p pl
	(need-ok t))
    ;;----------------------------------
    (if vm-buffer-type-debug
	(setq vm-buffer-type-trail (cons 'message-data vm-buffer-type-trail)))
    (vm-buffer-type:assert 'process)
    (vm-imap-session-type:assert-active)
    ;;----------------------------------
    (vm-imap-send-command 
     process (format "FETCH %s:%s (UID RFC822.SIZE FLAGS)" first last))
    (while need-ok
      (setq response (vm-imap-read-response-and-verify process "FLAGS FETCH"))
      (cond 
       ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	(setq p (cdr (nth 3 response)))
	(setq tok (nth 1 response))
	(goto-char (nth 1 tok))
	(setq msg-num (read imap-buffer))
	(while p
	  (cond 
	   ((vm-imap-response-matches p 'UID 'atom)
	    (setq tok (nth 1 p))
	    (setq uid (buffer-substring (nth 1 tok) (nth 2 tok)))
	    (setq p (nthcdr 2 p)))
	   ((vm-imap-response-matches p 'RFC822\.SIZE 'atom)
	    (setq tok (nth 1 p))
	    (setq size (buffer-substring (nth 1 tok) (nth 2 tok)))
	    (setq p (nthcdr 2 p)))
	   ((vm-imap-response-matches p  'FLAGS 'list)
	    (setq pl (cdr (nth 1 p))
		  flags nil)
	    (while pl
	      (setq tok (car pl))
	      (if (not (vm-imap-response-matches (list tok) 'atom))
		  (vm-imap-protocol-error
		   "expected atom in FLAGS list in FETCH response"))
	      (setq flag (downcase
			  (buffer-substring (nth 1 tok) (nth 2 tok)))
		    flags (cons flag flags)
		    pl (cdr pl)))
	    (setq p (nthcdr 2 p)))
	   (t
	    (vm-imap-protocol-error
	     "expected UID, RFC822.SIZE and (FLAGS list) in FETCH response"))
	   ))
	(setq list 
	      (cons (cons msg-num (cons uid (cons size flags)))
		    list)))
       ((vm-imap-response-matches response 'VM 'OK)
	(setq need-ok nil))))
    ;; returning nil means the fetch failed so return
    ;; something other than nil if there aren't any messages.
    (if (null list)
	(cons nil nil)
      list )))

(defun vm-imap-ask-about-large-message (process size n)
  (let ((work-buffer nil)
	(imap-buffer (current-buffer))
	(need-ok t)
	(need-header t)
	response fetch-response
	list p
	start end)
    (unwind-protect
	(save-excursion
	  ;;------------------------
	  (vm-buffer-type:duplicate)
	  ;;------------------------
	  (save-window-excursion
	    ;;----------------------------------
	    (vm-imap-session-type:assert 'valid)
	    ;;----------------------------------
	    (vm-imap-send-command process
				  (format "FETCH %d (RFC822.HEADER)" n))
	    (while need-ok
	      (setq response (vm-imap-read-response-and-verify process "header FETCH"))
	      (cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
		     (setq fetch-response response
			   need-header nil))
		    ((vm-imap-response-matches response 'VM 'OK)
		     (setq need-ok nil))))
	    (if need-header
		(vm-imap-protocol-error "FETCH OK sent before FETCH response"))
	    (setq vm-imap-read-point (point-marker))
	    (setq list (cdr (nth 3 fetch-response)))
	    (if (not (vm-imap-response-matches list 'RFC822\.HEADER 'string))
		(vm-imap-protocol-error
		 "expected (RFC822.HEADER string) in FETCH response"))
	    (setq p (nth 1 list)
		  start (nth 1 p)
		  end (nth 2 p))
	    (setq work-buffer (generate-new-buffer "*imap-glop*"))
	    ;;--------------------------
	    (vm-buffer-type:set 'scratch)
	    ;;--------------------------
	    (set-buffer work-buffer)
	    (insert-buffer-substring imap-buffer start end)
	    (vm-imap-cleanup-region (point-min) (point-max))
	    (vm-display-buffer work-buffer)
	    (setq minibuffer-scroll-window (selected-window))
	    (goto-char (point-min))
	    (if (re-search-forward "^Received:" nil t)
		(progn
		  (goto-char (match-beginning 0))
		  (vm-reorder-message-headers
		   nil vm-visible-headers
		   vm-invisible-header-regexp)))
	    (set-window-point (selected-window) (point))
	    ;;-------------------
	    (vm-buffer-type:exit)
	    ;;-------------------
	    (if (y-or-n-p (format "Retrieve message %d (size = %d)? " n size))
		'retrieve
	      (if (y-or-n-p (format "Delete message %d from maildrop? " n))
		  'delete
		'skip))))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-imap-retrieve-to-target (process target statblob bodypeek)
  ;; Read a mail message from PROCESS and store it in TARGET, which is
  ;; either a file or a buffer.  Report status using STATBLOB.  The
  ;; boolean BODYPEEK tells if the bodypeek function is available for
  ;; the IMAP server.
  (vm-assert (not (null vm-imap-read-point)))
  (let ((***start vm-imap-read-point)	; avoid dynamic binding of 'start'
	end fetch-response list p)
    (goto-char ***start)
    (vm-set-imap-stat-x-got statblob 0)
    (let* ((func
	    (function
	     (lambda (beg end len)
	       (if vm-imap-read-point
		   (progn
		     (vm-set-imap-stat-x-got statblob (- end ***start))
		     (if (zerop (% (random) 10))
			 (vm-imap-report-retrieval-status statblob)))))))
	   ;; this seems to slow things down
	   ;;(after-change-functions (cons func after-change-functions))
	   
	   (need-ok t)
	   response)
      (setq response (vm-imap-read-response-and-verify process "message FETCH"))
      (cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	     (setq fetch-response response))
	    (t
	     (vm-imap-protocol-error "Expected FETCH response not received"))))
      
    ;; must make the read point a marker so that it stays fixed
    ;; relative to the text when we modify things below.
    (setq vm-imap-read-point (point-marker))
    (setq list (cdr (nth 3 fetch-response)))
    (cond
     (bodypeek
      (if (not (vm-imap-response-matches list 'BODY '(vector) 'string))
	  (vm-imap-protocol-error
	   "expected (BODY[] string) in FETCH response"))
      (setq p (nth 2 list)
	    ***start (nth 1 p)))
     (t
      (if (not (vm-imap-response-matches list 'RFC822 'string))
	  (vm-imap-protocol-error
	   "expected (RFC822 string) in FETCH response"))
      (setq p (nth 1 list)
	    ***start (nth 1 p))))
    (goto-char (nth 2 p))
    (setq end (point-marker))
    (vm-set-imap-stat-x-need statblob nil)
    (vm-imap-cleanup-region ***start end)
    (vm-munge-message-separators vm-folder-type ***start end)
    (goto-char ***start)
    (vm-set-imap-stat-x-got statblob nil)
    ;; avoid the consing and stat() call for all but babyl
    ;; files, since this will probably slow things down.
    ;; only babyl files have the folder header, and we
    ;; should only insert it if the crash box is empty.
    (if (and (eq vm-folder-type 'babyl)
	     (cond ((stringp target)
		    (let ((attrs (file-attributes target)))
		      (or (null attrs) (equal 0 (nth 7 attrs)))))
		   ((bufferp target)
		    (save-excursion
		      ;;----------------------------
		      (vm-buffer-type:enter 'unknown)
		      ;;----------------------------
		      (set-buffer target)
		      ;;-------------------
		      (vm-buffer-type:exit)
		      ;;-------------------
		      (zerop (buffer-size))))))
	(let ((opoint (point)))
	  (vm-convert-folder-header nil vm-folder-type)
	  ;; if start is a marker, then it was moved
	  ;; forward by the insertion.  restore it.
	  (setq ***start opoint)
	  (goto-char ***start)
	  (vm-skip-past-folder-header)))
    (insert (vm-leading-message-separator))
    (save-restriction
      (narrow-to-region (point) end)
      (vm-convert-folder-type-headers 'baremessage vm-folder-type))
    (goto-char end)
    (insert-before-markers (vm-trailing-message-separator))
    ;; Some IMAP servers don't understand Sun's stupid
    ;; From_-with-Content-Length style folder and assume the last
    ;; newline in the message is a separator.  And so the server
    ;; strips it, leaving us with a message that does not end
    ;; with a newline.  Add the newline if needed.
    ;;
    ;; HP Openmail seems to have this problem.
    (if (and (not (eq ?\n (char-after (1- (point)))))
	     (memq vm-folder-type '(From_-with-Content-Length BellFrom_)))
	(insert-before-markers "\n"))
    (if (stringp target)
	;; Set file type to binary for DOS/Windows.  I don't know if
	;; this is correct to do or not; it depends on whether the
	;; the CRLF or the LF newline convention is used on the inbox
	;; associated with this crashbox.  This setting assumes the LF
	;; newline convention is used.
	(let ((buffer-file-type t)
	      (selective-display nil))
	  (write-region ***start end target t 0))
      (let ((b (current-buffer)))
	(save-excursion
	  ;;----------------------------
	  (vm-buffer-type:enter 'unknown)
	  ;;----------------------------
	  (set-buffer target)
	  (let ((buffer-read-only nil))
	    (insert-buffer-substring b ***start end))
	  ;;-------------------
	  (vm-buffer-type:exit)
	  ;;-------------------
	  )))
    (delete-region ***start end)
    t ))

(defsubst vm-imap-delete-message (process n)
  (vm-imap-delete-messages process n n))

(defun vm-imap-delete-messages (process beg end)
  ;;----------------------------------
  (vm-buffer-type:assert 'process)
  (vm-imap-session-type:assert 'valid)
  ;;----------------------------------
  (vm-imap-send-command process (format "STORE %d:%d +FLAGS.SILENT (\\Deleted)"
					beg end))
  (if (null (vm-imap-read-ok-response process))
      (vm-imap-protocol-error "STORE ... +FLAGS.SILENT (\\Deleted) failed")))

(defun vm-imap-get-message-size (process n)
  (let ((imap-buffer (current-buffer))
	tok size response p
	(need-size t)
	(need-ok t))
    ;;----------------------------------
    (vm-buffer-type:assert 'process)
    (vm-imap-session-type:assert 'valid)
    ;;----------------------------------
    (vm-imap-send-command process (format "FETCH %d:%d (RFC822.SIZE)" n n))
    (while need-ok
      (setq response (vm-imap-read-response-and-verify process "size FETCH"))
      (cond ((and need-size
		  (vm-imap-response-matches response '* 'atom 'FETCH 'list))
	     (setq need-size nil)
	     (setq p (cdr (nth 3 response)))
	     (if (not (vm-imap-response-matches p 'RFC822\.SIZE 'atom))
		 (vm-imap-protocol-error
		  "expected (RFC822.SIZE number) in FETCH response"))
	     (setq tok (nth 1 p))
	     (goto-char (nth 1 tok))
	     (setq size (read imap-buffer)))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))))
    size ))

(defun vm-imap-read-capability-response (process)
  ;;----------------------------------
  (vm-buffer-type:assert 'process)
  ;;----------------------------------
  (let (response r cap-list auth-list (need-ok t))
    (while need-ok
      (setq response (vm-imap-read-response-and-verify process "CAPABILITY"))
      (if (vm-imap-response-matches response 'VM 'OK)
	  (setq need-ok nil)
	(if (not (vm-imap-response-matches response '* 'CAPABILITY))
	    nil
	  ;; skip * CAPABILITY
	  (setq response (cdr (cdr response)))
	  (while response
	    (setq r (car response))
	    (if (not (eq (car r) 'atom))
		nil
	      (if (save-excursion
		    (goto-char (nth 1 r))
		    (let ((case-fold-search t))
		      (eq (re-search-forward "AUTH=." (nth 2 r) t)
			  (+ 6 (nth 1 r)))))
		  (progn
		    (setq auth-list (cons (intern
					   (upcase (buffer-substring
						    (+ 5 (nth 1 r))
						    (nth 2 r))))
					  auth-list)))
		(setq r (car response))
		(if (not (eq (car r) 'atom))
		    nil
		  (setq cap-list (cons (intern
					(upcase (buffer-substring
						 (nth 1 r) (nth 2 r))))
				       cap-list)))))
	    (setq response (cdr response))))))
    (if (or cap-list auth-list)
	(list (nreverse cap-list) (nreverse auth-list))
      nil)))

(defun vm-imap-read-greeting (process)
  ;;----------------------------------
  (vm-buffer-type:assert 'process)
  ;;----------------------------------
  (let (response)
    (setq response (vm-imap-read-response process))
    (cond ((vm-imap-response-matches response '* 'OK)
	   t )
	  ((vm-imap-response-matches response '* 'PREAUTH)
	   'preauth )
	  (t nil))))

(defun vm-imap-read-ok-response (process)
  ;;----------------------------------
  (vm-buffer-type:assert 'process)
  ;;----------------------------------
  (let (response retval (done nil))
    (while (not done)
      (setq response (vm-imap-read-response process))
      (cond ((vm-imap-response-matches response '*)
	     nil )
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq retval t done t))
	    (t (setq retval nil done t))))
    retval ))

(defun vm-imap-cleanup-region (start end)
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    ;; CRLF -> LF
    (while (and (< (point) end) (search-forward "\r\n"  end t))
      (replace-match "\n" t t)))
  (set-marker end nil))

(defun vm-imap-read-response (process)
  ;; Reads a line of respose from the imap PROCESS
  ;;--------------------------------------------
  ;; This assertion often fails for some reason,
  ;; perhaps some asynchrony involved?
  ;; Assertion check being disabled unless debugging is on.
  (if vm-buffer-type-debug
      (vm-buffer-type:assert 'process))
  (if vm-buffer-type-debug
      (setq vm-buffer-type-trail (cons 'read vm-buffer-type-trail)))
  ;;--------------------------------------------
  (let ((list nil) tail obj)
    (goto-char vm-imap-read-point)
    (while (not (eq (car (setq obj (vm-imap-read-object process)))
		    'end-of-line))
      (if (null list)
	  (setq list (cons obj nil)
		tail list)
	(setcdr tail (cons obj nil))
	(setq tail (cdr tail))))
    list ))

(defun vm-imap-read-response-and-verify (process &optional command-desc)
  ;; Reads a line of response from the imap PROCESS and checks for
  ;; standard errors like "BAD" and "BYE".  Optional COMMAND-DESC is a
  ;; command description that can be printed with the error message.
  ;;--------------------------------------------
  ;; This assertion often fails for some reason,
  ;; perhaps some asynchrony involved?
  ;; Assertion check being disabled unless debugging is on.
  (if vm-buffer-type-debug
      (vm-buffer-type:assert 'process))
  (if vm-buffer-type-debug
      (setq vm-buffer-type-trail (cons 'verify vm-buffer-type-trail)))
  ;;--------------------------------------------
  (let ((response (vm-imap-read-response process)))
    (if (vm-imap-response-matches response 'VM 'NO)
	(vm-imap-protocol-error
	 (format "server said NO to %s" (or command-desc "command"))))
    (if (vm-imap-response-matches response 'VM 'BAD)
	(vm-imap-protocol-error 
	 (format "server said BAD to %s" (or command-desc "command"))))
    (if (vm-imap-response-matches response '* 'BYE)
	(vm-imap-protocol-error
	 (format "server said BYE to %s" (or command-desc "command"))))
    response))

(defun vm-imap-read-object (process &optional skip-eol)
  ;;----------------------------------
  ;; Originally, this assertion failed often for some reason,
  ;; perhaps some asynchrony involved?
  ;; It has been mostly chased up by now. (Nov 2009)
  ;; Still assertion check being disabled unless debugging is on.
  (if vm-buffer-type-debug
      (vm-buffer-type:assert 'process))
  ;;----------------------------------
  (let ((done nil)
	opoint
	(token nil))
    (while (not done)
      (skip-chars-forward " \t")
      (cond ((< (- (point-max) (point)) 2)
	     (setq opoint (point))
	     (vm-imap-check-connection process)
	     (accept-process-output process)
	     (goto-char opoint))
	    ((looking-at "\r\n")
	     (forward-char 2)
	     (setq token '(end-of-line) done (not skip-eol)))
	    ((looking-at "\\[")
	     (forward-char 1)
	     (let* ((list (list 'vector))
		    (tail list)
		    obj)
	       (while (not (eq (car (setq obj (vm-imap-read-object process t)))
			       'close-bracket))
		 (if (eq (car obj) 'close-paren)
		     (vm-imap-protocol-error "unexpected )"))
		 (setcdr tail (cons obj nil))
		 (setq tail (cdr tail)))
	       (setq token list done t)))
	    ((looking-at "\\]")
	     (forward-char 1)
	     (setq token '(close-bracket) done t))
	    ((looking-at "(")
	     (forward-char 1)
	     (let* ((list (list 'list))
		    (tail list)
		    obj)
	       (while (not (eq (car (setq obj (vm-imap-read-object process t)))
			       'close-paren))
		 (if (eq (car obj) 'close-bracket)
		     (vm-imap-protocol-error "unexpected ]"))
		 (setcdr tail (cons obj nil))
		 (setq tail (cdr tail)))
	       (setq token list done t)))
	    ((looking-at ")")
	     (forward-char 1)
	     (setq token '(close-paren) done t))
	    ((looking-at "{")
	     (forward-char 1)
	     (let (start obj n-octets)
	       (setq obj (vm-imap-read-object process))
	       (if (not (eq (car obj) 'atom))
		   (vm-imap-protocol-error "number expected after {"))
	       (setq n-octets (string-to-number
			       (buffer-substring (nth 1 obj)
						 (nth 2 obj))))
	       (setq obj (vm-imap-read-object process))
	       (if (not (eq (car obj) 'close-brace))
		   (vm-imap-protocol-error "} expected"))
	       (setq obj (vm-imap-read-object process))
	       (if (not (eq (car obj) 'end-of-line))
		   (vm-imap-protocol-error "CRLF expected"))
	       (setq start (point))
	       (while (< (- (point-max) start) n-octets)
		 (vm-imap-check-connection process)
		 (accept-process-output process))
	       (goto-char (+ start n-octets))
	       (setq token (list 'string start (point))
		     done t)))
	    ((looking-at "}")
	     (forward-char 1)
	     (setq token '(close-brace) done t))
	    ((looking-at "\042") ;; double quote
	     (forward-char 1)
	     (let ((start (point))
		   (curpoint (point)))
	       (while (not done)
		 (skip-chars-forward "^\042")
		 (setq curpoint (point))
		 (if (looking-at "\042")
		     (progn
		       (setq done t)
		       (forward-char 1))
		   (vm-imap-check-connection process)
		   (accept-process-output process)
		   (goto-char curpoint))
	       (setq token (list 'string start curpoint)))))
	    ;; should be (looking-at "[\000-\040\177-\377]")
	    ;; but Microsoft Exchange emits 8-bit chars.
	    ((and (looking-at "[\000-\040\177]") 
		  (= vm-imap-tolerant-of-bad-imap 0))
	     (vm-imap-protocol-error "unexpected char (%d)"
				     (char-after (point))))
	    (t
	     (let ((start (point))
		   (curpoint (point))
		   ;; We should be considering 8-bit chars as
		   ;; non-word chars also but Microsoft Exchange
		   ;; uses them, despite the RFC 2060 prohibition.
		   ;; If we ever resume disallowing 8-bit chars,
		   ;; remember to write the range as \177-\376 ...
		   ;; \376 instead of \377 because Emacs 19.34 has
		   ;; a bug in the fastmap initialization code
		   ;; that causes it to infloop.
		   (not-word-chars "^\000-\040\177()[]{}")
		   (not-word-regexp "[][\000-\040\177(){}]"))
	       (while (not done)
		 (skip-chars-forward not-word-chars)
		 (setq curpoint (point))
		 (if (looking-at not-word-regexp)
		     (setq done t)
		   (vm-imap-check-connection process)
		   (accept-process-output process)
		   (goto-char curpoint))
		 (setq token (list 'atom start curpoint)))))))
    (setq vm-imap-read-point (point))
    token ))

(defun vm-imap-response-matches (response &rest expr)
  (let ((case-fold-search t) e r)
    (catch 'done
      (while (and expr response)
	(setq e (car expr)
	      r (car response))
	(cond ((stringp e)
	       (if (or (not (eq (car r) 'string))
		       (save-excursion
			 (goto-char (nth 1 r))
			 (not (eq (search-forward e (nth 2 r) t) (nth 2 r)))))
		   (throw 'done nil)))
	      ((numberp e)
	       (if (or (not (eq (car r) 'atom))
		       (save-excursion
			 (goto-char (nth 1 r))
			 (not (eq (search-forward (int-to-string e)
						  (nth 2 r) t)
				  (nth 2 r)))))
		   (throw 'done nil)))
	      ((consp e)
	       (if (not (eq (car e) (car r)))
		   (throw 'done nil))
	       (apply 'vm-imap-response-matches (cdr r) (cdr e)))
	      ((eq e 'atom)
	       (if (not (eq (car r) 'atom))
		   (throw 'done nil)))
	      ((eq e 'vector)
	       (if (not (eq (car r) 'vector))
		   (throw 'done nil)))
	      ((eq e 'list)
	       (if (not (eq (car r) 'list))
		   (throw 'done nil)))
	      ((eq e 'string)
	       (if (not (eq (car r) 'string))
		   (throw 'done nil)))
	      ;; this must to come after all the comparisons for
	      ;; specific symbols.
	      ((symbolp e)
	       (if (or (not (eq (car r) 'atom))
		       (save-excursion
			 (goto-char (nth 1 r))
			 (not (eq (search-forward (symbol-name e) (nth 2 r) t)
				  (nth 2 r)))))
		   (throw 'done nil))))
	(setq response (cdr response)
	      expr (cdr expr)))
      t )))

(defun vm-imap-bail-if-server-says-farewell (response)
  (if (vm-imap-response-matches response '* 'BYE)
      (throw 'end-of-session t)))

(defun vm-imap-protocol-error (&rest args)
  (set (make-local-variable 'vm-imap-keep-trace-buffer) t)
  (signal 'vm-imap-protocol-error (list (apply 'format args))))

(defun vm-imap-scan-list-for-flag (list flag)
  (setq list (cdr list))
  (let ((case-fold-search t) e)
    (catch 'done
      (while list
	(setq e (car list))
	(if (not (eq (car e) 'atom))
	    nil
	  (goto-char (nth 1 e))
	  (if (eq (search-forward flag (nth 2 e) t) (nth 2 e))
	      (throw 'done t)))
	(setq list (cdr list)))
      nil )))

;; like Lisp get but for IMAP property lists like those returned by FETCH.
(defun vm-imap-plist-get (list name)
  (setq list (cdr list))
  (let ((case-fold-search t) e)
    (catch 'done
      (while list
	(setq e (car list))
	(if (not (eq (car e) 'atom))
	    nil
	  (goto-char (nth 1 e))
	  (if (eq (search-forward name (nth 2 e) t) (nth 2 e))
	      (throw 'done (car (cdr list)))))
	(setq list (cdr (cdr list))))
      nil )))

(defun vm-imap-quote-string (string)
  (vm-with-string-as-temp-buffer string 'vm-imap-quote-buffer))

(defun vm-imap-quote-buffer ()
  (goto-char (point-min))
  (insert "\"")
  (while (re-search-forward "[\"\\]" nil t)
    (forward-char -1)
    (insert "\\")
    (forward-char 1))
  (goto-char (point-max))
  (insert "\""))

(defun vm-re-establish-folder-imap-session (&optional interactive)
  (let ((process (vm-folder-imap-process)))
    (if (and (processp process)
	     (memq (process-status process) '(open run)))
	process
      (vm-establish-new-folder-imap-session interactive))))

;; Kill and restart the IMAP session for the current folder.  This is
;; necessary because we might unexpected EXPUNGE responses which we
;; don't know how to deal with.

(defun vm-establish-new-folder-imap-session (&optional interactive)
  (let ((process (vm-folder-imap-process))
	mailbox select mailbox-count uid-validity permanent-flags
	read-write can-delete body-peek
	(vm-imap-ok-to-ask interactive))
    (if (processp process)
	(vm-imap-end-session process))
    (setq process (vm-imap-make-session (vm-folder-imap-maildrop-spec)))
    (when (processp process)
      (vm-set-folder-imap-process process)
      (setq mailbox (vm-imap-parse-spec-to-list (vm-folder-imap-maildrop-spec))
	    mailbox (nth 3 mailbox))
      (save-excursion
	;;----------------------------
	(vm-buffer-type:enter 'process)
	;;----------------------------
	(set-buffer (process-buffer process))
	(setq select (vm-imap-select-mailbox process mailbox))
	(setq mailbox-count (nth 0 select)
	      uid-validity (nth 1 select)
	      read-write (nth 2 select)
	      can-delete (nth 3 select)
	      permanent-flags (nth 4 select)
	      body-peek (vm-imap-capability 'IMAP4REV1))
	;;---------------------------------
	(vm-imap-session-type:set 'active)
	(vm-buffer-type:exit)
	;;---------------------------------
	)
      (vm-set-folder-imap-uid-validity uid-validity) ; unique per session
      (vm-set-folder-imap-mailbox-count mailbox-count)
      (vm-set-folder-imap-read-write read-write)
      (vm-set-folder-imap-can-delete can-delete)
      (vm-set-folder-imap-body-peek body-peek)
      (vm-set-folder-imap-permanent-flags permanent-flags)
      (vm-imap-dump-uid-and-flags-data)
      process )))

(defun vm-imap-retrieve-uid-and-flags-data ()
  ;;------------------------------
  (if vm-buffer-type-debug
      (setq vm-buffer-type-trail (cons 'uid-and-flags-data 
				       vm-buffer-type-trail)))
  (vm-buffer-type:assert 'folder)
  ;;------------------------------
  (if (vm-folder-imap-uid-list)
      nil ; don't retrieve twice
    (let ((there (make-vector 67 0))
	  (flags (make-vector 67 0))
	  (process (vm-folder-imap-process))
	  (mailbox-count (vm-folder-imap-mailbox-count))
	  list tuples tuple uid)
      (save-excursion
	;;----------------------------
	(vm-buffer-type:enter 'process)
	;;----------------------------
	(set-buffer (process-buffer process))
	(if (eq mailbox-count 0)
	    (setq list nil)
	  (setq list (vm-imap-get-message-data-list process 1 mailbox-count)))
	(setq tuples list)
	(while tuples
	  (setq tuple (car tuples))
	  (set (intern (cadr tuple) there) (car tuple))
	  (set (intern (cadr tuple) flags) (nthcdr 2 tuple))
	  (setq tuples (cdr tuples)))
	;;-------------------------------
	(vm-imap-session-type:set 'valid)
	(vm-buffer-type:exit)
	;;-------------------------------
	)
      (vm-set-folder-imap-uid-list list)
      (vm-set-folder-imap-uid-obarray there)
      (vm-set-folder-imap-flags-obarray flags))))

(defun vm-imap-dump-uid-and-flags-data ()
  (when (and vm-folder-access-data
             (eq (car vm-buffer-types) 'folder))
             
    ;;------------------------------
    (vm-buffer-type:assert 'folder)
    ;;------------------------------
    (vm-set-folder-imap-uid-list nil)
    (vm-set-folder-imap-uid-obarray nil)
    (vm-set-folder-imap-flags-obarray nil)
    (if (processp (vm-folder-imap-process))
	(save-excursion
	  (set-buffer (process-buffer (vm-folder-imap-process)))
	  ;;---------------------------------
	  (vm-imap-session-type:set 'active)
	  ;;---------------------------------
	  ))
    ))

;; This function is now obsolete.  It is faster to get flags of
;; several messages at once, using vm-imap-get-message-data-list

(defun vm-imap-get-message-flags (process m &optional norecord)
  ;; gives an error if the message has an invalid uid
  (let (need-ok p r flag response saw-Seen)
    (if (not (equal (vm-imap-uid-validity-of m)
		    (vm-folder-imap-uid-validity)))
	(vm-imap-protocol-error "message has invalid uid"))
    (save-excursion
      ;;----------------------------------
      (vm-buffer-type:enter 'process)
      (vm-imap-session-type:assert-active)
      ;;----------------------------------
      (set-buffer (process-buffer process))
      (vm-imap-send-command process
			    (format "UID FETCH %s (FLAGS)"
				    (vm-imap-uid-of m)))
      ;;--------------------------------
      (vm-imap-session-type:set 'active)
      ;;--------------------------------
      (setq need-ok t)
      (while need-ok
	(setq response (vm-imap-read-response-and-verify process "UID FETCH (FLAGS)"))
	(cond ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil))
	      ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	       (setq r (nthcdr 3 response)
		     r (car r)
		     r (vm-imap-plist-get r "FLAGS")
		     r (cdr r))
	       (while r
		 (setq p (car r))
		 (if (not (eq (car p) 'atom))
		     nil
		   (setq flag (downcase (buffer-substring (nth 1 p) (nth 2 p))))
		   (cond ((string= flag "\\answered")
			  (vm-set-replied-flag m t norecord))
			 ((string= flag "\\deleted")
			  (vm-set-deleted-flag m t norecord))
			 ((string= flag "\\seen")
			  (vm-set-unread-flag m nil norecord)
			  (vm-set-new-flag m nil norecord)
			  (setq saw-Seen t))
			 ((string= flag "\\recent")
			  (vm-set-new-flag m t norecord))))
		 (setq r (cdr r)))
	       (if (not saw-Seen)
		   (vm-set-unread-flag m t norecord)))))
      ;;-------------------
      (vm-buffer-type:exit)
      ;;-------------------
      )))

(defun vm-imap-update-message-flags (m flags &optional norecord)
  ;; Update the flags of the message M in the folder to FLAGS.
  ;; Optional argument NORECORD says whether this fact should not be
  ;; recorded in the undo stack.
  (let (flag saw-Seen saw-Deleted saw-Flagged seen-labels labels)
    (while flags
      (setq flag (car flags))
      (cond ((string= flag "\\answered")
	     (if (null (vm-replied-flag m))
		 (vm-set-replied-flag m t norecord)))

	    ((string= flag "\\deleted")
	     (if (null (vm-deleted-flag m))
		 (vm-set-deleted-flag m t norecord))
	     (setq saw-Deleted t))

	    ((string= flag "\\seen")
	     (if (vm-unread-flag m)
		 (vm-set-unread-flag m nil norecord))
	     (if (vm-new-flag m)
		 (vm-set-new-flag m nil norecord))
	     (setq saw-Seen t))

	    ((string= flag "\\recent")
	     (if (null (vm-new-flag m))
		 (vm-set-new-flag m t norecord)))

	    ((string= flag "forwarded")
	     (if (null (vm-forwarded-flag m))
		 (vm-set-forwarded-flag m t norecord)))

	    ((string= flag "redistributed")
	     (if (null (vm-redistributed-flag m))
		 (vm-set-redistributed-flag m t norecord)))

	    ((string= flag "filed")
	     (if (null (vm-filed-flag m))
		 (vm-set-filed-flag m t norecord)))

	    ((string= flag "written")
	     (if (null (vm-written-flag m))
		 (vm-set-written-flag m t norecord)))

	    (t				; all other flags including \flagged
	     (setq seen-labels (cons flag seen-labels)))
	     )
      (setq flags (cdr flags)))

    (if (not saw-Seen)
	(if (null (vm-unread-flag m))
	    (vm-set-unread-flag m t norecord)))
    (if (not saw-Deleted)
	(if (vm-deleted-flag m)
	    (vm-set-deleted-flag m nil norecord)))
    (setq labels (sort (vm-labels-of m) 'string-lessp))
    (setq seen-labels (sort seen-labels 'string-lessp))
    (if (equal labels seen-labels)
	t
      (vm-set-labels-of m seen-labels)
      (vm-set-label-string-of m nil)
      (vm-mark-for-summary-update m)
      (vm-set-stuff-flag-of m t))
    ))

(defun vm-imap-save-message-flags (process m &optional by-uid)
  ;; Saves the message flags of a message on the IMAP server, adding
  ;; or deleting flags on the servers as necessary.  Irreversible
  ;; flags, however, are not deleted.
  ;; Optional argument BY-UID says that the save messages should be
  ;; issued by UID, not message sequence number.

  ;; Comment by USR
  ;; According to RFC 2060, it is not an error to store flags that
  ;; are not listed in PERMANENTFLAGS.  Removed unnecessary checks to
  ;; this effect.

  ;;-----------------------------------------------------
  (vm-buffer-type:assert 'folder)
  (or by-uid (vm-imap-folder-session-type:assert 'valid))
  ;;-----------------------------------------------------
  (if (not (equal (vm-imap-uid-validity-of m)
		  (vm-folder-imap-uid-validity)))
      (vm-imap-protocol-error "message has invalid uid"))
  (let* ((uid (vm-imap-uid-of m))
	 (uid-key1 (intern uid (vm-folder-imap-uid-obarray)))
	 (uid-key2 (intern-soft uid (vm-folder-imap-flags-obarray)))
	 (message-num (and (boundp uid-key1) (symbol-value uid-key1)))
	 (server-flags (and (boundp uid-key2) (symbol-value uid-key2)))
					; leave uid as the dummy header
	 (labels (vm-labels-of m))
	 need-ok flags+ flags- response)
    (when message-num
      ;; Reversible flags are treated the same as labels
      (if (not (vm-unread-flag m))
	  (setq labels (cons "\\seen" labels)))
      (if (vm-deleted-flag m)
	  (setq labels (cons "\\deleted" labels)))
      ;; Irreversible flags
      (if (and (vm-replied-flag m) 
	       (not (member "\\answered" server-flags)))
	  (setq flags+ (cons (intern "\\Answered") flags+)))
      (if (and (vm-filed-flag m) (not (member "filed" server-flags)))
	  (setq flags+ (cons 'filed flags+)))
      (if (and (vm-written-flag m) 
	       (not (member "written" server-flags)))
	  (setq flags+ (cons 'written flags+)))
      (if (and (vm-forwarded-flag m)
	       (not (member "forwarded" server-flags)))
	  (setq flags+ (cons 'forwarded flags+)))
      (if (and (vm-redistributed-flag m)
	       (not (member "redistributed" server-flags)))
	  (setq flags+ (cons 'redistributed flags+)))
      (mapcar (lambda (flag) (delete flag server-flags))
	      '("\\answered" "filed" "written" "forwarded" "redistributed"))
      ;; Make a copy of labels for side effects
      (setq labels (cons nil (copy-sequence labels)))
      ;; Ignore labels that are both in vm and the server
      (delete-common-elements labels server-flags 'string<)
      ;; Ignore reversible flags that we have locally reversed -- Why?
      ;; (mapcar (lambda (flag) (delete flag server-flags))
      ;;  '("\\seen" "\\deleted" "\\flagged"))
      ;; Flags to be added to the server
      (setq flags+ (append (mapcar 'intern (cdr labels)) flags+))
      ;; Flags to be deleted from the server
      (setq flags- (append (mapcar 'intern (cdr server-flags)) flags-))

      (save-excursion
	(set-buffer (process-buffer process))
	;;----------------------------------
	(vm-buffer-type:enter 'process)
	;;----------------------------------
	(when flags+
	  (vm-imap-send-command 
	   process
	   (format "%sSTORE %s +FLAGS.SILENT %s" 
		   (if by-uid "UID " "")
		   (if by-uid uid message-num)
		   flags+))
	  (setq need-ok t)
	  (while need-ok
	    (setq response 
		  (vm-imap-read-response-and-verify 
		   process "STORE +FLAGS.SILENT"))
	    (cond ((vm-imap-response-matches response 'VM 'OK)
		   (setq need-ok nil)))))

	(when flags-
	  (vm-imap-send-command 
	   process
	   (format "%sSTORE %s -FLAGS.SILENT %s"
		   (if by-uid "UID " "")
		   (if by-uid uid message-num)
		   flags-))
	  (setq need-ok t)
	  (while need-ok
	    (setq response 
		  (vm-imap-read-response-and-verify 
		   process "STORE -FLAGS.SILENT"))
	    (cond ((vm-imap-response-matches response 'VM 'OK)
		   (setq need-ok nil)))))

	(vm-set-attribute-modflag-of m nil)
	;;-------------------
	(vm-buffer-type:exit)
	;;-------------------
	))))

(defvar vm-imap-subst-char-in-string-buffer
  (get-buffer-create " *subst-char-in-string*"))

(defun vm-imap-subst-CRLF-for-LF (string)
  (with-current-buffer vm-imap-subst-char-in-string-buffer
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n" nil t))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun vm-imap-save-message (process m mailbox)
  "Using the IMAP process PROCESS, save the message M to IMAP mailbox
MAILBOX." 
  (let (need-ok need-plus flags response string)
    ;; save the message's flag along with it.
    ;; don't save the deleted flag.
    (if (vm-replied-flag m)
	(setq flags (cons (intern "\\Answered") flags)))
    (if (not (vm-unread-flag m))
	(setq flags (cons (intern "\\Seen") flags)))
    (save-excursion
      ;;----------------------------
      (vm-buffer-type:enter 'folder)
      ;;----------------------------
      (set-buffer (vm-buffer-of m))
      (save-restriction
	(widen)
	(setq string (buffer-substring (vm-headers-of m) (vm-text-end-of m))
              string (vm-imap-subst-CRLF-for-LF string)))
      ;;-------------------
      (vm-buffer-type:exit)
      ;;-------------------
      )
    (save-excursion
      ;;----------------------------
      (vm-buffer-type:enter 'process)
      ;;----------------------------
      (set-buffer (process-buffer process))
      (condition-case nil
	  (vm-imap-create-mailbox process mailbox)
	(vm-imap-protocol-error 
	 (vm-buffer-type:set 'process)))
      ;;----------------------------------
      (vm-imap-session-type:assert-active)
      ;;----------------------------------
      (vm-imap-send-command process
			    (format "APPEND %s %s {%d}"
				    (vm-imap-quote-string mailbox)
				    (if flags flags "()")
				    (length string)))
      ;;--------------------------------
      (vm-imap-session-type:set 'active)
      ;;--------------------------------
      (setq need-plus t)
      (while need-plus
	(setq response (vm-imap-read-response-and-verify process "APPEND"))
	(cond ((vm-imap-response-matches response '+)
	       (setq need-plus nil))))
      (vm-imap-send-command process string nil t)
      (setq need-ok t)
      (while need-ok
	(setq response (vm-imap-read-response-and-verify process "APPEND data"))
	(cond ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil))))
      ;;-------------------
      (vm-buffer-type:exit)
      ;;-------------------
      )))

;; Incomplete -- Yet to be finished.  USR
;; creation of new mailboxes has to be straightened out

(defun vm-imap-copy-message (process m mailbox)
  "Use IMAP session PROCESS to copy message M to MAILBOX.  The PROCESS
is expected to have logged in and selected the current folder.

This is similar to vm-imap-save-message but uses the internal copy
operation of the server to minimize I/O."
  ;;-----------------------------
  (vm-buffer-type:set 'folder)
  ;;-----------------------------
  (let ((uid (vm-imap-uid-of m))
	(uid-validity (vm-imap-uid-validity-of m))
	need-ok response string)
    (if (not (equal uid-validity (vm-folder-imap-uid-validity)))
	(error "Message does not have a valid UID"))
    (save-excursion
      ;;------------------------
      (vm-buffer-type:duplicate)
      ;;------------------------
      (if (vm-attribute-modflag-of m)
	  (condition-case nil
	      (progn
		(if (null (vm-folder-imap-flags-obarray))
		    (vm-imap-retrieve-uid-and-flags-data))
		(vm-imap-save-message-flags process m 'by-uid))
	    (vm-imap-protocol-error nil)))
;;       (condition-case nil
;; 	    (vm-imap-create-mailbox process mailbox)
;; 	  (vm-imap-protocol-error nil))

      (set-buffer (process-buffer process))
      ;;-----------------------------------------
      (vm-buffer-type:set 'process)
      (vm-imap-session-type:assert-active)
      ;;-----------------------------------------
      (vm-imap-send-command 
       process
       (format "UID COPY %s %s"
	       (vm-imap-uid-of m)
	       (vm-imap-quote-string mailbox)))
      ;;--------------------------------
      (vm-imap-session-type:set 'active)
      ;;--------------------------------
      (setq need-ok t)
      (while need-ok
	(setq response 
	      (vm-imap-read-response-and-verify process "UID COPY"))
	(cond ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil))))
      ;;-------------------
      (vm-buffer-type:exit)
      ;;-------------------
      )))

;; ------------------------------------------------------------------------
;; 
;; interactive commands:
;; vm-create-imap-folder: string -> void
;; vm-delete-imap-folder: string -> void
;; vm-rename-imap-folder: string & string -> void
;; 
;; top-level operations
;; vm-fetch-imap-message: (vm-message) -> void
;; vm-imap-synchronize-folder:
;;	(&optional interactive & bool & bool & bool & bool) -> void
;; vm-imap-save-attributes: (&optional interactive) -> void
;; vm-imap-folder-check-for-mail: (&optional interactive) -> ?
;;
;; vm-imap-get-synchronization-data: (&optional bool) -> 
;;		(retrieve-list: (uid . int) list &
;;		 expunge-list: vm-message list & 
;;		 stale-list: vm-message list)
;;
;; ------------------------------------------------------------------------



(defun vm-imap-get-synchronization-data (do-retrieves)
  ;; Compares the UID's of messages in the local cache and the IMAP
  ;; server.  Returns a list containing:
  ;; RETRIEVE-LIST: A list of pairs consisting of UID's and message
  ;; sequence numbers of the messages that are not present in the
  ;; local cache and, hence, need to be retrieved.
  ;; EXPUNGE-LIST: A list of message descriptors for messages in the
  ;; local cache which are not present on the server and, hence, need
  ;; to expunged locally.
  ;; STALE-LIST: A list of message descriptors for messages in the
  ;; local cache whose uidvalidity values are stale.
  ;; If the argument DO-RETRIEVES is 'full, then all the messages that
  ;; are not presently in cache are retrieved.  Otherwise, the
  ;; messages previously retrieved are ignored.

  ;; Comments by USR
  ;; - Originally, messages with stale UIDVALIDITY values were
  ;; ignored.  So, they would never get expunged from the cache.  The
  ;; STALE-LIST component was added to fix this.
  
  ;;-----------------------------
  (if vm-buffer-type-debug
      (setq vm-buffer-type-trail (cons 'synchronization-data
				       vm-buffer-type-trail)))
  (vm-buffer-type:assert 'folder)
  ;;-----------------------------
  (let ((here (make-vector 67 0))	; OBARRAY(uid, vm-message)
	there flags
	(uid-validity (vm-folder-imap-uid-validity))
	(do-full-retrieve (eq do-retrieves 'full))
	retrieve-list expunge-list stale-list uid
	mp)
    (vm-imap-retrieve-uid-and-flags-data)
    (setq there (vm-folder-imap-uid-obarray))
    ;; Figure out stale uidvalidity values and messages to be expunged
    ;; in the cache.
    (setq mp vm-message-list)
    (while mp
      (cond ((not (equal (vm-imap-uid-validity-of (car mp)) uid-validity))
	     (setq stale-list (cons (car mp) stale-list)))
	    ((member "stale" (vm-labels-of (car mp)))
	     nil)
	    (t
	     (setq uid (vm-imap-uid-of (car mp)))
	     (set (intern uid here) (car mp))
	     (if (not (boundp (intern uid there)))
		 (setq expunge-list (cons (car mp) expunge-list)))))
      (setq mp (cdr mp)))
    ;; Figure out messages that need to be retrieved
    (mapatoms (function
	       (lambda (sym)
		 (if (and (not (boundp (intern (symbol-name sym) here)))
			  (or do-full-retrieve
			      (not (assoc (symbol-name sym)
					  vm-imap-retrieved-messages))))
		     ;; don't retrieve messages that have been
		     ;; retrieved previously
		     ;; This is bad because if a message got lost
		     ;; somehow, it won't be retrieved!  USR
		     (setq retrieve-list (cons
					  (cons (symbol-name sym)
						(symbol-value sym))
					  retrieve-list)))))
	      there)
    (setq retrieve-list 
	  (sort retrieve-list 
		(lambda (**pair1 **pair2)
		  (< (cdr **pair1) (cdr **pair2)))))	  
    (list retrieve-list expunge-list stale-list)))

;;;###autoload
(defun vm-imap-synchronize-folder (&optional interactive
					     do-remote-expunges
					     do-local-expunges
					     do-retrieves
					     save-attributes
					     retrieve-attributes)
  "* Synchronize IMAP folder with the server.
   INTERACTIVE, true if the function was invoked interactively, e.g., as
   vm-get-spooled-mail.
   DO-REMOTE-EXPUNGES indicates whether the server mail box should be
   expunged.
   DO-LOCAL-EXPUNGES indicates whether the cache buffer should be
   expunged.
   DO-RETRIEVES indicates if new messages that are not already in the
   cache should be retrieved from the server.  If this flag is 'full
   then messages previously retrieved but not in cache are retrieved
   as well.
   SAVE-ATTRIBUTES indicates if the message attributes should be updated on
   the server.  If it is 'all, then the attributes of all messages are
   updated irrespective of whether they were modified or not.
   RETRIEVE-ATTRIBTUES indicates if the message attributes on the server
   should be retrieved, updating the cache.
"
  ;; -- Comments by USR
  ;; Not clear why do-local-expunges and do-remote-expunges should be
  ;; separate.  It doesn't make sense to do one but not the other!

  ;;--------------------------
  (if vm-buffer-type-debug
      (setq vm-buffer-type-trail (cons 'synchronize vm-buffer-type-trail)))
  (vm-buffer-type:set 'folder)
  ;;--------------------------
  (if (and do-retrieves vm-block-new-mail)
      (error "Can't get new mail until you save this folder."))
  (if (or vm-global-block-new-mail
	  (null (vm-establish-new-folder-imap-session interactive)))
      nil
    (if do-retrieves
	(vm-assimilate-new-messages))	; Funny that this should be
					; necessary.  Indicates bugs?
    (message "Logging into the IMAP server...")
    (let* ((sync-data (vm-imap-get-synchronization-data do-retrieves))
	   (retrieve-list (nth 0 sync-data))
	   (expunge-list (nth 1 sync-data))
	   (stale-list (nth 2 sync-data))
	   (flags (vm-folder-imap-flags-obarray))
	   (process (vm-folder-imap-process))
	   (n 1)
	   (statblob nil) (m nil) (mflags nil)
	   (uid nil)
	   (uid-validity (vm-folder-imap-uid-validity))
	   (imapdrop (vm-folder-imap-maildrop-spec))
	   (safe-imapdrop (vm-safe-imapdrop-string imapdrop))
	   (use-body-peek (vm-folder-imap-body-peek))
	   r-list range k mp got-some message-size old-eob
	   (folder-buffer (current-buffer)))
      (when save-attributes
	(let ((mp vm-message-list))
	  ;;  (perm-flags (vm-folder-imap-permanent-flags))
	  (message "Updating attributes on the IMAP server... ")
	  (while mp
	    (if (or (eq save-attributes 'all)
		    (vm-attribute-modflag-of (car mp)))
		(condition-case nil
		    (vm-imap-save-message-flags process (car mp))
		  (vm-imap-protocol-error 
		   (vm-buffer-type:set 'folder))))
	    (setq mp (cdr mp)))
	  (message "Updating attributes on the IMAP server... done")))
      (when retrieve-attributes
	(let ((mp vm-message-list)
	      (len (length vm-message-list))
	      (n 0))
	  (message "Retrieving message attributes and labels... ")
	  (while mp
	    (setq m (car mp))
	    (setq uid (vm-imap-uid-of m))
	    (if (and (equal (vm-imap-uid-validity-of m) uid-validity)
		     (boundp (intern uid flags))
		     (setq mflags (cdr (symbol-value (intern uid flags)))))
		(vm-imap-update-message-flags m mflags t))
	    ;; 	      (message "Retrieving message attributes and labels... %d%%" 
	    ;; 		       (* (/ (+ n 0.0) len) 100))
	    (setq mp (cdr mp)
		  n (1+ n)))
	  (message "Retrieving message atrributes and labels... done")
	  ))
      (when (and do-retrieves retrieve-list)
	(save-excursion
	  (message "Retrieving new messages... ")
	  (vm-save-restriction
	   (widen)
	   (setq old-eob (point-max))
	   (goto-char (point-max))
	   (unwind-protect
	       (condition-case error-data
		   (save-excursion
		     ;;----------------------------
		     (vm-buffer-type:enter 'process)
		     ;;----------------------------
		     (set-buffer (process-buffer process))
		     (setq statblob (vm-imap-start-status-timer))
		     (vm-set-imap-stat-x-box statblob safe-imapdrop)
		     (vm-set-imap-stat-x-maxmsg statblob
						(length retrieve-list))
		     (setq r-list (vm-imap-bunch-messages 
				   (mapcar (function cdr) retrieve-list)))
		     (while r-list
		       (setq range (car r-list))
		       (vm-set-imap-stat-x-currmsg statblob n)
		       (setq message-size 
			     (vm-imap-get-message-size
			      process (car range))) ; sloppy
		       (vm-set-imap-stat-x-need statblob message-size)
		       ;;----------------------------------
		       (vm-imap-session-type:assert 'valid)
		       ;;----------------------------------
		       (vm-imap-fetch-messages process (car range) (cdr range)
					       use-body-peek vm-load-headers-only)
		       (setq k (1+ (- (cdr range) (car range))))
		       (while (> k 0)
			 (vm-imap-retrieve-to-target process folder-buffer
						     statblob use-body-peek)
			 (setq k (1- k)))
		       (vm-imap-read-ok-response process)
		       (setq r-list (cdr r-list)
			     n (+ n (1+ (- (cdr range) (car range)))))))
		 (vm-imap-protocol-error
		  (message "Retrieval from %s signaled: %s" safe-imapdrop
			   error-data))
		 ;; Continue with whatever messages have been read
		 (quit
		  (delete-region old-eob (point-max))
		  (error (format "Quit received during retrieval from %s"
				 safe-imapdrop))))
	     ;; cleanup
	     (when statblob 
	       (vm-imap-stop-status-timer statblob))	   
	     )
	   ;;-------------------
	   (vm-buffer-type:exit)
	   ;;-------------------
	   ;; to make the "Mail" indicator go away
	   (setq vm-spooled-mail-waiting nil)
	   (intern (buffer-name) vm-buffers-needing-display-update)
	   (message "Updating summary... ")
	   (vm-update-summary-and-mode-line)
	   (setq mp (vm-assimilate-new-messages t))
	   (setq got-some mp)
           (if got-some
               (vm-increment vm-modification-counter))
           (setq r-list retrieve-list)
	   (while mp
	     ;; headers-only loading is still experimental. USR, 2010-01-12
	     (if vm-load-headers-only 
		 (vm-set-body-to-be-retrieved (car mp) t))
	     (setq uid (car (car r-list)))
	     (vm-set-imap-uid-of (car mp) uid)
	     (vm-set-imap-uid-validity-of (car mp) uid-validity)
	     (vm-set-byte-count-of 
	      (car mp) (car (symbol-value (intern uid flags))))
	     (vm-imap-update-message-flags 
	      (car mp) (cdr (symbol-value (intern uid flags))) t)
	     (setq mp (cdr mp)
		   r-list (cdr r-list)))
	   )))

      (when do-local-expunges
	(message "Expunging messages in cache... ")
	(vm-expunge-folder t t expunge-list)
	(if (and interactive stale-list)
	    (if (y-or-n-p 
		 (format 
		  "Found %s messages with invalid UIDs.  Expunge them? "
		  (length stale-list)))
		(vm-expunge-folder t t stale-list)
	      (message "They will be labelled 'stale'")
	      (mapcar 
	       (lambda (m)
		 (vm-set-labels m (cons "stale" (vm-labels-of m)))
		 (vm-set-attribute-modflag-of m t)
		 (vm-set-stuff-flag-of m t))
	       stale-list)
	      ))
	(message "Expunging messages in cache... done"))
      (when (and do-remote-expunges
		 vm-imap-messages-to-expunge)
	;; New code.  Kyle's version was piggybacking on IMAP spool
	;; file code and wasn't ideal.
	(save-excursion
	  ;;-----------------------------
	  (vm-buffer-type:duplicate)
	  ;;-----------------------------
	  (message "Expunging messages on the server... ")
	  (condition-case error-data
	      (let ((mailbox-count (vm-folder-imap-mailbox-count))
		    (expunge-count (length vm-imap-messages-to-expunge))
		    (uid-obarray (vm-folder-imap-uid-obarray))
		    uids-to-delete m-list message e-list count)
		;; uids-to-delete to have UID's of all UID-valid messages in
		;; vm-imap-messages-to-expunge 
		(while vm-imap-messages-to-expunge
		  (setq message (car vm-imap-messages-to-expunge))
		  (if (equal (cdr message) uid-validity)
		      (setq uids-to-delete (cons (car message) uids-to-delete)))
		  (setq vm-imap-messages-to-expunge 
			(cdr vm-imap-messages-to-expunge)))
		(if (not (equal expunge-count (length uids-to-delete)))
		    (progn
		      (message "%s stale deleted messages are ignored"
			       (- expunge-count (length uids-to-delete)))
		      (sit-for 2)))

		;;---------------------------
		(vm-buffer-type:set 'process)
		;;---------------------------
		(set-buffer (process-buffer process))
		;; (setq uid-alist (vm-imap-get-uid-list 
		;;		 process 1 mailbox-count))
		;; m-list to have the message sequence numbers of
		;; messages to be expunged, in descending order.
		;; the message sequence numbers don't change in the
		;; process, according to the IMAP4 protocol
		(setq m-list 
		      (delete nil
			      (mapcar 
			       (lambda (uid)
				 (let* ((key (intern uid uid-obarray)))
				   (and (boundp key)
					(progn
					  (vm-imap-delete-message 
					   process (symbol-value key))
					  (symbol-value key)))))
			       uids-to-delete)))
		(setq m-list (cons nil (sort m-list '>)))
					; dummy header added
		(setq count 0)
		(while (and (cdr m-list) (<= count vm-imap-expunge-retries))
		  ;;----------------------------------
		  (vm-imap-session-type:assert-active)
		  ;;----------------------------------
		  (vm-imap-send-command process "EXPUNGE")
		  ;;--------------------------------
		  (vm-imap-session-type:set 'active)
		  ;;--------------------------------
		  ;; e-list to have the message sequence numbers of
		  ;; messages that got expunged
		  (setq e-list (sort 
				(vm-imap-read-expunge-response
				 process)
				'>))
		  (while e-list		; for each message expunged
		    (let ((e (car e-list))
			  (pair m-list)
			  (done nil))
		      (while (not done)	; remove it from m-list
			(cond ((null (cdr pair))
			       (setq done t))
			      ((> (car (cdr pair)) e) 
					; decrement the message sequence
					; numbers following e in m-list
			       (rplaca (cdr pair) 
				       (- (car (cdr pair)) 1)))
			      ((= (car (cdr pair)) e)
			       (rplacd pair (cdr (cdr pair)))
			       (setq done t))
			      ((< (car (cdr pair)) e)
					; oops. somebody expunged e!?!
			       (setq done t)))
			(setq pair (cdr pair)))
		      (setq e-list (cdr e-list))))
		  ;; m-list has message sequence numbers of messages
		  ;; that haven't yet been expunged
		  (if (cdr m-list)
		      (message "%s messages yet to be expunged"
			       (length (cdr m-list))))
					; try again, if the user wants us to
		  (setq count (1+ count)))
		(message "Expunging messages on the server... done"))
	    (vm-imap-protocol-error 
	     (message "Expunge from %s signalled: %s"
		      safe-imapdrop error-data))
	    (quit 
	     (error "Quit received during expunge from %s"
		    safe-imapdrop)))
	  ;;-------------------
	  (vm-buffer-type:exit)
	  ;;-------------------
	  )
	(vm-imap-dump-uid-and-flags-data))
      got-some)))

(defvar vm-imap-message-bunch-size 10
  "* Number of messages in a bunch to be used for IMAP server
operations")

(defun vm-imap-bunch-messages (seq-nums)
  ;; Given a sorted list of message sequence numbers, creates a list
  ;; of bunched message sequences, each of the form 
  ;; (begin-num . end-num)
  (let ((seqs nil)
	beg last next diff)
    (when seq-nums
      (setq beg (car seq-nums))
      (setq last beg)
      (setq seq-nums (cdr seq-nums)))
    (while seq-nums
      (setq next (car seq-nums))
      (if (and (= (- next last) 1)
	       (< (- next beg) vm-imap-message-bunch-size))
	  (setq last next)
	(setq seqs (cons (cons beg last) seqs))
	(setq beg next)
	(setq last next))
      (setq seq-nums (cdr seq-nums)))
    (setq seqs (cons (cons beg last) seqs))
    (nreverse seqs)))

(defun vm-fetch-imap-message (m)
  "Insert the message body of M in the current buffer."
  (let ((body-buffer (current-buffer)))
    (save-excursion
      ;;----------------------------------
      (vm-buffer-type:enter 'folder)
      ;;----------------------------------
      (set-buffer (vm-buffer-of (vm-real-message-of m)))
      (let* ((statblob nil)
	     (uid (vm-imap-uid-of m))
	     (imapdrop (vm-folder-imap-maildrop-spec))
	     (safe-imapdrop (vm-safe-imapdrop-string imapdrop))
	     (process (vm-re-establish-folder-imap-session imapdrop))
	     (use-body-peek (vm-folder-imap-body-peek))
	     (server-uid-validity (vm-folder-imap-uid-validity))
	     (uid-key1 (intern-soft uid (vm-folder-imap-uid-obarray)))
	     (uid-key2 (intern-soft uid (vm-folder-imap-flags-obarray)))
	     (old-eob (point-max))
	     message-num message-size
	     )
	(when (null uid-key1)
	  (vm-imap-retrieve-uid-and-flags-data)
	  (setq uid-key1 (intern-soft uid (vm-folder-imap-uid-obarray)))
	  (setq uid-key2 (intern-soft uid (vm-folder-imap-flags-obarray))))
	(setq message-num (symbol-value uid-key1))
	(setq message-size (string-to-number (car (symbol-value uid-key2))))

	(message "Retrieving message body... ")
	(condition-case error-data
	    (save-excursion
	      (set-buffer (process-buffer process))
	      ;;----------------------------------
	      (vm-buffer-type:enter 'process)
	      (vm-imap-session-type:assert 'valid)
	      ;;----------------------------------
	      (setq statblob (vm-imap-start-status-timer))
	      (vm-set-imap-stat-x-box statblob safe-imapdrop)
	      (vm-set-imap-stat-x-maxmsg statblob 1)
	      (vm-set-imap-stat-x-currmsg statblob message-num)
	      ;; (setq message-size (vm-imap-get-message-size process message-num))
	      (vm-set-imap-stat-x-need statblob message-size)
	      (vm-imap-fetch-message process message-num use-body-peek nil)
	      (vm-imap-retrieve-to-target process body-buffer statblob
				     use-body-peek)
	      (vm-imap-read-ok-response process)
	      ;;-------------------
	      (vm-buffer-type:exit)
	      ;;-------------------
	      )
	  (vm-imap-protocol-error
	   ;;-------------------
	   (vm-buffer-type:exit)
	   ;;-------------------
	   (message "Retrieval from %s signaled: %s" safe-imapdrop
		    error-data)
	   ;; Continue with whatever messages have been read
	   )
	  (quit
	   ;;-------------------
	   (vm-buffer-type:exit)
	   ;;-------------------
	   (delete-region old-eob (point-max))
	   (error (format "Quit received during retrieval from %s"
			  safe-imapdrop))))
	(message "Retrieving message body... done")
	)
      ;;-------------------
      (vm-buffer-type:exit)
      ;;-------------------
      )))

;;;###autoload
(defun vm-load-message (&optional count)
  "Load the message by retrieving its body from its
permanent location.  Currently this facility is only available for IMAP
folders.

With a prefix argument COUNT, the current message and the next 
COUNT - 1 messages are loaded.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
loaded.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only marked messages are loaded, other messages are ignored."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(mlist (vm-select-marked-or-prefixed-messages count))
	(buffer-read-only nil)
	(inhibit-read-only t)
	(buffer-undo-list t)
	(text-begin nil)
	(text-end nil)
	m mm)
;;     (if (not used-marks) 
;; 	(setq mlist (list (car vm-message-pointer))))
    (save-excursion
      (while mlist
	(setq m (car mlist))
	(setq mm (vm-real-message-of m))
	(set-buffer (vm-buffer-of mm))
	(if (not (eq vm-folder-access-method 'imap))
	    (error "This is currently available only for imap folders."))
	(vm-save-restriction
	 (widen)
	 (setq text-begin (marker-position (vm-text-of mm)))
	 (setq text-end (marker-position (vm-text-end-of mm)))
	 (narrow-to-region (marker-position (vm-headers-of mm)) text-end)
	 (goto-char text-begin)
	 (delete-region (point) (point-max))
	 (apply (intern (format "vm-fetch-%s-message" "imap"))
		mm nil)
	 ;; delete the new headers
	 (delete-region text-begin
			(or (re-search-forward "\n\n" (point-max) t)
			    (point-max)))
	 ;; fix markers now
	 ;; FIXME the text-end is guessed
	 (set-marker (vm-text-of mm) text-begin)
	 (set-marker (vm-text-end-of mm) 
		     (save-excursion
		       (goto-char (point-max))
		       (end-of-line 0)	; move back one line
		       (kill-line 1)
		       (point)))
	 (goto-char text-begin)
	 ;; now care for the layout of the message
	 (vm-set-mime-layout-of mm (vm-mime-parse-entity-safe mm))
	 (vm-set-body-to-be-retrieved mm nil)
	 (setq mlist (cdr mlist)))))				
    ))

;;;###autoload
(defun vm-refresh-message (&optional count)
  "This is an alias for vm-load-message."
  (interactive "p")
  (call-interactively (function vm-load-message)))

;;;###autoload
(defun vm-unload-message (&optional count)
  "Unload the message body, i.e., delete it from the folder
buffer.  It can be retrieved again in future from its permanent
external location.  Currently this facility is only available for
IMAP folders.

With a prefix argument COUNT, the current message and the next 
COUNT - 1 messages are unloaded.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
unloaded.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only marked messages are unloaded, other messages are ignored."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(mlist (vm-select-marked-or-prefixed-messages count))
	(buffer-read-only nil)
	(inhibit-read-only t)
	(buffer-undo-list t)
	(text-begin nil)
	(text-end nil)
	m mm)
    ;;     (if (not used-marks) 
    ;; 	(setq mlist (list (car vm-message-pointer))))
    (save-excursion
      (while mlist
	(setq m (car mlist))
	(setq mm (vm-real-message-of m))
	(set-buffer (vm-buffer-of mm))
	(if (not (eq vm-folder-access-method 'imap))
	    (error "This is currently available only for imap folders."))
	(vm-save-restriction
	 (widen)
	 (setq text-begin (marker-position (vm-text-of mm)))
	 (setq text-end (marker-position (vm-text-end-of mm)))
	 (goto-char text-begin)
	 (delete-region (point) text-end)
	 (vm-set-mime-layout-of mm nil)
	 (vm-set-body-to-be-retrieved mm t)
	 (setq mlist (cdr mlist)))))				
    ))



(defun vm-imap-save-attributes (&optional interactive all-flags)
  "* Save the attributes of changed messages to the IMAP folder.
   INTERACTIVE, true if the function was invoked interactively, e.g., as
   vm-get-spooled-mail.
   ALL-FLAGS, if true says that the attributes of all messages should
   be saved to the IMAP folder, not only those of changed messages.
"
  ;;--------------------------
  (vm-buffer-type:set 'folder)
  ;;--------------------------
  (let* ((process (vm-folder-imap-process))
	 (uid-validity (vm-folder-imap-uid-validity))
	 (mp vm-message-list))
      ;;  (perm-flags (vm-folder-imap-permanent-flags))
      (message "Updating attributes on the IMAP server... ")
      ;;-----------------------------------------
      (vm-imap-folder-session-type:assert 'valid)
      ;;-----------------------------------------
      (while mp
	(if (or all-flags (vm-attribute-modflag-of (car mp)))
	    (condition-case nil
		(vm-imap-save-message-flags process (car mp))
	      (vm-imap-protocol-error 
	       (vm-buffer-type:set 'folder))))
	(setq mp (cdr mp)))
      (message "Updating attributes on the IMAP server... done")))


(defun vm-imap-synchronize (&optional all-flags)
  "Synchronize the current folder with the IMAP mailbox.
Deleted messages are not expunged.
Changes made to the buffer are uploaded to the server first before
downloading the server data.
Prefix argument ALL-FLAGS says that all the messages' flags should be
written to the server irrespective of whether they were changed in the
VM session.  This is useful for saving offline work."
  (interactive "P")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-display nil nil '(vm-imap-synchronize) '(vm-imap-synchronize))
  (if (not (eq vm-folder-access-method 'imap))
      (message "This is not an IMAP folder")
    (if (null (vm-establish-new-folder-imap-session t))
	nil

      (vm-imap-retrieve-uid-and-flags-data)
      (vm-imap-save-attributes all-flags)
      ;; (vm-imap-synchronize-folder t nil nil nil 
      ;; 			(if all-flags 'all t) nil)
					; save-attributes
      (vm-imap-synchronize-folder t t t 'full nil t)
					; do-remote-expunges, 
					; do-local-expunges,
					; do-retrieves and
					; retrieve-attributes 
      ;; stuff the attributes of messages that need it.
      ;; (message "Stuffing attributes...")
      ;; (vm-stuff-folder-attributes nil)
      ;; (message "Stuffing attributes... done")
      ;; stuff bookmark and header variable values
      (if vm-message-list
	  (progn
	    ;; get summary cache up-to-date
	    (message "Updating summary... ")
	    (vm-update-summary-and-mode-line)
	    (message "Updating summary... done")
	    ;; 	  (vm-stuff-bookmark)
	    ;; 	  (vm-stuff-pop-retrieved)
	    ;; 	  (vm-stuff-imap-retrieved)
	    ;; 	  (vm-stuff-last-modified)
	    ;; 	  (vm-stuff-header-variables)
	    ;; 	  (vm-stuff-labels)
	    ;; 	  (vm-stuff-summary)
	    ;; 	  (and vm-message-order-changed
	    ;; 	       (vm-stuff-message-order))
	    )))))
  

;;;###autoload
(defun vm-imap-folder-check-for-mail (&optional interactive)
  "Check if there is new mail in th current IMAP folder.  The optional
argument INTERACTIVE says if the function is being invoked
interactively."
  ;;--------------------------
  (vm-buffer-type:set 'folder)
  ;;--------------------------
  (if (or vm-global-block-new-mail
	  (null (vm-establish-new-folder-imap-session interactive)))
      nil
    (let ((result (car (vm-imap-get-synchronization-data))))
      (vm-imap-end-session (vm-folder-imap-process))
      result )))

;; ----------- missing functions-----------
;;;###autoload
(defun vm-imap-find-name-for-spec (spec)
  "This is a stub for a function that has not been defined."
  (error "vm-imap-find-name-for-spec has not been defined.  Please report it."
	 ))
;;-----------------------------------------

;;;###autoload
(defun vm-imap-find-spec-for-buffer (buffer)
  "Find the IMAP maildrop spec for the folder BUFFER."
  (save-excursion
    (set-buffer buffer)
    (vm-folder-imap-maildrop-spec)))
;;   (let ((list (mapcar 'car vm-imap-account-alist))
;; 	(done nil)
;; 	(spec-items nil))
;;     (while (and (not done) list)
;;       (setq spec-items (vm-imap-parse-spec-to-list (car list)))
;;       (setcar (nthcdr 3 spec-items) folder)
;;       (if (eq buffer (vm-get-file-buffer 
;; 		      (vm-imap-make-filename-for-spec
;; 		       (mapconcat 'identity spec-items ":"))))
;; 	  (setq done t)
;; 	(setq list (cdr list))))
;;     (and list (car list)))

;;;###autoload
(defun vm-imap-make-filename-for-spec (spec)
  "Returns a cache file name appropriate for the IMAP maildrop
specification SPEC."
  (let (md5)
    (setq spec (vm-imap-normalize-spec spec))
    (setq md5 (vm-md5-string spec))
    (expand-file-name (concat "imap-cache-" md5)
		      (or vm-imap-folder-cache-directory
			  vm-folder-directory
			  (getenv "HOME")))))

(defun vm-imap-normalize-spec (spec)
  (let (list)
    (setq list (vm-imap-parse-spec-to-list spec))
    (setcar (vm-last list) "*")		; scrub password
    (setcar list "imap")		; standardise protocol name
    (setcar (nthcdr 2 list) "*")	; scrub portnumber
    (setcar (nthcdr 4 list) "*")	; scrub authentication method
    (setq spec (mapconcat (function identity) list ":"))
    spec ))

;;;###autoload
(defun vm-imap-parse-spec-to-list (spec)
  "Parses the IMAP maildrop specification SPEC and returns a list of
its components."
  (vm-parse spec "\\([^:]+\\):?" 1 6))

(defun vm-imap-spec-list-to-host-alist (spec-list)
  (let (host-alist spec host)
    (while spec-list
      (setq spec (vm-imapdrop-sans-password-and-mailbox (car spec-list)))
      (setq host-alist (cons
			(list
			 (nth 1 (vm-imap-parse-spec-to-list spec))
			 spec)
			host-alist)
	    spec-list (cdr spec-list)))
    host-alist ))

(defvar vm-imap-account-folder-cache nil
  "Caches the list of all folders on an account.")

(defun vm-imap-folder-completion-list (string predicate flag)
  ;; selectable-only is used via dynamic binding
  (let ((completion-list (mapcar (lambda (a) (list (concat (cadr a) ":")))
				 vm-imap-account-alist))
	folder account spec process mailbox-list)
    
    ;; check for account 
    (setq folder (try-completion (or string "") completion-list predicate))
    
    ;; get folders of this account
    (if (stringp folder)
	(setq account (car (vm-parse folder "\\([^:]+\\):?" 1)))
      (setq account (car (vm-parse string "\\([^:]+\\):?" 1))))
    
    (when account
      (setq mailbox-list (cdr (assoc account vm-imap-account-folder-cache)))
      (setq spec (car (rassoc (list account) vm-imap-account-alist)))
      (when (and (null mailbox-list) spec)
	(setq process (vm-imap-make-session spec))
	(when process
	  (setq mailbox-list (vm-imap-mailbox-list process selectable-only))
	  (vm-imap-end-session process)
	  (when mailbox-list
	    (add-to-list 'vm-imap-account-folder-cache 
			 (cons account mailbox-list)))))
      (setq completion-list 
	    (mapcar '(lambda (m) (list (format "%s:%s" account m))) mailbox-list))
      (setq folder (try-completion (or string "") completion-list predicate)))
    
    (setq folder (or folder ""))
    (if (eq folder t)
	(setq folder string))
    (cond ((null flag)
	   folder)
	  ((or (eq t flag) (string= " " folder))
	   (mapcar 'car completion-list))
	  ((eq 'lambda flag)
	   (try-completion folder completion-list predicate)))))

;;;###autoload
(defun vm-read-imap-folder-name (prompt &optional selectable-only
					newone default) 
  "Read an IMAP folder name in the format account:mailbox, return an
IMAP mailbox spec." 
  (let* (folder-input completion-list spec process list 
	 default-account default-folder
	 (vm-imap-ok-to-ask t)
	 (account-list (mapcar 'cadr vm-imap-account-alist))
	 account-and-folder account folder mailbox-list)
    (if (null account-list)
	(error "No known IMAP accounts.  Please set vm-imap-account-alist."))
    (if default 
	(setq list (vm-imap-parse-spec-to-list default)
	      default-account 
	      (cadr (assoc (vm-imapdrop-sans-password-and-mailbox default)
			   vm-imap-account-alist))
	      default-folder (nth 3 list))
      (setq default-account vm-last-visit-imap-account))
    (setq folder-input
	  (completing-read
	   (format			; prompt
;;	    "IMAP folder:%s " 
	    "%s%s" prompt
	    (if (and default-account default-folder)
		(format "(default %s:%s) " default-account default-folder)
	      ""))
	   'vm-imap-folder-completion-list
	   nil				; predicate
	   nil				; require-match
	   (if default-account		; initial-input
	       (format "%s:" default-account)
	     "")))
    (if (or (equal folder-input "")  
	    (equal folder-input (format "%s:" default-account)))
	(if (and default-account default-folder)
	    (setq folder-input (format "%s:%s" default-account default-folder))
	  (error 
	   "IMAP folder required in the format account-name:folder-name"))) 
    (setq account-and-folder (vm-parse folder-input "\\([^:]+\\):?" 1 2)
	  account (car account-and-folder)
	  folder (cadr account-and-folder)
	  spec (car (rassoc (list account) vm-imap-account-alist)))
    (if (null folder)
	(error 
	 "IMAP folder required in the format account-name:folder-name"))
    (if (null spec)
	(error "Unknown IMAP account-name:folder-name"))
    (setq list (vm-imap-parse-spec-to-list spec))
    (setcar (nthcdr 3 list) folder)
    (setq vm-last-visit-imap-account account)
    (mapconcat 'identity list ":")))

(defun vm-imap-directory-separator (process ref)
  (let ((c-list nil)
	sep p r response need-ok)
    (vm-imap-check-connection process)
    (save-excursion
      (set-buffer (process-buffer process))
      ;;----------------------------------
      (vm-buffer-type:enter 'process)
      (vm-imap-session-type:assert-active)
      ;;----------------------------------
      (vm-imap-send-command process (format "LIST %s \"\""
					    (vm-imap-quote-string ref)))
      ;;--------------------------------
      (vm-imap-session-type:set 'active)
      ;;--------------------------------
      (vm-imap-dump-uid-and-flags-data)
      (setq need-ok t)
      (while need-ok
	(setq response (vm-imap-read-response-and-verify process "LIST"))
	(cond ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil))
	      ((vm-imap-response-matches response '* 'LIST 'list 'string)
	       (setq r (nthcdr 3 response)
		     p (car r)
		     sep (buffer-substring (nth 1 p) (nth 2 p))))
	      ((vm-imap-response-matches response '* 'LIST 'list)
	       (vm-imap-protocol-error "unexpedcted LIST response"))))
      ;;-------------------
      (vm-buffer-type:exit)
      ;;-------------------
      sep )))

(defun vm-imap-mailbox-list (process selectable-only)
  "Query the IMAP PROCESS to get a list of the mailboxes (folders)
available in the IMAP account.  SELECTABLE-ONLY flag asks only
selectable mailboxes to be listed.  Returns a list of mailbox names."
  (let ((c-list nil)
	p r response need-ok)
    (vm-imap-check-connection process)
    (save-excursion
      (set-buffer (process-buffer process))
      ;;----------------------------------
      (vm-buffer-type:enter 'process)
      (vm-imap-session-type:assert-active)
      ;;----------------------------------
      (vm-imap-send-command process "LIST \"\" \"*\"")
      (vm-imap-dump-uid-and-flags-data)
      (setq need-ok t)
      (while need-ok
	(setq response (vm-imap-read-response-and-verify process "LIST"))
	(cond ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil))
	      ((vm-imap-response-matches response '* 'LIST 'list)
	       (setq r (nthcdr 2 response)
		     p (car r))
	       (if (and selectable-only
			(vm-imap-scan-list-for-flag p "\\Noselect"))
		   nil
		 (setq r (nthcdr 4 response)
		       p (car r))
		 (if (memq (car p) '(atom string))
		     (setq c-list (cons (buffer-substring (nth 1 p) (nth 2 p))
					c-list)))))))
      ;;-------------------
      (vm-buffer-type:exit)
      ;;-------------------
      c-list )))

;; This is unfinished
(defun vm-imap-mailbox-p (process mailbox selectable-only)
  "Query the IMAP PROCESS to check if MAILBOX exists as a folder.
SELECTABLE-ONLY flag asks whether the mailbox is selectable as
well. Returns a boolean value."
  (let ((c-list nil)
	p r response need-ok)
    (vm-imap-check-connection process)
    (save-excursion
      (set-buffer (process-buffer process))
      ;;----------------------------------
      (vm-buffer-type:enter 'process)
      (vm-imap-session-type:assert-active)
      ;;----------------------------------
      (vm-imap-send-command process (concat "LIST \"\" \"" mailbox "\""))
      (vm-imap-dump-uid-and-flags-data)
      (setq need-ok t)
      (while need-ok
	(setq response (vm-imap-read-response-and-verify process "LIST"))
	(cond ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil))
	      ((vm-imap-response-matches response '* 'LIST 'list)
	       (setq r (nthcdr 2 response)
		     p (car r))
	       (if (and selectable-only
			(vm-imap-scan-list-for-flag p "\\Noselect"))
		   nil
		 (setq r (nthcdr 4 response)
		       p (car r))
		 (if (memq (car p) '(atom string))
		     (setq c-list (cons (buffer-substring (nth 1 p) (nth 2 p))
					c-list)))))))
      ;;-------------------
      (vm-buffer-type:exit)
      ;;-------------------
      c-list )))

(defun vm-imap-read-boolean-response (process)
  (let ((need-ok t) retval response)
    (while need-ok
      (vm-imap-check-connection process)
      (setq response (vm-imap-read-response process))
      (cond ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil retval t))
	    ((vm-imap-response-matches response 'VM 'NO)
	     (setq need-ok nil retval nil))
	    ((vm-imap-response-matches response '* 'BYE)
	     (error "server said BYE"))
	    ((vm-imap-response-matches response 'VM 'BAD)
	     (vm-imap-protocol-error "server said BAD"))))
    retval ))

(defun vm-imap-create-mailbox (process mailbox
			       &optional dont-create-parent-directories)
  (if (not dont-create-parent-directories)
      (let (dir sep sep-regexp i)
	(setq sep (vm-imap-directory-separator process "")
	      sep-regexp (regexp-quote sep)
	      i 0)
	(while (string-match sep-regexp mailbox i)
	  (setq dir (substring mailbox i (match-end 0)))
	  (vm-imap-create-mailbox process dir t)
	  ;; ignore command result since creating a directory will
	  ;; routinely fail with "File exists".  We'll generate a
	  ;; real error if the final mailbox creation fails.
	  (vm-imap-read-boolean-response process)
	  (setq i (match-end 0)))))
  (vm-imap-send-command process (format "CREATE %s"
					(vm-imap-quote-string mailbox)))
  (if (null (vm-imap-read-boolean-response process))
      (vm-imap-protocol-error "IMAP CREATE of %s failed" mailbox)))

(defun vm-imap-delete-mailbox (process mailbox)
  (vm-imap-send-command process (format "DELETE %s"
					(vm-imap-quote-string mailbox)))
  (if (null (vm-imap-read-boolean-response process))
      (vm-imap-protocol-error "IMAP DELETE of %s failed" mailbox)))

(defun vm-imap-rename-mailbox (process source dest)
  (vm-imap-send-command process (format "RENAME %s %s"
					(vm-imap-quote-string source)
					(vm-imap-quote-string dest)))
  (if (null (vm-imap-read-boolean-response process))
      (vm-imap-protocol-error "IMAP RENAME of %s to %s failed" source dest)))

;;;###autoload
(defun vm-create-imap-folder (folder)
  "Create a folder on an IMAP server.
First argument FOLDER is read from the minibuffer if called
interactively.  Non-interactive callers must provide an IMAP
maildrop specification for the folder as described in the
documentation for `vm-spool-files'."
  (interactive
   (save-excursion
     ;;------------------------
     (vm-buffer-type:duplicate)
     ;;------------------------
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((this-command this-command)
	   (last-command last-command)
	   (folder (vm-read-imap-folder-name "Create IMAP folder: " nil t)))
       ;;-------------------
       (vm-buffer-type:exit)
       ;;-------------------
       (list folder))
     ))
  (let ((vm-imap-ok-to-ask t)
	process mailbox)
    (save-excursion
      (setq process (vm-imap-make-session folder))
      (if (null process)
	  (error "Couldn't open IMAP session for %s"
		 (vm-safe-imapdrop-string folder)))
      ;;-----------------------------
      (vm-buffer-type:enter 'process)
      ;;-----------------------------
      (set-buffer (process-buffer process))
      (setq mailbox (nth 3 (vm-imap-parse-spec-to-list folder)))
      (vm-imap-create-mailbox process mailbox t)
      (message "Folder %s created" (vm-safe-imapdrop-string folder))
      ;;-------------------
      (vm-buffer-type:exit)
      ;;-------------------
      (when (and (processp process)
		 (memq (process-status process) '(open run)))
	(vm-imap-end-session process)))
    ))

;;;###autoload
(defun vm-delete-imap-folder (folder)
  "Delete a folder on an IMAP server.
First argument FOLDER is read from the minibuffer if called
interactively.  Non-interactive callers must provide an IMAP
maildrop specification for the folder as described in the
documentation for `vm-spool-files'."
  (interactive
   (save-excursion
     ;;------------------------
     (vm-buffer-type:duplicate)
     ;;------------------------
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((this-command this-command)
	   (last-command last-command))
       (list (vm-read-imap-folder-name "Delete IMAP folder: " nil nil)))))
  (let ((vm-imap-ok-to-ask t)
	process mailbox)
    (setq process (vm-imap-make-session folder))
    (if (null process)
	(error "Couldn't open IMAP session for %s"
	       (vm-safe-imapdrop-string folder)))
    (save-excursion
      ;;-----------------------------
      (vm-buffer-type:enter 'process)
      ;;-----------------------------
      (set-buffer (process-buffer process))
      (setq mailbox (nth 3 (vm-imap-parse-spec-to-list folder)))
      (vm-imap-delete-mailbox process mailbox)
      (message "Folder %s deleted" (vm-safe-imapdrop-string folder))
      ;;-------------------
      (vm-buffer-type:exit)
      ;;-------------------
      (when (and (processp process)
		 (memq (process-status process) '(open run)))
	(vm-imap-end-session process))
      )))

;;;###autoload
(defun vm-rename-imap-folder (source dest)
  "Rename a folder on an IMAP server.
Argument SOURCE and DEST are read from the minibuffer if called
interactively.  Non-interactive callers must provide full IMAP
maildrop specifications for SOURCE and DEST as described in the
documentation for `vm-spool-files'."
  (interactive
   (save-excursion
     ;;------------------------
     (vm-buffer-type:duplicate)
     ;;------------------------
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((this-command this-command)
	   (last-command last-command)
	   source dest)
       (setq source (vm-read-imap-folder-name "Rename IMAP folder: " t nil))
       (setq dest (vm-read-imap-folder-name
		   (format "Rename %s to: " (vm-safe-imapdrop-string source))
		    nil t))
       (list source dest))))
  (let ((vm-imap-ok-to-ask t)
	process mailbox-source mailbox-dest)
    (setq process (vm-imap-make-session source))
    (if (null process)
	(error "Couldn't open IMAP session for %s"
	       (vm-safe-imapdrop-string source)))
    (save-excursion
      ;;-----------------------------
      (vm-buffer-type:enter 'process)
      ;;-----------------------------
      (set-buffer (process-buffer process))
      (setq mailbox-source (nth 3 (vm-imap-parse-spec-to-list source)))
      (setq mailbox-dest (nth 3 (vm-imap-parse-spec-to-list dest)))
      (vm-imap-rename-mailbox process mailbox-source mailbox-dest)
      (message "Folder %s renamed to %s" (vm-safe-imapdrop-string source)
	       (vm-safe-imapdrop-string dest))
      ;;-------------------
      (vm-buffer-type:exit)
      ;;-------------------
      (when (and (processp process)
		 (memq (process-status process) '(open run)))
	(vm-imap-end-session process))
      )))


;;; Robert Fenk's draft function for saving messages to IMAP folders.

;;;###autoload
(defun vm-imap-save-composition ()
  "Saves the current composition in the IMAP folder given by the
IMAP-FCC header. 
Add this to your `mail-send-hook' and start composing from an IMAP
folder." 
  (let ((mailbox (vm-mail-get-header-contents "IMAP-FCC:"))
	(mailboxes nil)
	(fcc-string (vm-mail-get-header-contents "FCC:" ","))
	fcc-list fcc maildrop spec-list 
	process flags response string m
	(vm-imap-ok-to-ask t))
    (if (null mailbox)
	(setq mailboxes nil)
      (save-excursion
	;;----------------------------
	(vm-buffer-type:enter 'folder)
	;;----------------------------
        (vm-select-folder-buffer)
	(setq m (car vm-message-pointer))
	(if m 
	    (set-buffer (vm-buffer-of (vm-real-message-of m))))
	(if (not (eq vm-folder-access-method 'imap))
	    (error "Cannot do IMAP-FCC because the parent folder is not an IMAP folder"))
	(vm-establish-new-folder-imap-session)
	(vm-imap-dump-uid-and-flags-data)
	(setq process (vm-folder-imap-process))
	(setq mailboxes (list (cons mailbox process)))
	;;-------------------
	(vm-buffer-type:exit)
	;;-------------------
	)
      (vm-mail-mode-remove-header "IMAP-FCC:")
      )

    (when fcc-string
      (setq fcc-list (vm-parse fcc-string "\\([^,]+\\),?"))
      (while fcc-list
	(setq fcc (car fcc-list))
	(setq spec-list (vm-parse fcc "\\([^:]+\\):?"))
	(when (member (car spec-list) '("imap" "imap-ssl" "imap-ssh"))
	  (setq process (vm-imap-make-session fcc))
	  (setq mailboxes (cons (cons (nth 3 spec-list) process) 
				mailboxes)))
	(setq fcc-list (cdr fcc-list))))
    
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (setq string (concat (buffer-substring (point-min) (match-beginning 0))
			 (buffer-substring
			  (match-end 0) (point-max))))
    
    (while mailboxes
      (setq mailbox (car (car mailboxes)))
      (setq process (cdr (car mailboxes)))
      (unwind-protect
	  (save-excursion
	    ;;-----------------------------
	    (vm-buffer-type:enter 'process)
	    ;;-----------------------------
	    ;; this can go awry if the process has died...
	    (set-buffer (process-buffer process))
	    (condition-case nil
		(vm-imap-create-mailbox process mailbox)
	      (vm-imap-protocol-error 
	       (vm-buffer-type:set 'process)))
	    ;;----------------------------------
	    (vm-imap-session-type:assert-active)
	    ;;----------------------------------

	    (vm-imap-send-command process
				  (format "APPEND %s %s {%d}"
					  (vm-imap-quote-string mailbox)
					  (if flags flags "()")
					  (length string)))
	    ;; could these be done with vm-imap-read-boolean-response?
	    (let ((need-plus t) response)
	      (while need-plus
		(setq response (vm-imap-read-response process))
		(cond ((vm-imap-response-matches response 'VM 'NO)
		       (vm-imap-protocol-error
			"server said NO to APPEND command"))
		      ((vm-imap-response-matches response 'VM 'BAD)
		       (vm-imap-protocol-error 
			"server said BAD to APPEND command"))
		      ((vm-imap-response-matches response '* 'BYE)
		       (vm-imap-protocol-error 
			"server said BYE to APPEND command"))
		      ((vm-imap-response-matches response '+)
		       (setq need-plus nil)))))

	    (vm-imap-send-command process string nil t)
	    (let ((need-ok t) response)
	      (while need-ok

		(setq response (vm-imap-read-response process))
		(cond ((vm-imap-response-matches response 'VM 'NO)
		       (vm-imap-protocol-error "server said NO to APPEND data"))
		      ((vm-imap-response-matches response 'VM 'BAD)
		       (vm-imap-protocol-error "server said BAD to APPEND data"))
		      ((vm-imap-response-matches response '* 'BYE)
		       (vm-imap-protocol-error "server said BYE to APPEND data"))
		      ((vm-imap-response-matches response 'VM 'OK)
		       (setq need-ok nil)))))
	    ;;-------------------
	    (vm-buffer-type:exit)
	    ;;-------------------
	    )
	(when (and (processp process)
		   (memq (process-status process) '(open run)))
	  (vm-imap-end-session process)))
      (setq mailboxes (cdr mailboxes)))
    ))

(defun vm-imap-start-bug-report ()
  "Begin to compose a bug report for IMAP support functionality."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (setq vm-kept-imap-buffers nil)
  (setq vm-imap-keep-trace-buffer t)
  (setq vm-imap-keep-failed-trace-buffers 20))

(defun vm-imap-submit-bug-report ()
  "Submit a bug report for VM's IMAP support functionality.  
It is necessary to run vm-imap-start-bug-report before the problem
occurrence and this command after the problem occurrence, in
order to capture the trace of IMAP sessions during the occurrence."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (if (or vm-imap-keep-trace-buffer
	  (y-or-n-p "Did you run vm-imap-start-bug-report earlier? "))
      (message "Thank you. Preparing the bug report... ")
    (message "Consider running vm-imap-start-bug-report before the problem occurrence"))
  (let ((process (vm-folder-imap-process)))
    (if process
	(vm-imap-end-session (vm-folder-imap-process))))
  (let ((trace-buffer-hook
	 '(lambda ()
	    (let ((bufs vm-kept-imap-buffers) 
		  buf)
	      (insert "\n\n")
	      (insert "IMAP Trace buffers - most recent first\n\n")
	      (while bufs
		(setq buf (car bufs))
		(insert "----") 
		(insert (format "%s" buf))
		(insert "----------\n")
		(insert (save-excursion
			  (set-buffer buf)
			  (buffer-string)))
		(setq bufs (cdr bufs)))
	      (insert "--------------------------------------------------\n"))
	    )))
    (vm-submit-bug-report nil (list trace-buffer-hook))
  ))


(defun vm-imap-set-default-attributes (m)
  (vm-set-headers-to-be-retrieved m nil)
  (vm-set-body-to-be-retrieved m vm-load-headers-only))

(defun vm-imap-unset-body-retrieve ()
  "Unset the body-to-be-retrieved flag of all the messages.  May
  be needed if the folder has become corrupted somehow."
  (interactive)
  (save-excursion
   (vm-select-folder-buffer)
   (let ((mp vm-message-list))
     (while mp
       (vm-set-body-to-be-retrieved (car mp) nil)
       (setq mp (cdr mp))))
   (message "Marked %s messages as having retrieved bodies" 
	    (length vm-message-list))
   ))

(defun vm-imap-unset-byte-counts ()
  "Unset the byte counts of all the messages, so that the size of the
downloaded bodies will be displayed."
  (interactive)
  (save-excursion
   (vm-select-folder-buffer)
   (let ((mp vm-message-list))
     (while mp
       (vm-set-byte-count-of (car mp) nil)
       (setq mp (cdr mp))))
   (message "Unset the byte counts of %s messages" 
	    (length vm-message-list))
   ))


(provide 'vm-imap)

;;; vm-imap.el ends here
