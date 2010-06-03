;From: blueman <NOSPAM@nospam.com>
;Subject: Function to fit displayed mime images to width
;Newsgroups: gnu.emacs.vm.info
;Date: Tue, 12 Dec 2006 18:07:44 GMT

;Was going through some old code and would like to share this helpful
;function..

;; Stretch/Shrink mime image to fit exactly in frame width.
;; The shrink functionality is particularly helpful since images displayed
;; by emacs look wacked when they extend past a line width
(defun vm-mime-fitwidth-image (extent)
"Stretch/Shrink mime image to fit exactly in frame width (JJK)."
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
		  (blob (get (vm-mm-layout-cache layout)
					     'vm-mime-display-internal-image-xxxx))
		   dims tempfile factor)
    ;; Emacs 19 uses a different layout cache than XEmacs or Emacs 21+.
    ;; The cache blob is a list in that case.
    (if (consp blob)
		(setq tempfile (car blob))
      (setq tempfile blob))
    (setq dims (vm-get-image-dimensions tempfile))
	(setq factor (/ (float (* (1- (frame-width)) (frame-char-width))) (car dims)))
    (vm-mime-frob-image-xxxx extent
			 "-scale"
			 (concat (int-to-string (* factor (car dims)))
					 "x"
					 (int-to-string (* factor (nth 1 dims)))))))

;; Functionality to add above function to standard attachment menu
(add-hook 'vm-menu-setup-hook 
	  (lambda ()
		(require 'easymenu)
		(easy-menu-add-item vm-menu-fsfemacs-image-menu
		     nil
			 ["Fit to width"
			  (vm-mime-run-display-function-at-point 'vm-mime-fitwidth-image)
			  (stringp vm-imagemagick-convert-program)]
			 "4x Larger" )
		(easy-menu-add-item vm-menu-fsfemacs-attachment-menu
			 nil
			 ["Save attachment..."
			  (vm-mime-run-display-function-at-point 
			   'vm-mime-send-body-to-file) 
			  t ]
			 "Set Content Disposition..." )
		(easy-menu-add-item vm-menu-fsfemacs-attachment-menu
			 nil
			 ["Delete attachment..."
			  (vm-delete-mime-object)
			  t ]
			 "Set Content Disposition..." )
		(easy-menu-add-item vm-menu-fsfemacs-attachment-menu
			 nil
			 ["Attach to message..."
			  (call-interactively 'vm-mime-attach-object-from-message)
			  t ]
			 "Set Content Disposition..." )
		(easy-menu-add-item vm-menu-fsfemacs-attachment-menu
			 nil
			 ["Display as Ascii"
			  (vm-mime-run-display-function-at-point 
			   'vm-mime-display-body-as-text)
			  t ]
			 "Set Content Disposition..." )
		(easy-menu-add-item vm-menu-fsfemacs-attachment-menu
			 nil
			 ["Pipe to Command"
			  (vm-mime-run-display-function-at-point 
			   'vm-mime-pipe-body-to-queried-command-discard-output)
			  t ]
			 "Set Content Disposition..." )
		))



;From: blueman <NOSPAM@nospam.com>
;Subject: Function to retrieve mail via fetchmail from emacs/vm
;Newsgroups: gnu.emacs.vm.info
;Date: Tue, 12 Dec 2006 18:31:57 GMT

;Was going through some old code and would like to share this helpful
;function..

;Note this runs the users local fetchmail process as configured by
;~/.fetchmailrc
(defun vm-fetchmail ()
"*Fetch mail asynchronously from remote server (JJK)"
  (interactive)
  (cond
   ((file-executable-p vm-fetchmail-function)
	(set-process-sentinel
	 (start-process "Fetchmail" "*Fetchmail*" vm-fetchmail-function)
	 'vm-fetchmail-sentinel)
	(message "Fetching new mail..."))
   (t (error "Error: Fetchmail not found on system!"))))

(defvar vm-fetchmail-function "/usr/bin/fetchmail"
"Function used to fetch remote mail (JJK)")

(defun vm-fetchmail-sentinel (process status)
  (beep t)
  (setq status (substring status -2 -1))
  (message "Finished fetching... %s"
   (if (string= status "d") "*New mail*"
	 (setq status (string-to-number status))
	 (cond
	  ((= status 1) "No new mail")
	  ((= status 2) "Error opening socket")
	  ((= status 3) "User authentication failed")
	  ((= status 4) "Fatal protocol error")
	  ((= status 5) "Syntax error")
	  ((= status 6) "Bad permissions on run control file")
	  ((= status 7) "Error condition reported by server")
	  ((= status 8)  "Client-side exclusion error")
	  ((= status 9)  "Lock busy")
	  (t "Other error")))))

