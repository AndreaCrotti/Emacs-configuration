;;; p4.el --- Simple Perforce-Emacs Integration
;;
;; $Id: p4.el,v 1.68 2004/06/12 00:46:26 rvgnu Exp $

;;; Commentary:
;;
;;    Applied the GNU G.P.L. to this file - rv 3/27/1997

;;    Programs for  Emacs <-> Perforce Integration.
;;    Copyright (C) 1996, 1997	Eric Promislow
;;    Copyright (C) 1997-2004	Rajesh Vaidheeswarran
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;    If you have any problems to report, or suggestions, please send them
;;    to p4el-bugs@lists.sourceforge.net

;; LCD Archive Entry:
;; p4|Rajesh Vaidheeswarran|rv@NoSpAm.lOsEtHiS.dsmit.com|
;; P4 SCM Integration into Emacs/XEmacs|
;; 2004/06/11|10.6|not_assigned_yet|

;; WARNING:
;; --------
;;
;;    % p4 edit foo.c
;;    ... make changes to foo.c in emacs
;;    % p4 submit
;;     ... keep the writable copy of foo.c in emacs.  Start making changes
;;     to it.  Discover that you can't save it.	 If you do M-x:p4-edit,
;;     you'll lose your changes.  You need to do a 'p4 edit' at the
;;     command-line.
;;

;; NOTES:
;; ------
;;
;; It is best if you take this file and byte compile it. To do that, you
;; need to do the following:
;;
;; % emacs -batch -f batch-byte-compile /full/path/to/file/p4.el
;;
;; This creates a binary file p4.elc in the path. Add the path to your
;; load-path variable in .emacs like this:
;;
;; (setq load-path (cons "/full/path/to/file" load-path))
;;
;; Then add the library like this:
;;
;; (load-library "p4")
;;

;;; Code:

(defvar p4-emacs-version "10.6" "The Current P4-Emacs Integration Revision.")

;; Find out what type of emacs we are running in. We will be using this
;; quite a few times in this program.
(eval-and-compile
  (defvar p4-running-emacs nil
    "If the current Emacs is not XEmacs, then, this is non-nil.")
  (defvar p4-running-xemacs nil
    "If the current Emacs is XEmacs/Lucid, then, this is non-nil.")
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (setq p4-running-xemacs t)
    (setq p4-running-emacs t)))

;; Pick up a couple of missing function defs
(if p4-running-xemacs
    (eval-when-compile
      (require 'timer)
      (require 'dired)))

(defvar p4-emacs-maintainer
  "p4.el maintainers <p4el-bugs@lists.sourceforge.net>"
  "The maintainer(s) of the emacs-p4 integration. Used for bug reports.")

(defvar p4-web-page "http://p4el.sourceforge.net/" "The home of p4.el.")

;; For flavors of Emacs which don't define `defgroup' and `defcustom'.
(eval-when-compile
  (if (not (fboundp 'defgroup))
      (defmacro defgroup (sym memb doc &rest args)
	"Null macro for defgroup in all versions of Emacs that don't define
defgroup"
	t))
  (if (not (fboundp 'defcustom))
      (defmacro defcustom (sym val doc &rest args)
	"Macro to alias defcustom to defvar in all versions of Emacs that
don't define defcustom"
	`(defvar ,sym ,val ,doc))))

(defgroup p4 nil "Perforce VC System." :group 'tools)

;; This can be set to wherever 'p4' lies using p4-set-p4-executable
(eval-and-compile
  (defun p4-windows-os ()
    (memq system-type '(ms-dos windows-nt)))

  (defcustom p4-executable
    (let ((lst (append
		exec-path
		(list "/usr/local/bin/p4"
		      (concat (getenv "HOME") "/bin/p4")
		      "p4")))
	  (p4-progname (if (p4-windows-os) "p4.exe" "p4"))
	  p4ex)
      (while (and lst (not p4ex))
	(let ((tmp (concat (file-name-as-directory (car lst))
			   p4-progname)))
	  (if (and (file-executable-p tmp)
		   (not (file-directory-p tmp)))
	      (setq p4ex tmp))
	  (setq lst (cdr lst))))
      p4ex)
    "This is the p4 executable.
To set this, use the function  `p4-set-p4-executable' or `customize'"
    :type 'string
    :group 'p4)

  (defcustom p4-cygpath-exec "cygpath" "Path to cygpath binary on cygwin
systems."
    :type 'string
    :group 'p4))
;; This is a string with default arguments to pass to "p4 diff",
;; "p4 diff2", "p4 describe", etc.
(defcustom p4-default-diff-options "-du"
  "Type of p4 diff output to be displayed. \(regular or context or
unified.\)"
  :type 'string
  :group 'p4)

(defcustom p4-default-depot-completion-prefix "//depot/"
  "Prefix to be used for completion prompt when prompting user for a depot
file."
  :type 'string
  :group 'p4)

;; Set this variable to nil to turn off colorized diff buffers.
(defcustom p4-colorized-diffs t
  "Set this to nil to disable colorized diffs."
  :type 'boolean
  :group 'p4)

;; Set whether P4CONFIG should be used exclusively for VC checking
(defcustom p4-use-p4config-exclusively nil
  "Whether P4 mode should use P4CONFIG exclusively to check whether a file
is under P4 version control. If set to nil, `p4-check-mode' is always
called; otherwise, it checks to see if the file named by P4CONFIG exists in
this or a parent directory, and if so, only then runs p4-check-mode.

This provides for a much faster `p4-find-file-hook'."
  :type 'boolean
  :group 'p4)

;; Auto-refresh?
(defcustom p4-auto-refresh t
  "Set this to automatically refresh p4 submitted files in buffers."
  :type 'boolean
  :group 'p4)

;; Check for empty diffs at submit time
(defcustom p4-check-empty-diffs t
  "Set this to check for files with empty diffs before submitting."
  :type 'boolean
  :group 'p4)

(defcustom p4-verbose t
  "When set, p4 will pop up the output buffer with the result of the
command."
  :type 'boolean
  :group 'p4)

;; Follow Symlinks?
(defcustom p4-follow-symlinks nil
  "When set, p4 will call `file-truename' on all opened files."
  :type 'boolean
  :group 'p4)

(defcustom p4-mode-hook nil
  "Hook run by `p4-mode'."
  :type 'sexp
  :group 'p4)

(eval-and-compile
  (defvar p4-output-buffer-name "*P4 Output*" "P4 Output Buffer."))

;; Set this variable in .emacs if you want p4-set-client-name to complete
;; your client name for you.
(defvar p4-my-clients nil
  "This variable holds the alist of p4 clients that the function
`p4-set-client-name' can complete on.

Set this variable *only* if you don't want P4 to complete on all the clients
in the P4 server.

This is a alist, and should be set using the function
`p4-set-my-clients'. For example, in your .emacs:

\(load-library \"p4\"\)
\(p4-set-my-clients \'(client1 client2 client3)\)")

;; Set this variable in .emacs if you want to alter the completion
;; behavior of p4-set-client-name.

(defcustom p4-strict-complete t
  "Set this variable in .emacs \(or using `customize'\) if you want to alter
the completion behavior of `p4-set-client-name'.
"
  :type 'boolean
  :group 'p4)

(if (not (getenv "P4PORT"))
    (setenv "P4PORT" "perforce:1666"))

(defvar p4-notify-list (getenv "P4NOTIFY") "The P4 Notify List.")

(defcustom p4-sendmail-program (if (boundp 'sendmail-program)
				   sendmail-program
				 nil)
  "The sendmail program. To set this use `customize'."
  :type 'string
  :group 'p4)

(defcustom p4-user-email (if (boundp 'user-mail-address)
			     user-mail-address nil)
  "The e-mail address of the current user. This is used with the
notification system, and must be set if notification should take place. To
set this, use `customize'."
  :type 'string
  :group 'p4)

(defcustom p4-notify nil
  "If this is t then the users in the notification list set by
`p4-set-notify-list' will get a notification of any P4 change submitted from
within emacs."
  :type 'boolean
  :group 'p4)

;; This can be set with p4-toggle-vc-mode
(defcustom p4-do-find-file t
  "If non-nil, the `p4-find-file-hook' will run when opening files."
  :type 'boolean
  :group 'p4)

;; Now add a hook to find-file-hooks
(add-hook 'find-file-hooks 'p4-find-file-hook)
;; .. and one to kill-buffer-hook
(add-hook 'kill-buffer-hook 'p4-kill-buffer-hook)

;; Tell Emacs about this new kind of minor mode
(defvar p4-mode nil "Is this file under p4?")
(make-variable-buffer-local 'p4-mode)
(put 'p4-mode 'permanent-local t)

(defvar p4-offline-mode nil "Is this file under p4 but handled in offline mode?")
(make-variable-buffer-local 'p4-offline-mode)
(put 'p4-offline-mode 'permanent-local t)

(defvar p4-minor-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-q" 'p4-toggle-read-only)
    map)
  "Keymap for p4 minor mode")
(fset 'p4-minor-map p4-minor-map)
(or (assoc 'p4-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(p4-mode p4-mode)
				 minor-mode-alist)))
(or (assoc 'p4-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons '(p4-mode . p4-minor-map) minor-mode-map-alist)))
(or (assoc 'p4-offline-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(p4-offline-mode p4-offline-mode)
				 minor-mode-alist)))
(or (assoc 'p4-offline-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons '(p4-offline-mode . p4-minor-map) minor-mode-map-alist)))

(defvar p4-async-minor-mode nil
  "The minor mode for editing p4 asynchronous command buffers.")
(make-variable-buffer-local 'p4-async-minor-mode)
(defvar p4-async-minor-map (make-sparse-keymap) "Keymap for p4 async minor mode")
(fset 'p4-async-minor-map p4-async-minor-map)

(or (assoc 'p4-async-minor-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(p4-async-minor-mode " P4") minor-mode-alist)))

(or (assoc 'p4-async-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons '(p4-async-minor-mode . p4-async-minor-map) minor-mode-map-alist)))

(defvar p4-current-command nil)
(make-variable-buffer-local 'p4-current-command)
(put 'p4-current-command 'permanent-local t)
(set-default 'p4-current-command nil)

(defvar p4-current-args nil)
(make-variable-buffer-local 'p4-current-args)
(put 'p4-current-args 'permanent-local t)
(set-default 'p4-current-args nil)

;; To check if the current buffer's modeline and menu need to be altered
(defvar p4-vc-check nil)
(make-variable-buffer-local 'p4-vc-check)
(put 'p4-vc-check 'permanent-local t)
(set-default 'p4-vc-check nil)

(defvar p4-set-client-hooks nil
  "List of functions to be called after a p4 client is changed.
The buffer's local variables (if any) will have been processed before the
functions are called.")

(if p4-running-emacs (require 'timer))

(defvar p4-timer nil "Timer object that will be set to cleanup the caches
periodically.")

(defcustom p4-cleanup-time 600 "seconds after which `p4-cache-cleanup' will
check for dirty caches."
  :type 'integer
  :group 'p4)

(defcustom p4-cleanup-cache t "`p4-cache-cleanup' will cleanup the
branches/clients/dirs/labels caches once in a while if this is non-nil."
  :type 'boolean
  :group 'p4)

(defvar p4-all-buffer-files nil "An associated list of all buffers and
their files under p4 version control. This is to enable autorefreshing of
p4 submitted files being visited by the buffer.")

(defvar p4-file-refresh-timer nil "Timer object that will be set to refresh
the files in Emacs buffers that have been modified by a `p4-submit'.")

(defcustom p4-file-refresh-timer-time 60 "seconds after which
`p4-file-refresh' will check for modified files in Emacs buffers. Set this
variable to 0 to disable periodic refreshing."
  :type 'integer
  :group 'p4)

(defvar p4-async-command-hook nil
  "This hook is run after an async buffer has been set up by
`p4-async-process-command'")

(defvar p4-window-config-stack nil
  "Stack of saved window configurations.")

(defcustom p4-window-config-stack-size 20 "Maximum stack size
for saved window configurations."
  :type 'integer
  :group 'p4)

(defcustom p4-exec-arg-len-max 20000 "Maximum total length of all
arguments to p4 commands."
  :type 'integer
  :group 'p4)

(defvar p4-basic-map
  (let ((map (make-sparse-keymap)))
    (cond (p4-running-xemacs
	   (define-key map [button2] 'p4-buffer-mouse-clicked)
	   (define-key map [button3] 'p4-buffer-mouse-clicked-3))
	  (p4-running-emacs
	   (define-key map [mouse-2] 'p4-buffer-mouse-clicked)
	   (define-key map [mouse-3] 'p4-buffer-mouse-clicked-3)))
    (define-key map [return] 'p4-buffer-commands)
    (define-key map "\r" 'p4-buffer-commands)
    (define-key map "q"	 'p4-quit-current-buffer)
    (define-key map "k"	 'p4-scroll-down-1-line)
    (define-key map "j"	 'p4-scroll-up-1-line)
    (define-key map "b"	 'p4-scroll-down-1-window)
    (define-key map [backspace] 'p4-scroll-down-1-window)
    (define-key map " "	 'p4-scroll-up-1-window)
    (define-key map "<"	 'p4-top-of-buffer)
    (define-key map ">"	 'p4-bottom-of-buffer)
    (define-key map "="	 'p4-delete-other-windows)
    map))

(defun p4-make-derived-map (base-map)
  (let (map)
    (cond (p4-running-xemacs
	   (setq map (make-sparse-keymap))
	   (set-keymap-parents map (list base-map)))
	  (p4-running-emacs
	   (setq map (cons 'keymap base-map))))
    map))

(defvar p4-filelog-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "d"	 'p4-diff2)
    (define-key map "f"	 'p4-find-file-other-window)
    (define-key map "s"	 'p4-filelog-short-format)
    (define-key map "l"	 'p4-filelog-long-format)
    (define-key map "k"	 'p4-scroll-down-1-line-other-w)
    (define-key map "j"	 'p4-scroll-up-1-line-other-w)
    (define-key map "b"	 'p4-scroll-down-1-window-other-w)
    (define-key map [backspace] 'p4-scroll-down-1-window-other-w)
    (define-key map " "	 'p4-scroll-up-1-window-other-w)
    (define-key map "<"	 'p4-top-of-buffer-other-w)
    (define-key map ">"	 'p4-bottom-of-buffer-other-w)
    (define-key map "="	 'p4-delete-other-windows)
    (define-key map "n"	 'p4-goto-next-change)
    (define-key map "p"	 'p4-goto-prev-change)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for selecting filelog properties.")

(defvar p4-opened-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n"	 'p4-next-depot-file)
    (define-key map "p"	 'p4-prev-depot-file)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for selecting opened files.")

(defvar p4-diff-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n"	 'p4-goto-next-diff)
    (define-key map "p"	 'p4-goto-prev-diff)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "d"	 'p4-next-depot-diff)
    (define-key map "u"	 'p4-prev-depot-diff)
    map))

(defvar p4-print-rev-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n"	 'p4-next-change-rev-line)
    (define-key map "p"	 'p4-prev-change-rev-line)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "l"	 'p4-toggle-line-wrap)
    map)
  "The key map to use for browsing print-revs buffers.")

;;; All functions start here.

;; A generic function that we use to execute p4 commands
(eval-and-compile
  (defun p4-exec-p4 (output-buffer args &optional clear-output-buffer)
    "Internal function called by various p4 commands.
Executes p4 in the current buffer's current directory
with output to a dedicated output buffer.
If successful, adds the P4 menu to the current buffer.
Does auto re-highlight management (whatever that is)."
    (save-excursion
      (if (eq major-mode 'dired-mode)
	  (let ((dir (dired-current-directory)))
	    (set-buffer output-buffer)
	    (setq default-directory dir)))
      (if clear-output-buffer
	  (progn
	    (set-buffer output-buffer)
	    (delete-region (point-min) (point-max))))
      (let ((result
	     ;; XXX - call-process has changed from using
	     ;; p4-null-device to nil as its second argument
	     ;; in emacs version 21.1.1?? - rv 1/25/2002
	     (apply 'call-process (p4-check-p4-executable) nil
		    output-buffer
		    nil			; update display?
		    "-d" default-directory  ;override "PWD" env var
		    args)))
	(p4-menu-add)
	(if (and p4-running-emacs
		 (boundp 'hilit-auto-rehighlight))
	    (setq hilit-auto-rehighlight nil))
	result)))
  (defun p4-call-p4-here (&rest args)
    "Internal function called by various p4 commands.
Executes p4 in the current buffer (generally a temp)."
    (apply 'call-process (p4-check-p4-executable) nil
	   t
	   nil			; update display?
	   "-d" default-directory  ;override "PWD" env var
	   args)))

(defun p4-push-window-config ()
  "Push the current window configuration on the `p4-window-config-stack'
stack."
  (interactive)
  (setq p4-window-config-stack
	(cons (current-window-configuration)
	      p4-window-config-stack))
  (while (> (length p4-window-config-stack) p4-window-config-stack-size)
    (setq p4-window-config-stack
	  (reverse (cdr (reverse p4-window-config-stack))))))

(defun p4-pop-window-config (num)
  "Pop `num' elements from the `p4-window-config-stack' stack and use
the last popped element to restore the window configuration."
  (interactive "p")
  (while (> num 0)
    (if (eq p4-window-config-stack nil)
	(error "window config stack empty"))
    (set-window-configuration (car p4-window-config-stack))
    (setq p4-window-config-stack (cdr p4-window-config-stack))
    (setq num (1- num)))
  (message "window config popped (stack size %d)"
	   (length p4-window-config-stack)))


;; The menu definition is in the XEmacs format. Emacs parses and converts
;; this definition to its own menu creation commands.

(defalias 'p4-toggle-vc-mode-off 'p4-toggle-vc-mode)
(defalias 'p4-toggle-vc-mode-on 'p4-toggle-vc-mode)

(eval-and-compile
  (defvar p4-menu-def
    '(["Specify Arguments..." universal-argument t]
      ["--" nil nil]
      ["Add Current to P4" p4-add
       (and (p4-buffer-file-name) (not p4-mode))]
      ["Check out/Edit"    p4-edit
       (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
      ["Re-open"	       p4-reopen
       (and (p4-buffer-file-name-2) (or (not p4-mode) (not buffer-read-only)))]
      ["Revert File"  p4-revert
       (and (p4-buffer-file-name-2) (or (not p4-mode) (not buffer-read-only)))]
      ["Delete File from Depot"  p4-delete
       (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
      ["Rename Depot File" p4-rename
       (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
      ["Submit Changes"  p4-submit t]
      ["--" nil nil]
      ["Sync/Get Files from Depot" p4-get t]
      ["--" nil nil]
      ["Show Opened Files"	p4-opened t]
      ["Filelog" p4-filelog (p4-buffer-file-name-2)]
      ["Changes" p4-changes t]
      ["Describe Change" p4-describe t]
      ["--" nil nil]
      ["Diff 2 Versions" p4-diff2 (p4-buffer-file-name-2)]
      ["Diff Current" p4-diff t]
      ["Diff All Opened Files" p4-diff-all-opened t]
      ["Diff Current with Ediff"   p4-ediff
       (and (p4-buffer-file-name) (not buffer-read-only) p4-mode)]
      ["Diff 2 Versions with Ediff"   p4-ediff2 (p4-buffer-file-name-2)]
      ["--" nil nil]
      ["Schedule Integrations" p4-integ t]
      ["Resolve Conflicts" p4-resolve t]
      ["--" nil nil]
      ["Print" p4-print (p4-buffer-file-name-2)]
      ["Print with Revision History" p4-blame
       (p4-buffer-file-name-2)]
      ["Find File using Depot Spec" p4-depot-find-file
       p4-do-find-file]
      ["--" nil nil]
      ["Edit a Branch Specification" p4-branch t]
      ["Edit a Label Specification" p4-label t]
      ["Edit a Client Specification" p4-client t]
      ["Edit a User Specification" p4-user t]
      ["--" nil nil]
      ["Show Version" p4-emacs-version t]
      ["Disable P4 VC Check"  p4-toggle-vc-mode-off
       p4-do-find-file]
      ["Enable P4 VC Check"	 p4-toggle-vc-mode-on
       (not p4-do-find-file)]
      ["--" nil nil]
      ["Set P4 Config"  p4-set-client-config p4-do-find-file]
      ["Get Current P4 Config"  p4-get-client-config
       p4-do-find-file]
      ["--" nil nil]
      ["Set P4 Client"  p4-set-client-name p4-do-find-file]
      ["Get Current P4 Client"  p4-get-client-name
       p4-do-find-file]
      ["--" nil nil]
      ["Set P4 Server/Port"	 p4-set-p4-port p4-do-find-file]
      ["Get Current P4 Server/Port"	 p4-get-p4-port
       p4-do-find-file]
      ["--" nil nil]
      ["Set P4 Notification List"  p4-set-notify-list
       p4-mode]
      ["Get P4 Notification List"  p4-get-notify-list p4-notify]
      ["--" nil nil]
      ["Describe Key Bindings"  p4-describe-bindings t]
      ["Check for later versions of p4.el" p4-browse-web-page t]
      ["--" nil nil]
      ["Report Bug in p4.el"  p4-bug-report t])
    "The P4 menu definition")

  (cond (p4-running-xemacs
	 ;; Menu Support for XEmacs
	 (require 'easymenu)
	 (defun p4-mode-menu (modestr)
	   (cons modestr p4-menu-def)))

	(p4-running-emacs
	 ;; Menu support for Emacs
	 (or (lookup-key global-map [menu-bar])
	     (define-key global-map [menu-bar] (make-sparse-keymap "menu-bar")))
	 (defvar menu-bar-p4-menu (make-sparse-keymap "P4"))
	 (setq menu-bar-final-items (cons 'p4-menu menu-bar-final-items))
	 (define-key global-map [menu-bar p4-menu]
	   (cons "P4" menu-bar-p4-menu))
	 (let ((m (reverse p4-menu-def))
	       (separator-number 0))
	   (while m
	     (let ((menu-text (elt (car m) 0))
		   (menu-action (elt (car m) 1))
		   (menu-pred (elt (car m) 2)))
	       (if menu-action
		   (progn
		     (define-key menu-bar-p4-menu (vector menu-action)
		       (cons menu-text menu-action))
		     (put menu-action 'menu-enable menu-pred))
		 (define-key menu-bar-p4-menu
		   (vector (make-symbol
			    (concat "separator-"
				    (int-to-string separator-number))))
		   '("--"))
		 (setq separator-number (1+ separator-number))))
	     (setq m (cdr m))))))

  (defun p4-depot-output (command &optional args)
    "Executes p4 command inside a buffer.
Returns the buffer."
    (let ((buffer (generate-new-buffer p4-output-buffer-name)))
      (p4-exec-p4 buffer (cons command args) t)
      buffer))

  (defun p4-check-p4-executable ()
    "Check if the `p4-executable' is nil, and if so, prompt the user for a
valid `p4-executable'."
    (interactive)
    (if (not p4-executable)
	(call-interactively 'p4-set-p4-executable)
      p4-executable))

  (defun p4-menu-add ()
    "To add the P4 menu bar button for files that are already not in
the P4 depot or in the current client view.."
    (interactive)
    (cond (p4-running-xemacs
	   (if (not (boundp 'p4-mode))
	       (setq p4-mode nil))
	   (easy-menu-add (p4-mode-menu "P4"))))
    t)

  (defun p4-help-text (cmd text)
    (if cmd
	(let ((buf (generate-new-buffer p4-output-buffer-name))
	      (help-text ""))
	  (if (= (p4-exec-p4 buf (list "help" cmd) t) 0)
	      (setq help-text (save-excursion
				(set-buffer buf)
				(buffer-string))))
	  (kill-buffer buf)
	  (concat text help-text))
      text))

  ;; To set the path to the p4 executable
  (defun p4-set-p4-executable (p4-exe-name)
    "Set the path to the correct P4 Executable.

To set this as a part of the .emacs, add the following to your .emacs:

\(load-library \"p4\"\)
\(p4-set-p4-executable \"/my/path/to/p4\"\)

Argument P4-EXE-NAME The new value of the p4 executable, with full path."
    (interactive "fFull path to your P4 executable: " )
    (setq p4-executable p4-exe-name)
    p4-executable))

;; The kill-buffer hook for p4.
(defun p4-kill-buffer-hook ()
  "To Remove a file and its associated buffer from our global list of P4
controlled files."
  (if p4-vc-check
      (p4-refresh-refresh-list (p4-buffer-file-name)
			       (buffer-name))))

(defmacro defp4cmd (fkn &rest all-args)
  (let ((args (car all-args))
	(help-cmd (cadr all-args))
	(help-txt (eval (cadr (cdr all-args))))
	(body (cdr (cddr all-args))))
    `(defalias ',fkn
       ,(append (list 'lambda args
		      (p4-help-text help-cmd help-txt))
		body))))

(defun p4-noinput-buffer-action (cmd
				 do-revert
				 show-output
				 &optional arguments preserve-buffer)
  "Internal function called by various p4 commands."
  (save-excursion
    (save-excursion
      (if (not preserve-buffer)
	  (progn
	    (get-buffer-create p4-output-buffer-name);; We do these two lines
	    (kill-buffer p4-output-buffer-name)))    ;; to ensure no duplicates
      (p4-exec-p4 (get-buffer-create p4-output-buffer-name)
		  (append (list cmd) arguments)
		  t))
    (p4-partial-cache-cleanup cmd)
    (if show-output
	(if (and
	     (eq show-output 's)
	     (= (save-excursion
		  (set-buffer p4-output-buffer-name)
		  (count-lines (point-min) (point-max)))
		1)
	     (not (save-excursion
		    (set-buffer p4-output-buffer-name)
		    (goto-char (point-min))
		    (looking-at "==== "))))
	    (save-excursion
	      (set-buffer p4-output-buffer-name)
	      (message (buffer-substring (point-min)
					 (save-excursion
					   (goto-char (point-min))
					   (end-of-line)
					   (point)))))
	  (p4-push-window-config)
	  (if (not (one-window-p))
	      (delete-other-windows))
	  (display-buffer p4-output-buffer-name t))))
  (if (and do-revert (p4-buffer-file-name))
      (revert-buffer t t)))

;; The p4 edit command
(defp4cmd p4-edit (show-output)
  "edit" "To open the current depot file for edit, type \\[p4-edit].\n"
  (interactive (list p4-verbose))
  (let ((args (p4-buffer-file-name))
	refresh-after)
    (if (or current-prefix-arg (not args))
	(progn
	  (setq args (if (p4-buffer-file-name-2)
			 (p4-buffer-file-name-2)
		       ""))
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 edit: " (cons args 0))))
	  (setq refresh-after t))
      (setq args (list args)))
    (p4-noinput-buffer-action "edit" t (and show-output 's) args)
    (if refresh-after
	(p4-refresh-files-in-buffers)))
  (p4-check-mode)
  (p4-update-opened-list))

;; The p4 reopen command
(defp4cmd p4-reopen (show-output)
  "reopen"
  "To change the type or changelist number of an opened file, type \\[p4-reopen].

Argument SHOW-OUTPUT displays the *P4 Output* buffer on executing the
command if t.\n"

  (interactive (list p4-verbose))
  (let ((args (if (p4-buffer-file-name-2)
		  (p4-buffer-file-name-2)
		"")))
    (setq args (p4-make-list-from-string
		(p4-read-arg-string "p4 reopen: " (cons args 0))))
    (p4-noinput-buffer-action "reopen" t (and show-output 's) args))
  (p4-check-mode)
  (p4-update-opened-list))

;; The p4 revert command
(defp4cmd p4-revert (show-output)
  "revert" "To revert all change in the current file, type \\[p4-revert].\n"
  (interactive (list p4-verbose))
  (let ((args (p4-buffer-file-name))
	refresh-after)
    (if (or current-prefix-arg (not args))
	(progn
	  (setq args (if (p4-buffer-file-name-2)
			 (p4-buffer-file-name-2)
		       ""))
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 revert: " args)))
	  (setq refresh-after t))
      (setq args (list args)))
    (if (yes-or-no-p "Really revert changes? ")
	(progn
	  (p4-noinput-buffer-action "revert" t (and show-output 's) args)
	  (if refresh-after
	      (progn
		(p4-refresh-files-in-buffers)
		(p4-check-mode-all-buffers))
	    (p4-check-mode))
	  (p4-update-opened-list)))))

;; The p4 lock command
(defp4cmd p4-lock ()
  "lock" "To lock an opened file against changelist submission, type \\[p4-lock].\n"
  (interactive)
  (let ((args (list (p4-buffer-file-name-2))))
    (if (or current-prefix-arg (not (p4-buffer-file-name-2)))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 lock: "
					(p4-buffer-file-name-2)))))
    (p4-noinput-buffer-action "lock" t 's args)
    (p4-update-opened-list)))

;; The p4 unlock command
(defp4cmd p4-unlock ()
  "unlock" "To release a locked file but leave open, type \\[p4-unlock].\n"
  (interactive)
  (let ((args (list (p4-buffer-file-name-2))))
    (if (or current-prefix-arg (not (p4-buffer-file-name-2)))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 unlock: "
					(p4-buffer-file-name-2)))))
    (p4-noinput-buffer-action "unlock" t 's args)
    (p4-update-opened-list)))

;; The p4 diff command
(defp4cmd p4-diff ()
  "diff" "To diff the current file and topmost depot version, type \\[p4-diff].\n"
  (interactive)
  (let ((args (p4-make-list-from-string p4-default-diff-options)))
    (if (p4-buffer-file-name-2)
	(setq args (append args
			   (list (p4-buffer-file-name-2)))))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 diff: " p4-default-diff-options))))
    (p4-noinput-buffer-action "diff" nil 's args)
    (p4-activate-diff-buffer "*P4 diff*")))

(defun p4-diff-all-opened ()
  (interactive)
  (p4-noinput-buffer-action "diff" nil 's
			    (p4-make-list-from-string p4-default-diff-options))
  (p4-activate-diff-buffer "*P4 diff*"))


(defun p4-get-file-rev (default-name rev)
  (if (string-match "^\\([0-9]+\\|none\\|head\\|have\\)$" rev)
      (setq rev (concat "#" rev)))
  (cond ((string-match "^[#@]" rev)
	 (concat default-name rev))
	((string= "" rev)
	 default-name)
	(t
	 rev)))

;; The p4 diff2 command
(defp4cmd p4-diff2 (version1 version2)
  "diff2" "Display diff of two depot files.

When visiting a depot file, type \\[p4-diff2] and enter the versions.\n"
  (interactive
   (let ((rev (get-char-property (point) 'rev)))
     (if (and (not rev) (p4-buffer-file-name-2))
	 (let ((rev-num 0))
	   (setq rev (p4-is-vc nil (p4-buffer-file-name-2)))
	   (if rev
	       (setq rev-num (string-to-number rev)))
	   (if (> rev-num 1)
	       (setq rev (number-to-string (1- rev-num)))
	     (setq rev nil))))
     (list (p4-read-arg-string "First Depot File or Version# to diff: " rev)
	   (p4-read-arg-string "Second Depot File or Version# to diff: "))))
  (let (diff-version1
	diff-version2
	(diff-options (p4-make-list-from-string p4-default-diff-options)))
    (if current-prefix-arg
	(setq diff-options (p4-make-list-from-string
			    (p4-read-arg-string "Optional Args: "
						p4-default-diff-options))))
    ;; try to find out if this is a revision number, or a depot file
    (setq diff-version1 (p4-get-file-rev (p4-buffer-file-name-2) version1))
    (setq diff-version2 (p4-get-file-rev (p4-buffer-file-name-2) version2))

    (p4-noinput-buffer-action "diff2" nil t
			      (append diff-options
				      (list diff-version1
					    diff-version2)))
    (p4-activate-diff-buffer "*P4 diff2*")))

(defp4cmd p4-diff-head ()
  "diff-head" "Display diff of file against the head revision in depot.

When visiting a depot file, type \\[p4-diff-head].\n"

  (interactive)
  (let (head-revision
	(diff-options (p4-make-list-from-string p4-default-diff-options)))
    (if current-prefix-arg
	(setq diff-options (p4-make-list-from-string
			    (p4-read-arg-string "Optional Args: "
						p4-default-diff-options))))
    (setq head-revision (p4-get-file-rev (p4-buffer-file-name-2) "head"))

    (p4-noinput-buffer-action "diff" nil t
			      (append diff-options
				      (list head-revision)))
    (p4-activate-diff-buffer "*P4 diff vs. head*")))


;; p4-ediff for all those who diff using ediff

(defun p4-ediff ()
  "Use ediff to compare file with its original client version."
  (interactive)
  (require 'ediff)
  (if current-prefix-arg
      (call-interactively 'p4-ediff2)
    (progn
      (p4-noinput-buffer-action "print" nil nil
				(list "-q"
				      (concat (p4-buffer-file-name) "#have")))
      (let ((local (current-buffer))
	    (depot (get-buffer-create p4-output-buffer-name)))
	(ediff-buffers local
		       depot
		       `((lambda ()
			   (make-local-variable 'ediff-cleanup-hook)
			   (setq ediff-cleanup-hook
				 (cons (lambda ()
					 (kill-buffer ,depot)
					 (p4-menu-add))
				       ediff-cleanup-hook)))))))))

(defp4cmd p4-ediff2 (version1 version2)
  "ediff2" "Use ediff to display two versions of a depot file.

When visiting a depot file, type \\[p4-ediff2] and enter the versions.\n"
  (interactive
   (let ((rev (get-char-property (point) 'rev)))
     (if (and (not rev) (p4-buffer-file-name-2))
	 (let ((rev-num 0))
	   (setq rev (p4-is-vc nil (p4-buffer-file-name-2)))
	   (if rev
	       (setq rev-num (string-to-number rev)))
	   (if (> rev-num 1)
	       (setq rev (number-to-string (1- rev-num)))
	     (setq rev nil))))
     (list (p4-read-arg-string "First Depot File or Version# to diff: " rev)
	   (p4-read-arg-string "Second Depot File or Version# to diff: "))))
  (let* ((file-name (p4-buffer-file-name-2))
         (basename (file-name-nondirectory file-name))
         (bufname1 (concat "*P4 ediff " basename "#" version1  "*"))
         (bufname2 (concat "*P4 ediff " basename "#" version2  "*"))
         (diff-version1 (p4-get-file-rev file-name version1))
         (diff-version2 (p4-get-file-rev file-name version2)))
    (p4-noinput-buffer-action "print" nil nil (list "-q" diff-version1))
    (set-buffer p4-output-buffer-name)
    (rename-buffer bufname1 t)
    (p4-noinput-buffer-action "print" nil nil (list "-q" diff-version2))
    (set-buffer p4-output-buffer-name)
    (rename-buffer bufname2 t)
    (let ((buffer-version-1 (get-buffer-create bufname1))
          (buffer-version-2 (get-buffer-create bufname2)))
      (ediff-buffers buffer-version-1
                     buffer-version-2
                     `((lambda ()
                         (make-local-variable 'ediff-cleanup-hook)
                         (setq ediff-cleanup-hook
                               (cons (lambda ()
                                       (kill-buffer ,buffer-version-1)
                                       (kill-buffer ,buffer-version-2)
                                       (p4-menu-add))
                                     ediff-cleanup-hook))))))))
;; The p4 add command
(defp4cmd p4-add ()
  "add" "To add the current file to the depot, type \\[p4-add].\n"
  (interactive)
  (let ((args (p4-buffer-file-name))
	refresh-after)
    (if (or current-prefix-arg (not args))
	(progn
	  (setq args (if (p4-buffer-file-name-2)
			 (p4-buffer-file-name-2)
		       ""))
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 add: " (cons args 0))))
	  (setq refresh-after t))
      (setq args (list args)))
    (p4-noinput-buffer-action "add" nil 's args)
    (if refresh-after
	(p4-check-mode-all-buffers)
      (p4-check-mode)))
  (p4-update-opened-list))


;; The p4 delete command
(defp4cmd p4-delete ()
  "delete" "To delete the current file from the depot, type \\[p4-delete].\n"
  (interactive)
  (let ((args (p4-buffer-file-name)))
    (if (or current-prefix-arg (not args))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 delete: "
					(p4-buffer-file-name-2))))
      (setq args (list args)))
    (if (yes-or-no-p "Really delete from depot? ")
	(p4-noinput-buffer-action "delete" nil 's args)))
  (p4-check-mode)
  (p4-update-opened-list))

;; The p4 filelog command
(defp4cmd p4-filelog ()
  "filelog"
  "To view a history of the change made to the current file, type \\[p4-filelog].\n"
  (interactive)
  (let ((file-name (p4-buffer-file-name-2)))
    (if (or current-prefix-arg (not file-name))
	(setq file-name (p4-make-list-from-string
			 (p4-read-arg-string "p4 filelog: " file-name)))
      (setq file-name (list file-name)))
    (p4-file-change-log "filelog" file-name)))

(defun p4-set-extent-properties (start end prop-list)
  (cond (p4-running-xemacs
	 (let ((ext (make-extent start end)))
	   (while prop-list
	     (set-extent-property ext (caar prop-list) (cdar prop-list))
	     (setq prop-list (cdr prop-list)))))
	(p4-running-emacs
	 (let ((ext (make-overlay start end)))
	   (while prop-list
	     (overlay-put ext (caar prop-list) (cdar prop-list))
	     (setq prop-list (cdr prop-list)))))))

(defun p4-create-active-link (start end prop-list)
  (p4-set-extent-properties start end
			    (append (list (cons 'face 'bold)
					  (cons 'mouse-face 'highlight))
				    prop-list)))

(defun p4-move-buffer-point-to-top (buf-name)
  (if (get-buffer-window buf-name)
      (save-selected-window
	(select-window (get-buffer-window buf-name))
	(goto-char (point-min)))))

(defun p4-file-change-log (cmd file-list-spec)
  (let ((p4-filelog-buffer
	 (concat "*P4 " cmd ": "
		 (p4-list-to-string file-list-spec) "*")))
    (p4-noinput-buffer-action cmd nil t (cons "-l" file-list-spec))
    (p4-activate-file-change-log-buffer p4-filelog-buffer)))

(defun p4-activate-file-change-log-buffer (bufname)
  (let (p4-cur-rev p4-cur-change p4-cur-action
	p4-cur-user p4-cur-client)
    (p4-activate-print-buffer bufname nil)
    (set-buffer bufname)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (while (re-search-forward (concat
			       "^\\(\\.\\.\\. #\\([0-9]+\\) \\)?[Cc]hange "
			       "\\([0-9]+\\) \\([a-z]+\\)?.*on.*by "
			       "\\([^ @]+\\)@\\([^ \n]+\\).*\n"
			       "\\(\\(\\([ \t].*\\)?\n\\)*\\)") nil t)
      (let ((rev-match 2)
	    (ch-match 3)
	    (act-match 4)
	    (user-match 5)
	    (cl-match 6)
	    (desc-match 7))
	(setq p4-cur-rev (match-string rev-match))
	(setq p4-cur-change (match-string ch-match))
	(setq p4-cur-action (match-string act-match))
	(setq p4-cur-user (match-string user-match))
	(setq p4-cur-client (match-string cl-match))

	(if (match-beginning rev-match)
	    (p4-create-active-link (match-beginning rev-match)
				   (match-end rev-match)
				   (list (cons 'rev p4-cur-rev))))
	(p4-create-active-link (match-beginning ch-match)
			       (match-end ch-match)
			       (list (cons 'change p4-cur-change)))
	(if (match-beginning act-match)
	    (p4-create-active-link (match-beginning act-match)
				   (match-end act-match)
				   (list (cons 'action p4-cur-action)
					 (cons 'rev p4-cur-rev))))
	(p4-create-active-link (match-beginning user-match)
			       (match-end user-match)
			       (list (cons 'user p4-cur-user)))
	(p4-create-active-link (match-beginning cl-match)
			       (match-end cl-match)
			       (list (cons 'client p4-cur-client)))
	(p4-set-extent-properties (match-beginning desc-match)
				  (match-end desc-match)
				  (list (cons 'invisible t)
					(cons 'isearch-open-invisible t)))))
    (p4-find-change-numbers bufname (point-min) (point-max))
    (use-local-map p4-filelog-map)
    (setq buffer-invisibility-spec (list))
    (setq buffer-read-only t)
    (p4-move-buffer-point-to-top bufname)))

;; Scan specified region for references to change numbers
;; and make the change numbers clickable.
(defun p4-find-change-numbers (buffer start end)
  (save-excursion
    (set-buffer buffer)
    (goto-char start)
    (while (re-search-forward "\\(changes?\\|submit\\|p4\\)[:#]?[ \t\n]+" end t)
      (while (looking-at
	      (concat "\\([#@]\\|number\\|no\\.\\|\\)[ \t\n]*"
		      "\\([0-9]+\\)[-, \t\n]*"
		      "\\(and/or\\|and\\|&\\|or\\|\\)[ \t\n]*"))
	(let ((ch-start (match-beginning 2))
	      (ch-end (match-end 2))
	      (ch-str (match-string 2))
	      (next (match-end 0)))
	  (set-text-properties 0 (length ch-str) nil ch-str)
	  (p4-create-active-link ch-start ch-end (list (cons 'change ch-str)))
	  (goto-char next))))))

;; The p4 files command
(defp4cmd p4-files ()
  "files" "List files in the depot. Type, \\[p4-files].\n"
  (interactive)
  (let ((args (p4-buffer-file-name-2)))
    (if (or current-prefix-arg (not args))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 files: " (p4-buffer-file-name-2))))
      (setq args (list args)))
    (p4-noinput-buffer-action "files" nil t args)
    (save-excursion
      (set-buffer p4-output-buffer-name)
      (p4-find-change-numbers p4-output-buffer-name (point-min) (point-max)))
    (p4-make-depot-list-buffer
     (concat "*P4 Files: (" (p4-current-client) ") " (car args) "*"))))


(defvar p4-server-version-cache nil)

(defun p4-get-server-version ()
  "To get the version number of the p4 server."
  (let ((p4-port (p4-current-server-port))
	ser-ver pmin)
    (setq ser-ver (cdr (assoc p4-port p4-server-version-cache)))
    (if (not ser-ver)
	(save-excursion
	  (get-buffer-create p4-output-buffer-name)
	  (set-buffer p4-output-buffer-name)
	  (goto-char (point-max))
	  (setq pmin (point))
	  (if (zerop (p4-call-p4-here "info"))
	      (progn
		(goto-char pmin)
		(re-search-forward
		 "^Server version: .*\/.*\/\\(\\([0-9]+\\)\.[0-9]+\\)\/.*(.*)$")
		(setq ser-ver (string-to-number (match-string 2)))
		(setq p4-server-version-cache (cons (cons p4-port ser-ver)
						    p4-server-version-cache))
		(delete-region pmin (point-max))))))
    ser-ver))

(defun p4-get-client-root (client-name)
  "To get the current value of Client's root type \\[p4-get-client-root].
   This can be used by any other macro that requires this value."
  (let (p4-client-root pmin)
    (save-excursion
      (get-buffer-create p4-output-buffer-name)
      (set-buffer p4-output-buffer-name)
      (goto-char (point-max))
      (setq pmin (point))
      (if (zerop (p4-call-p4-here "client" "-o" client-name))
	  (progn
	    (goto-char pmin)
	    (re-search-forward "^Root:[ \t]+\\(.*\\)$")
	    (setq p4-client-root (p4-canonize-client-root (match-string 1)))
	    (delete-region pmin (point-max)))))
    p4-client-root))

(defun p4-canonize-client-root (p4-client-root)
  "Canonizes client root"
  (let ((len (length p4-client-root)))
    ;; For Windows, since the client root may be terminated with
    ;; a \ as in c:\ or drive:\foo\bar\, we need to strip the
    ;; trailing \ .
    (if (and (p4-windows-os)
	     (> len 1)
	     (equal (substring p4-client-root (1- len) len) "\\"))
	(setq p4-client-root (substring p4-client-root 0 (1- len))))
    p4-client-root))

(defun p4-map-depot-files (file-list)
  "Map a list of files in the depot on the current client.
Return a list of pairs, where each pair consists of a depot
name and a client name."
  (let (file-map)
    (while file-list
      (let (sub-list (arg-len 0) elt)
	(while (and file-list (< arg-len p4-exec-arg-len-max))
	  (setq elt (car file-list))
	  (setq file-list (cdr file-list))
	  (setq sub-list (cons elt sub-list))
	  (setq arg-len (+ arg-len (length elt) 1)))
	(setq file-map (append file-map
			       (p4-map-depot-files-int sub-list)))))
    file-map))

(defun p4-map-depot-files-int (file-list)
  (let* ((current-client (p4-current-client))
	 (client-root (p4-get-client-root current-client))
	 (re-current-client (regexp-quote current-client))
	 (re-client-root (regexp-quote client-root))
	 files pmin)
    (save-excursion
      (get-buffer-create p4-output-buffer-name)
      (set-buffer p4-output-buffer-name)
      (goto-char (point-max))
      (setq pmin (point))
      (insert "\n")
      (apply 'p4-call-p4-here "where" file-list)
      (goto-char pmin)
      (if (< (p4-get-server-version) 98)
	  (while (re-search-forward
		  (concat "^\\([^\n]+\\) //" re-current-client
			  "\\(.*\\)$") nil t)
	    (setq files (cons
			 (cons
			  (match-string 1)
			  (concat client-root (match-string 2)))
			 files)))
	(while (re-search-forward
		(concat "^\\([^\n]+\\) //" re-current-client
			"\\([^\n]+\\) \\(" re-client-root ".*\\)$") nil t)
	  (setq files (cons
		       (cons
			(match-string 1) (match-string 3)) files))))
      (delete-region pmin (point-max)))
    files))

(defun p4-make-face (face-name fg bg)
  "Creates a new face if it does not already exist."
  (let ((face (facep face-name)))
    (cond
     ((null face)
      (make-face face-name)
      (if (not (null bg))
	  (set-face-background face-name bg) t)
      (if (not (null fg))
	  (set-face-foreground face-name fg) t)))))

(p4-make-face 'p4-depot-unmapped-face "grey30" nil)
(p4-make-face 'p4-depot-deleted-face "red" nil)
(p4-make-face 'p4-depot-added-face "blue" nil)
(p4-make-face 'p4-depot-branch-op-face "blue4" nil)

(defun p4-make-depot-list-buffer (bufname &optional print-buffer)
  "Take the p4-output-buffer-name buffer, rename it to bufname, and
make all depot file names active, so that clicking them opens
the corresponding client file."
  (let (args files depot-regexp)
    (set-buffer p4-output-buffer-name)
    (goto-char (point-min))
    (setq depot-regexp
	  (if print-buffer
	      "\\(^\\)\\(//[^/@# ][^/@#]*/[^@#]+\\)#[0-9]+ - "
	    "^\\(\\.\\.\\. [^/\n]*\\|==== \\)?\\(//[^/@# ][^/@#]*/[^#\n]*\\)"))
    (while (re-search-forward depot-regexp nil t)
      (setq args (cons (match-string 2) args)))
    (setq files (p4-map-depot-files args))
    (get-buffer-create bufname);; We do these two lines
    (kill-buffer bufname);; to ensure no duplicates
    (set-buffer p4-output-buffer-name)
    (rename-buffer bufname t)
    (goto-char (point-min))
    (while (re-search-forward depot-regexp nil t)
      (let ((p4-client-file (cdr (assoc (match-string 2) files)))
	    (p4-depot-file (match-string 2))
	    (start (match-beginning 2))
	    (end (match-end 2))
	    (branching-op-p (and (match-string 1)
				 (string-match "\\.\\.\\. \\.\\.\\..*"
					       (match-string 1))))
	    prop-list)
	(if (and p4-client-file
		 (file-readable-p p4-client-file))
	    (setq prop-list (list (cons 'link-client-name
					p4-client-file)))
	  (setq prop-list (list (cons 'link-depot-name
				      p4-depot-file))))
	;; some kind of operation related to branching/integration
	(if branching-op-p
	    (setq prop-list (append (list
				     (cons 'history-for p4-depot-file)
				     (cons 'face
					   'p4-depot-branch-op-face))
				    prop-list)))
	(cond
	 ((not p4-client-file)
	  (p4-set-extent-properties
	   start end
	   (append (list (cons 'face 'p4-depot-unmapped-face))
		   prop-list)))
	 ((save-excursion
	    (goto-char end)
	    (looking-at ".* deleted?[ \n]"))
	  (p4-set-extent-properties
	   start end
	   (append (list (cons 'face 'p4-depot-deleted-face))
		   prop-list)))
	 ((save-excursion
	    (goto-char end)
	    (looking-at ".* \\(add\\|branch\\)\\(ed\\)?[ \n]"))
	  (p4-create-active-link
	   start end
	   (append (list (cons 'face 'p4-depot-added-face))
		   prop-list)))
	 (t
	  (p4-create-active-link start end prop-list)))))
    (use-local-map p4-opened-map)
    (setq buffer-read-only t)
    (p4-move-buffer-point-to-top bufname)))

;; The p4 print command
(defp4cmd p4-print ()
  "print" "To print a depot file to a buffer, type \\[p4-print].\n"
  (interactive)
  (let ((arg-string (p4-buffer-file-name-2))
	(rev (get-char-property (point) 'rev))
	(change (get-char-property (point) 'change)))
    (cond (rev
	   (setq arg-string (concat arg-string "#" rev)))
	  (change
	   (setq arg-string (concat arg-string "@" change))))
    (if (or current-prefix-arg (not arg-string))
	(setq arg-string (p4-make-list-from-string
			  (p4-read-arg-string "p4 print: " arg-string)))
      (setq arg-string (list arg-string)))
    (p4-noinput-buffer-action "print" nil t arg-string)
    (p4-activate-print-buffer "*P4 print*" t)))

;; Insert text in a buffer, but make sure that the inserted text doesn't
;; inherit any properties from surrounding text. This is needed for xemacs
;; because the insert function makes the inserted text inherit properties.
(defun p4-insert-no-properties (str)
  (let ((start (point))
	end)
    (insert str)
    (setq end (point))
    (set-text-properties start end nil)))

(defun p4-font-lock-buffer (buf-name)
  (save-excursion
    (let (file-name (first-line ""))
      (set-buffer buf-name)
      (goto-char (point-min))
      (if (looking-at "^//[^#@]+/\\([^/#@]+\\)")
	  (progn
	    (setq file-name (match-string 1))
	    (forward-line 1)
	    (setq first-line (buffer-substring (point-min) (point)))
	    (delete-region (point-min) (point))))
      (setq buffer-file-name file-name)
      (set-auto-mode)
      (setq buffer-file-name nil)
      (condition-case nil
	  (font-lock-fontify-buffer)
	(error nil))
      (fundamental-mode)
      (if (and p4-running-emacs
	       (boundp 'hilit-auto-rehighlight))
	  (setq hilit-auto-rehighlight nil))
      (goto-char (point-min))
      (p4-insert-no-properties first-line))))

(defun p4-activate-print-buffer (buffer-name print-buffer)
  (if print-buffer
      (p4-font-lock-buffer p4-output-buffer-name))
  (p4-make-depot-list-buffer buffer-name print-buffer)
  (let ((depot-regexp
	 (if print-buffer
	     "^\\(//[^/@# ][^/@#]*/\\)[^@#]+#[0-9]+ - "
	   "^\\(//[^/@# ][^/@#]*/\\)")))
    (save-excursion
      (set-buffer buffer-name)
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (while (re-search-forward depot-regexp nil t)
	(let ((link-client-name (get-char-property (match-end 1)
						   'link-client-name))
	      (link-depot-name (get-char-property (match-end 1)
						  'link-depot-name))
	      (start (match-beginning 1))
	      (end (point-max)))
	  (save-excursion
	    (if (re-search-forward depot-regexp nil t)
		(setq end (match-beginning 1))))
	  (if link-client-name
	      (p4-set-extent-properties start end
					(list (cons 'block-client-name
						    link-client-name))))
	  (if link-depot-name
	      (p4-set-extent-properties start end
					(list (cons 'block-depot-name
						    link-depot-name))))))
      (setq buffer-read-only t))))

(defconst p4-blame-change-regex
  (concat "^\\.\\.\\. #"     "\\([0-9]+\\)"   ;; revision
	  "\\s-+change\\s-+" "\\([0-9]+\\)"   ;; change
	  "\\s-+"            "\\([^ \t]+\\)"  ;; type
	  "\\s-+on\\s-+"     "\\([^ \t]+\\)"  ;; date	   
	  "\\s-+by\\s-+"     "\\([^ \t]+\\)"  ;; author
	  "@"))

(defconst p4-blame-branch-regex
  "^\\.\\.\\. \\.\\.\\. branch from \\(//[^#]*\\)#")

(defconst p4-blame-revision-regex
  (concat "^\\([0-9]+\\),?"
	  "\\([0-9]*\\)"
	  "\\([acd]\\)"
	  "\\([0-9]+\\),?"
	  "\\([0-9]*\\)"))

(defconst p4-blame-index-regex
  (concat " *\\([0-9]+\\)"               ;; change
	  " *\\([0-9]+\\)"               ;; revision
	  " *\\([0-9]+/[0-9]+/[0-9]+\\)" ;; date
	  "\\s-+\\([^:]*\\)"             ;; author
	  ":"))          

(defconst P4-REV  0)
(defconst P4-DATE 1)
(defconst P4-AUTH 2)
(defconst P4-FILE 3)

(defun p4-blame ()
  "To Print a depot file with revision history to a buffer,
type \\[p4-blame]"
  (interactive)
  (let ((arg-string (p4-buffer-file-name-2))
	(rev (get-char-property (point) 'rev))
	(change (get-char-property (point) 'change)))
    (cond (rev
	   (setq arg-string (concat arg-string "#" rev)))
	  (change
	   (setq arg-string (concat arg-string "@" change))))
    (if (or current-prefix-arg (not arg-string))
	(setq arg-string (p4-read-arg-string "p4 print-revs: " arg-string)))
    (p4-blame-int arg-string)))

(defalias 'p4-print-with-rev-history 'p4-blame)

(defun p4-blame-int (file-spec)
  (get-buffer-create p4-output-buffer-name);; We do these two lines
  (kill-buffer p4-output-buffer-name)      ;; to ensure no duplicates
  (let ((file-name file-spec)
	(buffer (get-buffer-create p4-output-buffer-name))
	head-name  ;; file spec of the head revision for this blame assignment
	branch-p   ;; have we tracked into a branch?
	cur-file   ;; file name of the current branch during blame assignment
	change ch-alist fullname head-rev headseen)

    ;; we asked for blame constrained by a change number
    (if (string-match "\\(.*\\)@\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq change (string-to-int (match-string 2 file-spec)))))

    ;; we asked for blame constrained by a revision
    (if (string-match "\\(.*\\)#\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq head-rev (string-to-int (match-string 2 file-spec)))))

    ;; make sure the filespec is unambiguous
    (p4-exec-p4 buffer (list "files" file-name) t)
    (save-excursion
      (set-buffer buffer)
      (if (> (count-lines (point-min) (point-max)) 1)
	  (error "File pattern maps to more than one file.")))

    ;; get the file change history:
    (p4-exec-p4 buffer (list "filelog" "-i" file-spec) t)
    (setq fullname (p4-read-depot-output buffer)
	  cur-file  fullname
	  head-name fullname)

    ;; parse the history:
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (< (point) (point-max))

	;; record the current file name (and the head file name,
	;; if we have not yet seen one):
	(if (looking-at "^\\(//.*\\)$")
	    (setq cur-file (match-string 1)))

	;; a non-branch change:
	(if (looking-at p4-blame-change-regex)
	    (let ((rev (string-to-int (match-string 1)))
		  (ch (string-to-int (match-string 2)))
		  (op (match-string 3))
		  (date (match-string 4))
		  (author (match-string 5)))
	      (cond
	       ;; after the change constraint, OR
	       ;; after the revision constraint _for this file_
	       ;;   [remember, branches complicate this]:
	       ((or (and change   (< change ch))
		    (and head-rev (< head-rev rev)
			 (string= head-name cur-file))) nil)
	       
	       ;; file has been deleted, can't assign blame:
	       ((string= op "delete") 
		(if (not headseen) (goto-char (point-max))))

	       ;; OK, we actually want to look at this one:
	       (t
		(setq ch-alist
		      (cons
		       (cons ch (list rev date author cur-file)) ch-alist))
		(if (not head-rev) (setq head-rev rev))
		(setq headseen t)) ))

	  ;; not if we have entered a branch (this used to be used, isn't
	  ;; right now - maybe again later:
	  (if (and headseen (looking-at p4-blame-branch-regex))
	      (setq branch-p t)) )
	(forward-line)))
    
    (if (< (length ch-alist) 1)
	(error "Head revision not available"))
  
    (let ((base-ch (int-to-string (caar ch-alist)))
	  (ch-buffer (get-buffer-create "p4-ch-buf"))
	  (tmp-alst (copy-alist ch-alist)))
      (p4-exec-p4 ch-buffer
		  (list "print" "-q" (concat cur-file "@" base-ch)) t)
      (save-excursion
	(set-buffer ch-buffer)
	(goto-char (point-min))
	(while (re-search-forward ".*\n" nil t)
	  (replace-match (concat base-ch "\n"))))
      (while (> (length tmp-alst) 1)
	(let ((ch-1 (car (car  tmp-alst)))
	      (ch-2 (car (cadr tmp-alst)))
	      (file1 (nth P4-FILE (cdr (car  tmp-alst))))
	      (file2 (nth P4-FILE (cdr (cadr tmp-alst))))
	      ins-string)
	  (setq ins-string (format "%d\n" ch-2))
	  (p4-exec-p4 buffer (list "diff2"
				   (format "%s@%d" file1 ch-1)
				   (format "%s@%d" file2 ch-2)) t)
	  (save-excursion
	    (set-buffer buffer)
	    (goto-char (point-max))
	    (while (re-search-backward p4-blame-revision-regex nil t)
	      (let ((la (string-to-int (match-string 1)))
		    (lb (string-to-int (match-string 2)))
		    (op (match-string 3))
		    (ra (string-to-int (match-string 4)))
		    (rb (string-to-int (match-string 5))))
		(if (= lb 0)
		    (setq lb la))
		(if (= rb 0)
		    (setq rb ra))
		(cond ((string= op "a")
		       (setq la (1+ la)))
		      ((string= op "d")
		       (setq ra (1+ ra))))
		(save-excursion
		  (set-buffer ch-buffer)
		  (goto-line la)
		  (let ((beg (point)))
		    (forward-line (1+ (- lb la)))
		    (delete-region beg (point)))
		  (while (<= ra rb)
		    (insert ins-string)
		    (setq ra (1+ ra)))))))
	  (setq tmp-alst (cdr tmp-alst))))
      (p4-noinput-buffer-action "print" nil t
				(list (format "%s#%d" fullname head-rev))
				t)
      (p4-font-lock-buffer p4-output-buffer-name)
      (let (line cnum (old-cnum 0) change-data
	    xth-rev xth-date xth-auth xth-file)
	(save-excursion
	  (set-buffer buffer)
	  (goto-line 2)
	  (move-to-column 0)
	  (p4-insert-no-properties "Change  Rev       Date  Author\n")
	  (while (setq line (p4-read-depot-output ch-buffer))
	    (setq cnum (string-to-int line))
	    (if (= cnum old-cnum)
		(p4-insert-no-properties (format "%29s : " ""))

	      ;; extract the change data from our alist: remember,
	      ;; `eq' works for integers so we can use assq here:
	      (setq change-data (cdr (assq cnum ch-alist))
		    xth-rev     (nth P4-REV  change-data)
		    xth-date    (nth P4-DATE change-data)
		    xth-auth    (nth P4-AUTH change-data)
		    xth-file    (nth P4-FILE change-data))
	      
	      (p4-insert-no-properties
	       (format "%6d %4d %10s %7s: " cnum xth-rev xth-date xth-auth))
	      (move-to-column 0)
	      (if (looking-at p4-blame-index-regex)
		  (let ((nth-cnum (match-string 1))
			(nth-revn (match-string 2))
			(nth-user (match-string 4)))
		    (p4-create-active-link (match-beginning 1)
					   (match-end 1)
					   (list (cons 'change nth-cnum)))
		    ;; revision needs to be linked to a file now that we
		    ;; follow integrations (branches):
		    (p4-create-active-link (match-beginning 2)
					   (match-end 2)
					   (list (cons 'rev  nth-revn)
						 (cons 'link-depot-name xth-file)))
		    (p4-create-active-link (match-beginning 4)
					   (match-end 4)
					   (list (cons 'user nth-user)))
		    ;; truncate the user name:
		    (let ((start (+ (match-beginning 4) 7))
			  (end (match-end 4)))
		      (if (> end start)
			  (delete-region start end))))))
	    (setq old-cnum cnum)
	    (forward-line))))

      (kill-buffer ch-buffer))
    (let ((buffer-name (concat "*P4 print-revs " file-name "*")))
      (p4-activate-print-buffer buffer-name nil)
      (save-excursion
	(set-buffer buffer-name)
	(setq truncate-lines t)
	(use-local-map p4-print-rev-map)))))

;; The p4 refresh command
(defp4cmd p4-refresh ()
  "sync" "Refresh the contents of an unopened file. \\[p4-refresh].

This is equivalent to \"sync -f\"
"
  (interactive)
  (let ((args (p4-buffer-file-name)))
    (if (or current-prefix-arg (not args))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 refresh: ")))
      (setq args (list args)))
    (p4-noinput-buffer-action "refresh" nil t args)
    (p4-refresh-files-in-buffers)
    (p4-make-depot-list-buffer
     (concat "*P4 Refresh: (" (p4-current-client) ") " (car args) "*"))))

;; The p4 get/sync command
(defp4cmd p4-sync ()
  "sync"
  "To synchronise the local view with the depot, type \\[p4-get].\n"
  (interactive)
  (p4-get))

(defp4cmd p4-get ()
  "sync"
  "To synchronise the local view with the depot, type \\[p4-get].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string (p4-read-arg-string "p4 get: "))))
    (p4-noinput-buffer-action "get" nil t args)
    (p4-refresh-files-in-buffers)
    (p4-make-depot-list-buffer
     (concat "*P4 Get: (" (p4-current-client) ") " (car args) "*"))))

;; The p4 have command
(defp4cmd p4-have ()
  "have" "To list revisions last gotten, type \\[p4-have].\n"
  (interactive)
  (let ((args (list "...")))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 have: " (p4-buffer-file-name-2)))))
    (p4-noinput-buffer-action "have" nil t args)
    (p4-make-depot-list-buffer
     (concat "*P4 Have: (" (p4-current-client) ") " (car args) "*"))))

;; The p4 changes command
(defp4cmd p4-changes ()
  "changes" "To list changes, type \\[p4-changes].\n"
  (interactive)
  (let ((arg-list (list "-m" "200" "...")))
    (if current-prefix-arg
	(setq arg-list (p4-make-list-from-string
			(p4-read-arg-string "p4 changes: " "-m 200"))))
    (p4-file-change-log "changes" arg-list)))

;; The p4 help command
(defp4cmd p4-help (arg)
  "help" "To print help message, type \\[p4-help].

Argument ARG command for which help is needed.
"
  (interactive (list (p4-make-list-from-string
		      (p4-read-arg-string "Help on which command: "
					  nil "help"))))
  (p4-noinput-buffer-action "help" nil t arg)
  (p4-make-basic-buffer "*P4 help*"))

(defun p4-make-basic-buffer (buf-name &optional map)
  "rename `p4-output-buffer-name' to buf-name \(which will be killed first if
it already exists\), set its local map to map, if specified, or
`p4-basic-map' otherwise. Makes the buffer read only."
  (get-buffer-create buf-name)
  (kill-buffer buf-name)
  (set-buffer p4-output-buffer-name)
  (goto-char (point-min))
  (rename-buffer buf-name t)
  (use-local-map (if (keymapp map) map p4-basic-map))
  (setq buffer-read-only t)
  (p4-move-buffer-point-to-top buf-name))

;; The p4 info command
(defp4cmd p4-info ()
  "info" "To print out client/server information, type \\[p4-info].\n"
  (interactive)
  (p4-noinput-buffer-action "info" nil t)
  (p4-make-basic-buffer "*P4 info*"))

;; The p4 integrate command
(defp4cmd p4-integ ()
  "integ" "To schedule integrations between branches, type \\[p4-integ].\n"
  (interactive)
  (let ((args (p4-make-list-from-string
	       (p4-read-arg-string "p4 integ: " "-b "))))
    (p4-noinput-buffer-action "integ" nil t args)
    (p4-make-depot-list-buffer "*P4 integ*")))

(defp4cmd p4-resolve ()
  "resolve"
  "To merge open files with other revisions or files, type \\[p4-resolve].\n"
  (interactive)
  (let (buffer args (buf-name "*p4 resolve*"))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 resolve: " nil))))
    (setq buffer (get-buffer buf-name))
    (if (and (buffer-live-p buffer)
	     (not (comint-check-proc buffer)))
	(save-excursion
	  (let ((cur-dir default-directory))
	    (set-buffer buffer)
	    (cd cur-dir)
	    (goto-char (point-max))
	    (insert "\n--------\n\n"))))
    (setq args (cons "resolve" args))
    (setq buffer (apply 'make-comint "p4 resolve" p4-executable nil "-d" default-directory args))
    (set-buffer buffer)
    (comint-mode)
    (display-buffer buffer)
    (select-window (get-buffer-window buffer))
    (goto-char (point-max))))

(defp4cmd p4-rename ()
  "rename" "To rename a file in the depot, type \\[p4-rename].

This command will execute the integrate/delete commands automatically.
"
  (interactive)
  (let (from-file to-file)
    (setq from-file (p4-read-arg-string "rename from: " (p4-buffer-file-name-2)))
    (setq to-file (p4-read-arg-string "rename to: " (p4-buffer-file-name-2)))
    (p4-noinput-buffer-action "integ" nil t (list from-file to-file))
    (p4-exec-p4 (get-buffer-create p4-output-buffer-name)
		(list "delete" from-file)
		nil)))

(defun p4-scroll-down-1-line ()
  "Scroll down one line"
  (interactive)
  (scroll-down 1))

(defun p4-scroll-up-1-line ()
  "Scroll up one line"
  (interactive)
  (scroll-up 1))

(defun p4-scroll-down-1-window ()
  "Scroll down one window"
  (interactive)
  (scroll-down
   (- (window-height) next-screen-context-lines)))

(defun p4-scroll-up-1-window ()
  "Scroll up one window"
  (interactive)
  (scroll-up
   (- (window-height) next-screen-context-lines)))

(defun p4-top-of-buffer ()
  "Top of buffer"
  (interactive)
  (goto-char (point-min)))

(defun p4-bottom-of-buffer ()
  "Bottom of buffer"
  (interactive)
  (goto-char (point-max)))

(defun p4-delete-other-windows ()
  "Make buffer full height"
  (interactive)
  (delete-other-windows))

(defun p4-goto-next-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^====" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-goto-prev-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^====" nil "")
  (set-window-start (selected-window) (point)))

(defun p4-next-depot-file ()
  "Next file"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^//[^/@# ][^/@#]*/[^@#]+#[0-9]+ - " nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-prev-depot-file ()
  "Previous file"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^//[^/@# ][^/@#]*/[^@#]+#[0-9]+ - " nil "")
  (set-window-start (selected-window) (point)))


(defun p4-next-depot-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-prev-depot-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (set-window-start (selected-window) (point)))

(defun p4-moveto-print-rev-column (old-column)
  (let ((colon (save-excursion
		 (move-to-column 0)
		 (if (looking-at "[^:\n]*:")
		     (progn
		       (goto-char (match-end 0))
		       (current-column))
		   0))))
    (move-to-column old-column)
    (if (and (< (current-column) colon)
	     (re-search-forward "[^ ][ :]" nil t))
	(goto-char (match-beginning 0)))))

(defun p4-next-change-rev-line ()
  "Next change/revision line"
  (interactive)
  (let ((c (current-column)))
    (move-to-column 1)
    (re-search-forward "^ *[0-9]+ +[0-9]+[^:]+:" nil "")
    (p4-moveto-print-rev-column c)))

(defun p4-prev-change-rev-line ()
  "Previous change/revision line"
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (move-to-column 32)
    (re-search-backward "^ *[0-9]+ +[0-9]+[^:]*:" nil "")
    (p4-moveto-print-rev-column c)))

(defun p4-toggle-line-wrap ()
  "Toggle line wrap mode"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (save-window-excursion
    (recenter)))

(defun p4-quit-current-buffer (pnt)
  "Quit a buffer"
  (interactive "d")
  (if (not (one-window-p))
      (delete-window)
    (bury-buffer)))

(defun p4-buffer-mouse-clicked (event)
  "Function to translate the mouse clicks in a P4 filelog buffer to
character events"
  (interactive "e")
  (let (win pnt)
    (cond (p4-running-xemacs
	   (setq win (event-window event))
	   (setq pnt (event-point event)))
	  (p4-running-emacs
	   (setq win (posn-window (event-end event)))
	   (setq pnt (posn-point (event-start event)))))
    (select-window win)
    (goto-char pnt)
    (p4-buffer-commands pnt)))

(defun p4-buffer-mouse-clicked-3 (event)
  "Function to translate the mouse clicks in a P4 filelog buffer to
character events"
  (interactive "e")
  (let (win pnt)
    (cond (p4-running-xemacs
	   (setq win (event-window event))
	   (setq pnt (event-point event)))
	  (p4-running-emacs
	   (setq win (posn-window (event-end event)))
	   (setq pnt (posn-point (event-start event)))))
    (select-window win)
    (goto-char pnt)
    (let ((link-name (or (get-char-property pnt 'link-client-name)
			 (get-char-property pnt 'link-depot-name)))
	  (rev (get-char-property pnt 'rev)))
      (cond (link-name
	     (p4-diff))
	    (rev
	     (p4-diff2 rev "#head"))
	    (t
	     (error "No file to diff!"))))))

(defun p4-buffer-commands (pnt)
  "Function to get a given property and do the appropriate command on it"
  (interactive "d")
  (let ((rev (get-char-property pnt 'rev))
	(change (get-char-property pnt 'change))
	(action (get-char-property pnt 'action))
	(user (get-char-property pnt 'user))
	(group (get-char-property pnt 'group))
	(client (get-char-property pnt 'client))
	(label (get-char-property pnt 'label))
	(branch (get-char-property pnt 'branch))
	(filename (p4-buffer-file-name-2)))
    (cond ((and (not action) rev)
	   (let ((fn1 (concat filename "#" rev)))
	     (p4-noinput-buffer-action "print" nil t (list fn1))
	     (p4-activate-print-buffer "*P4 print*" t)))
	  (action
	   (let* ((rev2 (int-to-string (1- (string-to-int rev))))
		  (fn1 (concat filename "#" rev))
		  (fn2 (concat filename "#" rev2)))
	     (if (> (string-to-int rev2) 0)
		 (progn
		   (p4-noinput-buffer-action
		    "diff2" nil t
		    (append (p4-make-list-from-string
			     p4-default-diff-options)
			    (list fn2 fn1)))
		   (p4-activate-diff-buffer "*P4 diff*"))
	       (error "There is no earlier revision to diff."))))
	  (change (p4-describe-internal
		   (append (p4-make-list-from-string p4-default-diff-options)
			   (list change))))
	  (user (p4-async-process-command "user" nil
					  (concat
					   "*P4 User: " user "*")
					  "user" (list user)))
	  (client (p4-async-process-command
		   "client" "Description:\n\t"
		   (concat "*P4 Client: " client "*") "client" (list client)))
	  (label (p4-label (list label)))
	  (branch (p4-branch (list branch)))

	  ;; Check if a "filename link" or an active "diff buffer area" was
	  ;; selected.
	  (t
	   (let ((link-client-name (get-char-property pnt 'link-client-name))
		 (link-depot-name (get-char-property pnt 'link-depot-name))
		 (block-client-name (get-char-property pnt 'block-client-name))
		 (block-depot-name (get-char-property pnt 'block-depot-name))
		 (p4-history-for (get-char-property pnt 'history-for))
		 (first-line (get-char-property pnt 'first-line))
		 (start (get-char-property pnt 'start)))
	     (cond
	      (p4-history-for
	       (p4-file-change-log "filelog" (list p4-history-for)))
	      ((or link-client-name link-depot-name)
	       (p4-find-file-or-print-other-window
		link-client-name link-depot-name))
	      ((or block-client-name block-depot-name)
	       (if first-line
		   (let ((c (max 0 (- pnt
				      (save-excursion
					(goto-char pnt)
					(beginning-of-line)
					(point))
				      1)))
			 (r first-line))
		     (save-excursion
		       (goto-char start)
		       (while (re-search-forward "^[ +>].*\n" pnt t)
			 (setq r (1+ r))))
		     (p4-find-file-or-print-other-window
		      block-client-name block-depot-name)
		     (goto-line r)
		     (if (not block-client-name)
			 (forward-line 1))
		     (beginning-of-line)
		     (goto-char (+ (point) c)))
		 (p4-find-file-or-print-other-window
		  block-client-name block-depot-name)))
	      (t
	       (error "There is no file at that cursor location!"))))))))

(defun p4-find-file-or-print-other-window (client-name depot-name)
  (if client-name
      (find-file-other-window client-name)
    (p4-noinput-buffer-action "print" nil t
			      (list depot-name))
    (p4-activate-print-buffer depot-name t)
    (other-window 1)))

(defun p4-find-file-other-window ()
  "Open/print file"
  (interactive)
  (let ((link-client-name (get-char-property (point) 'link-client-name))
	(link-depot-name (get-char-property (point) 'link-depot-name))
	(block-client-name (get-char-property (point) 'block-client-name))
	(block-depot-name (get-char-property (point) 'block-depot-name)))
    (cond ((or link-client-name link-depot-name)
	   (p4-find-file-or-print-other-window
	    link-client-name link-depot-name)
	   (other-window 1))
	  ((or block-client-name block-depot-name)
	   (p4-find-file-or-print-other-window
	    block-client-name block-depot-name)
	   (other-window 1)))))

(defun p4-filelog-short-format ()
  "Short format"
  (interactive)
  (setq buffer-invisibility-spec t)
  (redraw-display))

(defun p4-filelog-long-format ()
  "Long format"
  (interactive)
  (setq buffer-invisibility-spec (list))
  (redraw-display))

(defun p4-scroll-down-1-line-other-w ()
  "Scroll other window down one line"
  (interactive)
  (scroll-other-window -1))

(defun p4-scroll-up-1-line-other-w ()
  "Scroll other window up one line"
  (interactive)
  (scroll-other-window 1))

(defun p4-scroll-down-1-window-other-w ()
  "Scroll other window down one window"
  (interactive)
  (scroll-other-window
   (- next-screen-context-lines (window-height))))

(defun p4-scroll-up-1-window-other-w ()
  "Scroll other window up one window"
  (interactive)
  (scroll-other-window
   (- (window-height) next-screen-context-lines)))

(defun p4-top-of-buffer-other-w ()
  "Top of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-min))
  (other-window -1))

(defun p4-bottom-of-buffer-other-w ()
  "Bottom of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun p4-goto-next-change ()
  "Next change"
  (interactive)
  (let ((c (current-column)))
    (forward-line 1)
    (while (get-char-property (point) 'invisible)
      (forward-line 1))
    (move-to-column c)))

(defun p4-goto-prev-change ()
  "Previous change"
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (while (get-char-property (point) 'invisible)
      (forward-line -1))
    (move-to-column c)))


;; Activate special handling for a buffer generated with a diff-like command
(p4-make-face 'p4-diff-file-face nil "gray90")
(p4-make-face 'p4-diff-head-face nil "gray95")
(p4-make-face 'p4-diff-ins-face "blue" nil)
(p4-make-face 'p4-diff-del-face "red" nil)
(p4-make-face 'p4-diff-change-face "dark green" nil)

(defun p4-buffer-set-face-property (regexp face-property)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 0))
	    (end (match-end 0)))
	(p4-set-extent-properties start end
				  (list (cons 'face face-property)))))))

(defun p4-activate-diff-buffer (buffer-name)
  (p4-make-depot-list-buffer buffer-name)
  (save-excursion
    (set-buffer buffer-name)
    (setq buffer-read-only nil)
    (if p4-colorized-diffs
	(progn
	  (p4-buffer-set-face-property "^=.*\n" 'p4-diff-file-face)
	  (p4-buffer-set-face-property "^[@*].*" 'p4-diff-head-face)
	  (p4-buffer-set-face-property "^\\([+>].*\n\\)+" 'p4-diff-ins-face)
	  (p4-buffer-set-face-property "^\\([-<].*\n\\)+" 'p4-diff-del-face)
	  (p4-buffer-set-face-property "^\\(!.*\n\\)+" 'p4-diff-change-face)))

    (goto-char (point-min))
    (while (re-search-forward "^\\(==== //\\).*\n"
			      nil t)
      (let* ((link-client-name (get-char-property (match-end 1) 'link-client-name))
	     (link-depot-name (get-char-property (match-end 1) 'link-depot-name))
	     (start (match-beginning 0))
	     (end (save-excursion
		    (if (re-search-forward "^==== " nil t)
			(match-beginning 0)
		      (point-max)))))
	(if link-client-name
	    (p4-set-extent-properties start end
				      (list (cons 'block-client-name
						  link-client-name))))
	(if link-depot-name
	    (p4-set-extent-properties start end
				      (list (cons 'block-depot-name
						  link-depot-name))))))

    (goto-char (point-min))
    (while (re-search-forward
	    (concat "^[@0-9].*\\([cad+]\\)\\([0-9]*\\).*\n"
		    "\\(\\(\n\\|[^@0-9\n].*\n\\)*\\)") nil t)
      (let ((first-line (string-to-int (match-string 2)))
	    (start (match-beginning 3))
	    (end (match-end 3)))
	(p4-set-extent-properties start end
				  (list (cons 'first-line first-line)
					(cons 'start start)))))

    (goto-char (point-min))
    (let ((stop
	   (if (re-search-forward "^\\(\\.\\.\\.\\|====\\)" nil t)
	       (match-beginning 0)
	     (point-max))))
      (p4-find-change-numbers buffer-name (point-min) stop))

    (goto-char (point-min))
    (if (looking-at "^Change [0-9]+ by \\([^ @]+\\)@\\([^ \n]+\\)")
	(let ((user-match 1)
	      (cl-match 2)
	      cur-user cur-client)
	  (setq cur-user (match-string user-match))
	  (setq cur-client (match-string cl-match))
	  (p4-create-active-link (match-beginning user-match)
				 (match-end user-match)
				 (list (cons 'user cur-user)))
	  (p4-create-active-link (match-beginning cl-match)
				 (match-end cl-match)
				 (list (cons 'client cur-client)))))

    (use-local-map p4-diff-map)
    (setq buffer-read-only t)))


;; The p4 describe command
(defp4cmd p4-describe ()
  "describe" "To get a description for a change number, type \\[p4-describe].\n"
  (interactive)
  (let ((arg-string (p4-make-list-from-string
		     (read-string "p4 describe: "
				  (concat p4-default-diff-options " ")))))
    (p4-describe-internal arg-string)))

;; Internal version of the p4 describe command
(defun p4-describe-internal (arg-string)
  (p4-noinput-buffer-action
   "describe" nil t arg-string)
  (p4-activate-diff-buffer
   (concat "*P4 describe: " (p4-list-to-string arg-string) "*")))

;; The p4 opened command
(defp4cmd p4-opened ()
  "opened"
  "To display list of files opened for pending change, type \\[p4-opened].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 opened: "
					(p4-buffer-file-name-2)))))
    (p4-opened-internal args)))

(defun p4-opened-internal (args)
  (let ((p4-client (p4-current-client)))
    (p4-noinput-buffer-action "opened" nil t args)
    (p4-make-depot-list-buffer (concat "*Opened Files: " p4-client "*"))))

(defun p4-update-opened-list ()
  (if (get-buffer-window (concat "*Opened Files: " (p4-current-client) "*"))
      (progn
	(setq current-prefix-arg nil)
	(p4-opened-internal nil))))

(defun p4-regexp-create-links (buffer-name regexp property)
  (save-excursion
    (set-buffer buffer-name)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 1))
	    (end (match-end 1))
	    (str (match-string 1)))
	(p4-create-active-link start end (list (cons property str)))))
    (setq buffer-read-only t)))

;; The p4 users command
(defp4cmd p4-users ()
  "users" "To display list of known users, type \\[p4-users].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 users: " nil "user"))))
    (p4-noinput-buffer-action "users" nil t args))
  (p4-make-basic-buffer "*P4 users*")
  (p4-regexp-create-links "*P4 users*" "^\\([^ ]+\\).*\n" 'user))

(defp4cmd p4-groups ()
  "groups" "To display list of known groups, type \\[p4-groups].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 groups: " nil "group"))))
    (p4-noinput-buffer-action "groups" nil t args))
  (p4-make-basic-buffer "*P4 groups*")
  (p4-regexp-create-links "*P4 groups*" "^\\(.*\\)\n" 'group))

;; The p4 jobs command
(defp4cmd p4-jobs ()
  "jobs" "To display list of jobs, type \\[p4-jobs].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string (p4-read-arg-string "p4 jobs: "))))
    (p4-noinput-buffer-action "jobs" nil t args))
  (p4-make-basic-buffer "*P4 jobs*"))

;; The p4 fix command
(defp4cmd p4-fix ()
  "fix" "To mark jobs as being fixed by a changelist number, type \\[p4-fix].\n"
  (interactive)
  (let ((args (p4-make-list-from-string (p4-read-arg-string "p4 fix: "
							    nil "job"))))
    (p4-noinput-buffer-action "fix" nil t args)))

;; The p4 fixes command
(defp4cmd p4-fixes ()
  "fixes" "To list what changelists fix what jobs, type \\[p4-fixes].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string (p4-read-arg-string "p4 fixes: "))))
    (p4-noinput-buffer-action "fixes" nil t args)
    (p4-make-basic-buffer "*P4 fixes*")))

;; The p4 where command
(defp4cmd p4-where ()
  "where"
  "To show how local file names map into depot names, type \\[p4-where].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 where: "
					(p4-buffer-file-name-2)))))
    (p4-noinput-buffer-action "where" nil 's args)))


(defun p4-async-process-command (p4-this-command &optional
						 p4-regexp
						 p4-this-buffer
						 p4-out-command
						 p4-in-args
						 p4-out-args)
  "Internal function to call an asynchronous process with a local buffer,
instead of calling an external client editor to run within emacs.

Arguments:
P4-THIS-COMMAND is the command that called this internal function.

P4-REGEXP is the optional regular expression to search for to set the cursor
on.

P4-THIS-BUFFER is the optional buffer to create. (Default is *P4 <command>*).

P4-OUT-COMMAND is the optional command that will be used as the command to
be called when `p4-async-call-process' is called.

P4-IN-ARGS is the optional argument passed that will be used as the list of
arguments to the P4-THIS-COMMAND.

P4-OUT-ARGS is the optional argument passed that will be used as the list of
arguments to P4-OUT-COMMAND."
  (let ((dir default-directory))
    (if p4-this-buffer
	(set-buffer (get-buffer-create p4-this-buffer))
      (set-buffer (get-buffer-create (concat "*P4 " p4-this-command "*"))))
    (setq p4-current-command p4-this-command)
    (cd dir))
  (if (zerop (apply 'call-process-region (point-min) (point-max)
		    (p4-check-p4-executable) t t nil
		    "-d" default-directory
		    p4-current-command "-o"
		    p4-in-args))
      (progn
	(goto-char (point-min))
	(insert (concat "# Created using " (p4-emacs-version) ".\n"
			"# Type C-c C-c to submit changes and exit buffer.\n"
			"# Type C-x k to kill current changes.\n"
			"#\n"))
	(if p4-regexp (re-search-forward p4-regexp))
	(indented-text-mode)
	(setq p4-async-minor-mode t)
	(setq fill-column 79)
	(p4-push-window-config)
	(switch-to-buffer-other-window (current-buffer))
	(if p4-out-command
	    (setq p4-current-command p4-out-command))
	(setq p4-current-args p4-out-args)
	(setq buffer-offer-save t)

	(define-key p4-async-minor-map "\C-c\C-c" 'p4-async-call-process)
	(run-hooks 'p4-async-command-hook)
	(set-buffer-modified-p nil)
	(message "C-c C-c to finish editing and exit buffer."))
    (error "%s %s -o failed to complete successfully."
	   (p4-check-p4-executable) p4-current-command)))

(defun p4-async-call-process ()
  "Internal function called by `p4-async-process-command' to process the
buffer after editing is done using the minor mode key mapped to `C-c C-c'."
  (interactive)
  (message "p4 %s ..." p4-current-command)
  (let ((max (point-max)) msg
	(current-command p4-current-command)
	(current-args p4-current-args))
    (goto-char max)
    (if (zerop (apply 'call-process-region (point-min)
		      max (p4-check-p4-executable)
		      nil '(t t) nil
		      "-d" default-directory
		      current-command "-i"
		      current-args))
	(progn
	  (goto-char max)
	  (setq msg (buffer-substring max (point-max)))
	  (delete-region max (point-max))
	  (save-excursion
	    (set-buffer (get-buffer-create p4-output-buffer-name))
	    (delete-region (point-min) (point-max))
	    (insert msg))
	  (kill-buffer nil)
	  (display-buffer p4-output-buffer-name)
	  (p4-partial-cache-cleanup current-command)
	  (message "p4 %s done." current-command)
	  (if (equal current-command "submit")
	      (progn
		(p4-refresh-files-in-buffers)
		(p4-check-mode-all-buffers)
		(if p4-notify
		    (p4-notify p4-notify-list)))))
      (error "%s %s -i failed to complete successfully."
	     (p4-check-p4-executable)
	     current-command))))

(defun p4-cmd-line-flags (args)
  (memq t (mapcar (lambda (x) (not (not (string-match "^-" x))))
		  args)))

;; The p4 change command
(defp4cmd p4-change ()
  "change" "To edit the change specification, type \\[p4-change].\n"
  (interactive)
  (let (args
	(change-buf-name "*P4 New Change*"))
    (if (buffer-live-p (get-buffer change-buf-name))
	(switch-to-buffer-other-window (get-buffer change-buf-name))
      (if current-prefix-arg
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 change: " nil))))
      (if (p4-cmd-line-flags args)
	  (p4-noinput-buffer-action "change" nil t args)
	(p4-async-process-command "change" "Description:\n\t"
				  change-buf-name nil args)))))

;; The p4 client command
(defp4cmd p4-client ()
  "client" "To edit a client specification, type \\[p4-client].\n"
  (interactive)
  (let (args
	(client-buf-name "*P4 client*"))
    (if (buffer-live-p (get-buffer client-buf-name))
	(switch-to-buffer-other-window (get-buffer client-buf-name))
      (if current-prefix-arg
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 client: " nil "client"))))
      (if (p4-cmd-line-flags args)
	  (p4-noinput-buffer-action "client" nil t args)
	(p4-async-process-command "client" "\\(Description\\|View\\):\n\t"
				  client-buf-name nil args)))))

(defp4cmd p4-clients ()
  "clients" "To list all clients, type \\[p4-clients].\n"
  (interactive)
  (p4-noinput-buffer-action "clients" nil t nil)
  (p4-make-basic-buffer "*P4 clients*")
  (p4-regexp-create-links "*P4 clients*" "^Client \\([^ ]+\\).*\n" 'client))

(defp4cmd p4-branch (args)
  "branch" "Edit a P4-BRANCH specification using \\[p4-branch]."
  (interactive (list
		(p4-make-list-from-string
		 (p4-read-arg-string "p4 branch: " nil "branch"))))
  (if (or (null args) (equal args (list "")))
      (error "Branch must be specified!")
    (if (p4-cmd-line-flags args)
	(p4-noinput-buffer-action "branch" nil t args)
      (p4-async-process-command "branch" "Description:\n\t"
				(concat "*P4 Branch: "
					(car (reverse args)) "*")
				"branch" args))))

(defp4cmd p4-branches ()
  "branches" "To list all branches, type \\[p4-branches].\n"
  (interactive)
  (p4-noinput-buffer-action "branches" nil t nil)
  (p4-make-basic-buffer "*P4 branches*")
  (p4-regexp-create-links "*P4 branches*" "^Branch \\([^ ]+\\).*\n" 'branch))

(defp4cmd p4-label (args)
  "label" "Edit a P4-label specification using \\[p4-label].\n"
  (interactive (list
		(p4-make-list-from-string
		 (p4-read-arg-string "p4 label: " nil "label"))))
  (if (or (null args) (equal args (list "")))
      (error "label must be specified!")
    (if (p4-cmd-line-flags args)
	(p4-noinput-buffer-action "label" nil t args)
      (p4-async-process-command "label" "Description:\n\t"
				(concat "*P4 label: "
					(car (reverse args)) "*")
				"label" args))))

(defp4cmd p4-labels ()
  "labels" "To display list of defined labels, type \\[p4-labels].\n"
  (interactive)
  (p4-noinput-buffer-action "labels" nil t nil)
  (p4-make-basic-buffer "*P4 labels*")
  (p4-regexp-create-links "*P4 labels*" "^Label \\([^ ]+\\).*\n" 'label))

;; The p4 labelsync command
(defp4cmd p4-labelsync ()
  "labelsync"
  "To synchronize a label with the current client contents, type \\[p4-labelsync].\n"
  (interactive)
  (let ((args (p4-make-list-from-string
	       (p4-read-arg-string "p4 labelsync: "))))
    (p4-noinput-buffer-action "labelsync" nil t args))
  (p4-make-depot-list-buffer "*P4 labelsync*"))

(defun p4-filter-out (pred lst)
  (let (res)
    (while lst
      (if (not (funcall pred (car lst)))
	  (setq res (cons (car lst) res)))
      (setq lst (cdr lst)))
    (reverse res)))

;; The p4 submit command
(defp4cmd p4-submit (&optional arg)
  "submit" "To submit a pending change to the depot, type \\[p4-submit].\n"
  (interactive "P")
  (let (args
	(submit-buf-name "*P4 Submit*")
	(change-list (if (integerp arg) arg)))
    (if (buffer-live-p (get-buffer submit-buf-name))
	(switch-to-buffer-other-window (get-buffer submit-buf-name))
      (if change-list
	  (setq args (list "-c" (int-to-string change-list)))
	(if current-prefix-arg
	    (setq args (p4-make-list-from-string
			(p4-read-arg-string "p4 submit: " nil)))))
      (setq args (p4-filter-out (lambda (x) (string= x "-c")) args))
      (p4-save-opened-files)
      (if (or (not (and p4-check-empty-diffs (p4-empty-diff-p)))
	      (progn
		(ding t)
		(yes-or-no-p
		 "File with empty diff opened for edit. Submit anyway? ")))
	  (p4-async-process-command "change" "Description:\n\t"
				    submit-buf-name "submit" args)))))

;; The p4 user command
(defp4cmd p4-user ()
  "user" "To create or edit a user specification, type \\[p4-user].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 user: " nil "user"))))
    (if (p4-cmd-line-flags args)
	(p4-noinput-buffer-action "user" nil t args)
      (p4-async-process-command "user" nil nil nil args))))

;; The p4 group command
(defp4cmd p4-group ()
  "group" "To create or edit a group specification, type \\[p4-group].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 group: " nil "group"))))
    (if (p4-cmd-line-flags args)
	(p4-noinput-buffer-action "group" nil t args)
      (p4-async-process-command "group" nil nil nil args))))

;; The p4 job command
(defp4cmd p4-job ()
  "job" "To create or edit a job, type \\[p4-job].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 job: " nil "job"))))
    (if (p4-cmd-line-flags args)
	(p4-noinput-buffer-action "job" nil t args)
      (p4-async-process-command "job" "Description:\n\t" nil nil args))))

;; The p4 jobspec command
(defp4cmd p4-jobspec ()
  "jobspec" "To edit the job template, type \\[p4-jobspec].\n"
  (interactive)
  (p4-async-process-command "jobspec"))

;; A function to set the current P4 client name
(defun p4-set-client-name (p4-new-client-name)
  "To set the current value of P4CLIENT, type \\[p4-set-client-name].

This will change the current client from the previous client to the new
given value.

Setting this value to nil would disable P4 Version Checking.

`p4-set-client-name' will complete any client names set using the function
`p4-set-my-clients'. The strictness of completion will depend on the
variable `p4-strict-complete' (default is t).

Argument P4-NEW-CLIENT-NAME The new client to set to. The default value is
the current client."
  (interactive (list
		(completing-read "Change Client to: "
				 (if p4-my-clients
				     p4-my-clients
				   'p4-clients-completion)
				 nil p4-strict-complete (p4-current-client))
		))
  (if (or (null p4-new-client-name) (equal p4-new-client-name "nil"))
      (progn
	(setenv "P4CLIENT" nil)
	(if (not (getenv "P4CONFIG"))
	    (message
	     "P4 Version check disabled. Set a valid client name to enable."
	     )))
    (setenv "P4CLIENT" p4-new-client-name)
    (message "P4CLIENT changed to %s" p4-new-client-name)
    (run-hooks 'p4-set-client-hooks)))

(defun p4-get-client-config ()
  "To get the current value of the environment variable P4CONFIG,
type \\[p4-get-client-config].

This will be the current configuration that is in use for access through
Emacs P4."

  (interactive)
  (message "P4CONFIG is %s" (getenv "P4CONFIG")))

(defun p4-set-client-config (p4config)
  "To set the P4CONFIG variable, for use with the current versions of the p4
client.

P4CONFIG is a more flexible mechanism wherein p4 will find the current
client automatically by checking the config file found at the root of a
directory \(recursing all the way to the top\).

In this scenario, a P4CLIENT variable need not be explicitly set.
"
  (interactive "sP4 Config: ")
  (if (or (null p4config) (equal p4config ""))
      (message "P4CONFIG not changed.")
    (setenv "P4CONFIG" p4config)
    (message "P4CONFIG changed to %s" p4config)))

(defun p4-set-my-clients (client-list)
  "To set the client completion list used by `p4-set-client-name', use
this function in your .emacs (or any lisp interaction buffer).

This will change the current client list from the previous list to the new
given value.

Setting this value to nil would disable client completion by
`p4-set-client-name'.

The strictness of completion will depend on the variable
`p4-strict-complete' (default is t).

Argument CLIENT-LIST is the 'list' of clients.

To set your clients using your .emacs, use the following:

\(load-library \"p4\"\)
\(p4-set-my-clients \'(client1 client2 client3)\)"
  (setq p4-my-clients nil)
  (let (p4-tmp-client-var)
    (while client-list
      (setq p4-tmp-client-var (format "%s" (car client-list)))
      (setq client-list (cdr client-list))
      (setq p4-my-clients (append p4-my-clients
				  (list (list p4-tmp-client-var)))))))

;; A function to get the current P4PORT
(defun p4-get-p4-port ()
  "To get the current value of the environment variable P4PORT, type \
\\[p4-get-p4-port].

This will be the current server/port that is in use for access through Emacs
P4."
  (interactive)
  (let ((port (p4-current-server-port)))
    (message "P4PORT is [local: %s], [global: %s]" port (getenv "P4PORT"))
    port))

;; A function to set the current P4PORT
(defun p4-set-p4-port (p4-new-p4-port)
  "To set the current value of P4PORT, type \\[p4-set-p4-port].

This will change the current server from the previous server to the new
given value.

Argument P4-NEW-P4-PORT The new server:port to set to. The default value is
the current value of P4PORT."
  (interactive (list (let
			 ((symbol (read-string "Change server:port to: "
					       (getenv "P4PORT"))))
		       (if (equal symbol "")
			   (getenv "P4PORT")
			 symbol))))
  (if (or (null p4-new-p4-port) (equal p4-new-p4-port "nil"))
      (progn
	(setenv "P4PORT" nil)
	(if (not (getenv "P4CONFIG"))
	    (message
	     "P4 Version check disabled. Set a valid server:port to enable.")))
    (setenv "P4PORT" p4-new-p4-port)
    (message "P4PORT changed to %s" p4-new-p4-port)))

;; The find-file hook for p4.
(defun p4-find-file-hook ()
  "To check while loading the file, if it is a P4 version controlled file."
  (if (or (getenv "P4CONFIG") (getenv "P4CLIENT"))
      (p4-detect-p4)))

(defun p4-refresh-refresh-list (buffile bufname)
  "Refresh the list of files to be refreshed."
  (setq p4-all-buffer-files (delete (list buffile bufname)
				    p4-all-buffer-files))
  (if (not p4-all-buffer-files)
      (progn
	(if (and p4-running-emacs (timerp p4-file-refresh-timer))
	    (cancel-timer p4-file-refresh-timer))
	(if (and p4-running-xemacs p4-file-refresh-timer)
	    (disable-timeout p4-file-refresh-timer))
	(setq p4-file-refresh-timer nil))))

;; Set keymap. We use the C-x p Keymap for all perforce commands
(defvar p4-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'p4-add)
    (define-key map "b" 'p4-bug-report)
    (define-key map "B" 'p4-branch)
    (define-key map "c" 'p4-client)
    (define-key map "C" 'p4-changes)
    (define-key map "d" 'p4-diff2)
    (define-key map "D" 'p4-describe)
    (define-key map "e" 'p4-edit)
    (define-key map "E" 'p4-reopen)
    (define-key map "\C-f" 'p4-depot-find-file)
    (define-key map "f" 'p4-filelog)
    (define-key map "F" 'p4-files)
    (define-key map "g" 'p4-get-client-name)
    (define-key map "G" 'p4-get)
    (define-key map "h" 'p4-help)
    (define-key map "H" 'p4-have)
    (define-key map "i" 'p4-info)
    (define-key map "I" 'p4-integ)
    (define-key map "j" 'p4-job)
    (define-key map "J" 'p4-jobs)
    (define-key map "l" 'p4-label)
    (define-key map "L" 'p4-labels)
    (define-key map "\C-l" 'p4-labelsync)
    (define-key map "m" 'p4-rename)
    (define-key map "n" 'p4-notify)
    (define-key map "o" 'p4-opened)
    (define-key map "p" 'p4-print)
    (define-key map "P" 'p4-set-p4-port)
    (define-key map "q" 'p4-pop-window-config)
    (define-key map "r" 'p4-revert)
    (define-key map "R" 'p4-refresh)
    (define-key map "\C-r" 'p4-resolve)
    (define-key map "s" 'p4-set-client-name)
    (define-key map "S" 'p4-submit)
    (define-key map "t" 'p4-toggle-vc-mode)
    (define-key map "u" 'p4-user)
    (define-key map "U" 'p4-users)
    (define-key map "v" 'p4-emacs-version)
    (define-key map "V" 'p4-blame)
    (define-key map "w" 'p4-where)
    (define-key map "x" 'p4-delete)
    (define-key map "X" 'p4-fix)
    (define-key map "=" 'p4-diff)
    (define-key map "-" 'p4-ediff)
    (define-key map "?" 'p4-describe-bindings)
    map)
  "The Prefix for P4 Library Commands.")

(if (not (keymapp (lookup-key global-map "\C-xp")))
    (define-key global-map "\C-xp" p4-prefix-map))

;; For users interested in notifying a change, a notification list can be
;; set up using this function.
(defun p4-set-notify-list (p4-new-notify-list &optional p4-supress-stat)
  "To set the current value of P4NOTIFY, type \\[p4-set-notify-list].

This will change the current notify list from the existing list to the new
given value.

An empty string will disable notification.

Argument P4-NEW-NOTIFY-LIST is new value of the notification list.
Optional argument P4-SUPRESS-STAT when t will suppress display of the status
message. "

  (interactive (list (let
			 ((symbol (read-string
				   "Change Notification List to: "
				   p4-notify-list)))
		       (if (equal symbol "")
			   nil
			 symbol))))
  (let ((p4-old-notify-list p4-notify-list))
    (setenv "P4NOTIFY" p4-new-notify-list)
    (setq p4-notify-list p4-new-notify-list)
    (setq p4-notify (not (null p4-new-notify-list)))
    (if (not p4-supress-stat)
	(message "Notification list changed from '%s' to '%s'"
		 p4-old-notify-list p4-notify-list))))

;; To get the current notification list.
(defun p4-get-notify-list ()
  "To get the current value of the environment variable P4NOTIFY,
type \\[p4-get-notify-list].

   This will be the current notification list that is in use for mailing
   change notifications through Emacs P4."

  (interactive)
  (message "P4NOTIFY is %s" p4-notify-list))

(defun p4-notify (users)
  "To notify a list of users of a change submission manually, type
\\[p4-notify].

To do auto-notification, set the notification list with `p4-set-notify-list'
and on each submission, the users in the list will be notified of the
change.

Since this uses the sendmail program, it is mandatory to set the correct
path to the sendmail program in the variable `p4-sendmail-program'.

Also, it is mandatory to set the user's email address in the variable
`p4-user-email'.

Argument USERS The users to notify to. The default value is the notification
list."
  (interactive (list (let
			 ((symbol (read-string "Notify whom? "
					       p4-notify-list)))
		       (if (equal symbol "")
			   nil
			 symbol))))
  (p4-set-notify-list users t)
  (if (and p4-sendmail-program p4-user-email)
      (p4-do-notify)
    (message "Please set p4-sendmail-program and p4-user-email variables.")))

(defun p4-do-notify ()
  "This is the internal notification function called by `p4-notify'."
  (save-excursion
    (if (and p4-notify-list (not (equal p4-notify-list "")))
	(save-excursion
	  (set-buffer (get-buffer-create p4-output-buffer-name))
	  (goto-char (point-min))
	  (if (re-search-forward "[0-9]+.*submitted" (point-max) t)
	      (let (p4-matched-change)
		(setq p4-matched-change (substring (match-string 0) 0 -10))
		(set-buffer (get-buffer-create "*P4 Notify*"))
		(delete-region (point-min) (point-max))
		(call-process-region (point-min) (point-max)
				     (p4-check-p4-executable)
				     t t nil
				     "-d" default-directory
				     "describe" "-s"
				     p4-matched-change)
		(switch-to-buffer "*P4 Notify*")
		(goto-char (point-min))
		(let (p4-chg-desc)
		  (if (re-search-forward "^Change.*$" (point-max) t)
		      (setq p4-chg-desc (match-string 0))
		    (setq p4-chg-desc (concat
				       "Notification of Change "
				       p4-matched-change)))
		  (goto-char (point-min))
		  (insert
		   "From: " p4-user-email "\n"
		   "To: P4 Notification Recipients:;\n"
		   "Subject: " p4-chg-desc "\n")
		  (call-process-region (point-min) (point-max)
				       p4-sendmail-program t t nil
				       "-odi" "-oi" p4-notify-list)

		  (kill-buffer nil)))
	    (save-excursion
	      (set-buffer (get-buffer-create p4-output-buffer-name))
	      (goto-char (point-max))
	      (insert "\np4-do-notify: No Change Submissions found."))))
      (save-excursion
	(set-buffer (get-buffer-create p4-output-buffer-name))
	(goto-char (point-max))
	(insert "\np4-do-notify: Notification list not set.")))))

;; Function to return the current version.
(defun p4-emacs-version ()
  "Return the current Emacs-P4 Integration version."
  (interactive)
  (message (concat (cond (p4-running-xemacs "X")) "Emacs-P4 Integration v%s")
	   p4-emacs-version))

(defun p4-find-p4-config-file ()
  (let ((p4config (getenv "P4CONFIG"))
	(p4-cfg-dir (cond ((p4-buffer-file-name)
			   (file-name-directory
			    (file-truename (p4-buffer-file-name))))
			  (t (file-truename default-directory)))))
    (if (not p4config)
	nil
      (let (found at-root)
	(while (not (or found at-root))
	  (let ((parent-dir (file-name-directory
			     (directory-file-name
			      p4-cfg-dir))))
	    (if (file-exists-p (concat p4-cfg-dir p4config))
		(setq found (concat p4-cfg-dir p4config)))
	    (setq at-root (string-equal parent-dir p4-cfg-dir))
	    (setq p4-cfg-dir parent-dir)))
	found))))

(defun p4-detect-p4 ()
  (if (or (not p4-use-p4config-exclusively)
	  (p4-find-p4-config-file))
      (p4-check-mode)))

(defun p4-get-add-branch-files (&optional name-list)
  (let ((output-buffer (p4-depot-output "opened" name-list))
	files depot-map)
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^\\(//[^/@#]+/[^#\n]*\\)#[0-9]+ - add " nil t)
	(setq files (cons (cons (match-string 1) "Add")
			  files)))
      (goto-char (point-min))
      (while (re-search-forward "^\\(//[^/@#]+/[^#\n]*\\)#[0-9]+ - branch " nil t)
	(setq files (cons (cons (match-string 1) "Branch")
			  files))))
    (kill-buffer output-buffer)
    (setq depot-map (p4-map-depot-files (mapcar 'car files)))
    (mapcar (lambda (x) (cons (cdr (assoc (car x) depot-map))
			      (cdr x))) files)))

(defun p4-get-have-files (file-list)
  (let ((output-buffer (p4-depot-output "have" file-list))
	line files depot-map elt)
    (while (setq line (p4-read-depot-output output-buffer))
      (if (string-match "^\\(//[^/@#]+/[^#\n]*\\)#\\([0-9]+\\) - " line)
	  (setq files (cons (cons (match-string 1 line)
				  (match-string 2 line))
			    files))))
    (kill-buffer output-buffer)
    (setq depot-map (p4-map-depot-files (mapcar 'car files)))
    (setq files (mapcar (lambda (x) (cons (cdr (assoc (car x) depot-map))
					  (cdr x))) files))
    (while file-list
      (setq elt (car file-list))
      (setq file-list (cdr file-list))
      (if (not (assoc elt files))
	  (setq files (cons (cons elt nil) files))))
    files))

;; A function to check if the file being opened is version controlled by p4.
(defun p4-is-vc (&optional file-mode-cache filename)
  "If a file is controlled by P4 then return version else return nil."
  (if (not filename)
      (setq filename (p4-buffer-file-name)))
  (let (version done)
    (let ((el (assoc filename file-mode-cache)))
      (setq done el)
      (setq version (cdr el)))
    (if (and (not done) filename)
	(let ((output-buffer (p4-depot-output "have" (list filename)))
	      line)
	  (setq line (p4-read-depot-output output-buffer))
	  (kill-buffer output-buffer)
	  (if (string-match "^//[^/@#]+/[^#\n]*#\\([0-9]+\\) - " line)
	      (setq version (match-string 1 line)))
	  (setq done version)))
    (if (and (not done) (not file-mode-cache))
	(progn
	  (setq file-mode-cache
		(p4-get-add-branch-files (and filename (list filename))))
	  (setq version (cdr (assoc filename file-mode-cache)))))
    version))

(defun p4-check-mode (&optional file-mode-cache)
  "Check to see whether we should export the menu map to this buffer.

Turning on P4 mode calls the hooks in the variable `p4-mode-hook' with
no args."
  (setq p4-mode nil)
  (if p4-do-find-file
      (progn
	(setq p4-vc-check (p4-is-vc file-mode-cache))
	(if p4-vc-check
	    (progn
	      (p4-menu-add)
	      (setq p4-mode (concat " P4:" p4-vc-check))))
	(p4-force-mode-line-update)
	(let ((buffile (p4-buffer-file-name))
	      (bufname (buffer-name)))
	  (if (and p4-vc-check (not (member (list buffile bufname)
					    p4-all-buffer-files)))
	      (add-to-list 'p4-all-buffer-files (list buffile bufname))))
	(if (and (not p4-file-refresh-timer) (not (= p4-file-refresh-timer-time 0)))
	    (setq p4-file-refresh-timer
		  (cond (p4-running-emacs
			 (run-at-time nil p4-file-refresh-timer-time
				      'p4-refresh-files-in-buffers))
			(p4-running-xemacs
			 (add-timeout p4-file-refresh-timer-time
				      'p4-refresh-files-in-buffers nil
				      p4-file-refresh-timer-time)))))
	;; run hooks
	(and p4-vc-check (run-hooks 'p4-mode-hook))
	p4-vc-check)))

(defun p4-refresh-files-in-buffers (&optional arg)
  "Check to see if all the files that are under P4 version control are
actually up-to-date, if in buffers, or need refreshing."
  (interactive)
  (let ((p4-all-my-files p4-all-buffer-files) buffile bufname thiselt)
    (while p4-all-my-files
      (setq thiselt (car p4-all-my-files))
      (setq p4-all-my-files (cdr p4-all-my-files))
      (setq buffile (car thiselt))
      (setq bufname (cadr thiselt))
      (if (buffer-live-p (get-buffer bufname))
	  (save-excursion
	    (let ((buf (get-buffer bufname)))
	      (set-buffer buf)
	      (if p4-auto-refresh
		  (if (not (buffer-modified-p buf))
		      (if (not (verify-visited-file-modtime buf))
			  (if (file-readable-p buffile)
			      (revert-buffer t t)
			    (p4-check-mode))))
		(if (file-readable-p buffile)
		    (find-file-noselect buffile t)
		  (p4-check-mode)))
	      (setq buffer-read-only (not (file-writable-p
					   (p4-buffer-file-name))))))
	(p4-refresh-refresh-list buffile bufname)))))

(defun p4-check-mode-all-buffers ()
  "Call p4-check-mode for all buffers under P4 version control"
  (let ((p4-all-my-files p4-all-buffer-files) buffile bufname thiselt
	file-mode-cache)
    (if (and p4-all-my-files p4-do-find-file)
	(setq file-mode-cache
	      (append (p4-get-add-branch-files)
		      (p4-get-have-files (mapcar 'car p4-all-my-files)))))
    (while p4-all-my-files
      (setq thiselt (car p4-all-my-files))
      (setq p4-all-my-files (cdr p4-all-my-files))
      (setq buffile (car thiselt))
      (setq bufname (cadr thiselt))
      (if (buffer-live-p (get-buffer bufname))
	  (save-excursion
	    (set-buffer (get-buffer bufname))
	    (p4-check-mode file-mode-cache))
	(p4-refresh-refresh-list buffile bufname)))))

;; Force mode line updation for different Emacs versions
(defun p4-force-mode-line-update ()
  "To Force the mode line update for different flavors of Emacs."
  (cond (p4-running-xemacs
	 (redraw-modeline))
	(p4-running-emacs
	 (force-mode-line-update))))

;; In case, the P4 server is not available, or when operating off-line, the
;; p4-find-file-hook becomes a pain... this functions toggles the use of the
;; hook when opening files.

(defun p4-toggle-vc-mode ()
  "In case, the P4 server is not available, or when working off-line, toggle
the VC check on/off when opening files."
  (interactive)
  (setq p4-do-find-file (not p4-do-find-file))
  (message (concat "P4 mode check " (if p4-do-find-file
					"enabled."
				      "disabled."))))

;; Wrap C-x C-q to allow p4-edit/revert and also to ensure that
;; we don't stomp on vc-toggle-read-only.

(defun p4-toggle-read-only (&optional arg)
  "If p4-mode is non-nil, \\[p4-toggle-read-only] toggles between `p4-edit'
and `p4-revert'. If ARG is non-nil, p4-offline-mode will be enabled for this
buffer before the toggling takes place. In p4-offline-mode, toggle between
making the file writable and write protected."
  (interactive "P")
  (if (and arg p4-mode)
      (setq p4-mode nil
	    p4-offline-mode t))
  (cond
   (p4-mode
    (if buffer-read-only
	(p4-edit p4-verbose)
      (p4-revert p4-verbose)))
   (p4-offline-mode
    (toggle-read-only)
    (if buffer-file-name
	(let ((mode (file-modes buffer-file-name)))
	  (if buffer-read-only
	      (setq mode (logand mode (lognot 128)))
	    (setq mode (logior mode 128)))
	  (set-file-modes buffer-file-name mode))))))

(defun p4-browse-web-page ()
  "Browse the p4.el web page."
  (interactive)
  (require 'browse-url)
  (browse-url p4-web-page))

(defun p4-bug-report ()
  (interactive)
  (if (string-match " 19\\." (emacs-version))
      ;; unfortunately GNU Emacs 19.x doesn't have compose-mail
      (mail nil p4-emacs-maintainer (concat "BUG REPORT: "
					    (p4-emacs-version)))
    (compose-mail p4-emacs-maintainer (concat "BUG REPORT: "
					      (p4-emacs-version))))
  (goto-char (point-min))
  (re-search-forward (concat "^" (regexp-quote mail-header-separator) "\n"))
  ;; Insert warnings for novice users.
  (insert
   "This bug report will be sent to the P4-Emacs Integration Maintainer,\n"
   p4-emacs-maintainer "\n\n")
  (insert (concat (emacs-version) "\n\n"))
  (insert "A brief description of the problem and how to reproduce it:\n")
  (save-excursion
    (let ((message-buf (get-buffer
			(cond (p4-running-xemacs " *Message-Log*")
			      (p4-running-emacs "*Messages*")))))
      (if message-buf
	  (let (beg-pos
		(end-pos (point-max)))
	    (save-excursion
	      (set-buffer message-buf)
	      (goto-char end-pos)
	      (forward-line -10)
	      (setq beg-pos (point)))
	    (insert "\n\nRecent messages:\n")
	    (insert-buffer-substring message-buf beg-pos end-pos))))))

(defun p4-describe-bindings ()
  "A function to list the key bindings for the p4 prefix map"
  (interactive)
  (save-excursion
    (p4-push-window-config)
    (let ((map (make-sparse-keymap))
	  (p4-bindings-buffer "*P4 key bindings*"))
      (get-buffer-create p4-bindings-buffer)
      (cond
       (p4-running-xemacs
	(set-buffer p4-bindings-buffer)
	(delete-region (point-min) (point-max))
	(insert "Key Bindings for P4 Mode\n------------------------\n")
	(describe-bindings-internal p4-prefix-map))
       (p4-running-emacs
	(kill-buffer p4-bindings-buffer)
	(describe-bindings "\C-xp")
	(set-buffer "*Help*")
	(rename-buffer p4-bindings-buffer)))
      (define-key map "q" 'p4-quit-current-buffer)
      (use-local-map map)
      (display-buffer p4-bindings-buffer))))

;; Break up a string into a list of words
;; (p4-make-list-from-string "ab c de  f") -> ("ab" "c" "de" "f")
(defun p4-make-list-from-string (str)
  (let (lst)
    (while (or (string-match "^ *\"\\([^\"]*\\)\"" str)
	       (string-match "^ *\'\\([^\']*\\)\'" str)
	       (string-match "^ *\\([^ ]+\\)" str))
      (setq lst (append lst (list (match-string 1 str))))
      (setq str (substring str (match-end 0))))
    lst))

(defun p4-list-to-string (lst)
  (mapconcat (lambda (x) x) lst " "))

;; Return the file name associated with a buffer. If the real buffer file
;; name doesn't exist, try special filename tags set in some of the p4
;; buffers.
(defun p4-buffer-file-name-2 ()
  (cond ((p4-buffer-file-name))
	((get-char-property (point) 'link-client-name))
	((get-char-property (point) 'link-depot-name))
	((get-char-property (point) 'block-client-name))
	((get-char-property (point) 'block-depot-name))
	((if (and (fboundp 'dired-get-filename)
		  (dired-get-filename nil t))
	     (p4-follow-link-name (dired-get-filename nil t))))))

(defun p4-buffer-file-name ()
  (cond (buffer-file-name
	 (p4-follow-link-name buffer-file-name))
	(t nil)))

(defun p4-follow-link-name (name)
  (p4-cygpath
  (if p4-follow-symlinks
      (file-truename name)
     name)))

(defun p4-cygpath (name)
  (if (memq system-type '(cygwin32))
      (replace-in-string (exec-to-string (format "%s -w %s" p4-cygpath-exec name)) "\n" "")
    name))

(defvar p4-depot-filespec-history nil
  "History for p4-depot filespecs.")

(defvar p4-depot-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a filespec and
cdr is the list of anwers")

(defvar p4-branches-history nil
  "History for p4 clients.")

(defvar p4-branches-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-clients-history nil
  "History for p4 clients.")

(defvar p4-clients-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-jobs-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a job and
cdr is the list of answers??")

(defvar p4-labels-history nil
  "History for p4 clients.")

(defvar p4-labels-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a label and
cdr is the list of answers??")

(defvar p4-users-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a user and
cdr is the list of answers??")

(defvar p4-groups-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a group and
cdr is the list of answers??")

(defvar p4-arg-string-history nil
  "History for p4 command arguments")

(defun p4-depot-completion-search (filespec cmd)
  "Look into `p4-depot-completion-cache' for filespec.
Filespec is the candidate for completion, so the
exact file specification is \"filespec*\".

If found in cache, return a list whose car is FILESPEC and cdr is the list
of matches.
If not found in cache, return nil.
So the 'no match' answer is different from 'not in cache'."
  (let ((l (cond
	    ((equal cmd "branches") p4-branches-completion-cache)
	    ((equal cmd "clients") p4-clients-completion-cache)
	    ((equal cmd "dirs") p4-depot-completion-cache)
	    ((equal cmd "jobs") p4-jobs-completion-cache)
	    ((equal cmd "labels") p4-labels-completion-cache)
	    ((equal cmd "users") p4-users-completion-cache)
	    ((equal cmd "groups") p4-groups-completion-cache)))
	dir list)

    (if (and p4-cleanup-cache (not p4-timer))
	(setq p4-timer (cond (p4-running-emacs
			      (run-at-time p4-cleanup-time nil
					   'p4-cache-cleanup))
			     (p4-running-xemacs
			      (add-timeout p4-cleanup-time 'p4-cache-cleanup
					   nil nil)))))
    (while l
      (if (string-match (concat "^" (car (car l)) "[^/]*$") filespec)
	  (progn
	    ;; filespec is included in cache
	    (if (string= (car (car l)) filespec)
		(setq list (cdr (car l)))
	      (setq dir (cdr (car l)))
	      (while dir
		(if (string-match (concat "^" filespec) (car dir))
		    (setq list (cons (car dir) list)))
		(setq dir (cdr dir))))
	    (setq l nil
		  list (cons filespec list))))
      (setq l (cdr l)))
    list))

(defun p4-cache-cleanup (&optional arg)
  "Cleanup all the completion caches."
  (message "Cleaning up the p4 caches ...")
  (setq p4-branches-completion-cache nil)
  (setq p4-clients-completion-cache nil)
  (setq p4-depot-completion-cache nil)
  (setq p4-jobs-completion-cache nil)
  (setq p4-labels-completion-cache nil)
  (setq p4-users-completion-cache nil)
  (setq p4-groups-completion-cache nil)
  (if (and p4-running-emacs (timerp p4-timer)) (cancel-timer p4-timer))
  (if (and p4-running-xemacs p4-timer) (disable-timeout p4-timer))
  (setq p4-timer nil)
  (message "Cleaning up the p4 caches ... done."))

(defun p4-partial-cache-cleanup (type)
  "Cleanup a specific completion cache."
  (cond ((string= type "branch")
	 (setq p4-branches-completion-cache nil))
	((string= type "client")
	 (setq p4-clients-completion-cache nil))
	((or (string= type "submit") (string= type "change"))
	 (setq p4-depot-completion-cache nil))
	((string= type "job")
	 (setq p4-jobs-completion-cache nil))
	((string= type "label")
	 (setq p4-labels-completion-cache nil))
	((string= type "user")
	 (setq p4-users-completion-cache nil))
	((string= type "group")
	 (setq p4-groups-completion-cache nil))))

(defun p4-read-depot-output (buffer &optional regexp)
  "Reads first line of BUFFER and returns it.
Read lines are deleted from buffer.

If optional REGEXP is passed in, return the substring of the first line that
matched the REGEXP."

  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (forward-line)

    (let ((line (buffer-substring (point-min) (point))))
      (if (string= line "")
	  nil
	(delete-region (point-min) (point))
	(if (and regexp (string-match regexp line))
	    (setq line (substring line (match-beginning 1) (match-end 1))))

	;; remove trailing newline
	(if (equal (substring line (1- (length line)) (length line)) "\n")
	    (substring line 0 (1- (length line)))
	  line)))))

(defun p4-completion-helper (filespec cmd var regexp)
  (let (output-buffer line list)
    (message "Making %s completion list..." cmd)
    (setq output-buffer (p4-depot-output cmd))
    (while (setq line (p4-read-depot-output
		       output-buffer regexp))
      (if line
	  (setq list (cons line list))))
    (kill-buffer output-buffer)
    (set var
	 (cons (cons filespec list) (eval var)))
    list))

(defun p4-depot-completion-build (filespec cmd)
  "Ask Perforce for a list of files and directories beginning with FILESPEC."
  (let (output-buffer line list)
    (cond
     ((equal cmd "branches")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-branches-completion-cache
		  "^Branch \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))
     ((equal cmd "clients")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-clients-completion-cache
		  "^Client \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))

     ((equal cmd "dirs")
      (message "Making p4 completion list...")
      (setq output-buffer (p4-depot-output cmd
					   (list (concat filespec "*"))))
      (while (setq line (p4-read-depot-output output-buffer))
	(if (not (string-match "no such file" line))
	    (setq list (cons (concat line "/") list))))
      (kill-buffer output-buffer)
      (setq output-buffer (p4-depot-output "files"
					   (list (concat filespec "*"))))
      (while (setq line (p4-read-depot-output output-buffer))
	(if (string-match "^\\(.+\\)#[0-9]+ - " line)
	    (setq list (cons (match-string 1 line) list))))
      (kill-buffer output-buffer)
      (setq p4-depot-completion-cache
	    (cons (cons filespec list) p4-depot-completion-cache)))

     ((equal cmd "jobs")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-jobs-completion-cache
		  "\\([^ \n]*\\) on [0-9][0-9][0-9][0-9]/.*$")))
     ((equal cmd "labels")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-labels-completion-cache
		  "^Label \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))
     ((equal cmd "users")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-users-completion-cache
		  "^\\([^ ]+\\).*$")))
     ((equal cmd "groups")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-groups-completion-cache
		  "^\\(.*\\)$"))))
    (message nil)
    (cons filespec list)))

(defun p4-completion-builder (type)
  `(lambda (string predicate action)
     ,(concat "Completion function for Perforce " type ".

Using the mouse in completion buffer on a client will select it
and exit, unlike standard selection. This is because
`choose-completion-string' (in simple.el) has a special code for
file name selection.")
     (let (list)
       ,(if (string= type "dirs")
	    ;; when testing for an exact match, remove trailing /
	    `(if (and (eq action 'lambda)
		      (eq (aref string (1- (length string))) ?/))
		 (setq string (substring string 0 (1- (length string))))))

       ;; First, look in cache
       (setq list (p4-depot-completion-search string ,type))

       ;; If not found in cache, build list.
       (if (not list)
	   (setq list (p4-depot-completion-build string ,type)))

       (cond
	;; try completion
	((null action)
	 (try-completion string (mapcar 'list (cdr list)) predicate))
	;; all completions
	((eq action t)
	 (let ((lst
		(all-completions string (mapcar 'list (cdr list)) predicate)))
	   ,(if (string= type "dirs")
		`(setq lst (mapcar (lambda (s)
				     (if (string-match ".*/\\(.+\\)" s)
					 (match-string 1 s)
				       s))
				   lst)))
	   lst))
	;; Test for an exact match
	(t
	 (and (>= (length list) 2)
	      (member (car list) (cdr list))))))))

(defalias 'p4-branches-completion (p4-completion-builder "branches"))
(defalias 'p4-clients-completion (p4-completion-builder "clients"))
(defalias 'p4-depot-completion (p4-completion-builder "dirs"))
(defalias 'p4-jobs-completion (p4-completion-builder "jobs"))
(defalias 'p4-labels-completion (p4-completion-builder "labels"))
(defalias 'p4-users-completion (p4-completion-builder "users"))
(defalias 'p4-groups-completion (p4-completion-builder "groups"))


(defun p4-read-arg-string (prompt &optional initial type)
  (let ((minibuffer-local-completion-map
	 (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map " " 'self-insert-command)
    (completing-read prompt
		     (cond ((not type)
			    'p4-arg-string-completion)
			   ((string= type "branch")
			    'p4-branch-string-completion)
			   ((string= type "client")
			    'p4-client-string-completion)
			   ((string= type "label")
			    'p4-label-string-completion)
			   ((string= type "job")
			    'p4-job-string-completion)
			   ((string= type "user")
			    'p4-user-string-completion)
			   ((string= type "group")
			    'p4-group-string-completion))
		     nil nil
		     initial 'p4-arg-string-history)))

(defun p4-arg-string-completion (string predicate action)
  (let ((first-part "") completion)
    (if (string-match "^\\(.* +\\)\\(.*\\)" string)
	(progn
	  (setq first-part (match-string 1 string))
	  (setq string (match-string 2 string))))
    (cond ((string-match "-b +$" first-part)
	   (setq completion (p4-branches-completion string predicate action)))
	  ((string-match "-t +$" first-part)
	   (setq completion (p4-list-completion
			     string (list "text " "xtext " "binary "
					  "xbinary " "symlink ")
			     predicate action)))
	  ((string-match "-j +$" first-part)
	   (setq completion (p4-jobs-completion string predicate action)))
	  ((string-match "-l +$" first-part)
	   (setq completion (p4-labels-completion string predicate action)))
	  ((string-match "\\(.*status=\\)\\(.*\\)" string)
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-list-completion
			     string (list "open " "closed " "suspended ")
			     predicate action)))
	  ((or (string-match "\\(.*@.+,\\)\\(.*\\)" string)
	       (string-match "\\(.*@\\)\\(.*\\)" string))
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-labels-completion string predicate action)))
	  ((string-match "\\(.*#\\)\\(.*\\)" string)
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-list-completion
			     string (list "none" "head" "have")
			     predicate action)))
	  ((string-match "^//" string)
	   (setq completion (p4-depot-completion string predicate action)))
	  ((string-match "\\(^-\\)\\(.*\\)" string)
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-list-completion
			     string (list "a " "af " "am " "as " "at " "ay "
					  "b " "c " "d " "dc " "dn "
					  "ds " "du " "e " "f " "i " "j "
					  "l " "m " "n " "q " "r " "s " "sa "
					  "sd " "se " "sr " "t " "v ")
			     predicate action)))
	  (t
	   (setq completion (p4-file-name-completion string
						     predicate action))))
    (cond ((null action) ;; try-completion
	   (if (stringp completion)
	       (concat first-part completion)
	     completion))
	  ((eq action t) ;; all-completions
	   completion)
	  (t             ;; exact match
	   completion))))

(defun p4-list-completion (string lst predicate action)
  (let ((collection (mapcar 'list lst)))
    (cond ((not action)
	   (try-completion string collection predicate))
	  ((eq action t)
	   (all-completions string collection predicate))
	  (t
	   (eq (try-completion string collection predicate) t)))))

(defun p4-file-name-completion (string predicate action)
  (if (string-match "//\\(.*\\)" string)
      (setq string (concat "/" (match-string 1 string))))
  (setq string (substitute-in-file-name string))
  (setq string (p4-follow-link-name (expand-file-name string)))
  (let ((dir-path "") completion)
    (if (string-match "^\\(.*[/\\]\\)\\(.*\\)" string)
	(progn
	  (setq dir-path (match-string 1 string))
	  (setq string (match-string 2 string))))
    (cond ((not action)
	   (setq completion (file-name-completion string dir-path))
	   (if (stringp completion)
	       (concat dir-path completion)
	     completion))
	  ((eq action t)
	   (file-name-all-completions string dir-path))
	  (t
	   (eq (file-name-completion string dir-path) t)))))

(defun p4-string-completion-builder (completion-function)
  `(lambda (string predicate action)
     (let ((first-part "") completion)
       (if (string-match "^\\(.* +\\)\\(.*\\)" string)
	   (progn
	     (setq first-part (match-string 1 string))
	     (setq string (match-string 2 string))))
       (cond ((string-match "^-" string)
	      (setq completion nil))
	     (t
	      (setq completion
		    (,completion-function string predicate action))))
       (cond ((null action);; try-completion
	      (if (stringp completion)
		  (concat first-part completion)
		completion))
	     ((eq action t);; all-completions
	      completion)
	     (t;; exact match
	      completion)))))

(defalias 'p4-branch-string-completion (p4-string-completion-builder
					'p4-branches-completion))

(defalias 'p4-client-string-completion (p4-string-completion-builder
					'p4-clients-completion))

(defalias 'p4-job-string-completion (p4-string-completion-builder
				     'p4-jobs-completion))

(defalias 'p4-label-string-completion (p4-string-completion-builder
				       'p4-labels-completion))

(defalias 'p4-user-string-completion (p4-string-completion-builder
				      'p4-users-completion))

(defalias 'p4-group-string-completion (p4-string-completion-builder
				      'p4-groups-completion))

(defun p4-depot-find-file (file)
  (interactive (list (completing-read "Enter filespec: "
				      'p4-depot-completion
				      nil nil
				      p4-default-depot-completion-prefix
				      'p4-depot-filespec-history)))
  (let ((lfile (cdar (p4-map-depot-files (list file)))))
    (if lfile
	(find-file lfile)
      (if (get-file-buffer file)
	  (switch-to-buffer-other-window file)
	(get-buffer-create file)
	(set-buffer file)
	(p4-noinput-buffer-action "print" nil t (list file))
	(p4-activate-print-buffer file t)))))


;; A function to get the current P4 client name
(defun p4-get-client-name ()
  "To get the current value of the environment variable P4CLIENT,
type \\[p4-get-client-name].

This will be the current client that is in use for access through
Emacs P4."
  (interactive)
  (let ((client (p4-current-client)))
    (message "P4CLIENT [local: %s], [global: %s]" client (getenv "P4CLIENT"))
    client))

(defun p4-get-config-info (file-name token)
  (let ((output-buffer (generate-new-buffer p4-output-buffer-name))
	(data (getenv token)))
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (insert-file-contents file-name)
      (goto-char (point-min))
      (if (re-search-forward (concat "^" (regexp-quote token) "=\\(.*\\)")
			     nil t)
	  (setq data (match-string 1))))
    (kill-buffer output-buffer)
    data))

(defun p4-current-client ()
  "Get the current local client, or the global client, if that."
  (let ((p4-config-file (p4-find-p4-config-file))
	cur-client pmin)
    (if (not p4-config-file)
	(setq cur-client (getenv "P4CLIENT"))
      (setq cur-client (p4-get-config-info p4-config-file "P4CLIENT")))
    (if (not cur-client)
	(save-excursion
	  (get-buffer-create p4-output-buffer-name)
	  (set-buffer p4-output-buffer-name)
	  (goto-char (point-max))
	  (setq pmin (point))
	  (if (zerop (p4-call-p4-here "info"))
	      (progn
		(goto-char pmin)
		(if (re-search-forward "^Client name:[ \t]+\\(.*\\)$" nil t)
		    (setq cur-client (match-string 1)))
		(delete-region pmin (point-max))))))
    cur-client))

(defun p4-current-server-port ()
  "Get the current local server:port address, or the global server:port, if
that."
  (let ((p4-config-file (p4-find-p4-config-file)))
    (if (not p4-config-file)
	(getenv "P4PORT")
      (p4-get-config-info p4-config-file "P4PORT"))))

(defun p4-save-opened-files ()
  (get-buffer-create p4-output-buffer-name);; We do these two lines
  (kill-buffer p4-output-buffer-name)      ;; to ensure no duplicates
  (let ((output-buffer (p4-depot-output "opened"))
	opened)
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\)#[0-9]+ - " nil t)
	(setq opened (cons (match-string 1) opened))))
    (kill-buffer output-buffer)
    (setq opened (mapcar 'cdr (p4-map-depot-files opened)))
    (save-window-excursion
      (map-y-or-n-p
       (function
	(lambda (buffer)
	  (and (buffer-modified-p buffer)
	       (not (buffer-base-buffer buffer))
	       (buffer-file-name buffer)
	       (member (buffer-file-name buffer) opened)
	       (format "Save file %s? "
		       (buffer-file-name buffer)))))
       (function
	(lambda (buffer)
	  (set-buffer buffer)
	  (save-buffer)))
       (buffer-list)
       '("buffer" "buffers" "save")))))

(defun p4-empty-diff-p ()
  "Return t if there exists a file opened for edit with an empty diff"
  (let ((buffer (get-buffer-create "p4-edp-buf"))
	opened empty-diff)
    (p4-exec-p4 buffer (list "opened") t)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\)#[0-9]* - edit.*" nil t)
	(setq opened (cons (match-string 1) opened))))
    (if opened
	(progn
	  (p4-exec-p4 buffer (list "diff") t)
	  (save-excursion
	    (set-buffer buffer)
	    (goto-char (point-max))
	    (insert "====\n")
	    (goto-char (point-min))
	    (while (re-search-forward "^==== \\([^#\n]+\\)#.*\n====" nil t)
	      (if (member (match-string 1) opened)
		  (progn
		    (setq empty-diff t)
		    (goto-char (point-max))))))))
    (kill-buffer buffer)
    empty-diff))

;; this next chunk is not currently used, but my plan is to
;; reintroduce it as configurable bury-or-kill-on-q behaviour:

;; (defcustom p4-blame-2ary-disp-method 'default
;;   "Method to use when displaying p4-blame secondary buffers
;;    (currently change and rev buffers)

;;    new-frame   --  pop a new frame for the buffer
;;    new-window  --  create a new window for the buffer
;;    default     --  just do what `display-buffer' would do

;;    Any other value is equivalent to default."
;;   :type '(radio (const default) (const  new-frame) (const new-window))
;;   :group 'p4)

(defun p4-blame-kill-blame ()
  "Don\'t ask any questions, just kill the current buffer"
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))

(defun p4-blame-secondary-buffer-cleanup ()
  "Attempt to clean up a` p4-blame' secondary buffer neatly, deleting
windows or frames when we think that\'s necessary"
  (let* ((this-buffer (current-buffer))
	 (this-window (get-buffer-window this-buffer t)))
    (cond
     ;; in new-frame mode, delete the frame
     ((eq p4-blame-2ary-disp-method 'new-frame)
      (if (one-window-p 'ignore-minibuffer 'just-this-frame)
	  (delete-frame (window-frame this-window))
	(delete-window this-window)) t)
     ;; in new-window mode, just zap the window,
     ;; provided it is not the only one:
     ((eq p4-blame-2ary-disp-method 'new-window)
      (if (not (one-window-p 'ignore-minibuffer 'just-this-frame))
	  (delete-window this-window)) t)
     ;; any other mode, nothing special need be done
     (t
      t))))

(provide 'p4)

;;; p4.el ends here
