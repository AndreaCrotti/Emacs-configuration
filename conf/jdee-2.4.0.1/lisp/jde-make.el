;;; jde-make.el -- Integrated Development Environment for Java.
;; $Id: jde-make.el 127 2009-08-12 08:22:57Z paullandes $

;; Author: Paul Kinnucan <pkinnucan@attbi.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001, 2002, 2003, 2004 Paul Kinnucan.
;; Copyright (C) 2009 by Paul Landes

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'compile)

(defgroup jde-make nil
  "JDE Make Interface"
  :group 'jde
  :prefix "jde-make-")

(defcustom jde-make-program "make"
  "*Specifies name of make program."
 :group 'jde-make
 :type 'string)

(defcustom jde-make-working-directory ""
  "*Path of the working directory to use in 'make' build mode. This
string must end in a slash, for example, c:/foo/bar/ or ./  .
If this string is empty, the 'make' build mode uses the current file
location as its working directory."
  :group 'jde-make
  :type 'string)

(defcustom jde-make-enable-find nil
"*Specify whether jde-make find the Makefile based on your current
directory. If non-nil, we will search up the directory hierarchy from the
current directory for the build definition file. Also note that, if non-nil,
this will relax the requirement for an explicit jde project file."
   :group 'jde-make
   :type 'boolean)

(defcustom jde-make-args ""
  "*Specifies arguments to be passed to make program."
  :group 'jde-make
  :type 'string)

(defcustom jde-make-finish-hook
  '(jde-compile-finish-refresh-speedbar jde-compile-finish-update-class-info)
  "List of functions to be invoked when compilation of a
Java source file terminates. Each function should accept
two arguments: the compilation buffer and a string
describing how the compilation finished."
  :group 'jde-make
  :type 'hook)

(defvar jde-interactive-make-args ""
"String of compiler arguments entered in the minibuffer.")

(defcustom jde-read-make-args nil
"*Specify whether to prompt for additional make arguments.
If this variable is non-nil, and if `jde-build-use-make' is non nil
the jde-build command prompts you to enter additional make
arguments in the minibuffer. These arguments are appended to those
specified by customization variables. The JDE maintains a history
list of arguments entered in the minibuffer."
  :group 'jde-make
  :type 'boolean
)


(defun jde-make-make-command (more-args)
  "Constructs the java compile command as: jde-compiler + options + buffer file name."
  (concat jde-make-program " " jde-make-args
	  (if (not (string= more-args ""))
	      (concat " " more-args))
	  " "))

(defun jde-make-find-build-file (dir)
  "Find the next Makefile upwards in the directory tree from DIR.
Returns nil if it cannot find a project file in DIR or an ascendmake directory."
  (let ((file (find "Makefile"
		    (directory-files dir) :test 'string=)))

    (if file
	(setq file (expand-file-name file dir))
      (if (not (jde-root-dir-p dir))
	  (setq file (jde-make-find-build-file (concat dir "../")))))

    file))

;;;###autoload
(defun jde-make ()
  "Run the make program specified by `jde-make-program' with the
command-line arguments specified by `jde-make-args'. If
`jde-read-make-args' is nonnil, this command also prompts you to enter
make arguments in the minibuffer and passes any arguments that you
enter to the make program along with the arguments specified by
`jde-make-args'."
  (interactive)
  (if jde-read-make-args
      (setq jde-interactive-make-args
	      (read-from-minibuffer
	       "Make args: "
	       jde-interactive-make-args
	       nil nil
	       '(jde-interactive-make-arg-history . 1)))
    (setq jde-interactive-make-args ""))

  (let ((make-command
	 (jde-make-make-command
	  jde-interactive-make-args))
	(save-default-directory default-directory)
	(default-directory
	  (if (string= jde-make-working-directory "")
	      (if jde-make-enable-find
		  (let ((jde-make-buildfile
			 (jde-make-find-build-file default-directory)))
		    (if jde-make-buildfile
			(file-name-directory jde-make-buildfile)
		      default-directory))
		default-directory)
	    (jde-normalize-path 'jde-make-working-directory))))


    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-make from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
	     (not jde-xemacsp))
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that jde-make
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))

    (setq compilation-finish-function
      (lambda (buf msg)
	(run-hook-with-args 'jde-make-finish-hook buf msg)
	(setq compilation-finish-function nil)))

    (cd default-directory)
    (compile-internal make-command "No more errors")
    (cd save-default-directory)))

;;;###autoload
(defun jde-make-show-options ()
  "Show the JDE Make Options panel."
  (interactive)
  (customize-apropos "jde-make" 'groups))

;; Register and initialize the customization variables defined
;; by this package.
(jde-update-autoloaded-symbols)

(provide 'jde-make)

;; End of jde-make.el
