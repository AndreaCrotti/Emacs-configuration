;;; jde-checkstyle.el --- Checkstyle interface for JDE
;; $Id: jde-checkstyle.el 179 2009-12-27 01:58:29Z lenbok $

;; Copyright (C) 2001, 2002, 2003, 2004 Markus Mohnen and Paul Kinnucan
;; Copyright (C) 2009 by Paul Landes

;; Authors: Markus Mohnen and Paul Kinnucan
;; Maintainers: Markus Mohnen and Paul Landes
;; Created: 06 Jun 2001
;;
;;
;; Keywords: Java coding standard checker Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; ) or from the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; jde-checkstyle|Markus Mohnen|
;; |Checkstyle interface for JDE
;; |$Date: 2009-12-26 19:58:29 -0600 (Sat, 26 Dec 2009) $|$Revision: 179 $|~/packages/jde-checkstyle.el

;;; Commentary:

;;; This package provides an interface from JDE (see
;;; http://jdee.sourceforge.net/) to Oliver Burn's CheckStyle (see
;;; http://checkstyle.sourceforge.net/) a development tool
;;; to help programmers write Java code that adheres to a coding
;;; standard.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;      (require 'jde-checkstyle)

;;; Usage:
;;
;;  M-x `jde-checkstyle' to check the java file in the current buffer.
;;

;;; Customization:
;;
;;  M-x `jde-checkstyle-customize' to customize all the jde-checkstyle options.

;;; Code:

(require 'jde-compile)

(defconst jde-checkstyle-version "3.1")

(defgroup jde-checkstyle nil
  "This group specifies options for the JDEE's interface to the CheckStyle
package (http://checkstyle.sourceforge.net). The CheckStyle package
checks Java source files for conformity to a specified coding
style."
  :group 'jde)

(defcustom jde-checkstyle-class "com.puppycrawl.tools.checkstyle.Main"
  "*Java checker class.
Specifies the class of the the program to be used to check the source
in the current buffer. The default is the checkstyle program."
  :group 'jde-checkstyle
  :type 'string)

(defcustom jde-checkstyle-classpath nil
  "*Specify paths of classes required to run the jde-checkstyle application.
The JDE uses the specified paths to construct a -classpath
argument to pass to the Java interpreter. This option overrides the
`jde-global-classpath' option."
  :group 'jde-checkstyle
  :type '(repeat (file :tag "Path")))

(defcustom jde-checkstyle-read-args nil
  "*Specify whether to prompt for additional checker arguments.
If this variable is non-nil, the jde-checkstyle command prompts
you to enter additional checker arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The JDE maintains a history list of arguments
entered in the minibuffer."
  :group 'jde-checkstyle
  :type 'boolean)

(defvar jde-checkstyle-interactive-args ""
  "String of checker arguments entered in the minibuffer.")

(defvar jde-checkstyle-interactive-arg-history nil
  "History of checker arguments entered in the minibuffer.")

;; (makunbound 'jde-checkstyle-style)
(defcustom jde-checkstyle-style nil
  "*Style used to check this project's Java code. \"Sun\"
checks for conformity to the Java code style standard established by
Sun Microsystems. \"Custom\" specifies a a user-defined
style. Selecting this option causes Emacs to display an edit
field. Enter the path of a CheckStyle configuration file that defines
the custom coding style in this field (see the CheckStyle
documentation for information on configuration files). Use
`jde-checkstyle-properties' to specify the values of properties that
the configuration file reads from the CheckStyle command line."
   :group 'jde-checkstyle
   :type '(choice (const :tag "Sun" :value nil)
		 (file :menu-tag "Custom" :tag "Config. File")))


(defcustom jde-checkstyle-expanded-properties nil
  "*Specify the values of the expanded properties specified by the
`jde-checkstyle-style' configuration file. (See the CheckStyle
documentation for information about expanded properties.) To enter a
property, select the INS button. Emacs displays a Property Name field
and a Property Value field for the property. Enter the name of the
property, for example, checkstyle.header.file, in the Property Name
field; enter its value, for example, docs/java.header, in the Property
Value field.  Repeat this process to display additional
properties. You can specify as many properties as you like in
this way. To delete a property, select the DEL button next
to the property."
  :group 'jde-checkstyle
  :type '(repeat (cons
		  (string :tag "Property Name")
		  (string :tag "Property Value"))))

;; (makunbound 'jde-checkstyle-expanded-properties-file)
(defcustom jde-checkstyle-expanded-properties-file nil
  "*Path of a file that specifies the values of a configuration
file's expanded properties. If this option is set, the JDEE ignores
the settings of the `jde-checkstyle-expanded-properties' variable."
   :group 'jde-checkstyle
   :type '(choice (const :tag "None" :value nil)
		  (file :menu-tag "Properties File" :tag "Path")))

;; (makunbound 'jde-checkstyle-module-package-names-file)
(defcustom jde-checkstyle-module-package-names-file nil
  "*Path of a file that specifies the package names of
custom style checking modules used by this project."
   :group 'jde-checkstyle
   :type '(choice (const :tag "None" :value nil)
		  (file :menu-tag "Package Names File" :tag "Path")))

;; (makunbound 'jde-checkstyle-output-file)
(defcustom jde-checkstyle-output-file nil
  "*Path of a file to store CheckStyle's output."
   :group 'jde-checkstyle
   :type '(choice (const :tag "None" :value nil)
		  (file :menu-tag "Output File" :tag "Path")))


;; (makunbound 'jde-checkstyle-output-format)
(defcustom jde-checkstyle-output-format nil
  "*Format of CheckStyle's output. Options are plain or XML."
  :group 'jde-checkstyle
  :type '(choice (const :tag "Plain" :value nil)
		 (const :tag "XML" :value "xml")))


;; (makunbound 'jde-checkstyle-source-dir)
(defcustom jde-checkstyle-source-dir nil
  "*Path of a directory to check. If you specify a
path, CheckStyle checks all the files in the specified
directory. Otherwise, it checks the file in the current
buffer."
   :group 'jde-checkstyle
   :type '(choice (const :tag "None" :value nil)
		  (file :menu-tag "Source Directory" :tag "Path")))


;; (makunbound 'jde-checkstyle-finish-hook)
(defcustom jde-checkstyle-finish-hook
  '(jde-compile-finish-kill-buffer)
  "List of functions to be invoked when CheckStyle terminates.  Each
function should accept two arguments: the compilation buffer and a
string describing how the compilation finished."
  :group 'jde-checkstyle
  :type 'hook)


;; (makunbound 'jde-checkstyle-source-file-extension)
(defcustom jde-checkstyle-source-file-extension nil
  "*Extension of Java source files (if not java)."
  :group 'jde-checkstyle
  :type '(choice (const :tag "java" :value nil)
		 (string :menu-tag "other" :tag "Extension")))


(defmethod jde-checkstyle-get-property-args ((this jde-run-vm))
    "Get property arguments."
    (mapcar
     (lambda (prop)
       (format "-D%s=%s" (car prop) (cdr prop)))
     jde-run-option-properties))


;;;###autoload
(defun jde-checkstyle-customize ()
  "Set Java style checking options."
  (interactive)
  (customize-group "jde-checkstyle"))


(defclass jde-checkstyle-checker ()
  ((buffer           :initarg :buffer
		     :type buffer
		     :documentation
		     "Compilation buffer")
   (window           :initarg :window
		     :type window
		     :documentation
		     "Window that displays the compilation buffer.")
   (interactive-args :initarg :interactive-args
		     :type list
		     :documentation
		     "Arguments entered in the minibuffer."))
  "Class of Java style checkers.")

(defmethod jde-checkstyle-create-checker-buffer ((this jde-checkstyle-checker))
  (save-excursion
    (let ((buf (get-buffer-create "*check style*"))
	  (error-regexp-alist compilation-error-regexp-alist)
	  (enter-regexp-alist (if (boundp 'compilation-enter-directory-regexp-alist)
				  compilation-enter-directory-regexp-alist))
	  (leave-regexp-alist (if (boundp 'compilation-leave-directory-regexp-alist)
				  compilation-leave-directory-regexp-alist))
	  (file-regexp-alist (if (boundp 'compilation-file-regexp-alist)
				 compilation-file-regexp-alist))
	  (nomessage-regexp-alist (if (boundp 'compilation-nomessage-regexp-alist)
				      compilation-nomessage-regexp-alist))
	  (parser compilation-parse-errors-function)
	  (error-message "No further errors")
	  (thisdir default-directory))

      (oset this :buffer buf)

      (set-buffer buf)

      ;; Make sure a style checker process is not
      ;; already running.
      (let ((check-proc (get-buffer-process (current-buffer))))
	(if check-proc
	    (if (or (not (eq (process-status check-proc) 'run))
		    (yes-or-no-p
			 "A check style process is running; kill it?"))
		(condition-case ()
		    (progn
		      (interrupt-process check-proc)
		      (sit-for 1)
		      (delete-process check-proc))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once"
			 (buffer-name)))))

      ;; In case the checker buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables)

      ;; Clear out the compilation buffer and make it writable.
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))

      (compilation-mode)
      (setq buffer-read-only nil)

      (set (make-local-variable 'compilation-finish-function)
	   (lambda (buf msg)
	     (run-hook-with-args 'jde-checkstyle-finish-hook buf msg)
	     (setq compilation-finish-function nil)))
      (if (boundp 'compilation-parse-errors-function)
	  (set (make-local-variable 'compilation-parse-errors-function) parser))
      (if (boundp 'compilation-error-message)
	  (set (make-local-variable 'compilation-error-message) error-message))
      (set (make-local-variable 'compilation-error-regexp-alist)
	     error-regexp-alist)
      (when (not (featurep 'xemacs))
	(dolist (elt `((compilation-enter-directory-regexp-alist
			,enter-regexp-alist)
		       (compilation-leave-directory-regexp-alist
			,leave-regexp-alist)
		       (compilation-file-regexp-alist
			,file-regexp-alist)
		       (compilation-nomessage-regexp-alist
			,nomessage-regexp-alist)))
	  (if (boundp (car elt))
	      (set (make-local-variable (car elt)) (second elt)))))

      (if (boundp 'compilation-directory-stack)
	  (setq default-directory thisdir
		compilation-directory-stack (list default-directory))))))

(defmethod jde-checkstyle-get-property-args ((this jde-checkstyle-checker))
    "Get property arguments."
    (mapcar
     (lambda (prop)
       (format "-D%s=%s" (car prop) (cdr prop)))
     jde-checkstyle-expanded-properties))

(defmethod jde-checkstyle-exec ((this jde-checkstyle-checker))

  (jde-checkstyle-create-checker-buffer this)

  ;; Pop to checker buffer.
  (let ((outwin (display-buffer (oref this :buffer))))
    (compilation-set-window-height outwin)
    (oset this :window outwin))

  (if (not jde-xemacsp)
      (if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))

  (let* ((outbuf (oref this :buffer))
	 (vm-path (oref (jde-run-get-vm) :path))
	 (source-file
	  (concat (file-name-nondirectory buffer-file-name)))
	 (jde-java-directory
	  (concat
	   (jde-find-jde-data-directory)
	   "java/"))
	 (args (append
		(unless jde-checkstyle-expanded-properties-file
		  (jde-checkstyle-get-property-args this))
		(oref this :interactive-args)
		(list "-classpath"
		      (if jde-checkstyle-classpath
			  (jde-build-classpath jde-checkstyle-classpath)
			(jde-normalize-path
			 (expand-file-name "lib/checkstyle-all.jar" jde-java-directory))))
		(list jde-checkstyle-class)
		(list "-c"
		      (if jde-checkstyle-style
			  (jde-normalize-path jde-checkstyle-style)
			(concat (jde-find-jde-data-directory) "java/lib/sun_checks.xml")))
		(if jde-checkstyle-expanded-properties-file
		    (list "-p" (jde-normalize-path jde-checkstyle-expanded-properties-file)))
		(if jde-checkstyle-module-package-names-file
		    (list "-n" (jde-normalize-path jde-checkstyle-module-package-names-file)))
		(if jde-checkstyle-output-format
		    (list "-f" jde-checkstyle-output-format))
		(if jde-checkstyle-output-file
		    (list "-o" (jde-normalize-path jde-checkstyle-output-file)))
		(if jde-checkstyle-source-file-extension
		    (list "-e" jde-checkstyle-source-file-extension))
		(if jde-checkstyle-source-dir
		    (list "-r" (jde-normalize-path jde-checkstyle-source-dir))
		  (list source-file)))))

    (save-excursion
      (set-buffer outbuf)

      (insert (format "cd %s\n" default-directory))

      (insert (concat
	       vm-path
	       " "
	       (mapconcat 'identity args " ")
	       "\n\n"))

      (let* ((process-environment (cons "EMACS=t" process-environment))
	     (w32-quote-process-args ?\")
	     (win32-quote-process-args ?\") ;; XEmacs
	     (proc (apply 'start-process
			  (downcase mode-name)
			  outbuf
			  vm-path
			  args)))
	(set-process-sentinel proc 'compilation-sentinel)
	(set-process-filter proc 'compilation-filter)
	(set-marker (process-mark proc) (point) outbuf)
	(setq compilation-in-progress
	      (cons proc compilation-in-progress)))

      (set-buffer-modified-p nil)
      (setq compilation-last-buffer (oref this :buffer)))))



;;;###autoload
(defun jde-checkstyle ()
  "Checks the Java program in the current buffer.
This command invokes the style checker specified by `jde-checkstyle-class'
with the options specif2ied by the JDEE customization variables
that begin with `jde-checkstyle'. If the variable
`jde-checkstyle-read-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled."
  (interactive)

  (if jde-checkstyle-read-args
      (setq jde-checkstyle-interactive-args
	    (read-from-minibuffer
	     "Check args: "
	     jde-checkstyle-interactive-args
	     nil nil
	     '(jde-checkstyle-interactive-arg-history . 1))))

  (let ((checker (jde-checkstyle-checker
		  "checker"
		  :interactive-args (if jde-checkstyle-read-args
					jde-checkstyle-interactive-args))))

    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-checkstyle from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
	     (not jde-xemacsp))
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that jde-checkstyle
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))

    (jde-checkstyle-exec checker)))

;; Register and initialize the customization variables defined
;; by this package.
(jde-update-autoloaded-symbols)

(provide 'jde-checkstyle)

;; End of jde-checkstyle.el
