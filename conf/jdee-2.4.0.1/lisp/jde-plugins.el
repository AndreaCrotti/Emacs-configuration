;;; jde-plugins.el -- Support for JDEE plugins
;; $Id: jde-plugins.el 127 2009-08-12 08:22:57Z paullandes $

;; Author: Paul Kinnucan <pkinnucan@attbi.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 2003, 2004 Paul Kinnucan.
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

;;; Commentary:

(require 'eieio)
(require 'executable)

(defcustom jde-plugins-directory (expand-file-name "plugins" (jde-root))
  "Location of the JDEE's plugins directory."
  :group 'jde
  :type 'file)

(defclass jde-plugin ()
  ((bsh-cp    :initarg :bsh-cp
	      :type list
	      :documentation "Beanshell classpath for this plugin.")
   (menu-spec :initarg :menu-spec
	      :type list
	      :documentation "Specifies menu for this plugin.")
   (plugins   :type list
	      :allocation :class
	      :initform nil
	      :documentation
	     "Installed plugins."))
"Class of plugins.")


(defun jde-pi-register (plugin)
  "Register PLUGIN, which must be an object of
type `jde-plugin'."
  (oset-default
   'jde-plugin
   plugins
   (cons plugin (oref 'jde-plugin plugins))))


(defun jde-pi-get-plugin-dir (plugin)
  "Returns the path of the directory containing PLUGIN."
  (expand-file-name plugin jde-plugins-directory))


(defun jde-pi-load-plugin (plugin)
  "Loads the plugin named PLUGIN. This function assumes that the plugin resides
in a subdirectory of the JDEE's plugins directory named PLUGIN and that this
subdirectory contains a subdirectory name lisp that contains
a file named jde-PLUGIN.el. This function loads jde-PLUGIN.el."
  (let* ((plugin-dir (expand-file-name plugin jde-plugins-directory))
	 (plugin-lisp-dir (expand-file-name "lisp" plugin-dir))
	 (plugin-lisp-package-name (concat "jde-" plugin))
	 (plugin-lisp-file-name (concat plugin-lisp-package-name ".el"))
	 (plugin-lisp-file
	  (expand-file-name
	   plugin-lisp-file-name
	   plugin-lisp-dir)))
    (if (file-exists-p plugin-lisp-file)
	(progn
	  (add-to-list 'load-path plugin-lisp-dir)
	  (require (intern plugin-lisp-package-name)))
      (error "JDEE plugin Lisp file %s missing" plugin-lisp-file-name))))


(defun jde-pi-load-plugins ()
  "Loads the plugins in the JDEE's plugins directory."
  (interactive)
  (if (file-exists-p jde-plugins-directory)
      (let ((plugins
	     (delq
	      nil
	      (mapcar
	       (lambda (file)
		 (let ((file-name (file-name-nondirectory file)))
		   (if (and
			(file-directory-p file)
			(not (string= file-name "."))
			(not (string= file-name ".."))
			(not (string= file-name "CVS"))
			(not (string= file-name "RCS")))
		       file-name)))
	       (directory-files jde-plugins-directory t)))))
	(loop for plugin in plugins do
	  (jde-pi-load-plugin plugin)))))

(jde-pi-load-plugins)

(defun jde-pi-get-bsh-classpath ()
  "Get the plugin directories and jar files to include in the Beanshell classpath."
  (let ((plugins (oref 'jde-plugin plugins))
	classpath)
    (loop for plugin in plugins do
	  (setq classpath (append classpath (oref plugin bsh-cp))))
    classpath))


(defun jde-pi-install-plugins ()
  "This command installs any plugin distributables that it
finds in the JDEE's plugins directory. It assumes that
the distributables are in jar or zip format and that the
jar program is on the system path."
  (interactive)

  (assert (executable-find "jar") nil
    "Cannot find the jar program on the system path.")

  (let ((zip-files
	 (directory-files jde-plugins-directory nil ".*[.]\\(zip\\|jar\\)")))

    (when zip-files
      (let ((buf (get-buffer-create "*plugins*")))
	  (with-current-buffer buf
	    (erase-buffer)
	    (insert "JDEE Plugin Installation Log")
	    (pop-to-buffer buf)
	    (cd jde-plugins-directory)
	    (loop for zip-file in zip-files do
		  (let ((result
			 (shell-command-to-string
			(concat "jar xvf " zip-file))))
		    (insert "\n\n")
		    (insert (format "Installing %s ..."
				    (file-name-sans-extension zip-file)))
		    (insert "\n\n")
		    (insert result)))
	    (insert "\n\nInstallation complete"))))))


(defun jde-plugin-make-menu-spec ()
  (if (oref 'jde-plugin plugins)
      (append
       (list "JDEpi")
       (delq
	nil
	(mapcan
	 (lambda (plugin)
	   (oref plugin menu-spec))
	 (oref 'jde-plugin plugins))))))

(defvar jde-plugin-mode-map
  (let ((km (make-sparse-keymap))
	(menu-spec (jde-plugin-make-menu-spec)))
    (if menu-spec
	(easy-menu-define jde-plugin-menu km "JDEE Plugin Minor Mode Menu"
	  menu-spec))
    km)
  "Keymap for JDEE plugin minor mode.")

(defvar jde-plugin-minor-mode nil
  "Non-nil if JDEE plugin minor mode is enabled.")

(make-variable-buffer-local 'jde-plugin-minor-mode)

(defun jde-plugin-minor-mode (&optional arg)
  "Toggle JDEE plugin minor mode.
With prefix argument ARG, turn on if positive, otherwise off..

\\{jde-plugin-mode-map}"
  (interactive
   (list (or current-prefix-arg
	     (if jde-plugin-minor-mode 0 1))))

  (setq jde-plugin-minor-mode
	(if arg
	    (>
	     (prefix-numeric-value arg)
	     0)
	  (not jde-plugin-minor-mode)))

  (if (featurep 'xemacs)
      (let ((menu-spec (jde-plugin-make-menu-spec)))
	(if menu-spec
	    (if jde-plugin-minor-mode
		(easy-menu-add menu-spec jde-plugin-mode-map)
	      (easy-menu-remove menu-spec))))))

(semantic-add-minor-mode 'jde-plugin-minor-mode " plugin" jde-plugin-mode-map)


(provide 'jde-plugins)

;; Change History

;; End of jde-plugins.el
