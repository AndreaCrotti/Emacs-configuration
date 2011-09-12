;;; cedet-java.el --- Support functions on top of Java's JDK.
;;
;; Copyright (C) 2011 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Java support utilities for CEDET.

;;; Code:
(require 'inversion)

(defvar cedet-java-min-version "1.4"
  "Minimum version of the java JDR.")

(defcustom cedet-java-command "java"
  "The command used for running Java."
  :group 'java
  :type 'string)

(defcustom cedet-jar-command "jar"
  "The command used for running Java jar command."
  :group 'java
  :type 'string)

(defcustom cedet-javap-command "javap"
  "The command used for running Java's javap command."
  :group 'java
  :type 'string)

(defcustom cedet-java-classpath-extension nil
  "List of extended classpath directories and Jar files to pass to java commands."
  :group 'java
  :type '(repeat string))

;;; Java command Support
;;
(defun cedet-java-call (flags)
  "Call java with the list of FLAGS."
  (let ((b (get-buffer-create "*Java Output*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (when cedet-java-classpath-extension
      (setq flags (cons "-classpath"
			(cons (mapconcat 'identity cedet-java-classpath-extension ":")
			      flags))))
    (apply 'call-process cedet-java-command
	   nil b nil flags)
    b))

;;;###autoload
(defun cedet-java-version-check (&optional noerror)
  "Check the version of the installed java command.
If optional programatic argument NOERROR is non-nil, then
instead of throwing an error if Global isn't available, then
return nil."
  (interactive)
  (let ((b (condition-case nil
	       (cedet-java-call (list "-version"))
	     (error nil)))
	(rev nil))
    (if (not b)
	(progn
	  (when (cedet-called-interactively-p 'interactive)
	    (message "java not found."))
	  nil)
      (with-current-buffer b
	(goto-char (point-min))
	(re-search-forward "java version \"\\([0-9._]+\\)\"" nil t)
	(setq rev (match-string 1))
	(if (inversion-check-version rev nil cedet-java-min-version)
	    (if noerror
		nil
	      (error "Version of Java is %s.  Need at least %s"
		     rev cedet-java-min-version))
	  ;; Else, return TRUE, as in good enough.
	  (when (cedet-called-interactively-p 'interactive)
	    (message "Java %s  - Good enough." rev))
	  t)))))

;;; Java "jar" command Support
;;
(defun cedet-jar-table-of-contents (jarfile)
  "Extract the table of contents from JARFILE.
Return the contents as a list of paths to files.
Exclude empty directories."
  (let* ((b (cedet-jar-call (list "-tf" jarfile)))
	 (strs (split-string
		(save-excursion (set-buffer b) (buffer-string))
		"\n" t))
	 (ans nil))
    (dolist (C strs)
      (when (string-match "\\.class$" C)
	(push C ans)))
    (nreverse ans)))

(defun cedet-jar-call (flags)
  "Call java's jar command with the list of FLAGS."
  (let ((b (get-buffer-create "*Jar Output*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'call-process cedet-jar-command
	   nil b nil flags)
    b))

;;; Javap command Support
;;
(defun cedet-javap-call (flags)
  "Call javap with the list of FLAGS."
  (let ((b (get-buffer-create "*javap output*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (when cedet-java-classpath-extension
      (setq flags (cons "-classpath"
			(cons (mapconcat 'identity cedet-java-classpath-extension ":")
			      flags))))
    (apply 'call-process cedet-javap-command
	   nil b nil
	   flags)
    b))

(defun cedet-javap-get-class (jar class)
  "In JAR, get a javap dump of CLASS, return the buffer."
  (let ((cedet-java-classpath-extension
	 (if jar (list jar) cedet-java-classpath-extension)))
    (cedet-javap-call
     (if cedet-java-classpath-extension
	 (list "-protected" "-bootclasspath" "" class)
       (list "-protected" class)
       ))))

;;;###autoload
(defun cedet-javap-dump-class (class)
  "Dump out a Java signatures for CLASS.
Display in a javap output buffer."
  (interactive "sClass: ")
  (switch-to-buffer (cedet-javap-get-class class)))



(provide 'cedet-java)

;;; cedet-java.el ends here
