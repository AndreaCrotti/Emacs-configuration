;;; ob-octave.el --- org-babel functions for octave and matlab evaluation

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.01trans

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Requirements:

;; octave
;; octave-mode.el and octave-inf.el come with GNU emacs

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(declare-function matlab-shell "ext:matlab-mode")
(declare-function matlab-shell-run-region "ext:matlab-mode")

(defvar org-babel-default-header-args:matlab '())
(defvar org-babel-default-header-args:octave '())

(defvar org-babel-matlab-shell-command "matlab -nosplash"
  "Shell command to run matlab as an external process.")
(defvar org-babel-octave-shell-command "octave -q"
  "Shell command to run octave as an external process.")

(defun org-babel-expand-body:matlab (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (org-babel-expand-body:octave body params processed-params))
(defun org-babel-expand-body:octave (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat
      (lambda (pair)
        (format "%s=%s"
                (car pair)
                (org-babel-octave-var-to-octave (cdr pair))))
      vars "\n") "\n" body "\n")))

(defvar org-babel-matlab-with-emacs-link nil
  "If non-nil use matlab-shell-run-region for session evaluation.
  This will use EmacsLink if (matlab-with-emacs-link) evaluates
  to a non-nil value.")

(defvar org-babel-matlab-emacs-link-wrapper-method
   "%s
if ischar(ans), fid = fopen('%s', 'w'); fprintf(fid, '%%s\\n', ans); fclose(fid);
else, save -ascii %s ans
end
delete('%s')
")
(defvar org-babel-octave-wrapper-method
  "%s
if ischar(ans), fid = fopen('%s', 'w'); fprintf(fid, '%%s\\n', ans); fclose(fid);
else, save -ascii %s ans
end")

(defvar org-babel-octave-eoe-indicator "\'org_babel_eoe\'")

(defvar org-babel-octave-eoe-output "ans = org_babel_eoe")

(defun org-babel-execute:matlab (body params)
  "Execute a block of matlab code with Babel."
  (require 'matlab)
  (org-babel-execute:octave body params 'matlab))
(defun org-babel-execute:octave (body params &optional matlabp)
  "Execute a block of octave code with Babel."
  (let* ((processed-params (org-babel-process-params params))
         (session
	  (funcall (intern (format "org-babel-%s-initiate-session"
				   (if matlabp "matlab" "octave")))
		   (nth 0 processed-params) params))
         (vars (nth 1 processed-params))
         (result-params (nth 2 processed-params))
         (result-type (nth 3 processed-params))
	 (out-file (cdr (assoc :file params)))
	 (augmented-body
	  (org-babel-expand-body:octave body params processed-params))
	 (result (org-babel-octave-evaluate
		  session augmented-body result-type matlabp)))
    (or out-file
        (org-babel-reassemble-table
         result
         (org-babel-pick-name
	  (nth 4 processed-params) (cdr (assoc :colnames params)))
         (org-babel-pick-name
	  (nth 5 processed-params) (cdr (assoc :rownames params)))))))

(defun org-babel-prep-session:matlab (session params)
  "Prepare SESSION according to PARAMS."
  (require 'matlab)
  (org-babel-prep-session:octave session params 'matlab))
(defun org-babel-octave-var-to-octave (var)
  "Convert an emacs-lisp value into an octave variable.
Converts an emacs-lisp variable into a string of octave code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-octave-var-to-octave var ", ") "]")
    (format "%S" var)))

(defun org-babel-prep-session:octave (session params &optional matlabp)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-octave-initiate-session session params matlabp))
         (vars (org-babel-ref-variables params))
         (var-lines (mapcar
                     (lambda (pair)
                       (format "%s=%s"
                               (car pair)
                               (org-babel-octave-var-to-octave (cdr pair))))
                     vars)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-matlab-initiate-session (&optional session params)
  "Create a matlab inferior process buffer.
If there is not a current inferior-process-buffer in SESSION then
create. Return the initialized session."
  (require 'matlab)
  (org-babel-octave-initiate-session session params 'matlab))
(defun org-babel-octave-initiate-session (&optional session params matlabp)
  "Create an octave inferior process buffer.
If there is not a current inferior-process-buffer in SESSION then
create. Return the initialized session."
  (require 'octave-inf)
  (unless (string= session "none")
    (let ((session (or session
		       (if matlabp "*Inferior Matlab*" "*Inferior Octave*"))))
      (if (org-babel-comint-buffer-livep session) session
	(save-window-excursion
	  (if matlabp (unless org-babel-matlab-with-emacs-link (matlab-shell))
	    (run-octave))
	  (rename-buffer (if (bufferp session) (buffer-name session)
			   (if (stringp session) session (buffer-name))))
	  (current-buffer))))))

(defun org-babel-octave-evaluate
  (session body result-type lang &optional matlabp)
  "Pass BODY to the octave process in SESSION.
If RESULT-TYPE equals 'output then return the outputs of the
statements in BODY, if RESULT-TYPE equals 'value then return the
value of the last statement in BODY, as elisp."
  (if session
      (org-babel-octave-evaluate-session session body result-type matlabp)
    (org-babel-octave-evaluate-external-process body result-type matlabp)))

(defun org-babel-octave-evaluate-external-process (body result-type matlabp)
  "Evaluate BODY in an external octave process."
  (let ((cmd (if matlabp
		 org-babel-matlab-shell-command
	       org-babel-octave-shell-command)))
    (case result-type
      (output (org-babel-eval cmd body))
      (value (let ((tmp-file (make-temp-file "org-babel-results-")))
	       (org-babel-eval
		cmd
		(format org-babel-octave-wrapper-method body tmp-file tmp-file))
	       (org-babel-eval-read-file tmp-file))))))

(defun org-babel-octave-evaluate-session
  (session body result-type &optional matlabp)
  "Evaluate BODY in SESSION."
  (let* ((tmp-file (make-temp-file "org-babel-results-"))
	 (wait-file (make-temp-file "org-babel-matlab-emacs-link-wait-signal-"))
	 (full-body
	  (case result-type
	    (output
	     (mapconcat
	      #'org-babel-chomp
	      (list body org-babel-octave-eoe-indicator) "\n"))
	    (value
	     (if (and matlabp org-babel-matlab-with-emacs-link)
		 (concat
		  (format org-babel-matlab-emacs-link-wrapper-method
			  body tmp-file tmp-file wait-file) "\n")
	       (mapconcat
		#'org-babel-chomp
		(list (format org-babel-octave-wrapper-method
			      body tmp-file tmp-file)
		      org-babel-octave-eoe-indicator) "\n")))))
	 (raw (if (and matlabp org-babel-matlab-with-emacs-link)
		  (save-window-excursion
		    (with-temp-buffer
		      (insert full-body)
		      (write-region "" 'ignored wait-file nil nil nil 'excl)
		      (matlab-shell-run-region (point-min) (point-max))
		      (message "Waiting for Matlab Emacs Link")
		      (while (file-exists-p wait-file) (sit-for 0.01))
		      "")) ;; matlab-shell-run-region doesn't seem to
			   ;; make *matlab* buffer contents easily
			   ;; available, so :results output currently
			   ;; won't work
		(org-babel-comint-with-output
		    (session
		     (if matlabp
			 org-babel-octave-eoe-indicator
		       org-babel-octave-eoe-output)
		     t full-body)
		  (insert full-body) (comint-send-input nil t)))) results)
    (case result-type
      (value
       (org-babel-octave-import-elisp-from-file
	(org-babel-maybe-remote-file tmp-file)))
      (output
       (progn
	 (setq results
	       (if matlabp
		   (cdr (reverse (delq "" (mapcar
					   #'org-babel-octave-read-string
					   (mapcar #'org-babel-trim raw)))))
		 (cdr (member org-babel-octave-eoe-output
			      (reverse (mapcar
					#'org-babel-octave-read-string
					(mapcar #'org-babel-trim raw)))))))
	 (mapconcat #'identity (reverse results) "\n"))))))

(defun org-babel-octave-import-elisp-from-file (file-name)
  "Import data from FILE-NAME.
This removes initial blank and comment lines and then calls
`org-babel-import-elisp-from-file'."
  (let ((temp-file (make-temp-file "org-babel-results-")) beg end)
    (with-temp-file temp-file
      (insert-file-contents file-name)
      (re-search-forward "^[ \t]*[^# \t]" nil t)
      (if (< (setq beg (point-min))
	     (setq end (point-at-bol)))
	  (delete-region beg end)))
    (org-babel-import-elisp-from-file temp-file)))

(defun org-babel-octave-read-string (string)
  "Strip \\\"s from around octave string"
  (if (string-match "^\"\\([^\000]+\\)\"$" string)
      (match-string 1 string)
    string))

(provide 'ob-octave)

;; arch-tag: d8e5f68b-ba13-440a-a495-b653e989e704

;;; ob-octave.el ends here
