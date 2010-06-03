;;; vm-mime-display-internal-application.el --- Display application attachments
;;; -*-unibyte: t; coding: iso-8859-1;-*-

;; Copyright © 2004 Kevin Rodgers

;; Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; Created: 11 Jun 2004
;; Version: $Revision: 1.5 $
;; Keywords: mail, mime
;; RCS: $Id: vm-mime-display-internal-application.el,v 1.5 2004/07/14 23:29:04 kevinr Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; VM does not provide a way to display additional MIME media types
;; internally.  This file defines a new user variable to control which
;; application/* subtypes can be displayed within Emacs:
;; C-h v vm-mime-internal-application-subtypes
;; 
;; It also defines user commands to register a subtype and to install
;; all registered subtypes as internally displayable applications:
;; M-x vm-mime-register-internal-application
;; M-x vm-mime-install-internal-applications
;; 
;; Usage:
;; (load-library "vm-mime-display-internal-application")
;; (vm-mime-register-internal-application "foo" t) ; to run foo-mode
;; (vm-mime-register-internal-application "bar" 'baz-mode)
;; (vm-mime-install-internal-applications)

;;; Code:

(require 'vm)

(defvar vm-mime-internal-application-subtypes
  ;; see http://www.iana.org/assignments/media-types/application/
  '(("emacs-lisp" . t)                  ; lisp-mode.el
    ("tar" . t)                         ; tar-mode.el
    ("arc" . archive-mode)              ; arc-mode.el
    ("lzh" . archive-mode)              ; arc-mode.el
    ("zip" . archive-mode)              ; arc-mode.el
    ("zoo" . archive-mode)              ; arc-mode.el
    ;; For file-name-handler subtypes, let find-file-noselect ->
    ;; after-find-file -> (normal-mode t) choose the mode.  Specify
    ;; ignore instead of normal-mode for these subtypes, so that the
    ;; optional FIND-FILE argument doesn't override enable-local-variables.
    ("gzip" . ignore)                   ; jka-compr.el
    ("bzip2" . ignore)                  ; jka-compr.el
    ("compress" . ignore))              ; jka-compr.el
  "List of MIME \"application/*\" subtypes that should be displayed internally.

Each (SUBTYPE . MODE) element maps the \"applicaton/SUBTYPE\" MIME
content type to the major MODE used to display it.  Both the MODE and
`vm-mime-display-internal-application/SUBTYPE' functions must be
defined.

If MODE is t, SUBTYPE-mode is used to display \"application/SUBTYPE\"
attachments.")

(defvar vm-mime-internal-application-x-subtypes nil
  "*If non-nil, display application/x-SUBTYPE attachments the same as application/SUBTYPE attachments.
See `vm-mime-internal-application-subtypes'.")

(defadvice vm-mime-can-display-internal (after application/xxxx activate
                                         compile)
  "Respect `vm-mime-internal-application-subtypes'."
  (or ad-return-value
      (setq ad-return-value
            (let* ((layout (ad-get-arg 0))
                   (type (car (vm-mm-layout-type layout)))
                   (subtype (if (vm-mime-types-match "application" type)
                                (substring type (1+ (match-end 0)))))
                   (mode (if subtype
                             (vm-mime-can-display-internal-application
                              subtype))))
              (if mode
                  (let ((charset (or (vm-mime-get-parameter layout "charset")
                                     "us-ascii")))
                    (or (vm-mime-charset-internally-displayable-p charset)
                        (vm-mime-can-convert-charset charset))))))))

(defun vm-mime-can-display-internal-application (subtype)
  "Return the Emacs mode for displaying \"application/SUBTYPE\" MIME objects."
  (catch 'major-mode
    (let ((subtypes vm-mime-internal-application-subtypes)
          mode)
      (while subtypes
        (if (or (equal subtype (car (car subtypes)))
                (and vm-mime-internal-application-x-subtypes
                     (equal subtype (concat "x-" (car (car subtypes))))))
            (cond ((and (eq (cdr (car subtypes)) 't)
                        (fboundp (setq mode (intern (concat subtype "-mode")))))
                   (throw 'major-mode mode))
                  ((fboundp (setq mode (cdr (car subtypes))))
                   (throw 'major-mode mode))))
        (setq subtypes (cdr subtypes)))
      nil)))

(defun vm-mime-display-internal-application/xxxx (layout)
  "Display LAYOUT in its own buffer."
  ;; see vm-mime-display-external-generic
  (let* ((tempfile (or (get (vm-mm-layout-cache layout)
                            'vm-mime-display-internal-application/xxxx)
                       (let ((suffix
                              (or (vm-mime-extract-filename-suffix layout)
                                  (vm-mime-find-filename-suffix-for-type
                                   layout)))
                             (filename
                              (or (vm-mime-get-disposition-parameter layout
                                                                     "filename")
                                  (vm-mime-get-parameter layout "name"))))
                         (vm-make-tempfile-name suffix filename))))
         (type (car (vm-mm-layout-type layout)))
         (subtype (if (vm-mime-types-match "application" type)
                      (substring type (1+ (match-end 0))))))
    (vm-mime-send-body-to-file layout nil tempfile)
    (vm-register-message-garbage-files (list tempfile))
    (put (vm-mm-layout-cache layout)
         'vm-mime-display-internal-application/xxxx
         tempfile)
    (let* ((inhibit-local-variables t)
           (enable-local-variables nil)
           (enable-local-eval nil)
           (pop-up-frames vm-mutable-frames)
           (pop-up-windows vm-mutable-windows)
           (mode (vm-mime-can-display-internal-application subtype)))
      (pop-to-buffer
       (find-file-noselect tempfile)) ; (with-auto-compression-mode ...)
      (or (eq major-mode mode)
          (funcall mode))
;;      (when pop-up-frames
;;        (set-window-dedicated-p (selected-window) t))
      (cond (pop-up-frames
             (add-hook 'kill-buffer-hook 'delete-frame t t))
            (pop-up-windows
             (add-hook 'kill-buffer-hook 'delete-window t t))))))

(defun vm-mime-register-internal-application (subtype mode)
  "Add (SUBTYPE . MODE) to `vm-mime-internal-application-subtypes'.
Also define the `vm-mime-display-internal-application/SUBTYPE' and
`vm-mime-display-button-application/SUBTYPE' functions.

If MODE is nil, just define the functions."
  (interactive
   (let* ((subtype (completing-read "Subtype: "
                                    vm-mime-internal-application-subtypes))
          (subtype-mode (fboundp (intern (concat subtype "-mode"))))
          (completion-ignore-case nil)
          (mode (intern (completing-read (if subtype-mode
                                             "Mode: (default t) "
                                           "Mode: ")
                                         obarray
                                         (lambda (s)
                                           (and (fboundp s)
                                                (string-match "-mode\\'"
                                                              (symbol-name s))))
                                         t nil nil (if subtype-mode "t")))))
     (or (eq mode 't)
         (fboundp mode)                ; i.e. (equal (symbol-name mode) "")
         (error "Undefined mode: %s" mode)) ; (unintern mode)
     (list subtype mode)))
  (if mode
      (setq vm-mime-internal-application-subtypes
            (cons (cons subtype mode) vm-mime-internal-application-subtypes)))
  (let ((internal
         (intern (concat "vm-mime-display-internal-application/" subtype)))
        (button
         (intern (concat "vm-mime-display-button-application/" subtype))))
    (defalias internal 'vm-mime-display-internal-application/xxxx)
    (fset button (lambda (layout)
                   (vm-mime-display-button-xxxx layout nil)))
    (if vm-mime-internal-application-x-subtypes
        (progn
          (defalias (intern (concat "vm-mime-display-internal-application/x-"
                                    subtype))
            internal)
          (defalias (intern (concat "vm-mime-display-button-application/x-"
                                    subtype))
            button)))))

(defun vm-mime-install-internal-applications ()
  "Define display and button functions for each registered subtype.
See `vm-mime-internal-application-subtypes'."
  (interactive)
  (let ((subtypes vm-mime-internal-application-subtypes))
    (while subtypes
      (vm-mime-register-internal-application (car (car subtypes)) nil)
      (setq subtypes (cdr subtypes)))))

;;; vm-mime-display-internal-application.el ends here
