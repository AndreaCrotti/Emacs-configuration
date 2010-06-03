;;; vm-summary-faces.el --- faces support for VM summary buffers
;; 
;; Copyright (C) 2001 Robert Fenk
;;
;; Author:      Robert Fenk
;; Status:      Tested with XEmacs 21.4.15 & VM 7.18
;; Keywords:    VM 
;; X-URL:       http://www.robf.de/Hacking/elisp

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

;;; Commentary:
;;
;;  to use this add the following line to your ~/.vm file
;;
;;  (require 'vm-summary-faces)
;;  (vm-summary-faces-mode 1)
;;

(defgroup vm nil
  "VM"
  :group 'mail)

(defgroup vm-summary-faces nil
  "VM additional virtual folder selectors and functions."
  :group 'vm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'advice)
  (require 'vm-summary)
  (require 'vm-virtual))

(eval-and-compile
  (if vm-xemacs-p (require 'overlay)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface vm-summary-selected-face
  '((t (:bold on)))
  "The face used in VM Summary buffers for the selected message."
  :group 'vm-summary-faces)

(defface vm-summary-marked-face
  '((((type x)) (:foreground "red3")))
  "The face used in VM Summary buffers for marked messages."
  :group 'vm-summary-faces)

(defface vm-summary-deleted-face
     (if (featurep 'xemacs)
         '((t (:foreground "grey50" :strikethru t)))
       '((t (:foreground "grey50" :strike-through "grey70"))))
     "The face used in VM Summary buffers for deleted messages."
     :group 'vm-summary-faces)

(defface vm-summary-new-face
  '((t (:foreground "blue")))
  "The face used in VM Summary buffers for new messages."
  :group 'vm-summary-faces)

(defface vm-summary-unread-face
  '((t (:foreground "blue4")))
  "The face used in VM Summary buffers for unread messages."
  :group 'vm-summary-faces)

(defface vm-summary-filed-face
  '((t (:foreground "green4" :underline t)))
  "The face used in VM Summary buffers for filed messages."
  :group 'vm-summary-faces)

(defface vm-summary-written-face
  '((t (:foreground "green4" :underline t)))
  "The face used in VM Summary buffers for written messages."
  :group 'vm-summary-faces)

(defface vm-summary-replied-face
  '((t (:foreground "grey50")))
  "The face used in VM Summary buffers for replied messages."
  :group 'vm-summary-faces)

(defface vm-summary-forwarded-face
  '((t (:foreground "grey50")))
  "The face used in VM Summary buffers for forwarded messages."
  :group 'vm-summary-faces)

(defface vm-summary-edited-face 
  nil
  "The face used in VM Summary buffers for edited messages."
  :group 'vm-summary-faces)

(defface vm-summary-redistributed-face
  '((t (:foreground "grey50")))
  "The face used in VM Summary buffers for redistributed messages."
  :group 'vm-summary-faces)

(defface vm-summary-outgoing-face
  '((t (:foreground "grey50")))
  "The face used in VM Summary buffers for outgoing messages."
  :group 'vm-summary-faces)

(defface vm-summary-high-priority-face
  '((t (:foreground "red")))
  "The face used in VM Summary buffers for high-priority messages."
  :group 'vm-summary-faces)

(defface vm-summary-default-face
  nil
  "The default face used in VM Summary buffers."
  :group 'vm-summary-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vm-summary-faces-alist
  '(
    ((or (header "Priority: urgent")
         (header "Importance: high")
         (header "X-Priority: 1")
         (label "!")
	 (label "\\flagged")
         (header "X-VM-postponed-data:"))
     vm-summary-high-priority-face)
    ((deleted)   vm-summary-deleted-face)
    ((new)       vm-summary-new-face)
    ((unread)    vm-summary-unread-face)
    ((filed)     vm-summary-filed-face)
    ((written)   vm-summary-written-face)
    ((replied)   vm-summary-replied-face)
    ((forwarded) vm-summary-forwarded-face)
    ((edited)    vm-summary-edited-face)
    ((redistributed) vm-summary-redistributed-face)
    ((marked)    vm-summary-marked-face)
    ((outgoing)  vm-summary-outgoing-face)
    ((any)       vm-summary-default-face))
  "*Alist of virtual folder conditions and corresponding faces.
Order matters. The first matching one will be used as face."
  :type '(repeat (cons (sexp) (face)))
  :group 'vm-summary-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (if (fboundp 'mapcar-extents)
      (defun vm-summary-faces-list-extents () (mapcar-extents 'identity))
    (defun vm-summary-faces-list-extents ()
      (let ((o (overlay-lists))) (nconc (car o) (cdr o))))))

(defvar vm-summary-faces-hide nil
  "Last face hidden by `vm-summary-faces-hide'.")

;;;###autoload
(defun vm-summary-faces-hide (&optional face)
  "Toggle visibility of messages with FACE.
When called with a prefix arg prompt for the face."
  (interactive "P")
  (if (and (listp face) (numberp (car face)))
      (setq face (completing-read "Face name: "
                                  (mapcar (lambda (f)
                                            (list (format "%s" (caar f))))
                                          vm-summary-faces-alist)
                                  nil t "deleted")))
  (setq face (or face vm-summary-faces-hide "deleted"))
  (vm-summarize)
  (vm-select-folder-buffer)
  (set-buffer vm-summary-buffer)
  (let ((extents (vm-summary-faces-list-extents))
        (face (intern (concat "vm-summary-" face "-face")))
        x)
    (while extents
      (setq x (car extents)) 
      (when (equal face (vm-extent-property x 'face))
        (vm-set-extent-property x 'invisible (not (vm-extent-property x 'invisible))))
      (setq extents (cdr extents)))))

;;;###autoload
(defun vm-summary-faces-add (msg)
  "Add a face to a summary entry according to `vm-summary-faces-alist'."
  (let ((faces vm-summary-faces-alist)
        (x (or (vm-su-summary-mouse-track-overlay-of msg)
               (vm-extent-at (vm-su-start-of msg))
               (vm-extent-at (vm-su-end-of msg)))))
    (while faces
      (when (apply 'vm-vs-or msg (list (caar faces)))
        (vm-set-extent-property x 'face (cadar faces))
        (setq faces nil))
      (setq faces (cdr faces)))))

(defun vm-summary-faces-destroy ()
  "Removes the face from all summary entries."
  (let ((extents (vm-summary-faces-list-extents))
        x)
    (while extents
      (setq x (car extents))
      (vm-set-extent-property x 'face nil)
      (setq extents (cdr extents)))))

(defvar vm-summary-faces-mode nil)

;;;###autoload
(defun vm-summary-faces-mode (&optional arg)
  "Toggle `vm-summary-faces-mode'.
Remove/add the `vm-summary-fontify-buffer' hook from the hook variable
`vm-summary-mode-hook' and when in a summary buffer, then toggle the
`font-lock-mode'."
  (interactive "P")
  (if (null arg)
      (setq vm-summary-faces-mode (not vm-summary-faces-mode))
    (if (> (prefix-numeric-value arg) 0)
        (setq vm-summary-faces-mode t)
      (setq vm-summary-faces-mode nil)))

  (when (interactive-p)
    (message "VM summary faces mode is %s"
             (if vm-summary-faces-mode "on" "off")))
  
  (if (memq major-mode '(vm-mode vm-virtual-mode vm-summary-mode
                                 vm-presentation-mode))
      (save-excursion
        (vm-select-folder-buffer)
        (vm-summarize)
        (set-buffer vm-summary-buffer)
        (if vm-summary-faces-mode
            (let ((mp vm-message-list))
              (while mp
                (vm-summary-faces-add (car mp))
                (setq mp (cdr mp))))
          (vm-summary-faces-destroy)
          (if vm-summary-overlay
              (vm-set-extent-property vm-summary-overlay 'face
                                      vm-summary-highlight-face))))))

(defadvice vm-mouse-set-mouse-track-highlight (after vm-summary-faces activate)
  (when (and vm-summary-faces-mode
             (eq major-mode 'vm-summary-mode)
             (boundp 'm)
             m)
    ;; FIXME there is a warning about a free variable here, sorry!
    (vm-summary-faces-add m)))

(defun vm-summary-faces-fix-pointer ()
  (if vm-summary-overlay
      (vm-set-extent-property vm-summary-overlay 'face
			            (if vm-summary-faces-mode
					'vm-summary-selected-face
				      vm-summary-highlight-face))))

(add-hook 'vm-summary-pointer-update-hook 'vm-summary-faces-fix-pointer)

(provide 'vm-summary-faces)
