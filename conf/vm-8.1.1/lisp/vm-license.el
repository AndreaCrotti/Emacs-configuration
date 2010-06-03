;;; vm-license.el --- Code to show VM's warranty and copying restrictions
;;
;; Copyright (C) 1989, 1994 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
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

;;;###autoload
(defun vm-show-copying-restrictions (&optional warranty)
  "Show VM's license, i.e. the GPL."
  (interactive)
  (require 'info)
  (let ((pop-up-windows (eq vm-mutable-windows t))
	(pop-up-frames (and vm-mutable-frames vm-frame-per-help)))
    (or 
     (condition-case ()
	 (progn (Info-goto-node "(vm)License") t)
       (error nil))
     (condition-case ()
	 (progn (Info-goto-node "(vm.info)License") t)
       (error nil))
     (error "VM Info documentation appears not to be installed"))
    (vm-display (current-buffer) t nil nil)
    (vm-display nil nil '(vm-show-copying-restrictions vm-show-no-warranty)
		(list this-command))
    (if warranty
	(let ((case-fold-search nil))
	  (search-forward "NO WARRANTY\n" nil t)
	  (forward-line -1)
	  (set-window-start (selected-window) (point))))))

;;;###autoload
(defun vm-show-no-warranty ()
  "Display \"NO WARRANTY\" section of the GNU General Public License."
  (interactive)
  (vm-show-copying-restrictions t))

(provide 'vm-license)

;;; vm-license.el ends here
