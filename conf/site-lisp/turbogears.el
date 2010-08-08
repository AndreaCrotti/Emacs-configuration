;;; turbogears.el --- Turbogears minor mode

;; Copyright (C) 2010  Andrea Crotti

;; Author: Andrea Crotti <andrea.crotti.0@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode for working with turbogears

;;; Code:

;;; turbogears.el ends here


(defun turn-on-turbogears-mode ()
  "Enable turbogears mode"
  (turbogears-mode 1))

(defvar turbogears-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'tg-restart-server)
    ;; other useful keys could be
    ;; - see the server logs
    ;; - go to setup.py
    ;; - update and edit the gettext texts
    map)
  "Keymapping used in turbogears mode")

(defun tg-restart-server ()
  nil)

;; Now create a possible keymap

(defvar turbogears-mode-hook nil
  "*Hook called by `turbogears-mode' buffers.")

; TODO: We need load the environment from the bin/activate file
; otherwise no way we can use the right paster

(defun run-turbogears ()
  "Run an interactive turbogears shell"
  (interactive)
  (require 'comint)
  (switch-to-buffer (make-comint "turbogears" "paster" "shell" "ldapper/development.ini"))
  ;; now maybe pass to the right mode
)

(define-minor-mode turbogears-mode "mode for developing with turbogears"
  :lighter " tg"
  :init-value nil
  :keymap turbogears-mode-map)

(provide 'turbogears)
