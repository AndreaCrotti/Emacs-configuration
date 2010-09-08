;;; translate.el --- use babel to create a translate mode

;; Copyright (C) 2010  Andrea Crotti

;; Author: Andrea Crotti <andrea.crotti.0@gmail.com>
;; Keywords:
;; Use post-command-hook to update the status of the translated buffer

(require 'babel)

(define-minor-mode translated-buffer-mode
  "Minor mode for the translation"
  nil
  :group translated-buffer-mode
;TODO: add other necessary info 
  )

(defun translate-mode ()
  (interactive)
  )