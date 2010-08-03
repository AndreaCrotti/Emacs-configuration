;;; virtual.el --- Support for virtualenv for python-mode.el

;; Copyright (C) 2010  Andrea Crotti

;; Author: Andrea Crotti <andrea.crotti.0@gmail.com>
;; Keywords: languages

;; TODO: change exec path also
;; TODO: make it less intrusive (don't use the global variable)

(require 'python-mode)

(defun virtual-set-interpreter ()
  (interactive)
  (let
      ((interpreter (expand-file-name "bin/python")))
    (if 
        (file-exists-p "bin/python")
        (progn
          (message "setting the local interpreter")
          (setq py-python-command interpreter)
          (add-to-list 'exec-path (expand-file-name "bin")))
      (message "not possible to setup the environment"))))

(defun virtual-deactivate ()
  (interactive)
  (message "deactivate the environment")
  ;; TODO: add a way to revert the exec path change
  (py-toggle-shells 1))

(setq paster-shell "paster"
      paster-args '("shell" "ldapper/development.ini"))

(defun run-turbogears ()
  "Run an interactive turbogears shell"
  (interactive)
  (require 'comint)
  (switch-to-buffer (make-comint "turbogears" "paster" "shell" "ldapper/development.ini"))
  ;; now maybe pass to the right mode
)

(provide 'virtual)
