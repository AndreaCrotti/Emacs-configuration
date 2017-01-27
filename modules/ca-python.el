(require 'virtualenvwrapper)
(require 'jedi)
(require 'ca-customs)
(require 'ca-environment)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'load-path (make-conf-path "python-mode/test"))
(autoload 'doctest-mode "doctest-mode" "doc test python mode" t)

(venv-initialize-interactive-shells)
(venv-initialize-eshell)

(setq
 jedi:complete-on-dot t
 jedi:setup-keys t
 py-electric-colon-active t
 py-smart-indentation t)


(defun ca-python-remove-pdb ()
  "Remove the pdb tracking lines"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (delete-matching-lines "import pdb")))

(defun jedi-setup-venv ()
  "Activates the virtualenv of the current buffer."
  (let ((project-name (ca-project-name buffer-file-name)))
    (if (venv-is-valid project-name)
        (venv-workon project-name)
      (if (and (boundp 'project-venv-name)
               (venv-is-valid project-venv-name))
          (venv-workon project-venv-name)
        (print "Not finding any virtualenv")))))


(require 'company-jedi)
;TODO: add some binding to jump to things that are not defined
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-jedi)
            (hack-local-variables)
            (jedi-setup-venv)
            (jedi:setup)
            (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            ;; (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)
            (local-set-key (kbd "M-D") 'ca-python-remove-pdb)))

(provide 'ca-python)
