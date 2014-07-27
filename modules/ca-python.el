(require 'python-mode)
(require 'virtualenvwrapper)
(require 'jedi)
(require 'ca-customs)
(require 'ca-environment)

(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)

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


;TODO: add some binding to jump to things that are not defined
(add-hook 'python-mode-hook
          (lambda ()
            (jedi-setup-venv)
            (jedi:setup)
            (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)
            (local-set-key (kbd "M-D") 'ca-python-remove-pdb)))

(setq
 py-electric-colon-active t
 py-smart-indentation t
)

; use eval-after-load to set the right keys for python with cedet
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'load-path (make-conf-path "python-mode/test"))
(autoload 'doctest-mode "doctest-mode" "doc test python mode" t)

(if ca-linux
    ;; TODO: should also check if it's actually in the path and check
    ;; that he automatic settings are also working
    (setq py-shell-name "python2"))

;FIXME: not really working yet, and try to make it more generic
(defun ca-python-setup ()
  (interactive)
  (let
      ((project-root (ca-find-project-root))
       (command (completing-read "command to launch: "
                                 '("test" "develop" "bdist_egg")
                                 nil 'confirm nil "test")))
    (with-temp-buffer
      (compilation-mode)
      (shell-command (format "cd %s && %s setup.py %s"
                             project-root py-shell-name command)))))


;TODO: check that this really works
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline 'pdb.py
                            (file-name-nondirectory buffer-file-name)))))

(provide 'ca-python)
