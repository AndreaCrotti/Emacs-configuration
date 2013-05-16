(require 'jedi)
(require 'ca-customs)
(require 'ca-environment)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)))

(setq jedi:setup-keys t)

(setq
 py-install-directory (make-conf-path "python-mode")
 py-electric-colon-active t)

(load-library (concat py-install-directory "/python-mode"))

; use eval-after-load to set the right keys for python with cedet
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'load-path (make-conf-path "python-mode/test"))
(autoload 'doctest-mode "doctest-mode" "doc test python mode" t)

(if ca-linux
    ;; TODO: should also check if it's actually in the path and check
    ;; that he automatic settings are also working
    (setq py-shell-name "python2"))

(setq
 py-smart-indentation t
 )


(defun ca-python-remove-pdb ()
  "Remove the pdb tracking lines"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (delete-matching-lines "import pdb")))

(add-hook 'python-mode-hook
          (lambda ()
            ;TODO: check if it's actually a good idea
            ;; (py-smart-operator-mode-on)
            (local-set-key (kbd "M-D") 'ca-python-remove-pdb)))

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


; support for coverage
;; (add-to-list 'load-path (make-conf-path "pycoverage.el"))
;; (load-library "pycov2")
;; (require 'linum)
;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (pycov2-mode)
;;              (linum-mode)))

(provide 'ca-python)
