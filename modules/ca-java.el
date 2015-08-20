(autoload 'malabar-mode "malabar-mode" "malabar mode" t)
(add-to-list 'auto-mode-alist '("\\.java$" . malabar-mode))

(require 'cedet)
(require 'semantic)
(load "semantic/loaddefs.el")
(semantic-mode 1);;

(require 'malabar-mode)

(add-hook 'malabar-mode-hook
     (lambda ()
       (add-hook 'after-save-hook 'malabar-compile-file-silently
                  nil t)))

(provide 'ca-java)
