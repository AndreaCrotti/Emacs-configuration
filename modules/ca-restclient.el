(require 'restclient)
(require 'outline)
(require 'outline-magic)

(add-to-list 'auto-mode-alist '("\\.rest" . restclient-mode))

(eval-after-load 'outline
  '(progn
     (require 'outline-magic)))

(add-hook 'restclient-mode-hook 'outline-minor-mode)
(add-hook 'restclient-mode-hook
          (lambda ()
            (outline-minor-mode t)
            (local-set-key (kbd "<tab>") 'outline-cycle)
            (setq outline-regexp "#+")))

(provide 'ca-restclient)