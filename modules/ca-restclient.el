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
            (define-key outline-minor-mode-map (kbd "<tab>") 'outline-cycle)))

(provide 'ca-restclient)
