(require 'flycheck)

(with-eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-elm))

(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

(provide 'ca-elm)
