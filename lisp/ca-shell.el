
(use-package vterm)

(use-package shell
  :init
  (dirtrack-mode))

(use-package company-shell)

(use-package fish-mode
  :ensure-system-package fish)

(provide 'ca-shell)
