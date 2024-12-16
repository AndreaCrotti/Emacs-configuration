
(use-package vterm
  :custom
  (vterm-shell "/usr/bin/fish"))

(use-package shell
  :init
  (dirtrack-mode))

(use-package company-shell)

(use-package fish-mode
  :ensure-system-package fish)

(use-package bats-mode)

(provide 'ca-shell)
