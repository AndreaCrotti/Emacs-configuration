
(use-package vterm
  :custom
  (vterm-shell "/usr/bin/fish"))

(use-package shell
  :init
  (dirtrack-mode))

(use-package company-shell)

(use-package fish-mode)

(use-package bats-mode)

(provide 'ca-shell)
