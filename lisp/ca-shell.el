
(use-package vterm
  :custom
  (vterm-shell "/usr/bin/fish"))

(use-package shell
  :config
  (dirtrack-mode t))

(use-package company-shell)

(use-package fish-mode)

(use-package bats-mode)

(add-to-list 'exec-path "~/.local/bin")

(provide 'ca-shell)
