;; -*- lexical-binding: t; -*-

(setq python-shell-completion-native-enable nil)

(use-package blacken
  :ensure t)

(use-package python
  :bind (:map python-ts-mode-map
              ("C-<f5>" . python-pytest-run-def-or-class-at-point-dwim))

  :custom
  ;; should use ipython really
  (python-shell-interpreter "ipython"))

(use-package lsp-pyright
  :after lsp)

(use-package python-pytest)

(use-package ruff-format)

(defun pet-setup ()
  (ruff-format-on-save-mode t)
  (pet-mode t))

(use-package pet
  :hook (python-mode . pet-setup)
  :hook (python-ts-mode . pet-setup)
  )

(provide 'ca-python)
