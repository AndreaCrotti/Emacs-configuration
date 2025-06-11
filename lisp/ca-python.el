(setq python-shell-completion-native-enable nil)

(use-package blacken
  :ensure t)

(use-package python
  :bind (:map python-ts-mode-map
              ("C-<f5>" . python-pytest-run-def-or-class-at-point-dwim))

  :custom
  ;; should use ipython really
  (python-shell-interpreter "ipython"))

(use-package elpy
  :config (elpy-enable))

(use-package lsp-jedi
  :after lsp)

(use-package lsp-pyright
  :after lsp)

(use-package python-pytest)

(use-package ruff-format)

(use-package pet
  :config
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "ipython")
                          python-shell-virtualenv-root (pet-virtualenv-root))
              (setq-local lsp-pyright-python-executable-cmd (pet-executable-find "python")
                          lsp-pyright-venv-path python-shell-virtualenv-root)
              (setq-local dap-python-executable (pet-executable-find "python"))
              (setq-local python-pytest-executable (pet-executable-find "pytest"))
              (setq-local ruff-format-command (pet-executable-find "ruff"))
              (setq-local flycheck-python-mypy-python-executable (pet-executable-find "mypy"))
              (ruff-format-on-save-mode t))))

(provide 'ca-python)
