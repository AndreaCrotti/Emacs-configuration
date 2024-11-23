(use-package blacken)
(use-package python
  :custom
  ;; should use ipython really
  (python-shell-interpreter "python3"))

(use-package elpy
  :ensure t
  :hook ((python-mode . elpy-mode))
  :init (elpy-enable))

(use-package lsp-jedi
  :after lsp)

(use-package lsp-pyright
  :after lsp)

(use-package dap-python
  :after lsp)

(use-package python-pytest)

(use-package python-black)

(use-package python-isort
  :ensure t)

(use-package ruff-format
  :ensure t)

(use-package pet
  :ensure t
  :ensure-system-package (dasel sqlite3)
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))

              (pet-flycheck-setup)
              (flycheck-mode)

              (setq-local lsp-jedi-executable-command
                          (pet-executable-find "jedi-language-server"))

              (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
                          lsp-pyright-venv-path python-shell-virtualenv-root)

              (lsp)

              (setq-local dap-python-executable python-shell-interpreter)

              (setq-local python-pytest-executable (pet-executable-find "pytest"))

              (when-let ((ruff-executable (pet-executable-find "ruff")))
                (setq-local ruff-format-command ruff-executable)
                (ruff-format-on-save-mode))

              (when-let ((black-executable (pet-executable-find "black")))
                (setq-local python-black-command black-executable)
                (python-black-on-save-mode))

              (when-let ((isort-executable (pet-executable-find "isort")))
                (setq-local python-isort-command isort-executable)
                (python-isort-on-save-mode)))))

(provide 'ca-python)
