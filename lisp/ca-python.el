(use-package blacken
  :ensure t)

(use-package python
  :custom
  ;; should use ipython really
  (python-shell-interpreter "ipython"))

(use-package elpy
  :hook ((python-ts-mode . elpy-mode))
  :init (elpy-enable))

(use-package lsp-jedi
  :after lsp)

(use-package lsp-pyright
  :after lsp)

(use-package python-pytest
  :ensure t)

(use-package python-black
  :ensure t)

(use-package python-isort)

(use-package ruff-format)

(use-package pet
  ;; :ensure-system-package (dasel sqlite3)
  :config
  (add-hook 'python-ts-mode-hook
            (lambda ()
               (setq-local python-shell-interpreter (pet-executable-find "ipython")
                          python-shell-virtualenv-root (pet-virtualenv-root))
              (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
                          lsp-pyright-venv-path python-shell-virtualenv-root)
              (setq-local dap-python-executable python-shell-interpreter)
              (setq-local python-pytest-executable (pet-executable-find "pytest"))

              (pet-flycheck-setup)
              (flycheck-mode)
              (ruff-format-on-save-mode t))))

(use-package company-jedi
  ;; is this actually doing anything?
  ;; :config
  ;; (add-to-list 'company-backends 'company-jedi)
  )

(provide 'ca-python)
