(require 'cargo)
(require 'toml-mode)
(require 'flycheck-rust)
(require 'lsp-rust)

(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(add-hook 'rust-mode-hook 'lsp)

(provide 'ca-rust)
