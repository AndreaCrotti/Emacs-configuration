;; -*- lexical-binding: t; -*-

(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package tree-sitter-indent)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;TODO: enable when surely works
  (treesit-auto-add-to-auto-mode-alist nil)
  ;; (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(provide 'ca-treesit)
