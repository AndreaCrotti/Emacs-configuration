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

;; (use-package combobulate
;;    :custom
;;    ;; You can customize Combobulate's key prefix here.
;;    ;; Note that you may have to restart Emacs for this to take effect!
;;    (combobulate-key-prefix "C-c o")
;;    :hook ((prog-mode . combobulate-mode))
;;    ;; Amend this to the directory where you keep Combobulate's source
;;    ;; code.
;;    ;;TODO: make sure this actually exists somewhere
;;    :load-path ("~/src/forks/combobulate/"))

(provide 'ca-treesit)
