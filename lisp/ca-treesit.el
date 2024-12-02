(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package tree-sitter-indent)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package combobulate
   :custom
   ;; You can customize Combobulate's key prefix here.
   ;; Note that you may have to restart Emacs for this to take effect!
   (combobulate-key-prefix "C-c o")
   :hook ((prog-mode . combobulate-mode))
   ;; Amend this to the directory where you keep Combobulate's source
   ;; code.
   :load-path ("~/src/forks/combobulate/"))

;; (use-package ellama
;;   :init
;;   (setopt ellama-language "English")
;;   ;; (require 'llm-ollama)
;;   (setopt ellama-provider
;; 	  (make-llm-ollama
;; 	   :chat-model "codellama" :embedding-model "codellama")))

(provide 'ca-treesit)
