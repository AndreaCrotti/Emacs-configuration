(autoload 'malabar-mode "malabar-mode" "malabar mode" t)
(add-to-list 'auto-mode-alist '("\\.java$" . malabar-mode))

(require 'cedet)
(require 'semantic)
(load "semantic/loaddefs.el")
(semantic-mode 1);;

(require 'malabar-mode)

(add-hook 'malabar-mode-hook
     (lambda ()
       (add-hook 'after-save-hook 'malabar-compile-file-silently
                  nil t)))
(add-hook 'malabar-mode-hook
          '(lambda ()

             (add-to-list 'ac-omni-completion-sources
                          (cons "\\." '(ac-source-semantic)))
             ;; ac-sources was also made buffer local in new versions of
             ;; autocomplete.  In my case, I want AutoComplete to use
             ;; semantic and yasnippet (order matters, if reversed snippets
             ;; will appear before semantic tag completions).
             (setq ac-sources '(ac-source-semantic ac-source-yasnippet ac-source-gtags ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))))

(provide 'ca-java)
