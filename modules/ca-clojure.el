(require 'cider)
(require 'clj-refactor)

(autoload 'clojure-mode "clojure-mode" "clojure mode" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(add-hook 'clojure-mode-hook 'cider-mode)
(setq nrepl-log-messages t)
(setq cider-repl-use-clojure-font-lock t)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq-local cider-repl-use-pretty-printing t)
            (clj-refactor-mode t)
            (cljr-add-keybindings-with-prefix "C-c C-m")))

(defun figwheel-repl ()
  (interactive)
  (run-clojure "lein figwheel"))

(add-hook 'clojurescript-mode-hook #'inf-clojure-minor-mode)

(provide 'ca-clojure)
