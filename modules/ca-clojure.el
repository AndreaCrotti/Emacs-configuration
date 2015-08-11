(require 'cider)
(require 'clj-refactor)

(autoload 'clojure-mode "clojure-mode" "clojure mode" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(add-hook 'clojure-mode-hook 'cider-mode)
(setq nrepl-log-messages t)
(setq cider-repl-use-clojure-font-lock t)

; cider mode works better with company-mode than ac apparently
(require 'company)
(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)


(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode t)
            (cljr-add-keybindings-with-prefix "C-c C-m")))

(defun figwheel-repl ()
  (interactive)
  (run-clojure "lein figwheel"))

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

(provide 'ca-clojure)
