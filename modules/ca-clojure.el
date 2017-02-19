(require 'cider)
(require 'cider-eldoc)
(require 'clj-refactor)
(require 'ca-utils)

(autoload 'clojure-mode "clojure-mode" "clojure mode" t)
(add-hook 'cider-mode-hook
          (lambda () (setq next-error-function #'flycheck-next-error-function)))

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojurescript-mode-hook 'cider-mode)
(setq nrepl-log-messages t)
(setq cider-repl-use-clojure-font-lock t)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq-local cider-repl-use-pretty-printing t)
            (local-set-key [f5] 'helm-imenu)
            (local-set-key [f6] 'cljr-helm)
            (cider-auto-test-mode t)
            ;;TODO: these two can be global keys, possibly checking
            ;;first if M-p/M-n is not already bound to something
            (clj-refactor-mode t)
            (cljr-add-keybindings-with-prefix "C-c C-m")))

(defun figwheel-repl ()
  (interactive)
  (run-clojure "lein figwheel"))

;; this seems to work quite well with figwheel
(setq cider-cljs-lein-repl
      "(do (user/run)
           (user/browser-repl))")

(provide 'ca-clojure)
