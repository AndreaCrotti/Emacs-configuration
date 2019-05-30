(require 'cider)
(require 'cider-eldoc)
;;(require 'clj-refactor)
(require 'ca-utils)
(require 'flycheck-joker)
(require 'flycheck-clj-kondo)

(dolist (checkers '((clj-kondo-clj . clojure-joker)
                    (clj-kondo-cljs . clojurescript-joker)
                    (clj-kondo-cljc . clojure-joker)))
  (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))

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
            (local-set-key (kbd "<C-f5>") 'cider-test-run-test)
            (cider-auto-test-mode t)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (local-set-key [f6] 'cljr-helm)))

;; a few useful functions
(defun kaocha ()
  (interactive)
  (elein-run-cmd "kaocha"))

(defun lein-test ()
  (interactive)
  (elein-run-cmd "test"))

(add-to-list 'auto-mode-alist '("\\riemann.config\\'" . clojure-mode))
(provide 'ca-clojure)
