(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(add-to-list 'auto-mode-alist '("\\.clj$" . cider-mode))
(autoload 'clojure-mode "clojure-mode" "clojure mode" t)

(setq nrepl-log-messages t)
(setq cider-repl-use-clojure-font-lock t)

(require 'company)
(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)

;; (autoload 'slime "swank-clojure" "loading the swank-clojure" t)
;; (add-hook 'slime-mode-hook
;;           '(lambda ()
;;              (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))))

;; (require 'ac-slime)
;; (add-hook 'slime-mode-hook 'set-up-slime-ac)

(provide 'ca-clojure)
