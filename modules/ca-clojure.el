
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(autoload 'clojure-mode "clojure-mode" "clojure mode" t)

(defun turn-on-paredit ()
  (paredit-mode t))

(add-hook 'clojure-mode-hook 'turn-on-paredit)

(autoload 'slime "swank-clojure" "loading the swank-clojure" t)
(add-hook 'slime-mode-hook
          '(lambda ()
             (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))))

(provide 'ca-clojure)