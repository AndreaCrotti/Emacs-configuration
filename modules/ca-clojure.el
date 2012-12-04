
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(autoload 'clojure-mode "clojure-mode" "clojure mode" t)

(autoload 'slime "swank-clojure" "loading the swank-clojure" t)
(add-hook 'slime-mode-hook
          '(lambda ()
             (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))))

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

(provide 'ca-clojure)
