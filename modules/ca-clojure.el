(require 'cider)
(require 'clj-refactor)

(autoload 'clojure-mode "clojure-mode" "clojure mode" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojurescript-mode-hook 'cider-mode)
(setq nrepl-log-messages t)
(setq cider-repl-use-clojure-font-lock t)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)

(defun ca-next-defun ()
  (interactive)
  (end-of-defun 2)
  (beginning-of-defun 1))

(defun ca-prev-defun ()
  (interactive)
  (beginning-of-defun))

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq-local cider-repl-use-pretty-printing t)
            (local-set-key [f5] 'helm-imenu)
            ;;TODO: these two can be global keys, possibly checking
            ;;first if M-p/M-n is not already bound to something
            (local-set-key (kbd "M-p") 'ca-prev-defun)
            (local-set-key (kbd "M-n") 'ca-next-defun)
            (clj-refactor-mode t)
            (cljr-add-keybindings-with-prefix "C-c C-m")))

(defun figwheel-repl ()
  (interactive)
  (run-clojure "lein figwheel"))

(provide 'ca-clojure)
