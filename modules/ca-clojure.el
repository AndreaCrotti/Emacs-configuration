(require 'cider)
(require 'cider-eldoc)
;;(require 'clj-refactor)
(require 'ca-utils)
(require 'flycheck-clj-kondo)

(autoload 'clojure-mode "clojure-mode" "clojure mode" t)
(add-hook 'cider-mode-hook
          (lambda ()
            (setq next-error-function #'flycheck-next-error-function)))

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(setq nrepl-log-messages t)
(setq cider-repl-use-clojure-font-lock t)

(defun ca-cider-or-dumb-jump ()
  (interactive)
  (if (cider-connected-p)
      (cider-find-var)
    (dumb-jump-go)))

(add-hook 'clojure-mode-hook #'cider-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq-local cider-repl-use-pretty-printing t)
            (local-set-key (kbd "M-?") 'ca-cider-or-dumb-jump)
            (local-set-key (kbd "M-.") 'ca-cider-or-dumb-jump)
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

(defun ca-carve ()
  (interactive)
  (compile
   "clojure -Acarve --opts '{:paths [\"src\" \"test\"] :report {:format :text}}' && exit 1"))

(defun ca-carve-src-only ()
  (interactive)
  (compile
   "clojure -Acarve --opts '{:paths [\"src\"] :report {:format :text}}' && exit 1"))

(add-to-list 'auto-mode-alist '("\\riemann.config\\'" . clojure-mode))

(defun prf/cider/send-to-repl (sexp &optional eval ns)
  "Send SEXP to Cider Repl. If EVAL is t, evaluate it.
Optionally, we can change namespace by specifying NS."
  (cider-switch-to-repl-buffer ns)
  (goto-char cider-repl-input-start-mark)
  (delete-region (point) (point-max))
  (save-excursion
    (insert sexp)
    (when (equal (char-before) ?\n)
      (delete-char -1)))
  (when eval
    (cider-repl--send-input t)))

(defun prf/clj/pomegranate-dep (dep)
  "Format a Clojure Pomegranate dependency import for DEP."
  (concat
   (format
    "%s"
    ;; NB: this is clojure!
    `(use '[cemerick.pomegranate :only (add-dependencies)]))
   (s-replace-all
    `(("\\." . ".")
      ("mydep" . ,dep))
    (format
     "%S"
     ;; NB: this is clojure!
     `(add-dependencies :coordinates '[mydep]
                        :repositories (merge cemerick.pomegranate.aether/maven-central
                                             {"clojars" "https://clojars.org/repo"}))))))

(defun prf/cider/inject-pomegranate-dep (&optional dep ns)
  "Auto-import DEP in the current Clojure Repl using Pomegranate.
Optionally, we can change namespace by specifying NS."
  (interactive)
  (setq dep (or dep (read-string "Dep: ")))
  (prf/cider/send-to-repl (prf/clj/pomegranate-dep dep) t ns))

(provide 'ca-clojure)
