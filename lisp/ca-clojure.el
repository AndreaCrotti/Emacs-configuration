(use-package flycheck-clj-kondo)

(use-package elein)

(use-package inf-clojure)

(defhydra lsp-clojure-refactor-menu (:color blue :hint nil)
  "
Threading                      Code Manip                      Namespace                       Misc
------------------------------------------------------------------------------------------------------------------------------------------------------
_th_: Thread first             _el_: Expand let                _cn_: Clean ns                  _cp_: Cycle privacy
_tf_: Thread first all         _il_: Introduce let             _am_: Add missing libspec       _cc_: Cycle coll
_tt_: Thread last              _ml_: Move to let
_tl_: Thread last all          _ef_: Extract function
_ua_: Unwind all               _rn_: Rename
_uw_: Unwind thread            _mf_: Move formattedtextfield
"

  ("am" lsp-clojure-add-missing-libspec)
  ("cc" lsp-clojure-cycle-coll)
  ("cn" lsp-clojure-clean-ns)
  ("cp" lsp-clojure-cycle-privacy)
  ("ef" lsp-clojure-extract-function)
  ("el" lsp-clojure-expand-let)
  ("il" lsp-clojure-introduce-let)
  ("mf" lsp-clojure-move-form)
  ("ml" lsp-clojure-move-to-let)
  ("rn" lsp-rename)
  ("tf" lsp-clojure-thread-first-all)
  ("th" lsp-clojure-thread-first)
  ("tl" lsp-clojure-thread-last-all)
  ("tt" lsp-clojure-thread-last)
  ("ua" lsp-clojure-unwind-all)
  ("uw" lsp-clojure-unwind-thread))

(use-package kaocha-runner
  :bind (:map clojure-mode-map
              ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k r" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k w" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))

(use-package clj-refactor
  :custom
  (cljr-auto-clean-ns nil)
  (cljr-auto-sort-ns nil)
  (cljr-add-ns-to-blank-clj-files nil))

(defun portal.api/open ()
  "Open the API portal."
  (interactive)
  (cider-nrepl-sync-request:eval
   "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open) {:theme :portal.colors/solarized-dark})) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal.api/clear ()
  "Clear the state of the portal."
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  "Close the portal."
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

(use-package cider
  :pin melpa-stable
  :hook (cider-mode . clj-refactor-mode)
  :diminish subword-mode
  :bind (:map clojure-mode-map
              ("C-<f5>" . cider-test-run-test))

  ;; add this when the syntax is fixed
  ;; :hook
  ;; (lambda ()
  ;;   (add-to-list
  ;;    'completion-at-point-functions
  ;;    #'cape-cider-lsp))

  :custom
  (cider-auto-test-mode t)
  (cider-enrich-classpath t)
  (cider-font-lock-dynamically '(macro core function var))
  (cider-ns-code-reload-tool 'clj-reload)
  (cider-overlays-use-font-lock t)
  (cider-prompt-for-symbol nil)
  (cider-repl-buffer-size-limit 100000)
  (cider-repl-display-help-banner nil)
  (cider-repl-display-in-current-window nil)
  (cider-repl-display-output-before-window-boundaries nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-prompt-function 'cider-repl-prompt-abbreviated)
  (cider-repl-require-ns-on-set nil)
  (cider-repl-tab-command #'indent-for-tab-command)
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-use-pretty-printing t)
  (nrepl-hide-special-buffers t)
  (nrepl-log-messages t))

(use-package cider-decompile)
(use-package clj-decompiler)

(use-package babashka)

(use-package neil
  :custom
  (neil-inject-dep-to-project-p t))

(use-package cider-hydra
  :after cider
  :hook
  (clojure-mode . cider-hydra-mode))

(use-package kibit-helper)
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :bind
  (("C-c l" . lsp-clojure-refactor-menu/body))
  :hook
  (clojure-mode . subword-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.bb" . clojure-mode)))

;; not working for some reason with font-locking
;; (use-package clojure-ts-mode)

(use-package clojure-mode-extra-font-locking)
(use-package jet)
(use-package ob-clojurescript)
(require 'ob-clojure)

(defun clerk-show ()
  "Show clerk."
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)

(provide 'ca-clojure)
