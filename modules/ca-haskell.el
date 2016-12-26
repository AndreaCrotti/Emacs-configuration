(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(autoload 'haskell-mode "haskell-mode" "haskell mode" t)
(autoload 'turn-on-haskell-doc-mode "haskell-doc" "haskell doc mode" t)
(autoload 'turn-on-haskell-indent "haskell-indent" "haskell indent facilities" t)

(autoload 'inf-haskell "inf-haskell" "inf-haskell" t)
(autoload 'hs-lint "hs-lint" "haskell checker" t)
(autoload 'run-haskell "inf-haskell" "inferior haskell" t)

;; here some haskell variables
(setq
 haskell-doc-show-global-types t
 haskell-program-name "ghci"
 haskell-indent-thenelse 1)

;; If nothing found pass the control
(add-hook 'haskell-mode-hook
          '(lambda ()
             (require 'haskell-doc) ; Is this the only way?
             (require 'haskell-indent)
             (require 'inf-haskell)
             (haskell-doc-mode t)
             (haskell-indentation-mode t)

             (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
             (define-key haskell-mode-map "\C-cl" 'hs-lint)
             ;FIXME: the yas triggering is not working in haskell mode
             (make-variable-buffer-local 'yas/trigger-key)
             (setq yas/trigger-key [tab])
             (define-key yas/keymap [tab] 'yas/next-field)))

(setq ghc-ghc-options '("-fno-warn-missing-signatures")
      haskell-compile-cabal-build-command "cd %s && stack build"
      haskell-process-type 'stack-ghci
      haskell-interactive-popup-errors nil
      haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans" "--with-ghc=ghci-ng")
      haskell-process-path-ghci "stack"
)

(provide 'ca-haskell)
