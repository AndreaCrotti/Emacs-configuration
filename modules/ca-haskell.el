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
             (require 'haskell-complete)
             (require 'inf-haskell)
             (turn-on-haskell-doc-mode)
             (turn-on-haskell-indentation)
             ;; This would be very nice but it conflicts with yasnippet
             ;; (define-key haskell-mode-map [tab] 'haskell-indent-cycle)
             (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
             (define-key haskell-mode-map "\C-cl" 'hs-lint)
             ;FIXME: the yas triggering is not working in haskell mode
             (make-variable-buffer-local 'yas/trigger-key)
             (setq yas/trigger-key [tab])
             (define-key yas/keymap [tab] 'yas/next-field)
             (add-to-list 'ac-sources 'my/ac-source-haskell)))

(provide 'ca-haskell)
