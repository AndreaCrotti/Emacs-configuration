;; (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;; (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
;; (setq tuareg-library-path "/usr/lib/ocaml")

(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . typerex-mode))
(autoload 'typerex-mode "typerex" "Major mode for editing Caml code" t)

;; TypeRex mode configuration
(setq ocp-server-command "/usr/bin/ocp-wizard")
(setq typerex-in-indent 0)
(setq-default indent-tabs-mode nil)

(setq typerex-library-path "/usr/lib/ocaml")
(setq ocp-theme "tuareg_like")
(setq ocp-auto-complete t)

;not necessary if all the dicts are already there
;; (add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete-mode/ac-dict")
(ac-config-default)

(provide 'ca-ocaml)
