;; (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;; (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
;; (setq tuareg-library-path "/usr/lib/ocaml")

(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . typerex-mode))
(autoload 'typerex-mode "typerex" "Major mode for editing Caml code" t)

;; TypeRex mode configuration
(setq
 ocp-auto-complete t
 ocp-theme "tuareg_like"
 typerex-library-path "/usr/lib/ocaml"
 ocp-server-command "/usr/bin/ocp-wizard"
 typerex-in-indent 0)


(setq-default indent-tabs-mode nil)

;not necessary if all the dicts are already there
;; (add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete-mode/ac-dict")
(ac-config-default)

;; (when ca-linux
;;   (add-to-list 'load-path "/usr/share/emacs/site-lisp")
;;   (require 'typerex))

;TODO: support cedet or some other semantic analysis if possible
(defun ca-find-ocaml-declaration (&optional value)
  "Show the ocaml declarations for the given function"
  (interactive)
  (let*
      ((to_lookup
        (or value (read-from-minibuffer "symbol:" (thing-at-point 'symbol))))
       (grep_cmd
        (format "grep -nH -e \"val %s\" %s/*.mli" to_lookup typerex-library-path)))

    (grep grep_cmd)))

(provide 'ca-ocaml)
