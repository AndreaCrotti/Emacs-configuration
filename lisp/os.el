(defun is-mac? ()
  (eq system-type 'darwin))
  ;; key bindings

;; key bindings
(when (is-mac?)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

(provide 'os)
