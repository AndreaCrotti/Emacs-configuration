
;; Using uniquify for better handling of buffers with same name
(require 'uniquify)
;; Using part of the directory in this case
(setq uniquify-buffer-name-style 'forward)

;; Set some automatic filters
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Chat" (or
                  (mode . garak-mode)
                  (name . "^\\*Garak\\*$")
                  (mode . rcirc-mode)))
         ("Organization" (or
                          (mode . diary-mode)
                          (mode . org-mode)
                          (mode . org-agenda-mode)))
         ("Gnus & News" (or
                         (mode . message-mode)
                         (mode . bbdb-mode)
                         (mode . mail-mode)
                         (mode . gnus-group-mode)
                         (mode . gnus-summary-mode)
                         (mode . gnus-article-mode)
                         (name . "^\\(\\.bbdb\\|dot-bbdb\\)$")
                         (name . "^\\.newsrc-dribble$")
                         (mode . newsticker-mode)))
         ("Files" (filename . ".*"))
         ("File Management" (or
                             (mode . dired-mode)
                             (mode . shell-mode)))
         ("Documentation" (or
                           (mode . Info-mode)
                           (mode . apropos-mode)
                           (mode . woman-mode)
                           (mode . help-mode)
                           (mode . Man-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            ;; make jumping between filters easier
            (define-key ibuffer-mode-map "\C-n" 'ibuffer-forward-filter-group)
            (define-key ibuffer-mode-map "\C-p" 'ibuffer-backward-filter-group)))

(provide 'ca-buffers)
