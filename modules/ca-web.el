;; web framework things
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
;TODO: add some binding to jump to things that are not defined

(add-hook 'sgml-mode-hook
          (lambda ()
            (local-set-key (kbd "\C-j") 'emmet-expand-line)))

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(provide 'ca-web)
