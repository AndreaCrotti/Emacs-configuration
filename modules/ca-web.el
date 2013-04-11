;; web framework things
(add-to-list 'load-path (make-conf-path "pony-mode/src"))
;(require 'pony-mode)
(require 'coffee-mode)
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.iced$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))

(provide 'ca-web)
