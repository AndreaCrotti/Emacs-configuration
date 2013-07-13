;; web framework things
(add-to-list 'load-path (make-conf-path "pony-mode/src"))
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))

(provide 'ca-web)
