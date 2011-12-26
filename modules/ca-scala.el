(require 'scala-mode-auto)
(require 'scala-mode-feature-electric)
(scala-electric-mode t)

;TODO: use ensime instead
(add-to-list 'load-path
             (make-conf-path "ensime/src/main/elisp"))

(provide 'ca-scala)
