(use-package dart-mode)

(use-package lsp-dart
  :hook (dart-mode . lsp))

(use-package cmake-ide)

(use-package flutter)

(use-package hover
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-z" . #'hover-run-or-hot-reload)
              ("C-M-x" . #'hover-run-or-hot-restart)))


(provide 'ca-mobile)
