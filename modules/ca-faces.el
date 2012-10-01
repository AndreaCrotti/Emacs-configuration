(require 'ca-vc)

(add-hook 'rst-mode-hook
          (lambda ()
            ;; set the face attributes
            (set-face-attribute 'rst-level-1 nil
                                :background "grey18")

            (set-face-attribute 'rst-level-2 nil
                                :background "grey20")

            (set-face-attribute 'rst-level-3 nil
                                :background "grey22")

            (set-face-attribute 'rst-level-4 nil
                                :background "grey24")

            (local-set-key (kbd "M-n") 'rst-forward-section)
            (local-set-key (kbd "M-p") 'rst-backward-section)))

(add-hook 'diff
          (lambda ()
            (set-face-attribute 'diff-changed nil
                                :background "dark red")))

(add-hook 'magit-mode-hook
          (lambda ()
            (set-face-attribute 'magit-item-highlight nil
                                :weight 'bold :height 1.2)))

(provide 'ca-faces)
