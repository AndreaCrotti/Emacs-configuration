(require 'ca-vc)

(add-hook 'rst-mode-hook
          (lambda ()
            ;; set the face attributes
            (set-face-attribute 'rst-level-1-face nil
                                :background "grey18")

            (set-face-attribute 'rst-level-2-face nil
                                :background "grey20")

            (set-face-attribute 'rst-level-3-face nil
                                :background "grey22")

            (set-face-attribute 'rst-level-4-face nil
                                :background "grey24")

            (local-set-key (kbd "M-n") 'rst-forward-section)
            (local-set-key (kbd "M-p") 'rst-backward-section)))

(set-face-attribute 'diff-changed nil
                    :background "dark red")

(set-face-attribute 'magit-item-highlight nil
                    :weight 'bold :height 1.2)

(provide 'ca-faces)
