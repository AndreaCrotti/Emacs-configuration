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

(provide 'ca-faces)
