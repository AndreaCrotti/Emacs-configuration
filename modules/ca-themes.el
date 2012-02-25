(require 'color-theme)

(when (not (fboundp 'load-theme))
  (eval-after-load "color-theme"
    '(progn
       (color-theme-initialize)
       (color-theme-charcoal-black))))

(provide 'ca-themes)
