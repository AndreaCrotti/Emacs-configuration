(require 'color-theme)
;TODO: this eval-after-load is quite useless since we're requiring it just before
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     ;TODO: call the favourite theme instead
     (color-theme-charcoal-black)))

(provide 'ca-themes)
