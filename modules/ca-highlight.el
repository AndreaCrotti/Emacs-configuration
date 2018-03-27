(require 'auto-highlight-symbol)
(require 'highlight-blocks)
(require 'highlight-changes)
(require 'highlight-indent-guides)
(require 'diff-hl)

(global-auto-highlight-symbol-mode t)

(add-hook 'prog-mode-hook 'highlight-blocks-mode)
(indent-guide-global-mode t)
(global-diff-hl-mode t)

(provide 'ca-highlight)
