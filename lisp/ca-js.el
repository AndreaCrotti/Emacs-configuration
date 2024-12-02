(use-package json-mode)
(use-package nodejs-repl)
(use-package biome)
(use-package biomejs-format)
(use-package js-format)
(use-package prettier-js)
(use-package astro-ts-mode
  :config
  (add-to-list 'auto-mode-alist (cons "\\.astro\\'" 'astro-ts-mode)))

(use-package tide)
(use-package tern)

(provide 'ca-js)
