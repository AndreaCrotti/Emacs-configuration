(use-package graphql)
(use-package graphql-doc)
(use-package graphql-mode)
(use-package ob-graphql)

(use-package json-mode)
(use-package nodejs-repl)
(use-package biome)
(use-package biomejs-format)
(use-package js-format)
(use-package prettier-js)

(use-package company-web)
(use-package typescript-mode)
(use-package web-mode)
(use-package emmet-mode
  :hook (html-mode . emmet-mode))

(use-package lsp-tailwindcss)

(provide 'ca-web)
