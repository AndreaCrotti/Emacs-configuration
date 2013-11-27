(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-to-list 'auto-mode-alist '("build.sbt" . scala-mode))

(provide 'ca-scala)
