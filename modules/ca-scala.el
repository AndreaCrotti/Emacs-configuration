(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-to-list 'auto-mode-alist '("build.sbt" . scala-mode))

(add-hook 'scala-mode-hook
          (lambda ()
            (local-set-key (kbd "<C-f5>") 'ensime-sbt-do-run)
            (local-set-key [f5] 'ensime-sbt-do-test)))

(provide 'ca-scala)
