(use-package forge)
(use-package magit
  :after forge
  :bind (("\C-xg" . magit-status))
  ;; should we load forge automatically if possible?
  :config (require 'forge)
  :ensure-system-package git)


(use-package magit-todos
  :after magit)

(use-package magit-delta
  ;; :hook (magit-mode . magit-delta-mode)
  )

(use-package magit-lfs
  :after magit)

(use-package magit-stats
  :after magit)

(use-package magit-annex
  :after magit)

(use-package magit-find-file
  :after magit)

(provide 'ca-git)
