
(use-package github-browse-file)
(use-package gist)
(use-package git-modes)
(use-package git-auto-commit-mode
  :custom
  (gac-debounce-interval 0.5))

(use-package git-timemachine)
(use-package gitconfig)

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
