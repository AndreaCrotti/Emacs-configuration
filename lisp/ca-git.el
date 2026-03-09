;; -*- lexical-binding: t; -*-

(use-package git-auto-commit-mode)
(use-package github-browse-file
  :defer t)
(use-package gist
  :defer t)
(use-package git-modes)
(use-package git-auto-commit-mode
  :custom
  (gac-debounce-interval 0.5))

(use-package git-timemachine)

(use-package forge)
(use-package magit
  :after forge
  :bind (("\C-xg" . magit-status))
  ;; should we load forge automatically if possible?
  :config (require 'forge))

(use-package magit-todos
  :after magit)

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package magit-lfs
  :after magit)

(use-package magit-stats
  :defer t
  :after magit)

(use-package magit-annex
  :defer t
  :after magit)

(use-package magit-find-file
  :defer t
  :after magit)

(provide 'ca-git)
