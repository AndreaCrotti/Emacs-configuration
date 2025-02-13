(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path-from-shell-variables "LSP_USE_PLISTS"))

(use-package direnv
  :config
  (direnv-mode t))

(use-package envrc
  :hook
  (after-init . envrc-global-mode))

(use-package all-the-icons)

(use-package treemacs-icons-dired)

(setq dired-auto-revert-buffer 1)
(setq dired-isearch-filenames 'dwim)
(setq dired-listing-switches "-al")

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package diredfl
  :config
  (diredfl-global-mode t))

(use-package treemacs-all-the-icons)

(use-package projectile
  :diminish projectile
  :config
  (projectile-global-mode t)
  :bind (("<f6>" . projectile-ripgrep)
         ("C-<f6>" . projectile-replace)
         ("<f7>" . projectile-find-file)
         ("<f8>" . projectile-run-vterm)
         ("<f9>" . projectile-command-map)
         :map projectile-mode-map
         ("s-d" . projectile-find-dir)
         ("s-p" . projectile-switch-project)
         ("s-f" . projectile-find-file)
         ("s-a" . projectile-ag))
  :custom
  (projectile-completion-system 'default)
  (projectile-switch-project-action #'projectile-find-file))


(use-package treemacs
  :after (projectile)
  :custom
  (treemacs-resize-icons 10)
  (treemacs-filewatch-mode t)
  (treemacs-follow-mode t)
  ;; this keeps on asking the mode otherwise
  (treemacs-fringe-indicator-mode nil)
  (treemacs-git-commit-diff-mode nil)
  (treemacs-git-mode t)
  (treemacs-indent-guide-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-tag-follow-mode nil)
  (treemacs-tag-follow-delay 3)
  :hook
  (treemacs-mode . (lambda () (text-scale-adjust -2))))

(provide 'ca-project)
