(provide 'ca-projectile)

(require 'projectile)

(defun projectile-use-magit-if-possible ()
  "If the project being switched to is a git repository, invoke
magit-status on the project root directory. Use dired otherwise."
  (interactive)
  (if (and (executable-find "git")
           (eq (projectile-project-vcs) 'git))
      (magit-status (projectile-project-root))
    (dired (projectile-project-root))))

;; (setq projectile-switch-project-action 'projectile-use-magit-if-possible)
(setq projectile-switch-project-action 'projectile-find-file-dwim)
