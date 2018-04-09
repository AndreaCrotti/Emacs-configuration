(require 'ca-customs)

(setq
 vc-handled-backends '(Git Hg CVS SVN Bzr SCCS RCS Mtn Arch)
 ;; always opening the real file instead!
 vc-follow-symlinks t)

;TODO: remove the requires if possible, making the auto-loading work
(require 'magit)
(require 'magithub)
(magithub-feature-autoinject t)

(setq magit-push-always-verify nil)

;; magit settings
(setq
 magit-process-connection-type nil
 magit-process-popup-time 10
 magit-status-buffer-switch-function 'switch-to-buffer)

(global-git-commit-mode t)

(defun ca-log-edit-modes ()
  "Activate modes for various log-edits"
  (orgtbl-mode t)
  (orgstruct-mode t)
  (flyspell-mode t)
  (auto-fill-mode t))

(add-hook 'git-commit-mode-hook 'ca-log-edit-modes)

(provide 'ca-vc)
