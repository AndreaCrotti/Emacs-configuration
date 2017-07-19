(require 'ca-customs)
;TODO: add support for more extensions, and add automatically magit-svn if possible

(setq
 vc-handled-backends '(Git Hg CVS SVN Bzr SCCS RCS Mtn Arch)
 ;; always opening the real file instead!
 vc-follow-symlinks t)

(autoload 'svn-status "psvn" "svn status" t)

;TODO: remove the requires if possible, making the auto-loading work
(require 'magit)
(require 'magithub)
(magithub-feature-autoinject t)

(autoload 'monky-status "monky" "mercurial mode" t)

(setq magit-push-always-verify nil)

(defun ca-detect-git-svn ()
  "Detects if the project is actually git-svn or not"
  (interactive)
  (with-temp-buffer
    (insert-file-contents ".git/config")
    (goto-line 0)
    ;; (catch)
    (condition-case err
        (re-search-forward "svn-remote")
      (search-failed -1))))

;; magit settings
(setq
 magit-auto-revert-mode nil
 magit-log-edit-confirm-cancellation t
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

;;TODO: this might be removed as well?
(defun ca-is-version-control-file ()
  "Return nil unless the file is in the git files"
  (if (vc-backend (buffer-file-name))
      (auto-revert-mode t)))

(add-hook 'find-file-hook 'ca-is-version-control-file)

;TODO: use vc-responsible-backend to find out what is the associated backend
(defun ca-provide-vc-backend ()
  "Return the right status function to call"
  (interactive)
  (when (ca-is-version-control-file)
    (let*
        ((backend (vc-backend (buffer-file-name)))
         (found (assoc backend ca-backend-assoc)))
      (if found
          (cdr found)
        'vc-dir))))

(provide 'ca-vc)
