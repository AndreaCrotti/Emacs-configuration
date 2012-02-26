(require 'ca-customs)
;TODO: add support for more extensions, and add automatically magit-svn if possible

(setq
 vc-handled-backends '(Git Hg CVS SVN Bzr SCCS RCS Mtn Arch)
 ;; always opening the real file instead!
 vc-follow-symlinks t)

(autoload 'svn-status "psvn" "svn status" t)

(require 'magit)
(require 'magit-svn)
;; enabling globally magit-svn
(add-hook 'magit-mode-hook 'turn-on-magit-svn)

(autoload 'monky-status "monky" "mercurial mode" t)

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
 magit-log-edit-confirm-cancellation t
 magit-process-connection-type nil
 magit-process-popup-time 10
 magit-status-buffer-switch-function 'switch-to-buffer)

(defun ca-log-edit-modes ()
  "Activate modes for various log-edits"
  (orgtbl-mode t)
  (orgstruct-mode t)
  (flyspell-mode t)
  (auto-fill-mode t))

(add-hook 'magit-log-edit-mode-hook 'ca-log-edit-modes)
(add-hook 'monky-log-edit-mode-hook 'ca-log-edit-modes)

;TODO: use  (vc-ensure-vc-buffer) to make it more general

(defun ca-is-version-control-file ()
  "Return nil unless the file is in the git files"
  (if (vc-working-revision (buffer-file-name))
      (auto-revert-mode t)))

(add-hook 'find-file-hook 'ca-is-version-control-file)

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
