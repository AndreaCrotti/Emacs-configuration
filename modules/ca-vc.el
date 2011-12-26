;TODO: add support for more extensions, and add automatically magit-svn if possible

(setq
 vc-handled-backends '(Git Hg CVS SVN Bzr)
 ;; always opening the real file instead!
 vc-follow-symlinks t)

(autoload 'svn-status "psvn" "svn status" t)

(autoload 'magit "magit" "magit" t)
(autoload 'magit-status "magit" "magit-status" t)

(eval-after-load 'magit
  '(require 'magit-svn))

(autoload 'hg-status "mercurial" "mercurial" t)

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

(setq magit-log-edit-confirm-cancellation t)
;; use tty which should be faster, passphrase not allowed here
(setq magit-process-connection-type nil)
(setq magit-process-popup-time 10)

(add-hook 'magit-log-edit-mode-hook 'orgtbl-mode)
(add-hook 'magit-log-edit-mode-hook 'orgstruct-mode)
(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
(add-hook 'magit-log-edit-mode-hook 'auto-fill-mode)

;TODO: use  (vc-ensure-vc-buffer) to make it more general

(defun ca-is-version-control-file ()
  "Return nil unless the file is in the git files"
  (if (vc-working-revision (buffer-file-name))
      (auto-revert-mode t)))

(add-hook 'find-file-hook 'ca-is-version-control-file)

(defcustom ca-backend-assoc
  '(('Git . 'magit-status)
    ('Hg . 'hg-status)
    ('Svn . 'svn-status))
  "Mapping between backend and function"
  :type 'list)

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
