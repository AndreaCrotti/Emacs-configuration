;; -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let* ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(require 'use-package)

(defvar ca-straight-versions-file
  (expand-file-name "straight/versions/default.el" user-emacs-directory)
  "Lockfile used by straight.el to pin package versions.")

(setq straight-frozen-versions-file ca-straight-versions-file)

(defun ca-straight-freeze-versions ()
  "Write current package versions to the straight lockfile."
  (interactive)
  (straight-freeze-versions))

(defun ca-straight-thaw-versions ()
  "Unpin packages by clearing the straight lockfile."
  (interactive)
  (straight-thaw-versions))

(provide 'ca-straight)
;;; ca-straight.el ends here
