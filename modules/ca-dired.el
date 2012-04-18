(require 'ca-environment)

;; reverting automatically the buffer
(setq
 dired-auto-revert-buffer 1
 dired-isearch-filenames 'dwim)

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
          (lambda ()
            ;; define some more useful keys
            (define-key dired-mode-map "b" 'browse-url-of-dired-file)))

(setq dired-listing-switches "-al -I .git -I .bzr -I .hg")

(require 'dired-details)
(dired-details-install)

(provide 'ca-dired)
