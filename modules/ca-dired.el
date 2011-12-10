
;; reverting automatically the buffer
(setq dired-auto-revert-buffer 1)
;; look for filenames if on one already
(setq dired-isearch-filenames 'dwim)
;; so it doesn't open a thousand buffers every time
(put 'dired-find-alternate-file 'disabled nil)

(setq file-extensions-separately
      '("\\.pdf$" "\\.rar$" "\\.html?$" "\\.mp3$" "\\.mp4$" "\\.flv$"))

(setq dired-guess-shell-alist-user ())

(if ca-mac
    (dolist (ext file-extensions-separately)
      (add-to-list 'dired-guess-shell-alist-user (list ext "open"))))

;TODO: add conditions for other operating systems

(add-hook 'dired-mode-hook
          (lambda ()
            ;; define some more useful keys
            (define-key dired-mode-map "b" 'browse-url-of-dired-file)))

(require 'dired-details)
(dired-details-install)
