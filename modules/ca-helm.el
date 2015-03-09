(require 'helm)
(require 'helm-projectile)
(require 'helm-config)

(helm-mode t)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(helm-autoresize-mode t)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-M-x-fuzzy-match t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(provide 'ca-helm)
