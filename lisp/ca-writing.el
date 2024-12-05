(use-package mixed-pitch)
(use-package olivetti)
(use-package markdown-mode
  ;; :hook
  ;; (markdown-mode . mixed-pitch-mode)
  ;; (markdown-mode . olivetti-mode)
  :init
  ;; (add-hook 'markdown-mode-hook (lambda () (buffer-face-mode t)))
  (add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
  :bind
  ("M-<left>" . markdown-promote)
  ("M-<right>" . markdown-demote))

(use-package writegood-mode
  :hook ((markdown-mode nroff-mode org-mode
                        mail-mode
                        git-commit-mode)
         . writegood-mode))

(provide 'ca-writing)
