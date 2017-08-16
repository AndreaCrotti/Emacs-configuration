(require 'magit)

(require 'find-file-in-repository)

(global-set-key [f1] 'delete-window)
(global-set-key [f2] 'split-window-horizontally)

;; compile facilities
(global-set-key [f5] 'helm-imenu)
(global-set-key (kbd "<C-f5>") 'recompile)
;; (global-set-key [f7] 'find-file-in-repository)
(global-set-key [f7] 'helm-projectile-find-file)
(global-set-key (kbd "<C-f7>") 'magit-checkout)

(global-set-key [f8] 'ca-new-shell)

;; textmate-like bindings
(global-set-key (kbd "M-RET") 'ca-newline-force)
(global-set-key [M-S-return] 'ca-newline-force-close)
(global-set-key [(meta shift l)] 'ca-select-line)
;; move between frames
(global-set-key (kbd "M-<left>") 'other-frame)
(global-set-key (kbd "M-<right>") 'other-frame)
(global-set-key (kbd "M-z") 'undo)

;; magit
;TODO: make it more general for all the possible vcs
(global-set-key "\C-xg" 'magit-status)

;; org settings
(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-c\C-x\C-o" 'org-clock-out)
(global-set-key "\C-c\C-x\C-i" 'org-clock-in)

(global-set-key "\C-x\C-j" 'session-jump-to-last-change)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-x\C-p" 'find-file-at-point)
(global-set-key "\C-x\C-r" 'ca-find-file-root)

; Helm settings
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "M-p") 'ca-prev-defun)
(global-set-key (kbd "M-n") 'ca-next-defun)

(provide 'ca-keys)
