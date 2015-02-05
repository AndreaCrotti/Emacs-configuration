(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

;; (global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
;; (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; ;; Jump to next/previous hunk
;; (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
;; (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; ;; Stage current hunk
;; (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; ;; Revert current hunk
;; (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

(provide 'ca-git-gutter)
