(require 'magit)
(require 'ca-environment)

(require 'find-file-in-repository)

(global-set-key [f1] 'delete-window)
(global-set-key [f2] 'split-window-horizontally)

;; compile facilities
(global-set-key [f5] 'recompile)
(global-set-key [f6] 'anything)
(global-set-key [f7] 'find-file-in-repository)
(global-set-key (kbd "<C-f7>") 'magit-checkout)

(global-set-key [f8] 'ca-new-shell)

; this could be made generic, with some nice linting for every language
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key [f9] 'ca-pyl)))

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

(global-set-key (kbd "<C-f9>") 'ca-cycle-font)


;; make shift-TAB works correctly also on osx
(when ca-mac
  (define-key (keymap-parent local-function-key-map) [S-tab] nil)
  (global-set-key [S-tab] #'tab-to-tab-stop))

(global-set-key (kbd "C-M-n") 'fixme-next)
(global-set-key (kbd "C-M-p") 'fixme-prev)

(provide 'ca-keys)
