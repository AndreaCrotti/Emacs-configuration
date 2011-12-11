;TODO: order by key type
(require 'ca-environment)

;; compile facilities
(global-set-key [f5] 'recompile)

;; newline like textmate
(global-set-key (kbd "M-RET") 'ca-newline-force)
(global-set-key [M-S-return] 'ca-newline-force-close)

;; cvs stuff
(global-set-key "\C-xg" 'magit-status)

;TODO: move to org-mode settings instead
;; some nice global keys
(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-c\C-x\C-o" 'org-clock-out)
(global-set-key "\C-c\C-x\C-i" 'org-clock-in)

;; session setting, jump to the last changed thing in the buffer
;; session.el package has to be loaded
(global-set-key "\C-x\C-j" 'session-jump-to-last-change)

;; overriding defualt not so smart visualization
(global-set-key "\C-x\C-b" 'ibuffer)

;; visualization
(global-set-key [f11] 'ca-full)

;FIXME: these two are not so useful
(global-set-key [f2] 'split-window-horizontally)
(global-set-key [f1] 'delete-window)

(global-set-key (kbd "<C-f9>") 'ca-cycle-font)

(global-set-key (kbd "M-<left>") 'other-frame)
(global-set-key (kbd "M-<right>") 'other-frame)


(global-set-key [(meta shift l)] 'ca-select-line)

(global-set-key (kbd "M-z") 'undo)
;; from here
(global-set-key "\C-x\C-p" 'find-file-at-point)

;; make shift-TAB works correctly also on osx
(when ca-mac
  (define-key (keymap-parent local-function-key-map) [S-tab] nil)
  (global-set-key [S-tab] #'tab-to-tab-stop))

(global-set-key (kbd "C-M-n") 'fixme-next)
(global-set-key (kbd "C-M-p") 'fixme-prev)

(global-set-key [f6] 'anything)

(provide 'ca-keys)
