(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 vc-make-backup-files t)       ; use versioned backups

(setq
 initial-major-mode 'emacs-lisp-mode
 inhibit-startup-message t
 initial-scratch-message nil)

(setq-default indent-tabs-mode nil)

(setq visible-bell nil) ; Turn beep off
(setq ring-bell-function 'ignore)
(savehist-mode t) ; save also minibuffer history, very useful

(global-set-key [f1] 'delete-window)
(global-set-key [f2] 'split-window-horizontally)


(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
