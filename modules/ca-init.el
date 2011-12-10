
(let
    ((tools (concat base "programming-tools")))
  (add-to-list 'exec-path tools)
  (setenv "PATH" (concat (getenv "PATH") ":" tools)))

(setq initial-major-mode 'emacs-lisp-mode)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(display-time-mode 1)
(transient-mark-mode 1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(show-paren-mode t)
(column-number-mode t)
;; always truncate lines (useful for netbook), not working yet in ORG MODE
(setq truncate-lines nil)
;; Setting indent-tabs-mode for only spaces
(setq-default indent-tabs-mode nil)

(require 'tramp)
;; this variable is needed there for some reasons
(setq warning-suppress-types nil)
(require 'tramp-adb)

(require 'ido)
(ido-mode t)
;; otherwise it will try to connect to old servers all the time
(setq ido-enable-tramp-completion t)

                                        ;TODO: those could be hard to grasp for a beginner, should make it customizable
(setq ido-enable-flex-matching t)
;; regexp matching also
(setq ido-enable-regexp nil)
(setq ido-use-url-at-point t)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
(ido-everywhere t)
(setq ido-default-buffer-method 'selected-window)

;;  (add-to-list ido-ignore-buffers "\\` ")

;; Using ido-mode hacks for advising more functions
;; (require 'ido-hacks)
;; (ido-hacks-mode t)

(defcustom ca-windmove-key
  'shift
  "key for moving between windows"
  :group 'ca
  :type 'symbol)

(windmove-default-keybindings ca-windmove-key)

(setq warning-suppress-types nil)

(setq calendar-date-style 'european)

;; compile facilities
(global-set-key [f5] 'recompile)

;; newline like textmate
(global-set-key (kbd "M-RET") 'ca-newline-force)
(global-set-key [M-S-return] 'ca-newline-force-close)

;; cvs stuff
(global-set-key "\C-xg" 'magit-status)

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

(defun ca-lock-screen ()
  (interactive)
  (shell-command "xlock"))

(when ca-linux
  (global-set-key (kbd "<kp-add>") 'ca-vol-up)
  (global-set-key (kbd "<kp-subtract>") 'ca-vol-down)
  (global-set-key [f12] 'ca-lock-screen))

(global-set-key (kbd "C-M-n") 'fixme-next)
(global-set-key (kbd "C-M-p") 'fixme-prev)

(global-set-key [f6] 'anything)
