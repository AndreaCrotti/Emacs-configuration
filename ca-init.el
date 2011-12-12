(defun make-conf-path (path)
  (expand-file-name (concat base path)))

(defun ca-gen-path-dirs (base-dir)
  "Add to load path all the subdirectories of first level"
  (interactive)
  (message "adding all directories in the first level to the load-path")
  (dolist (dir (directory-files base-dir t))
    (if (and
         (file-directory-p dir)
         (not (file-symlink-p dir)))
        (add-to-list 'load-path dir))))

;XXX: this has to be done as soon as possible or the default cedet will be loaded!!
(when (not (boundp 'cedet-version))
  (load (make-conf-path "cedet/common/cedet.el")))

; next step is to remove conf completely
(defun ca-reload-dirs ()
  (interactive)
  (ca-gen-path-dirs base))

;; all the subdirectories are added to the path, including modules
(ca-gen-path-dirs base)

;TODO: try to move it inside miniconf.org instead
(add-to-list 'load-path (make-conf-path "gnus/lisp"))
(require 'gnus-load)
(add-to-list 'Info-default-directory-list (make-conf-path "gnus/texi/"))

(add-to-list 'load-path (make-conf-path "tramp/lisp"))

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

(require 'ido)
(ido-mode t)
;; otherwise it will try to connect to old servers all the time
(setq ido-enable-tramp-completion t)

                                        ;TODO: those could be hard to grasp for a beginner, should make it customizable
(setq
 ido-enable-flex-matching t
 ido-enable-regexp nil
 ido-use-url-at-point t
 ido-create-new-buffer 'always
 ido-default-buffer-method 'selected-window
 ido-use-filename-at-point 'guess)

(ido-everywhere t)

(defcustom ca-windmove-key
  'shift
  "key for moving between windows"
  :group 'ca
  :type 'symbol)

(windmove-default-keybindings ca-windmove-key)

(setq calendar-date-style 'european)

(require 'epa)
(epa-file-enable)

; second argument as 0 to compile if they don't exist
(byte-recompile-directory (make-conf-path "modules") 0)
(require 'ca-themes)
(require 'ca-cedet)
(require 'ca-functions)
(require 'ca-yas) ;; takes more than 2 seconds to load due to the huge list of files
;; see if it's possible to postpone loading the snippets
;; is the order important anyhow?

(require 'ca-python)
(require 'ca-auto-complete)
(require 'ca-org)
;; these things change the global state
(require 'ca-keys)
(require 'ca-aliases)

;; some other things which might be optional
;; create a dictionary structure where
(require 'ca-dired)
(require 'ca-misc)
(require 'ca-other-modes)
(require 'ca-prog-mode)
(require 'ca-vc)
(require 'ca-bookmarks)

(setq ca-custom-file (make-conf-path "custom.el"))
(when (file-exists-p ca-custom-file)
  (load-file ca-custom-file))

(provide 'ca-init)
