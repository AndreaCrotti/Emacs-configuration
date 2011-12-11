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

; next step is to remove conf completely
(defun ca-reload-dirs ()
  (interactive)
  (ca-gen-path-dirs base))

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
;; (require 'tramp-adb)

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


(require 'epa)
(epa-file-enable)

(setq rfc-url-save-directory "~/rfc")
(setq rfc-index-url "http://www.ietf.org/iesg/1rfc_index.txt")
(setq rfc-archive-alist (list (concat rfc-url-save-directory "/rfc.zip")
                              rfc-url-save-directory
                              "http://www.ietf.org/rfc/"))
(setq rfc-insert-content-url-hook '(rfc-url-save))

(require 'irfc)
(setq irfc-directory "~/rfcs")
(add-to-list 'auto-mode-alist
             '("/rfc[0-9]+\\.txt\\'" . irfc-mode))

;; now try to load everything in modules?
(add-to-list 'load-path (make-conf-path "modules"))

(provide 'ca-init)
(require 'ca-functions)
(require 'ca-yas)
;; is the order important anyhow?
(require 'ca-python)
