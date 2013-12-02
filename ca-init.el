(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq
 ca-to-install '(
                 ac-slime
                 ac-js2
                 ack
                 adoc-mode
                 android-mode
                 apache-mode
                 ascii
                 auto-complete
                 autopair
                 auctex
                 batch-mode
                 bitlbee
                 bookmark+
                 browse-kill-ring
                 c-eldoc
                 clojure-mode
                 cider
                 cmake-mode
                 color-moccur
                 csharp-mode
                 csv-mode
                 dired-details
                 d-mode
                 dpaste
                 dired+
                 ebib
                 ein
                 emms
                 ensime
                 erlang
                 evil
                 emacs-eclim
                 edit-server
                 go-mode
                 groovy-mode
                 gtags
                 find-file-in-repository
                 flycheck
                 gist
                 google-contacts
                 ;; google-maps
                 google-translate
                 google-weather
                 graphviz-dot-mode
                 kanban
                 hackernews
                 haskell-mode
                 helm
                 heroku
                 htmlize
                 ipython
                 less-css-mode
                 log4j-mode
                 lua-mode
                 jedi
                 magit
                 markdown-mode
                 memory-usage
                 minimap
                 monky
                 multiple-cursors
                 notmuch
                 offlineimap
                 p4
                 phantomjs
                 php-mode
                 pomodoro
                 pony-mode
                 powerline
                 python-mode
                 prolog
                 puppet-mode
                 psvn
                 rebox2
                 rinari
                 ruby-compilation
                 ruby-mode
                 yasnippet
                 scala-mode
                 smartparens
                 slime
                 slime-clj
                 textile-mode
                 tuareg
                 undo-tree
                 virtualenv
                 web-mode
                 wget
                 yaml-mode
))

(mapc 'install-if-needed ca-to-install)

(add-hook 'after-init-hook #'global-flycheck-mode)
(smartparens-global-mode t)
(show-paren-mode t)
(column-number-mode t)


(defun make-conf-path (path)
  "Shortcut to create the path of the configuration"
  (expand-file-name (concat base path)))

(add-to-list 'load-path (make-conf-path "modules"))

; second argument as 0 to compile if they don't exist
(require 'ca-customs)
(require 'ca-utils)

;; what if this is set differently?
(setq custom-file (make-conf-path "custom.el"))

;; all the subdirectories are added to the path, including modules
(ca-gen-path-dirs base)

(add-to-list 'load-path (make-conf-path "tramp/lisp"))

(let
    ((tools (concat base "programming-tools")))
  (add-to-list 'exec-path tools)
  (setenv "PATH" (concat (getenv "PATH") ":" tools)))

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(display-time-mode 1)
(transient-mark-mode 1)

(setq
 initial-major-mode 'emacs-lisp-mode
 inhibit-startup-message t
 initial-scratch-message nil)

;; always truncate lines (useful for netbook), not working yet in ORG MODE
(setq truncate-lines nil)
;; Setting indent-tabs-mode for only spaces
(setq-default indent-tabs-mode nil)

(require 'tramp)

(require 'ido)
(ido-mode t)

(setq
 ido-enable-tramp-completion t
 ido-enable-flex-matching t
 ido-enable-regexp nil
 ido-use-url-at-point t
 ido-create-new-buffer 'always
 ido-default-buffer-method 'selected-window
 ido-everywhere t
 ido-use-filename-at-point 'guess)

;; make it possible to disable it
(windmove-default-keybindings 'shift)

(setq calendar-date-style 'european)

(require 'ca-fonts)
(require 'ca-themes)
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
(require 'ca-packages)
(require 'ca-buffers)
(require 'ca-desktop)
(require 'ca-faces)
(require 'ca-haskell)
(require 'ca-scala)
(require 'ca-java)
(require 'ca-ruby)
(require 'ca-latex)
(require 'ca-ocaml)
(require 'ca-c)
(require 'ca-clojure)
(require 'ca-web)


(when (file-exists-p custom-file)
  (message "loading custom file")
  (load-file custom-file))

(defun ca-network-mode ()
  (require 'ca-network)
  (desktop-save-mode nil)
  (gnus))

(provide 'ca-init)
