(require 'cl)
(require 'package)

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(require 'url-handlers)

(defun online? ()
  "Detect if it's online or not,
  ;TODO:find a better way to filter out interfaces"
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (or (equal "lo" (car iface)) (equal "docker0" (car iface)))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

(when (online?)
    (package-refresh-contents))

;; make more packages available with the package installer
(setq
 ca-to-install '(
                 4clojure
                 ack
                 adoc-mode
                 android-mode
                 apache-mode
                 arduino-mode
                 ascii
                 autopair
                 auctex
                 batch-mode
                 beacon
                 bitlbee
                 bookmark+
                 browse-kill-ring
                 c-eldoc
                 company
                 company-jedi
                 ;cljdoc
                 clj-refactor
                 clojure-cheatsheet
                 clojure-mode
                 cider
                 cmake-mode
                 color-moccur
                 color-theme
                 csharp-mode
                 csv-mode
                 dired-details
                 d-mode
                 dpaste
                 dired+
                 dockerfile-mode
                 ebib
                 ein
                 elein
                 elixir-mode
                 ;; elixir-mix
                 emmet-mode
                 emms
                 erlang
                 eval-in-repl
                 evil
                 edit-server
                 ensime
                 ess
                 git-gutter
                 go-mode
                 groovy-mode
                 ;;gtags
                 feature-mode
                 find-file-in-repository
                 flycheck
                 flycheck-pos-tip
                 gist
                 google-contacts
                 ;; google-maps
                 google-translate
                 graphviz-dot-mode
                 kanban
                 hackernews
                 haskell-mode
                 helm
                 helm-projectile
                 helm-swoop
                 heroku
                 hi2
                 htmlize
                 indent-guide
                 inf-clojure
                 ;;ipython
                 less-css-mode
                 log4j-mode
                 lua-mode
                 jedi
                 js-comint
                 magit
                 malabar-mode
                 markdown-mode
                 memory-usage
                 minimap
                 monky
                 multiple-cursors
                 noctilux-theme
                 notmuch
                 offlineimap
                 org-bullets
                 org-gcal
                 org-jekyll
                 org-present
                 ox-reveal
                 p4
                 persistent-scratch
                 ;;phantomjs
                 php-mode
                 pomodoro
                 pony-mode
                 powerline
                 prolog
                 puppet-mode
                 psvn
                 ;; python-mode
                 rainbow-mode
                 rainbow-delimiters
                 rebox2
                 restclient
                 rinari
                 ruby-compilation
                 ruby-mode
                 yasnippet
                 scala-mode2
                 smartparens
                 smart-mode-line
                 sly
                 sly-company
                 sos
                 textile-mode
                 tuareg
                 undo-tree
                 virtualenv
                 virtualenvwrapper
                 web-mode
                 ;wget
                 yaml-mode
                 zenburn-theme
))

(when (online?)
    (mapc 'install-if-needed ca-to-install))

(global-flycheck-mode t)
;; if desired we can change this
;; (eval-after-load 'flycheck
;;   '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
;; (setq flycheck-display-errors-function #'flycheck-display-error-messages)

(global-company-mode t)
(smartparens-global-mode t)
(show-paren-mode t)
(column-number-mode t)
(indent-guide-global-mode t)

(global-prettify-symbols-mode t)
(global-linum-mode t)
(show-paren-mode t)
(which-function-mode t)

(defun make-conf-path (path)
  "Shortcut to create the path of the configuration"
  (expand-file-name (concat base path)))

(add-to-list 'load-path (make-conf-path "python-mode"))
(add-to-list 'load-path (make-conf-path "modules"))

; second argument as 0 to compile if they don't exist
(require 'ca-customs)
(require 'ca-utils)

;; what if this is set differently?
(setq custom-file (make-conf-path "custom.el"))

;; all the subdirectories are added to the path, including modules
(ca-gen-path-dirs base)

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

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

;TODO: probably not needed this anymore
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
(require 'ca-yas) ;; takes more than 2 seconds to load due to the huge list of files
;; see if it's possible to postpone loading the snippets
;; is the order important anyhow?

(require 'ca-python)
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
;; (require 'ca-java)
(require 'ca-ruby)
(require 'ca-latex)
(require 'ca-ocaml)
(require 'ca-c)
(require 'ca-clojure)
(require 'ca-web)
(require 'ca-server)
;; (require 'ca-lisp)
(require 'ca-javascript)
;;(require 'ca-git-gutter)
(require 'ca-helm)
(require 'ca-present)
(require 'ca-clojure)
(require 'ca-smartparens-lisp)

(require 'helm-projectile)

(beacon-mode t)
(projectile-global-mode t)
(yas-global-mode t)
;; (desktop-save-mode t)
(load-theme 'noctilux)
(setq magit-auto-revert-mode nil)

(when (file-exists-p custom-file)
  (message "loading custom file")
  (load-file custom-file))

(defun ca-network-mode ()
  (require 'ca-network)
  (desktop-save-mode nil)
  (gnus))

(when (eq system-type 'darwin) ;; mac specific settings
  (add-to-list 'exec-path "/usr/local/bin")
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  ;;(global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

(provide 'ca-init)
