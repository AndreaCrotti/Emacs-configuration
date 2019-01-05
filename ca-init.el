;; custom file loaded at the very beginning
(defun make-conf-path (path)
  "Shortcut to create the path of the configuration"
  (expand-file-name (concat base path)))

;; what if this is set differently?
(setq custom-file (make-conf-path "custom.el"))

(when (file-exists-p custom-file)
  (message "loading custom file")
  (load-file custom-file))

(require 'cl)
(require 'package)


(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

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
                 ag
                 alchemist
                 android-mode
                 ansible
                 ansible-doc
                 ansible-vault
                 apache-mode
                 arduino-mode
                 auctex
                 autopair
                 auto-highlight-symbol
                 beacon
                 bitlbee
                 browse-kill-ring
                 c-eldoc
                 cask
                 cask-mode
                 cider
                 cider-decompile
                 cider-eval-sexp-fu
                 cider-spy
                 cljr-helm
                 clj-refactor
                 cljsbuild-mode
                 clojure-cheatsheet
                 clojure-mode
                 clojure-mode-extra-font-locking
                 cmake-mode
                 color-moccur
                 color-theme
                 color-theme-solarized
                 company
                 company-ansible
                 company-cabal
                 company-dict
                 company-erlang
                 company-ghc
                 company-ghci
                 company-go
                 company-inf-ruby
                 company-jedi
                 company-lua
                 company-restclient
                 company-shell
                 company-web
                 csharp-mode
                 csv-mode
                 cython-mode
                 d-mode
                 ;;dired+
                 ;; dired-details
                 diff-hl
                 docker
                 dockerfile-mode
                 dracula-theme
                 dpaste
                 ebib
                 edit-server
                 ein
                 elein
                 elixir-mode
                 elixir-yasnippets
                 elm-mode
                 elm-yasnippets
                 emamux
                 emmet-mode
                 emms
                 ensime
                 erlang
                 ess
                 eval-in-repl
                 evil
                 expand-region
                 exec-path-from-shell
                 feature-mode
                 find-file-in-repository
                 flycheck
                 flycheck-cask
                 flycheck-clojure
                 flycheck-cython
                 flycheck-elixir
                 flycheck-elm
                 flycheck-ghcmod
                 flycheck-haskell
                 flycheck-joker
                 flycheck-mix
                 flycheck-ocaml
                 flycheck-pony
                 flycheck-pos-tip
                 flycheck-perl6
                 flycheck-purescript
                 flycheck-rust
                 flycheck-mypy
                 ;; flycheck-scala-sbt
                 flycheck-stack
                 flycheck-swift
                 gist
                 gitconfig
                 git-annex
                 git-commit
                 gitlab
                 go-mode
                 golint
                 google-contacts
                 google-translate
                 graphviz-dot-mode
                 groovy-mode
                 hackernews
                 haskell-mode
                 helm
                 helm-ag
                 helm-aws
                 helm-cider
                 helm-chrome
                 helm-clojuredocs
                 helm-company
                 helm-dired-history
                 helm-dired-recent-dirs
                 helm-flycheck
                 helm-flyspell
                 helm-ghc
                 helm-git
                 helm-git-files
                 helm-gitlab
                 helm-google
                 helm-hoogle
                 helm-make
                 helm-spotify
                 helm-projectile
                 helm-swoop
                 heroku
                 hindent
                 hi2
                 highlight-blocks
                 highlight-indent-guides
                 htmlize
                 impatient-mode
                 indent-guide
                 inf-clojure
                 intero
                 jedi
                 jinja2-mode
                 js-comint
                 json-mode
                 js2-mode
                 kubernetes
                 know-your-http-well
                 kanban
                 kibit-helper
                 ledger-mode
                 less-css-mode
                 log4j-mode
                 lua-mode
                 magit
                 magithub
                 magit-annex
                 magit-gitflow
                 ;; magithub
                 markdown-mode
                 memory-usage
                 minimap
                 multiple-cursors
                 nginx-mode
                 nix-mode
                 noctilux-theme
                 notmuch
                 ob-elixir
                 ob-diagrams
                 ob-go
                 ob-http
                 ob-ipython
                 ob-lfe
                 ob-prolog
                 ob-rust
                 ob-sql-mode
                 ob-typescript
                 offlineimap
                 org-bullets
                 org-gcal
                 outline-magic
                 ox-reveal
                 p4
                 paradox
                 persistent-scratch
                 php-mode
                 pomodoro
                 pony-mode
                 powerline
                 prolog
                 protobuf-mode
                 ;;psvn
                 puppet-mode
                 purescript-mode
                 pytest
                 psc-ide
                 racket-mode
                 rainbow-delimiters
                 rainbow-mode
                 rebox2
                 restclient
                 rinari
                 ruby-compilation
                 ruby-electric
                 ruby-mode
                 sly
                 sly-company
                 skewer-mode
                 smartparens
                 smart-mode-line
                 smart-mode-line-powerline-theme
                 sos
                 slime
                 solarized-theme
                 sx
                 terraform-mode
                 textile-mode
                 tern
                 tern-auto-complete
                 tuareg
                 undo-tree
                 use-package
                 virtualenv
                 virtualenvwrapper
                 web-mode
                 yaml-mode
                 yasnippet
                 yasnippet-snippets
                 zenburn-theme
                 yafolding
                 which-key))

(when (online?)
  (mapc 'install-if-needed ca-to-install))

(global-flycheck-mode t)
;; if desired we can change this
;; (eval-after-load 'flycheck
;;   '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
;; (setq flycheck-display-errors-function #'flycheck-display-error-messages)

(column-number-mode t)
(display-time-mode t)
(global-company-mode t)
(global-linum-mode t)
(global-prettify-symbols-mode t)
(indent-guide-global-mode t)
(show-paren-mode t)
(smartparens-global-mode t)
(transient-mark-mode t)
(which-function-mode t)
(which-key-mode t)

(add-to-list 'load-path (make-conf-path "modules"))

; second argument as 0 to compile if they don't exist
(require 'ca-customs)
(require 'ca-utils)

;; all the subdirectories are added to the path, including modules
(ca-gen-path-dirs base)

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

;; always truncate lines (useful for netbook), not working yet in ORG MODE
(setq truncate-lines nil)
;; Setting indent-tabs-mode for only spaces
(setq-default indent-tabs-mode nil)

(require 'tramp)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

;; make it possible to disable it
(windmove-default-keybindings 'shift)

(setq calendar-date-style 'european)

(require 'ca-fonts)
;;(require 'ca-yas) ;; takes more than 2 seconds to load due to the huge list of files
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
(require 'ca-buffers)
(require 'ca-desktop)
(require 'ca-faces)
(require 'ca-haskell)
(require 'ca-scala)
(require 'ca-ruby)
(require 'ca-latex)
(require 'ca-ocaml)
(require 'ca-c)
(require 'ca-clojure)
(require 'ca-web)
(require 'ca-server)
;; (require 'ca-lisp)
(require 'ca-javascript)
(require 'ca-helm)
;;(require 'ca-present)
(require 'ca-smartparens-lisp)
(require 'ca-devops)
(require 'ca-purescript)
(require 'ca-elm)
(require 'ca-restclient)
(require 'ca-elixir)
(require 'ca-go)
(require 'ca-helm-swoop)

(require 'helm-projectile)
(require 'ca-mac)
(require 'ca-highlight)

(beacon-mode t)
(yas-global-mode t)

(projectile-global-mode t)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

;; (desktop-save-mode t)

(global-undo-tree-mode t)
(load-theme 'solarized-dark)

(provide 'ca-init)
