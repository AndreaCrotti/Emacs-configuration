;; -*- lexical-binding: t -*-
(desktop-save-mode t)
(setq desktop-dirname "~/.emacs.d/")

(setq base (expand-file-name "~/Emacs-Configuration/lisp/"))
(add-to-list 'load-path base)
(setq package-enable-at-startup nil)

(defvar bootstrap-version)

(setq use-package-always-ensure nil)
(setq gc-cons-threshold (* 50 1000 1000))

(setq native-comp-jit-compilation t
      native-comp-async-query-on-exit t
      native-comp-async-jobs-number 4
      native-comp-async-report-warnings-errors nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(eval-when-compile (require 'cl))

(defun make-relative-path
    (filename)
  "Create a relative path"
  (concat base filename))

(require 'package)

(setq custom-safe-themes t)
(require 'functions)
(require 'misc)

(require 'use-package)
(use-package use-package-hydra)
(use-package major-mode-hydra)
(use-package pretty-hydra)
(use-package async)

(defhydra ca-lsp-refactor-menu (:color blue :hint nil)
  "
  _rn_: Rename
  "
  ("rn" lsp-rename))

(use-package ack)
(use-package adoc-mode
  :init
  (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
  (add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
  :config
  (setq-local outline-regexp "[=]+"))

(use-package ag)
;; (use-package auto-highlight-symbol)
(use-package autorevert
  :custom
  (auto-revert-interval 2)
  :config
  (global-auto-revert-mode t))

(use-package log4j-mode
  :hook
  (log4j-mode . auto-revert-tail-mode))

(use-package beacon
  :config
  (beacon-mode t)
  :custom
  (beacon-blink-duration 0.5))

(use-package bf-mode)

(use-package browse-at-remote)
(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package command-log-mode)

(use-package ess)

(use-package csv-mode)
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)
  )

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :config
  (repeat-mode t))

(use-package hl-todo
  :config (global-hl-todo-mode t))

(use-package diminish)
(use-package dracula-theme)
(use-package eldoc
  :diminish eldoc-mode
  :config (global-eldoc-mode t))

(use-package ediff
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function (quote split-window-vertically))
  (ediff-window-setup-function (quote ediff-setup-windows-plain)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package find-file-in-repository)
(use-package flycheck
  :config (global-flycheck-mode t))

(use-package flycheck-pos-tip)

(use-package graphviz-dot-mode)

(use-package guru-mode
  :custom
  (guru-warn-only t)
  :config
  (guru-global-mode t))

(use-package idle-highlight-mode
  :diminish idle-highlight-mode
  :hook
  (prog-mode . idle-highlight-mode))

(use-package know-your-http-well)
(use-package imenu)

(use-package imenu-anywhere
  :bind (("<f5>" . imenu-anywhere)))

(use-package less-css-mode)
(use-package log4j-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (c-mode . lsp-deferred)
         (c-sharp-mode . lsp-deferred)
         (c-ts-mode . lsp-deferred)
         (cc-mode . lsp-deferred)
         (clojure-mode . lsp-deferred)
         (clojure-ts-mode . lsp-deferred)
         (clojurec-mode . lsp-deferred)
         (clojurescript-mode . lsp-deferred)
         (dockerfile-mode . lsp-deferred)
         (dockerfile-ts-mode . lsp-deferred)
         (elixir-mode . lsp-deferred)
         (elm-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (graphql-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (html-ts-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (java-ts-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (json-ts-mode . lsp-deferred)
         (kotlin-mode . lsp-deferred)
         (lua-mode . lsp-deferred)
         (markdown-mode . lsp-deferred)
         (ocaml-mode  . lsp-deferred)
         (protobuf-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (scala-mode . lsp-deferred)
         (sh-mode . lsp-deferred)
         (sql-mode. lsp-deferred)
         (tex-mode . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (tuareg-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (xml-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (yaml-ts-mode . lsp-deferred)
         (zig-mode . lsp-deferred)
         (zig-ts-mode . lsp-deferred))

  :bind (("M-?" . lsp-find-definition)
         ;; ("M-/" . lsp-find-references)
         ("M-'" . lsp-treemacs-call-hierarchy)
         ("C-c l" . ca-lsp-refactor-menu/body))

  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (add-to-list 'lsp-language-id-configuration '(forge-post-mode . "text"))

  :custom
  (gc-cons-threshold 100000000)
  (lsp-completion-enable nil)
  ;; change it to :none if using corfu
  (lsp-completion-provider :capf)
  (lsp-completion-show-detail t)
  (lsp-dired-mode t)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding t)
  (lsp-enable-indentation nil)
  (lsp-file-watch-threshold 5000)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-idle-delay 0.5)
  (lsp-keymap-prefix nil)
  (lsp-lens-enable t)
  (lsp-log-io nil)
  (lsp-signature t)
  (lsp-treemacs-sync-mode t)
  (read-process-output-max (* 1024 1024)))

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-mode nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory nil)
  (lsp-ui-doc-side 'right)
  (lsp-ui-imenu-mode nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-doc-show-with-mouse t))

(use-package rfc-mode)
(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.rest" . restclient-mode))
  :hook
  ((restclient-mode . outline-minor-mode)
   (restclient-mode . (lambda ()
                        (outline-minor-mode t)
                        (local-set-key (kbd "<tab>") 'outline-toggle-children)
                        (setq-local outline-regexp "#+")))))

(use-package jq-mode)

(use-package jq-format)

(use-package restclient-jq)

(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package ox-asciidoc)

(defun ca-org-mode-setup ()
  "few org mode settings"
  (org-indent-mode t)
  ;; this messes up with the tables for example
  ;; (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . ca-org-mode-setup)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (calc . t)
     (rust . t)
     (clojure . t)
     (sql . t)))
  (add-to-list 'org-structure-template-alist '("N" . "notes"))
  :custom
  (org-babel-clojure-backend 'cider)
  (org-cycle-separator-lines 2)
  (org-edit-src-content-indentation 2)
  (org-ellipsis " ▾")
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-block-startup nil)
  (org-hide-emphasis-markers t)
  (org-hide-emphasis-markers t)
  (org-src-preserve-indentation nil)
  (org-src-tab-acts-natively t)
  (org-src-tab-acts-natively t)
  (org-startup-folded 'content)
  (org-agenda-files (list (file-truename "~/RoamNotes/")
                          (file-truename "~/RoamNotes/daily/"))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (file-truename "~/RoamNotes"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n d" . org-roam-dailies-goto-today)
         ("C-c n g" . org-roam-graph)
         ("C-c n c" . org-roam.capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol))

(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package org-msg)

(use-package paradox)
(use-package persistent-scratch
  :config
  (persistent-scratch-autosave-mode t))

(use-package posframe)

;; eval `M-x nerd-icons-install-fonts' if you are seeing weird unicode glyphs
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

(use-package smart-mode-line)

(use-package smartparens
  :config
  (show-smartparens-global-mode t)
  (require 'smartparens-config)
  :bind
  (("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-a" . sp-backward-down-sexp)
   ("C-M-e" . sp-up-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-t" . sp-transpose-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>" . sp-forward-barf-sexp)
   ("C-]" . sp-select-next-thing-exchange)
   ("C-<left_bracket>" . sp-select-previous-thing)
   ("C-M-]" . sp-select-next-thing)
   ("M-F" . sp-forward-symbol)
   ("M-B" . sp-backward-symbol)))

(use-package vertico
  :config
  (vertico-mode t)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-scroll-margin 10)
  (vertico-count 20))

(use-package vertico-prescient
  :config
  (vertico-prescient-mode t))

(use-package time
  :custom
  (display-time-24hr-format t)
  (display-time-default-load-average nil)
  (display-time-mode))

(use-package toml-mode)
(use-package undo-tree
  :diminish "U"
  :config (global-undo-tree-mode t))

(use-package which-key)
(use-package wordnut)
(use-package yaml-mode)
(use-package yaml-ts-mode)

(use-package yasnippet
  :custom
  (yas-verbosity 2)
  (yas-wrap-around-region t)

  :config
  (yas-global-mode t))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package time
  :config (display-time-mode t))

(use-package paren
  :config (show-paren-mode t))

(use-package which-key
  :config (which-key-mode t))

(use-package windmove
  :config (windmove-default-keybindings 'shift))

;; TODO: reconfigure these two??
(global-prettify-symbols-mode t)
(transient-mark-mode t)

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(size-indication-mode t)

(use-package ibuffer-vc
  :defer t
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(use-package winner
  :config (winner-mode t))

(use-package ripgrep
  :config
  (setq ripgrep-arguments '("--max-columns 150" "--max-columns-preview")))

;; Enable the www ligature in every possible major mode
(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode t))

(use-package sudo-edit
  :bind
  (("C-x C-r" . sudo-edit)))

(use-package hideshow
  :hook ((prog-mode . hs-minor-mode))
  :bind
  (("C-<tab>" . hs-toggle-hiding)))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode t))

(global-set-key (kbd "M-p") 'ca-prev-defun)
(global-set-key (kbd "M-n") 'ca-next-defun)

(use-package hydra)

(defhydra hydra-zoom (global-map "C-<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(use-package recentf
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 100))

(recentf-mode t)
(setq recentf-auto-cleanup 'never)

(use-package digit-groups
  :config
  (digit-groups-global-mode t))

(use-package writeroom-mode)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;; (add-hook 'before-save-hook #'lsp-organize-imports t t)
  )

(use-package highlight-indent-guides
  :custom (highlight-indent-guides-method 'character)
  :hook ((prog-mode . highlight-indent-guides-mode)))

(use-package crux)
(use-package minimap)
(use-package bm)
(use-package focus)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                cider-repl-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package carbon-now-sh)

(pixel-scroll-precision-mode t)

(smartparens-global-strict-mode t)
;; hack to work around https://github.com/Fuco1/smartparens/issues/1204 for now
(defalias 'sp--syntax-class-to-char #'syntax-class-to-char)

(use-package embark)
(use-package cape)

(setq tab-always-indent 'complete)
(column-number-mode t)

(use-package epkg)
(use-package separedit)
(use-package verb)
(use-package doom-themes)

(use-package protobuf-mode)

(use-package company
  :config (global-company-mode t)
  :custom
  (company-show-quick-access t)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2)
  (company-show-numbers t))

(use-package company-dict)
(use-package company-restclient)
(use-package company-math)
(use-package company-fuzzy)
(use-package company-quickhelp
  :config
  (company-quickhelp-mode t))

(use-package swiper
  :bind
  ("C-r" . swiper-isearch-backward)
  ("C-s" . swiper-isearch))

(use-package fancy-compilation
  :commands (fancy-compilation-mode))

(use-package so-long
  :config
  (global-so-long-mode t))

(with-eval-after-load 'compile
  (fancy-compilation-mode t))

(use-package multi-compile)

(use-package visual-regexp)

(dolist (f '("aliases.el" "hacks.el" "custom.el"))
  (when (file-exists-p (make-relative-path f))
    ;; use require here also if possible
    (message "loading extra file" f)
    (load-file (make-relative-path f))))

(provide 'ca-init)
;;; ca-init ends here
