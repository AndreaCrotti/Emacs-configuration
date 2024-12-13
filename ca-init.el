;; -*- lexical-binding: t -*-
(setq base (expand-file-name "~/Emacs-Configuration/lisp/"))
(add-to-list 'load-path base)
(setq package-enable-at-startup nil)

(defvar bootstrap-version)

(require 'use-package-ensure-system-package)
;; performance changes
(use-package system-packages
  :custom
  (system-packages-use-sudo t))

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

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path (make-relative-path "use-package"))
  (require 'use-package))

(add-to-list 'load-path (make-relative-path "cider-storm"))

(require 'package)

(setq custom-safe-themes t)
(require 'functions)
(require 'misc)
(require 'os)

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-unsure t)

(use-package use-package-hydra)
(use-package major-mode-hydra)
(use-package pretty-hydra)
(use-package async)

(defhydra ca-lsp-refactor-menu (:color blue :hint nil)
  "
  _rn_: Rename
  "
  ("rn" lsp-rename))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package ack)
(use-package adoc-mode
  :init
  (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
  (add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
  :config
  (setq outline-regexp "[=]+")
  )

(use-package ag)
;; (use-package auto-highlight-symbol)
(use-package autorevert
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode))

(use-package log4j-mode
  :hook
  (log4j-mode . auto-revert-tail-mode))

(use-package beacon
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
  (repeat-mode))

(use-package hl-todo
  :config (global-hl-todo-mode t))

(use-package diminish)
(use-package dracula-theme)
(use-package edit-server)
(use-package eldoc
  :diminish eldoc-mode
  :config (global-eldoc-mode))

(use-package ediff
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function (quote split-window-vertically))
  (ediff-window-setup-function (quote ediff-setup-windows-plain)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package find-file-in-repository)
(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip)

(use-package graphviz-dot-mode
  :ensure-system-package dot)

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
  :custom
  ;; (lsp-pylsp-plugins-autopep8-enabled  nil)
  (lsp-pylsp-plugins-black-enabled t)
  ;; (lsp-pylsp-plugins-flake8-enabled nil)
  (lsp-pylsp-plugins-isort-enabled t)

  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (c-mode . lsp)
         (c-sharp-mode . lsp)
         (c-ts-mode . lsp)
         (cc-mode . lsp)
         (clojure-mode . lsp)
         (clojure-ts-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (dockerfile-mode . lsp)
         (dockerfile-ts-mode . lsp)
         (elixir-mode . lsp)
         (elm-mode . lsp)
         (go-mode . lsp)
         (graphql-mode . lsp)
         (haskell-mode . lsp)
         (html-mode . lsp)
         (html-ts-mode . lsp)
         (java-mode . lsp)
         (java-ts-mode . lsp)
         (json-mode . lsp)
         (json-ts-mode . lsp)
         (kotlin-mode . lsp)
         (lua-mode . lsp)
         (markdown-mode . lsp)
         (ocaml-mode  . lsp)
         (protobuf-mode . lsp)
         (python-mode . lsp)
         (python-ts-mode . lsp)
         (rust-mode . lsp)
         (scala-mode . lsp)
         (sh-mode . lsp)
         (sql-mode. lsp)
         (tex-mode . lsp)
         (terraform-mode . lsp)
         (tuareg-mode . lsp)
         (typescript-mode . lsp)
         (typescript-ts-mode . lsp)
         (web-mode . lsp)
         (xml-mode . lsp)
         (yaml-mode . lsp)
         (yaml-ts-mode . lsp)
         (zig-mode . lsp))

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
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-mode t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-doc-side 'right)
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-doc-show-with-mouse t))

(use-package tldr)
(use-package rfc-mode)
(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.rest" . restclient-mode))
  :hook
  ((restclient-mode . outline-minor-mode)
   (restclient-mode . (lambda ()
                        (outline-minor-mode t)
                        (local-set-key (kbd "<tab>") 'outline-toggle-children)
                        (setq outline-regexp "#+")))))

(use-package jq-mode
  :ensure-system-package jq)

(use-package jq-format
  :ensure-system-package jq)

(use-package restclient-jq
  :ensure-system-package jq)

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A". marginalia-cycle))

  :init
  (marginalia-mode))

(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package ox-reveal
  :custom
  (org-reveal-root "/home/andrea/src/forks/reveal.js"))

(use-package ox-asciidoc)

(defun ca-org-mode-setup ()
  "few org mode settings"
  (org-indent-mode t)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . ca-org-mode-setup)
  :bind
  (("C-c a" . org-agenda))
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (calc . t)
     (rust . t)
     (clojure . t)
     (sql . t)
     (plantuml . t)))
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
  :init
  (setq org-roam-v2-ack t)
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
  (persistent-scratch-autosave-mode))

(use-package plantuml-mode)

(use-package posframe)

;; eval `M-x nerd-icons-install-fonts' if you are seeing weird unicode glyphs
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package rainbow-delimiters
  :delight
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :delight
  :hook
  (prog-mode . rainbow-mode))

;; this might be a bit too much colour
(use-package rainbow-identifiers
  ;; :hook
  ;; (prog-mode . rainbow-identifiers-mode)
  )

(use-package restclient)
(use-package smart-mode-line)
;; (use-package smart-mode-line-powerline-theme)
;; (use-package smart-mode-line
;;   :custom
;;   (sml/theme 'powerline)
;;   :init (sml/setup))

(use-package smartparens
  :delight
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
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-scroll-margin 10)
  (vertico-count 20))

(use-package vertico-prescient
  :init
  (vertico-prescient-mode))

;; ;; Optionally use the `orderless' completion style.
;; (use-package orderless
;;   :init
;;   ;; Configure a custom style dispatcher (see the Consult wiki)
;;   ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
;;   ;;       orderless-component-separator #'orderless-escapable-split-on-space)
;;   (setq completion-styles '(orderless basic)
;;         ;; completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))


(use-package marginalia
  :config
  (marginalia-mode))

(use-package time
  :custom
  (display-time-24hr-format t)
  (display-time-default-load-average nil)
  (display-time-mode))

(use-package toml-mode)
(use-package undo-tree
  :diminish "U"
  :init (global-undo-tree-mode))

(use-package which-key)
(use-package wordnut)
(use-package yaml-mode)

(use-package yasnippet
  :custom
  (yas-verbosity 2)
  (yas-wrap-around-region t)

  :config
  (yas-reload-all)
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package time
  :init (display-time-mode))

(use-package paren
  :init (show-paren-mode))

;; (use-package vega-view)

;;enable again when the issue currently happening is fixed
;; (use-package which-func
;;   :config (which-func-mode t))

(use-package which-key
  :init (which-key-mode t))

(use-package windmove
  :init (windmove-default-keybindings 'shift))

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
  :ensure-system-package (rg . ripgrep)
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

(use-package zoom
  :init
  (setq zoom-mode t
        zoom-size '(0.618 . 0.618)))

(use-package hungry-delete
  :init
  (global-hungry-delete-mode t))

(use-package flycheck-grammarly
  :after flycheck
  :hook (flycheck-mode  . flycheck-grammarly-setup)
  :custom
  (flycheck-grammarly-check-time 0.8))

(global-set-key (kbd "M-p") 'ca-prev-defun)
(global-set-key (kbd "M-n") 'ca-next-defun)

(defhydra hydra-zoom (global-map "C-<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(use-package doom-themes)

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

(use-package eat)
(use-package carbon-now-sh)

(pixel-scroll-precision-mode t)

(smartparens-global-strict-mode t)
;; hack to work around https://github.com/Fuco1/smartparens/issues/1204 for now
(defalias 'sp--syntax-class-to-char #'syntax-class-to-char)

(use-package embark)
(use-package cape)
(use-package emacs
  :init
  (setq tab-always-indent 'complete)
  (column-number-mode t))

(use-package epkg)
(use-package separedit)
(use-package bats-mode)
(use-package verb)
(use-package doom-themes)

(use-package protobuf-mode)

(use-package company
  :init (global-company-mode)
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
  (company-quickhelp-mode))

(use-package swiper
  :bind
  ("C-r" . swiper-isearch-backward)
  ("C-s" . swiper-isearch))

(use-package fancy-compilation
  :ensure t
  :commands (fancy-compilation-mode))

(use-package so-long
  :config
  (global-so-long-mode t))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(use-package multi-compile)

(use-package visual-regexp)

(dolist (f '("aliases.el" "hacks.el" "custom.el"))
  (when (file-exists-p (make-relative-path f))
    ;; use require here also if possible
    (message "loading extra file" f)
    (load-file (make-relative-path f))))

(provide 'ca-init)
;;; ca-init ends here
