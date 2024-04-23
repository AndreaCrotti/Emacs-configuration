;; -*- lexical-binding: t -*-
(setq base (expand-file-name "~/Emacs-Configuration/lisp/"))
(add-to-list 'load-path base)
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; performance changes


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

(use-package clj-refactor
  :custom
  (cljr-auto-clean-ns nil)
  (cljr-auto-sort-ns nil)
  (cljr-add-ns-to-blank-clj-files nil))

(defun portal.api/open ()
  "Open the API portal."
  (interactive)
  (cider-nrepl-sync-request:eval
   "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open) {:theme :portal.colors/solarized-dark})) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal.api/clear ()
  "Clear the state of the portal."
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  "Close the portal."
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

(use-package cider
  :pin melpa-stable
  :hook (cider-mode . clj-refactor-mode)
  :diminish subword-mode
  :bind (("C-<f5>" . cider-test-run-test))
  ;; add this when the syntax is fixed
  ;; :hook
  ;; (lambda ()
  ;;   (add-to-list
  ;;    'completion-at-point-functions
  ;;    #'cape-cider-lsp))
  :config
  (setq cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t

        cider-overlays-use-font-lock t)
  :custom
  (cider-prompt-for-symbol nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-display-in-current-window nil)
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-use-pretty-printing t)
  (cider-repl-prompt-function 'cider-repl-prompt-abbreviated)
  (cider-repl-tab-command #'indent-for-tab-command)
  (cider-enrich-classpath t)
  (cider-repl-buffer-size-limit 100000)
  (cider-repl-require-ns-on-set nil)
  (nrepl-log-messages t)
  (cider-auto-test-mode t))

(use-package babashka)

(use-package neil
  :custom
  (neil-inject-dep-to-project-p t))

(use-package cider-hydra
  :after cider
  :hook
  (clojure-mode . cider-hydra-mode))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :bind
  (("C-c l" . lsp-clojure-refactor-menu/body))
  :hook
  (clojure-mode . subword-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.bb" . clojure-mode)))

(use-package clojure-mode-extra-font-locking)
(use-package jet)

(use-package command-log-mode)

(use-package merlin)
(use-package ess)

(use-package csv-mode)
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; (use-package diff-hl
;;   :config (global-diff-hl-mode t))

(use-package hl-todo
  :config (global-hl-todo-mode t))

(use-package diminish)
(use-package docker)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
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

(use-package elein)
;; (use-package ejc-sql
;;   :custom
;;   (clomacs-httpd-default-port 8090))

(use-package emmet-mode)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package fancy-narrow)
(use-package find-file-in-repository)
(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-clj-kondo)
;; (use-package flycheck-clojure)
(use-package flycheck-pos-tip)
(use-package flyspell
  :diminish flyspell-mode
  :config
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package graphviz-dot-mode)
(use-package gist)
(use-package gitlab)
(use-package gitlab-ci-mode)
(use-package gitlab-ci-mode-flycheck)
(use-package git-commit)
(use-package git-auto-commit-mode
  :custom
  (gac-debounce-interval 0.5))

(use-package git-timemachine)

(use-package gitconfig)

;; (use-package git-gutter
;;   :diminish
;;   :hook ((text-mode . git-gutter-mode)
;;          (prog-mode . git-gutter-mode))
;;   :config
;;   (setq git-gutter:update-interval 2))

(use-package guru-mode
  :custom
  (guru-warn-only t)
  :config
  (guru-global-mode t))

(use-package idle-highlight-mode
  :diminish idle-highlight-mode
  :hook
  (prog-mode . idle-highlight-mode))

(use-package inf-clojure)
(use-package json-mode)

(use-package kaocha-runner
  :bind (:map clojure-mode-map
              ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k r" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k w" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))

(use-package know-your-http-well)
(use-package kotlin-mode)
(use-package gradle-mode)
(use-package flycheck-gradle)
(use-package imenu)

(use-package imenu-anywhere
  :bind (("<f5>" . imenu-anywhere)))

(use-package less-css-mode)
(use-package log4j-mode)

(defhydra lsp-clojure-refactor-menu (:color blue :hint nil)
  "
Threading                      Code Manip                      Namespace                       Misc
------------------------------------------------------------------------------------------------------------------------------------------------------
_th_: Thread first             _el_: Expand let                _cn_: Clean ns                  _cp_: Cycle privacy
_tf_: Thread first all         _il_: Introduce let             _am_: Add missing libspec       _cc_: Cycle coll
_tt_: Thread last              _ml_: Move to let
_tl_: Thread last all          _ef_: Extract function
_ua_: Unwind all               _rn_: Rename
_uw_: Unwind thread            _mf_: Move formattedtextfield
"

  ("am" lsp-clojure-add-missing-libspec)
  ("cc" lsp-clojure-cycle-coll)
  ("cn" lsp-clojure-clean-ns)
  ("cp" lsp-clojure-cycle-privacy)
  ("ef" lsp-clojure-extract-function)
  ("el" lsp-clojure-expand-let)
  ("il" lsp-clojure-introduce-let)
  ("mf" lsp-clojure-move-form)
  ("ml" lsp-clojure-move-to-let)
  ("rn" lsp-rename)
  ("tf" lsp-clojure-thread-first-all)
  ("th" lsp-clojure-thread-first)
  ("tl" lsp-clojure-thread-last-all)
  ("tt" lsp-clojure-thread-last)
  ("ua" lsp-clojure-unwind-all)
  ("uw" lsp-clojure-unwind-thread))

(use-package lsp-mode
  :custom
  ;; (lsp-pylsp-plugins-autopep8-enabled  nil)
  (lsp-pylsp-plugins-black-enabled t)
  ;; (lsp-pylsp-plugins-flake8-enabled nil)
  (lsp-pylsp-plugins-isort-enabled t)

  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (c-mode . lsp)
         (c-ts-mode . lsp)
         (cc-mode . lsp)
         (clojure-mode . lsp)
         (clojure-ts-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (dockerfile-mode . lsp)
         (go-mode . lsp)
         (java-mode . lsp)
         (json-mode . lsp)
         (elixir-mode . lsp)
         (elm-mode . lsp)
         (graphql-mode . lsp)
         (haskell-mode . lsp)
         (lua-mode . lsp)
         (json-mode . lsp)
         (kotlin-mode . lsp)
         (markdown-mode . lsp)
         (python-mode . lsp)
         (python-ts-mode . lsp)
         (rust-mode . lsp)
         (scala-mode . lsp)
         (sh-mode . lsp)
         (typescript-mode . lsp)
         (terraform-mode . lsp)
         (ocaml-mode  . lsp)
         (tuareg-mode . lsp)
         (web-mode . lsp)
         (yaml-mode . lsp)
         (xml-mode . lsp)
         (zig-mode . lsp))

  :bind (("M-?" . lsp-find-definition)
         ;; ("M-/" . lsp-find-references)
         ("M-'" . lsp-treemacs-call-hierarchy))

  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (add-to-list 'lsp-language-id-configuration '(direnv-envrc-mode . "shellscript"))
  (add-to-list 'lsp-language-id-configuration '(forge-post-mode . "text"))

  :custom
  (gc-cons-threshold 100000000)
  (lsp-clojure-custom-server-command '("/opt/homebrew/bin/clojure-lsp"))
  (lsp-completion-provider :none)
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

(use-package graphql)
(use-package graphql-doc)
(use-package graphql-mode)
;; (use-package graphql-ts-mode)
(use-package ob-graphql)

(use-package lsp-java)

(use-package lsp-metals)

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-mode t))

(use-package lsp-tailwindcss)

(use-package eredis)
(use-package redis)
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

(use-package rust-mode
  :config
  (setq rust-format-on-save t))

(use-package flycheck-rust
  :hook
  (flycheck-mode . flycheck-rust-setup))

(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode))

(use-package cargo-mode)

(use-package jq-mode)

(use-package jq-format)

(use-package restclient-jq)

(use-package forge)

(use-package magit
  :bind (("\C-xg" . magit-status))
  ;; should we load forge automatically if possible?
  :config (require 'forge)
  )


(use-package magit-todos
  :after magit)

(use-package magit-delta
  ;; :hook (magit-mode . magit-delta-mode)
  )

(use-package magit-lfs)

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A". marginalia-cycle))

  :init
  (marginalia-mode))

(use-package mixed-pitch)

(use-package olivetti)

(use-package markdown-mode
  ;; :hook
  ;; (markdown-mode . mixed-pitch-mode)
  ;; (markdown-mode . olivetti-mode)
  :init
  ;; (add-hook 'markdown-mode-hook (lambda () (buffer-face-mode t)))
  (add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
  :bind
  ("M-<left>" . markdown-promote)
  ("M-<right>" . markdown-demote))

(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package nix-mode)
(use-package guix)

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

(use-package ob-clojurescript)
(use-package ob-rust)

(require 'ob-clojure)

(use-package ob-nix)
(use-package ob-sql-mode)
(use-package org-sql)

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
     (nix . t)
     (rust . t)
     (clojure . t)
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

(use-package org-roam-bibtex)

(use-package citar
  :custom
  (citar-bibliography '("~/RoamNotes/references.bib")))

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

(use-package projectile
  :diminish projectile
  :config
  (projectile-global-mode)
  :bind (("<f6>" . projectile-ripgrep)
         ("C-<f6>" . projectile-replace)
         ("<f7>" . projectile-find-file)
         ("<f8>" . projectile-run-shell)
         ("<f9>" . projectile-command-map)
         :map projectile-mode-map
         ("s-d" . projectile-find-dir)
         ("s-p" . projectile-switch-project)
         ("s-f" . projectile-find-file)
         ("s-a" . projectile-ag))
  :custom
  (projectile-completion-system 'default)
  (projectile-switch-project-action 'projectile-find-file))

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
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

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
  (vertico-scroll-margin 0)
  (vertico-count 20))

(use-package vertico-prescient
  :init
  (vertico-prescient-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


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

(use-package web-mode)
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

(setq dired-auto-revert-buffer 1)
(setq dired-isearch-filenames 'dwim)
(setq dired-listing-switches "-al")

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

(use-package diredfl
  :config
  (diredfl-global-mode))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(defun ac/enter-focus-mode ()
  "Enter focus mode."
  (interactive)
  (darkroom-mode 1)
  (display-line-numbers-mode 0))

(defun ac/leave-focus-mode ()
  "Leave focus mode."
  (interactive)
  (darkroom-mode 0)
  (display-line-numbers-mode 1))

(defun ac/toggle-focus-mode ()
  "Toggle focus mode."
  (interactive)
  (if (symbol-value darkroom-mode)
      (ac/leave-focus-mode)
    (ac/enter-focus-mode)))

(use-package ibuffer-vc
  :defer t
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(use-package typescript-mode)

(use-package winner
  :config (winner-mode t))

(use-package wakatime-mode)

(use-package all-the-icons)

(use-package treemacs
  :after (projectile)
  :custom
  (treemacs-filewatch-mode t)
  (treemacs-follow-mode t)
  ;; this keeps on asking the mode otherwise
  (treemacs-fringe-indicator-mode nil)
  (treemacs-git-commit-diff-mode nil)
  (treemacs-git-mode t)
  (treemacs-indent-guide-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-tag-follow-mode nil)
  (treemacs-tag-follow-delay 3)
  )

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-all-the-icons)

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

(use-package fish-mode)

(use-package hideshow
  :hook ((prog-mode . hs-minor-mode))
  :bind
  (("C-<tab>" . hs-toggle-hiding)))

(use-package shell
  :init
  (dirtrack-mode))

(use-package zoom
  :init
  (setq zoom-mode t
        zoom-size '(0.618 . 0.618)))

(use-package terraform-mode)
(use-package terraform-doc)

(use-package hungry-delete
  :init
  (global-hungry-delete-mode t))

(use-package flycheck-grammarly
  :after flycheck
  :hook (flycheck-mode  . flycheck-grammarly-setup)
  :custom
  (flycheck-grammarly-check-time 0.8))

(use-package tide)

(use-package tern)

(use-package sly)

(global-set-key (kbd "M-p") 'ca-prev-defun)
(global-set-key (kbd "M-n") 'ca-next-defun)

(dolist (f '("aliases.el" "hacks.el" "custom.el"))
  (when (file-exists-p (make-relative-path f))
    ;; use require here also if possible
    (message "loading extra file" f)
    (load-file (make-relative-path f))))

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

(use-package github-browse-file)

(use-package sqlite3)
(use-package sqlformat
  :custom
  (sqlformat-command 'sqlfluff)
  ;; how do we make this smarter?
  (sqlformat-args '("--dialect" "mysql"))
  :hook (sql-interactive-mode . (lambda ()
                                  (toggle-truncate-lines t))))


(use-package zig-mode)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;; (add-hook 'before-save-hook #'lsp-organize-imports t t)
  )

(use-package lua-mode)

(use-package go-mode
  :hook
  (go-mode . lsp-go-install-save-hooks))

(use-package gorepl-mode
  :hook
  (go-mode . gorepl-mode))

(use-package chezmoi)

(use-package highlight-indent-guides
  :custom (highlight-indent-guides-method 'character)
  :hook ((prog-mode . highlight-indent-guides-mode)))

(use-package crux)
(use-package minimap)
(use-package bm)
(use-package ssh-tunnels)
(use-package ssh)
(use-package ssh-agency)
(use-package ssh-config-mode)
(use-package focus)

(use-package svelte-mode)

(use-package direnv
 :config
 (direnv-mode))

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                cider-repl-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package eshell
  :hook
  (eshell-mode . (lambda ()
                   (setq-local corfu-auto nil)
                   (corfu-mode))))
(use-package eat)
(use-package exercism)
(use-package leetcode)
(use-package carbon-now-sh)
(use-package dune)
(use-package tuareg)

(defun clerk-show ()
  "Show clerk."
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)
(pixel-scroll-precision-mode t)

(use-package gptel)
(use-package org-ai)

(smartparens-global-strict-mode t)
;; hack to work around https://github.com/Fuco1/smartparens/issues/1204 for now
(defalias 'sp--syntax-class-to-char #'syntax-class-to-char)

(use-package erlang)
(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package tree-sitter-indent)


(use-package treesit-auto
  ;; causes some issue for cider for example
  ;; :config
  ;; (treesit-auto-add-to-auto-mode-alist 'all)
  )

(use-package combobulate
  :straight t
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o"))

(use-package embark)

(use-package corfu
  :custom
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  (corfu-cycle t)
  (corfu-count 20)
  (corfu-max-width 120)
  (corfu-popupinfo-delay '(1.5 0.5))
  (corfu-popupinfo-hide nil)
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("TAB" . corfu-insert)
              ([tab] . corfu-insert)
              ("C-f" . corfu-insert))

  :init
  (global-corfu-mode)

  :config
  (corfu-popupinfo-mode t)
  (corfu-history-mode t)
  (corfu-indexed-mode t))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 0.8 :background nil))
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape)

(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package emacs
  :init
  (setq tab-always-indent 'complete))

(use-package epkg)
(use-package separedit)
(use-package bats-mode)
(use-package verb)

(use-package haskell-mode)
(use-package lsp-haskell)
(use-package nix-haskell-mode)
;; (use-package hsearch)
(use-package ghci-completion)
(use-package flymake-hlint)
(use-package doom-themes)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path-from-shell-variables "LSP_USE_PLISTS"))

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(use-package protobuf-mode)
(provide 'ca-init)
;;; ca-init ends here
