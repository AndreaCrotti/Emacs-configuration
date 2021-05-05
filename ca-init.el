(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(eval-when-compile (require 'cl))

(defun make-relative-path (filename)
  (concat base filename))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path (make-relative-path "use-package"))
  (require 'use-package))

(require 'package)
(package-initialize)
(package-refresh-contents)
(setq use-package-always-ensure t)

(setq custom-safe-themes t)
(load-file (make-relative-path "functions.el"))
(load-file (make-relative-path "misc.el"))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-unsure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package ack)
(use-package adoc-mode)
(use-package ag)
(use-package auto-highlight-symbol)
(use-package autorevert
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode))

(use-package beacon
  :ensure t
  :custom
  (beacon-blink-duration 0.5))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package clj-refactor
  :custom
  (cljr-add-ns-to-blank-clj-files nil))

(use-package cider
  :ensure t
  :defer t
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :bind (("C-<f5>" . cider-test-run-test))
  :config
  (setq cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t

        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing)
  :custom
  (cider-prompt-for-symbol nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-display-in-current-window nil)
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-use-pretty-printing t)
  (cider-repl-prompt-function 'cider-repl-prompt-abbreviated)
  (cider-repl-tab-command #'indent-for-tab-command)
  (cider-repl-buffer-size-limit 100000)
  (cider-repl-require-ns-on-set nil)
  (nrepl-log-messages t)
  (cider-auto-test-mode t))

(use-package clj-refactor)
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :bind (("M-." . lsp-find-definition))
  :init
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package clojure-mode-extra-font-locking)

(use-package company
  :init (global-company-mode)
  :ensure t
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.3)
  (company-show-numbers t))

(use-package company-dict)
(use-package company-restclient)
(use-package company-shell)
(use-package csv-mode)
(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package docker)
(use-package dockerfile-mode)
(use-package dracula-theme)
(use-package edit-server)
(use-package eldoc
  :diminish eldoc-mode
  :config (global-eldoc-mode))

(use-package elein)
(use-package emmet-mode)
(use-package expand-region)
(use-package fancy-narrow)
(use-package find-file-in-repository)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-clj-kondo)
(use-package flycheck-clojure)
(use-package flycheck-pos-tip)
(use-package flyspell
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package gist)
(use-package git-commit)
(use-package gitconfig)
(use-package guru-mode
  :custom
  (guru-warn-only t)
  :config
  (guru-global-mode t))

(use-package idle-highlight-mode
  :diminish idle-highlight-mode
  :config (add-hook 'prog-mode-hook 'idle-highlight-mode))

(use-package inf-clojure)
(use-package json-mode)

(use-package kaocha-runner
  :ensure t
  :bind (:map clojure-mode-map
              ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k r" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k w" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))

(use-package know-your-http-well)
(use-package kotlin-mode)
(use-package imenu
  :bind (("<f5>" . lsp-ui-menu)))

(use-package less-css-mode)
(use-package log4j-mode)

(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (web-mode . lsp)
         (sh-mode . lsp))

  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))

  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  ;; Optional: In case `clojure-lsp` is not in your PATH
  (setq lsp-clojure-server-command '("bash" "-c" "clojure-lsp"))

  :custom
  ;; turn this on to capture client/server comms before
  ;; submitting bug reports with `lsp-workspace-show-log`
  ;; (lsp-log-io t)

  (lsp-eldoc-enable-hover nil)
  (lsp-enable-indentation nil)
  (lsp-enable-folding nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay .01)
  (lsp-keymap-prefix nil))

(use-package lsp-ui
  :disabled t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package restclient
  :init
  (add-to-list 'auto-mode-alist '("\\.rest" . restclient-mode))
  :config
  (add-hook 'restclient-mode-hook 'outline-minor-mode)
  (add-hook 'restclient-mode-hook
            (lambda ()
              (outline-minor-mode t)
              (local-set-key (kbd "<tab>") 'outline-toggle-children)
              (setq outline-regexp "#+"))))

(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'company-mode))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode))

(use-package magit
  :bind (("\C-xg" . magit-status)))

(use-package markdown-mode)
(use-package multiple-cursors
  :bind ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package nix-mode)
(use-package org-bullets)
(use-package paradox)
(use-package persistent-scratch
  :config
  (persistent-scratch-autosave-mode))

(use-package powerline
  :custom
  (powerline-arrow-shape 'curve)
  (powerline-default-separator-dir '(right . left)))

(use-package projectile
  :diminish projectile
  :ensure t
  :config
  (projectile-global-mode)
  :bind (("<f6>" . projectile-ag)
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
  (projectile-completion-system 'default))

(use-package rainbow-delimiters
  :ensure t
  :delight
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :delight
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package restclient)
(use-package smart-mode-line-powerline-theme)
(use-package smart-mode-line
  :custom
  (sml/theme 'powerline)
  :init (sml/setup))

(use-package smartparens
  :ensure t
  :delight
  :config
  (require 'smartparens-config)
  (turn-on-smartparens-strict-mode)
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

(use-package selectrum
  :ensure t
  :config (selectrum-mode))

(use-package selectrum-prescient
  :ensure t
  :config (selectrum-prescient-mode))

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

;; (use-package yasnippet-snippets
;;   :ensure t)

(use-package yasnippet
  :ensure t
  :custom
  (yas-verbosity 2)
  (yas-wrap-around-region t)

  :config
  (yas-reload-all)
  (yas-global-mode))

(setq dired-auto-revert-buffer 1)
(setq dired-isearch-filenames 'dwim)
(setq dired-listing-switches "-al")

(use-package time
  :init (display-time-mode))

(use-package linum
  :init (global-linum-mode))

(use-package paren
  :init (show-paren-mode))

(use-package which-func
  :init (which-function-mode))

(use-package which-key
  :init (which-key-mode))

(use-package windmove
  :init (windmove-default-keybindings 'shift))

;; TODO: reconfigure these two??
(global-prettify-symbols-mode t)
(transient-mark-mode t)

(use-package dumb-jump
  :ensure t
  :bind (("M-?" . dumb-jump-go)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-vc
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package winner
  :config (winner-mode t))

(global-set-key (kbd "M-p") 'ca-prev-defun)
(global-set-key (kbd "M-n") 'ca-next-defun)

(defalias 'bb 'bury-buffer)
(defalias 'dml 'delete-matching-lines)
(defalias 'eb 'eval-buffer)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'er 'eval-region)
(defalias 'go 'google-search-it)
(defalias 'gs 'google-search-selection)
(defalias 'll 'load-library)
(defalias 'qrs 'query-replace-regexp)
(defalias 'qs 'query-replace)
(defalias 'rs 'replace-string)
(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'ys 'yas/reload-all)
(defalias 'yv 'yas/visit-snippet-file)

(defalias 'rb 'revert-buffer)

(defalias 'sh 'shell)

(defalias 'ws 'whitespace-mode)
(defalias 'bu 'browse-url)

(dolist (f '("custom.el" "hacks.el"))
  (when (file-exists-p (make-relative-path f))
    (message "loading extra file" f)
    (load-file (make-relative-path f))))
