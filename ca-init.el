(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/Emacs-Configuration/use-package")
  (require 'use-package))

(load-file "~/Emacs-Configuration/functions.el")
(load-file "~/Emacs-Configuration/misc.el")

(use-package ack)
(use-package adoc-mode)
(use-package ag)
(use-package auto-highlight-symbol)
(use-package browse-kill-ring)
(use-package c-eldoc)
(use-package cider
  :init (cider-mode)
  :hook clojure-mode)

(use-package cider-decompile)
(use-package cider-eval-sexp-fu)
(use-package clj-refactor)
(use-package cljr-helm)
(use-package cljsbuild-mode)
(use-package clojure-mode
  :custom
  (cider-repl-use-pretty-printing t))

(use-package clojure-mode-extra-font-locking)
(use-package color-moccur)
(use-package company
  :ensure t
  :init (global-company-mode))
(use-package company-dict)
(use-package company-restclient)
(use-package company-shell)
(use-package csv-mode)
(use-package docker)
(use-package dockerfile-mode)
(use-package dracula-theme)
(use-package edit-server)
(use-package elein)
(use-package emmet-mode)
(use-package expand-region)
(use-package eshell
  :bind (("<f8>" . eshell)))
(use-package fancy-narrow)
(use-package find-file-in-repository)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(use-package flycheck-clj-kondo)
(use-package flycheck-clojure)
(use-package flycheck-pos-tip)
(use-package gist)
(use-package git-commit)
(use-package gitconfig)
(use-package helm)
(use-package helm-ag)
(use-package helm-cider)
(use-package helm-clojuredocs)
(use-package helm-company)
(use-package helm-flycheck)
(use-package helm-flyspell)
(use-package helm-google)
(use-package helm-make)
(use-package helm-projectile
  :bind (( "<f7>" . helm-projectile-find-file)))
(use-package helm-swoop)
(use-package inf-clojure)
(use-package json-mode)
(use-package know-your-http-well)
(use-package less-css-mode)
(use-package log4j-mode)
(use-package magit
  :ensure t
  :bind (("\C-xg" . magit-status)))
(use-package markdown-mode)
(use-package multiple-cursors)
(use-package nginx-mode)
(use-package nix-mode)
(use-package ob-diagrams)
(use-package ob-http)
(use-package org-bullets)
(use-package org-gcal)
(use-package paradox)
(use-package persistent-scratch)
(use-package powerline
  :custom
  (powerline-arrow-shape 'curve)
  (powerline-default-separator-dir '(right . left)))

(use-package projectile
  :init (projectile-global-mode)
  :bind (("C-c C-p" . projectile-command-map)
	 ("s-p" . projectile-command-map)))
(use-package rainbow-delimiters
  :ensure t
  :init (rainbow-delimiters-mode))
(use-package rainbow-mode
  :ensure t
  :init (rainbow-mode))
(use-package restclient)
(use-package smart-mode-line
  :custom
  (sml/theme 'powerline)
  :init (sml/setup))

(use-package smart-mode-line-powerline-theme)
(use-package smartparens
  :ensure t
  :init (smartparens-global-strict-mode)
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

(use-package toml-mode)
(use-package undo-tree
  :init (global-undo-tree-mode))
(use-package web-mode)
(use-package which-key)
(use-package wordnut)
(use-package yaml-mode)
(use-package yasnippet)
(use-package yasnippet-snippets)

(use-package dired
  :custom
  (dired-auto-revert-buffer 1)
  (dired-isearch-filenames 'dwim)
  (dired-listing-switches "-al"))

(use-package simple
  :init (column-number-mode))

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

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
	 ("M-s o" . helm-occur)
	 ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ([f10] . helm-buffers-list)
         ([S-f10] . helm-recentf))
  :custom
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-locate-fuzzy-match t)
  (helm-use-frame-when-more-than-two-windows nil)
  (helm-M-x-fuzzy-match t)
  (helm-autoresize-mode t))

(use-package nrepl-client
  :custom
  (nrepl-log-messages t))

(use-package cider-repl
  :custom
  (cider-repl-use-clojure-font-lock t))

(use-package cider-test
  :custom
  (cider-auto-test-mode t))

(use-package dumb-jump
  :bind (("M-?" . dumb-jump-go)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package winner)

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

(when (file-exists-p "~/Emacs-Configuration/custom.el")
  (message "loading custom file")
  (load-file "~/Emacs-Configuration/custom.el"))
