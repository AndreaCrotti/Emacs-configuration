;; performance changes

(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
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
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq use-package-always-ensure t)

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

(setq custom-safe-themes t)
(load-file (make-relative-path "functions.el"))
(load-file (make-relative-path "misc.el"))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-unsure t)

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
  ;; (add-hook 'adoc-mode-hook (lambda () (buffer-face-mode t)))
  )

(use-package ag)
;; (use-package auto-highlight-symbol)
(use-package autorevert
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode))

(use-package beacon
  :custom
  (beacon-blink-duration 0.5))

(use-package browse-at-remote)
(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package clj-refactor
  :custom
  (cljr-add-ns-to-blank-clj-files nil))

(use-package cider
  :defer t
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :bind (("C-<f5>" . cider-test-run-test))
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
  (cider-repl-buffer-size-limit 100000)
  (cider-repl-require-ns-on-set nil)
  (nrepl-log-messages t)
  (cider-auto-test-mode t))

(use-package clj-refactor)
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.bb" . clojure-mode)))

(use-package clojure-mode-extra-font-locking)

(use-package company
  :init (global-company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.3)
  (company-show-numbers t))

(use-package command-log-mode)

(use-package company-dict)
(use-package company-restclient)
(use-package company-shell)
(use-package csharp-mode)
(use-package csv-mode)
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package diminish)
(use-package docker)
(use-package dockerfile-mode)
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
(use-package emmet-mode)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package fancy-narrow)
(use-package find-file-in-repository)
(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-clj-kondo)
(use-package flycheck-clojure)
(use-package flycheck-pos-tip)
(use-package flyspell
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package graphviz-dot-mode)
(use-package gist)
(use-package gitlab)
(use-package gitlab-ci-mode)
(use-package gitlab-ci-mode-flycheck)
(use-package git-commit)
(use-package git-auto-commit-mode
  :custom
  (gac-debounce-interval 0.5))

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
  :config (add-hook 'prog-mode-hook 'idle-highlight-mode))

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
(use-package imenu
  :bind (("<f5>" . imenu)))

(use-package less-css-mode)
(use-package log4j-mode)

(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (scala-mode . lsp)
         (web-mode . lsp)
         (sh-mode . lsp))

  :bind (("M-?" . lsp-find-definition)
         ;; ("M-/" . lsp-find-references)
         ("M-'" . lsp-treemacs-call-hierarchy))
  :config
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
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
  (lsp-lens-enable t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-indentation nil)
  (lsp-enable-folding t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-idle-delay .01)
  (lsp-keymap-prefix nil))

(use-package lsp-java)

(use-package lsp-metals)

(use-package lsp-ui
  :disabled t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(use-package eredis)
(use-package redis)
(use-package tldr)

(use-package rfc-mode)

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

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A". marginalia-cycle))

  :init
  (marginalia-mode))

(use-package markdown-mode
  :init
  ;; (add-hook 'markdown-mode-hook (lambda () (buffer-face-mode t)))
  (add-to-list 'auto-mode-alist '("\\.md" . markdown-mode)))

(use-package multiple-cursors
  :bind ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package nix-mode)
(use-package ox-reveal)
(use-package ox-asciidoc)

(defun ca-org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(require 'ob-clojure)

(use-package org
  :hook (org-mode . ca-org-mode-setup)
  :bind
  (("C-c a" . org-agenda))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-babel-clojure-backend 'cider)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (calc . t)
     (plantuml . t)))
  :custom
  (org-src-tab-acts-natively t)
  (org-hide-emphasis-markers t)
  (org-agenda-files (list (file-truename "~/RoamNotes/")
                          (file-truename "~/RoamNotes/daily/"))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/RoamNotes"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n g" . org-roam-graph)
         ("C-c n c" . org-roam.capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol)
)
(use-package paradox)
(use-package persistent-scratch
  :config
  (persistent-scratch-autosave-mode))

(use-package plantuml-mode)

(use-package posframe)

(use-package powerline
  :custom
  (powerline-arrow-shape 'curve)
  (powerline-default-separator-dir '(right . left)))

(use-package projectile
  :diminish projectile
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
  :delight
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :delight
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package restclient)
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package smart-mode-line-powerline-theme)
(use-package smart-mode-line
  :custom
  (sml/theme 'powerline)
  :init (sml/setup))

(use-package smartparens
  :delight
  :config
  (smartparens-global-strict-mode t)
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

(use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))

(use-package selectrum
  :config (selectrum-mode))

(use-package selectrum-prescient
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

(use-package linum
  :init (global-linum-mode))

(use-package paren
  :init (show-paren-mode))

(use-package vega-view)

(use-package which-func
  :init (which-function-mode))

(use-package which-key
  :init (which-key-mode))

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

(use-package darkroom
  :commands darkroom-mode
  :config
  (setq darkroom-text-scale-increase 1))

(defun ac/enter-focus-mode ()
  (interactive)
  (darkroom-mode 1)
  (display-line-numbers-mode 0))

(defun ac/leave-focus-mode ()
  (interactive)
  (darkroom-mode 0)
  (display-line-numbers-mode 1))

(defun ac/toggle-focus-mode ()
  (interactive)
  (if (symbol-value darkroom-mode)
      (ac/leave-focus-mode)
    (ac/enter-focus-mode)))

(use-package ibuffer-vc
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

;; this might affect multiple cursors in a weird way,
;; since you can't just write to replace
;; (use-package selected
;;   :commands selected-minor-mode
;;   :init
;;   (setq selected-org-mode-map (make-sparse-keymap))
;;   :bind (:map selected-keymap
;;               ("q" . selected-off)
;;               ("u" . upcase-region)
;;               ("d" . downcase-region)
;;               ("w" . count-words-region)
;;               ("m" . apply-macro-to-region-lines)
;;               :map selected-org-mode-map
;;               ("t" . org-table-convert-region)))

(use-package typescript-mode)

(use-package winner
  :config (winner-mode t))

(use-package wakatime-mode)

(global-set-key (kbd "M-p") 'ca-prev-defun)
(global-set-key (kbd "M-n") 'ca-next-defun)

(dolist (f '("aliases.el" "hacks.el" "custom.el"))
  (when (file-exists-p (make-relative-path f))
    (message "loading extra file" f)
    (load-file (make-relative-path f))))
