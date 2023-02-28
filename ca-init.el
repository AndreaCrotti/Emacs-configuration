
;; performance changes

(setq gc-cons-threshold (* 50 1000 1000))

;; emacs 28 native compilation settings
(setq native-comp-deferred-compilation t
      native-comp-async-query-on-exit t
      native-comp-async-jobs-number 4
      native-comp-async-report-warnings-errors nil)

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

(setq custom-safe-themes t)
(load-file (make-relative-path "functions.el"))
(load-file (make-relative-path "misc.el"))

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
  :config
  (add-hook 'log4j-mode-hook #'auto-revert-tail-mode))

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
  (cljr-add-ns-to-blank-clj-files nil))

(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open) {:theme :portal.colors/solarized-dark})) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

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

(use-package cider-hydra
  :after cider
  :config
  (add-hook 'clojure-mode-hook #'cider-hydra-mode))

(use-package clj-refactor)
(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :bind
  (("C-c l" . lsp-clojure-refactor-menu/body))
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.bb" . clojure-mode)))

(use-package clojure-mode-extra-font-locking)
(use-package jet)

(use-package company
  :init (global-company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-show-numbers t))

(use-package command-log-mode)

(use-package company-dict)
(use-package company-restclient)
(use-package company-shell)
(use-package company-math)
(use-package merlin)
(use-package ess)
(use-package company-terraform)
(use-package company-web)

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
(use-package ejc-sql
  :custom
  (clomacs-httpd-default-port 8090))

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
(use-package graphql-mode)
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
  (lsp-log-io t)
  (lsp-lens-enable t)
  (lsp-signature t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-indentation t)
  (lsp-enable-folding t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay .01)
  (lsp-keymap-prefix nil))

(use-package lsp-java)

(use-package lsp-metals)

(use-package lsp-ui
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

(use-package forge
  :after magit)

(use-package magit-delta
  ;; :hook (magit-mode . magit-delta-mode)
  )

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A". marginalia-cycle))

  :init
  (marginalia-mode))

(use-package markdown-mode
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
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package org-msg)

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
  :after magit
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
  (projectile-switch-project-action 'magit-status))

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

(use-package smart-mode-line)
;; (use-package smart-mode-line-powerline-theme)
;; (use-package smart-mode-line
;;   :custom
;;   (sml/theme 'powerline)
;;   :init (sml/setup))

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

(use-package typescript-mode)

(use-package winner
  :config (winner-mode t))

(use-package wakatime-mode)

(use-package all-the-icons)

(defun treemacs-enable-and-show ()
  (when (and buffer-file-name
             ;; hack since RoamNotes capture buffers get weird
             (not (string-match-p "RoamNotes" buffer-file-name)))
    (when (equal 'none (treemacs-current-visibility))
      (treemacs--init))

    (with-current-buffer (find-buffer-visiting buffer-file-name)
      (treemacs-display-current-project-exclusively))))

(use-package treemacs
  :after (projectile)
  :custom
  (treemacs-tag-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode nil)
  (treemacs-indent-guide-mode t)
  (treemacs-git-commit-diff-mode nil)

  :config
  (add-hook 'find-file-hook 'treemacs-enable-and-show))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-all-the-icons)

(use-package ripgrep
  :config
  (setq ripgrep-arguments '("--max-columns 150" "--max-columns-preview")))

(use-package indium)

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

(global-set-key (kbd "M-p") 'ca-prev-defun)
(global-set-key (kbd "M-n") 'ca-next-defun)

(dolist (f '("aliases.el" "hacks.el" "custom.el"))
  (when (file-exists-p (make-relative-path f))
    (message "loading extra file" f)
    (load-file (make-relative-path f))))

(defhydra hydra-zoom (global-map "C-<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))
