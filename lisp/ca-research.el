(use-package merlin)

(use-package haskell-mode)
(use-package lsp-haskell)
(use-package ghci-completion)

(use-package zig-mode
  :ensure-system-package zig)

(use-package lua-mode
  :ensure-system-package lua)

(use-package go-mode
  :ensure-system-package go
  :hook
  (go-mode . lsp-go-install-save-hooks))

(use-package gorepl-mode
  :hook
  (go-mode . gorepl-mode))

(use-package dune)
(use-package tuareg)
(use-package exercism)
(use-package leetcode)
(use-package erlang)
(use-package flymake-hlint)
(use-package sly
  :ensure-system-package sbcl)

(use-package rust-mode
  :config
(use-package flycheck-rust
  :hook
  (flycheck-mode . flycheck-rust-setup))

(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode))

(use-package cargo-mode)

  (setq rust-format-on-save t))

(use-package ob-rust)

(use-package kotlin-mode)
(use-package gradle-mode)
(use-package flycheck-gradle)

(use-package elixir-mode)
(use-package inf-elixir)
(use-package flycheck-elixir)
(use-package mix
  :hook
  (elixir-mode . mix-mode-hook))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(provide 'ca-research)