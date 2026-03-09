;; -*- lexical-binding: t -*-

;; Increase GC threshold for faster startup
(setq gc-cons-threshold (* 100 1000 1000))

;; Don't let package.el initialize packages before init.el
(setq package-enable-at-startup nil)

;; Disable UI elements early to avoid momentary display
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Don't resize frame to preserve rows/columns when setting font
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
