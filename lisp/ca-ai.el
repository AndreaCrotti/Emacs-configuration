;; -*- lexical-binding: t; -*-

(use-package org-ai)

(use-package eca)

(use-package gptel
  :config
  (require 'macher)
  :custom
  (gptel-default-mode #'org-mode)
  (gptel-max-tokens 5000))

(provide 'ca-ai)
