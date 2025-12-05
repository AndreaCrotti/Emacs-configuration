(use-package gptel
  :custom
  (gptel-default-mode #'org-mode))

(use-package org-ai)

(use-package aider
  :config
  ;; For latest claude sonnet model
  (setq aider-args '("--model" "sonnet" "--no-auto-accept-architect"))
  (setenv "ANTHROPIC_API_KEY" anthropic-key)
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu) ;; for wider screen
  (aider-magit-setup-transients))

(use-package aidermacs
  :bind (("C-c b" . aidermacs-transient-menu))
  :config
  (setenv "ANTHROPIC_API_KEY" anthropic-key)
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))

(use-package aider)

(provide 'ca-ai)
