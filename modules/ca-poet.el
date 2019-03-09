(require 'org-bullets)
(require 'olivetti)
(require 'typo)

(defun writer-mode ()
  (variable-pitch-mode 1)
  (set-face-attribute 'default nil :family "Iosevka" :height 130)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka")
  (set-face-attribute 'variable-pitch nil :family "Baskerville")
  (olivetti-mode t)
  (typo-mode t)
  (blink-cursor-mode t)
  (linum-mode nil)
  (load-theme 'poet-dark))

(add-hook 'org-mode-hook 'writer-mode)
(add-hook 'markdown-mode-hook 'writer-mode)

(provide 'ca-poet)
