(require 'ca-customs)

(defun ca-next-tag ()
  (interactive)
  (semantic-refresh-tags-safe)
  (senator-next-tag))

(defun ca-activate-more-semantic-bindings ()
  "add some other nice bindings to modes supported by semantic"
  (interactive)
  (local-set-key (kbd "M-n") 'ca-next-tag)
  (local-set-key (kbd "M-p") 'senator-previous-tag)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  ;; TODO: the senator stuff should be enabled only where senator actually works!!
  (local-set-key [f6] 'senator-fold-tag-toggle)
  ;; narrows to the actual function or class analyzed
  ;; C-x n w to widen again
  (local-set-key "\C-xnn" 'semantic-narrow-to-tag)
  (local-set-key (kbd "M-.") 'semantic-complete-jump)
  (local-set-key (kbd "M-?") 'semantic-ia-fast-jump))

;TODO: problem here is that semantic might still load the shipped version
;if load-path is not set correctly
(setq semantic-load-turn-everything-on t)
(global-ede-mode nil)
(setq ede-locate-setup-options '(ede-locate-global ede-locate-locate ede-locate-idutils))

(dolist
    (hook ca-cedet-modes)
  (add-hook hook 'ca-activate-more-semantic-bindings))

(global-semantic-stickyfunc-mode 1)
;; (global-semantic-decoration-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-highlight-edits-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-mru-bookmark-mode 1)

(defun ca-c-like-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert))

(defun ca-cpp-cedet-hook ()
  (local-set-key ":" 'semantic-complete-self-insert))

(global-semanticdb-minor-mode 1)
(require 'semanticdb-global)

(provide 'ca-cedet)
