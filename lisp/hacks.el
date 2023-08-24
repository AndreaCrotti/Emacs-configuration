;; hacky changes that should be done directly in the original package
;; they were in


(define-derived-mode wordnut-mode special-mode "WordNut"
  "Major mode interface to WordNet lexical database.
Turning on wordnut mode runs the normal hook `wordnut-mode-hook'.

\\{wordnut-mode-map}"

  (setq-local visual-line-fringe-indicators '(nil top-right-angle))
  (visual-line-mode 1)

  ;; we make a custom imenu index
  (setq imenu-generic-expression nil)
  (setq-local imenu-create-index-function 'wordnut--imenu-make-index)
  (imenu-add-menubar-index)

  (setq font-lock-defaults '(wordnut-font-lock-keywords))

  ;; if user has adaptive-wrap mode installed, use it
  (if (and (fboundp 'adaptive-wrap-prefix-mode)
	   (boundp 'adaptive-wrap-extra-indent))
      (progn
	(setq adaptive-wrap-extra-indent 3)
	(adaptive-wrap-prefix-mode 1)))
  ;; make the wordnet buffer nicer to use, see:
  ;; https://github.com/gromnitsky/wordnut/pull/10
  ;; for a PR to the upstream repository

  (setq org-startup-folded nil)
  (org-mode)
  (view-mode))

;; change the size of markup in asciidoc documents to
;; something that mmakes more sense
;; (dolist (i (number-sequence 0 5))
;;   (let ((new-height (+ 1.0 (/ (- 10.0 i) 20.0))))
;;     (set-face-attribute
;;      (intern (format "markup-title-%d-face" i)) nil :height new-height)))

(defun 4k-p ()
  (> (x-display-pixel-width) 7000))

;; use a different font height depending on resolution
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (4k-p)
            (set-face-attribute 'default frame :font "Fira Code" :height 150)
          (set-face-attribute 'default frame :font "Fira Code" :height 120)))))
