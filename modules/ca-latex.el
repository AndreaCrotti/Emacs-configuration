;FIXME: by default the more stupid default version is loaded, find out
;how to load the new one
(add-to-list 'auto-mode-alist '("\\.[tT]e[xX]\\'" . tex-mode))

(setq
 TeX-auto-save t
 TeX-parse-self t
 LaTeX-command "latex"
 TeX-PDF-mode t
 TeX-master nil)

;; using flyspell also here
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
;; make auctex work with reftex
(setq reftex-plug-into-AUCTeX t)

;; enable auto-fill-mode always
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(defun ca-reftex-next-label ()
  (interactive)
  (search-forward "label"))

(defun ca-reftex-previous-label ()
  (interactive)
  (search-backward "label"))

;; set a shortcut to access to labels
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'reftex-goto-label)
            (local-set-key (kbd "M-n") 'ca-reftex-next-label)
            (local-set-key (kbd "M-p") 'ca-reftex-previous-label)))

;; require reftex cite for citations
(require 'reftex-cite)
(setq reftex-default-bibliography '("cit"))

;; tell org to use listings
(setq org-export-latex-listings t)

;; you must include the listings package
;; (add-to-list 'org-export-latex-packages-alist '("" "listings"))

;; if you want colored source code then you need to include the color package
;; (add-to-list 'org-export-latex-packages-alist '("" "color"))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key [f7] 'browse-paper-at-point)))

; FIXME: overwriting too much in this case, some old program-selection
; should be kept as it was
(when ca-linux
  (setq TeX-view-program-list '(("Evince" "evince --page-label=%(outpage) %o")
                                ("Okular" "okular --page %(outpage) %o")))
  (setq TeX-view-program-selection '((output-pdf "Evince"))))

(when ca-mac
  (setq
   TeX-view-program-list '(("open" "open %o"))
   TeX-view-program-selection '((output-pdf "open"))
   TeX-source-correlate-method 'synctex
   LaTeX-command "latex -synctex=1"))

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq Tex-source-correlate-start-server t)

;; code found and adapted from http://www.emacswiki.org/emacs/RefTeX
;; Change this to the place where you store all the electronic versions.
;; redefine it to the right thing if necessary
(defvar bibtex-papers-directory
  (expand-file-name  "~/thesis/literature/"))

;; Translates a BibTeX key into the base filename of the corresponding
;; file. Change to suit your conventions.
;; Mine is:
;; - author1-author2-author3.conferenceYYYY for the key
;; - author1-author2-author3_conferenceYYYY.{ps,pdf} for the file
(defun ca-bibtex-key->base-filename (key)
  (concat bibtex-papers-directory
          (replace-regexp-in-string "\\." "_" key)))

;; Finds the BibTeX key the point is on.
;; You might want to change the regexp if you use other strange characters in the keys.
(defun ca-bibtex-key-at-point ()
  (let ((chars-in-key "A-Z-a-z0-9_:-\\."))
    (save-excursion
      (buffer-substring-no-properties
       (progn (skip-chars-backward chars-in-key) (point))
       (progn (skip-chars-forward chars-in-key) (point))))))

;; Opens the appropriate viewer on the electronic version of the paper referenced at point.
;; Again, customize to suit your preferences.
(defun ca-browse-paper-at-point ()
  (interactive)
  (let ((base (bibtex-key->base-filename (ca-bibtex-key-at-point))))
    (cond
     ((file-exists-p (concat base ".pdf"))
      (shell-command (concat "okular " base ".pdf &")))
     ((file-exists-p (concat base ".ps"))
      (shell-command (concat "okular " base ".ps &")))
     (t (message (concat "No electronic version available: " base ".{pdf,ps}"))))))

;; (global-set-key [S-f6] 'ca-browse-paper-at-point)

(add-to-list 'load-path (make-conf-path "ebib/src"))
(autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)
(add-to-list 'Info-default-directory-list (make-conf-path "ebib/manual"))

(provide 'ca-latex)
