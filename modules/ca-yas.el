(require 'ca-customs)
(require 'ca-utils)
(require 'yasnippet)

;; Maybe needed to set to fixed for some modes
(setq
 yas-snippet-dirs (list (make-conf-path "yasnippet-snippets/"))
 yas-prompt-functions '(yas/ido-prompt yas/completing-prompt yas/x-prompt yas/dropdown-prompt yas/no-prompt)
 yas-indent-line 'auto)

(yas-reload-all)

(message "loading all the snippets")
(mapc 'yas-load-directory yas-snippet-dirs)

;; simple function to create a .yas-parents
(defun ca-make-yas-parents-file (path)
  (interactive "DPath: ")
  (find-file (concat path ".yas-parents"))
  (insert "text-mode"))

(defun ca-find-matching-snippet (filename)
  (assoc-if
   (lambda (x) (string-match x filename))
   ca-auto-header-conses))

(defun ca-insert-at-startup (snippet)
  "try to expand a snippet at startup"
  (if
      (yes-or-no-p (format "expand snippet %s?" snippet))
      (progn
        (insert snippet)
        ;; add checking
        (yas/expand))))

(add-hook 'find-file-hook 'ca-insert-header)

(provide 'ca-yas)
