(require 'ca-customs)
(require 'yasnippet)

(setq yas/root-directory
      (list (make-conf-path "yasnippet-snippets/")))

;; Maybe needed to set to fixed for some modes
(setq yas/indent-line 'auto)
(yas/initialize)
(setq yas/ignore-filenames-as-triggers nil)

(mapc 'yas/load-directory yas/root-directory)

;; don't make backups in the snippet folder, they mess up yasnippet
(add-to-list 'backup-directory-alist '("/my-snippets/" . "/tmp/"))

;; simple function to create a .yas-parents
(defun ca-make-yas-parents-file (path)
  (interactive "DPath: ")
  (find-file (concat path ".yas-parents"))
  (insert "text-mode"))

(defun ca-with-comment (str)
 (format "%s%s%s" comment-start str comment-end))

(defun ca-is-new-file ()
 "Check if it's a new file"
 (not (file-exists-p buffer-file-name)))

(defun ca-insert-header ()
  "try to insert the header smartly"
  (when
      (ca-is-new-file)
    (let
        ((snip
          (ca-find-matching-snippet (file-name-nondirectory (buffer-file-name)))))
      (when
          snip
        (ca-insert-at-startup (cdr snip))))))

(defun ca-find-matching-snippet (filename)
  (assoc-if (lambda (x) (string-match x filename))
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

;; an interesting possibility to explore is to compile a bundle for
;; each mode, and compile it on demand??
;; (yas/compile-bundle "yasnippet.el"
;;                     "mysnippets.el"
;;                     "../yasnippet-snippets")

(provide 'ca-yas)
