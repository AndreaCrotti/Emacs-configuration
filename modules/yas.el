
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
