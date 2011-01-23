(defun make-conf-path (path)
  (expand-file-name (concat base path)))

(add-to-list 'load-path (make-conf-path "tramp/lisp"))

(add-to-list 'load-path (make-conf-path "org-mode/lisp"))
(add-to-list 'load-path (make-conf-path "org-mode/contrib/babel/lisp"))
(add-to-list 'load-path (make-conf-path "org-mode/contrib/babel/lisp/langs"))

;; this variable must be set BEFORE org-mode is loaded or it will have no effect
(setq org-replace-disputed-keys t)

(require 'org-install)
(require 'org)

(org-babel-load-file (concat base "miniconf.org"))

;; If custom file doesn't exist create it
(if (file-exists-p custom-file)
    (load-file custom-file)
  (progn
    (find-file custom-file)
    (insert ";; Add here your customizations\n")
    (write-file custom-file)))
