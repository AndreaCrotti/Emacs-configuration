(defun make-conf-path (path)
  (expand-file-name (concat base path)))

;TODO: try to move it inside miniconf.org instead 
(add-to-list 'load-path (make-conf-path "gnus/lisp"))
(require 'gnus-load)
(add-to-list 'Info-default-directory-list (make-conf-path "gnus/texi/"))

(add-to-list 'load-path (make-conf-path "tramp/lisp"))
(add-to-list 'load-path (make-conf-path "org-mode/lisp"))
(add-to-list 'load-path (make-conf-path "org-mode/contrib/babel/lisp"))
(add-to-list 'load-path (make-conf-path "org-mode/contrib/babel/lisp/langs"))

;; this variable must be set BEFORE org-mode is loaded or it will have no effect
(setq org-replace-disputed-keys t)

(require 'org-install)
(require 'org)
(require 'org-exp)

(org-babel-load-file (concat base "conf.org"))

;; If custom file doesn't exist create it
(if (file-exists-p custom-file)
    (load-file custom-file)
  (progn
    (find-file custom-file)
    (insert ";; Add here your customizations\n")
    (write-file custom-file)))
