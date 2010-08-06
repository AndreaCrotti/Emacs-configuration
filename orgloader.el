(setq base (file-name-directory (buffer-file-name)))
(setq conf (concat base "conf/"))

(add-to-list 'load-path (concat conf "org-mode/lisp"))
(add-to-list 'load-path (concat conf "org-mode/contrib/babel/lisp"))
(add-to-list 'load-path (concat conf "org-mode/contrib/babel/lisp/langs"))

;; this variable must be set BEFORE org-mode is loaded or it will have no effect
(setq org-replace-disputed-keys t)

(require 'org)

(org-babel-load-file (concat base "miniconf.org"))

(load-file "custom.el")
