(setq base "~/Documents/pycon/")
(setq conf (concat base "conf/"))

(add-to-list 'load-path (concat conf "org-mode/lisp"))
(add-to-list 'load-path (concat conf "org-mode/contrib/babel/lisp"))
(add-to-list 'load-path (concat conf "org-mode/contrib/babel/lisp/langs"))

(require 'org)
(require 'org-babel)
(require 'org-babel-tangle)
(require 'org-babel-init)

(org-babel-load-file (concat base "miniconf.org"))
