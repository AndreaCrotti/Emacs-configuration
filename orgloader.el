(setq base "~/Documents/pycon/")
(setq conf (concat base "conf/"))

(add-to-list 'load-path (concat conf "org-mode/lisp"))
(add-to-list 'load-path (concat conf "org-mode/contrib/babel/lisp"))

(require 'org)
(require 'org-babel)
(require 'org-babel-tangle)

(org-babel-tangle (concat base "miniconf.org"))