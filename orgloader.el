(add-to-list 'load-path "~/Documents/pycon/conf/org-mode/lisp")
(add-to-list 'load-path "~/Documents/pycon/conf/org-mode/contrib/babel/lisp")

(require 'org)
(require 'org-babel)
(require 'org-babel-tangle)

(org-babel-load-file "~/Documents/pycon/miniconf.org")