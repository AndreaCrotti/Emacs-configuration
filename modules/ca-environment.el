(defconst ca-sysop
  (cond ((string-match "linux" system-configuration) "linux")
        ((string-match "apple" system-configuration) "mac")
        ((string-match "win" system-configuration) "win") (t "other")))

(defconst ca-linux (string= "linux" ca-sysop))
(defconst ca-mac (string= "mac" ca-sysop))
(defconst ca-win (string= "win" ca-sysop))
(defconst ca-other (string= "other" ca-sysop))

(when ca-mac
  (add-to-list 'exec-path "/opt/local/bin")
  (setq
   ns-alternate-modifier (quote none)
   ns-pop-up-frames 1
   ns-command-modifier (quote meta)))

(when ca-mac
  (let ((ports-lisp "/opt/local/share/emacs/site-lisp/"))
    (when
        (file-exists-p ports-lisp)
      (add-to-list 'load-path ports-lisp))))

(provide 'ca-environment)
