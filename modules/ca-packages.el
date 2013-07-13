(when (locate-library "package")
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t))

(defvar ca-packages
  '(ack-and-a-half auctex clojure-mode coffee-mode deft expand-region
                   gist groovy-mode haml-mode haskell-mode inf-ruby
                   magit magithub markdown-mode projectile python
                   sass-mode rainbow-mode scss-mode solarized-theme
                   volatile-highlights yaml-mode yari zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun ca-packages-installed-p ()
  (loop for p in ca-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun ca-install-missing-packages ()
  (interactive)
  (unless (ca-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs packages is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p ca-packages)
      (when (not (package-installed-p p))
        (package-install p)))))


(autoload 'auto-install-from-emacswiki "auto-install" "auto install from emacswiki" t)
(setq auto-install-directory (make-conf-path "auto-install/"))

(provide 'ca-packages)
