(setq jdibug-use-jde-source-paths nil)

(add-to-list 'load-path (make-conf-path "jdee/lisp"))

(autoload 'jde-mode "jde" "jde mode" t)

;; In this way we only load if really necessary
(add-hook 'jde-mode-hook
          '(lambda ()
             (require 'ecb)
             (setq indent-tabs-mode nil)))

;; (defun turn-on-font-lock-if-enabled ()
;;   "set up to make jdee shut up")

;; TODO: put some conditional stuff for the different operating systems
;; make it more general usage
(setq jde-jdk-registry
      '(("1.6" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6/")
        ("1.5" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.5/")
        ("1.3.1" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/")))

(setq jde-jdk '("1.6" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6/"))

(setq bsh-jar "/opt/local/share/java/bsh.jar")

(provide 'ca-java)
