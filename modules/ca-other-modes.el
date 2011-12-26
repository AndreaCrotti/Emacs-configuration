(require 'ca-customs)

;TODO: is this the same as just passing in the variable?
(when ca-show-battery
  (display-battery-mode t))

(add-to-list 'load-path (make-conf-path "ess-mirror/lisp"))
(autoload 'R "ess-site" "loading R env" t)
(setq ess-directory (expand-file-name "~/"))
(setq ess-ask-for-ess-directory nil)

(autoload 'paredit-mode "paredit" "paredit mode" t)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(add-to-list 'auto-mode-alist '("\\.mirah$" . ruby-mode))

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

(autoload 'nesc-mode "nesc" nil t)
(add-to-list 'auto-mode-alist '("\\.nc$" . nesc-mode))

(autoload 'ned-mode "ned-mode" "Major Mode for editing Ned files" t)
(add-to-list 'auto-mode-alist '("\\.ned$" . ned-mode))

;TODO: enable a finer mechanism to let this work
;read from http://yasnippet.googlecode.com/svn/trunk/doc/snippet-expansion.html#the-condition-system

(add-to-list 'auto-mode-alist '("\\.msg$" . c-mode))

(add-to-list 'auto-mode-alist
             '("\\.applescript$" . applescript-mode))
(autoload 'applescript-mode "applescript-mode" "mode for applescript files" t)

(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup '(slime-fancy))

(setq nxhtml-menu-mode nil)

(autoload 'mako-html-mumamo-mode "autostart" "auto starting of nxhtml" t)
;; add other modes whenever needed
(add-to-list 'auto-mode-alist '("\\.mak$" . mako-html-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.mako$" . mako-html-mumamo-mode))

(setq mumamo-chunk-coloring 3)

(add-to-list 'auto-mode-alist '("\\.ml\\w?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;; lua mode
(autoload 'lua-mode "lua-mode" "mode for lua" t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

(autoload 'yaml-mode "yaml-mode" "mode for yaml" t)
(add-to-list 'auto-mode-alist
             '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist
             '("\\.yml$" . yaml-mode))

(autoload 'go-mode "go-mode" "go mode" t)
(add-to-list 'auto-mode-alist
             '("\\.go$" . go-mode))

(autoload 'django-html-mode "django-html-mode" "mode for django templates" t)
(add-to-list 'auto-mode-alist
             '("views$" . django-html-mode))

(setq gdb-show-main nil)
(setq gdb-many-windows t)

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline 'pdb.py
                            (file-name-nondirectory buffer-file-name)))))

(autoload 'nsis-mode "nsis-mode" "NSIS mode" t)

(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Ii]\\)$" .
                                 nsis-mode)) auto-mode-alist))

(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Hh]\\)$" .
                                 nsis-mode)) auto-mode-alist))

(add-hook 'before-save-hook 'whitespace-cleanup)

(autoload 'batch-mode "batch-mode" t)
(add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))
(add-to-list 'auto-mode-alist '("\\.cmd\\'" . batch-mode))

(autoload 'erlang-mode "erlang" "erlang mode" t)

(setq auto-mode-alist
      (append '(("erl\\'" . erlang-mode)) auto-mode-alist))

(autoload 'cmake-mode "cmake-mode" "cmake mode" t)

(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

(setq cmake-mode-hook ())
(add-to-list 'cmake-mode-hook
             '(lambda ()
                (local-set-key [f9] 'cmake-help-command)))

(autoload 'anything "anything" "Anything framework" t)

(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

(setq battery-mode-line-format "[%b%p%%]")

(autoload 'synonyms "synonyms" "thesaurus" t nil)
(setq synonyms-file (make-conf-path "misc/mthesaur.txt"))

(require 'session)

(autoload 'dot-mode "graphiz-dot-mode" "graphviz dot mode" t)

(autoload 'log4j-mode "log4j-mode" t)
(add-to-list 'auto-mode-alist '("\\.log$" . log4j-mode))

;TODO: add support for smart matching of keywords on the log files also
(setq ca-log-files '("boot" "everything" "kernel"))
(setq ca-log-basepath "/var/log/")

(defun ca-view-log-file ()
  "View log files"
  (interactive)
  (let ((to-read (completing-read "which file?\n" ca-log-files)))
    (view-file (concat ca-log-basepath to-read))
    (log4j-mode)))

;;  (autoload 'ledger-mode "ledger" "ledger mode for accounting" t)

;; separator might be different in the different conf mode dialects
(setq ca-conf-section-regexp "\\[.*\\]")

(defun ca-next-conf-section ()
  (interactive)
  (re-search-forward ca-conf-section-regexp))

(defun ca-prev-conf-section ()
  (interactive)
  (re-search-backward ca-conf-section-regexp))

(add-hook 'conf-unix-mode-hook '(lambda ()
                                  (local-set-key (kbd "M-n") 'ca-next-conf-section)
                                  (local-set-key (kbd "M-p") 'ca-prev-conf-section)))

(add-to-list 'auto-mode-alist
             '("\\.pkla\\'" . conf-mode))

(add-to-list 'auto-mode-alist
             '("shorewall" . conf-space-mode))

(add-to-list 'auto-mode-alist
             '("pylintrc" . conf-unix-mode))

(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))

(autoload 'eimp-mode "eimp" "Emacs Image Manipulation Package." t)
(add-hook 'image-mode-hook 'eimp-mode)
(setq eimp-enable-undo t)
(setq eimp-max-concurrent-processes 4)

(load-library "auto-pair")
(autopair-global-mode t)
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(add-hook 'latex-mode-hook
          #'(lambda ()
              (set (make-local-variable 'autopair-handle-action-fns)
                   (list #'autopair-default-handle-action
                         #'autopair-latex-mode-paired-delimiter-action))))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (push '(?< . ?>)
                    (getf autopair-extra-pairs :code))))

(add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))


(require 'command-frequency)
(command-frequency-autosave-mode t)
(setq command-frequency-autosave-timeout 100)
(command-frequency-mode t)

;; also enable all the keys used
(open-dribble-file (expand-file-name "~/.emacs.dribble"))

; set conkeror as the default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")

(provide 'ca-other-modes)
