;; TODO: with emacs23 is sufficient to enable subword-mode probably
(autoload 'camelCase-mode "camelCase-mode")
(defcustom ca-camelCase-modes
  '(python-mode-hook java-mode-hook c-mode-common-hook nesc-mode-hook)
  "Modes where camelizing is allowed"
  :group 'ca
  :type 'list)

(dolist (hook ca-camelCase-modes)
  (add-hook hook 'camelCase-mode))

(require 'eldoc)
;; Maybe better a direct activation??
(dolist (hook '(ruby-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))

(require 'etags-select)

(require 'etags-table)
(setq etags-table-search-up-depth 1)

(defun ca-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (when
          (yes-or-no-p "do you want to enable gtags?")
        (let ((olddir default-directory)
              (topdir (read-directory-name
                       "gtags: top of source tree:" default-directory)))
          (cd topdir)
          (shell-command "gtags && echo 'created tagfile'")
          (cd olddir)) ; restore
        ;;  tagfile already exists; update it
        (shell-command "global -u && echo 'updated tagfile'"))))

(autoload 'po-mode "po-mode+"
  "Major mode for translators to edit PO files" t)

(add-to-list 'auto-mode-alist
             '("\\.po$" . po-mode))

(add-to-list 'auto-mode-alist
             '("\\.pot$" . po-mode))


;; to automatically find out the coding system
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                            'po-find-file-coding-system)
(autoload 'po-find-file-coding-system "po-mode")

(defcustom ca-spell-langs
  '(emacs-lisp-mode-hook python-mode-hook c-mode-common-hook nesc-mode-hook java-mode-hook jde-mode-hook haskell-mode-hook)
  "Set of programming modes for which I want to enable spelling in comments and strings"
  :group 'ca
  :type 'list)

(dolist (lang-hook ca-spell-langs)
  (add-hook  lang-hook 'flyspell-prog-mode))

(require 'auto-complete)
(ac-flyspell-workaround)

(require 'gist)

(autoload 'lisppaste "lisppaste" "lisppaste" t)

(defun ca-kill-compile-buffer-if-successful (buffer string)
  " kill a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "WARNING" nil t)
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      'kill-buffer
                      buffer)))

(add-hook 'compilation-finish-functions 'ca-kill-compile-buffer-if-successful)

;; for changelogs
(setq add-log-always-start-new-record 1)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-format "%02d-%02m-%:y, %02H:%02M")

(add-to-list 'auto-mode-alist '("Doxyfile" . conf-unix-mode))

(add-to-list 'load-path (make-conf-path "doxymacs/lisp"))
;; supporting doxymacs and doxymacs font locking
(add-hook 'c-mode-common-hook
          '(lambda ()
             (require 'doxymacs)
             (doxymacs-mode t)
             (doxymacs-font-lock)))

(defun ca-doxy-path (basepath classname)
  "convert the class name to the format used by doxygen"
  (concat basepath "doc/html/class_" (un-camelcase-string classname "_") ".html"))

(defun ca-jump-to-doxygen-doc (basepath)
  "jump to the corresponding doxygen page"
  (interactive "D")
  (let
      ((fname (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (browse-url (ca-doxy-path basepath fname))))

; TODO: add it globally if possible
(require 'fixme-mode)
; for each of the modes we add it to the hook
(add-to-list 'fixme-modes 'org-mode)
(dolist (hook '(python-mode-hook
                c-mode-common-hook
                ruby-mode-hook
                lisp-interaction-mode-hook
                org-mode-hook
                haskell-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook 'fixme-mode))

(autoload 'gtags-mode "gtags" "gtags mode" t)

(provide 'ca-prog-mode)
