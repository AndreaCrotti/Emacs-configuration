(require 'ca-customs)

(dolist (hook ca-camelCase-modes)
  (add-hook hook (lambda () (subword-mode t))))

(require 'eldoc)
(dolist (hook ca-eldoc-modes)
  (add-hook hook 'turn-on-eldoc-mode))

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
      (run-with-timer 3 nil
                      'kill-buffer
                      buffer)))

(add-hook 'compilation-finish-functions 'ca-kill-compile-buffer-if-successful)

;; for changelogs
(setq add-log-always-start-new-record 1)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-format "%02d-%02m-%:y, %02H:%02M")

(add-to-list 'auto-mode-alist '("Doxyfile" . conf-unix-mode))

(defun ca-doxy-path (basepath classname)
  "convert the class name to the format used by doxygen"
  (concat basepath "doc/html/class_" (un-camelcase-string classname "_") ".html"))

(defun ca-jump-to-doxygen-doc (basepath)
  "jump to the corresponding doxygen page"
  (interactive "D")
  (let
      ((fname (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (browse-url (ca-doxy-path basepath fname))))

(require 'fixme-mode)

(dolist (hook ca-fixme-mode-hooks)
  (add-hook hook (lambda () (fixme-mode t))))

(autoload 'gtags-mode "gtags" "gtags mode" t)

(add-to-list 'auto-mode-alist '("\\.gdb$" . gdb-script-mode))

(provide 'ca-prog-mode)
