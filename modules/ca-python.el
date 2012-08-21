(require 'ca-customs)
(require 'ca-environment)

(setq
 py-install-directory (make-conf-path "python-mode")
 py-electric-colon-active t)

(load-library (concat py-install-directory "/python-mode"))

; use eval-after-load to set the right keys for python with cedet
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'load-path (make-conf-path "python-mode/test"))
(autoload 'doctest-mode "doctest-mode" "doc test python mode" t)

(if ca-linux
    ;; TODO: should also check if it's actually in the path and check
    ;; that he automatic settings are also working
    (setq py-shell-name "python2"))

(setq
 py-smart-indentation t
 )


(defun ca-python-remove-pdb ()
  "Remove the pdb tracking lines"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (delete-matching-lines "import pdb")))

(add-hook 'python-mode-hook
          (lambda ()
            ;TODO: check if it's actually a good idea
            ;; (py-smart-operator-mode-on)
            (local-set-key (kbd "M-D") 'ca-python-remove-pdb)))

;FIXME: not really working yet, and try to make it more generic
(defun ca-python-setup ()
  (interactive)
  (let
      ((project-root (ca-find-project-root))
       (command (completing-read "command to launch: "
                                 '("test" "develop" "bdist_egg")
                                 nil 'confirm nil "test")))
    (with-temp-buffer
      (compilation-mode)
      (shell-command (format "cd %s && %s setup.py %s"
                             project-root py-shell-name command)))))

; taken from http://www.enigmacurry.com/2009/01/21/autocompleteel-python-code-completion-in-emacs/, see https://github.com/EnigmaCurry/emacs/blob/master/ryan-python.el for an updated version
(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")

(defun ca-python-auto-complete ()
  (eval-after-load 'ca-python-auto-complete
    (add-hook 'python-mode-hook
              '(lambda ()
                 (add-to-list 'ac-sources ac-source-rope)))))

(when (and ca-linux ca-python-enable-rope)
  (progn
    ;; pymacs section
    (add-to-list 'load-path (concat py-install-directory "/pymacs"))
    (setenv "PYMACS_PYTHON" "python2.7")
    (require 'pymacs)
    (pymacs-load "ropemacs" "rope-")
    (add-hook 'python-mode-hook
              (lambda ()
                ;FIXME: not taken in the right consideration?
                (local-set-key (kbd "M-/") 'dabbrev-expand)
                (local-set-key [f7] 'rope-find-file)))

    (setq ropemacs-enable-autoimport t)
    ;; (ca-python-auto-complete)
    ))


;TODO: check that this really works
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline 'pdb.py
                            (file-name-nondirectory buffer-file-name)))))


; support for coverage
(add-to-list 'load-path (make-conf-path "pycoverage.el"))
(load-library "pycov2")
(require 'linum)
;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (pycov2-mode)
;;              (linum-mode)))

(provide 'ca-python)
