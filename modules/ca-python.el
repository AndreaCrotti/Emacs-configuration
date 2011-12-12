(require 'ca-environment)

(setq
 py-install-directory (make-conf-path "python-mode")
 py-electric-colon-active t)

(load-library (concat py-install-directory "/python-mode"))

(defcustom ca-python-enable-rope t
  "True if rope is enabled"
  :type 'boolean
  :group 'ca)

;FIXME: not used at the moment
(defcustom ca-python-enable-cedet t
  "True if we should use cedet also for python"
  :type 'boolean
  :group 'ca)

; use eval-after-load to set the right keys for python with cedet

(if ca-linux
    ;; TODO: should also check if it's actually in the path and check
    ;; that he automatic settings are also working
    (setq py-shell-name "python2"))

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-to-list 'load-path (make-conf-path "python-mode/test"))
(autoload 'doctest-mode "doctest-mode" "doc test python mode" t)

(defun ca-python-remove-pdb ()
  "Remove the pdb tracking lines"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (delete-matching-lines "import pdb")))

(add-hook 'python-mode-hook
          (lambda ()
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

(when ca-python-enable-rope
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

(provide 'ca-python)
