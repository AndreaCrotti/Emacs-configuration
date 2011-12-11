
;; (setq ca-reset-python-test-file "reset_python_test.py")
;; (set-buffer (get-buffer-create ca-reset-python-test-file))
;; (erase-buffer)
;; (when
;;     (featurep 'python-mode)
;;   (unload-feature 'python-mode t))

;; (fundamental-mode)
;; (setq py-python-command-args '("-colors" "Linux"))
;; (kill-buffer ca-reset-python-test-file)

(require 'python-mode)
(setq py-install-directory
      (expand-file-name "~/Emacs-configuration/python-mode"))

(setq py-load-python-mode-pymacs-p nil)
(setq py-electric-colon-active t)

;; (py-load-python-mode-pymacs)
;; (setenv "PYMACS_PYTHON" "python2.7")

;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-confirm-saving 'nil)

(load-library (make-conf-path "python-mode/python-mode"))
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

;TODO: the ca-find-project-root is not python specific
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
                             project-root py-python-command command)))))

(provide 'ca-python)