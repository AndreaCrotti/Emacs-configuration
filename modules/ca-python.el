;; this is because it can be loaded from two different places
;; load-library loads the elc if available

(setq py-install-directory
      (make-conf-path "python-mode"))

(setq py-electric-colon-active t)

(load-library (concat py-install-directory "python-mode"))

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
                             project-root py-python-command command)))))

(provide 'ca-python)

;; these lines where just to make semantic happy
;; (setq ca-reset-python-test-file "reset_python_test.py")
;; (set-buffer (get-buffer-create ca-reset-python-test-file))
;; (erase-buffer)
;; (when
;;     (featurep 'python-mode)
;;   (unload-feature 'python-mode t))

;; (fundamental-mode)
;; (kill-buffer ca-reset-python-test-file)
