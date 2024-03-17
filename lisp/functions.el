(defun ca-next-defun ()
  (interactive)
  (end-of-defun 2)
  (beginning-of-defun 1))

(defun ca-prev-defun ()
  (interactive)
  (beginning-of-defun))

(defun ca-err-switch()
  "switch on/off error debugging"
  (interactive)
  (if debug-on-error
      (setq debug-on-error nil)
    (setq debug-on-error t))
  (message "debug-on-error now %s" debug-on-error))

(defun ca-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  ;; (ca-indent-buffer)
  (ca-untabify-buffer)
  ;TODO: use whitespace cleanup instead?
  (delete-trailing-whitespace))

(defun ca-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; TODO: trigger it automatically whenever you are on a data structure??
(defun get-path ()
  "Get the path in a certain data structure"
  (let (path done)
    (save-excursion
      (while (not done)
        (if (and (sp-backward-sexp)
                 (save-excursion (sp-backward-up-sexp)))
            (progn
              (when-let ((thing (thing-at-point 'symbol)))
                (push (substring-no-properties thing) path))
              (sp-backward-up-sexp))
          (setq done t))))
    (message "Path: %s" path)))

(provide 'functions)
