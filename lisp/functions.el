;; -*- lexical-binding: t; -*-

(defun ca-next-defun ()
  (interactive)
  (end-of-defun 2)
  (beginning-of-defun 1))

(defun ca-err-switch ()
  "Toggle error debugging."
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error now %s" debug-on-error))

(defun ca-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  ;; (ca-indent-buffer)
  (ca-untabify-buffer)
  (delete-trailing-whitespace))

(defun ca-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

(defun advice-ns-get-selection (original &rest args)
  (let* ((actual-string (apply original args))
         (str-length (length actual-string)))
    (if (> str-length (* 1024 1024))
      (let ((what-do (yes-or-no-p "The string you copied is > 1MB. Do you really want to do that?")))
        (if what-do
          actual-string
          "refusing to paste that massive thing"))
      actual-string)))

(advice-add 'ns-get-selection :around 'advice-ns-get-selection)

(provide 'functions)
