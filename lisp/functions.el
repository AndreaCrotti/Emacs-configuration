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

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

(defun redis-ks ()
  (interactive)
  (let ((buffer (apply #'make-comint-in-buffer
                       "redis"
                       redis-process-buffer-name
                       redis-cli-executable
                       nil
                       '("-h"
                         "localhost"
                         "-p"
                         "16378"))))
    (with-current-buffer buffer
      (redis-cli-mode))
    (pop-to-buffer buffer t)))

(defun git-blame-line ()
  "Runs `git blame` on the current line and
   adds the commit id to the kill ring"
  (interactive)
  (let* ((line-number (save-excursion
                        (goto-char (point-at-bol))
                        (+ 1 (count-lines 1 (point)))))
         (line-arg (format "%d,%d" line-number line-number))
         (commit-buf (generate-new-buffer "*git-blame-line-commit*")))
    (call-process "git" nil commit-buf nil
                  "blame" (buffer-file-name) "-L" line-arg)
    (let* ((commit-id (with-current-buffer commit-buf
                        (buffer-substring 1 9)))
           (log-buf (generate-new-buffer "*git-blame-line-log*")))
      (kill-new commit-id)
      (call-process "git" nil log-buf nil
                    "log" "-1" "--pretty=%h   %an   %s" commit-id)
      (with-current-buffer log-buf
        (message "Line %d: %s" line-number (buffer-string)))
      (kill-buffer log-buf))
    (kill-buffer commit-buf)))

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
