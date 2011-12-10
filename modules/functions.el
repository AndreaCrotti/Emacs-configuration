
(defun ca-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun ca-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun ca-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (ca-indent-buffer)
  (ca-untabify-buffer)
  (delete-trailing-whitespace))

; FIXME: previous-line should be only used as interactive function
(defun ca-ditto ()
  "*Copy contents of previous line, starting at the position above point."
  (interactive)
  (let ((last-command nil))
    (save-excursion
      (previous-line 1)
      (copy-region-as-kill (point) (progn (end-of-line) (point))))
    (yank 1)))
