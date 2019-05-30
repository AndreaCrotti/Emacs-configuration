(defun sql-beautify-region (beg end)
  "Beautify SQL in region between beg and END.
Dependency:
npm i -g sql-formatter-cli"
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "sql-formatter-cli" nil t)))

(defun sql-beautify-buffer ()
  "Beautify SQL in buffer."
  (interactive)
  (sql-beautify-region (point-min) (point-max)))

(add-hook 'sql-mode-hook '(lambda ()
                            ;; beautify region or buffer
                            (local-set-key (kbd "C-c t") 'sql-beautify-region)))

(provide 'ca-sql)
