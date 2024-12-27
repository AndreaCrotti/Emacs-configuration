(use-package sqlite3)

(use-package sqlformat
  :custom
  (sqlformat-command 'sqlfluff)
  ;; how do we make this smarter?
  (sqlformat-args '("--dialect" "mysql"))
  :hook (sql-interactive-mode . (lambda ()
                                  (toggle-truncate-lines t))))

(use-package ob-sql-mode)
(use-package org-sql)
(use-package sql-indent
  ;; :hook (sql-mode . sqlind-minor-mode)
  )

(provide 'ca-sql)
