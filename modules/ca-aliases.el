(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'rs 'replace-string)
(defalias 'qs 'query-replace)
(defalias 'qrs 'query-replace-regexp)
(defalias 'ac 'auto-complete-mode)
(defalias 'go 'google-search-it)
(defalias 'gs 'google-search-selection)
(defalias 'spell 'flyspell-mode)
(defalias 'spell-prog 'flyspell-prog-mode)
(defalias 'dml 'delete-matching-lines)
(defalias 'bb 'bury-buffer)
(defalias 'elm 'emacs-lisp-mode)

(defalias 'ys 'yas/reload-all)
(defalias 'yv 'yas/visit-snippet-file)

(defalias 'ascii 'org-export-as-ascii)
(defalias 'html 'org-export-as-html-and-open)
(defalias 'pdf 'org-export-as-pdf-and-open)
(defalias 'box 'comment-box)
(defalias 'rb 'revert-buffer)

(defalias 'sh 'shell)

(defalias 'ws 'whitespace-mode)
(defalias 'bu 'browse-url)

(defalias 'mem 'doxymacs-insert-member-comment)
(defalias 'fun 'doxymacs-insert-function-comment)
(defalias 'file 'doxymacs-insert-file-comment)

;; Those below are my favourite themes
(defalias 'black 'color-theme-hober)
(defalias 'blue 'color-theme-deep-blue)
(defalias 'grey 'color-theme-black-on-gray)
(defalias 'blipp 'color-theme-blippblopp)
(defalias 'high 'color-theme-high-contrast)
(defalias 'billw 'color-theme-billw)
(defalias 'coal 'color-theme-charcoal-black)

(defalias 'batt 'display-battery-mode)

(defun ca-get-some-messages ()
  (interactive)
  (gnus-summary-rescan-group 1000))
;; gnus
(defalias 'gg 'ca-get-some-messages)
(defalias 'jd 'javadoc-lookup)
(defalias 'br 'babel-region-default)
(defalias 'git 'ca-open-git-files)

(defalias 'fold 'senator-fold-tag-toggle)

(defalias 'pd 'ca-print-desktop)
(defalias 'dcd 'desktop-change-dir)
(defalias 'gcb 'ca-git-change-branch)

(defalias 'ga 'ca-git-add-file)
(defalias 'loc 'locate)

(provide 'ca-aliases)
