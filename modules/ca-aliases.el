(require 'ca-utils)

(defalias 'bb 'bury-buffer)
(defalias 'dml 'delete-matching-lines)
(defalias 'eb 'eval-buffer)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'er 'eval-region)
(defalias 'go 'google-search-it)
(defalias 'gr 'ca-grep-in-current)
(defalias 'gs 'google-search-selection)
(defalias 'll 'load-library)
(defalias 'qrs 'query-replace-regexp)
(defalias 'qs 'query-replace)
(defalias 'rs 'replace-string)
(defalias 'spell 'flyspell-mode)
(defalias 'spell-prog 'flyspell-prog-mode)
(defalias 'yes-or-no-p 'y-or-n-p)

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

;; Those below are my favourite themes
(defalias 'black 'color-theme-hober)
(defalias 'blue 'color-theme-deep-blue)
(defalias 'grey 'color-theme-black-on-gray)
(defalias 'blipp 'color-theme-blippblopp)
(defalias 'high 'color-theme-high-contrast)
(defalias 'billw 'color-theme-billw)
(defalias 'coal 'color-theme-charcoal-black)

(defalias 'batt 'display-battery-mode)

(defalias 'jd 'javadoc-lookup)
(defalias 'br 'babel-region-default)

(defalias 'fold 'senator-fold-tag-toggle)

(defalias 'pd 'ca-print-desktop)
(defalias 'dcd 'desktop-change-dir)
(defalias 'gcb 'ca-git-change-branch)

(defalias 'loc 'locate)
(defalias 'ga 'ca-git-add-file)
(defalias 'go 'ca-open-git-files)
(defalias 'co 'ca-occur)

(provide 'ca-aliases)
