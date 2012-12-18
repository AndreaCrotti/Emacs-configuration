(require 'ca-customs)

(defun my-get-source-directory (path)
  "Please imlement me. Currently returns `path' inchanged."
  (expand-file-name path))

(defvar ca-anything-file-search
  '((name . "File Search")
    (init
     . (lambda ()
         (setq anything-default-directory
               default-directory)))
    (candidates
     . (lambda ()
         (let ((args
                (format "'%s' \\( -path \\*/.svn \\) -prune -o -iregex '.*%s.*' -print"
                        (my-get-source-directory anything-default-directory)
                        anything-pattern)))
           (start-process-shell-command "file-search-process" nil
                                        "find" args))))
                                        ;TODO: maybe other types as well?
    (type . file)
    (requires-pattern . 2)
    (delayed))
  "Source for searching matching files recursively.")

(add-to-list 'anything-sources 'ca-anything-file-search)
