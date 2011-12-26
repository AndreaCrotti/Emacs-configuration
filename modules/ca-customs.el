;FIXME: move this somewhere else

(defcustom ca-default-closing-char ";"
  "default closing char, change in ca-newline-force-close-alist if needed"
  :group 'ca
  :type 'string)

(defcustom ca-newline-force-close-alist
  '((python-mode . ":")
    (jython-mode . ":")
    (prolog-mode . ".")
    (latex-mode . " \\newline")
    (org-mode . " \\newline")
    (tuareg-mode . ";;")
    (html-mode . " <br>"))
  "Closing char for different modes"
  :group 'ca
  :type 'list)

(defcustom ca-show-battery
  t
  "show the battery level"
  :group 'ca
  :type 'boolean)

(provide 'ca-customs)
