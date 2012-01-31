(defcustom ca-preferred-reopen-rw-mode "sudo"
  "preferred mode for reopen"
  :type 'string
  :group 'ca
  )

(defcustom ca-dired-git-after-desktop
  nil
  "asking to open a dired buffer every time"
  :group 'ca
  :type 'boolean)

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
  nil
  "show the battery level"
  :group 'ca
  :type 'boolean)

(defcustom ca-spell-langs
  '(emacs-lisp-mode-hook python-mode-hook c-mode-common-hook nesc-mode-hook java-mode-hook jde-mode-hook haskell-mode-hook)
  "Set of programming modes for which I want to enable spelling in comments and strings"
  :group 'ca
  :type 'list)

(defcustom ca-camelCase-modes
  '(python-mode-hook java-mode-hook c-mode-common-hook nesc-mode-hook)
  "Modes where camelizing is allowed"
  :group 'ca
  :type 'list)

(defcustom ca-python-enable-rope t
  "True if rope is enabled"
  :type 'boolean
  :group 'ca)

;FIXME: not used at the moment
(defcustom ca-python-enable-cedet t
  "True if we should use cedet also for python"
  :type 'boolean
  :group 'ca)

(defcustom ca-backend-assoc
  '(('Git . 'magit-status)
    ('Hg . 'hg-status)
    ('Svn . 'svn-status))
  "Mapping between backend and function"
  :type 'list)

;TODO: how do we check if we can check for the existing snippets?
;TODO: make it smarter, it would be good to accept also a function, in this way it can be made more generic
(defcustom ca-auto-header-conses
      '(
        ("setup.py" . "setup")
        ("\.sh$" . "bash")
        ("\.h$"  . "once")
        ("\.hpp$" . "once"))
      "snippets to expand per file extension"
      :group 'ca
      :type 'list)

(defcustom ca-whitespace-ask-modes
  ()
  "Modes where the whitespace cleanup should query the user"
  :type 'list
  :group 'ca)

(defcustom ca-non-whitespaces-modes
      '(makefile-gmake-mode
        makefile-mode
        makefile-bsdmake-mode)
      "Modes that should not run the whitespace cleanup automatically"
      :type 'list
      :group 'ca)

(defcustom ca-command-frequency-enabled
  nil
  "Enable recording the command frequency"
  :type 'boolean
  :group 'ca)

(defcustom ca-extra-conf-files
  '("\\.pkla\\'" "shorewall" "pylintrc" "\\.spec\\'" "etc")
  "Extra list of regexp to enable conf-mode"
  :type 'list
  :group 'ca)


(defcustom ca-conf-section-regexp
  "\\[.*\\]"
  :type 'string
  :group 'ca)

(provide 'ca-customs)
