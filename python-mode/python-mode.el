;; python-mode.el --- Major mode for editing Python programs

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2003-2011 https://launchpad.net/python-mode
;;         1995-2002 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: python-mode@python.org
;; Created:    Feb 1992
;; Keywords:   python languages oop

(defconst py-version "5.2.0+"
  "`python-mode' version number.")

;; This file is part of python-mode.el.
;;
;; python-mode.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; python-mode.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with python-mode.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing Python programs.  It was developed by Tim
;; Peters after an original idea by Michael A. Guravage.  Tim subsequently
;; left the net and in 1995, Barry Warsaw inherited the mode.  Tim came back
;; but disavowed all responsibility for the mode.  In fact, we suspect he
;; doesn't even use Emacs any more <wink>.  In 2003, python-mode.el was moved
;; to its own SourceForge project apart from the Python project, and in 2008
;; it was moved to Launchpad for all project administration.  python-mode.el
;; is maintained by the volunteers at the python-mode@python.org mailing
;; list.

;; python-mode.el is different than, and pre-dates by many years, the
;; python.el that comes with FSF Emacs.  We'd like to merge the two modes but
;; have few cycles to do so.  Volunteers are welcome.

;; pdbtrack support contributed by Ken Manheimer, April 2001.  Skip Montanaro
;; has also contributed significantly to python-mode's development.

;; Please use Launchpad to submit bugs or patches:
;;
;;     https://launchpad.net/python-mode

;; INSTALLATION:

;; To install, just drop this file into a directory on your load-path and
;; byte-compile it.  To set up Emacs to automatically edit files ending in
;; ".py" using python-mode, add to your emacs init file
;;
;; GNU Emacs: ~/.emacs, ~/.emacs.el, or ~/.emacs.d/init.el
;;
;; XEmacs: ~/.xemacs/init.el
;;
;; the following code:
;;
;;    (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;;    (setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                       interpreter-mode-alist))
;;    (autoload 'python-mode "python-mode" "Python editing mode." t)
;;
;; In XEmacs syntax highlighting should be enabled automatically.  In GNU
;; Emacs you may have to add these lines to your init file:
;;
;;    (global-font-lock-mode t)
;;    (setq font-lock-maximum-decoration t)

;; BUG REPORTING:

;; As mentioned above, please use the Launchpad python-mode project for
;; submitting bug reports or patches.  The old recommendation, to use C-c C-b
;; will still work, but those reports have a higher chance of getting buried
;; in our inboxes.  Please include a complete, but concise code sample and a
;; recipe for reproducing the bug.  Send suggestions and other comments to
;; python-mode@python.org.

;; When in a Python mode buffer, do a C-h m for more help.  It's doubtful that
;; a texinfo manual would be very useful, but if you want to contribute one,
;; we'll certainly accept it!

;;; ToDo
;; Question calls of `py-guess-indent-offset', as
;; required indent may change throughout the buffer.
;; Rather may it be called by argument


;;; Code:

(require 'comint)
(require 'custom)
(require 'compile)
(require 'ansi-color)
(eval-when-compile (require 'cl))


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defgroup python nil
  "Support for the Python programming language, <http://www.python.org/>"
  :group 'languages
  :prefix "py-")

(defcustom py-tab-always-indent t
  "*Non-nil means TAB in Python mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'python)

(defcustom py-indent-honors-multiline-listing nil
  "If `t', indents to 1+ column of opening delimiter. If `nil', indent adds one level to the beginning of statement. Default is `nil'. "
  :type 'boolean
  :group 'python)

(defcustom py-python-command "python"
  "*Shell command used to start Python interpreter."
  :type 'string
  :group 'python)

(make-obsolete-variable 'py-jpython-command 'py-jython-command nil)
(defcustom py-jython-command "jython"
  "*Shell command used to start the Jython interpreter."
  :type 'string
  :group 'python
  :tag "Jython Command")

(defcustom py-default-interpreter 'cpython
  "*Which Python interpreter is used by default.
The value for this variable can be either `cpython' or `jython'.

When the value is `cpython', the variables `py-python-command' and
`py-python-command-args' are consulted to determine the interpreter
and arguments to use.

When the value is `jython', the variables `py-jython-command' and
`py-jython-command-args' are consulted to determine the interpreter
and arguments to use.

Note that this variable is consulted only the first time that a Python
mode buffer is visited during an Emacs session.  After that, use
\\[py-toggle-shells] to change the interpreter shell."
  :type '(choice (const :tag "Python (a.k.a. CPython)" cpython)
                 (const :tag "Jython" jython))
  :group 'python)

(defcustom py-python-command-args '("-i")
  "*List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python)

(make-obsolete-variable 'py-jpython-command-args 'py-jython-command-args nil)
(defcustom py-jython-command-args '("-i")
  "*List of string arguments to be used when starting a Jython shell."
  :type '(repeat string)
  :group 'python
  :tag "Jython Command Args")

(defcustom py-indent-offset 4
  "*Amount of offset per level of indentation.
`\\[py-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Python code."
  :type 'integer
  :group 'python)
(make-variable-buffer-local 'py-indent-offset)

(defcustom py-backslashed-continuation-indent 2
  "Indent of continuation-lines realised by backslashes. "
  :type 'integer
  :group 'python)
(make-variable-buffer-local 'py-indent-in-delimiter)

(defcustom py-lhs-inbound-indent 1
  "When line starts a multiline-assignement: How many colums indent should be more than opening bracket, brace or parenthesis. "
  :type 'integer
  :group 'python)
(make-variable-buffer-local 'py-lhs-inbound-indent)

(defcustom py-rhs-inbound-indent 1
  "When inside a multiline-assignement: How many colums indent should be more than opening bracket, brace or parenthesis. "
  :type 'integer
  :group 'python)
(make-variable-buffer-local 'py-rhs-inbound-indent)

(defcustom py-continuation-offset 4
  "*Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line. "
  :type 'integer
  :group 'python)

(defcustom py-smart-indentation t
  "*Should `python-mode' try to automagically set some indentation variables?
When this variable is non-nil, two things happen when a buffer is set
to `python-mode':

    1. `py-indent-offset' is guessed from existing code in the buffer.
       Only guessed values between 2 and 8 are considered.  If a valid
       guess can't be made (perhaps because you are visiting a new
       file), then the value in `py-indent-offset' is used.

    2. `indent-tabs-mode' is turned off if `py-indent-offset' does not
       equal `tab-width' (`indent-tabs-mode' is never turned on by
       Python mode).  This means that for newly written code, tabs are
       only inserted in indentation if one tab is one indentation
       level, otherwise only spaces are used.

Note that both these settings occur *after* `python-mode-hook' is run,
so if you want to defeat the automagic configuration, you must also
set `py-smart-indentation' to nil in your `python-mode-hook'."
  :type 'boolean
  :group 'python)

(defcustom py-align-multiline-strings-p t
  "*Flag describing how multi-line triple quoted strings are aligned.
When this flag is non-nil, continuation lines are lined up under the
preceding line's indentation.  When this flag is nil, continuation
lines are aligned to column zero."
  :type '(choice (const :tag "Align under preceding line" t)
                 (const :tag "Align to column zero" nil))
  :group 'python)

(defcustom py-block-comment-prefix "##"
  "*String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
`...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python)

(defcustom py-honor-comment-indentation t
  "*Controls how comment lines influence subsequent indentation.

When nil, all comment lines are skipped for indentation purposes, and
if possible, a faster algorithm is used (i.e. X/Emacs 19 and beyond).

When t, lines that begin with a single `#' are a hint to subsequent
line indentation.  If the previous line is such a comment line (as
opposed to one that starts with `py-block-comment-prefix'), then its
indentation is used as a hint for this line's indentation.  Lines that
begin with `py-block-comment-prefix' are ignored for indentation
purposes.

When not nil or t, comment lines that begin with a single `#' are used
as indentation hints, unless the comment character is in column zero."
  :type '(choice
          (const :tag "Skip all comment lines (fast)" nil)
          (const :tag "Single # `sets' indentation for next line" t)
          (const :tag "Single # `sets' indentation except at column zero"
                 other)
          )
  :group 'python)

(defcustom py-temp-directory
  (let ((ok '(lambda (x)
               (and x
                    (setq x (expand-file-name x)) ; always true
                    (file-directory-p x)
                    (file-writable-p x)
                    x))))
    (or (funcall ok (getenv "TMPDIR"))
        (funcall ok "/usr/tmp")
        (funcall ok "/tmp")
        (funcall ok "/var/tmp")
        (funcall ok  ".")
        (error
         "Couldn't find a usable temp directory -- set `py-temp-directory'")))
  "*Directory used for temporary files created by a *Python* process.
By default, the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, /var/tmp, or the current directory."
  :type 'string
  :group 'python)

(defcustom py-beep-if-tab-change t
  "*Ring the bell if `tab-width' is changed.
If a comment of the form

  \t# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning."
  :type 'boolean
  :group 'python)

(defcustom py-jump-on-exception t
  "*Jump to innermost exception frame in *Python Output* buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame."
  :type 'boolean
  :group 'python)

(defcustom py-ask-about-save t
  "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking."
  :type 'boolean
  :group 'python)

(defcustom py-backspace-function 'backward-delete-char-untabify
  "*Function called by `py-electric-backspace' when deleting backwards."
  :type 'function
  :group 'python)

(defcustom py-delete-function 'delete-char
  "*Function called by `py-electric-delete' when deleting forwards."
  :type 'function
  :group 'python)

(defcustom py-pdbtrack-do-tracking-p t
  "*Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Python* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb."
  :type 'boolean
  :group 'python)
(make-variable-buffer-local 'py-pdbtrack-do-tracking-p)

(defcustom py-pdbtrack-minor-mode-string " PDB"
  "*String to use in the minor mode list when pdbtrack is enabled."
  :type 'string
  :group 'python)

(defcustom py-import-check-point-max
  20000
  "Maximum number of characters to search for a Java-ish import statement.
When `python-mode' tries to calculate the shell to use (either a
CPython or a Jython shell), it looks at the so-called `shebang' line
-- i.e. #! line.  If that's not available, it looks at some of the
file heading imports to see if they look Java-like."
  :type 'integer
  :group 'python
  )

(make-obsolete-variable 'py-jpython-packages 'py-jython-packages nil)
(defcustom py-jython-packages
  '("java" "javax" "org" "com")
  "Imported packages that imply `jython-mode'."
  :type '(repeat string)
  :group 'python)

(defcustom py-current-defun-show  t 
 "If `py-current-defun' should jump to the definition, highlight it while waiting PY-WHICH-FUNC-DELAY seconds, before returning to previous position.

Default is `t'." 

:type 'boolean
:group 'python)

(defcustom py-current-defun-delay  2 
 "When called interactively, `py-current-defun' should wait PY-WHICH-FUNC-DELAY seconds at the definition name found, before returning to previous position. " 

:type 'number
:group 'python)

;; Not customizable
(defvar py-master-file nil
  "If non-nil, execute the named file instead of the buffer's file.
The intent is to allow you to set this variable in the file's local
variable section, e.g.:

    # Local Variables:
    # py-master-file: \"master.py\"
    # End:

so that typing \\[py-execute-buffer] in that buffer executes the named
master file instead of the buffer's file.  If the file name has a
relative path, the value of variable `default-directory' for the
buffer is prepended to come up with a file name.")
(make-variable-buffer-local 'py-master-file)

(defcustom py-pychecker-command "pychecker"
  "*Shell command used to run Pychecker."
  :type 'string
  :group 'python
  :tag "Pychecker Command")

(defcustom py-pychecker-command-args '("--stdlib")
  "*List of string arguments to be passed to pychecker."
  :type '(repeat string)
  :group 'python
  :tag "Pychecker Command Args")

(defvar py-shell-alist
  '(("jython" . 'jython)
    ("python" . 'cpython))
  "*Alist of interpreters and python shells. Used by `py-choose-shell'
to select the appropriate python interpreter mode for a file.")

(defcustom py-shell-input-prompt-1-regexp "^>>> "
  "*A regular expression to match the input prompt of the shell."
  :type 'string
  :group 'python)

(defcustom py-shell-input-prompt-2-regexp "^[.][.][.] "
  "*A regular expression to match the input prompt of the shell after the
  first line of input."
  :type 'string
  :group 'python)

(defcustom py-shell-switch-buffers-on-execute t
  "*Controls switching to the Python buffer where commands are
  executed.  When non-nil the buffer switches to the Python buffer, if
  not no switching occurs."
  :type 'boolean
  :group 'python)

(defcustom py-hide-show-keywords
  '(
    "class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with"
    )
  "*Keywords that can be hidden by hide-show"
  :type '(repeat string)
  :group 'python)

(defcustom py-hide-show-hide-docstrings t
  "*Controls if doc strings can be hidden by hide-show"
  :type 'boolean
  :group 'python)

(defcustom py-mark-decorators nil
  "If py-mark-def-or-class functions should mark decorators too. Default is `nil'. "
  :type 'boolean
  :group 'python)


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defvar py-line-number-offset 0
  "When an exception occurs as a result of py-execute-region, a
subsequent py-up-exception needs the line number where the region
started, in order to jump to the correct file line.  This variable is
set in py-execute-region and used in py-jump-to-exception.")

;; Skip's XE workaround
(unless (functionp 'string-to-syntax)
    (defun string-to-syntax (s)
      (cond
       ((equal s "|") '(15))
       ((equal s "_") '(3))
       (t (error "Unhandled string: %s" s))))
  )

;; GNU's syntax-ppss-context
(unless (functionp 'syntax-ppss-context)
 (defsubst syntax-ppss-context (ppss)
  (cond
   ((nth 3 ppss) 'string)
   ((nth 4 ppss) 'comment)
   (t nil))))

(defvar empty-line-p-chars "^[ \t\r\f]*$"
  "Empty-line-p-chars.")

;;;###autoload
(defun empty-line-p (&optional iact)
  "Returns t if cursor is at an empty line, nil otherwise."
  (interactive "p")
  (save-excursion
    (let ((erg (progn
                 (beginning-of-line)
                 (looking-at empty-line-p-chars))))
      (when iact
        (message "%s" erg))
      erg)))

(defalias 'py-in-list-p 'py-list-beginning-position)
(defun py-list-beginning-position (&optional start)
  "Return lists beginning position, nil if not inside.
Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or start (point-min)))
         (erg (nth 1 (parse-partial-sexp ppstart (point)))))
    (when (interactive-p) (message "%s" erg))
    erg))

;; 2009-09-10 a.roehler@web.de changed section start
;; from python.el, version "22.1"

(defconst py-font-lock-syntactic-keywords
  '(("[^\\]\\\\\\(?:\\\\\\\\\\)*\\(\\s\"\\)\\1\\(\\1\\)"
     (2
      (7)))
    ("\\([RUBrub]?\\)[Rr]?\\(\\s\"\\)\\2\\(\\2\\)"
     (1
      (python-quote-syntax 1))
     (2
      (python-quote-syntax 2))
     (3
      (python-quote-syntax 3)))))

(defun python-quote-syntax (n)
  "Put `syntax-table' property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it's
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as `syntax-ppss' to be correct and it seems to be OK
  ;; to use it here despite initial worries.) We also have to sort
  ;; out a possible prefix -- well, we don't _have_ to, but I think it
  ;; should be treated as part of the string.
  ;; Test cases:
  ;;  ur"""ar""" x='"' # """
  ;; x = ''' """ ' a
  ;; '''
  ;; x '"""' x """ \"""" x
  (save-excursion
    (goto-char (match-beginning 0))
    (cond
     ;; Consider property for the last char if in a fenced string.
     ((= n 3)
      (let* ((font-lock-syntactic-keywords nil)
             (syntax (parse-partial-sexp (point-min) (point))))
        (when (eq t (nth 3 syntax))     ; after unclosed fence
          (goto-char (nth 8 syntax))    ; fence position
          (skip-chars-forward "uUrRbB") ; skip any prefix
          ;; Is it a matching sequence?
          (if (eq (char-after) (char-after (match-beginning 2)))
                (eval-when-compile (string-to-syntax "|"))
            ))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2)                  ; leading quote (not prefix)
               (= (match-beginning 1) (match-end 1))) ; prefix is null
          (and (= n 1)                  ; prefix
               (/= (match-beginning 1) (match-end 1)))) ; non-empty
      (let ((font-lock-syntactic-keywords nil))
        (unless (eq 'string (syntax-ppss-context (parse-partial-sexp (point-min) (point))))
          ;; (eval-when-compile (string-to-syntax "|"))
            (eval-when-compile (string-to-syntax "|"))
          )))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
     )))
(defvar py-mode-syntax-table nil)
(setq py-mode-syntax-table
      (let ((table (make-syntax-table))
            (tablelookup (if (featurep 'xemacs)
                             'get-char-table
                           'aref)))
        ;; Give punctuation syntax to ASCII that normally has symbol
        ;; syntax or has word syntax and isn't a letter.
        (if (featurep 'xemacs)
            (setq table (standard-syntax-table))
          (let ((symbol (string-to-syntax "_"))
                ;; (symbol (string-to-syntax "_"))
                (sst (standard-syntax-table)))
            (dotimes (i 128)
              (unless (= i ?_)
                (if (equal symbol (funcall tablelookup sst i))
                    (modify-syntax-entry i "." table))))))
        (modify-syntax-entry ?$ "." table)
        (modify-syntax-entry ?% "." table)
        ;; exceptions
        (modify-syntax-entry ?# "<" table)
        (modify-syntax-entry ?\n ">" table)
        (modify-syntax-entry ?' "\"" table)
        (modify-syntax-entry ?` "$" table)
        (modify-syntax-entry ?\_ "w" table)
        table))

(defun py-in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (save-restriction
    (widen)
    (let* ((pps (parse-partial-sexp (line-beginning-position) (point)))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (looking-at (concat "^[ \t]*" comment-start-skip))
          (setq erg (point))))
      erg)))

(defun py-in-string-p ()
  "Returns character address of start of comment or string, nil if not inside. "
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (erg (when (nth 3 pps) (nth 8 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\\|'")
                            (forward-char 1)
                            (setq pps (parse-partial-sexp (point-min) (point)))
                            (when (nth 3 pps) (nth 8 pps)))))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defsubst py-in-string-or-comment-p ()
    "Return beginning position if point is in a Python literal (a comment or string)."
    (nth 8 (parse-partial-sexp (point-min) (point))))

(defun py-in-triplequoted-string-p ()
  "Returns character address of start tqs-string, nil if not inside. "
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (erg (when (and (nth 3 pps) (nth 8 pps))(nth 2 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\"\"\\|''''")
                            (goto-char (match-end 0))
                            (setq pps (parse-partial-sexp (point-min) (point)))
                            (when (and (nth 3 pps) (nth 8 pps)) (nth 2 pps)))))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defconst python-space-backslash-table
  (let ((table (copy-syntax-table py-mode-syntax-table)))
    (modify-syntax-entry ?\\ " " table)
    table)
  "`python-mode-syntax-table' with backslash given whitespace syntax.")

;; 2009-09-10 a.roehler@web.de changed section end

;; ;; Face for None, True, False, self, and Ellipsis
(defface py-pseudo-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for pseudo keywords in Python mode, like self, True, False, Ellipsis."
  :group 'python)
(defvar py-pseudo-keyword-face 'py-pseudo-keyword-face)

;; PEP 318 decorators
(defface py-decorators-face
  '((t (:inherit font-lock-keyword-face)))
  "Face method decorators."
  :group 'python)
(defvar py-decorators-face 'py-decorators-face)

;; Face for builtins
(defface py-builtins-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for builtins like TypeError, object, open, and exec."
  :group 'python)
(defvar py-builtins-face 'py-builtins-face)

(defface py-class-name-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for builtins like TypeError, object, open, and exec."
  :group 'python)
(defvar py-class-name-face 'py-class-name-face)

;; XXX, TODO, and FIXME comments and such
(defface py-exception-name-face
  '((t (:inherit font-lock-builtin-face)))
  "."
  :group 'python)
(defvar py-exception-name-face 'py-exception-name-face)

(defvar py-font-lock-keywords
  (let ((kw1 (mapconcat 'identity
                        '("and"      "assert"   "break"     "class"
                          "continue" "def"      "del"       "elif"
                          "else"     "except"   "for"       "from"
                          "global"   "if"       "import"    "in"
                          "is"       "lambda"   "not"       "or"
                          "pass"     "raise"    "as"        "return"
                          "while"    "with"    "yield"
                          )
                        "\\|"))
        (kw2 (mapconcat 'identity
                        '("else:" "except:" "finally:" "try:" "lambda:")
                        "\\|"))
        (kw3 (mapconcat 'identity
                        ;; Don't include Ellipsis in this list, since it is
                        ;; already defined as a pseudo keyword.
                        '("__debug__"
                          "__import__" "__name__" "abs" "all" "any" "apply"
                          "basestring" "bin" "bool" "buffer" "bytearray"
                          "callable" "chr" "classmethod" "cmp" "coerce"
                          "compile" "complex" "copyright" "credits"
                          "delattr" "dict" "dir" "divmod" "enumerate" "eval"
                          "exec" "execfile" "exit" "file" "filter" "float"
                          "format" "getattr" "globals" "hasattr" "hash" "help"
                          "hex" "id" "input" "int" "intern" "isinstance"
                          "issubclass" "iter" "len" "license" "list" "locals"
                          "long" "map" "max" "memoryview" "min" "next"
                          "object" "oct" "open" "ord" "pow" "print" "property"
                          "quit" "range" "raw_input" "reduce" "reload" "repr"
                          "round" "set" "setattr" "slice" "sorted"
                          "staticmethod" "str" "sum" "super" "tuple" "type"
                          "unichr" "unicode" "vars" "xrange" "zip")
                        "\\|"))
        (kw4 (mapconcat 'identity
                        ;; Exceptions and warnings
                        '("ArithmeticError" "AssertionError"
                          "AttributeError" "BaseException" "BufferError"
                          "BytesWarning" "DeprecationWarning" "EOFError"
                          "EnvironmentError" "Exception"
                          "FloatingPointError" "FutureWarning" "GeneratorExit"
                          "IOError" "ImportError" "ImportWarning"
                          "IndentationError" "IndexError"
                          "KeyError" "KeyboardInterrupt" "LookupError"
                          "MemoryError" "NameError" "NotImplemented"
                          "NotImplementedError" "OSError" "OverflowError"
                          "PendingDeprecationWarning" "ReferenceError"
                          "RuntimeError" "RuntimeWarning" "StandardError"
                          "StopIteration" "SyntaxError" "SyntaxWarning"
                          "SystemError" "SystemExit" "TabError" "TypeError"
                          "UnboundLocalError" "UnicodeDecodeError"
                          "UnicodeEncodeError" "UnicodeError"
                          "UnicodeTranslateError" "UnicodeWarning"
                          "UserWarning" "ValueError" "Warning"
                          "ZeroDivisionError")
                        "\\|"))
        )
    (list
     ;; decorators
     '("^[ \t]*\\(@[a-zA-Z_][a-zA-Z_0-9.]+\\)\\((.+)\\)?" 1 'py-decorators-face)
     ;; keywords
     (cons (concat "\\<\\(" kw1 "\\)\\>[ \n\t(]") 1)
     ;; builtins when they don't appear as object attributes
     (list (concat "\\([^. \t]\\|^\\)[ \t]*\\<\\(" kw3 "\\)\\>[ \n\t(]") 2
           'py-builtins-face)
     ;; block introducing keywords with immediately following colons.
     ;; Yes "except" is in both lists.
     (cons (concat "\\<\\(" kw2 "\\)[ \n\t(]") 1)
     ;; Exceptions
     (list (concat "\\<\\(" kw4 "\\)[ \n\t:,()]") 1 'py-exception-name-face)
     ;; raise stmts
     '("\\<raise[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_.]*\\)" 1 py-exception-name-face)
     ;; except clauses
     '("\\<except[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_.]*\\)" 1 py-exception-name-face)
     ;; classes
     '("\\<class[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)" 1 py-class-name-face)
     ;; functions
     '("\\<def[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     ;; pseudo-keywords
     '("\\<\\(self\\|cls\\|Ellipsis\\|True\\|False\\|None\\)\\>"
       1 py-pseudo-keyword-face)
     ;; XXX, TODO, and FIXME tags
     '("XXX\\|TODO\\|FIXME" 0 py-XXX-tag-face t)
     ;; special marking for string escapes and percent substitutes;
     ;; loops adapted from lisp-mode in font-lock.el
     ;; '((lambda (bound)
     ;;     (catch 'found
     ;;       (while (re-search-forward
     ;;               (concat
     ;;                "\\(\\\\\\\\\\|\\\\x..\\|\\\\u....\\|\\\\U........\\|"
     ;;                "\\\\[0-9][0-9]*\\|\\\\[abfnrtv\"']\\)") bound t)
     ;;         (let ((face (get-text-property (1- (point)) 'face)))
     ;;           (when (or (and (listp face) (memq 'font-lock-string-face face))
     ;;                     (eq 'font-lock-string-face face))
     ;;             (throw 'found t))))))
     ;;   (1 'font-lock-regexp-grouping-backslash prepend))
     ;; '((lambda (bound)
     ;;     (catch 'found
     ;;       (while (re-search-forward "\\(%[^(]\\|%([^)]*).\\)" bound t)
     ;;         (let ((face (get-text-property (1- (point)) 'face)))
     ;;           (when (or (and (listp face) (memq 'font-lock-string-face face))
     ;;                     (eq 'font-lock-string-face face))
     ;;             (throw 'found t))))))
     ;;   (1 'font-lock-regexp-grouping-construct prepend))
     ))
  "Additional expressions to highlight in Python mode.")


;; have to bind py-file-queue before installing the kill-emacs-hook
(defvar py-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar py-pdbtrack-is-tracking-p nil)

(defvar py-pychecker-history nil)


;; Constants
(defconst py-assignement-re "\\<\\w+\\>[ \t]*\\(=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)"
  "If looking at the beginning of an assignement. ")

(defconst py-block-re "[ \t]*\\<\\(class\\|def\\|for\\|if\\|try\\|while\\|with\\)\\>"
  "Matches the beginning of a class, method or compound statement. ")

(defconst py-return-re
  "[ \t]*\\<\\(return\\)\\>"
  "Regular expression matching keyword which typically closes a function. ")

(defconst py-class-re "[ \t]*\\<\\(class\\)\\>"
  "Matches the beginning of a class definition. ")

(defconst py-def-or-class-re "[ \t]*\\<\\(def\\|class\\)\\>"
  "Matches the beginning of a class- or functions definition. ")

(defconst py-def-re "[ \t]*\\<\\(def\\)\\>"
  "Matches the beginning of a functions definition. ")

(defconst py-if-clause-re "[ \t]*\\<\\elif\\>"
  "Matches the beginning of a compound statement's clause. ")

(defconst py-try-clause-re
  (concat "\\(" (mapconcat 'identity
                           '("except\\(\\s +.*\\)?:"
                             "finally:")
                           "\\|")
          "\\)")
  "Matches the beginning of a try-statement's clause. ")

(defconst py-if-block-re "[ \t]*\\<if\\>"
  "Matches the beginning of a compound statement saying `if'. ")

(defconst py-try-block-re "[ \t]*\\<try\\>"
  "Matches the beginning of a compound statement saying `try'. " )

(defconst py-stringlit-re
  (concat
   ;; These fail if backslash-quote ends the string (not worth
   ;; fixing?).  They precede the short versions so that the first two
   ;; quotes don't look like an empty short string.
   ;;
   ;; (maybe raw), long single quoted triple quoted strings (SQTQ),
   ;; with potential embedded single quotes
   "[rRuUbB]?'''[^']*\\(\\('[^']\\|''[^']\\)[^']*\\)*'''"
   "\\|"
   ;; (maybe raw), long double quoted triple quoted strings (DQTQ),
   ;; with potential embedded double quotes
   "[rRuUbB]?\"\"\"[^\"]*\\(\\(\"[^\"]\\|\"\"[^\"]\\)[^\"]*\\)*\"\"\""
   "\\|"
   "[rRuUbB]?'\\([^'\n\\]\\|\\\\.\\)*'"     ; single-quoted
   "\\|"                                    ; or
   "[rRuUbB]?\"\\([^\"\n\\]\\|\\\\.\\)*\""  ; double-quoted
   )
  "Regular expression matching a Python string literal.")

(defconst py-continued-re
  ;; This is tricky because a trailing backslash does not mean
  ;; continuation if it's in a comment
  (concat
   "\\(" "[^#'\"\n\\]" "\\|" py-stringlit-re "\\)*"
   "\\\\$")
  "Regular expression matching Python backslash continuation lines.")

(defconst py-blank-or-comment-re "[ \t]*\\($\\|#\\)"
  "Regular expression matching a blank or comment line.")

(defconst py-block-or-clause-re "[ \t]*\\<\\(if\\|else\\|elif\\|while\\|for\\|def\\|class\\|try\\|except\\|finally\\|with\\)\\>"
  "Matches the beginning of a compound statement or it's clause. ")

(defconst py-clause-re "[ \t]*\\<\\(else\\|except\\|finally\\|elif\\)\\>"
  "Matches the beginning of a compound statement's clause. ")

(defconst py-block-closing-keywords-re
  "\\(return\\|raise\\|break\\|continue\\|pass\\)"
  "Regular expression matching keywords which typically close a block.")

(defconst py-no-outdent-re
  (concat
   "\\("
   (mapconcat 'identity
              (list "try:"
                    "except\\(\\s +.*\\)?:"
                    "while\\s +.*:"
                    "for\\s +.*:"
                    "if\\s +.*:"
                    "elif\\s +.*:"
                    (concat py-block-closing-keywords-re "[ \t\n]")
                    )
              "\\|")
          "\\)")
  "Regular expression matching lines not to dedent after.")

(defconst py-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")

;; pdbtrack constants
(defconst py-pdbtrack-stack-entry-regexp
;  "^> \\([^(]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_]+\\)()"
  "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression pdbtrack uses to find a stack trace entry.")

(defconst py-pdbtrack-input-prompt "\n[(<]*[Pp]db[>)]+ "
  "Regular expression pdbtrack uses to recognize a pdb prompt.")

(defconst py-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")


;; Major mode boilerplate

;; define a mode-specific abbrev table for those who use such things
(defvar python-mode-abbrev-table nil
  "Abbrev table in use in `python-mode' buffers.")
(define-abbrev-table 'python-mode-abbrev-table nil)

(defcustom python-mode-hook nil
  "Hook run when entering Python mode."
  :group 'python
  :type 'hook)

(custom-add-option 'python-mode-hook 'imenu-add-menubar-index)
(custom-add-option 'python-mode-hook
		   (lambda ()
		     "Turn off Indent Tabs mode."
		     (setq indent-tabs-mode nil)))
(custom-add-option 'python-mode-hook 'turn-on-eldoc-mode)
(custom-add-option 'python-mode-hook 'abbrev-mode)
;; (custom-add-option 'python-mode-hook 'python-setup-brm)

(make-obsolete-variable 'jpython-mode-hook 'jython-mode-hook nil)
(defvar jython-mode-hook nil
  "*Hook called by `jython-mode'. `jython-mode' also calls
`python-mode-hook'.")

(defvar py-shell-hook nil
  "*Hook called by `py-shell'.")

;; In previous version of python-mode.el, the hook was incorrectly
;; called py-mode-hook, and was not defvar'd.  Deprecate its use.
(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'py-mode-hook 'python-mode-hook nil))

(defvar py-mode-map
  (let ((map (make-sparse-keymap)))
    ;; electric keys
    (define-key map ":" 'py-electric-colon)
    ;; indentation level modifiers
    (define-key map "\C-c\C-l"  'py-shift-region-left)
    (define-key map "\C-c\C-r"  'py-shift-region-right)
    (define-key map "\C-c<"     'py-shift-region-left)
    (define-key map "\C-c>"     'py-shift-region-right)
    ;; subprocess commands
    (define-key map "\C-c\C-c"  'py-execute-buffer)
    (define-key map "\C-c\C-m"  'py-execute-import-or-reload)
    (define-key map "\C-c\C-s"  'py-execute-string)
    (define-key map "\C-c|"     'py-execute-region)
    (define-key map "\e\C-x"    'py-execute-def-or-class)
    (define-key map "\C-c!"     'py-shell)
    (define-key map "\C-c\C-t"  'py-toggle-shells)
    (define-key map [?\C-c ?\d] 'py-hungry-delete-backwards)
    (define-key map [?\C-c ?\C-\d] 'py-hungry-delete-backwards)
    (define-key map [?\C-c deletechar] 'py-hungry-delete-forward)
    (if (not (boundp 'delete-key-deletes-forward))
        (define-key map "\177" 'py-electric-backspace)
      (define-key map [delete]    'py-electric-delete)
      (define-key map [backspace] 'py-electric-backspace))
    (define-key map [(control meta h)] 'py-mark-def-or-class)
    (define-key map "\C-c\C-k"  'py-mark-block-or-clause)
    ;; Miscellaneous
    (define-key map "\C-c:"     'py-guess-indent-offset)
    (define-key map "\C-c\t"    'py-indent-region)
    (define-key map "\C-c\C-d"  'py-pdbtrack-toggle-stack-tracking)
    (define-key map "\C-c\C-f"  'py-sort-imports)
    (define-key map "\C-c\C-n"  'py-end-of-statement)
    (define-key map "\C-c\C-a"  'py-mark-statement)
    (define-key map "\C-c\C-p"  'py-beginning-of-statement)
    (define-key map "\C-c\C-u"  'py-beginning-of-block)
    (define-key map "\C-c\C-q"  'py-end-of-block)
    (define-key map "\C-c#"     'py-comment-region)
    (define-key map "\C-c?"     'py-describe-mode)
    (define-key map "\C-c\C-e"  'py-help-at-point)
    (define-key map "\e\C-a"    'py-beginning-of-def-or-class)
    (define-key map "\e\C-e"    'py-end-of-def-or-class)
    (define-key map "\C-c-"     'py-up-exception)
    (define-key map "\C-c="     'py-down-exception)
    ;; stuff that is `standard' but doesn't interface well with
    ;; python-mode, which forces us to rebind to special commands
    (define-key map "\C-xnd"    'py-narrow-to-defun)
    ;; information
    (define-key map "\C-c\C-b" 'py-submit-bug-report)
    (define-key map "\C-c\C-b" 'py-submit-bug-report)
    (define-key map "\C-c\C-v" 'py-version)
    (define-key map "\C-c\C-w" 'py-pychecker-run)
    ;; shadow global bindings for newline-and-indent w/ the py- version.
    (mapc #'(lambda (key)
              (define-key map key 'py-newline-and-indent))
          (where-is-internal 'newline-and-indent))
    ;; Most Pythoneers expect RET to do a `py-newline-and-indent' 
    (define-key map "\C-m" 'py-newline-and-indent)
    (define-key map [(control return)] 'py-newline-and-dedent)
    (easy-menu-define py-menu map "Python Mode menu"
      `("Python"
	:help "Python-specific Features"
	["Shift region left" py-shift-left :active mark-active
	 :help "Shift by a single indentation step"]
	["Shift region right" py-shift-right :active mark-active
	 :help "Shift by a single indentation step"]
	"-"
	["Mark block" py-mark-block
	 :help "Mark innermost block around point"]
	["Mark def/class" mark-defun
	 :help "Mark innermost definition around point"]
	"-"
	["Start of block" py-beginning-of-block
	 :help "Go to start of innermost definition around point"]
	["End of block" py-end-of-block
	 :help "Go to end of innermost definition around point"]
	["Start of def/class" beginning-of-defun
	 :help "Go to start of innermost definition around point"]
	["End of def/class" end-of-defun
	 :help "Go to end of innermost definition around point"]
	"-"
	("Templates..."
	 :help "Expand templates for compound statements"
	 :filter (lambda (&rest junk)
                   (abbrev-table-menu python-mode-abbrev-table)))
	"-"
	["Start interpreter" py-shell
	 :help "Run `inferior' Python in separate buffer"]
	["Import/reload file" py-load-file
	 :help "Load into inferior Python session"]
	["Eval buffer" py-execute-buffer
	 :help "Evaluate buffer en bloc in inferior Python session"]
	["Eval region" py-execute-region :active mark-active
	 :help "Evaluate region en bloc in inferior Python session"]
	["Eval def/class" py-execute-defun
	 :help "Evaluate current definition in inferior Python session"]
	["Switch to interpreter" py-switch-to-python
	 :help "Switch to inferior Python buffer"]
	["Set default process" py-set-proc
	 :help "Make buffer's inferior process the default"
	 :active (buffer-live-p py-buffer)]
	["Check file" py-check :help "Run pychecker"]
	["Debugger" pdb :help "Run pdb under GUD"]
	"-"
	["Help on symbol" py-describe-symbol
	 :help "Use pydoc on symbol at point"]
	["Complete symbol" completion-at-point
	 :help "Complete (qualified) symbol before point"]
	["Find function" py-find-function
	 :help "Try to find source definition of function at point"]
	["Update imports" py-find-imports
	 :help "Update list of top-level imports for completion"]))
    map))

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")
(if py-mode-output-map
    nil
  (setq py-mode-output-map (make-sparse-keymap))
  (define-key py-mode-output-map [button2]  'py-mouseto-exception)
  (define-key py-mode-output-map "\C-c\C-c" 'py-goto-exception)
  ;; TBD: Disable all self-inserting keys.  This is bogus, we should
  ;; really implement this as *Python Output* buffer being read-only
  (mapc #' (lambda (key)
             (define-key py-mode-output-map key
               #'(lambda () (interactive) (beep))))
           (where-is-internal 'self-insert-command))
  )

(defvar py-shell-map nil
  "Keymap used in *Python* shell buffers.")
(if py-shell-map
    nil
  (setq py-shell-map (copy-keymap comint-mode-map))
  (define-key py-shell-map [tab]   'tab-to-tab-stop)
  (define-key py-shell-map "\C-c-" 'py-up-exception)
  (define-key py-shell-map "\C-c=" 'py-down-exception)
  )

;; (when (featurep 'xemacs) (defvar py-mode-syntax-table nil))
;; (when (featurep 'xemacs)
;;   (when (not py-mode-syntax-table)
;;     (setq py-mode-syntax-table (make-syntax-table))
;;     (modify-syntax-entry ?\( "()" py-mode-syntax-table)
;;     (modify-syntax-entry ?\) ")(" py-mode-syntax-table)
;;     (modify-syntax-entry ?\[ "(]" py-mode-syntax-table)
;;     (modify-syntax-entry ?\] ")[" py-mode-syntax-table)
;;     (modify-syntax-entry ?\{ "(}" py-mode-syntax-table)
;;     (modify-syntax-entry ?\} "){" py-mode-syntax-table)
;;     ;; Add operator symbols misassigned in the std table
;;     (modify-syntax-entry ?\$ "."  py-mode-syntax-table)
;;     (modify-syntax-entry ?\% "."  py-mode-syntax-table)
;;     (modify-syntax-entry ?\& "."  py-mode-syntax-table)
;;     (modify-syntax-entry ?\* "."  py-mode-syntax-table)
;;     (modify-syntax-entry ?\+ "."  py-mode-syntax-table)
;;     (modify-syntax-entry ?\- "."  py-mode-syntax-table)
;;     (modify-syntax-entry ?\/ "."  py-mode-syntax-table)
;;     (modify-syntax-entry ?\< "."  py-mode-syntax-table)
;;     (modify-syntax-entry ?\= "."  py-mode-syntax-table)
;;     (modify-syntax-entry ?\> "."  py-mode-syntax-table)
;;     (modify-syntax-entry ?\| "."  py-mode-syntax-table)
;;     ;; For historical reasons, underscore is word class instead of
;;     ;; symbol class.  GNU conventions say it should be symbol class, but
;;     ;; there's a natural conflict between what major mode authors want
;;     ;; and what users expect from `forward-word' and `backward-word'.
;;     ;; Guido and I have hashed this out and have decided to keep
;;     ;; underscore in word class.  If you're tempted to change it, try
;;     ;; binding M-f and M-b to py-forward-into-nomenclature and
;;     ;; py-backward-into-nomenclature instead.  This doesn't help in all
;;     ;; situations where you'd want the different behavior
;;     ;; (e.g. backward-kill-word).
;;     (modify-syntax-entry ?\_ "w"  py-mode-syntax-table)
;;     ;; Both single quote and double quote are string delimiters
;;     (modify-syntax-entry ?\' "\"" py-mode-syntax-table)
;;     (modify-syntax-entry ?\" "|" py-mode-syntax-table)
;;     ;; backquote is open and close paren
;;     (modify-syntax-entry ?\` "$"  py-mode-syntax-table)
;;     ;; comment delimiters
;;     (modify-syntax-entry ?\# "<"  py-mode-syntax-table)
;;     (modify-syntax-entry ?\n ">"  py-mode-syntax-table)))

;; An auxiliary syntax table which places underscore and dot in the
;; symbol class for simplicity
(defvar py-dotted-expression-syntax-table nil
  "Syntax table used to identify Python dotted expressions.")
(when (not py-dotted-expression-syntax-table)
  (setq py-dotted-expression-syntax-table
        (copy-syntax-table py-mode-syntax-table))
  (modify-syntax-entry ?_ "_" py-dotted-expression-syntax-table)
  (modify-syntax-entry ?. "_" py-dotted-expression-syntax-table))


;; Utilities
(defsubst py-keep-region-active ()
  "Keep the region active in XEmacs."
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently; its policy doesn't require us
  ;; to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defsubst py-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol  -- beginning of line
  eol  -- end of line
  bod  -- beginning of def or class
  eod  -- end of def or class
  bob  -- beginning of buffer
  eob  -- end of buffer
  boi  -- back to indentation
  bos  -- beginning of statement

This function does not modify point or mark."
  (let ((here (point)))
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'eol) (end-of-line))
     ((eq position 'bod) (py-beginning-of-def-or-class 'either))
     ((eq position 'eod) (py-end-of-def-or-class 'either))
     ;; Kind of funny, I know, but useful for py-up-exception.
     ((eq position 'bob) (goto-char (point-min)))
     ((eq position 'eob) (goto-char (point-max)))
     ((eq position 'boi) (back-to-indentation))
     ((eq position 'bos) (py-goto-initial-line))
     (t (error "Unknown buffer position requested: %s" position))
     )
    (prog1
        (point)
      (goto-char here))))

(when (featurep 'xemacs)
  (defsubst py-highlight-line (from to file line)
    (cond
     ((fboundp 'make-extent)
      ;; XEmacs
      (let ((e (make-extent from to)))
        (set-extent-property e 'mouse-face 'highlight)
        (set-extent-property e 'py-exc-info (cons file line))
        (set-extent-property e 'keymap py-mode-output-map)))
     (t
      ;; Emacs -- Please port this!
      )
     )))


;;;; Imenu.

;; For possibily speeding this up, here's the top of the ELP profile
;; for rescanning pydoc.py (2.2k lines, 90kb):
;; Function Name                         Call Count  Elapsed Time  Average Time
;; ====================================  ==========  =============  ============
;; python-imenu-create-index             156         2.430906      0.0155827307
;; python-end-of-defun                   155         1.2718260000  0.0082053290
;; python-end-of-block                   155         1.1898689999  0.0076765741
;; python-next-statement                 2970        1.024717      0.0003450225
;; python-end-of-statement               2970        0.4332190000  0.0001458649
;; python-beginning-of-defun             265         0.0918479999  0.0003465962
;; python-skip-comments/blanks           3125        0.0753319999  2.410...e-05

(defvar python-recursing)
;; (defun python-imenu-create-index ()
(defun py-imenu-create-index ()
    "`imenu-create-index-function' for Python.

Makes nested Imenu menus from nested `class' and `def' statements.
The nested menus are headed by an item referencing the outer
definition; it has a space prepended to the name so that it sorts
first with `imenu--sort-by-name' (though, unfortunately, sub-menus
precede it)."
  (unless (boundp 'python-recursing)	; dynamically bound below
    ;; Normal call from Imenu.
    (goto-char (point-min))
    ;; Without this, we can get an infloop if the buffer isn't all
    ;; fontified.  I guess this is really a bug in syntax.el.  OTOH,
    ;; _with_ this, imenu doesn't immediately work; I can't figure out
    ;; what's going on, but it must be something to do with timers in
    ;; font-lock.
    ;; This can't be right, especially not when jit-lock is not used.  --Stef
    ;; (unless (get-text-property (1- (point-max)) 'fontified)
    ;;   (font-lock-fontify-region (point-min) (point-max)))
    )
  (let (index-alist)			; accumulated value to return
    (while (re-search-forward
	    (rx line-start (0+ space)	; leading space
		(or (group "def") (group "class"))	   ; type
		(1+ space) (group (1+ (or word ?_))))	   ; name
	    nil t)
      (unless (py-in-string-or-comment-p)
	(let ((pos (match-beginning 0))
	      (name (match-string-no-properties 3)))
	  (if (match-beginning 2)	; def or class?
	      (setq name (concat "class " name)))
	  (save-restriction
	    (narrow-to-defun)
	    (let* ((python-recursing t)
;; 		   (sublist (python-imenu-create-index)))
	           (sublist (py-imenu-create-index)))
              (if sublist
		  (progn (push (cons (concat " " name) pos) sublist)
			 (push (cons name sublist) index-alist))
		(push (cons name pos) index-alist)))))))
    (unless (boundp 'python-recursing)
      ;; Look for module variables.
      (let (vars)
	(goto-char (point-min))
	(while (re-search-forward
		(rx line-start (group (1+ (or word ?_))) (0+ space) "=")
		nil t)
	  (unless (py-in-string-or-comment-p)
	    (push (cons (match-string 1) (match-beginning 1))
		  vars)))
	(setq index-alist (nreverse index-alist))
	(if vars
	    (push (cons "Module variables"
			(nreverse vars))
		  index-alist))))
    index-alist))




(defun py-choose-shell-by-shebang ()
  "Choose CPython or Jython mode by looking at #! on the first line.
Returns the appropriate mode function.
Used by `py-choose-shell', and similar to but distinct from
`set-auto-mode', though it uses `auto-mode-interpreter-regexp' (if available)."
  ;; look for an interpreter specified in the first line
  ;; similar to set-auto-mode (files.el)
  (let* ((re (if (boundp 'auto-mode-interpreter-regexp)
                 auto-mode-interpreter-regexp
               ;; stolen from Emacs 21.2
               "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)"))
         (interpreter (save-excursion
                        (goto-char (point-min))
                        (if (looking-at re)
                            (match-string 2)
                          "")))
         elt)
    ;; Map interpreter name to a mode.
    (setq elt (assoc (file-name-nondirectory interpreter)
                     py-shell-alist))
    (and elt (caddr elt))))


(defun py-choose-shell-by-import ()
  "Choose CPython or Jython mode based imports.
If a file imports any packages in `py-jython-packages', within
`py-import-check-point-max' characters from the start of the file,
return `jython', otherwise return nil."
  (let (mode)
    (save-excursion
      (goto-char (point-min))
      (while (and (not mode)
                  (search-forward-regexp
                   "^\\(\\(from\\)\\|\\(import\\)\\) \\([^ \t\n.]+\\)"
                   py-import-check-point-max t))
        (setq mode (and (member (match-string 4) py-jython-packages)
                        'jython
                        ))))
    mode))


(defun py-choose-shell ()
  "Choose CPython or Jython mode. Returns the appropriate mode function.
This does the following:
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py-choose-shell-by-import'
 - default to the variable `py-default-interpreter'"
  (interactive)
  (or (py-choose-shell-by-shebang)
      (py-choose-shell-by-import)
      py-default-interpreter
;      'cpython ;; don't use to py-default-interpreter, because default
;               ;; is only way to choose CPython
      ))

(defvar py-which-shell nil)
;;;###autoload
(define-derived-mode python-mode fundamental-mode "Python"
  "Major mode for editing Python files.
To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{py-mode-map}
VARIABLES

py-indent-offset\t\tindentation increment
py-block-comment-prefix\t\tcomment string used by `comment-region'
py-python-command\t\tshell command to invoke Python interpreter
py-temp-directory\t\tdirectory used for temp files (if needed)
py-beep-if-tab-change\t\tring the bell if `tab-width' is changed"
  (set (make-local-variable 'font-lock-defaults)
       '(py-font-lock-keywords nil nil nil nil
                               (font-lock-syntactic-keywords
                                . py-font-lock-syntactic-keywords)))
  (set (make-local-variable 'paragraph-separate) "^[ \t]*$")
  (set (make-local-variable 'paragraph-start) "^[ \t]*$" )
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "# *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) 'py-comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  (set (make-local-variable 'add-log-current-defun-function) 'py-current-defun)
  (set (make-local-variable 'fill-paragraph-function) 'py-fill-paragraph)
  (set-syntax-table py-mode-syntax-table)
  (use-local-map py-mode-map)
  ;; Emacs 19 requires this
  (if (boundp 'comment-multi-line)
      (setq comment-multi-line nil))
  (set (make-local-variable 'skeleton-further-elements)
       '((< '(backward-delete-char-untabify (min py-indent-offset
						 (current-column))))
	 (^ '(- (1+ (current-indentation))))))
  ;; Add support for HideShow
  (add-to-list 'hs-special-modes-alist (list
                                        'python-mode (concat (if py-hide-show-hide-docstrings "^\\s-*\"\"\"\\|" "") (mapconcat 'identity (mapcar #'(lambda (x) (concat "^\\s-*" x "\\>")) py-hide-show-keywords) "\\|")) nil "#"
                                        (lambda (arg)
                                          (py-goto-beyond-block)
                                          (skip-chars-backward " \v\f\t\n"))
                                        nil))
    ;; Now do the automagical guessing
  (if py-smart-indentation
      (let ((offset py-indent-offset))
        ;; It's okay if this fails to guess a good value
        (if (and (ignore-errors (py-guess-indent-offset))
                 (<= py-indent-offset 8)
                 (>= py-indent-offset 2))
            (setq offset py-indent-offset))
        (setq py-indent-offset offset)
        ;; Only turn indent-tabs-mode off if tab-width !=
        ;; py-indent-offset.  Never turn it on, because the user must
        ;; have explicitly turned it off.
        (if (/= tab-width py-indent-offset)
            (setq indent-tabs-mode nil))))
  ;; Set the default shell if not already set
  (unless py-which-shell
    (py-toggle-shells (py-choose-shell)))
  (when (interactive-p) (message "python-mode loaded from: %s" "python-mode.el"))
  )

(make-obsolete 'jpython-mode 'jython-mode nil)
(defun jython-mode ()
  "Major mode for editing Jython/Jython files.
This is a simple wrapper around `python-mode'.
It runs `jython-mode-hook' then calls `python-mode.'
It is added to `interpreter-mode-alist' and `py-choose-shell'.
"
  (interactive)
  (python-mode)
  (py-toggle-shells 'jython)
  (when jython-mode-hook
      (run-hooks 'jython-mode-hook)))

;; It's handy to add recognition of Python files to the
;; interpreter-mode-alist and to auto-mode-alist.  With the former, we
;; can specify different `derived-modes' based on the #! line, but
;; with the latter, we can't.  So we just won't add them if they're
;; already added.
;;;###autoload
(let ((modes '(("jython" . jython-mode)
               ("python" . python-mode)
               ("python3" . python-mode)
               )))
  (while modes
    (when (not (assoc (car modes) interpreter-mode-alist))
      (push (car modes) interpreter-mode-alist))
    (setq modes (cdr modes))))
;;;###autoload
(when (not (or (rassq 'python-mode auto-mode-alist)
               (rassq 'jython-mode auto-mode-alist)))
  (push '("\\.py$" . python-mode) auto-mode-alist))


;; electric characters
(defun py-outdent-p ()
  "Returns non-nil if the current line should dedent one level."
  (save-excursion
    (and (progn (back-to-indentation)
                (looking-at py-clause-re))
         ;; short circuit infloop on illegal construct
         (not (bobp))
         (progn (forward-line -1)
                (py-goto-initial-line)
                (back-to-indentation)
                (when (looking-at py-blank-or-comment-re)
                  (backward-to-indentation 1))
                (not (looking-at py-no-outdent-re)))
         )))

(defun py-electric-colon (arg)
  "Insert a colon.
In certain cases the line is dedented appropriately.  If a numeric
argument ARG is provided, that many colons are inserted
non-electrically.  Electric behavior is inhibited inside a string or
comment."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  ;; are we in a string or comment?
  (if (save-excursion
        (let ((pps (parse-partial-sexp (save-excursion
                                         (py-beginning-of-def-or-class)
                                         (point))
                                       (point))))
          (not (or (nth 3 pps) (nth 4 pps)))))
      (save-excursion
        (let ((here (point))
              (outdent 0)
              (indent (py-compute-indentation t)))
          (if (and (not arg)
                   (py-outdent-p)
                   (= indent (save-excursion
                               (py-next-statement -1)
                               (py-compute-indentation t)))
                   )
              (setq outdent py-indent-offset))
          ;; Don't indent, only dedent.  This assumes that any lines
          ;; that are already dedented relative to
          ;; py-compute-indentation were put there on purpose.  It's
          ;; highly annoying to have `:' indent for you.  Use TAB, C-c
          ;; C-l or C-c C-r to adjust.  TBD: Is there a better way to
          ;; determine this???
          (if (< (current-indentation) indent) nil
            (goto-char here)
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to (- indent outdent))
            )))))


;; Python subprocess utilities and filters
(defun py-execute-file (proc filename)
  "Send to Python interpreter process PROC \"execfile('FILENAME')\".
Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing."
  (let ((curbuf (current-buffer))
        (procbuf (process-buffer proc))
;       (comint-scroll-to-bottom-on-output t)
        (msg (format "## working on region in file %s...\n" filename))
        ;; add some comment, so that we can filter it out of history
        (cmd (format "execfile(r'%s') # PYTHON-MODE\n" filename)))
    (unwind-protect
        (save-excursion
          (set-buffer procbuf)
          (goto-char (point-max))
          (move-marker (process-mark proc) (point))
          (funcall (process-filter proc) proc msg))
      (set-buffer curbuf))
    (process-send-string proc cmd)))

(defun py-comint-output-filter-function (string)
  "Watch output for Python prompt and exec next file waiting in queue.
This function is appropriate for `comint-output-filter-functions'."
  ;;remove ansi terminal escape sequences from string, not sure why they are
  ;;still around...
  (setq string (ansi-color-filter-apply string))
  (when (and (string-match py-shell-input-prompt-1-regexp string)
                   py-file-queue)
    (if py-shell-switch-buffers-on-execute
      (pop-to-buffer (current-buffer)))
    (ignore-errors (delete-file (car py-file-queue)))
    (setq py-file-queue (cdr py-file-queue))
    (if py-file-queue
        (let ((pyproc (get-buffer-process (current-buffer))))
          (py-execute-file pyproc (car py-file-queue))))
    ))

(defun py-pdbtrack-overlay-arrow (activation)
  "Activate or de arrow at beginning-of-line in current buffer."
  ;; This was derived/simplified from edebug-overlay-arrow
  (cond (activation
         (setq overlay-arrow-position (make-marker))
         (setq overlay-arrow-string "=>")
         (set-marker overlay-arrow-position (py-point 'bol) (current-buffer))
         (setq py-pdbtrack-is-tracking-p t))
        (overlay-arrow-position
         (setq overlay-arrow-position nil)
         (setq py-pdbtrack-is-tracking-p nil))
        ))

(defun py-pdbtrack-track-stack-file (text)
  "Show the file indicated by the pdb stack entry line, in a separate window.

Activity is disabled if the buffer-local variable
`py-pdbtrack-do-tracking-p' is nil.

We depend on the pdb input prompt matching `py-pdbtrack-input-prompt'
at the beginning of the line.

If the traceback target file path is invalid, we look for the most
recently visited python-mode buffer which either has the name of the
current function \(or class) or which defines the function \(or
class).  This is to provide for remote scripts, eg, Zope's 'Script
\(Python)' - put a _copy_ of the script in a buffer named for the
script, and set to python-mode, and pdbtrack will find it.)"
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next pdb prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other pdb commands wipe out the highlight.  You can always do a
  ;; 'where' (aka 'w') command to reveal the overlay arrow.
  (let* ((origbuf (current-buffer))
         (currproc (get-buffer-process origbuf)))

    (if (not (and currproc py-pdbtrack-do-tracking-p))
        (py-pdbtrack-overlay-arrow nil)

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              py-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat py-pdbtrack-input-prompt "$") block))
            (py-pdbtrack-overlay-arrow nil)

          (setq target (py-pdbtrack-get-source-buffer block))

          (if (stringp target)
              (message "pdbtrack: %s" target)

            (setq target_lineno (car target))
            (setq target_buffer (cadr target))
            (setq target_fname (buffer-file-name target_buffer))
            (switch-to-buffer-other-window target_buffer)
            (goto-char (point-min))
            (forward-line target_lineno)
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (py-pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)

            )))))
  )

(defun py-pdbtrack-get-source-buffer (block)
  "Return line number and buffer of code indicated by block's traceback text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited python-mode buffer
with the same name or having the named function.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (not (string-match py-pdbtrack-stack-entry-regexp block))

      "Traceback cue not found"

    (let* ((filename (match-string 1 block))
           (lineno (string-to-number (match-string 2 block)))
           (funcname (match-string 3 block))
           funcbuffer)

      (cond ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((setq funcbuffer (py-pdbtrack-grub-for-buffer funcname lineno))
             (if (string-match "/Script (Python)$" filename)
                 ;; Add in number of lines for leading '##' comments:
                 (setq lineno
                       (+ lineno
                          (save-excursion
                            (set-buffer funcbuffer)
                            (count-lines
                             (point-min)
                             (max (point-min)
                                  (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
                                                (buffer-substring (point-min)
                                                                  (point-max)))
                                  ))))))
             (list lineno funcbuffer))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s(), %s" funcname filename)))
      )
    )
  )

(defun py-pdbtrack-grub-for-buffer (funcname lineno)
  "Find most recent buffer itself named or having function funcname.

We walk the buffer-list history for python-mode buffers that are
named for funcname or define a function funcname."
  (let ((buffers (buffer-list))
        buf
        got)
    (while (and buffers (not got))
      (setq buf (car buffers)
            buffers (cdr buffers))
      (if (and (save-excursion (set-buffer buf)
                               (string= major-mode "python-mode"))
               (or (string-match funcname (buffer-name buf))
                   (string-match (concat "^\\s-*\\(def\\|class\\)\\s-+"
                                         funcname "\\s-*(")
                                 (save-excursion
                                   (set-buffer buf)
                                   (buffer-substring (point-min)
                                                     (point-max))))))
          (setq got buf)))
    got))

(defun py-postprocess-output-buffer (buf)
  "Highlight exceptions found in BUF.
If an exception occurred return t, otherwise return nil.  BUF must exist."
  (let (line file bol err-p)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (re-search-forward py-traceback-line-re nil t)
        (setq file (match-string 1)
              line (string-to-number (match-string 2))
              bol (py-point 'bol))
        (py-highlight-line bol (py-point 'eol) file line)))
    (when (and py-jump-on-exception line)
      (beep)
      (py-jump-to-exception file line)
      (setq err-p t))
    err-p))


;;; Subprocess commands
(defvar py-output-buffer "*Python Output*")
(make-variable-buffer-local 'py-output-buffer)

;; for toggling between CPython and Jython
(defvar py-which-shell nil)
(defvar py-which-args  py-python-command-args)
(defvar py-which-bufname "Python")
(make-variable-buffer-local 'py-which-shell)
(make-variable-buffer-local 'py-which-args)
(make-variable-buffer-local 'py-which-bufname)

(defun py-toggle-shells (arg)
  "Toggles between the CPython and Jython shells.

With positive argument ARG (interactively \\[universal-argument]),
uses the CPython shell, with negative ARG uses the Jython shell, and
with a zero argument, toggles the shell.

Programmatically, ARG can also be one of the symbols `cpython' or
`jython', equivalent to positive arg and negative arg respectively."
  (interactive "P")
  ;; default is to toggle
  (unless arg
      (setq arg 0))
  ;; preprocess arg
  (cond
   ((equal arg 0)
    ;; toggle
    (if (string-equal py-which-bufname "Python")
        (setq arg -1)
      (setq arg 1)))
   ((equal arg 'cpython) (setq arg 1))
   ((equal arg 'jython) (setq arg -1)))
  (let (msg)
    (cond
     ((< 0 arg)
      ;; set to CPython
      (setq py-which-shell py-python-command
            py-which-args py-python-command-args
            py-which-bufname "Python"
            msg "CPython")
      (if (string-equal py-which-bufname "Jython")
          (setq mode-name "Python")))
     ((> 0 arg)
      (setq py-which-shell py-jython-command
            py-which-args py-jython-command-args
            py-which-bufname "Jython"
            msg "Jython")
      (if (string-equal py-which-bufname "Python")
          (setq mode-name "Jython")))
     )
    (message "Using the %s shell" msg)
    (setq py-output-buffer (format "*%s Output*" py-which-bufname))))

;;;###autoload
(defun py-shell (&optional argprompt)
  "Start an interactive Python interpreter in another window.
This is like Shell mode, except that Python is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Python*' buffer.

With optional \\[universal-argument], the user is prompted for the
flags to pass to the Python interpreter.  This has no effect when this
command is used to switch to an existing process, only when a new
process is started.  If you use this, you will probably want to ensure
that the current arguments are retained (they will be included in the
prompt).  This argument is ignored when this function is called
programmatically, or when running in Emacs 19.34 or older.

Note: You can toggle between using the CPython interpreter and the
Jython interpreter by hitting \\[py-toggle-shells].  This toggles
buffer local variables which control whether all your subshell
interactions happen to the `*Jython*' or `*Python*' buffers (the
latter is the name used for the CPython buffer).

Warning: Don't use an interactive Python if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `python-mode' can't
distinguish your output from Python's output, and assumes that `>>> '
at the start of a line is a prompt from Python.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Python prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Python, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Python process buffers using the default (Emacs-supplied) process
filter."
  (interactive "P")
  ;; Set the default shell if not already set
  (when (null py-which-shell)
    (py-toggle-shells py-default-interpreter))
  (let ((args py-which-args))
    (when (and argprompt
               (interactive-p)
               (fboundp 'split-string))
      ;; TBD: Perhaps force "-i" in the final list?
      (setq args (split-string
                  (read-string (concat py-which-bufname
                                       " arguments: ")
                               (concat
                                (mapconcat 'identity py-which-args " ") " ")
                               ))))
    (if (not (equal (buffer-name) "*Python*"))
        (switch-to-buffer-other-window
         (apply 'make-comint py-which-bufname py-which-shell nil args))
      (apply 'make-comint py-which-bufname py-which-shell nil args))
    (make-local-variable 'comint-prompt-regexp)
    (setq comint-prompt-regexp (concat py-shell-input-prompt-1-regexp "\\|"
                                       py-shell-input-prompt-2-regexp "\\|"
                                       "^([Pp]db) "))
    (add-hook 'comint-output-filter-functions
              'py-comint-output-filter-function)
    ;; pdbtrack
    (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
    (setq py-pdbtrack-do-tracking-p t)
    (set-syntax-table py-mode-syntax-table)
    (use-local-map py-shell-map)
    (run-hooks 'py-shell-hook)
    ))

(defun py-clear-queue ()
  "Clear the queue of temporary files waiting to execute."
  (interactive)
  (let ((n (length py-file-queue)))
    (mapc 'delete-file py-file-queue)
    (setq py-file-queue nil)
    (message "%d pending files de-queued." n)))


(defun py-execute-region (start end &optional async)
  "Execute the region in a Python interpreter.

The region is first copied into a temporary file (in the directory
`py-temp-directory').  If there is no Python interpreter shell
running, this file is executed synchronously using
`shell-command-on-region'.  If the program is long running, use
\\[universal-argument] to run the command asynchronously in its own
buffer.

When this function is used programmatically, arguments START and END
specify the region to execute, and optional third argument ASYNC, if
non-nil, specifies to run the command asynchronously in its own
buffer.

If the Python interpreter shell is running, the region is execfile()'d
in that shell.  If you try to execute regions too quickly,
`python-mode' will queue them up and execute them one at a time when
it sees a `>>> ' prompt from Python.  Each time this happens, the
process buffer is popped into a window (if it's not already in some
window) so you can see it, and a comment of the form

    \t## working on region in file <name>...

is inserted at the end.  See also the command `py-clear-queue'."
  (interactive "r\nP")
  ;; Skip ahead to the first non-blank line
  (let* ((proc (get-process py-which-bufname))
         (temp (make-temp-name "python-"))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (cur (current-buffer))
         (buf (get-buffer-create file))
         shell)
    ;; Write the contents of the buffer, watching out for indented regions.
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (while (and (looking-at "\\s *$")
                  (< (point) end))
        (forward-line 1))
      (setq start (point))
      (or (< start end)
          (error "Region is empty"))
      (setq py-line-number-offset (count-lines 1 start))
      (let ((needs-if (/= (py-point 'bol) (py-point 'boi))))
        (set-buffer buf)
        (python-mode)
        (insert "#-*- coding: utf-8 -*-\n")
        (when needs-if
          (insert "if 1:\n")
          (setq py-line-number-offset (- py-line-number-offset 1)))
        (insert-buffer-substring cur start end)
        ;; Set the shell either to the #! line command, or to the
        ;; py-which-shell buffer local variable.
        (setq shell (or (py-choose-shell-by-shebang)
                        (py-choose-shell-by-import)
                        py-which-shell))))
    (cond
     ;; always run the code in its own asynchronous subprocess
     (async
      ;; User explicitly wants this to run in its own async subprocess
      (save-excursion
        (set-buffer buf)
        (write-region (point-min) (point-max) file nil 'nomsg))
      (let* ((buf (generate-new-buffer-name py-output-buffer))
             ;; TBD: a horrible hack, but why create new Custom variables?
             (arg (if (string-equal py-which-bufname "Python")
                      "-u" "")))
        (start-process py-which-bufname buf shell arg file)
        (pop-to-buffer buf)
        (py-postprocess-output-buffer buf)
        ;; TBD: clean up the temporary file!
        ))
     ;; if the Python interpreter shell is running, queue it up for
     ;; execution there.
     (proc
      ;; use the existing python shell
      (save-excursion
        (set-buffer buf)
        (write-region (point-min) (point-max) file nil 'nomsg))
      (if (not py-file-queue)
          (py-execute-file proc file)
        (message "File %s queued for execution" file))
      (setq py-file-queue (append py-file-queue (list file)))
      (setq py-exception-buffer (cons file (current-buffer))))
     (t
      ;; TBD: a horrible hack, but why create new Custom variables?
      (let ((cmd (concat py-which-shell (if (string-equal py-which-bufname
                                                          "Jython")
                                            " -" ""))))
        ;; otherwise either run it synchronously in a subprocess
        (save-excursion
          (set-buffer buf)
          (shell-command-on-region (point-min) (point-max)
                                   cmd py-output-buffer))
        ;; shell-command-on-region kills the output buffer if it never
        ;; existed and there's no output from the command
        (if (not (get-buffer py-output-buffer))
            (message "No output.")
          (setq py-exception-buffer (current-buffer))
          (let ((err-p (py-postprocess-output-buffer py-output-buffer)))
            (pop-to-buffer py-output-buffer)
            (if err-p
                (pop-to-buffer py-exception-buffer)))
          ))
      ))
    ;; Clean up after ourselves.
    (kill-buffer buf)))


;; Code execution commands
(defun py-execute-buffer (&optional async)
  "Send the contents of the buffer to a Python interpreter.
If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

If there is a *Python* process buffer it is used.  If a clipping
restriction is in effect, only the accessible portion of the buffer is
sent.  A trailing newline will be supplied if needed.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "P")
  (let ((old-buffer (current-buffer)))
    (if py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
    (py-execute-region (point-min) (point-max) async)
       (pop-to-buffer old-buffer)))

(defun py-execute-import-or-reload (&optional async)
  "Import the current buffer's file in a Python interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in \".py\", then do execfile instead.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument.

This may be preferable to `\\[py-execute-buffer]' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions."
  (interactive "P")
  ;; Check file local variable py-master-file
  (if py-master-file
      (let* ((filename (expand-file-name py-master-file))
             (buffer (or (get-file-buffer filename)
                         (find-file-noselect filename))))
        (set-buffer buffer)))
  (let ((file (buffer-file-name (current-buffer))))
    (if file
        (progn
          ;; Maybe save some buffers
          (save-some-buffers (not py-ask-about-save) nil)
          (py-execute-string
           (if (string-match "\\.py$" file)
               (let ((f (file-name-sans-extension
                         (file-name-nondirectory file))))
                 (format "if globals().has_key('%s'):\n    reload(%s)\nelse:\n    import %s\n"
                         f f f))
             (format "execfile(r'%s')\n" file))
           async))
      ;; else
      (py-execute-buffer async))))

(defun py-execute-def-or-class (&optional async)
  "Send the current function or class definition to a Python interpreter.

If there is a *Python* process buffer it is used.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "P")
  (save-excursion
    (py-mark-def-or-class)
    ;; mark is before point
    (py-execute-region (mark) (point) async)))

(defun py-execute-string (string &optional async)
  "Send the argument STRING to a Python interpreter.

If there is a *Python* process buffer it is used.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "sExecute Python command: ")
  (save-excursion
    (set-buffer (get-buffer-create
                 (generate-new-buffer-name " *Python Command*")))
    (insert string)
    (py-execute-region (point-min) (point-max) async)))


(defun py-jump-to-exception (file line)
  "Jump to the Python code in FILE at LINE."
  (let ((buffer (cond ((string-equal file "<stdin>")
                       (if (consp py-exception-buffer)
                           (cdr py-exception-buffer)
                         py-exception-buffer))
                      ((and (consp py-exception-buffer)
                            (string-equal file (car py-exception-buffer)))
                       (cdr py-exception-buffer))
                      ((ignore-errors (find-file-noselect file)))
                      ;; could not figure out what file the exception
                      ;; is pointing to, so prompt for it
                      (t (find-file (read-file-name "Exception file: "
                                                    nil
                                                    file t))))))
    ;; Fiddle about with line number
    (setq line (+ py-line-number-offset line))

    (pop-to-buffer buffer)
    ;; Force Python mode
    (unless(eq major-mode 'python-mode)
        (python-mode))
    (goto-char (point-min))
    (forward-line line)
    (message "Jumping to exception in file %s on line %d" file line)))

(when (featurep 'xemacs)
  (defun py-mouseto-exception (event)
    "Jump to the code which caused the Python exception at EVENT.
EVENT is usually a mouse click."
    (interactive "e")
    (cond
     ((fboundp 'event-point)
      ;; XEmacs
      (let* ((point (event-point event))
             (buffer (event-buffer event))
             (e (and point buffer (extent-at point buffer 'py-exc-info)))
             (info (and e (extent-property e 'py-exc-info))))
        (message "Event point: %d, info: %s" point info)
        (and info
             (py-jump-to-exception (car info) (cdr info)))))
     ;; Emacs -- Please port this!
     )))

(defun py-goto-exception ()
  "Go to the line indicated by the traceback."
  (interactive)
  (let (file line)
    (save-excursion
      (beginning-of-line)
      (if (looking-at py-traceback-line-re)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (if (not file)
        (error "Not on a traceback line"))
    (py-jump-to-exception file line)))

(defun py-find-next-exception (start buffer searchdir errwhere)
  "Find the next Python exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
`re-search-backward' or `re-search-forward' indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (set-buffer buffer)
      (goto-char (py-point start))
      (if (funcall searchdir py-traceback-line-re nil t)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (if (and file line)
        (py-jump-to-exception file line)
      (error "%s of traceback" errwhere))))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.
With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
         (buffer (if proc "*Python*" py-output-buffer)))
    (if bottom
        (py-find-next-exception 'eob buffer 're-search-backward "Bottom")
      (py-find-next-exception 'eol buffer 're-search-forward "Bottom"))))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.
With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
         (buffer (if proc "*Python*" py-output-buffer)))
    (if top
        (py-find-next-exception 'bob buffer 're-search-forward "Top")
      (py-find-next-exception 'bol buffer 're-search-backward "Top"))))


;; Electric deletion
(defun py-electric-backspace (&optional arg)
  "Delete preceding character or level of indentation.
With ARG do that ARG times. "
  (interactive "*p")
  (let ((arg (or arg 1)))
    (dotimes (i arg)
      (if (looking-back "^[ \t]+")
          (let* ((remains (% (current-column) py-indent-offset)))
            (if (< 0 remains)
                (delete-char (- remains))
              (indent-line-to (- (current-indentation) py-indent-offset))))
        (delete-char (- 1))))))
    
(defun py-electric-delete (&optional arg)
  "Delete preceding or following character or levels of whitespace.

With ARG do that ARG times. "
  (interactive "*p")
  (let ((arg (or arg 1)))
    (dotimes (i arg)
      (if (and (or (bolp)(looking-back "^[ \t]+")) (looking-at "[ \t]+"))
          (let* ((remains (% (+ (current-column) (- (match-end 0)(match-beginning 0))) py-indent-offset)))
            (if (< 0 remains)
                (delete-char remains)
              (delete-char py-indent-offset)))
        (delete-char 1)))))

;; required for pending-del and delsel modes
(put 'py-electric-colon 'delete-selection t) ;delsel
(put 'py-electric-colon 'pending-delete   t) ;pending-del
(put 'py-electric-backspace 'delete-selection 'supersede) ;delsel
(put 'py-electric-backspace 'pending-delete   'supersede) ;pending-del
(put 'py-electric-delete    'delete-selection 'supersede) ;delsel
(put 'py-electric-delete    'pending-delete   'supersede) ;pending-del

33333

(defun py-indent-line ()
  "Fix the indentation of the current line according to Python rules.

This function is normally bound to `indent-line-function' so
\\[indent-for-tab-command] will call it."
  (interactive)
  (let* ((need (py-compute-indentation (point))))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to need)))

(defun py-newline-and-indent ()
  "Strives to act like the Emacs `newline-and-indent'.
This is just `strives to' because correct indentation can't be computed
from scratch for Python code.  In general, deletes the whitespace before
point, inserts a newline, and takes an educated guess as to how you want
the new line indented."
  (interactive)
  (let ((ci (current-indentation)))
    (if (< ci (current-column))         ; if point beyond indentation
        (newline-and-indent)
      ;; else try to act like newline-and-indent "normally" acts
      (beginning-of-line)
      (insert-char ?\n 1)
      (move-to-column ci))))

(defalias 'py-newline-and-close-block 'py-newline-and-dedent)
(defun py-newline-and-dedent ()
  "Add a newline and indent to one level below current. "
  (interactive "*")
  (let ((cui (current-indentation))
        goal)
    (newline)
    (when (< 0 cui)
      (setq goal (py-compute-indentation))
      (indent-to-column (- goal py-indent-offset)))))

(defun py-compute-indentation (&optional orig origline)
  "Compute Python indentation.
 When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of dedenting."
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (let* ((orig (or orig (point))) 
             (origline (or origline (py-count-lines)))
             (pps (parse-partial-sexp (point-min) (point)))
             erg indent)
        ;;        (back-to-indentation)
        (setq indent
              (cond
               ;; (py-in-triplequoted-string-p)
               ((and (nth 3 pps)(nth 8 pps)(save-excursion (ignore-errors (goto-char (nth 2 pps))) (not (eq origline (py-count-lines)))))
                (goto-char (nth 2 pps))
                (current-column))
               ;; beginning of statement from before got beginning
               ((looking-at "\"\"\"\\|'''")
                (py-beginning-of-statement)
                (py-compute-indentation orig origline))
               ;; comments and strings
               ((nth 8 pps)
                (goto-char (nth 8 pps))
                (skip-chars-backward " \t\r\n\f")
                (py-compute-indentation orig origline))
               ;; lists
               ((and (nth 1 pps) py-indent-honors-multiline-listing)
                (progn (goto-char (+ py-lhs-inbound-indent (nth 1 pps)))
                       (when (looking-at "[ \t]+")
                         (goto-char (match-end 0)))
                       (current-column)))
               ((nth 1 pps)
                (save-excursion
                  (py-beginning-of-statement)
                  (+ (current-column) (* (nth 0 pps) py-indent-offset))))
               ((py-backslashed-continuation-line-p)
                (progn (py-beginning-of-statement)
                       (+ (current-indentation) py-continuation-offset)))
               ((not (py-beginning-of-statement-p))
                (py-beginning-of-statement)
                (py-compute-indentation orig origline))
               ((looking-at py-if-clause-re)
                (py-beginning-of-if-block)
                (current-indentation))
               ((looking-at py-try-clause-re)
                (py-beginning-of-try-block)
                (current-indentation))
               ((looking-at py-clause-re)
                (py-beginning-of-block)
                (current-indentation))
               ((looking-at py-block-closing-keywords-re) 
                (save-excursion
                  (py-beginning-of-block)
                  (current-indentation)))
               ((looking-at py-assignement-re)
                (current-indentation))
               ((and (eq origline (py-count-lines))
                     (setq erg (py-go-to-keyword py-block-or-clause-re -1)))
                (+ (car erg) py-indent-offset))
               ((looking-at py-block-or-clause-re)
                (+ (current-indentation) py-indent-offset))
               ((py-statement-opens-block-p)
                (+ py-indent-offset (current-indentation)))
               ((bobp)
                (current-indentation))
               (t (current-indentation))))
        (when (interactive-p) (message "%s" indent))
        indent))))

(defun py-guess-indent-offset (&optional global)
  "Guess a value for, and change, `py-indent-offset'.

By default, make a buffer-local copy of `py-indent-offset' with the
new value.  
With optional argument GLOBAL,
change the global value of `py-indent-offset'. "
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (lexical-let ((old-value py-indent-offset)
                    (orig (point))
                    done indent first)
        (while (and (not done) (not (bobp)))
          (setq first (progn (unless done (py-beginning-of-statement)(current-indentation))))
          (save-excursion
            (py-beginning-of-statement)
            (setq indent (abs (- first (current-indentation))))
            (if (and indent (>= indent 2) (<= indent 8)) ; sanity check
                (setq done t))))
        (unless done
          ;; search downward
          (goto-char orig) 
          (while (and (not done) (not (eobp)))
            (setq first (progn (unless done (py-end-of-statement)(current-indentation))))
            (py-end-of-statement)
            (save-excursion
              (py-beginning-of-statement)
              (setq indent (abs (- (current-indentation) first)))
              (if (and indent (>= indent 2) (<= indent 8)) ; sanity check
                  (setq done t))))) 
	(when done
	  (when (/= indent (default-value 'py-indent-offset))
            (funcall (if global 'kill-local-variable 'make-local-variable)
                     'py-indent-offset)
            (setq py-indent-offset indent)
	    (unless (= tab-width py-indent-offset)
	      (setq indent-tabs-mode nil)))
          (unless (and (eq old-value py-indent-offset) (eq py-indent-offset (default-value 'py-indent-offset)))
          (message "%s value of py-indent-offset:  %d"
                   (if global "Global" "Local")
                   py-indent-offset)))
        py-indent-offset))))

(defun py-comment-indent-function ()
  "Python version of `comment-indent-function'."
  ;; This is required when filladapt is turned off.  Without it, when
  ;; filladapt is not used, comments which start in column zero
  ;; cascade one character to the right
  (save-excursion
    (beginning-of-line)
    (let ((eol (py-point 'eol)))
      (and comment-start-skip
           (re-search-forward comment-start-skip eol t)
           (setq eol (match-beginning 0)))
      (goto-char eol)
      (skip-chars-backward " \t")
      (max comment-column (+ (current-column) (if (bolp) 0 1)))
      )))

(defun py-narrow-to-defun (&optional class)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Optional CLASS is passed directly to `py-beginning-of-def-or-class'."
  (interactive "P")
  (save-excursion
    (widen)
    (py-end-of-def-or-class class)
    (let ((end (point)))
      (py-beginning-of-def-or-class class)
      (narrow-to-region (point) end))))


(defun py-shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (let (deactivate-mark)
      (indent-rigidly start end count))))

(defun py-shift-region-left (start end &optional count)
  "Shift region of Python code to the left.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the left, by `py-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, dedent only the current line.
You cannot dedent the region if any line is already at column zero."
  (interactive
   (let ((p (point))
         (m (condition-case nil (mark) (mark-inactive nil)))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
               (not (looking-at "\\s *$")))
          (error "Region is at left edge"))
      (forward-line 1)))
  (py-shift-region start end (- (prefix-numeric-value
                                 (or count py-indent-offset))))
  (py-keep-region-active))

(defun py-shift-region-right (start end &optional count)
  "Shift region of Python code to the right.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the right, by `py-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, indent only the current line."
  (interactive
   (let ((p (point))
         (m (condition-case nil (mark) (mark-inactive nil)))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (py-shift-region start end (prefix-numeric-value
                              (or count py-indent-offset)))
  (py-keep-region-active))

(defun py-indent-region (start end &optional indent-offset)
  "Reindent a region of Python code.

The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
reindented.  If the first line of the region has a non-whitespace
character in the first column, the first line is left alone and the
rest of the region is reindented with respect to it.  Else the entire
region is reindented with respect to the (closest code or indenting
comment) statement immediately preceding the region.

This is useful when code blocks are moved or yanked, when enclosing
control structures are introduced or removed, or to reformat code
using a new value for the indentation offset.

If a numeric prefix argument is given, it will be used as the value of
the indentation offset.  Else the value of `py-indent-offset' will be
used.

Warning: The region must be consistently indented before this function
is called!  This function does not compute proper indentation from
scratch (that's impossible in Python), it merely adjusts the existing
indentation to be correct in context.

Warning: This function really has no idea what to do with
non-indenting comment lines, and shifts them as if they were indenting
comment lines.  Fixing this appears to require telepathy.

Special cases: whitespace is deleted from blank lines; continuation
lines are shifted by the same amount their initial line was shifted,
in order to preserve their relative indentation with respect to their
initial line; and comment lines beginning in column 1 are ignored."
  (interactive "*r\nP")                 ; region; raw prefix arg
  (save-excursion
    (goto-char end)   (beginning-of-line) (setq end (point-marker))
    (goto-char start) (beginning-of-line)
    (let ((py-indent-offset (prefix-numeric-value
                             (or indent-offset py-indent-offset)))
          (indents '(-1))               ; stack of active indent levels
          (target-column 0)             ; column to which to indent
          (base-shifted-by 0)           ; amount last base line was shifted
          (indent-base (if (looking-at "[ \t\n]")
                           (py-compute-indentation t)
                         0))
          ci)
      (while (< (point) end)
        (setq ci (current-indentation))
        ;; figure out appropriate target column
        (cond
         ((or (eq (following-char) ?#)  ; comment in column 1
              (looking-at "[ \t]*$"))   ; entirely blank
          (setq target-column 0))
         ((py-continuation-line-p)      ; shift relative to base line
          (setq target-column (+ ci base-shifted-by)))
         (t                             ; new base line
          (if (> ci (car indents))      ; going deeper; push it
              (setq indents (cons ci indents))
            ;; else we should have seen this indent before
            (setq indents (memq ci indents)) ; pop deeper indents
            (if (null indents)
                (error "Bad indentation in region, at line %d"
                       (save-restriction
                         (widen)
                         (1+ (count-lines 1 (point)))))))
          (setq target-column (+ indent-base
                                 (* py-indent-offset
                                    (- (length indents) 2))))
          (setq base-shifted-by (- target-column ci))))
        ;; shift as needed
        (if (/= ci target-column)
            (progn
              (delete-horizontal-space)
              (indent-to target-column)))
        (forward-line 1))))
  (set-marker end nil))

(defun py-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start py-block-comment-prefix))
    (comment-region beg end arg)))

(defun py-join-words-wrapping (words separator line-prefix line-length)
  (let ((lines ())
        (current-line line-prefix))
    (while words
      (let* ((word (car words))
             (maybe-line (concat current-line word separator)))
        (if (> (length maybe-line) line-length)
            (setq lines (cons (substring current-line 0 -1) lines)
                  current-line (concat line-prefix word separator " "))
          (setq current-line (concat maybe-line " "))))
      (setq words (cdr words)))
    (setq lines (cons (substring
                       current-line 0 (- 0 (length separator) 1)) lines))
    (mapconcat 'identity (nreverse lines) "\n")))

(defun py-sort-imports ()
  "Sort multiline imports.
Put point inside the parentheses of a multiline import and hit
\\[py-sort-imports] to sort the imports lexicographically"
  (interactive)
  (save-excursion
    (let ((open-paren (save-excursion (progn (up-list -1) (point))))
          (close-paren (save-excursion (progn (up-list 1) (point))))
          sorted-imports)
      (goto-char (1+ open-paren))
      (skip-chars-forward " \n\t")
      (setq sorted-imports
            (sort
             (delete-dups
              (split-string (buffer-substring
                             (point)
                             (save-excursion (goto-char (1- close-paren))
                                             (skip-chars-backward " \n\t")
                                             (point)))
                            ", *\\(\n *\\)?"))
             ;; XXX Should this sort case insensitively?
             'string-lessp))
      ;; Remove empty strings.
      (delete-region open-paren close-paren)
      (goto-char open-paren)
      (insert "(\n")
      (insert (py-join-words-wrapping (remove "" sorted-imports) "," "    " 78))
      (insert ")")
      )))


;; Functions for moving point
(defalias 'py-hungry-delete-forward 'c-hungry-delete-forward)
(defalias 'py-hungry-delete-backwards 'c-hungry-delete-backwards)

;; Statement
(defalias 'py-backward-statement 'py-beginning-of-statement)
(defalias 'py-previous-statement 'py-beginning-of-statement)
(defalias 'py-statement-backward 'py-beginning-of-statement)
(defalias 'py-goto-statement-at-or-above 'py-beginning-of-statement)
(defun py-beginning-of-statement (&optional orig origline)
  "Go to the initial line of a simple statement.

For beginning of compound statement use py-beginning-of-block.
For beginning of clause py-beginning-of-clause.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html
"
  (interactive)
  (save-restriction
    (widen)
    (if (and (bobp) orig)
        (point)
      (unless (bobp)
        (let ((orig (or orig (point)))
              (cui (current-indentation))
              (origline (or origline (py-count-lines)))
              (pps (parse-partial-sexp (point-min) (point)))
              erg)
          (unless (< (point) orig)(skip-chars-backward " \t\r\n\f"))
          
          (setq erg
                (cond
                 ((empty-line-p)
                  (forward-line -1)
                  (while (and (not (bobp))(empty-line-p))
                    (forward-line -1))
                  (end-of-line)
                  (py-beginning-of-statement orig origline))
                 ;; if in string
                 ((and (nth 3 pps)(nth 8 pps)
                       (save-excursion
                         (ignore-errors
                           (goto-char (nth 2 pps))) 
                         ;; (not (eq origline (py-count-lines)))
                         ))
                  (goto-char (nth 2 pps))
                  (py-beginning-of-statement orig origline))
                 ;; comments left, as strings are done
                 ((nth 8 pps)
                  (goto-char (1- (nth 8 pps)))
                  (py-beginning-of-statement orig origline))
                 ((looking-at "[ \t]*#")
                  (forward-line -1) (end-of-line)
                  (py-beginning-of-statement orig origline))
                 ((py-backslashed-continuation-line-p)
                  (forward-line -1)
                  (py-beginning-of-statement orig origline))
                 ;; character address of start of innermost containing list; nil if none.
                 ((nth 1 pps)
                  (goto-char (nth 1 pps))
                  (forward-char -1)
                  (py-beginning-of-statement orig origline))
                 ((and (eq (current-indentation) (current-column))
                       (eq (point) orig) (not (bolp)))
                  (beginning-of-line)
                  (py-beginning-of-statement orig origline))
                 ((and
                   (not (string= "" (make-string (- orig (point)) ? )))
                   (looking-at (make-string (- orig (point)) ? )))
                  (forward-line -1)
                  (py-beginning-of-statement orig origline))
                 ((not (eq (current-column) (current-indentation)))
                  (back-to-indentation)
                  (py-beginning-of-statement orig origline))
                 ((and (eq (point) orig)(or (bolp) (<= (current-column)(current-indentation))))
                  (forward-line -1)
                  (end-of-line)
                  (skip-chars-backward " \t")
                  (py-beginning-of-statement orig origline))
                 (t (point))))
          (when (interactive-p) (message "%s" erg))
          erg)))))

(defalias 'py-statement-forward 'py-end-of-statement)
(defalias 'py-next-statement 'py-end-of-statement)
(defalias 'py-forward-statement 'py-end-of-statement)
(defun py-end-of-statement (&optional orig origline done)
  "Go to the point just beyond the final line of the current statement. "
  (interactive)
  (save-restriction
    (widen)
    (unless (eobp)
      (let* ((orig (or orig (point)))
             (origline (or origline (py-count-lines)))
             (pps (parse-partial-sexp (point-min) (point))))
        (cond
         ((and (empty-line-p)(not done)(not (eobp)))
          (while
              (and (empty-line-p)(not done)(not (eobp)))
            (forward-line 1)
            (py-end-of-statement (point) origline done)))
         ;; start of comment or string; nil if not in one
         ((nth 8 pps)
          (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
            (goto-char (match-end 0)))
          (re-search-forward "[^\\]\"\"\"\\|[^\\]'''\\|[^\\]\"\\|[^\\]'" nil (quote move) 1)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f")
          (py-end-of-statement orig origline done))
         ((looking-at "[ \t]*#")
          (while (looking-at "[ \t]*#")
            (forward-line 1)
            (beginning-of-line))
          (end-of-line)
          (skip-chars-backward " \t\r\n\f")
          (py-end-of-statement orig origline done))
         ((py-backslashed-continuation-line-p)
          (py-forward-line)
          (py-end-of-statement orig origline done))
         ;; start of innermost containing list; nil if none.
         ((nth 1 pps)
          (goto-char (nth 1 pps))
          (forward-list)
          (setq done t)
          (py-end-of-statement orig origline done))
         ((and (eq (point) orig)(not (looking-at "[ \t]*$")))
          (end-of-line) (skip-chars-backward " \t")
          (py-beginning-of-comment)
          (skip-chars-backward " \t")
          (if (< orig (point))
              (py-end-of-statement orig origline done)
            (py-forward-line)
            (py-end-of-statement orig origline done)))
         ((and (eq orig (point))(looking-at "[ \t]*$")(not (eobp)))
          (py-forward-line)
          (py-end-of-statement orig origline done))
         ((and (eq orig (point))(not (eobp)))
          (py-forward-line)
          (py-end-of-statement orig origline done))))
      (when (interactive-p) (message "%s" (point)))
      (point))))

(defun py-goto-statement-below ()
  "Goto beginning of next statement. "
  (interactive)
  (lexical-let ((orig (point))
                (erg (py-end-of-statement)))
    (py-beginning-of-statement)
    (when (< (point) orig)
      (goto-char erg)
      (py-end-of-statement)
      (py-beginning-of-statement))))

;; Block
(defalias 'py-previous-block 'py-beginning-of-block)
(defalias 'py-goto-block-up 'py-beginning-of-block)
(defalias 'py-backward-block 'py-beginning-of-block)
(defun py-beginning-of-block (&optional count)
  "Looks up for nearest opening block, i.e. compound statement

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "p")
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-block-re -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-beginning-of-if-block (&optional count)
  "Looks up for nearest opening if-block, i.e. compound statement

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "p")
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-if-block-re -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-beginning-of-try-block (&optional count)
  "Looks up for nearest opening try-block, i.e. compound statement

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "p")
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-try-block-re -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-block 'py-end-of-block)
(defun py-end-of-block (&optional count)
  "Move point beyond compound statement. "
  (interactive "p")
  (py-end-of-block-or-clause))

;; Block or clause
(defalias 'py-goto-initial-line 'py-beginning-of-block-or-clause)
(defalias 'py-previous-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'py-goto-block-or-clause-up 'py-beginning-of-block-or-clause)
(defalias 'py-backward-block-or-clause 'py-beginning-of-block-or-clause)

(defun py-beginning-of-block-or-clause (&optional arg)
  "Looks up for nearest opening clause or block.
With universal argument looks for next compound statements
i.e. blocks only.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P\\np")
  (let* ((regexp (if arg
                     py-block-re
                   py-block-or-clause-re))
         (erg (ignore-errors (cdr (py-go-to-keyword regexp -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-block-or-clause 'py-end-of-block-or-clause)
(defalias 'py-goto-beyond-block-or-clause 'py-end-of-block-or-clause)
(defun py-end-of-block-or-clause (&optional arg)
  "Without arg, go to the end of a compound statement.
With arg , move point to end of clause at point.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let ((regexp (if arg
                    py-block-re
                  py-block-or-clause-re))
        (orig (point)))
    (py-end-base regexp orig (interactive-p))))

;; Def
(defun py-beginning-of-def (&optional class)
  "Move point to start of `def'. 
Returns position reached, if any, nil otherwise."
  (interactive "P")
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-def-re -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-end-of-def (&optional iact)
  "Move point beyond next method definition.

Returns position reached, if any, nil otherwise."
  (interactive "p")
  (let* ((orig (point))
         (regexp py-def-re))
    (py-end-base regexp orig iact)))

;; Class
(defalias 'py-backward-class 'py-beginning-of-class)
(defalias 'py-previous-class 'py-beginning-of-class)
(defun py-beginning-of-class ()
  "Move point to start of next `class'.
See also `py-beginning-of-def-or-class'. 
Returns position reached, if any, nil otherwise."
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-class-re -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-class 'py-end-of-class)
(defalias 'py-next-class 'py-end-of-class)
(defun py-end-of-class (&optional iact)
  "Move point beyond next method definition.

Returns position reached, if any, nil otherwise."
  (interactive "p")
  (let ((orig (point))
        (regexp py-class-re))
    (py-end-base regexp orig iact)))

;; Clause
(defalias 'py-previous-clause 'py-beginning-of-clause)
(defalias 'py-goto-clause-up 'py-beginning-of-clause)
(defalias 'py-backward-clause 'py-beginning-of-clause)
(defun py-beginning-of-clause ()
  "Looks up for nearest opening clause, i.e. a compound statements
subform.
Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "p")
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-clause-re -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-clause 'py-end-of-clause)
(defalias 'py-goto-beyond-clause 'py-end-of-clause)
(defun py-end-of-clause ()
  "Without arg, go to the end of a compound statement.
With arg , move point to end of clause at point.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((regexp py-block-or-clause-re)
        (orig (point)))
    (py-end-base regexp orig (interactive-p))))

;; Defun or Class
(defalias 'py-backward-def-or-class 'py-beginning-of-def-or-class)
(defalias 'py-previous-def-or-class 'py-beginning-of-def-or-class)
(defun py-beginning-of-def-or-class (&optional class)
  "Move point to start of `def' or `class', whatever is next.
With optional CLASS, move to the beginn of class definition.
Returns position reached, if any, nil otherwise. "
  (interactive "P")
  (let* ((regexp (if class
                     py-class-re
                   py-def-or-class-re))
         (erg (ignore-errors (cdr (py-go-to-keyword regexp -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'end-of-def-or-class 'py-end-of-def-or-class)
(defalias 'py-forward-def-or-class 'py-end-of-def-or-class)
(defalias 'py-next-def-or-class 'py-end-of-def-or-class)
(defun py-end-of-def-or-class (&optional class)
  "Move point beyond next `def' or `class' definition.
With optional CLASS, move to the end of class exclusively.
Returns position reached, if any, nil otherwise."
  (interactive "P")
  (let* ((orig (point))
         (regexp
          (cond (class py-class-re)
                (t py-def-or-class-re))))
    (py-end-base regexp orig (interactive-p))))

(defun py-travel-current-indent (listing)
  "Moves down until clause is closed, i.e. current indentation is reached.

Takes a list, INDENT and START position. "
  (let ((start (ignore-errors (cdr listing)))
        (indent (ignore-errors (car listing)))
        last)
    (if start
        (progn
          (goto-char start)
          (while (and (not (eobp))(setq last (point))(py-end-of-statement)
                      (ignore-errors (<= indent (current-indentation)))))
          (when last (goto-char last))
          last))))

(defun py-go-to-keyword (regexp arg)
  "Returns a list, whose car is indentation, cdr position. "
  (lexical-let ((orig (point))
                (stop (if (< 0 arg)'(eobp)'(bobp)))
                (function (if (< 0 arg) 'py-end-of-statement 'py-beginning-of-statement))
                erg)
    (setq erg (funcall function))
    (while (and (not (eval stop))(not (setq erg (py-statement-opens-block-p regexp))))
      (funcall function))
    (when erg (setq erg (cons (current-indentation) erg)))
    erg))

(defun py-end-base (regexp orig iact)
  "Used internal by functions going to the end forms. "
  (let (erg)
    (unless (py-statement-opens-block-p regexp)
      (py-go-to-keyword regexp -1))
    (when (py-statement-opens-block-p regexp)
      (setq erg (point))
      (forward-line 1)
      (setq erg (py-travel-current-indent (cons (current-indentation) (point))))
      (if (eq orig (point))
          (progn
            (while (not (or
                         (eobp)
                         (and (setq erg (re-search-forward regexp nil (quote move) 1))
                              (not (py-in-string-or-comment-p))
                              (not (py-in-list-p))))))
            (unless (eobp)(py-end-base regexp orig iact)))
        (when (and (< orig (point))(not (eobp)))
          (setq erg (point))))
      (when iact (message "%s" erg))
      erg)))

;; ripped from cc-mode
(defun py-forward-into-nomenclature (&optional arg)
  "Move forward to end of a nomenclature section or word.
With \\[universal-argument] (programmatically, optional argument ARG),
do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (> arg 0)
        (re-search-forward
         "\\(\\W\\|[_]\\)*\\([A-Z]*[a-z0-9]*\\)"
         (point-max) t arg)
      (while (and (< arg 0)
                  (re-search-backward
                   "\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\(\\W\\|[_]\\)\\w+"
                   (point-min) 0))
        (forward-char 1)
        (setq arg (1+ arg)))))
  (py-keep-region-active))

(defun py-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature section or word.
With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (py-forward-into-nomenclature (- arg))
  (py-keep-region-active))

(defun py-in-statement-p ()
  "Returns list of beginning and end-position if inside.
Result is useful for booleans too: (when (py-in-statement-p)...)
will work.
"
  (interactive)
  (let ((orig (point))
        beg end erg)
    (save-excursion
      (setq end (py-end-of-statement))
      (setq beg (py-beginning-of-statement))
      (when (and (<= beg orig)(<= orig end))
        (setq erg (cons beg end))
        (when (interactive-p) (message "%s" erg))
        erg))))

(defun py-beginning-of-statement-p ()
  (interactive)
  "Returns position, if cursor is at the beginning of a statement, nil otherwise. "
  (let ((orig (point)))
    (save-excursion
      (py-end-of-statement)
      (py-beginning-of-statement)
      (when (or (eq orig (point)) (and (looking-back "^[ \t]+") (<=  (line-beginning-position) orig)(<= orig (point))))
        (when (interactive-p)
          (message "%s" orig))
        orig))))

(defalias 'py-beginning-of-block-p 'py-statement-opens-block-p)
(defun py-statement-opens-block-p (&optional regexp)
  (interactive)
  "Return position if the current statement opens a block
in stricter or wider sense.
For stricter sense specify regexp. "
  (let* ((regexp (or regexp py-block-re))
        (erg (py-statement-opens-base regexp)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-beginning-of-clause-p 'py-statement-opens-clause-p)
(defun py-statement-opens-clause-p ()
  "Return position if the current statement opens block or clause. "
  (interactive)
  (py-statement-opens-base py-clause-re))

(defun py-statement-opens-base (regexp)
  (let ((orig (point))
        erg)
    (save-excursion
      (back-to-indentation)
      (py-end-of-statement)
      (py-beginning-of-statement)
      (when (and
             (looking-back "^[ \t]*") (<= (line-beginning-position)(point))(looking-at regexp))
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-beginning-of-block-or-clause-p 'py-statement-opens-block-or-clause-p)
(defun py-statement-opens-block-or-clause-p ()
  "Return position if the current statement opens block or clause. "
  (interactive)
  (py-statement-opens-base py-block-or-clause-re))


(defalias 'py-beginning-of-class-p 'py-statement-opens-class-p)
(defun py-statement-opens-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-class-re))

(defalias 'py-beginning-of-def-p 'py-statement-opens-def-p)
(defun py-statement-opens-def-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-def-re))

(defalias 'py-beginning-of-def-or-class-p 'py-statement-opens-def-or-class-p)
(defun py-statement-opens-def-or-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-def-or-class-re))

;; Mark forms
(defun py-mark-expression ()
  "Mark expression around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (py-mark-base "expression")
  (exchange-point-and-mark))

(defun py-mark-partial-expression ()
  "Mark partial-expression around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (py-mark-base "partial-expression")
  (exchange-point-and-mark))

(defun py-mark-statement ()
  "Mark statement around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (py-mark-base "statement")
  (exchange-point-and-mark))

(defun py-mark-block ()
  "Mark block around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (py-mark-base "block")
  (exchange-point-and-mark))

(defun py-mark-block-or-clause ()
  "Mark block-or-clause around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (py-mark-base "block-or-clause")
  (exchange-point-and-mark))

(defun py-mark-def (&optional arg)
  "Mark def around point.

With universal argument or `py-mark-decorators' set to `t' decorators are marked too.
Returns beginning and end positions of marked area, a cons."

  (interactive "P")
    (let ((py-mark-decorators (or arg py-mark-decorators)))
    (py-mark-base "def" py-mark-decorators)
    (exchange-point-and-mark)))

(defun py-mark-def-or-class (&optional arg)
  "Mark def-or-class around point.

With universal argument or `py-mark-decorators' set to `t' decorators are marked too.
Returns beginning and end positions of marked area, a cons."
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators)))
    (py-mark-base "def-or-class" py-mark-decorators)
    (exchange-point-and-mark)))

(defun py-mark-class (&optional arg)
  "Mark class around point.

With universal argument or `py-mark-decorators' set to `t' decorators are marked too.
Returns beginning and end positions of marked area, a cons."

  (interactive "P")
    (let ((py-mark-decorators (or arg py-mark-decorators)))
    (py-mark-base "class" py-mark-decorators)
    (exchange-point-and-mark)))

(defun py-mark-clause ()
  "Mark clause around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (py-mark-base "clause")
  (exchange-point-and-mark))

(defun py-mark-base (form &optional py-mark-decorators)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form)))
         (endform (intern-soft (concat "py-end-of-" form)))
         (begcheckform (intern-soft (concat "py-beginning-of-" form "-p")))
         (orig (point))
         beg end)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (forward-line -1)
      (back-to-indentation)
      (when (looking-at "@\\w+")
        (setq beg (match-beginning 0)))))
    (setq end (funcall endform))
    (progn
      (push-mark beg t t)
      (goto-char end))
    (when (interactive-p) (message "%s %s" beg end))
    (cons beg end)))

;; Copying
(defun py-copy-expression ()
  "Mark expression around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "expression")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defun py-copy-partial-expression ()
  "Mark partial-expression around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "partial-expression")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defun py-copy-statement ()
  "Mark statement around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "statement")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defun py-copy-block ()
  "Mark block around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "block")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defun py-copy-block-or-clause ()
  "Mark block-or-clause around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "block-or-clause")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defun py-copy-def (&optional arg)
  "Mark def around point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."
  
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "def" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defun py-copy-def-or-class (&optional arg)
  "Mark def-or-class around point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "def-or-class" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defun py-copy-class (&optional arg)
  "Mark class around point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."
  
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "class" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defun py-copy-clause ()
  "Mark clause around point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "clause")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))


;; Deleting
(defun py-kill-expression ()
  "Delete expression around point.
  Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive)
  (let ((erg (py-mark-base "expression")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-partial-expression ()
  "Delete partial-expression around point.
  Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive)
  (let ((erg (py-mark-base "partial-expression")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-statement ()
  "Delete statement around point.
  Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive)
  (let ((erg (py-mark-base "statement")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block ()
  "Delete block around point.
  Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive)
  (let ((erg (py-mark-base "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block-or-clause ()
  "Delete block-or-clause around point.
  Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive)
  (let ((erg (py-mark-base "block-or-clause")))
    (kill-region (region-beginning) (region-end))))

(defun py-kill-def-or-class ()
  "Delete def-or-class around point.
  Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive)
  (let ((erg (py-mark-base "def-or-class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-class ()
  "Delete class around point.
  Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive)
  (let ((erg (py-mark-base "class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-clause ()
  "Delete clause around point.
  Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive)
  (let ((erg (py-mark-base "clause")))
    (kill-region (car erg) (cdr erg))))


;; pdbtrack functions
(defun py-pdbtrack-toggle-stack-tracking (arg)
  (interactive "P")
  (if (not (get-buffer-process (current-buffer)))
      (error "No process associated with buffer '%s'" (current-buffer)))
  ;; missing or 0 is toggle, >0 turn on, <0 turn off
  (if (or (not arg)
          (zerop (setq arg (prefix-numeric-value arg))))
      (setq py-pdbtrack-do-tracking-p (not py-pdbtrack-do-tracking-p))
    (setq py-pdbtrack-do-tracking-p (> arg 0)))
  (message "%sabled Python's pdbtrack"
           (if py-pdbtrack-do-tracking-p "En" "Dis")))

(defun turn-on-pdbtrack ()
  (interactive)
  (py-pdbtrack-toggle-stack-tracking 1))

(defun turn-off-pdbtrack ()
  (interactive)
  (py-pdbtrack-toggle-stack-tracking 0))


;; Pychecker

;; hack for FSF Emacs
(unless (fboundp 'read-shell-command)
  (defalias 'read-shell-command 'read-string))

(defun py-pychecker-run (command)
  "*Run pychecker (default on the file currently visited)."
  (interactive
   (let ((default
           (format "%s %s %s" py-pychecker-command
                   (mapconcat 'identity py-pychecker-command-args " ")
                   (buffer-file-name)))
         (last (when py-pychecker-history
                 (let* ((lastcmd (car py-pychecker-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pychecker like this: "
                              (if last
                                  last
                                default)
                              'py-pychecker-history)
        (read-string "Run pychecker like this: "
                     (if last
                         last
                       default)
                     'py-pychecker-history))
        )))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))


(defalias 'py-describe-symbol 'py-help-at-point)
(defun py-help-at-point ()
  "Get help from Python based on the symbol nearest point."
  (interactive)
  (let* ((sym (prin1-to-string (symbol-at-point)))
         (base (substring sym 0 (or (string-match "\\." sym) 0)))
         cmd)
    (if (not (equal base ""))
        (setq cmd (concat "import " base "\n")))
    (setq cmd (concat "import pydoc\n"
                      cmd
                      "try: pydoc.help('" sym "')\n"
                      "except: print 'No help available on:', \"" sym "\""))
    (message cmd)
    (py-execute-string cmd)
    (set-buffer "*Python Output*")
    ;; BAW: Should we really be leaving the output buffer in help-mode?
    (help-mode)))


;; Documentation functions

;; dump the long form of the mode blurb; does the usual doc escapes,
;; plus lines of the form ^[vc]:name$ to suck variable & command docs
;; out of the right places, along with the keys they're on & current
;; values
(defun py-dump-help-string (str)
  (with-output-to-temp-buffer "*Help*"
    (let ((locals (buffer-local-variables))
          funckind funcname func funcdoc
          (start 0) mstart end
          keys )
      (while (string-match "^%\\([vc]\\):\\(.+\\)\n" str start)
        (setq mstart (match-beginning 0)  end (match-end 0)
              funckind (substring str (match-beginning 1) (match-end 1))
              funcname (substring str (match-beginning 2) (match-end 2))
              func (intern funcname))
        (princ (substitute-command-keys (substring str start mstart)))
        (cond
         ((equal funckind "c")          ; command
          (setq funcdoc (documentation func)
                keys (concat
                      "Key(s): "
                      (mapconcat 'key-description
                                 (where-is-internal func py-mode-map)
                                 ", "))))
         ((equal funckind "v")          ; variable
          (setq funcdoc (documentation-property func 'variable-documentation)
                keys (if (assq func locals)
                         (concat
                          "Local/Global values: "
                          (prin1-to-string (symbol-value func))
                          " / "
                          (prin1-to-string (default-value func)))
                       (concat
                        "Value: "
                        (prin1-to-string (symbol-value func))))))
         (t                             ; unexpected
          (error "Error in py-dump-help-string, tag `%s'" funckind)))
        (princ (format "\n-> %s:\t%s\t%s\n\n"
                       (if (equal funckind "c") "Command" "Variable")
                       funcname keys))
        (princ funcdoc)
        (terpri)
        (setq start end))
      (princ (substitute-command-keys (substring str start))))
    (help-print-return-message)))

(defun py-describe-mode ()
  "Dump long form of Python-mode docs."
  (interactive)
  (py-dump-help-string "Major mode for editing Python files.
Knows about Python indentation, tokens, comments and continuation lines.
Paragraphs are separated by blank lines only.

Major sections below begin with the string `@'; specific function and
variable docs begin with `->'.

@EXECUTING PYTHON CODE

\\[py-execute-import-or-reload]\timports or reloads the file in the Python interpreter
\\[py-execute-buffer]\tsends the entire buffer to the Python interpreter
\\[py-execute-region]\tsends the current region
\\[py-execute-def-or-class]\tsends the current function or class definition
\\[py-execute-string]\tsends an arbitrary string
\\[py-shell]\tstarts a Python interpreter window; this will be used by
\tsubsequent Python execution commands
%c:py-execute-import-or-reload
%c:py-execute-buffer
%c:py-execute-region
%c:py-execute-def-or-class
%c:py-execute-string
%c:py-shell

@VARIABLES

py-indent-offset\tindentation increment
py-block-comment-prefix\tcomment string used by comment-region

py-python-command\tshell command to invoke Python interpreter
py-temp-directory\tdirectory used for temp files (if needed)

py-beep-if-tab-change\tring the bell if tab-width is changed
%v:py-indent-offset
%v:py-block-comment-prefix
%v:py-python-command
%v:py-temp-directory
%v:py-beep-if-tab-change

@KINDS OF LINES

Each physical line in the file is either a `continuation line' (the
preceding line ends with a backslash that's not part of a comment, or
the paren/bracket/brace nesting level at the start of the line is
non-zero, or both) or an `initial line' (everything else).

An initial line is in turn a `blank line' (contains nothing except
possibly blanks or tabs), a `comment line' (leftmost non-blank
character is `#'), or a `code line' (everything else).

Comment Lines

Although all comment lines are treated alike by Python, Python mode
recognizes two kinds that act differently with respect to indentation.

An `indenting comment line' is a comment line with a blank, tab or
nothing after the initial `#'.  The indentation commands (see below)
treat these exactly as if they were code lines: a line following an
indenting comment line will be indented like the comment line.  All
other comment lines (those with a non-whitespace character immediately
following the initial `#') are `non-indenting comment lines', and
their indentation is ignored by the indentation commands.

Indenting comment lines are by far the usual case, and should be used
whenever possible.  Non-indenting comment lines are useful in cases
like these:

\ta = b   # a very wordy single-line comment that ends up being
\t        #... continued onto another line

\tif a == b:
##\t\tprint 'panic!' # old code we've `commented out'
\t\treturn a

Since the `#...' and `##' comment lines have a non-whitespace
character following the initial `#', Python mode ignores them when
computing the proper indentation for the next line.

Continuation Lines and Statements

The Python-mode commands generally work on statements instead of on
individual lines, where a `statement' is a comment or blank line, or a
code line and all of its following continuation lines (if any)
considered as a single logical unit.  The commands in this mode
generally (when it makes sense) automatically move to the start of the
statement containing point, even if point happens to be in the middle
of some continuation line.

@INDENTATION

Primarily for entering new code:
\t\\[indent-for-tab-command]\t indent line appropriately
\t\\[py-newline-and-indent]\t insert newline, then indent
\t\\[py-electric-backspace]\t reduce indentation, or delete single character

Primarily for reindenting existing code:
\t\\[py-guess-indent-offset]\t guess py-indent-offset from file content; change locally
\t\\[universal-argument] \\[py-guess-indent-offset]\t ditto, but change globally

\t\\[py-indent-region]\t reindent region to match its context
\t\\[py-shift-region-left]\t shift region left by py-indent-offset
\t\\[py-shift-region-right]\t shift region right by py-indent-offset

Unlike most programming languages, Python uses indentation, and only
indentation, to specify block structure.  Hence the indentation supplied
automatically by Python-mode is just an educated guess:  only you know
the block structure you intend, so only you can supply correct
indentation.

The \\[indent-for-tab-command] and \\[py-newline-and-indent] keys try to suggest plausible indentation, based on
the indentation of preceding statements.  E.g., assuming
py-indent-offset is 4, after you enter
\tif a > 0: \\[py-newline-and-indent]
the cursor will be moved to the position of the `_' (_ is not a
character in the file, it's just used here to indicate the location of
the cursor):
\tif a > 0:
\t    _
If you then enter `c = d' \\[py-newline-and-indent], the cursor will move
to
\tif a > 0:
\t    c = d
\t    _
Python-mode cannot know whether that's what you intended, or whether
\tif a > 0:
\t    c = d
\t_
was your intent.  In general, Python-mode either reproduces the
indentation of the (closest code or indenting-comment) preceding
statement, or adds an extra py-indent-offset blanks if the preceding
statement has `:' as its last significant (non-whitespace and non-
comment) character.  If the suggested indentation is too much, use
\\[py-electric-backspace] to reduce it.

Continuation lines are given extra indentation.  If you don't like the
suggested indentation, change it to something you do like, and Python-
mode will strive to indent later lines of the statement in the same way.

If a line is a continuation line by virtue of being in an unclosed
paren/bracket/brace structure (`list', for short), the suggested
indentation depends on whether the current line contains the first item
in the list.  If it does, it's indented py-indent-offset columns beyond
the indentation of the line containing the open bracket.  If you don't
like that, change it by hand.  The remaining items in the list will mimic
whatever indentation you give to the first item.

If a line is a continuation line because the line preceding it ends with
a backslash, the third and following lines of the statement inherit their
indentation from the line preceding them.  The indentation of the second
line in the statement depends on the form of the first (base) line:  if
the base line is an assignment statement with anything more interesting
than the backslash following the leftmost assigning `=', the second line
is indented two columns beyond that `='.  Else it's indented to two
columns beyond the leftmost solid chunk of non-whitespace characters on
the base line.

Warning:  indent-region should not normally be used!  It calls \\[indent-for-tab-command]
repeatedly, and as explained above, \\[indent-for-tab-command] can't guess the block
structure you intend.
%c:indent-for-tab-command
%c:py-newline-and-indent
%c:py-electric-backspace

The next function may be handy when editing code you didn't write:
%c:py-guess-indent-offset

The remaining `indent' functions apply to a region of Python code.  They
assume the block structure (equals indentation, in Python) of the region
is correct, and alter the indentation in various ways while preserving
the block structure:
%c:py-indent-region
%c:py-shift-region-left
%c:py-shift-region-right

@MARKING & MANIPULATING REGIONS OF CODE

\\[py-mark-block]\t mark block of lines
\\[py-mark-def-or-class]\t mark smallest enclosing def
\\[universal-argument] \\[py-mark-def-or-class]\t mark smallest enclosing class
\\[comment-region]\t comment out region of code
\\[universal-argument] \\[comment-region]\t uncomment region of code
%c:py-mark-block
%c:py-mark-def-or-class
%c:comment-region

@MOVING POINT

\\[py-previous-statement]\t move to statement preceding point
\\[py-next-statement]\t move to statement following point
\\[py-goto-block-up]\t move up to start of current block
\\[py-beginning-of-def-or-class]\t move to start of def
\\[universal-argument] \\[py-beginning-of-def-or-class]\t move to start of class
\\[py-end-of-def-or-class]\t move to end of def
\\[universal-argument] \\[py-end-of-def-or-class]\t move to end of class

The first two move to one statement beyond the statement that contains
point.  A numeric prefix argument tells them to move that many
statements instead.  Blank lines, comment lines, and continuation lines
do not count as `statements' for these commands.  So, e.g., you can go
to the first code statement in a file by entering
\t\\[beginning-of-buffer]\t to move to the top of the file
\t\\[py-next-statement]\t to skip over initial comments and blank lines
Or do `\\[py-previous-statement]' with a huge prefix argument.
%c:py-previous-statement
%c:py-next-statement
%c:py-goto-block-up
%c:py-beginning-of-def-or-class
%c:py-end-of-def-or-class

@LITTLE-KNOWN EMACS COMMANDS PARTICULARLY USEFUL IN PYTHON MODE

`\\[indent-new-comment-line]' is handy for entering a multi-line comment.

`\\[set-selective-display]' with a `small' prefix arg is ideally suited for viewing the
overall class and def structure of a module.

`\\[back-to-indentation]' moves point to a line's first non-blank character.

`\\[indent-relative]' is handy for creating odd indentation.

@OTHER EMACS HINTS

If you don't like the default value of a variable, change its value to
whatever you do like by putting a `setq' line in your .emacs file.
E.g., to set the indentation increment to 4, put this line in your
.emacs:
\t(setq  py-indent-offset  4)
To see the value of a variable, do `\\[describe-variable]' and enter the variable
name at the prompt.

When entering a key sequence like `C-c C-n', it is not necessary to
release the CONTROL key after doing the `C-c' part -- it suffices to
press the CONTROL key, press and release `c' (while still holding down
CONTROL), press and release `n' (while still holding down CONTROL), &
then release CONTROL.

Entering Python mode calls with no arguments the value of the variable
`python-mode-hook', if that value exists and is not nil; for backward
compatibility it also tries `py-mode-hook'; see the `Hooks' section of
the Elisp manual for details.

Obscure:  When python-mode is first loaded, it looks for all bindings
to newline-and-indent in the global keymap, and shadows them with
local bindings to py-newline-and-indent."))

(require 'info-look)
;; The info-look package does not always provide this function (it
;; appears this is the case with XEmacs 21.1)
(when (fboundp 'info-lookup-maybe-add-help)
  (info-lookup-maybe-add-help
   :mode 'python-mode
   :regexp "[a-zA-Z0-9_]+"
   :doc-spec '(("(python-lib)Module Index")
               ("(python-lib)Class-Exception-Object Index")
               ("(python-lib)Function-Method-Variable Index")
               ("(python-lib)Miscellaneous Index")))
  )


;; Helper functions
(when (featurep 'xemacs) 
  (unless (functionp 'looking-back)
    ;; from GNU Emacs subr.el
    (defun looking-back (regexp &optional limit greedy)
      "Return non-nil if text before point matches regular expression REGEXP.
    Like `looking-at' except matches before point, and is slower.
    LIMIT if non-nil speeds up the search by specifying a minimum
    starting position, to avoid checking matches that would start
    before LIMIT.
    If GREEDY is non-nil, extend the match backwards as far as possible,
    stopping when a single additional previous character cannot be part
    of a match for REGEXP."
      (let ((start (point))
            (pos
             (save-excursion
               (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
                    (point)))))
        (if (and greedy pos)
            (save-restriction
              (narrow-to-region (point-min) start)
              (while (and (> pos (point-min))
                          (save-excursion
                            (goto-char pos)
                            (backward-char 1)
                            (looking-at (concat "\\(?:"  regexp "\\)\\'"))))
                (setq pos (1- pos)))
              (save-excursion
                (goto-char pos)
                (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
        (not (null pos))))))

(defun py-count-lines (&optional start end)
  "Count lines in accessible part of buffer.

If no optional position arguments are given but region is active,
use the region.
Else count from point-min until curser position - (point)

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115"
  (interactive)
  (lexical-let ((beg (cond (start)
                           ((region-active-p)
                            (region-beginning))
                           (t (point-min))))
                (end (cond (end)
                           ((region-active-p)
                            (copy-marker (region-end)))
                           (t (point))))
                erg)
    (if (featurep 'xemacs)
        (setq erg (count-lines beg end))
      (setq erg (1+ (count-matches "[\n\C-m]" beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-forward-line (&optional arg)
  "Goes to end of line after forward move.
Travels right-margin comments. "
  (interactive "p")
  (let ((arg (or arg 1)))
    (forward-line arg)
    (end-of-line)
    (skip-chars-backward " \t")
    (py-beginning-of-comment)
    (skip-chars-backward " \t")))

(defvar py-parse-state-re
  (concat
   "^[ \t]*\\(elif\\|else\\|while\\|def\\|class\\)\\>"
   "\\|"
   "^[^ #\t\n]"))

(defun py-parse-state ()
  "Return the parse state at point (see `parse-partial-sexp' docs)."
  (save-excursion
    (let ((here (point))
          pps done)
      (while (not done)
        ;; back up to the first preceding line (if any; else start of
        ;; buffer) that begins with a popular Python keyword, or a
        ;; non- whitespace and non-comment character.  These are good
        ;; places to start parsing to see whether where we started is
        ;; at a non-zero nesting level.  It may be slow for people who
        ;; write huge code blocks or huge lists ... tough beans.
        (re-search-backward py-parse-state-re nil 'move)
        (beginning-of-line)
        ;; In XEmacs, we have a much better way to test for whether
        ;; we're in a triple-quoted string or not.  Emacs does not
        ;; have this built-in function, which is its loss because
        ;; without scanning from the beginning of the buffer, there's
        ;; no accurate way to determine this otherwise.
        (save-excursion (setq pps (parse-partial-sexp (point) here)))
        ;; make sure we don't land inside a triple-quoted string
        (setq done (or (not (nth 3 pps))
                       (bobp)))
        ;; Just go ahead and short circuit the test back to the
        ;; beginning of the buffer.  This will be slow, but not
        ;; nearly as slow as looping through many
        ;; re-search-backwards.
        (if (not done)
            (goto-char (point-min))))
      pps)))

(defun py-nesting-level ()
  "Return the buffer position of the last unclosed enclosing list.
If nesting level is zero, return nil."
  (let ((status (py-parse-state)))
    (if (zerop (car status))
        nil                             ; not in a nest
      (car (cdr status)))))             ; char# of open bracket

(defun py-backslash-continuation-preceding-line-p ()
  "Return t if preceding line ends with backslash. "
  (save-excursion
    (beginning-of-line)
    (and
     ;; use a cheap test first to avoid the regexp if possible
     ;; use 'eq' because char-after may return nil
     (eq (char-after (- (point) 2)) ?\\ )
     ;; make sure; since eq test passed, there is a preceding line
     (forward-line -1)                  ; always true -- side effect
     (looking-at py-continued-re))))

(defun py-continuation-line-p ()
  "Return t iff current line is a continuation line."
  (save-excursion
    (beginning-of-line)
    (or (py-backslash-continuation-preceding-line-p)
        (py-nesting-level))))

(defun py-backslashed-continuation-line-p ()
  (interactive)
  "Return t if current line is a backslashed continuation line. "
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward " \t\r\n\f")
    (let ((erg (eq (char-before (point)) ?\\ )))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-statement-closes-block-p ()
  "Return t iff the current statement closes a block.
I.e., if the line starts with `return', `raise', `break', `continue',
and `pass'.  This doesn't catch embedded statements."
  (let ((here (point)))
    (py-goto-initial-line)
    (back-to-indentation)
    (prog1
        (looking-at (concat py-block-closing-keywords-re "\\>"))
      (goto-char here))))

(defun py-goto-beyond-block ()
  "Go to point just beyond the final line of block begun by the current line. "
  (py-forward-block)
  (forward-line 1))

(defun py-beginning-of-comment (&optional count)
  "Go to the beginning of current line's comment, if any. "
  (interactive)
  (save-restriction
    (widen)
    (lexical-let ((pps (parse-partial-sexp (line-beginning-position) (point))))
      (when (nth 4 pps)
        (goto-char
         (nth 8 pps))))))

(defun py-current-defun (&optional arg)
  "Go to the outermost method or class definition in current scope. 

Python value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable.
Returns name of class or methods definition, if found, nil otherwise.

See customizable variables `py-current-defun-show' and `py-current-defun-delay'."
  (interactive "p")
  (save-restriction
    (widen)
    (save-excursion
      (while
          (or (not (py-beginning-of-def-or-class-p)) (< 0 (current-indentation)))
        (py-beginning-of-def-or-class 'either))
      (forward-word 1)
      (skip-chars-forward " \t")
      (let ((erg (prin1-to-string (symbol-at-point))))
        (when py-current-defun-show (push-mark (point) t t) (skip-chars-forward "^ (")
              (exchange-point-and-mark)
              (sit-for py-current-defun-delay))
        (when arg (message erg))
        erg))))


(defconst py-help-address "python-mode@python.org"
  "Address accepting submission of bug reports.")

(defun py-version ()
  "Echo the current version of `python-mode' in the minibuffer."
  (interactive)
  (message "Using `python-mode' version %s" py-version)
  (py-keep-region-active))

;; only works under Emacs 19
;(eval-when-compile
;  (require 'reporter))

(defun py-submit-bug-report (enhancement-p)
  "Submit via mail a bug report on `python-mode'.
With \\[universal-argument] (programmatically, argument ENHANCEMENT-P
non-nil) just submit an enhancement request."
  (interactive
   (list (not (y-or-n-p
               "Is this a bug report (hit `n' to send other comments)? "))))
  (let ((reporter-prompt-for-summary-p (if enhancement-p
                                           "(Very) brief summary: "
                                         t)))
    (require 'reporter)
    (reporter-submit-bug-report
     py-help-address                    ;address
     (concat "python-mode " py-version) ;pkgname
     ;; varlist
     (if enhancement-p nil
       '(py-python-command
         py-indent-offset
         py-block-comment-prefix
         py-temp-directory
         py-beep-if-tab-change))
     nil                                ;pre-hooks
     nil                                ;post-hooks
     "Dear Barry,")                     ;salutation
    (if enhancement-p nil
      (set-mark (point))
      (insert
"Please replace this text with a sufficiently large code sample\n\
and an exact recipe so that I can reproduce your problem.  Failure\n\
to do so may mean a greater delay in fixing your bug.\n\n")
      (exchange-point-and-mark)
      (py-keep-region-active))))


(defun py-kill-emacs-hook ()
  "Delete files in `py-file-queue'.
These are Python temporary files awaiting execution."
  (mapc #'(lambda (filename)
            (ignore-errors (delete-file filename)))
        py-file-queue))

;; arrange to kill temp files when Emacs exists
(add-hook 'kill-emacs-hook 'py-kill-emacs-hook)
(add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)

;; Add a designator to the minor mode strings
(or (assq 'py-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(py-pdbtrack-is-tracking-p py-pdbtrack-minor-mode-string)
          minor-mode-alist))


;;; paragraph and string filling code from Bernhard Herzog
;;; see http://mail.python.org/pipermail/python-list/2002-May/103189.html

(defun py-fill-comment (&optional justify)
  "Fill the comment paragraph around point"
  (let (;; Non-nil if the current line contains a comment.
        has-comment

        ;; If has-comment, the appropriate fill-prefix for the comment.
        comment-fill-prefix)

    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond
       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*#[# \t]*")
        (setq has-comment t
              comment-fill-prefix (buffer-substring (match-beginning 0)
                                                    (match-end 0))))

       ;; A line with some code, followed by a comment? Remember that the hash
       ;; which starts the comment shouldn't be part of a string or character.
       ((progn
          (while (not (looking-at "#\\|$"))
            (skip-chars-forward "^#\n\"'\\")
            (cond
             ((eq (char-after (point)) ?\\) (forward-char 2))
             ((memq (char-after (point)) '(?\" ?')) (forward-sexp 1))))
          (looking-at "#+[\t ]*"))
        (setq has-comment t)
        (setq comment-fill-prefix
              (concat (make-string (current-column) ? )
                      (buffer-substring (match-beginning 0) (match-end 0)))))))

    (if (not has-comment)
        (fill-paragraph justify)

      ;; Narrow to include only the comment, and then fill the region.
      (save-restriction
        (narrow-to-region

         ;; Find the first line we should include in the region to fill.
         (save-excursion
           (while (and (zerop (forward-line -1))
                       (looking-at "^[ \t]*#")))

           ;; We may have gone to far.  Go forward again.
           (or (looking-at "^[ \t]*#")
               (forward-line 1))
           (point))

         ;; Find the beginning of the first line past the region to fill.
         (save-excursion
           (while (progn (forward-line 1)
                         (looking-at "^[ \t]*#")))
           (point)))

        ;; Lines with only hashes on them can be paragraph boundaries.
        (let ((paragraph-start (concat paragraph-start "\\|[ \t#]*$"))
              (paragraph-separate (concat paragraph-separate "\\|[ \t#]*$"))
              (fill-prefix comment-fill-prefix))
          ;;(message "paragraph-start %S paragraph-separate %S"
          ;;paragraph-start paragraph-separate)
          (fill-paragraph justify))))
    t))

(defun py-fill-string (start &optional justify)
  "Fill the paragraph around (point) in the string starting at start"
  ;; basic strategy: narrow to the string and call the default
  ;; implementation
  (let (;; the start of the string's contents
        string-start
        ;; the end of the string's contents
        string-end
        ;; length of the string's delimiter
        delim-length
        ;; The string delimiter
        delim)

    (save-excursion
      (goto-char start)
      (if (looking-at "\\([urbURB]*\\(?:'''\\|\"\"\"\\|'\\|\"\\)\\)\\\\?\n?")
          (setq string-start (match-end 0)
                delim-length (- (match-end 1) (match-beginning 1))
                delim (buffer-substring-no-properties (match-beginning 1)
                                                      (match-end 1)))
        (error "The parameter start is not the beginning of a python string"))

      ;; if the string is the first token on a line and doesn't start with
      ;; a newline, fill as if the string starts at the beginning of the
      ;; line. this helps with one line docstrings
      (save-excursion
        (beginning-of-line)
        (and (/= (char-before string-start) ?\n)
             (looking-at (concat "[ \t]*" delim))
             (setq string-start (point))))

      ;; move until after end of string, then the end of the string's contents
      ;; is delim-length characters before that
      (forward-sexp)
      (setq string-end (- (point) delim-length)))

    ;; Narrow to the string's contents and fill the current paragraph
    (save-restriction
      (narrow-to-region string-start string-end)
      (let ((ends-with-newline (= (char-before (point-max)) ?\n)))
        (fill-paragraph justify)
        (if (and (not ends-with-newline)
                 (= (char-before (point-max)) ?\n))
            ;; the default fill-paragraph implementation has inserted a
            ;; newline at the end. Remove it again.
            (save-excursion
              (goto-char (point-max))
              (delete-char -1)))))

    ;; return t to indicate that we've done our work
    t))

(defun py-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Python comments and strings.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial `#'s.
If point is inside a string, narrow to that string and fill.
"
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (let ((pps (parse-partial-sexp (point-min) (point))))
        (cond
         ;; inside a comment 
         ((nth 4 pps)
          (py-fill-comment justify))
         ;; only whitespace before the comment start
         ((save-excursion (beginning-of-line) (looking-at "[ \t]*#"))
          (py-fill-comment justify))
         ;; inside a string
         ((nth 3 pps)
          (py-fill-string (nth 8 pps)))
         ;; opening quote of a string
         ((progn (save-excursion (forward-char 1)(nth 3 pps)))
          (save-excursion
            (forward-char 1)
            (py-fill-string (nth 8 pps)))))))))

;; credits to python.el
(defun py-beg-of-defun-function ()
  (set (make-local-variable 'beginning-of-defun-function)
       'py-beginning-of-def-or-class))

(defun py-end-of-defun-function ()
  (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class))

(add-hook 'python-mode-hook 'py-beg-of-defun-function)
(add-hook 'python-mode-hook 'py-end-of-defun-function)

(provide 'python-mode)
;;; python-mode.el ends here
