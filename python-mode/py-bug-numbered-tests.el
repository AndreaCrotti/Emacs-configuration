;;; py-bug-numbered-tests.el --- run single tests according to bug number

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;;
;;; Code:

(add-to-list 'load-path default-directory)
(require 'python-mode)
(defvar bug-numbered-tests nil
  "Tests following reports at https://bugs.launchpad.net/python-mode")

(setq bug-numbered-tests
      (if (featurep 'xemacs)
          (list
           'bullet-lists-in-comments-lp:328782-test
           'fill-paragraph-problems-lp:710373-test
           'nested-indents-lp:328775-test
           'previous-statement-lp:637955-test)
        (list
         'mark-block-region-lp:328806-test
         'mark-decorators-lp:328851-test
         'nested-dictionaries-indent-lp:328791-test
         'triple-quoted-string-dq-lp:302834-test
         'fore-00007F-breaks-indentation-lp:328788-test
         'dq-in-tqs-string-lp:328813-test
         'flexible-indentation-lp:328842-test
         'py-current-defun-lp:328846-test
         'cls-pseudo-keyword-lp:328849-test
         'hungry-delete-backwards-lp:328853-test
         'hungry-delete-forward-lp:328853-test
         'beg-end-of-defun-lp:303622-test
         'bullet-lists-in-comments-lp:328782-test
         'imenu-newline-arglist-lp:328783-test
         'imenu-matches-in-docstring-lp:436285-test
         'exceptions-not-highlighted-lp:473525-test
         'UnicodeEncodeError-lp:550661-test
         'fill-paragraph-problems-lp:710373-test
         'nested-indents-lp:328775-test
         'previous-statement-lp:637955-test
         'inbound-indentation-multiline-assignement-lp:629916-test
         'indentation-of-continuation-lines-lp:691185-test
         'syntaxerror-on-py-execute-region-lp:691542-test
         'goto-beginning-of-tqs-lp:735328-test
         'class-treated-as-keyword-lp:709478-test
         'py-decorators-face-lp:744335-test
         'indent-after-return-lp:745208-test
         'keep-assignements-column-lp:748198-test
         'indent-triplequoted-to-itself-lp:752252-test
         'multiline-listings-indent-lp:761946-test
         'new-page-char-causes-loop-lp:762498-test
         'nested-dicts-indent-lp:763756-test
         'bad-indent-after-except-lp:771289-test
         'indent-open-paren-not-last-lp:771291-test
         
         )))


(defun py-run-bug-numbered-tests (&optional arg)
  "With ARG greater 1 keep test buffers open. "
  (interactive "p")
  (dolist (ele bug-numbered-tests)
    (funcall ele arg)))

(defun py-bug-tests-intern (testname &optional arg teststring)
  (if arg
      (progn
        (set-buffer (get-buffer-create (replace-regexp-in-string "-base$" "-test" (prin1-to-string testname))))
        (switch-to-buffer (current-buffer))
        (erase-buffer)
        (insert teststring)
        (fundamental-mode)
        (python-mode)
        (funcall testname)
        (message "%s" (concat (replace-regexp-in-string "-base$" "-test" (prin1-to-string testname)) " passed"))
        (unless (< 1 arg)
          (set-buffer-modified-p 'nil)
          (when (get-process py-which-bufname)
            (kill-process (get-process py-which-bufname)))
          (kill-buffer (current-buffer))))
    (with-temp-buffer
      (let ((font-lock-verbose nil))
        (insert teststring)
        (funcall testname)))))


(defun sexp-commands-lp:328778-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked.

Reported by Montanaro on 2003-08-05
\[ ... ]
 You can kill balanced expressions on a
 particular line but it's not possible to remove the
 whole of an 'if' or 'while' block."
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):                          
    grammar = \"kant.xml\"                
    try:                                
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:          
        usage() 
        sys.exit(2) 
    for opt, arg in opts:                
        if opt in (\"-h\", \"--help\"):      
            usage() 
            sys.exit() 
        elif opt == '-d':                
            global _debug 
            _debug = 1 
        elif opt in (\"-g\", \"--grammar\"): 
            grammar = arg 
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'sexp-commands-lp:328778 arg teststring)))

(defun sexp-commands-lp:328778 ()
  (let ((size (buffer-size)))
    (goto-char (point-min))
    (forward-line 15)
    (py-kill-clause)
    (assert (< (buffer-size) size) nil "sexp-commands-lp:328778 test failed")
    (assert (eq (buffer-size) 526) nil "sexp-commands-lp:328778 test failed")
    (kill-line 1)
    (indent-according-to-mode)
    (forward-line -4)
    (py-kill-block)
    (assert (eq (buffer-size) 324) nil "sexp-commands-lp:328778 test failed")
    ))

(defun nested-dictionaries-indent-lp:328791-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 

If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
    d = {'a':{'b':3,
              'c':4}}
"))
    (py-bug-tests-intern 'nested-dictionaries-indent-lp:328791 arg teststring)))

(defun nested-dictionaries-indent-lp:328791 ()
  (let ((py-indent-honors-multiline-listing t))
    (goto-char (point-min))
    (forward-line 2)
    (assert (eq 14 (py-compute-indentation)))))

(defun mark-block-region-lp:328806-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "def f():
    \"\"\"
    class blah blah
    \"\"\"
    if a:
        ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
                'python-partial-expression',
                'python-statement',
                ])
"))
    (py-bug-tests-intern 'mark-block-region-lp:328806 arg teststring)))

(defun mark-block-region-lp:328806-base ()
  (forward-line -2)
  (py-mark-block)
  (assert (< (region-beginning) (region-end)) nil "mark-block-region-lp:328806 test failed!"))

(defun flexible-indentation-lp:328842-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "
\(long, sequence, of_items,
    that, needs, to_be, wrapped) = input_list

packed_entry = (long, sequence, of_items,
that, needs, to_be, wrapped)

\( whitespaced, long, sequence, of_items,
    that, needs, to_be, wrapped) = input_list
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'flexible-indentation-lp:328842 arg teststring)))

(defun flexible-indentation-lp:328842 ()
  (let ((py-indent-honors-multiline-listing t))
    (goto-char (point-min))
    (forward-line 2)
    (indent-according-to-mode)
    (assert (eq 1 (current-indentation)) nil "flexible-indentation-lp:328842 test failed")
    (forward-line 3)
    (indent-according-to-mode)
    (assert (eq 16 (current-indentation)) nil "flexible-indentation-lp:328842 test failed")
    (forward-line 3)
    (indent-according-to-mode)
    (assert (eq 2 (current-indentation)) nil "flexible-indentation-lp:328842 test failed")))

(defun py-current-defun-lp:328846-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-current-defun-lp:328846-base arg teststring)))

(defun py-current-defun-lp:328846-base ()
  (goto-char 331)
  (assert (string= "OrderedDict1" (py-current-defun)) nil "py-current-defun-lp:328846 test failed"))

(defun cls-pseudo-keyword-lp:328849-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class Foo(object):
    def summat(cls, x):
          .....
    summat = classmethod(summat)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'cls-pseudo-keyword-lp:328849-base arg teststring)))

(defun cls-pseudo-keyword-lp:328849-base ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 36)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-pseudo-keyword-face) nil "cls-pseudo-keyword-lp:328849 test failed ")))

(defun mark-decorators-lp:328851-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "@foo.bar
def baz():
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'mark-decorators-lp:328851-base arg teststring)))

(defun mark-decorators-lp:328851-base ()
  (goto-char 10)
  (py-mark-def t)
  (assert (eq 28 (- (region-end)(region-beginning))) nil "mark-decorators-lp:328851 test failed"))

(defun beg-end-of-defun-lp:303622-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
class f():
    \"\"\"
    class blah blah
    \"\"\"
    if a:
        ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
                'python-partial-expression',
                'python-statement',
                ])
"))
    (py-bug-tests-intern 'beg-end-of-defun-lp:303622 arg teststring)))

(defun beg-end-of-defun-lp:303622 ()
  (goto-char (point-min))
  (forward-line 2)
  (end-of-defun)
  (assert (eq 292 (point)) nil "beg-end-of-defun-lp:303622 test failed!")
  (beginning-of-defun)
  (assert (eq 2 (point)) nil "beg-end-of-defun-lp:303622 test failed!"))

(defun dq-in-tqs-string-lp:328813-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
# Bug #328813 (sf1775975)
print \"\"\" \"Hi!\" I'm a doc string\"\"\"
print ''' 'Hi!' I'm a doc string'''
print \"\"\" ''' \"Hi!\" I'm a doc string ''' \"\"\"
print ''' \"\"\" \"Hi!\" I'm a doc string \"\"\" '''
"))
    (py-bug-tests-intern 'dq-in-tqs-string-lp:328813 arg teststring)))

(defun dq-in-tqs-string-lp:328813 ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 78)
    (let ((erg (get-char-property (point) 'face)))
      (message "%s" erg)
      (insert "\"")
      (font-lock-fontify-buffer)
      (message "%s" erg)
      (message "%s" (get-char-property (point) 'face))
      (assert (eq erg (get-char-property (point) 'face)) nil "dq-in-tqs-string-lp:328813 test failed ")
      (goto-char 122))))

(defun imenu-matches-in-docstring-lp:436285-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "
class foo():
    \"\"\"
    class hello(object):
        def __init__(self):
        ...
    \"\"\"
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'imenu-matches-in-docstring-lp:436285-base arg teststring)))

(defun imenu-matches-in-docstring-lp:436285-base ()
  (goto-char 40)  
  (assert (eq (py-beginning-of-def-or-class) 2) nil "imenu-matches-in-docstring-lp:436285 test failed"))

(defun fill-paragraph-problems-lp:710373-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
    \"\"\"
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    \"\"\"
"))
    (fill-paragraph-problems-lp:710373-test-intern arg teststring)))

(defun fill-paragraph-problems-lp:710373-test-intern (arg teststring)
  (let ((tmp-dir "/tmp/")
        (fpp-exec-buffer "fill-paragraph-problems-lp:710373")
        (diff-buffer "fpp-lp:710373-old"))
    (set-buffer (get-buffer-create diff-buffer))
    (erase-buffer)
    (fundamental-mode)
    (insert teststring)
    (write-file (concat tmp-dir diff-buffer))
    (if arg
        (progn
          (set-buffer (get-buffer-create fpp-exec-buffer))
          (switch-to-buffer (current-buffer))
          (erase-buffer)
          (insert teststring)
          (fundamental-mode)
          (fill-paragraph-problems-lp:710373-test-base arg tmp-dir fpp-exec-buffer diff-buffer)
          )
      (with-temp-buffer
        (insert teststring)
        (fill-paragraph-problems-lp:710373-test-base arg tmp-dir fpp-exec-buffer diff-buffer)))))

(defun fill-paragraph-problems-lp:710373-test-base (arg tmp-dir fpp-exec-buffer diff-buffer)
  (goto-char 48)
  (if (functionp 'py-fill-paragraph)
      (py-fill-paragraph)
    (python-fill-paragraph))
  (write-file (concat tmp-dir fpp-exec-buffer))
  (diff (concat tmp-dir fpp-exec-buffer) (concat tmp-dir diff-buffer) "-u")
  (if (featurep 'xemacs)
      (progn
        (set-buffer "*Diff Output*")
        (switch-to-buffer (current-buffer)))
    (set-buffer "*Diff*")
    (sit-for 1)
    (assert (numberp (progn (goto-char (point-min))(search-forward "no differences" nil t 1))) t)
    (message "%s" "fill-paragraph-problems-lp:710373 passed"))
  (set-buffer "fill-paragraph-problems-lp:710373")
  (unless (< 1 arg)
    (set-buffer-modified-p 'nil)
    (kill-buffer (current-buffer))))

(defun triple-quoted-string-dq-lp:302834-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\""))
    (py-bug-tests-intern 'triple-quoted-string-dq-lp:302834 arg teststring)))

(defun triple-quoted-string-dq-lp:302834 ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 78)
    (let ((erg (get-char-property (point) 'face)))
      (insert "\"")
      (font-lock-fontify-buffer)
      (assert (eq erg (get-char-property (point) 'face)) "Being stuck inside triple-quoted-string. Did not reach beginning of class."))))

(defun inbound-indentation-multiline-assignement-lp:629916-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "foo_long_long_long_long = (
    bar_long_long_long_long[
        (x_long_long_long_long == X) &
        (y_long_long_long_long == Y)])
"))
    (py-bug-tests-intern 'inbound-indentation-multiline-assignement-lp:629916 arg teststring)))


(defun inbound-indentation-multiline-assignement-lp:629916 ()
  (let ((py-indent-honors-multiline-listing t))
    (goto-char (point-min))
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq 27 (current-indentation)) nil "inbound-indentation-multiline-assignement-lp:629916 test failed")
    (end-of-line)
    (search-backward "[")
    (newline)
    (indent-according-to-mode)
    (assert (eq 27 (current-indentation)) nil "inbound-indentation-multiline-assignement-lp:629916 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq 28 (current-indentation)) nil "inbound-indentation-multiline-assignement-lp:629916 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq 28 (current-indentation)) nil "inbound-indentation-multiline-assignement-lp:629916 test failed")))

(defun previous-statement-lp:637955-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\""))
    (py-bug-tests-intern 'previous-statement-lp:637955 arg teststring)))

(defun previous-statement-lp:637955 ()
  (beginning-of-line)
  (py-previous-statement)
  (assert (eq 31 (point)) nil "Being stuck inside triple-quoted-string 637955 test. Did not reach beginning of class.")
  )

(defun nested-indents-lp:328775-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
if x > 0:
    for i in range(100):
        print i
    else:
    print \"All done\"
elif x < 0:
    print \"x is negative\"
"))
    (py-bug-tests-intern 'nested-indents-lp:328775 arg teststring)))

(defun nested-indents-lp:328775 ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (assert (eq 4 (py-compute-indentation)) nil "nested-indents-lp:328775 test failed!")
    (goto-char 41)
    (assert (eq 8 (py-compute-indentation)) nil "nested-indents-lp:328775 test failed!")
    (forward-line 1)
    (assert (eq 4 (py-compute-indentation)) nil "nested-indents-lp:328775 test failed!")))

(defun bullet-lists-in-comments-lp:328782-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring))
    (bullet-lists-in-comments-lp:328782-test-intern arg teststring)))

(defun bullet-lists-in-comments-lp:328782-test-intern (&optional arg teststring)
  (let ((font-lock-verbose nil))
    (set-buffer (get-buffer-create "bullet-lists-in-comments-lp:328782-test"))
    (erase-buffer)
    (with-temp-buffer
      (insert "
## * If the filename is a directory and not a Maildir nor
##   an MH Mailbox, it will be processed as a Mailbox --this bug named here: bullet-lists-in-comments-lp:328782.htm--
##   directory consisting of just .txt and .lorien files.
")
      (when arg (switch-to-buffer (current-buffer)))
      (python-mode)
      (font-lock-mode 1)
      (font-lock-fontify-buffer)
      (goto-char 100)
      (if (functionp 'py-fill-paragraph)
          (py-fill-paragraph)
        (python-fill-paragraph)))
    (set-buffer "bullet-lists-in-comments-lp:328782-test")
    (unless (< 1 arg)
      (set-buffer-modified-p 'nil)
      (kill-buffer (current-buffer)))))

(defun imenu-newline-arglist-lp:328783-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "def editor(db, db_name, table_name,
    #api
    dbapi,dbapi_exceptions):
        pass"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'imenu-newline-arglist-lp:328783-base arg teststring)))

(defun imenu-newline-arglist-lp:328783-base ()
  (goto-char 60)
  (assert (eq (py-beginning-of-def-or-class) 1) nil "imenu-newline-arglist-lp:328783 test failed"))

(defun hungry-delete-backwards-lp:328853-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'hungry-delete-backwards-lp:328853 arg teststring)))

(defun hungry-delete-backwards-lp:328853 ()
  (goto-char 421)
  (py-hungry-delete-backwards)
  (assert (eq 416 (point)) nil "hungry-delete-backwards test failed"))

(defun hungry-delete-forward-lp:328853-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'hungry-delete-forward-lp:328853 arg teststring)))

(defun hungry-delete-forward-lp:328853 ()
  (goto-char 409)
  (py-hungry-delete-forward)
  (assert (looking-at "#") nil "hungry-delete-backwards test failed"))

(defun UnicodeEncodeError-lp:550661-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((newbuf "/UnicodeEncodeError-lp:550661-test.py")
        (default-directory "~/arbeit/emacs/python-modes/components-python-mode/")
        erg kill-buffer-query-functions)
    (when (buffer-live-p (get-buffer "*Python*"))
      (set-buffer "*Python*")
      (when (processp (get-process "Python"))
        (set-process-query-on-exit-flag (get-process "Python") nil)
;;        (process-kill-without-query (get-process "Python"))
)
      ;;      (kill-process "*Python*")
      (set-buffer-modified-p 'nil)
      (kill-buffer "*Python*"))
    (set-buffer (get-buffer-create newbuf))
    (erase-buffer)
    (insert-file-contents (concat default-directory "/" "UnicodeEncodeError-lp:550661-test.py") nil nil nil t)
    (goto-char 48)
    (push-mark)
    (end-of-line)
    (setq erg (py-execute-region (line-beginning-position) (point)))))


(defun indentation-of-continuation-lines-lp:691185-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "    def f(val):
        # current behavior - indent to just after the first space
        a_verry_loonng_variable_nammmee = \\
                                        val
"))
    (py-bug-tests-intern 'indentation-of-continuation-lines-lp:691185 arg teststring)))

(defun indentation-of-continuation-lines-lp:691185 ()
  (let ((py-continuation-offset 2))
    (goto-char (point-min))
    (forward-line 3)
    (indent-according-to-mode)  
    (assert (eq 10 (current-indentation)) nil "indentation-of-continuation-lines-lp:691185-test failed!")))

(defun goto-beginning-of-tqs-lp:735328-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class Foo(object):
\"\"\"
This docstring isn't indented, test should pass anyway.
\"\"\"
"))
    (py-bug-tests-intern 'goto-beginning-of-tqs-lp:735328 arg teststring)))

(defun goto-beginning-of-tqs-lp:735328 ()
  (goto-char (point-min))
  (forward-line 4)
  (indent-according-to-mode)
  (assert (eq 4 (current-column)) nil "goto-beginning-of-tqs-lp:735328-test failed")
  )

(defun class-treated-as-keyword-lp:709478-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "foo = [
    T.div(
        T.tabl(*trows),

        CLASS='blok',)
]
"))
    (py-bug-tests-intern 'class-treated-as-keyword-lp:709478 arg teststring)))

(defun class-treated-as-keyword-lp:709478 ()
  (let ((font-lock-verbose nil))
    (font-lock-fontify-buffer)
    (goto-char 63)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'font-lock-string-face) nil "class-treated-as-keyword-lp:709478d 1th test failed")
    (goto-char 57)
    (assert (not (get-char-property (point) 'face)) nil "class-treated-as-keyword-lp:709478-test 2th failed")))


(defun fore-00007F-breaks-indentation-lp:328788-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "class a:
    def __init__(self):
        self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
            self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
                self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
                    self.StyleSetSpec(self.STYLE_FIELD,
\"fore:#00007F\" )
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'fore-00007F-breaks-indentation-lp:328788 arg teststring)))

(defun fore-00007F-breaks-indentation-lp:328788 ()
  (goto-char (point-min))
  (forward-line 1)
  (indent-according-to-mode)
  (forward-line 1)
  (assert (eq 8 (py-compute-indentation)) nil "fore-00007F-breaks-indentation test failed") 
  (indent-according-to-mode)
  (forward-line 1)
  (indent-according-to-mode)
  (forward-line 1)
  (assert (eq 8 (py-compute-indentation)) nil "fore-00007F-breaks-indentation test failed")
  (indent-according-to-mode)
  (forward-line 1)
  (indent-according-to-mode)
  (forward-line 1)
  (assert (eq 8 (py-compute-indentation)) nil "fore-00007F-breaks-indentation test failed")
  (indent-according-to-mode)
  (forward-line 1)
  (indent-according-to-mode)
  (forward-line 1)
  (assert (eq 8 (py-compute-indentation)) nil "fore-00007F-breaks-indentation test failed")
  (indent-according-to-mode)
  (forward-line 1)
  (indent-according-to-mode)
  )

(defun exceptions-not-highlighted-lp:473525-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "excs = (SystemExit, Exception, KeyboardInterrupt)"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'exceptions-not-highlighted-lp:473525 arg teststring)))

(defun exceptions-not-highlighted-lp:473525 ()
  (let ((font-lock-verbose nil))
    (goto-char 39)
    (font-lock-fontify-buffer)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-exception-name-face) nil "exceptions-not-highlighted-lp:473525 test failed")))

(defun syntaxerror-on-py-execute-region-lp:691542-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# -*- coding: utf-8 -*-
print \"Poet Friedrich Hölderlin\"
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'syntaxerror-on-py-execute-region-lp:691542-base arg teststring)))

(defun syntaxerror-on-py-execute-region-lp:691542-base ()
  (let ((oldbuf (current-buffer))
        ;;        (default-directory "~/arbeit/emacs/python-modes/components-python-mode/")
        erg kill-buffer-query-functions py-switch-to-python)
    (when (buffer-live-p (get-buffer "*Python*"))
      (set-buffer "*Python*")
      (when (processp (get-process "Python"))
        (set-process-query-on-exit-flag (get-process "Python") nil)
        ;;        (process-kill-without-query (get-process "Python"))
        )
      ;;      (kill-process "*Python*")
      (set-buffer-modified-p 'nil)
      (kill-buffer "*Python*"))
    (set-buffer oldbuf)
    (forward-line -1)
    (py-execute-region (line-beginning-position) (line-end-position))
    (switch-to-buffer (current-buffer))
    (assert (search-forward "Hölderlin") nil "syntaxerror-on-py-execute-region-lp:691542 test failed")))

(defun backslashed-continuation-line-indent-lp:742993-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "
self.last_abc_attr = \
self.last_xyz_attr = \
self.last_abc_other = \
self.last_xyz_other = None

self.last_abc_attr = \\
self.last_xyz_attr = \\
self.last_abc_other = \\
self.last_xyz_other = None

self.last_abc_attr = \\
self.last_xyz_attr = \\
self.last_abc_other = \\
self.last_xyz_other = None

self.last_abc_attr = \\
self.last_xyz_attr = \\
self.last_abc_other = \\
self.last_xyz_other = None
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'backslashed-continuation-line-indent-lp:742993 arg teststring)))

(defun backslashed-continuation-line-indent-lp:742993 ()
  (let ((py-continuation-offset 2))
    (goto-char (point-min))
    (forward-line 2)
    (insert (concat "\n# py-continuation-offset: " (number-to-string py-continuation-offset)))
    (forward-line 2)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    
    (setq py-continuation-offset 4)
    (forward-line 1)
    (insert (concat "\n# py-continuation-offset: " (number-to-string py-continuation-offset)))
    (forward-line 2)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")

    (setq py-continuation-offset 6)
    (forward-line 1)
    (insert (concat "\n# py-continuation-offset: " (number-to-string py-continuation-offset)))
    (forward-line 2)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    (forward-line 1)
    (indent-according-to-mode)
    (assert (eq (current-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993 test failed")
    ))

(defun py-decorators-face-lp:744335-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "@foo.bar
def baz():
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-decorators-face-lp:744335 arg teststring)))

(defun py-decorators-face-lp:744335 ()
  (let ((font-lock-verbose nil))
    (goto-char 7)
    (font-lock-fontify-buffer)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-decorators-face) nil "py-decorators-face-lp:744335 test failed")))

(defun indent-after-return-lp:745208-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "class FOO\():
    if len(sys.argv)==1:
        usage\()
        sys.exit\()

    def build_extension\(self, ext):

        if ext.name == '_ctypes':
            if not self.configure_ctypes\(ext):
                return

        try:
            build_ext.build_extension\(self, ext)
        except \(CCompilerError, DistutilsError) as why:
            self.announce\('WARNING: building of extension \"%s\"
failed: %s' %
                          \(ext.name, sys.exc_info()\[1]))
            self.failed.append(ext.name)
            return
        # Workaround for Mac OS X: The Carbon-based modules cannot be
        # reliably imported into a command-line Python
        if 'Carbon' in ext.extra_link_args:
            self.announce\(
                'WARNING: skipping import check for Carbon-based
\"%s\"' %
                ext.name)
            return
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-after-return-lp:745208 arg teststring)))

(defun indent-after-return-lp:745208 ()
  (goto-char (point-max))
  (assert (eq 4 (py-compute-indentation)) nil "indent-after-return-lp:745208 test failed")
  )

(defun keep-assignements-column-lp:748198-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "bar = foo(a=1,
          b=2,
          c=3)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'keep-assignements-column-lp:748198 arg teststring)))

(defun keep-assignements-column-lp:748198 ()
  (goto-char 45)
  (py-newline-and-indent)
  (assert (eq 0 (current-column)) nil "py-vor test failed"))

(defun indent-triplequoted-to-itself-lp:752252-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open. 
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "def foo():
    \"\"\"The real foo thing.\n"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-triplequoted-to-itself-lp:752252-base arg teststring)))

(defun indent-triplequoted-to-itself-lp:752252-base ()
  (assert (eq 4 (py-compute-indentation)) nil "indent-triplequoted-to-itself-lp:752252 test failed"))

(defun multiline-listings-indent-lp:761946-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    do_something_first(
        a=1,
                       b=2,
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'multiline-listings-indent-lp:761946-base arg teststring)))

(defun multiline-listings-indent-lp:761946-base ()
  (goto-char (point-min))
  (forward-line 3)
  (back-to-indentation)
  (assert (eq 8 (py-compute-indentation)) nil "multiline-listings-indent-lp:761946 test failed"))

(defun new-page-char-causes-loop-lp:762498-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class Foo:
    def baz(self):


"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'new-page-char-causes-loop-lp:762498-base arg teststring)))

(defun new-page-char-causes-loop-lp:762498-base ()
  (goto-char (point-min))
  (forward-line 2)
  (assert (eq 8 (py-compute-indentation)) "new-page-char-causes-loop-lp:762498 test failed"))

(defun nested-dicts-indent-lp:763756-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }
))
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'nested-dicts-indent-lp:763756-base arg teststring)))

(defun nested-dicts-indent-lp:763756-base ()
  (let ((py-indent-honors-multiline-listing nil))
    (goto-char (point-min))
    (forward-line 1)
    (assert (eq 4 (py-compute-indentation)) nil "nested-dicts-indent-lp:763756 test failed")
    (forward-line 1)
    (assert (eq 8 (py-compute-indentation)) nil "nested-dicts-indent-lp:763756 test failed")
    (forward-line 1)
    (assert (eq 12 (py-compute-indentation)) nil "nested-dicts-indent-lp:763756 test failed")))

(defun bad-indent-after-except-lp:771289-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    try:
        baz()
    except ValueError:
"))
  (when load-branch-function (funcall load-branch-function))
  (py-bug-tests-intern 'bad-indent-after-except-lp:771289-base arg teststring)))

(defun bad-indent-after-except-lp:771289-base ()
  (assert (eq 8 (py-compute-indentation)) "bad-indent-after-except-lp:771289 test failed"))

(defun indent-open-paren-not-last-lp:771291-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    thing = call_it(with_something,"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-open-paren-not-last-lp:771291-base arg teststring)))

(defun indent-open-paren-not-last-lp:771291-base ()
  (assert (eq 20 (py-compute-indentation)) nil "indent-open-paren-not-last-lp:771291 test failed"))

(provide 'py-bug-numbered-tests)
;;; py-bug-numbered-tests.el ends here


