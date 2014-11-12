;;; pan.el --- Launch current tests with testr in its venv

;; Copyright (C) 2014 Chmouel Boudjnah <chmouel@chmouel.com>

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Homepage: https://github.com/chmouel/pan.el
;; Version: 0.1
;; Keywords: convenience, tox, python, tests

;;; Installation:

;;; Commentary:
;;;
;;; If you use tox and testr it will just work and run the current test with the
;;; environments that are already built without using tox directly but python -m
;;; testools.run.
;;;
;;; `pan-run-current-test' will run the function at point.
;;; `pan-run-all-until-fail' will run all tests until the first one that has
;;; failed.
;;; `pan-venv-workon' will use the virtualenvwrapper library to setup a virtualenv
;;; whitin your discovered tox directory.
;;;
;;; When you are in a function of your code you can use the function
;;; `pan-switch-test-func' to jump to one of the tests as discovered by
;;; testr. Pan will record from which function you are coming from and switch
;;; back to it if you run it again.
;;;
;;; TODO:
;;;
;;; - Caching of places between Emacs session (is it really needed?)
;;; - Caching of testr listing
;;; - Implement other testr command line (i.e: failing, isolated or others)
;;;
;;; License:

;; This file is NOT part of GNU Emacs.

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

;;; Code:

;;; Default setting lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pan-default-env 'nil)
(defvar pan-test-associations (make-hash-table :test 'equal))

;;; Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pan-jump-move-around (filename class_test)
  (if (not (file-exists-p filename))
      (error "cannot find filename: %s" filename))
  (find-file filename)
  (beginning-of-buffer)
  (if (re-search-forward (concat
                          "^class[[:blank:]]*"
                          (car class_test)))
      (re-search-forward (concat
                          "^[[:blank:]]*def[[:blank:]]*"
                          (car (cdr class_test)))))
  (message "Switched to: %s" (mapconcat 'identity class_test ".")))

(defun pan-jump-to-test-from-function (current askenv)
  (let ((asoc))
    (if (or askenv (not (gethash current pan-test-associations)))
        (puthash current (pan-ask-for-test toxenvs) pan-test-associations))
    (setq assoc (gethash current pan-test-associations))
    (let ((class_test
           (last (split-string assoc "\\.") 2))
          (filename
           (concat
            (mapconcat
             'identity (butlast (split-string assoc "\\.") 2) "/") ".py")))
      (pan-jump-move-around  filename class_test))))

(defun pan-jump-to-function-from-test (current askenv)
  (let ((matched)
        (currentbase (car (cdr (split-string current ":")))))
    (maphash
     (lambda (k v)
       (if (string= currentbase (mapconcat 'identity (last (split-string v "\\.") 2) "."))
           (setq matched k)))
     pan-test-associations)
    (let ((filename (car (split-string matched ":")))
          (class_test (split-string (car (cdr (split-string matched ":"))) "\\.")))
      (pan-jump-move-around filename class_test))))

(defun pan-get-all-tests (toxenv)
  "Get all tests from testr using discover -l"
  (let ((current-directory (pan-get-root-directory))
        (toxdir (concat (pan-get-root-directory) ".tox")))
    (with-temp-buffer
      (call-process
       (concat toxdir "/" toxenv "/bin/python")
       nil t nil "-m" "testtools.run"
       "discover" "-l")
    (let ((lines '()))
      (goto-char (point-min))
        (while (not (eobp))
          (push (car (split-string
                      (buffer-substring
                       (point) (point-at-eol))))
                lines)
          (beginning-of-line 2))
        lines))))

(defun pan-ask-for-test (toxenvs)
  (let ((tests (pan-get-all-tests toxenvs)))
    (funcall (or (and (featurep 'ido)
                      (symbol-function 'ido-completing-read))
                 #'completing-read) "Test: " tests)))

(defun pan-get-envlist()
  "Get tox dirs from the python directories. Only get the one that has testools
installed."
  (let ((envs '())
        (toxdir (concat (pan-get-root-directory) ".tox")))
    (dolist (dir (directory-files toxdir nil nil t))
      (when (file-exists-p (concat toxdir "/" dir "/bin/python"))
        (if (= 0 (call-process (concat toxdir "/" dir "/bin/python")
                               nil nil nil "-m" "testtools.run"))
            (push dir envs))))
    envs))

(defun pan-get-root-directory()
  "Return the root directory to run tests."
  (file-truename (or (locate-dominating-file
                      (buffer-file-name) "tox.ini")
                     "./")))

(defmacro with-pan (current &optional askenvs &rest body)
  "Macro which initialize environments variables to launch unit tests on current
test or current class."
    `(let ((toxenvs (if ,askenvs
            (completing-read
             "Tox Environment: " (pan-get-envlist))
              pan-default-env))
       (default-directory (pan-get-root-directory))
       (compilation-auto-jump-to-first-error nil)
       (compilation-scroll-output nil)
       (,current (python-info-current-defun)))
       (setq pan-default-env toxenvs)
       ,@body))

(defun pan-get-command (test env gettest)
  "Return the command to launch tests."
  (concat
   "./.tox/" env "/bin/python -m testtools.run "
   (when gettest
     (concat
     (subst-char-in-string
      ?/ ?.
      (file-name-sans-extension
       (substring
        (file-truename
         (buffer-file-name))
        (length (pan-get-root-directory)))))
     "."))
   test))

; Run test
;;;###autoload
(defun pan-run-current-test (&optional askenvs)
  (interactive "P")
  (with-pan current (or (not pan-default-env) askenvs)
     (unless current
       (error "No function at point"))
     (compile (pan-get-command current toxenvs t))))

;;;###autoload
(defun pan-switch-test-func (&optional askenvs)
  "Jump to a testr test from a function and record it"
  (interactive "P")
  (with-pan current (or (not pan-default-env) askenvs)
   (let* ((assoc)
         (current (python-info-current-defun))
         (full-current (concat
           (replace-regexp-in-string
            (pan-get-root-directory) "" (buffer-file-name))
           ":" current)))
     (if (and (string-match (rx-to-string `(: bos ,"test_") t)
                            (car (cdr (split-string current "\\.")))) t)
         (pan-jump-to-function-from-test full-current askenvs)
       (pan-jump-to-test-from-function full-current askenvs)))))

;;;###autoload
(defun pan-current-class (&optional askenvs)
  (interactive "P")
  (with-pan current (or (not pan-default-env) askenvs)
     (if current
         (let ((current-class (car (split-string current "\\."))))
           (compile (pan-get-command current-class toxenvs t)))
       (error "No class at point"))))

;;;###autoload
(defun pan-run-all (&optional askenvs)
  (interactive "P")
  (with-pan current (or (not pan-default-env) askenvs)
     (compile (pan-get-command "discover" toxenvs nil))))

;;;###autoload
(defun pan-run-all-until-fail (&optional askenvs)
  (interactive "P")
  (with-pan current (or (not pan-default-env) askenvs)
     (compile (pan-get-command "discover -f" toxenvs nil))))

;;;###autoload
(defun pan-choose-test-to-run (&optional askenvs)
  (interactive "P")
  (with-pan current (or (not pan-default-env) askenvs)
   (let ((tests (pan-get-all-tests toxenvs)))
     (compile (pan-get-command
               (pan-ask-for-test toxenvs)
               toxenvs nil)))))

(when (require 'virtualenvwrapper nil t)
  (defun pan-venv-workon()
    "Use virtualenvwrapper library to setup the virtualenv environements for our
    project tox venvs.."
    (interactive)
    (let ((venv-location (concat (pan-get-root-directory) "/.tox")))
      (venv-workon))))

;;; End pan.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'pan)

;;; tox.el ends here
