;;; ob-sql.el --- org-babel functions for sql evaluation

;; Copyright (C) 2009, 2010  Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.01trans

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating sql source code.
;;
;; SQL is somewhat unique in that there are many different engines for
;; the evaluation of sql (Mysql, PostgreSQL, etc...), so much of this
;; file will have to be implemented engine by engine.
;;
;; Also SQL evaluation generally takes place inside of a database.
;;
;; For now lets just allow a generic ':cmdline' header argument.
;;
;; TODO:
;;
;; - support for sessions
;; - add more useful header arguments (user, passwd, database, etc...)
;; - support for more engines (currently only supports mysql)
;; - what's a reasonable way to drop table data into SQL?
;; 

;;; Code:
(require 'ob)
(eval-when-compile (require 'cl))

(declare-function org-table-import "org-table" (file arg))

(defvar org-babel-default-header-args:sql '())

(defun org-babel-expand-body:sql (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body." body)

(defun org-babel-execute:sql (body params)
  "Execute a block of Sql code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
	 (processed-params (org-babel-process-params params))
         (cmdline (cdr (assoc :cmdline params)))
         (engine (cdr (assoc :engine params)))
         (in-file (make-temp-file "org-babel-sql-in"))
         (out-file (or (cdr (assoc :out-file params))
                       (make-temp-file "org-babel-sql-out")))
         (command (case (intern engine)
                    ('mysql (format "mysql %s -e \"source %s\" > %s"
                                    (or cmdline "") in-file out-file))
                    (t (error "no support for the %s sql engine" engine)))))
    (with-temp-file in-file
      (insert (org-babel-expand-body:sql body params)))
    (message command)
    (shell-command command)
    (with-temp-buffer
      (org-table-import out-file nil)
      (org-babel-reassemble-table
       (org-table-to-lisp)
       (org-babel-pick-name (nth 4 processed-params) (cdr (assoc :colnames params)))
       (org-babel-pick-name (nth 5 processed-params) (cdr (assoc :rownames params)))))))


(defun org-babel-prep-session:sql (session params)
  "Raise an error because Sql sessions aren't implemented."
  (error "sql sessions not yet implemented"))

(provide 'ob-sql)

;; arch-tag: a43ff944-6de1-4566-a83c-626814e3dad2

;;; ob-sql.el ends here
