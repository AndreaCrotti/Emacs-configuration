;;; rinari-merb.el --- Rinari-Merb Is Not A Merb IDE

;; Copyright (C) 2008 Phil Hagelberg, Eric Schulte

;; Authors: Phil Hagelberg, Eric Schulte
;; Keywords: ruby, rails, project, convenience, web, merb

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Rinari-Merb Is Not A Merb IDE.

;; Well, ok it kind of is. Rinari-Merb is a set of Emacs Lisp modes
;; that is aimed towards making Emacs into a top-notch Ruby and Rails
;; development environment.

;; Copy the directory containing this file into your Emacs lisp
;; directory, assumed here to be ~/.emacs.d. Add these lines of code
;; to your .emacs file:

;; ;; ido
;; (require 'ido)
;; (ido-mode t)
;; ;; rinari-merb
;; (add-to-list 'load-path "~/.emacs.d/rinari-merb")
;; (require 'rinari-merb)

;; Note: if you cloned this from a git repo, you probably have to grab
;; the submodules which can be done simply with the following commands
;; from the root of the rinari-merb directory

;;  git submodule init
;;  git submodule update

;; See TODO file in this directory.

;;; Code:
(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (util-dir (file-name-as-directory
		  (expand-file-name "util" this-dir)))
       (jump-dir (file-name-as-directory
		  (expand-file-name "jump" util-dir))))
  (add-to-list 'load-path this-dir)
  (add-to-list 'load-path util-dir)
  (add-to-list 'load-path jump-dir))
(require 'ruby-mode)
(require 'inf-ruby)
(require 'ruby-compilation)
(require 'jump)
(require 'cl)

(defcustom rinari-merb-tags-file-name
  "TAGS"
  "Path to your TAGS file inside of your rails project.  See `tags-file-name'."
  :group 'rinari-merb)

(defvar rinari-merb-minor-mode-hook nil
  "*Hook for customising Rinari-Merb.")

(defadvice ruby-compilation-run (around rinari-merb-compilation-run activate)
  "Set default directory to the root of the rails application
  before running ruby processes."
  (let ((default-directory (or (rinari-merb-root) default-directory)))
    ad-do-it
    (rinari-merb-launch)))

(defadvice ruby-compilation-rake (around rinari-merb-compilation-rake activate)
  "Set default directory to the root of the rails application
  before running rake processes."
  (let ((default-directory (or (rinari-merb-root) default-directory)))
    ad-do-it
    (rinari-merb-launch)))

(defun rinari-merb-parse-yaml ()
  (let ((start (point))
	(end (save-excursion (re-search-forward "^$" nil t) (point)))
	alist)
    (while (and (< (point) end)
		(re-search-forward "^ *:?\\(.*\\): \\(.*\\)$" nil t))
      (setf alist (cons (cons (match-string 1) (match-string 2)) alist)))
    alist))

(defun rinari-merb-root (&optional dir home)
  (or dir (setq dir default-directory))
  (if (file-exists-p (expand-file-name
		      "init.rb"
		      (file-name-as-directory (expand-file-name "config" dir))))
      dir
    (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
      (unless (string-match "\\(^[[:alpha:]]:/$\\|^/$\\)" dir)
	(rinari-merb-root new-dir)))))

;;--------------------------------------------------------------------------------
;; user functions

(defun rinari-merb-rake (&optional task edit-cmd-args)
  "Tab completion selection of a rake task to execute with the
output dumped to a compilation buffer allowing jumping between
errors and source code.  With optional prefix argument allows
editing of the rake command arguments."
  (interactive "P")
  (ruby-compilation-rake task edit-cmd-args))

(defun rinari-merb-script (&optional script)
  "Tab completing selection of a script from the script/
directory of the rails application."
  (interactive)
  (let* ((root (rinari-merb-root))
	 (script (or script
		     (completing-read "Script: " (directory-files (concat root "script") nil "^[^.]"))))
	 (ruby-compilation-error-regexp-alist ;; for jumping to newly created files
	  (if (equal script "generate")
	      '(("^ +\\(exists\\|create\\) +\\([^[:space:]]+\\.rb\\)" 2 3))
	    ruby-compilation-error-regexp-alist))
	 (script (concat "script/" script " ")))
    (ruby-compilation-run (concat root script (read-from-minibuffer script)))))

(defun rinari-merb-test (&optional edit-cmd-args)
  "Test the current ruby function.  If current function is not a
test, then try to jump to the related test using
`rinari-merb-find-test'.  Dump output to a compilation buffer allowing
jumping between errors and source code.  With optional prefix
argument allows editing of the test command arguments."
  (interactive "P")
  (or (string-match "spec" (or (ruby-add-log-current-method)
			       (file-name-nondirectory (buffer-file-name))))
      (rinari-merb-find-rspec))
  (let* ((funname (ruby-add-log-current-method))
	 (fn (and funname
		  (string-match "#\\(.*\\)" funname)
		  (match-string 1 funname)))
	 (path (buffer-file-name))
	 (default-command (if fn
			      (concat path " --name /" fn "/")
			    path))
	 (command (if edit-cmd-args
		      (read-string "Run w/Compilation: " default-command)
		    default-command)))
    (if path (ruby-compilation-run command)
      (message "no test available"))))

(defun rinari-merb-console (&optional edit-cmd-args)
  "Run script/console in a compilation buffer, with command
history and links between errors and source code.  With optional
prefix argument allows editing of the console command arguments."
  (interactive "P")
  (rinari-merb-merb "console" "-i" edit-cmd-args))

(defun rinari-merb-sql ()
  "Browse the application's database.  Looks up login information
from your conf/database.sql file."
  (interactive)
  (flet ((sql-name (env) (format "*%s-sql*" env)))
    (let* ((environment (or (getenv "RAILS_ENV") "development"))
	   (sql-buffer (get-buffer (sql-name environment))))
      (if sql-buffer
	  (pop-to-buffer sql-buffer)
	(let* ((database-alist (save-excursion
				 (with-temp-buffer
				   (insert-file-contents
				    (expand-file-name
				     "database.yml"
				     (file-name-as-directory
				      (expand-file-name "config" (rinari-merb-root)))))
				   (goto-char (point-min))
				   (re-search-forward (concat "^:?" environment ":"))
				   (rinari-merb-parse-yaml))))
	       (adapter (or (cdr (assoc "adapter" database-alist)) "sqlite"))
	       (sql-user (or (cdr (assoc "username" database-alist)) "root"))
	       (sql-password (or (cdr (assoc "password" database-alist)) ""))
	       (sql-database (or (cdr (assoc "database" database-alist))
				 (concat (file-name-nondirectory (rinari-merb-root))
					 "_" environment)))
	       (server (or (cdr (assoc "host" database-alist)) "localhost"))
	       (port (cdr (assoc "port" database-alist)))
	       (sql-server (if port (concat server ":" port) server)))
	  (if (string-match "sqlite" adapter) (setf adapter "sqlite"))
	  (eval (list (intern (concat "sql-" adapter))))
	  (rename-buffer (sql-name environment)) (rinari-merb-launch))))))

(defun rinari-merb-merb (&optional name args edit-cmd-args)
  "Run merb.  Dump output to a compilation buffer
allowing jumping between errors and source code.  With optional
prefix argument allows editing of the server command arguments."
  (interactive "P")
  (let* ((default-directory (rinari-merb-root))
	 (name (if name name "server"))
	 (args (if args args ""))
	 (args (if edit-cmd-args
		   (read-from-minibuffer "Edit Merb Command: " args)
		 args)))
    (pop-to-buffer (ruby-compilation-do
		    name (cons "merb"
				   (ruby-args-to-list args))))
    (rinari-merb-launch)))

(defun rinari-merb-insert-erb-skeleton (no-equals)
  "Insert an erb skeleton at point, with optional prefix argument
don't include an '='."
  (interactive "P")
  (insert "<%") (unless no-equals (insert "=")) (insert "  %>")
  (backward-char 3))

(defun rinari-merb-extract-partial (begin end partial-name)
  (interactive "r\nsName your partial: ")
  (let* ((path (buffer-file-name)) ending)
    (if (string-match "view" path)
	(let ((ending (and (string-match ".+?\\(\\..*\\)" path)
			   (match-string 1 path)))
	      (partial-name
	       (replace-regexp-in-string "[[:space:]]+" "_" partial-name)))
	  (kill-region begin end)
	  (if (string-match "\\(.+\\)/\\(.+\\)" partial-name)
	      (let ((default-directory (expand-file-name (match-string 1 partial-name)
							 (expand-file-name ".."))))
		(find-file (concat "_" (match-string 2 partial-name) ending)))
	    (find-file (concat "_" partial-name ending)))
	  (yank) (pop-to-buffer nil)
	  (insert (concat "<%= render :partial => '" partial-name "' %>\n")))
      (message "not in a view"))))

(defvar rinari-merb-rgrep-file-endings
  "*.[^l]*"
  "Ending of files to search for matches using `rinari-merb-rgrep'")

(defun rinari-merb-rgrep (&optional arg)
  "Search through the rails project for a string or `regexp'.
With optional prefix argument just run `rgrep'."
  (interactive "P")
  (grep-compute-defaults)
  (if arg (call-interactively 'rgrep)
    (let ((word (thing-at-point 'word)))
      (funcall 'rgrep (read-from-minibuffer "search for: " word)
	       rinari-merb-rgrep-file-endings (rinari-merb-root)))))

;;--------------------------------------------------------------------
;; rinari-merb movement using jump.el

(defun rinari-merb-generate (type name)
  (message (shell-command-to-string
	    (format "ruby %sscript/generate %s %s" (rinari-merb-root) type
		    (read-from-minibuffer (format "create %s: " type) name)))))

(defvar rinari-merb-ruby-hash-regexp
  "\\(:[^[:space:]]*?\\)[[:space:]]*\\(=>[[:space:]]*[\"\':]?\\([^[:space:]]*?\\)[\"\']?[[:space:]]*\\)?[,){}\n]"
  "Regexp to match subsequent key => value pairs of a ruby hash.")

(defun rinari-merb-ruby-values-from-render (controller action)
  "Adjusts CONTROLLER and ACTION acording to keyword arguments in
the hash at `point', then return (CONTROLLER . ACTION)"
  (let ((end (save-excursion
	       (re-search-forward "[^,{(]$" nil t)
	       (+ 1 (point)))))
    (save-excursion
      (while (and (< (point) end)
		  (re-search-forward rinari-merb-ruby-hash-regexp end t))
	(if (> (length (match-string 3)) 1)
	    (case (intern (match-string 1))
	      (:partial (setf action (concat "_" (match-string 3))))
	      (:action  (setf action (match-string 3)))
	      (:controller (setf controller (match-string 3)))))))
    (cons controller action)))

(defun rinari-merb-which-render (renders)
  (let ((path (jump-completing-read
	       "Follow: "
	       (mapcar (lambda (lis)
			 (concat (car lis) "/" (cdr lis)))
		       renders))))
    (string-match "\\(.*\\)/\\(.*\\)" path)
    (cons (match-string 1 path) (match-string 2 path))))

(defun rinari-merb-follow-controller-and-action (controller action)
  "Follow the current controller-and-action through all of the
renders and redirects to find the final controller or view."
  (save-excursion ;; if we can find the controller#action pair
    (if (and (jump-to-path (format "app/controllers/%s_controller.rb#%s" controller action))
	     (equalp (jump-method) action))
	(let ((start (point)) ;; demarcate the borders
	      (renders (list (cons controller action))) render view)
	  (ruby-forward-sexp)
	  ;; collect redirection options and pursue
	  (while (re-search-backward "re\\(?:direct_to\\|nder\\)" start t)
	    (add-to-list 'renders (rinari-merb-ruby-values-from-render controller action)))
	  (let ((render (if (equalp 1 (length renders))
			    (car renders)
			  (rinari-merb-which-render renders))))
	    (if (and (equalp (cdr render) action)
		     (equalp (car render) controller))
		(list controller action) ;; directed to here so return
	      (rinari-merb-follow-controller-and-action (or (car render)
						       controller)
						   (or (cdr render)
						       action)))))
      ;; no controller entry so return
      (list controller action))))

(setf
 rinari-merb-jump-schema
 '((model
    "m"
    (("app/controllers/\\1.rb#\\2"  . "app/models/\\1.rb#\\2")
     ("app/views/\\1/.*"                       . "app/models/\\1.rb")
     ("app/helpers/\\1_helper.rb"              . "app/models/\\1.rb")
     ("schema/migrations/.*_\\1_migration.rb"  . "app/models/\\1.rb")
     ("spec/models/\\1_spec.rb"                . "app/models/\\1.rb")
     ("spec/controllers/\\1_spec.rb". "app/models/\\1.rb")
     ("spec/views/\\1/.*"                      . "app/models/\\1.rb")
     ("spec/fixtures/\\1.yml"                  . "app/models/\\1.rb")
     ("test/functional/\\1_controller_test.rb" . "app/models/\\1.rb")
     ("test/unit/\\1_test.rb#test_\\2"         . "app/models/\\1.rb#\\2")
     ("test/unit/\\1_test.rb"                  . "app/models/\\1.rb")
     ("test/fixtures/\\1.yml"                  . "app/models/\\1.rb")
     (t                                        . "app/models/"))
    (lambda (path)
      (rinari-merb-generate "model"
		       (and (string-match ".*/\\(.+?\\)\.rb" path)
			    (match-string 1 path)))))
   (controller
    "c"
    (("app/models/\\1.rb"                      . "app/controllers/\\1.rb")
     ("app/views/\\1/\\2\\..*"                 . "app/controllers/\\1.rb#\\2")
     ("app/helpers/\\1_helper.rb"              . "app/controllers/\\1.rb")
     ("schema/migrations/.*_\\1_migration.rb"  . "app/controllers/\\1.rb")
     ("spec/models/\\1_spec.rb"                . "app/controllers/\\1.rb")
     ("spec/controllers/\\1_spec.rb"           . "app/controllers/\\1.rb")
     ("spec/views/\\1/\\2\\.*_spec.rb"         . "app/controllers/\\1.rb#\\2")
     ("spec/fixtures/\\1.yml"                  . "app/controllers/\\1.rb")
     ("test/functional/\\1_test.rb#test_\\2"   . "app/controllers/\\1.rb#\\2")
     ("test/functional/\\1_test.rb"            . "app/controllers/\\1.rb")
     ("test/unit/\\1_test.rb#test_\\2"         . "app/controllers/\\1.rb#\\2")
     ("test/unit/\\1_test.rb"                  . "app/controllers/\\1.rb")
     ("test/fixtures/\\1.yml"                  . "app/controllers/\\1.rb")
     (t                                        . "app/controllers/"))
    (lambda (path)
      (rinari-merb-generate "controller"
		       (and (string-match ".*/\\(.+?\\)_controller\.rb" path)
			    (match-string 1 path)))))
   (view
    "v"
    (("app/models/\\1.rb"                      . "app/views/\\1/.*")
     ((lambda () ;; find the controller/view
	(let* ((raw-file (and (buffer-file-name)
			      (file-name-nondirectory (buffer-file-name))))
	       (file (and raw-file
			  (string-match "^\\(.*\\)_controller.rb" raw-file)
			  (match-string 1 raw-file))) ;; controller
	       (raw-method (ruby-add-log-current-method))
	       (method (and file raw-method ;; action
			    (string-match "#\\(.*\\)" raw-method)
			    (match-string 1 raw-method))))
	  (if (and file method) (rinari-merb-follow-controller-and-action file method))))
      . "app/views/\\1/\\2.*")
     ("app/helpers/\\1_helper.rb"              . "app/views/\\1/.*")
     ("schema/migrations/.*_\\1_migration.rb"  . "app/views/\\1/.*")
     ("spec/models/\\1_spec.rb"                . "app/views/\\1/.*")
     ("spec/controllers/\\1_spec.rb"           . "app/views/\\1/.*")
     ("spec/views/\\1/\\2_spec.rb"             . "app/views/\\1/\\2.*")
     ("spec/fixtures/\\1.yml"                  . "app/views/\\1/.*")
     ("test/functional/\\1_controller_test.rb" . "app/views/\\1/.*")
     ("test/unit/\\1_test.rb#test_\\2"         . "app/views/\\1/_?\\2.*")
     ("test/fixtures/\\1.yml"                  . "app/views/\\1/.*")
     (t                                        . "app/views/.*"))
    t)
   (test
    "t"
    (("app/models/\\1.rb#\\2"                  . "test/unit/\\1_test.rb#test_\\2")
     ("app/controllers/\\1.rb#\\2"             . "test/functional/\\1_test.rb#test_\\2")
     ("app/views/\\1/_?\\2\\..*"               . "test/functional/\\1_controller_test.rb#test_\\2")
     ("app/helpers/\\1_helper.rb"              . "test/functional/\\1_controller_test.rb")
     ("schema/migrations/.*_\\1_migration.rb"  . "test/unit/\\1_test.rb")
     ("test/functional/\\1_controller_test.rb" . "test/unit/\\1_test.rb")
     ("test/unit/\\1_test.rb"                  . "test/functional/\\1_controller_test.rb")
     (t                                        . "test/.*"))
    t)
   (rspec
    "r"
    (("app/models/\\1.rb"                      . "spec/models/\\1_spec.rb")
     ("app/controllers/\\1.rb"                 . "spec/controllers/\\1_spec.rb")
     ("app/views/\\1\\..*"                     . "spec/views/\\1_spec.rb")
     ("schema/migrations/.*_\\1_migration.rb"  . "spec/models/\\1_spec.rb")
     ("spec/views/\\1_spec.rb"                 . "app/views/\\1")
     ("spec/\\1_spec.rb"                       . "app/\\1.rb")
     (t                                        . "spec/.*"))
    t)
   (fixture
    "x"
    (("app/models/\\1.rb"                      . "test/fixtures/\\1.yml")
     ("app/controllers/\\1.rb"      . "test/fixtures/\\1.yml")
     ("app/views/\\1/.*"                       . "test/fixtures/\\1.yml")
     ("app/helpers/\\1_helper.rb"              . "test/fixtures/\\1.yml")
     ("schema/migrations/.*_\\1_migration.rb"  . "test/fixtures/\\1.yml")
     ("spec/models/\\1_spec.rb"                . "test/fixtures/\\1.yml")
     ("spec/controllers/\\1_spec.rb". "test/fixtures/\\1.yml")
     ("spec/views/\\1/.*"                      . "test/fixtures/\\1.yml")
     ("test/functional/\\1_controller_test.rb" . "test/fixtures/\\1.yml")
     ("test/unit/\\1_test.rb"                  . "test/fixtures/\\1.yml")
     (t                                        . "test/fixtures/"))
    t)
   (rspec-fixture
    "z"
    (("app/models/\\1.rb"                      . "spec/fixtures/\\1.yml")
     ("app/controllers/\\1.rb"      . "spec/fixtures/\\1.yml")
     ("app/views/\\1/.*"                       . "spec/fixtures/\\1.yml")
     ("app/helpers/\\1_helper.rb"              . "spec/fixtures/\\1.yml")
     ("schema/migrations/.*_\\1_migration.rb"  . "spec/fixtures/\\1.yml")
     ("spec/models/\\1_spec.rb"                . "spec/fixtures/\\1.yml")
     ("spec/controllers/\\1_spec.rb". "spec/fixtures/\\1.yml")
     ("spec/views/\\1/.*"                      . "spec/fixtures/\\1.yml")
     ("test/functional/\\1_controller_test.rb" . "spec/fixtures/\\1.yml")
     ("test/unit/\\1_test.rb"                  . "spec/fixtures/\\1.yml")
     (t                                        . "spec/fixtures/"))
    t)
   (helper
    "h"
    (("app/models/\\1.rb"                      . "app/helpers/\\1_helper.rb")
     ("app/controllers/\\1.rb"      . "app/helpers/\\1_helper.rb")
     ("app/views/\\1/.*"                       . "app/helpers/\\1_helper.rb")
     ("app/helpers/\\1_helper.rb"              . "app/helpers/\\1_helper.rb")
     ("schema/migrations/.*_\\1_migration.rb"  . "app/helpers/\\1_helper.rb")
     ("spec/models/\\1_spec.rb"                . "app/helpers/\\1_helper.rb")
     ("spec/controllers/\\1_spec.rb"           . "app/helpers/\\1_helper.rb")
     ("spec/views/\\1/.*"                      . "app/helpers/\\1_helper.rb")
     ("test/functional/\\1_controller_test.rb" . "app/helpers/\\1_helper.rb")
     ("test/unit/\\1_test.rb#test_\\2"         . "app/helpers/\\1_helper.rb#\\2")
     ("test/unit/\\1_test.rb"                  . "app/helpers/\\1_helper.rb")
     (t                                        . "app/helpers/"))
    t)
   (migration
    "i"
    (("app/controllers/\\1.rb"      . "schema/migrations/.*_\\1_migration.rb")
     ("app/views/\\1/.*"                       . "schema/migrations/.*_\\1_migration.rb")
     ("app/helpers/\\1_helper.rb"              . "schema/migrations/.*_\\1_migration.rb")
     ("app/models/\\1.rb"                      . "schema/migrations/.*_\\1_migration.rb")
     ("spec/models/\\1_spec.rb"                . "schema/migrations/.*_\\1_migration.rb")
     ("spec/controllers/\\1_spec.rb"           . "schema/migrations/.*_\\1_migration.rb")
     ("spec/views/\\1/.*"                      . "schema/migrations/.*_\\1_migration.rb")
     ("test/functional/\\1_controller_test.rb" . "schema/migrations/.*_\\1_migration.rb")
     ("test/unit/\\1_test.rb#test_\\2"         . "schema/migrations/.*_\\1_migration.rb#\\2")
     ("test/unit/\\1_test.rb"                  . "schema/migrations/.*_\\1_migration.rb")
     (t                                        . "schema/migrations/"))
    (lambda (path)
      (rinari-merb-generate "migration"
		       (and (string-match ".*create_\\(.+?\\)\.rb" path)
			    (match-string 1 path)))))
   (environment "e" ((t . "config/environments/")) nil)
   (configuration "n" ((t . "config/")) nil)
   (script "s" ((t . "script/")) nil)
   (lib "l" ((t . "lib/")) nil)
   (log "o" ((t . "log/")) nil)
   (worker "w" ((t . "lib/workers/")) nil)
   (public "p" ((t . "public/")) nil)
   (stylesheet "y" ((t . "public/stylesheets/.*")) nil)
   (javascript "j" ((t . "public/javascripts/.*")) nil)
   (plugin "l" ((t . "vendor/plugins/")) nil)
   (file-in-project "f" ((t . ".*")) nil)))

(mapcar
 (lambda (type)
   (let ((name (first type))
	 (specs (third type))
	 (make (fourth type)))
     (eval `(defjump
	      (quote ,(read (format "rinari-merb-find-%S" name)))
	      (quote ,specs)
	      'rinari-merb-root
	      ,(format "Go to the most logical %S given the current location" name)
	      ,(if make `(quote ,make))
	      'ruby-add-log-current-method))))
 rinari-merb-jump-schema)

;;--------------------------------------------------------------------
;; minor mode and keymaps

(defvar rinari-merb-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Key map for Rinari-Merb minor mode.")

(defun rinari-merb-bind-key-to-func (key func)
  (eval `(define-key rinari-merb-minor-mode-map 
	   ,(format "\C-c;%s" key) ,func))
  (eval `(define-key rinari-merb-minor-mode-map 
	   ,(format "\C-c'%s" key) ,func)))

(defvar rinari-merb-minor-mode-keybindings
  '(("s" . 'rinari-merb-script)              ("q" . 'rinari-merb-sql)
    ("e" . 'rinari-merb-insert-erb-skeleton) ("t" . 'rinari-merb-test)
    ("r" . 'rinari-merb-rake)                ("c" . 'rinari-merb-console)
    ("m" . 'rinari-merb-merb)                ("g" . 'rinari-merb-rgrep)
    ("w" . 'rinari-merb-merb)                ("x" . 'rinari-merb-extract-partial))
  "alist mapping of keys to functions in `rinari-merb-minor-mode'")

(mapcar (lambda (el) (rinari-merb-bind-key-to-func (car el) (cdr el)))
	(append (mapcar (lambda (el)
			  (cons (concat "f" (second el))
				(read (format "'rinari-merb-find-%S" (first el)))))
			rinari-merb-jump-schema)
		rinari-merb-minor-mode-keybindings))

(defun rinari-merb-launch ()
  "Run `rinari-merb-minor-mode' if inside of a rails projecct,
otherwise turn `rinari-merb-minor-mode' off if it is on."
  (interactive)
  (let* ((root (rinari-merb-root)) (r-tags-path (concat root rinari-merb-tags-file-name)))
    (if root (progn
	       (set (make-local-variable 'tags-file-name)
		    (and (file-exists-p r-tags-path) r-tags-path))
	       (run-hooks 'rinari-merb-minor-mode-hook)
	       (rinari-merb-minor-mode t))
      (if rinari-merb-minor-mode (rinari-merb-minor-mode)))))

(defvar rinari-merb-major-modes
  '('find-file-hook 'mumamo-after-change-major-mode-hook 'dired-mode-hook)
  "Major Modes from which to launch Rinari-Merb.")

(mapcar (lambda (hook)
	  (eval `(add-hook ,hook
			   (lambda () (rinari-merb-launch)))))
	rinari-merb-major-modes)

(defadvice cd (after rinari-merb-on-cd activate)
  "Active/Deactive rinari-merb-minor-node when changing into and out
  of raills project directories."
  (rinari-merb-launch))

;;;###autoload
(define-minor-mode rinari-merb-minor-mode
  "Enable Rinari-Merb minor mode providing Emacs support for working
with the Ruby on Rails framework."
  nil
  " Rinari-Merb"
  rinari-merb-minor-mode-map)

(provide 'rinari-merb)
;;; rinari-merb.el ends here