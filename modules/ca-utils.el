(require 'calendar)
(require 'ca-customs)
(require 'ca-environment)

; add an ack command to replace the grep
(defvar ack-history nil
  "History for the `ack' command.")

(defun ack (command-args)
  (interactive
   (let ((ack-command "ack --nogroup --with-filename --all "))
     (list (read-shell-command "Run ack (like this): "
                               ack-command
                               'ack-history))))
  (let ((compilation-disable-input t))
    (compilation-start (concat command-args " < " null-device)
                       'grep-mode)))

(defun ack-todos ()
  (interactive)
  (let
      ((ack-command "ack --nogroup --with-filename --all \"TODO|FIXME|XXX\""))
    (compilation-start (concat ack-command " < " null-device)
                       'grep-mode)))

(defun ack-py (command-args)
  (interactive
   (let ((ack-command "ack --nogroup --with-filename --python "))
     (list (read-shell-command "Run ack (like this): "
                               ack-command
                               'ack-history))))
  (let ((compilation-disable-input t))
    (compilation-start (concat command-args " < " null-device)
                       'grep-mode)))

(defun ca-pylint ()
  (interactive)
  (let
      ((lint "pycheckers *.py"))
    (compilation-start lint 'grep-mode)))

(defun ca-pyl ()
  (interactive)
  (let
      ((lint (concat "pycheckers " (buffer-file-name))))
    (compilation-start lint 'grep-mode)))

;FIXME: this is not really working, fix it and make it only available on osx
(setq ca-growl-mode nil)

;; from TH on emacs mailing, list
;FIXME: check why is not working and the variable names
(defun ca-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;; (defadvice find-file (around ca-find-file activate)
;;   "Open FILENAME using tramp's sudo method if it's read-only."
;;   (if (and (not (file-writable-p (ad-get-argument 0)))
;;            (not (file-remote-p (ad-get-argument 0)))
;;            (y-or-n-p
;;             (concat "File " (ad-get-argument 0) " is read-only.  Open it as root? ")))
;;       (ca-find-file-sudo (ad-get-argument 0))
;;     ad-do-it))

(defun ca-growl ()
  (interactive)
  (if (not ca-growl-mode)
      (progn
        (message "enabling growl mode notification")
        (add-hook 'pre-command-hook 'ca-popup-last)
        (setq ca-growl-mode t))
    (progn
      (setq-default pre-command-hook (remq 'ca-popup-last pre-command-hook))
      (message "disabling growl mode notification")
      (setq ca-growl-mode nil))))

;TODO: this is only for mac anyway
(defun ca-presentation-mode ()
  "what to enable in a presentation mode"
  ;TODO: also add a function to cancel all this changes
  (interactive)
  (color-theme-high-contrast)
  (global-semantic-decoration-mode -1)
  (global-semantic-idle-summary-mode -1))

(when ca-mac
  (defun ca-do-applescript (str)
    "Synchronously run applescript STR."
    (with-temp-buffer
      (insert str)
      (shell-command-on-region (point-min) (point-max) "osascript" nil t)
      (buffer-string)))

  (defun ca-mac-open-terminal ()
    (interactive)
    (let ((dir ""))
      (cond
       ((and (local-variable-p 'dired-directory) dired-directory)
        (setq dir dired-directory))
       ((stringp (buffer-file-name))
        (setq dir (file-name-directory (buffer-file-name))))
       )
      (ca-do-applescript
       (format "
tell application \"Terminal\"
  activate
  try
    do script with command \"cd %s\"
  on error
    beep
  end try
end tell" dir))))

  (defun ca-growl-popup (msg)
    "Pop up a growl notification with MSG, or display an Emacs message.
The \"ca-growlnotify\" program is used if `window-system' is non-nil and
the program is found in `exec-path'; otherwise `message' is used."
    (interactive)
    (if (and window-system (executable-find "ca-growlnotify"))
        (shell-command (concat "growlnotify -a /Applications/Emacs.app/ -m "
                               (shell-quote-argument msg)))
      (message msg)))

  (defun ca-popup-last ()
    (interactive)
    (let
        ((last-key (key-description (this-command-keys))))
      ;; check if we don't have a "stupid" sequence
      (unless
          (= (length (this-command-keys-vector)) 1)
        (ca-growl-popup last-key)))))

(defun ca-toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(defun ca-full (&optional f)
  (interactive)
  (if
      ca-mac
      ;; included in emacs 23.2
      (ns-toggle-ca-fullscreen)
    (ca-toggle-fullscreen)))

(defun ca-recompile-modules-directory ()
  "Simple wrapper to recompile the modules directory"
  (interactive)
  (byte-recompile-directory (make-conf-path "modules") 0))

(defun ca-gen-path-dirs (base-dir)
  "Add to load path all the subdirectories of first level"
  (interactive)
  (message "adding all directories in the first level to the load-path")
  (dolist (dir (directory-files base-dir t))
    (if (and
         (file-directory-p dir)
         (not (file-symlink-p dir)))
        (add-to-list 'load-path dir))))

(defun ca-insdate-insert-any-date (date)
  "Insert DATE using the current locale."
  (interactive (list (calendar-read-date)))
  (insert (calendar-date-string date)))

(defun ca-insdate-insert-date-from (&optional days)
  "Insert date that is DAYS from current."
  (interactive "p*")
  (insert
   (calendar-date-string
    (calendar-gregorian-from-absolute
     (+ (calendar-absolute-from-gregorian (calendar-current-date))
        days)))))

; next step is to remove conf completely
(defun ca-reload-dirs ()
  "Add all the first-level directories to the path"
  (interactive)
  (ca-gen-path-dirs base))

(defun ca-reload-conf ()
  "Reload the current configuration"
  (interactive)
  (require 'ca-init))

;TODO: this is an utility function which might be in a library
(defun ca-mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

; TODO: maybe better as a macro

(defun ca-manipulate-matched-text (fn)
  "Apply a function on the matched text"
  (let
      ((matched-text (buffer-substring (match-beginning 0) (match-end 0))))
    (funcall fn matched-text)))

;FIXME: not correct yet
(defun ca-px()
  (interactive)
  (chmod (buffer-file-name) "777"))

(defun ca-make-fortune ()
(interactive)
(let ((beg (point)))
  (insert (shell-command-to-string "fortune"))
  (end-of-paragraph-text)))


(defun ca-occur ()
  "Find the interesting occurrences"
  (interactive)
  (occur "TODO\\|FIXME\\|XXX"))


(defun ca-google-map-it (address)
  "get the map of the given address"
  (interactive "sSearch for: ")
  (let
      ((base "http://maps.google.it/maps?q=%s"))
    (browse-url (format base (url-hexify-string address)))))

;; My own functions
(defun ca-newline-force()
  "Goes to newline leaving untouched the rest of the line"
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun ca-newline-force-close()
  "Same as ca-newline-force but putting a closing char at end unless it's already present"
  (interactive)
  (let ((closing-way (assoc major-mode ca-newline-force-close-alist))
        closing-char)
    ;; Setting the user defined or the constant if not found
    (if (not closing-way)
        (progn
          (message "closing char not defined for this mode, using default")
          (setq closing-char ca-default-closing-char))
      (setq closing-char (cdr closing-way)))
    (when (not (bobp))
      ;; if we're at beginning of buffer, the backward-char will beep
      ;; :( This works even in the case of narrowing (e.g. we don't
      ;; look outside of the narrowed area.
      (if (not (looking-at (format ".*%s.*" closing-char)))
          (progn
            (end-of-line)
            (insert closing-char))
        (message "%s already present" closing-char))
      (ca-newline-force))))

(defun ca-grep-in-current (to_grep)
  "grep in the current directory"
  (interactive "s\n")
  (let
      ((grep_cmd (format "grep -nH -e %s *" to_grep)))
    (grep grep_cmd)))

(defun ca-err-switch()
  "switch on/off error debugging"
  (interactive)
  (if debug-on-error
      (setq debug-on-error nil)
    (setq debug-on-error t))
  (message "debug-on-error now %s" debug-on-error))

;; someday might want to rotate windows if more than 2 of them
(defun ca-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond
   ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
   (t
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))))

(defun ca-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun ca-move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil) t))))

(defun ca-delete-current-file ()
  "Delete the file associated with the current buffer."
  (interactive)
  (let (currentFile)
    (setq currentFile (buffer-file-name))
    (when (yes-or-no-p (format "Delete file % s and kill buffer? " currentFile))
      (kill-buffer (current-buffer))
      (delete-file currentFile)
      (message "Deleted file: %s " currentFile))))

(defun ca-open-git-files ()
  "Visit all the files in the current git project"
  (interactive)
  (dolist
      (file (ca-ls-git-py-files))
    (message "Opening %s" file)
    ;; we have to keep the original position
    (save-excursion (find-file file))))

(defun ca-before-last (list)
  (nth (- (length list) 2) list))

(defun ca-dired-git (directory)
  (interactive "D")
  (ca-dired-git-files directory))

;TODO: extend or use magit functionalities for this
(defun ca-dired-git-files (directory)
  (cd directory)
  "Open a dired buffer containing the local git files"
  (let ((files (ca-ls-git-files)))
    (if
        (or
         (< (length files) 200)
         (yes-or-no-p (format "%d files, are you sure?" (length files))))
        ;; rename the buffer to something with a sense
        (progn
          (dired (ca-ls-git-files))
          (rename-buffer (ca-git-dired-buffer-name directory))))))

(defun ca-git-dired-buffer-name (directory)
  (concat "git-" (before-last (split-string directory "/"))))

;; TODO: take the return code instead
(defun ca-ls-git-files ()
  "List the files in the git repository"
  (let
      ((result (shell-command-to-string (concat "git ls-files"))))
    (if
        (string-match "fatal" result)
        nil
      (split-string result))))

(defun ca-ls-git-py-files ()
  "List the files in the git repository"
  (let
      ((result (shell-command-to-string (concat "git ls-files | grep .*py$"))))
    (if
        (string-match "fatal" result)
        nil
      (split-string result))))

(defun ca-git-add-file ()
  "Add current file to repository"
  (interactive)
  (shell-command (format "git add %s" (buffer-file-name))))

(defun ca-git-grep-string (string-to-find)
  "Look for a string using git-grep"
  (interactive "sString: ")
  (let ((grep-result-buffer (get-buffer-create "*git grep result*")))
    (shell-command (format "git --no-pager grep -nH -e %s" string-to-find) grep-result-buffer)
    (pop-to-buffer grep-result-buffer)
    (grep-mode)))

(defun ca-git-branches-list ()
  "list the current branches"
  (remove "*" (split-string (shell-command-to-string "git branch"))))

(defun ca-git-change-branch ()
  "change the actual git branch asking with completion"
  (interactive)
  (let
      ((branches (ca-git-branches-list)))
    (if
        (> (length branches) 1)
        (let
            ((branch (completing-read "checkout to: " branches)))
          (shell-command (concat "git checkout " branch)))
      (message "no other branches, sorry"))))

(defun ca-git-create-branch ()
  "creates a new branch"
  (interactive)
  (let
      ((branch-name (read-from-minibuffer "Name: ")))
    (shell-command (concat "git checkout -b " branch-name))))

(defun ca-query-replace-in-git (from to)
  "query replace regexp on the files given"
  (interactive "sFrom: \nsTo: ")
  (ca-dired-git (pwd))
  (dired-mark-files-regexp ".[ch]")
  (dired-do-query-replace-regexp from to))

;; When it's a git project we can use a grep over git ls-files
;; same thing for mercurial
;; check also with the Makefiles in general if we can do something like this
;; In this way is too simplicistic

(setq ca-project-roots
  '(".git" ".hg" "Rakefile" "Makefile" "README" "build.xml" "setup.py"))

(defun ca-root-match(root names)
  (member (car names) (directory-files root)))

(defun ca-root-matches(root names)
  (if (ca-root-match root names)
      (ca-root-match root names)
    (if (eq (length (cdr names)) 0)
        'nil
      (ca-root-matches root (cdr names)))))

;; should return also the type and the certainty level
(defun ca-find-project-root (&optional root)
  "Determines the current project root by recursively searching for an indicator."
  (interactive)
  (when (null root)
    (setq root default-directory))
  (cond
   ((ca-root-matches root ca-project-roots)
    (expand-file-name root))
   ((equal (expand-file-name root) "/") nil)
   (t
    ;; recursive call
    (ca-find-project-root (concat (file-name-as-directory root) "..")))))

(defun ca-select-line ()
  "If the mark is not active, select the current line.
Otherwise, expand the current region to select the lines the region touches."
  (interactive)
  (if mark-active ;; expand the selection to select lines
      (let ((top (= (point) (region-beginning)))
            (p1 (region-beginning))
            (p2 (region-end)))
        (goto-char p1)
        (beginning-of-line)
        (push-mark (point))
        (goto-char p2)
        (unless (looking-back "\n")
          (progn
            (end-of-line)
            (if (< (point) (point-max)) (forward-char))))
        (setq mark-active t
              transient-mark-mode t)
        (if top (exchange-point-and-mark)))
    (progn
      (beginning-of-line)
      (push-mark (point))
      (end-of-line)
      (if (< (point) (point-max)) (forward-char))
      (setq mark-active t
            transient-mark-mode t))))

(defun ca-all-asscs (asslist query)
  "returns a list of all corresponding values (like rassoc)"
  (cond
   ((null asslist) nil)
   (t
    (if (equal (cdr (car asslist)) query)
        (cons (car (car asslist)) (ca-all-asscs (cdr asslist) query))
      (ca-all-asscs (cdr asslist) query)))))

(defun ca-reopen-read-write ()
  "Reopen the file in rw mode, sui"
  (interactive)
  (let
      ((read-only-old-file (buffer-file-name)))
    (if (not (file-writable-p read-only-old-file))
        (when (yes-or-no-p "kill the read only and reopen in rw?")
          (progn
            (kill-buffer)
            (find-file (concat "/" ca-preferred-reopen-rw-mode "::" read-only-old-file))))
      (message "you can already write on this file"))))

;FIXME: Not really doing what is expected
(defun ca-wc-buffer ()
  "Print number of words in Buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

;; Taken from http://www.emacswiki.org/emacs/TrampMode
(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `ca-find-file-root'.")

(defvar ca-find-file-root-history nil
  "History list for files found using `ca-find-file-root'.")

(defvar ca-find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun ca-find-file-root ()
  "*Open a file as the root user.
   Prepends `ca-find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history ca-find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
            dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq ca-find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'ca-find-file-root-hook))))

(defface ca-find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun ca-find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
   This function is suitable to add to `ca-find-file-root-hook'."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!")
         (space (+ 6 (- (window-width) (length warning))))
         (bracket (make-string (/ space 2) ?-))
         (warning (concat bracket warning bracket)))
    (setq header-line-format
          (propertize  warning 'face 'ca-find-file-root-header-face))))

(add-hook 'ca-find-file-root-hook 'ca-find-file-root-header-warning)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun ca-unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; You can convert an entire buffer from paragraphs to lines by
;; recording a macro that calls ‘unfill-paragraph’ and moves past the
;; blank-line to the next unfilled paragraph and then executing that
;; macro on the whole buffer, ‘C-u 0 C-x e’ (see
;; InfiniteArgument). Or, use ca-unfill-region, below.

(defun ca-unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;; Handy key definitions
(define-key global-map "\M-Q" 'ca-unfill-paragraph)
(define-key global-map "\M-\C-q" 'ca-unfill-region)

(defun ca-camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun ca-camelize-method (s)
  "Convert under_score string S to camelCase string."
  (mapconcat 'identity (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun ca-un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is a hyphen \"-\".
    If third argument START is non-nil, convert words after that
    index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))

(defun ca-uncamel ()
  (ca-manipulate-matched-text 'un-camelcase-string))

(defun ca-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun ca-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun ca-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  ;; (ca-indent-buffer)
  (ca-untabify-buffer)
  ;TODO: use whitespace cleanup instead?
  (delete-trailing-whitespace))

; FIXME: previous-line should be only used as interactive function
(defun ca-ditto ()
  "*Copy contents of previous line, starting at the position above point."
  (interactive)
  (let ((last-command nil))
    (save-excursion
      (previous-line 1)
      (copy-region-as-kill (point) (progn (end-of-line) (point))))
    (yank 1)))

;; TODO: add support for different modes
(defun ca-look-for-function ()
  (interactive)
  (let ((baseurl "http://www.google.com/codesearch?q=%s"))
    (browse-url (format baseurl (thing-at-point 'symbol) ))))

(defun ca-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))

(defun ca-is-new-file ()
  "Check if it's a new file"
  (not (file-exists-p buffer-file-name)))

(defun ca-insert-header ()
  "try to insert the header smartly"
  (when
      (ca-is-new-file)
    (let
        ((snip
          (ca-find-matching-snippet (file-name-nondirectory (buffer-file-name)))))
      (when
          snip
        (ca-insert-at-startup (cdr snip))))))

(defun ca-explorer ()
  "Launch the windows explorer in the current directory and selects current file"
  (interactive)
  (w32-shell-execute
   "explore"
   (if buffer-file-name
       (concat (file-name-directory (buffer-file-name)))
       (nth 1 (split-string (pwd) " ")))))

(provide 'ca-utils)
