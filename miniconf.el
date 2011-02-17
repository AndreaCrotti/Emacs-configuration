
(defcustom comment-style 'indent
  "Style to be used for `comment-region'.
See `comment-styles' for a list of available styles."
  :type (if (boundp 'comment-styles)
            `(choice ,@(mapcar (lambda (s) `(const ,(car s)))
                               comment-styles))
          'symbol)
  :version "23.1"
  :group 'comment)

(defun java-mode-untabify ()
  "untabify the whole buffer"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
  nil)

;; example on how to use this function
;; (add-hook 'java-mode-hook 
;;           '(lambda ()
;;              (make-local-variable 'write-contents-hooks)
;;              (add-hook 'write-contents-hooks 'java-mode-untabify)))

(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'rs 'replace-string)
(defalias 'qs 'query-replace)
(defalias 'qrs 'query-replace-regexp)
(defalias 'ac 'auto-complete-mode)
(defalias 'go 'google-search-it)
(defalias 'gs 'google-search-selection)
(defalias 'spell 'flyspell-mode)
(defalias 'spell-prog 'flyspell-prog-mode)
(defalias 'dml 'delete-matching-lines)
(defalias 'bb 'bury-buffer)
(defalias 'elm 'emacs-lisp-mode)

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

(defalias 'mem 'doxymacs-insert-member-comment)
(defalias 'fun 'doxymacs-insert-function-comment)
(defalias 'file 'doxymacs-insert-file-comment)

;; Those below are my favourite themes
(defalias 'black 'color-theme-hober)
(defalias 'blue 'color-theme-deep-blue)
(defalias 'grey 'color-theme-black-on-gray)
(defalias 'blipp 'color-theme-blippblopp)
(defalias 'high 'color-theme-high-contrast)
(defalias 'billw 'color-theme-billw)
(defalias 'coal 'color-theme-charcoal-black)

(defalias 'batt 'display-battery-mode)

(defun get-some-messages ()
  (interactive)
  (gnus-summary-rescan-group 1000))
;; gnus
(defalias 'gg 'get-some-messages)
(defalias 'jd 'javadoc-lookup)
(defalias 'br 'babel-region-default)
(defalias 'git 'open-git-files)

(defalias 'fold 'senator-fold-tag-toggle)

(defalias 'pd 'print-desktop)
(defalias 'dcd 'desktop-change-dir)
(defalias 'gcb 'git-change-branch)

(provide 'miniconf)

(setq custom-file (concat base "custom.el"))

(defcustom default-closing-char ";"
  "default closing char, change in newline-force-close-alist if needed"
  :type 'string)

;; TODO: use a defcustom instead
(defcustom newline-force-close-alist
  '((python-mode . ":")
    (jython-mode . ":")
    (prolog-mode . ".")
    (latex-mode . " \\newline")
    (org-mode . " \\newline")
    (tuareg-mode . ";;")
    (html-mode . " <br>"))
  "Closing char for different modes"
  :type 'list)

(defun activate-more-semantic-bindings ()
  "add some other nice bindings to modes supported by semantic"
  (interactive)
  (local-set-key (kbd "M-n") 'senator-next-tag)
  (local-set-key (kbd "M-p") 'senator-previous-tag)
  ;; TODO: the senator stuff should be enabled only where senator actually works!!
  (local-set-key [f6] 'senator-fold-tag-toggle)
  ;; narrows to the actual function or class analyzed
  ;; C-x n w to widen again
  (local-set-key "\C-xnn" 'semantic-narrow-to-tag)
  (local-set-key "\M-." 'semantic-complete-jump)
  (local-set-key "\M-?" 'semantic-ia-fast-jump))

;FIXME: not correct yet
(defun px()
  (interactive)
  (chmod (buffer-file-name) "777"))

(defun make-fortune ()
(interactive)
(let ((beg (point)))
  (insert (shell-command-to-string "fortune"))
  (end-of-paragraph-text)))

(defun gen-path-dirs (base-dir)
  "Add to load path all the subdirectories of first level"
  (interactive)
  (message "adding all directories in the first level to the load-path")
  (dolist (dir (directory-files base-dir t))
    (if (and 
         (file-directory-p dir)
         (not (file-symlink-p dir)))
        (add-to-list 'load-path dir))))

; next step is to remove conf completely
(defun reload-dirs ()
  (interactive)
  (gen-path-dirs base))

(reload-dirs)

(let
    ((tools (concat base "programming-tools")))
  (add-to-list 'exec-path tools)
  (setenv "PATH" (concat (getenv "PATH") ":" tools)))

(defun google-map-it (address)
  "get the map of the given address"
  (interactive "sSearch for: ")
  (let
      ((base "http://maps.google.it/maps?q=%s"))
    (browse-url (format base (url-hexify-string address)))))

;; My own functions
(defun newline-force()
  "Goes to newline leaving untouched the rest of the line"
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun newline-force-close()
  "Same as newline-force but putting a closing char at end"
  (interactive)
  (end-of-line)
  (let ((closing-way (assoc major-mode newline-force-close-alist))
        closing-char)
    ;; Setting the user defined or the constant if not found
    (if (not closing-way)
        (progn
          (message "closing char not defined for this mode, using default")
          (setq closing-char default-closing-char))
      (setq closing-char (cdr closing-way)))
    (when (not (bobp))
      ;; if we're at beginning of buffer, the backward-char will beep
      ;; :( This works even in the case of narrowing (e.g. we don't
      ;; look outside of the narrowed area.
      ;; FIXME: there must be a way to look for only a char!
      (let ((closing-regexp (concat ".*" closing-char ".*")))
        (when (not (looking-at closing-regexp))
          (insert closing-char))
        (newline-force)))))

(defun err-switch()
  "switch on/off error debugging"
  (interactive)
  (if debug-on-error
      (setq debug-on-error nil)
    (setq debug-on-error t))
  (message "debug-on-error now %s" debug-on-error))

;; someday might want to rotate windows if more than 2 of them
(defun swap-windows ()
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

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn   (rename-file name new-name 1)   (rename-buffer new-name)        (set-visited-file-name new-name)        (set-buffer-modified-p nil)))))) ;;

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn         (copy-file filename newname 1)  (delete-file filename)  (set-visited-file-name newname)         (set-buffer-modified-p nil)     t))))

(defun delete-current-file () 
  "Delete the file associated with the current buffer." 
  (interactive) 
  (let (currentFile) 
    (setq currentFile (buffer-file-name)) 
    (when (yes-or-no-p (format "Delete file % s and kill buffer? " currentFile)) 
      (kill-buffer (current-buffer)) 
      (delete-file currentFile) 
      (message "Deleted file: %s " currentFile))))

(defun open-git-files ()
  "Visit all the files in the current git project"
  (interactive)
  (dolist
      (file (ls-git-files))
    (message "Opening %s" file)
    ;; we have to keep the original position
    (save-excursion (find-file file))))

(defun before-last (list)
  (nth (- (length list) 2) list))


(defun dired-git (directory)
  (interactive "D")
  (dired-git-files directory))

(defun dired-git-files (directory)
  (cd directory)
  "Open a dired buffer containing the local git files"
  (let ((files (ls-git-files)))
    (if
        (or 
         (< (length files) 200)
         (yes-or-no-p (format "%d files, are you sure?" (length files))))
        ;; rename the buffer to something with a sense
        (progn
          (dired (ls-git-files))
          (rename-buffer (git-dired-buffer-name directory))))))

(defun git-dired-buffer-name (directory)
  (concat "git-" (before-last (split-string directory "/"))))

;; TODO: take the return code instead
(defun ls-git-files ()
  (let
      ((result (shell-command-to-string (concat "git ls-files"))))
    (if
        (string-match "fatal" result)
        nil
      (split-string result))))

(defun git-branches-list ()
  "list the current branches"
  (remove "*" (split-string (shell-command-to-string "git branch"))))

(defun git-change-branch ()
  "change the actual git branch asking with completion"
  (interactive)
  (let
      ((branches (git-branches-list)))
    (if
        (> (length branches) 1)
        (let 
            ((branch (completing-read "checkout to: " branches)))
          (shell-command (concat "git checkout " branch)))
      (message "no other branches, sorry"))))

(defun git-create-branch ()
  "creates a new branch"
  (interactive)
  (let
      ((branch-name (read-from-minibuffer "Name: ")))
    (shell-command (concat "git checkout -b " branch-name))))

(defun query-replace-in-git (from to)
  "query replace regexp on the files given"
  (interactive "sFrom: \nsTo: ")
  (dired-git (pwd))
  (dired-mark-files-regexp ".[ch]")
  (dired-do-query-replace-regexp from to))

(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun camelize-method (s)
  "Convert under_score string S to camelCase string."
  (mapconcat 'identity (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun un-camelcase-string (s &optional sep start)
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

(defun manipulate-matched-text (fn)
  (let (matchedText newText)
    (setq matchedText
          (buffer-substring
           (match-beginning 0) (match-end 0)))
    (setq newText
          (apply 'fn match-end))
    newText))

; TODO: use the more general manipulation
(defun uncamel ()
  (let (matchedText newText)
    (setq matchedText
          (buffer-substring
           (match-beginning 0) (match-end 0)))
    (setq newText
          (un-camelcase-string matchedText "_"))
    newText))

  ;; (manipulate-matched-text 'un-camelcase-string))
  ;; "use this function with query-replace-regexp"

;; TODO: with emacs23 is sufficient to enable subword-mode probably
(autoload 'camelCase-mode "camelCase-mode")
(defcustom camelCase-modes
  '(python-mode-hook java-mode-hook c-mode-common-hook nesc-mode-hook)
  "Modes where camelizing is allowed"
  :type 'list)

(dolist (hook camelCase-modes)
  (add-hook hook 'camelCase-mode))

;; When it's a git project we can use a grep over git ls-files
;; same thing for mercurial
;; check also with the Makefiles in general if we can do something like this
;; In this way is too simplicistic

(defvar *project-roots*
  '(".git" ".hg" "Rakefile" "Makefile" "README" "build.xml")
  "The presence of any file/directory in this list indicates a project root.")

(defun root-match(root names)
  (member (car names) (directory-files root)))

(defun root-matches(root names)
  (if (root-match root names)
      (root-match root names)
    (if (eq (length (cdr names)) 0)
        'nil
      (root-matches root (cdr names)))))

;; should return also the type and the certainty level
(defun find-project-root (&optional root)
  "Determines the current project root by recursively searching for an indicator."
  (interactive)
  (when (null root)
    (setq root default-directory))
  (cond
   ((root-matches root *project-roots*)
    (expand-file-name root))
   ((equal (expand-file-name root) "/") nil)
   (t
    ;; recursive call
    (find-project-root (concat (file-name-as-directory root) "..")))))

(find-project-root)

(defun select-line ()
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

(defun all-asscs (asslist query)
  "returns a list of all corresponding values (like rassoc)"
  (cond
   ((null asslist) nil)
   (t
    (if (equal (cdr (car asslist)) query)
        (cons (car (car asslist)) (all-asscs (cdr asslist) query))
      (all-asscs (cdr asslist) query)))))

(defcustom preferred-reopen-rw-mode "sudo"
  "preferred mode for reopen"
  :type 'string
  )

(defun rw ()
  "Reopen the file in rw mode, sui"
  (interactive)
  (let
      ((read-only-old-file (buffer-file-name)))
    (if (not (file-writable-p read-only-old-file))
        (when (yes-or-no-p "kill the read only and reopen in rw?")
          (progn 
            (kill-buffer)
            (find-file (concat "/" preferred-reopen-rw-mode "::" read-only-old-file))))
      (message "you can already write on this file"))))

;FIXME: Not really doing what is expected
(defun wc-buffer ()
  "Print number of words in Buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

;; Taken from http://www.emacswiki.org/emacs/TrampMode
(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
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
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(global-set-key [(control x) (control r)] 'find-file-root)

(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
   This function is suitable to add to `find-file-root-hook'."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!")
         (space (+ 6 (- (window-width) (length warning))))
         (bracket (make-string (/ space 2) ?-))
         (warning (concat bracket warning bracket)))
    (setq header-line-format
          (propertize  warning 'face 'find-file-root-header-face))))

(add-hook 'find-file-root-hook 'find-file-root-header-warning)

(defun reload-conf ()
  (interactive)
  (org-babel-load-file (make-conf-path "miniconf.org")))

(defconst sysop 
  (cond ((string-match "linux" system-configuration) "linux")
        ((string-match "apple" system-configuration) "mac")
        ((string-match "win" system-configuration) "win") (t "other")))

(defconst linux (string= "linux" sysop))
(defconst mac (string= "mac" sysop))
(defconst win (string= "win" sysop))
(defconst other (string= "other" sysop))

;; ;; (defconst linux nil)
;; ;; (defconst mac nil)
;; ;; (defconst win nil)
;; ;; (defconst other nil)

;; (case system-type
;;   (gnu/linux (setq linux t))
;;   (window-nt (setq mac t))
;;   (darwin    (setq win t))
;;   (t         (setq other t)))

(if mac
    (progn
      (add-to-list 'exec-path "/opt/local/bin")
      (setq ns-alternate-modifier (quote none))
      ;; open a new frame only unless it's the scratch buffer
      (setq ns-pop-up-frames 1)
      (setq ns-command-modifier (quote meta))))

(if mac
    (let ((ports-lisp "/opt/local/share/emacs/site-lisp/"))
      (if 
          (file-exists-p ports-lisp)
          (add-to-list 'load-path ports-lisp))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun ditto ()
  "*Copy contents of previous line, starting at the position above point."
  (interactive)
  (let ((last-command nil))
    (save-excursion
      (previous-line 1)
      (copy-region-as-kill (point) (progn (end-of-line) (point))))
    (yank 1)))

;; reverting automatically the buffer
(setq dired-auto-revert-buffer 1)
;; look for filenames if on one already
(setq dired-isearch-filenames 'dwim)
;; so it doesn't open a thousand buffers every time
(put 'dired-find-alternate-file 'disabled nil)

(setq file-extensions-separately 
      '("\\.pdf$" "\\.rar$" "\\.html?$" "\\.mp3$" "\\.mp4$" "\\.flv$"))

(setq dired-guess-shell-alist-user ())

(if mac
    (dolist (ext file-extensions-separately)
      (add-to-list 'dired-guess-shell-alist-user (list ext "open"))))

;TODO: add conditions for other operating systems 

(add-hook 'dired-mode-hook
          (lambda ()
            ;; define some more useful keys
            (define-key dired-mode-map "b" 'browse-url-of-dired-file)))

(require 'dired-details)
(dired-details-install)

(require 'bookmark+)

;; Using uniquify for better handling of buffers with same name
(require 'uniquify)
;; Using part of the directory in this case
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)

(setq visible-bell nil) ; Turn beep off
(setq ring-bell-function 'ignore)
(savehist-mode t) ; save also minibuffer history, very useful

;; Set some automatic filters
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Chat" (or
                  (mode . garak-mode)
                  (name . "^\\*Garak\\*$")
                  (mode . rcirc-mode)))
         ("Organization" (or
                          (mode . diary-mode)
                          (mode . org-mode)
                          (mode . org-agenda-mode)))
         ("Gnus & News" (or
                         (mode . message-mode)
                         (mode . bbdb-mode)
                         (mode . mail-mode)
                         (mode . gnus-group-mode)
                         (mode . gnus-summary-mode)
                         (mode . gnus-article-mode)
                         (name . "^\\(\\.bbdb\\|dot-bbdb\\)$")
                         (name . "^\\.newsrc-dribble$")
                         (mode . newsticker-mode)))
         ("Files" (filename . ".*"))
         ("File Management" (or
                             (mode . dired-mode)
                             (mode . shell-mode)))
         ("Documentation" (or
                           (mode . Info-mode)
                           (mode . apropos-mode)
                           (mode . woman-mode)
                           (mode . help-mode)
                           (mode . Man-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            ;; make jumping between filters easier
            (define-key ibuffer-mode-map "\C-n" 'ibuffer-forward-filter-group)
            (define-key ibuffer-mode-map "\C-p" 'ibuffer-backward-filter-group)))

(defun th-hide-org-buffers (arg)
  "Hide org-mode buffers from completion by prepending a space at the buffer name.
When called with prefix arg (`C-u'), then remove this space again."
  (interactive "P")
  (dolist (b (buffer-list))
    (set-buffer b)
    (when (eq major-mode 'org-mode)
      (rename-buffer
       (if arg 
           (replace-regexp-in-string "^[[:space:]]+" "" (buffer-name))
         (concat " " (buffer-name)))))))

; Other autoloads
(autoload 'auto-install-from-emacswiki "auto-install" "auto install from emacswiki" t)
(setq auto-install-directory (make-conf-path "auto-install/"))

;TODO: make it automatically the right size (check why frame-width does not work)
(defun make-org-agenda-buffer ()
  "Generates a small frame below for showing the agenda"
  (interactive)
  (let ((org-agenda-frame
         (make-frame 
          '((name . "org-agenda")
            (width . 200)
            (heigth . 10)
            (minibuffer . t)))))
    (with-selected-frame org-agenda-frame
      (set-frame-position org-agenda-frame 0 400)
      (bury-buffer)
      (org-agenda 0 "t"))))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (and linux window-system) (menu-bar-mode -1))

(defun full (&optional f)
  (interactive)
  (if
      mac
      ;; included in emacs 23.2
      (ns-toggle-fullscreen)
    (set-frame-parameter f 'fullscreen
                         (if (frame-parameter f 'fullscreen) nil 'fullboth))))

;; this toogle the fullscreen for every new frame (window) created
;; (add-hook 'after-make-frame-functions 'full)

;; enabling winner mode for window reconfiguration
(winner-mode t)

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))

(defcustom preferred-color-theme-function
  'coal
  "preferred color theme"
  :type 'function
)

(setq current "monaco-12")
(setq font-list
      (list "monaco-12" "inconsolata-14" "courier-13"))

(defun cycle-font ()
  "Change font in current frame"
  (interactive)

  (let (fontToUse currentState)
    ;; states starts from 1.
    (setq currentState (if (get this-command 'state) (get this-command 'state) 1))
    (setq fontToUse (nth (1- currentState) font-list))

    (set-frame-parameter nil 'font fontToUse)
    (message "Current font is: %s" fontToUse)
    (put this-command 'state (1+ (% currentState (length font-list))))
    (redraw-frame (selected-frame))))

(frame-parameter nil 'font)

(autoload 'google-search-selection "google_search" "google search" t)
(autoload 'google-it "google_search" "google search" t)

(require 'browse-kill-ring)

(setq babel-preferred-from-language "German")
(setq babel-preferred-to-language "English")

(autoload 'babel-region-default "babel" "translating default" t)
(autoload 'babel-region "babel" "translating a region" t)
(autoload 'babel "babel" "translating interactively" t)
(autoload 'babel-buffer "babel" "translate buffer" t)

(setq text-translator-display-popup t)
(setq text-translator-default-engine "google.com_deen")

(defun text-translator-region-or-thing-at-point (&optional prompt)
  (interactive)
  "If mark is active, return the region, otherwise, thing at point."
  (cond
   (mark-active
    (buffer-substring-no-properties (region-beginning) (region-end)))
   (t
    (thing-at-point 'symbol ))))

(setq ispell-dictionary "english")


;; TODO: possible to refactor this code maybe?
(defun en ()
  "Check spelling in english"
   (interactive)
  (ispell-change-dictionary "english")
  (flyspell-mode t))

(defun it ()
  "Check spelling in english"
  (interactive)
  (ispell-change-dictionary "italian")
  (flyspell-mode t))

(defun fr ()
  "Check spelling in english"
  (interactive)
  (ispell-change-dictionary "french")
  (flyspell-mode t))

(defun de ()
  "Check spelling in english"
  (interactive)
  (ispell-change-dictionary "german")
  (flyspell-mode t))

; TODO: add some thing-at-point to guess the current word
(defun my-dictionary-search ()
  "look for a word here"
  (interactive)
  (let ((word (current-word))
        (enable-recursive-minibuffers t)
        (val))
    (setq val (read-from-minibuffer
               (concat "Word"
                       (when word
                         (concat " (" word ")"))
                       ": ")))
    (dictionary-new-search
     (cons (cond
            ((and (equal val "") word)
             word)
            ((> (length val) 0)
             val)
            (t
             (error "No word to lookup")))
           dictionary-default-dictionary))))

(setq org-log-done 'note)

(setq org-todo-keywords
      '((sequence "TODO(t)" "FEEDBACK(f)" "VERIFY(v)" "|" "DONE(d)" "DELEGATED(D)" "REJECTED(r)")))

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(setq org-completion-use-ido t)

(add-to-list 'Info-default-directory-list (make-conf-path "org-mode/doc/"))

;; Clock configuration
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; make it like a macro, takes a body and executes it
(defun check-org-mode ()
  "check if the buffer is in org mode"
  (if
      (eq major-mode 'org-mode)
      t
    (message "this action is possible only in org mode")))

; Not used at the moment
(defun org-add-eventually()
  "Adding a file to org-agenda when saved"
  (interactive)
  (if
      (org-agenda-is-filtered-p (buffer-file-name))
      (message "filtered out in org-agenda-filter-out, change it to include it again")
    (if 
         (and
          (string= major-mode "org-mode")
          ; TODO: check this condition
          (or (org-agenda-filter-remote-files) (file-remote-p buffer-file-name))
           ;TODO: there should be a function already in org-mode 
          (not (member (abbreviate-file-name buffer-file-name) org-agenda-files)))
         (if
             (yes-or-no-p "add the file to agenda?")
             (org-agenda-file-to-front)))))


(defcustom org-agenda-add-eventually-enabled
  nil
  "add interactively enabled or not"
  :type 'boolean)

(defcustom org-agenda-filter-remote-files
  t
  "filter buffers open with tramp-mode"
  :type 'boolean)

(if org-agenda-add-eventually-enabled
    (add-hook 'before-save-hook 'org-add-eventually))

(defcustom org-agenda-filter-out
  '("/Volumes/arch")
  "regexp that are never added to agenda"
  :type 'list)

(defun org-agenda-is-filtered-p (filename)
  (org-agenda-is-filtered-p-rec filename org-agenda-filter-out))

(defun org-agenda-is-filtered-p-rec (filename list)
  (cond
   ((null list) nil)
   ((string-match (car list) (expand-file-name filename)) t)
   (t (org-agenda-is-filtered-p-rec filename (cdr list)))))

(defcustom org-brainstorm-ideas 10
  "number of brainstorm ideas")

(defun org-brainstorm ()
  "starts a brainstorming of ideas"
  (interactive)
  (save-excursion
    (dotimes (i org-brainstorm-ideas)
      (org-meta-return))))

(setq questions '("Who" "What" "When" "Where" "Why" "How"))

(defun prompt-ideas ()
  (interactive)
  (dolist (q questions)
    (org-meta-return)
    (insert (concat q "?"))))

;; Defining a setup where org-mode takes care of remember notes
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))

;; TODO: is it possible to use autoload here?
(require 'ob-ditaa)
(require 'ob-sh)
(require 'ob-python)
(require 'ob-ruby)
(require 'ob-dot)

;; TODO: first check if it's installed maybe
;;(setq org-latex-to-pdf-process '("texi2dvi -p -b -c -V %f"))

;; TODO: check if this is really useful and how to autocomplete it
(org-add-link-type "ebib" 'ebib)

'(org-refile-targets (quote (("~/org/gtd.org" :maxlevel . 1) 
                             ("~/org/someday.org" :level . 2))))

(setq org-struct-hooks
      '(message-mode-hook
        mail-mode-hook))

(dolist (hook org-struct-hooks)
  (add-hook hook 'turn-on-orgstruct)
  (add-hook hook 'turn-on-orgtbl))

(setq org-footnote-tag-for-non-org-mode-files "*-*-*-*-*-*-*-*-*-*")

(require 'yasnippet)
(setq yas/root-directory 
      (list (make-conf-path "yasnippet-mirror/snippets/") (make-conf-path "yasnippet-snippets/")))
  
;; Maybe needed to set to fixed for some modes
(setq yas/indent-line 'auto)
(yas/initialize)
(setq yas/ignore-filenames-as-triggers nil)

(mapc 'yas/load-directory yas/root-directory)

;; don't make backups in the snippet folder, they mess up yasnippet
(add-to-list 'backup-directory-alist '("/my-snippets/" . "/tmp/"))

;; simple function to create a .yas-parents
(defun make-yas-parents-file (path)
  (interactive "DPath: ")
  (find-file (concat path ".yas-parents"))
  (insert "text-mode"))

(defun with-comment (str)
 (format "%s%s%s" comment-start str comment-end))

(require 'eldoc)
;; Maybe better a direct activation??
(dolist (hook '(python-mode-hook
                c-mode-common-hook
                ruby-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))

;;; Require
(require 'auto-complete)
;; Various configurations
(require 'auto-complete-config)
(ac-config-default)

(setq ac-auto-start 2)                  ;automatically start
(setq ac-override-local-map nil)        ;don't override local map
(setq ac-use-menu-map t)
(setq ac-candidate-limit 20)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)  

;; making it a bit faster
(setq ac-delay 5)
(setq ac-auto-show-menu 0.4)
(setq ac-quick-help-delay 0.5)
;; using a dictionary (emtpy now)
(add-to-list 'ac-dictionary-directories (make-conf-path "auto-complete/dict"))

(setq-default ac-sources
              (append ac-sources '(ac-source-yasnippet)))

(dolist
    (my-ac-mode '(nesc-mode org-mode html-mode xml-mode haskell-mode ned-mode cpp-omnet-mode))
  (add-to-list 'ac-modes my-ac-mode))

            
; this is used for trigger ac actions from org-mode also
(add-to-list 'ac-trigger-commands 'org-self-insert-command)

;; using a nice function is ac-config
(dolist (hook (list
               'lisp-interaction-mode-hook
               'ielm-mode-hook
               ))
  (add-hook hook 'ac-emacs-lisp-mode-setup))

(add-hook 'java-mode-hook 
          '(lambda ()
             (add-to-list 'ac-sources 'eclim-complete)))

(autoload 'predictive-mode "predictive" "predictive" t)
(set-default 'predictive-auto-add-to-dict t)
(setq predictive-main-dict 'dict-english
      predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)

;; TODO: add support for different modes
(defun look-for-function ()
  (interactive)
  (let ((baseurl "http://www.google.com/codesearch?q=%s"))
    (browse-url (format baseurl (thing-at-point 'symbol) ))))

(load (make-conf-path "cedet-mirror/common/cedet"))
(setq semantic-load-turn-everything-on t)

(semantic-load-enable-all-exuberent-ctags-support)

(dolist 
    (hook '(python-mode-hook c-mode-common-hook emacs-lisp-mode-hook makefile-mode-hook))
  (add-hook hook 'activate-more-semantic-bindings))

(global-semantic-stickyfunc-mode 1)
;; (global-semantic-decoration-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-highlight-edits-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-mru-bookmark-mode 1)

(defun my-c-like-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert))

(defun my-cpp-cedet-hook ()
  (local-set-key ":" 'semantic-complete-self-insert))

(global-ede-mode t)

(global-semanticdb-minor-mode 1)
(require 'semanticdb-global)

(add-to-list 'load-path (make-conf-path "emacs-eclim/"))
;; only add the vendor path when you want to use the libraries provided with emacs-eclim
(add-to-list 'load-path (make-conf-path "emacs-eclim/vendor"))
;; use it a hook instead with the java mode

;; TODO: maybe worth using on other modes also
;; (add-hook 'java-mode-hook
;;           '(lambda ()
;;              (require 'eclim)

;;              (setq eclim-auto-save t)
;;              (eclim-mode 1)))

(require 'etags-select)

(require 'etags-table)
(setq etags-table-search-up-depth 1)

(defun my-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (when 
          (yes-or-no-p "do you want to enable gtags?")
        (let ((olddir default-directory)
              (topdir (read-directory-name  
                       "gtags: top of source tree:" default-directory)))
          (cd topdir)
          (shell-command "gtags && echo 'created tagfile'")
          (cd olddir)) ; restore   
        ;;  tagfile already exists; update it
        (shell-command "global -u && echo 'updated tagfile'"))))

(setq 
 vc-handled-backends '(Git Hg CVS SVN Bzr)
 ;; always opening the real file instead!
 vc-follow-symlinks t)

(autoload 'svn-status "psvn" "svn status" t)

(autoload 'magit-status "magit" "magit" t)
(setq magit-log-edit-confirm-cancellation t)
;; use tty which should be faster, passphrase not allowed here
(setq magit-process-connection-type nil)
(setq magit-process-popup-time 10)

; this hooks make magit going crazy
(add-hook 'magit-log-edit-mode-hook 'orgtbl-mode)
(add-hook 'magit-log-edit-mode-hook 'orgstruct-mode)
(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)

;TODO: use  (vc-ensure-vc-buffer) to make it more general
 
(defun is-version-control-file ()
  "Return nil unless the file is in the git files"
  (if (vc-working-revision (buffer-file-name))
      (auto-revert-mode t)))

(add-hook 'find-file-hook 'is-version-control-file)

(autoload 'po-mode "po-mode+"
  "Major mode for translators to edit PO files" t)

(add-to-list 'auto-mode-alist
             '("\\.po$" . po-mode))

(add-to-list 'auto-mode-alist
             '("\\.pot$" . po-mode))


;; to automatically find out the coding system
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                            'po-find-file-coding-system)
(autoload 'po-find-file-coding-system "po-mode")

(defcustom to-spell-langs
  '(emacs-lisp-mode-hook python-mode-hook c-mode-common-hook nesc-mode-hook java-mode-hook jde-mode-hook haskell-mode-hook)
  "Set of programming modes for which I want to enable spelling in comments and strings"
  :type 'list)

(dolist (lang-hook to-spell-langs)
  (add-hook  lang-hook 'flyspell-prog-mode))

(require 'auto-complete)
(ac-flyspell-workaround)

(require 'gist)

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(autoload 'clojure-mode "clojure-mode" "clojure mode" t)

(autoload 'slime "swank-clojure" "loading the swank-clojure" t)
(add-hook 'slime-mode-hook
          '(lambda () 
             (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))))

(defun kill-compile-buffer-if-successful (buffer string)
  " kill a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not 
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      'kill-buffer
                      buffer)))

(add-hook 'compilation-finish-functions 'kill-compile-buffer-if-successful)

(add-to-list 'load-path (make-conf-path "ess-mirror/lisp"))
(autoload 'R "ess-site" "loading R env" t)

(autoload 'paredit-mode "paredit" "paredit mode" t)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

; FIXME: not working as expected yet
(defun do-splint ()
  (interactive)
  (shell-command (format "splint %s" buffer-file-name)))

(require 'c-eldoc)
; TODO: maybe we can as well modify it more simply in the default structure
(setq c-default-style
      '((java-mode . "java")
       (awk-mode . "awk")
       (other . "cc-mode")))

;; FIXME: eldoc mode, not working correctly apparently
;; See http://www.emacswiki.org/emacs/CEldocMode for more info
(add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)
;; adding the hook from cedet
(add-hook 'c-mode-common-hook 'my-c-like-cedet-hook)
(add-hook 'c++-mode-hook 'my-cpp-cedet-hook)

(c-add-style "qt-gnu" 
             '("gnu" 
               (c-access-key .
                             "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
               (c-basic-offset . 4)))

(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)
(add-hook 'c++-mode-hook (lambda () (local-set-key "\C-cm" #'expand-member-functions)))

(defun is-new-file ()
 "Check if it's a new file"
 (not (file-exists-p buffer-file-name)))

(defun my-insert-header ()
  "try to insert the header smartly"
  (when
      (is-new-file)
      (let
          ((snip (find-matching-snippet (file-name-nondirectory (buffer-file-name)))))
        (if
            snip
            (insert-at-startup (cdr snip))))))

(defun find-matching-snippet (filename)
  (assoc-if (lambda (x) (string-match x filename))
                 my-auto-header-conses))

(defun insert-at-startup (snippet)
  "try to expand a snippet at startup"
  (if
      (yes-or-no-p (format "expand snippet %s?" snippet))
      (progn
        (insert snippet)
        ;; add checking
        (yas/expand))))
  
(defcustom my-auto-header-conses
      '(
        ("setup.py" . "setup")
        ("sh$" . "!")
        ("h$"  . "once")
        ("hpp$" . "once"))
      "snippets to expand per file extension"
      :type 'list)

(add-hook 'find-file-hook 'my-insert-header)

;; TODO: check why is not working with the autoload
(load-library "python-mode")
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(autoload 'doctest-mode "doctest-mode" "doc test python mode" t)

;; Initialize Pymacs                                                                                           
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  ;; Initialize Rope                                                                                             
;;  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t)
  
  ;; now add the source rope
  ;; (add-hook 'python-mode-hook 
  ;;           (lambda ()
  ;;             (set (make-local-variable 'ac-sources)
  ;;                  (append ac-sources '(ac-source-ropemacs))
  ;; )))
  
  ;; (defun my-insert-dot-and-complete (arg)
  ;;   (interactive "p")
  ;;   (self-insert-command arg)
  ;;   (rope-code-assist (py-symbol-near-point))
  ;;   )
  
  ;; (add-hook 'python-mode-hook
  ;;           (lambda () 
  ;;             (local-set-key "." 'my-insert-dot-and-complete)))

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(autoload 'haskell-mode "haskell-mode" "haskell mode" t)
(autoload 'turn-on-haskell-doc-mode "haskell-doc" "haskell doc mode" t)
(autoload 'turn-on-haskell-indent "haskell-indent" "haskell indent facilities" t)

(autoload 'inf-haskell "inf-haskell" "inf-haskell" t)
(autoload 'hs-lint "hs-lint" "haskell checker" t)

;; here some haskell variables
(setq haskell-doc-show-global-types t)
(setq haskell-program-name "ghci")
                                        ; where haskell-hoogle is loaded?

;; enabled to get indentation over if-then-else
(setq haskell-indent-thenelse 1)

;; If nothing found pass the control
(add-hook 'haskell-mode-hook
          '(lambda ()
             (require 'haskell-doc) ; Is this the only way?
             (require 'haskell-indent)
             (require 'haskell-complete)
             (require 'inf-haskell)
             (turn-on-haskell-doc-mode)
             (turn-on-haskell-indentation)
             ;; This would be very nice but it conflicts with yasnippet
             (define-key haskell-mode-map [tab] 'haskell-indent-cycle)
             (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
             (define-key haskell-mode-map "\C-cl" 'hs-lint)
             (make-variable-buffer-local 'yas/trigger-key)
             (setq yas/trigger-key [tab])
             (define-key yas/keymap [tab] 'yas/next-field)
             (add-to-list 'ac-sources 'my/ac-source-haskell)
             ))

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

(autoload 'nesc-mode "nesc" nil t)
(add-to-list 'auto-mode-alist '("\\.nc$" . nesc-mode))

(autoload 'ned-mode "ned-mode" "Major Mode for editing Ned files" t)
(add-to-list 'auto-mode-alist '("\\.ned$" . ned-mode))

;TODO: enable a finer mechanism to let this work
;read from http://yasnippet.googlecode.com/svn/trunk/doc/snippet-expansion.html#the-condition-system

(add-to-list 'auto-mode-alist '("\\.msg$" . c-mode))

(defun c++-header-file-p ()
  "Return non-nil, if in a C++ header."
  (and (string-match "\\.h$"
                     (or (buffer-file-name)
                         (buffer-name)))
       (save-excursion
         (re-search-forward "\\_<class\\_>" nil t))))

(add-to-list 'magic-mode-alist
             '(c++-header-file-p . c++-mode))

;;  (add-hook 'c++-mode-hook 'is-omnet-cpp-file)
  ;; (remove-hook 'c++-mode-hook 'is-omnet-cpp-file)
  ;; this means that the variable is set locally
  (make-variable-buffer-local 'changing-to-omnet)
  
  (defun is-omnet-cpp-file ()
    "check if the file is to be considered omnet-mode or not"
    (when 
        (and (or
              (not changing-to-omnet)
              (file-exists-p "omnetpp.ini")
              (search-forward "<omnetpp.h>")))
      (setq changing-to-omnet t)
      (cpp-omnet-mode)
      (setq changing-to-omnet nil)))

(setq jdibug-use-jde-source-paths nil)

(add-to-list 'load-path (make-conf-path "jdee/lisp"))

(autoload 'jde-mode "jde" "jde mode" t)

;; In this way we only load if really necessary
(add-hook 'jde-mode-hook
          '(lambda ()
             (require 'ecb)
             (setq indent-tabs-mode nil)))

;; (defun turn-on-font-lock-if-enabled ()
;;   "set up to make jdee shut up")

;; TODO: put some conditional stuff for the different operating systems
;; make it more general usage
(setq jde-jdk-registry
      '(("1.6" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6/")
        ("1.5" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.5/")
        ("1.3.1" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.3.1/")))

(setq jde-jdk '("1.6" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6/"))

(setq bsh-jar "/opt/local/share/java/bsh.jar")

;; for changelogs
(setq add-log-always-start-new-record 1)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-format "%02d-%02m-%:y, %02H:%02M")

(add-to-list 'auto-mode-alist '("Doxyfile" . conf-unix-mode))

(add-to-list 'load-path (make-conf-path "doxymacs/lisp"))
;; supporting doxymacs and doxymacs font locking
(add-hook 'c-mode-common-hook 
          '(lambda () 
             (require 'doxymacs)
             (doxymacs-mode t)
             (doxymacs-font-lock)))

(defun doxy-path (basepath classname)
  "convert the class name to the format used by doxygen"
  (concat basepath "doc/html/class_" (un-camelcase-string classname "_") ".html"))

(defun jump-to-doxygen-doc (basepath)
  "jump to the corresponding doxygen page"
  (interactive "D")
  (let
      ((fname (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (browse-url (doxy-path basepath fname))))

(add-to-list 'auto-mode-alist
             '("\\.applescript$" . applescript-mode))
(autoload 'applescript-mode "applescript-mode" "mode for applescript files" t)

(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup)

(setq nxhtml-menu-mode nil)

(autoload 'mako-html-mumamo-mode "autostart" "auto starting of nxhtml" t)
;; add other modes whenever needed
(add-to-list 'auto-mode-alist '("\\.mak$" . mako-html-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.mako$" . mako-html-mumamo-mode))

(setq mumamo-chunk-coloring 3)

(add-to-list 'auto-mode-alist '("\\.ml\\w?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;; lua mode
(autoload 'lua-mode "lua-mode" "mode for lua" t)

; TODO: add it globally if possible
(require 'fixme-mode)
; for each of the modes we add it to the hook
(add-to-list 'fixme-modes 'org-mode)
(dolist (hook '(python-mode-hook
                c-mode-common-hook
                ruby-mode-hook
                lisp-interaction-mode-hook
                org-mode-hook
                haskell-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook 'fixme-mode))

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (require 'inf-ruby)
             (require 'ruby-electric)
             (require 'rubydb)
             (inf-ruby-keys)
             (load-library "rdoc-mode")))

(add-to-list 'load-path (make-conf-path "rinari/util"))

(autoload 'yaml-mode "yaml-mode" "mode for yaml" t)
(add-to-list 'auto-mode-alist
             '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist
             '("\\.yml$" . yaml-mode))

(autoload 'go-mode "go-mode" "go mode" t)
(add-to-list 'auto-mode-alist
             '("\\.go$" . go-mode))

(autoload 'django-html-mode "django-html-mode" "mode for django templates" t)
(add-to-list 'auto-mode-alist
             '("views" . django-html-mode))

(setq gdb-show-main nil)
(setq gdb-many-windows t)

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline 'pdb.py
                            (file-name-nondirectory buffer-file-name)))))

(add-to-list 'load-path (make-conf-path "ess-5.11/lisp"))

(defun get-autoconf-macro-definition (&optional macro)
  "jump to the definition of a macro"
  (interactive)
  (let
      ((macro (thing-at-point 'symbol)))
      (or
       (get-autoconf-macro-local macro)
       (lookup-in-info "autoconf" macro)
       (lookup-in-info "automake" macro)
       (google-it macro))))
       ;; (progn
       ;;   (message "definition not found")
       ;;   (get-autoconf-macro-definition (read-minibuffer "Look for macro:"))))))

(defun lookup-in-info (section string)
  "lookup string in the given section"
  (info section (format "*%s*<%s>" section string))
  (condition-case nil
      (Info-index string)
    (error nil)))

(defun get-autoconf-macro-local (macro)
  "Look for the definition of the macro in aclocal.m4 before"
  (let
      ((local-macro-file "aclocal.m4"))
    (if (file-exists-p local-macro-file)
        (with-temp-buffer 
          (find-file local-macro-file)
          (condition-case nil
              (search-forward (format "AC_DEFUN([%s])" macro))
            (error nil)))
          nil)))

(add-hook 'autoconf-mode-hook
          (lambda ()
            (local-set-key "\C-j" 'get-autoconf-macro-definition)))

;; if it's not found try to look for in aclocal.m4 if it exists

(require 'session)

(autoload 'dot-mode "graphiz-dot-mode" "graphviz dot mode" t)

(add-to-list 'load-path (make-conf-path "ebib/src"))
(autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)
(add-to-list 'Info-default-directory-list (make-conf-path "ebib/manual"))

(autoload 'log4j-mode "log4-mode" t)
(add-to-list 'auto-mode-alist '("\\.log$" . log4j-mode))

(autoload 'ledger-mode "ledger" "ledger mode for accounting" t)

;  (add-to-list 'auto-mode-alist '("conf" . conf-mode))
;  (add-to-list 'auto-mode-alist '(".*?rc$" . conf-mode))

(require 'epa)
(epa-file-enable)

(setq rfc-url-save-directory "~/rfc")
(setq rfc-index-url "http://www.ietf.org/iesg/1rfc_index.txt")
(setq rfc-archive-alist (list (concat rfc-url-save-directory "/rfc.zip")
                              rfc-url-save-directory
                              "http://www.ietf.org/rfc/"))
(setq rfc-insert-content-url-hook '(rfc-url-save))

(require 'irfc)
(setq irfc-directory "~/rfcs")
(add-to-list 'auto-mode-alist
             '("/rfc[0-9]+\\.txt\\'" . irfc-mode))

(add-to-list 'load-path (make-conf-path "auctex"))
(autoload 'latex-mode "auctex" "latex mode" t)
(autoload 'preview-latex "preview-latex" "preview latex in buffer" t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq LaTeX-command "latex")
(setq TeX-PDF-mode t)
(setq TeX-master nil)

;; using flyspell also here
(add-hook 'latex-mode-hook 'turn-on-flyspell)

(setq latex-symbols-file
      (expand-file-name "~/howto_guide/languages/latex/symbols-a4.pdf"))

(defvar latex-command-program
  (cond
   (mac "open")
   (linux "evince"))
  "latex program to execute for viewing pdf")

(defun latex-symbols ()
  "open the latex symbols file"
  (interactive)
  (if (file-exists-p latex-symbols-file)
      (shell-command (concat latex-command-program " " latex-symbols-file))
    (message "file not found")))

;; requite org-latex so that the following variables are defined
(require 'org-latex)

;; require reftex cite for citations
(require 'reftex-cite)

;; tell org to use listings
(setq org-export-latex-listings t)

;; you must include the listings package
(add-to-list 'org-export-latex-packages-alist '("" "listings"))

;; if you want colored source code then you need to include the color package
(add-to-list 'org-export-latex-packages-alist '("" "color"))

;; setting where the mail is coming from
(setq mail-setup-with-from t)

;; This is just to enable flyspell in mail-mode
;; FIXME: check if this dirty hack is still needed
(defvar message-signature-separator "^-- *$" "\
    Regexp matching the signature separator.")

(autoload 'smtpmail-send-it "smtpmail")

(setq send-mail-function 'smtpmail-send-it)
(setq smtp-default-server "smtp.gmail.com")
(setq smtp-service 587)
(setq smtp-debug-info t)
(setq smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil)))
(setq smtpmail-auth-credentials
      (expand-file-name "~/.authinfo"))
(setq smtp-local-domain "bookmag")

(if mac
    (progn
      (require 'external-abook)
       (setq external-abook-command "contacts -lf '%%e\t%%n' %s")
       ;; TODO: check if it's dynamic enough
       (eval-after-load "message"
         '(progn 
            (add-hook 'mail-mode-hook
                         '(lambda ()
                            (define-key message-mode-map "\C-c\t" 'external-abook-try-expand)))))))

;; disable the warning
(setq compose-mail-check-user-agent nil)
(setq mail-user-agent 'sendmail-user-agent)

(setq gnus-select-method '(nntp "news.gmane.org"))
;; Set also comp.* hierarchy
(setq gnus-secondary-select-methods
      '(
        ;; Configuration for http://www.eternal-september.org/
        (nntp "eternal"
              (nntp-authinfo-file "~/.authinfo")
              (nntp-address "news.eternal-september.org")
              (nntp-port-number 119))))

;; (setq gmail-maildir "~/mail_clones/andrea_gmail/")

;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnmaildir "mymailbox" (directory gmail-maildir)))

;; (add-to-list 'mail-sources
;;              '(maildir :path gmail-maildir :subdirs ("cur" "new")))

(setq gnus-large-newsgroup 500)
(setq gnus-fetch-old-headers nil)

;; Changing modeline to include also the date of the message
(setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s--%d\n")

(add-hook 'gnus-started-hook
          (lambda ()
            (when (buffer-live-p gnus-dribble-buffer)
              (with-current-buffer gnus-dribble-buffer
                (setq buffer-save-without-query t)))))

;; This is an example of how to make a new command.  Type "/uptime" to
;; use it.
(defun erc-cmd-UPTIME (&rest ignore)
  "Display the uptime of the system, as well as some load-related
     stuff, to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          ", load average: " "] {Load average} ["
          ;; Collapse spaces, remove
          (replace-regexp-in-string
           " +" " "
           ;; Remove beginning and trailing whitespace
           (replace-regexp-in-string
            "^ +\\|[ \n]+$" ""
            (shell-command-to-string "uptime"))))))
    (erc-send-message
     (concat "{Uptime} [" uname-output "]"))))

; TODO: create a new one if necessary or just
; activate the current one if possible
(defun bitlbee ()
  "connect to bitlbee"
  (interactive)
  (select-frame (make-frame))
  (erc :server "localhost" :port "6667" :nick "andrea"))

(defun freenode ()
  "connect to freenode channel"
  (interactive)
  (erc :server "irc.freenode.net" :port "6667" :nick "andrea_crotti"))

;; Join the #emacs and #erc channels whenever connecting to Freenode.
(setq erc-autojoin-channels-alist
      '(("freenode.net"
         "#emacs" "#erc" "#ruby-lang" "#python" "#ledger")))

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

(setq initial-major-mode 'emacs-lisp-mode)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(display-time-mode 1)
(transient-mark-mode 1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(show-paren-mode t)
(column-number-mode t)
;; always truncate lines (useful for netbook), not working yet in ORG MODE
(setq truncate-lines nil)
;; Setting indent-tabs-mode for only spaces
(setq-default indent-tabs-mode nil)

(require 'tramp)
;; this variable is needed there for some reasons
(setq warning-suppress-types nil)

(require 'ido)
  (ido-mode t)
  ;; otherwise it will try to connect to old servers all the time
  (setq ido-enable-tramp-completion t)
  
  ;TODO: those could be hard to grasp for a beginner, should make it customizable 
  (setq ido-enable-flex-matching t)
  ;; regexp matching also
  (setq ido-enable-regexp nil)
  (setq ido-use-url-at-point t)
  (setq ido-create-new-buffer 'always)
  (setq ido-use-filename-at-point 'guess)
  (ido-everywhere t)
  
;;  (add-to-list ido-ignore-buffers "\\` ")
  
  ;; Using ido-mode hacks for advising more functions
  (require 'ido-hacks)
  (ido-hacks-mode t)

(defcustom windmove-key
  'shift
  "key for moving between windows"
  :type 'symbol)

(windmove-default-keybindings windmove-key)

(setq warning-suppress-types nil)

(setq calendar-date-style 'european)

(defadvice comint-send-eof (around warn-me-please compile activate)
  "Confirm EOF when called interactively, because accidental EOF sucks."
  (when (or (not (member this-command '(comint-send-eof
                                        comint-delchar-or-maybe-eof)))
            (y-or-n-p "Really exit? "))
    ad-do-it))

(require 'flymake)
   
(defun activate-flymake ()
  "Activates flymake when real buffer and you have write access"
  (if (and
       (buffer-file-name)
       (file-writable-p buffer-file-name))
      (progn 
        (flymake-mode t)
        ;; this is necessary since there is no flymake-mode-hook...
        (local-set-key [f3] 'flymake-goto-prev-error)
        (local-set-key [f4] 'flymake-goto-prev-error))))

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)

(defun flymake-python-init () 
  (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                     'flymake-create-temp-inplace)) 
         (local-file (file-relative-name 
                      temp-file 
                      (file-name-directory buffer-file-name)))) 
    (list "pycheckers" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks 
             '("\\.py\\'" flymake-python-init))

(if window-system
    (progn
      (require 'server)
      (if
          (not (server-running-p))
          (server-start)
        (message "server already running, check your emacser"))))

;; TODO: make it a defcustom also 
(setq fortune-dir "/opt/local/share/games/fortune/")

(defun spotlight ()
  "locate with spotlight facility"
  (interactive)
  (let ((locate-command "mdfind"))
    (call-interactively 'locate nil)))

(defun do-applescript (str)
  "Synchronously run applescript STR."
  (with-temp-buffer
    (insert str)
    (shell-command-on-region (point-min) (point-max) "osascript" nil t)
    (buffer-string)))

(defun mac-open-terminal ()
  (interactive)
  (let ((dir ""))
    (cond
     ((and (local-variable-p 'dired-directory) dired-directory)
      (setq dir dired-directory))
     ((stringp (buffer-file-name))
      (setq dir (file-name-directory (buffer-file-name))))
     )
    (do-applescript
     (format "
tell application \"Terminal\"
  activate
  try
    do script with command \"cd %s\"
  on error
    beep
  end try
end tell" dir))
    ))

(defun growl-popup (msg)
  "Pop up a growl notification with MSG, or display an Emacs message.
The \"growlnotify\" program is used if `window-system' is non-nil and
the program is found in `exec-path'; otherwise `message' is used."
  (interactive)
  (if (and window-system (executable-find "growlnotify"))
      (shell-command (concat "growlnotify -a /Applications/Emacs.app/ -m "
                             (shell-quote-argument msg)))
    (message msg)))

(defun popup-last ()
  (interactive)
  (let
      ((last-key (key-description (this-command-keys))))
    ;; check if we don't have a "stupid" sequence
    (unless
        (= (length (this-command-keys-vector)) 1)
        (growl-popup last-key))))

;TODO: make it an external package and better a minor-mode, switching would also be much easier
 
(setq growl-mode nil)

(defun growl ()
  (interactive)
  (if (not growl-mode)
      (progn
        (message "enabling growl mode notification")
        (add-hook 'pre-command-hook 'popup-last)
        (setq growl-mode t))
    (progn
      (setq-default pre-command-hook (remq 'popup-last pre-command-hook))
      (message "disabling growl mode notification")
      (setq growl-mode nil))))

;; Using the right mode for portfiles
(add-to-list 'auto-mode-alist '("Portfile" . tcl-mode))

;; TODO: ask for password
;; Taken from http://github.com/febeling/emacsd/blob/master/init.el
(defun port-open (name)
  "Open the portfile for named MacPorts port."
  (interactive "MPort: ")
  (let ((path (substring (shell-command-to-string (format "port file %s" name)) 0 -1)))
    (if (file-exists-p path)
        (find-file-other-window path))))

(when linux
  (progn 
    (defun vol-down ()
      (interactive)
      (shell-command "amixer -c 0 set PCM 3%-"))

    (defun vol-up ()
      (interactive)
      (shell-command "amixer -c 0 set PCM 3%+"))))

(defun presentation-mode ()
  "what to enable in a presentation mode"
  ;TODO: also add a function to cancel all this changes 
  (interactive)
  (growl)
  (color-theme-high-contrast)
  (global-semantic-decoration-mode -1)
  (global-semantic-idle-summary-mode -1)
  (add-hook 'python-mode-hook (function activate-flymake)))

;; compile facilities
(global-set-key [f5] 'recompile)

;; newline like textmate
(global-set-key (kbd "M-RET") 'newline-force)
(global-set-key [M-S-return] 'newline-force-close)

;; cvs stuff
(global-set-key "\C-xg" 'magit-status)

;; some nice global keys
(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-c\C-x\C-o" 'org-clock-out)
(global-set-key "\C-c\C-x\C-i" 'org-clock-in)

;; session setting, jump to the last changed thing in the buffer
;; session.el package has to be loaded
(global-set-key "\C-x\C-j" 'session-jump-to-last-change)

;; overriding defualt not so smart visualization
(global-set-key "\C-x\C-b" 'ibuffer) ;; manage buffers with ibuffer

;; visualization
(global-set-key [f11] 'full)

;; window management keys
(defun sensibly ()
  "split sensibly the selected window"
  (interactive)
  (split-window-sensibly (selected-window)))

(global-set-key [f2] 'sensibly)
(global-set-key [f1] 'delete-window)

(global-set-key (kbd "<C-f9>") 'cycle-font)

(global-set-key (kbd "M-<left>") 'other-frame)
(global-set-key (kbd "M-<right>") 'other-frame)


(global-set-key [(meta shift l)] 'select-line)

(global-set-key [f7] 'dired-git)

(global-set-key (kbd "M-z") 'undo)
;; from here
(global-set-key "\C-x\C-p" 'find-file-at-point)
;; (defadvice find-file-at-point (around goto-line compile activate)
;;   (let ((line (and (looking-at ".*:\\([0-9]+\\)")
;;                    (string-to-number (match-string 1)))))
;;     ad-do-it
;;     (and line (goto-line line))))

;; make shift-TAB works correctly also on osx
(when mac
  (define-key (keymap-parent local-function-key-map) [S-tab] nil)
  (global-set-key [S-tab] #'tab-to-tab-stop))
