(require 'flymake)

(defun ca-activate-flymake ()
  "Activates flymake when real buffer and you have write access"
  (if (and
       (buffer-file-name)
       (file-writable-p buffer-file-name))
      (progn
        (flymake-mode t)
        ;; this is necessary since there is no flymake-mode-hook...
        (local-set-key (kbd "C-c n") 'flymake-goto-prev-error)
        (local-set-key (kbd "C-c p") 'flymake-goto-prev-error))))

(defun ca-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'ca-flymake-show-help)

(defun ca-flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pycheckers" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" ca-flymake-python-init))

(add-hook 'python-mode-hook 'ca-activate-flymake)

(eval-when-compile (require 'cl))

(defvar flymake-scala-tmpdir "/tmp")

(defvar flymake-scala-global-classpath ".")

;; (push '(".+\\.scala$" flymake-scala-init) flymake-allowed-file-name-masks)
;; (push '("^\\(.*\\):\\([0-9]+\\): error: \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(defun flymake-scala-string-join (sequence separator)
  (mapconcat #'identity sequence separator))

(defun flymake-scala-string-not-empty (str)
  (and (stringp str) (not (or (string-equal "" str)
                              (string-match "^ +$" str)))))

(defun flymake-scala-parent-dir (path)
  "return parent directory path of argument."
  (substring-no-properties (file-name-directory path) 0 -1))

(defun flymake-scala-find-target-file-dir (path target)
  (let* ((src (split-string (flymake-scala-parent-dir path) "/"))
         (paths (maplist #'(lambda (l) (flymake-scala-string-join (reverse l) "/")) (nreverse src))))
    (loop for path in paths
          if (file-exists-p (concat path "/" target))
          return path)))

(defun flymake-scala-maven-build-cmd ()
  (list "mvn" (list "-fn" "-Dmaven.compiler.showWarnings=true" "dependency:copy-dependencies" "scala:compile")))

(defun flymake-scala-build-cmd (target distdir classpath)
  (list "fsc" (list "-classpath" classpath "-d" distdir target)))

(defun flymake-scala-init ()
  (let ((dir (flymake-scala-find-target-file-dir buffer-file-name "pom.xml")))
    (if (flymake-scala-string-not-empty dir)
        (progn
          (cd dir)
          (let ((distdir (loop for path in '("target" "build")
                               if (file-exists-p path)
                               return path)))
            (if (flymake-scala-string-not-empty distdir)
                (let* ((classes (concat distdir "/classes"))
                       (dependency (concat distdir "/dependency"))
                       (jars (directory-files dependency t "^[^\.]"))
                       (classpath (flymake-scala-string-join (append (cons classes jars) flymake-scala-global-classpath) ":")))
                  (flymake-scala-build-cmd buffer-file-name classes classpath))
              (flymake-scala-maven-build-cmd))))
      (flymake-scala-build-cmd buffer-file-name flymake-scala-tmpdir flymake-scala-global-classpath))))

(defun flymake-scala-start-fsc-server ()
  (with-temp-buffer
    (call-process-shell-command "fsc" nil nil)))

(defun flymake-scala-maven-update ()
  (interactive)
  (let ((dir (flymake-scala-find-target-file-dir buffer-file-name "pom.xml")))
    (if (flymake-scala-string-not-empty dir)
        (progn
          (cd dir)
          (let* ((cmd (flymake-scala-maven-build-cmd))
                 (progname (car cmd))
                 (args (cadr cmd))
                 (buffname (format "*%s*" progname))
                 (buffer (get-buffer-create buffname)))
            (switch-to-buffer-other-window buffer)
            (start-process-shell-command progname buffer progname (flymake-scala-string-join args " "))))
      (message "No pom.xml found"))))

;; (add-hook 'scala-mode-hook (lambda () (flymake-scala-start-fsc-server) (flymake-mode-on)))

(provide 'ca-flymake)
