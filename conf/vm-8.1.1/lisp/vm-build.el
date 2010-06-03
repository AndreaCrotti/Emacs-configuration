;; Add the current dir to the load-path
(setq load-path (cons default-directory load-path))
;(setq debug-on-error t)
;(setq debug-ignored-errors nil)
;(message "load-path: %S" load-path)

(defun vm-fix-cygwin-path (path)
  "If PATH does not exist, try the DOS path instead.
    This handles EmacsW32 path problems when building on cygwin."
  (if (file-exists-p path)
      path
    (let ((dos-path (cond ((functionp 'mswindows-cygwin-to-win32-path)
			   (mswindows-cygwin-to-win32-path path))
			  ((and (locate-library "cygwin-mount")
    				(require 'cygwin-mount))
    			   (cygwin-mount-activate)
    			   (cygwin-mount-convert-file-name path))
			  ((string-match "^/cygdrive/\\([a-z]\\)" path)
			   (replace-match (format "%s:" 
						  (match-string 1 path))
					  t t path)))))
      (if (and dos-path (file-exists-p dos-path))
	  dos-path
	path))))

;; Add additional dirs to the load-path
(condition-case err
    (when (getenv "OTHERDIRS")
      (let ((otherdirs (read (format "%s" (getenv "OTHERDIRS"))))
	    dir)
	(while otherdirs
	  (setq dir (car otherdirs))
	  (if (not (file-exists-p dir))
	      (error "Extra `load-path' directory %S does not exist!" dir))
					;      (print (format "Adding %S" dir))
	  (setq load-path (cons dir load-path)
		otherdirs (cdr otherdirs)))))

  ((end-of-file) nil)
  ((invalid-read-syntax)   
   (message "OTHERDIRS=%S rejected by `read': %s"
	    (getenv "OTHERDIRS")
	    ;(error-message-string err)
	    err
	    )))
  
;; Load byte compile 
(require 'bytecomp)
(setq byte-compile-warnings '(free-vars))
(put 'inhibit-local-variables 'byte-obsolete-variable nil)

;; Preload these to get macros right 
(require 'cl)
(require 'sendmail)

;; now add VM source dirs to load-path and preload some
(setq load-path (append '("." "./lisp") load-path))
(require 'vm-version)
(require 'vm-message)
(require 'vm-macro)
(require 'vm-vars)


(defun vm-custom-make-dependencies ()
  (if (load-library "cus-dep")
      (if (functionp 'Custom-make-dependencies)
	  (Custom-make-dependencies)
	(let ((generated-custom-dependencies-file "vm-cus-load.el"))
	  (custom-make-dependencies)))
    (error "Failed to load 'cus-dep'")))

(defun vm-built-autoloads (&optional autoloads-file source-dir)
  (let ((autoloads-file (or autoloads-file
                            (vm-fix-cygwin-path (car command-line-args-left))))
	(source-dir (or source-dir
                        (vm-fix-cygwin-path (car (cdr command-line-args-left)))))
        (debug-on-error t)
        (enable-local-eval nil))
    (if (not (file-exists-p source-dir))
        (error "Built directory %S does not exist!" source-dir))
    (message "Building autoloads file %S\nin directory %S." autoloads-file source-dir)
    (load-library "autoload")
    (set-buffer (find-file-noselect autoloads-file))
    (erase-buffer)
    (setq generated-autoload-file autoloads-file)
    (setq autoload-package-name "vm")
    (setq make-backup-files nil)
    (if (featurep 'xemacs)
        (progn
          (update-autoloads-from-directory source-dir)
          (fixup-autoload-buffer (concat (if autoload-package-name
                                             autoload-package-name
                                           (file-name-nondirectory defdir))
                                         "-autoloads"))
          (save-some-buffers t))
      ;; GNU Emacs 21 wants some content, but 22 does not like it ...
      (insert ";;; vm-autoloads.el --- automatically extracted autoloads\n")
      (insert ";;\n")
      (insert ";;; Code:\n")
      (if (>= emacs-major-version 22)
          (update-autoloads-from-directories source-dir)
	(if (>= emacs-major-version 21)
            (update-directory-autoloads source-dir)
          (error "Do not know how to generate autoloads"))))))

(provide 'vm-build)
