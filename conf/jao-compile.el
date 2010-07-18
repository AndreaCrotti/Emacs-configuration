;;; compilation buffers:
(setq compilation-scroll-output t)

;;; compile using root directory:
(defvar jao-compilation-dominating-files
  '("Makefile" "configure.ac" "bootstrap.sh" "aclocal.m4"))

(defun jao-path-relative-to (path base)
  (let* ((path (file-name-directory path))
         (base (file-name-directory base))
         (blen (length base)))
    (if (<= (length path) blen)
        path
      (if (string-equal base (substring path 0 blen))
          (substring path blen)
        path))))

(defun jao-compilation-find-root (file doms)
  (cond ((null doms) (file-name-directory file))
        ((locate-dominating-file file (car doms)))
        (t (jao-compilation-find-root file (cdr doms)))))

(defun jao-compilation-root ()
  (let ((default-directory
          (expand-file-name
           (jao-compilation-find-root (buffer-file-name)
                                      jao-compilation-dominating-files))))
    (let* ((dir (file-name-directory (buffer-file-name)))
           (rel-path (jao-path-relative-to dir default-directory)))
      (if (file-directory-p "build")
          (expand-file-name rel-path (expand-file-name "build/"))
        default-directory))))

(defun jao-compile ()
  (interactive)
  (let ((default-directory (jao-compilation-root))
        (compilation-read-command 'compilation-read-command))
    (call-interactively 'compile)))


(provide 'jao-compile)