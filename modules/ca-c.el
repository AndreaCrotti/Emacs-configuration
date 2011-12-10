
(setq c-default-style
      '((java-mode . "java") (awk-mode . "awk") (other . "cc-mode")))

(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)

(autoload 'cclookup-lookup "cclookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'cclookup-update "cclookup"
  "Run cclookup-update and create the database at `cclookup-db-file'." t)

(setq cclookup-program (make-conf-path "cclookup/cclookup.py"))
(setq cclookup-db-file (make-conf-path "cclookup/cclookup.db"))

(c-add-style "qt-gnu"
             '("gnu"
               (c-access-key .
                             "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
               (c-basic-offset . 4)))

(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)
(add-hook 'c++-mode-hook (lambda () (local-set-key "\C-cm" #'expand-member-functions)))

(defun ca-is-new-file ()
 "Check if it's a new file"
 (not (file-exists-p buffer-file-name)))

(defun ca-insert-header ()
  "try to insert the header smartly"
  (when
      (ca-is-new-file)
      (let
          ((snip (ca-find-matching-snippet (file-name-nondirectory (buffer-file-name)))))
        (if
            snip
            (ca-insert-at-startup (cdr snip))))))

(defun ca-find-matching-snippet (filename)
  (assoc-if (lambda (x) (string-match x filename))
                 ca-auto-header-conses))

(defun ca-insert-at-startup (snippet)
  "try to expand a snippet at startup"
  (if
      (yes-or-no-p (format "expand snippet %s?" snippet))
      (progn
        (insert snippet)
        ;; add checking
        (yas/expand))))

(defcustom ca-auto-header-conses
      '(
        ("setup.py" . "setup")
        ("\.sh$" . "bash")
        ("\.h$"  . "once")
        ("\.hpp$" . "once"))
      "snippets to expand per file extension"
      :group 'ca
      :type 'list)

(add-hook 'find-file-hook 'ca-insert-header)

(defun ca-cpp-header-file-p ()
  "Return non-nil, if in a C++ header."
  (and (string-match "\\.h$"
                     (or (buffer-file-name)
                         (buffer-name)))
       (save-excursion
         (re-search-forward "\\_<class\\_>" nil t))))

(add-to-list 'magic-mode-alist
             '(ca-cpp-header-file-p . c++-mode))

;;  (add-hook 'c++-mode-hook 'ca-is-omnet-cpp-file)
;; (remove-hook 'c++-mode-hook 'ca-is-omnet-cpp-file)
;; this means that the variable is set locally
(make-variable-buffer-local 'changing-to-omnet)

(defun ca-is-omnet-cpp-file ()
  "check if the file is to be considered omnet-mode or not"
  (when
      (and (or
            (not changing-to-omnet)
            (file-exists-p "omnetpp.ini")
            (search-forward "<omnetpp.h>")))
    (setq changing-to-omnet t)
    (cpp-omnet-mode)
    (setq changing-to-omnet nil)))
