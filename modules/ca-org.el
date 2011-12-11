(require 'org)
(setq org-replace-disputed-keys t)

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

;; from http://orgmode.org/worg/org-tutorials/org-google-sync.html#sec-3
;;; define categories that should be excluded
(setq org-export-exclude-category (list "google" "private"))

;;; define filter. The filter is called on each entry in the agenda.
;;; It defines a regexp to search for two timestamps, gets the start
;;; and end point of the entry and does a regexp search. It also
;;; checks if the category of the entry is in an exclude list and
;;; returns either t or nil to skip or include the entry.

(defun ca-org-mycal-export-limit ()
  "Limit the export to items that have a date, time and a range. Also exclude certain categories."
  (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ... [0-9]\\{2\\}:[0-9]\\{2\\}[^\r\n>]*?\
\)>")

  (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
  (save-excursion
                                        ; get categories
    (setq mycategory (org-get-category))
                                        ; get start and end of tree
    (org-back-to-heading t)
    (setq mystart    (point))
    (org-end-of-subtree)
    (setq myend      (point))
    (goto-char mystart)
                                        ; search for timerange
    (setq myresult (re-search-forward org-tstr-regexp myend t))
                                        ; search for categories to exclude
    (setq mycatp (member mycategory org-export-exclude-category))
                                        ; return t if ok, nil when not ok
    (if (and myresult (not mycatp)) t nil)))

;;; activate filter and call export function
(defun ca-org-mycal-export ()
  (let ((org-icalendar-verify-function 'org-mycal-export-limit))
    (org-export-icalendar-combine-agenda-files)))

(add-to-list 'load-path (make-conf-path "org-mode/contrib/lisp"))
(require 'org-contacts)

(setq org-capture-mail-only-template
      '("c" "Contacts" entry (file "~/org/test_contacts.org")
       "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:"))

(setq org-capture-mail-birthday-template
      '("b" "ContactBirthday" entry (file "~/org/test_contacts.org")
       "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:BIRTHDAY: %t
:END:"))

;; set the template to store the contacts
(setq org-capture-templates
      (list
       org-capture-mail-only-template
       org-capture-mail-birthday-template))

(setq org-log-done 'note)

(setq org-todo-keywords
      '((sequence "TODO(t)" "FEEDBACK(f)" "VERIFY(v)" "|" "DONE(d)" "DELEGATED(D)" "REJECTED(r)")))

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(add-to-list 'Info-default-directory-list (make-conf-path "org-mode/doc/"))

;; Clock configuration
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

(setq org-completion-use-ido t)
; with ido enabled the following is not necessary anymore
(setq org-outline-path-complete-in-steps nil)

;; make it like a macro, takes a body and executes it
(defun ca-check-org-mode ()
  "check if the buffer is in org mode"
  (if
      (eq major-mode 'org-mode)
      t
    (message "this action is possible only in org mode")))

; Not used at the moment
(defun ca-org-add-eventually()
  "Adding a file to org-agenda when saved"
  (interactive)
  (if
      (ca-org-agenda-is-filtered-p (buffer-file-name))
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


(defcustom ca-org-agenda-filter-remote-files
  t
  "filter buffers open with tramp-mode"
  :group 'ca
  :type 'boolean)

(defcustom ca-org-brainstorm-ideas 10
  "number of brainstorm ideas"
  :group 'ca)

(defun ca-org-brainstorm ()
  "starts a brainstorming of ideas"
  (interactive)
  (save-excursion
    (dotimes (i ca-org-brainstorm-ideas)
      (org-meta-return))))

(setq questions '("Who" "What" "When" "Where" "Why" "How"))

(defun ca-prompt-ideas ()
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

; set conkeror as the default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")

;; TODO: check if this is really useful and how to autocomplete it
(org-add-link-type "ebib" 'ebib)

'(org-refile-targets (quote (("~/org/gtd.org" :maxlevel . 1)
                             ("~/org/someday.org" :level . 2))))

; open in another window and restore the configuration on closing
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-restore-windows-after-quit t)
; don't want to see the finished tasks in the agenda
(setq org-agenda-skip-scheduled-if-done t)

(setq org-struct-hooks
      '(message-mode-hook
        mail-mode-hook))

(dolist (hook org-struct-hooks)
  (add-hook hook 'turn-on-orgstruct)
  (add-hook hook 'turn-on-orgtbl))

(setq org-footnote-tag-for-non-org-mode-files "*-*-*-*-*-*-*-*-*-*")

(provide 'ca-org)
