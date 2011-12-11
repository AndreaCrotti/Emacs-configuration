(add-to-list 'load-path (make-conf-path "org-mode/lisp"))
(add-to-list 'load-path (make-conf-path "org-mode/contrib/babel/lisp"))
(add-to-list 'load-path (make-conf-path "org-mode/contrib/babel/lisp/langs"))

(autoload 'org-mode (make-conf-path "org-mode/lisp/org") "from git org mode" t)

(setq org-replace-disputed-keys t)

(defun ca-hide-org-buffers (arg)
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

;; Defining a setup where org-mode takes care of remember notes
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))

;; TODO: is it possible to use autoload here?
;; (require 'ob-ditaa)
;; (require 'ob-sh)
;; (require 'ob-python)
;; (require 'ob-ruby)
;; (require 'ob-dot)

(autoload 'ob-ditaa "ob-ditaa" "ob-ditaa" t)
(autoload 'ob-sh "ob-sh" "ob-sh" t)
(autoload 'ob-python "ob-python" "ob-python" t)
(autoload 'ob-ruby "ob-ruby" "ob-ruby" t)
(autoload 'ob-dot "ob-dot" "ob-dot" t)

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
