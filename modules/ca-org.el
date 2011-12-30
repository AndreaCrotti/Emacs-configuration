(add-to-list 'load-path (make-conf-path "org-mode/lisp"))
(add-to-list 'load-path (make-conf-path "org-mode/contrib/babel/lisp"))
(add-to-list 'load-path (make-conf-path "org-mode/contrib/babel/lisp/langs"))
(add-to-list 'load-path (make-conf-path "org-mode/contrib/lisp"))

(autoload 'org-mode (make-conf-path "org-mode/lisp/org") "from git org mode" t)

(setq
 org-replace-disputed-keys t
 org-goto-interface 'outline-path-completion)

;; org contact settings
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
(require 'ob-ditaa)
(require 'ob-sh)
(require 'ob-python)
(require 'ob-ruby)
(require 'ob-dot)

;; TODO: first check if it's installed maybe
;; (setq org-latex-to-pdf-process '("texi2dvi -p -b -c -V %f"))

;; TODO: check if this is really useful and how to autocomplete it
(org-add-link-type "ebib" 'ebib)

; open in another window and restore the configuration on closing
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-restore-windows-after-quit t)
; don't want to see the finished tasks in the agenda
(setq org-agenda-skip-scheduled-if-done t)

(setq org-struct-hooks
      '(message-mode-hook
        mail-mode-hook))

;TODO: move to some other settings
(dolist (hook org-struct-hooks)
  (add-hook hook 'turn-on-orgstruct)
  (add-hook hook 'turn-on-orgtbl))

(provide 'ca-org)
