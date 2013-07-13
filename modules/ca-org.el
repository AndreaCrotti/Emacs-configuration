(setq
 org-replace-disputed-keys t
 org-goto-interface 'outline-path-completion)

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

(setq
 org-enforce-todo-dependencies t
 org-enforce-todo-checkbox-dependencies t)

;; Clock configuration
(setq org-clock-persist t)
;; (org-clock-persistence-insinuate)

(setq
 org-completion-use-ido t
 org-outline-path-complete-in-steps nil)

;; Defining a setup where org-mode takes care of remember notes
(setq
 org-directory "~/org/"
 org-default-notes-file (concat org-directory "notes.org"))

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

(setq
; open in another window and restore the configuration on closing
 org-agenda-window-setup 'other-window
 org-agenda-restore-windows-after-quit t
 ; don't show if done in agenda
 org-agenda-skip-scheduled-if-done t)

(setq org-link-abbrev-alist
      '(
        ("google"   . "http://www.google.com/search?q=")
        ("gmap"     . "http://maps.google.com/maps?q=%s")
        ("omap"     . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
        ("mailman_bug" . "https://bugs.launchpad.net/mailman/+bug/%s"
        )))

(provide 'ca-org)
