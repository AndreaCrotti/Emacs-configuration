(setq
 org-replace-disputed-keys t
 org-goto-interface 'outline-path-completion)

; Nicer bullets
(require 'org-bullets)
(org-bullets-mode t)

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
;;(require 'ob-sh)
(require 'ob-dot)
(require 'org-agenda)

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (clojure . t)
   (lisp . t)
   (haskell . t)
   (dot . t)
   (ruby . t)
   (scheme . t)
   ;; (R . t)
   (ditaa . t)
   (lisp . t)
   (lua . t)
   (python . t)))


(setq org-publish-project-alist
      '(

        ("org-blog"
         ;; Path to your org files.
         :base-directory "~/projects/github_personal/blog/org/"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "~/projects/github_personal/blog/jekyll/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t ;; Only export section between <body> </body>
         )


        ("org-static-blog"
         :base-directory "~/projects/github_personal/blog/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/projects/github_personal/blog/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("ian" :components ("org-blog" "org-static-blog"))
        ))

(provide 'ca-org)
