;; setting where the mail is coming from
(setq mail-setup-with-from t)
(require 'nnir)

;; This is just to enable flyspell in mail-mode
;; FIXME: check if this dirty hack is still needed
(defvar message-signature-separator "^-- *$" "\
    Regexp matching the signature separator.")

(autoload 'smtpmail-send-it "smtpmail")

(setq
 message-send-mail-function 'smtpmail-send-it
 smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
 smtpmail-auth-credentials '(("smtp.gmail.com" 587 "andrea.crotti.0@gmail.com" nil))
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587)

(setq
 compose-mail-user-agent-warnings nil
 ;; message-mode is a superset of mail-mode and nicer to use
 mail-user-agent 'message-user-agent)

(setq imap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:$HOME/mail_clones/andrea_gmail")

(setq gnus-select-method
      '(nnimap "local"
               (nnir-search-engine imap)
               (nnimap-address "localhost")
               (nnimap-stream shell)))

;TODO: if it was received on gmail it's not copied twice
(setq gnus-message-archive-group "nnimap+gmail:Sent")

(setq gnus-secondary-select-methods
      '(
        (nntp "news.gmane.org")
        ;; Configuration for http://www.eternal-september.org/
        (nntp "eternal"
              (nntp-address "news.eternal-september.org")
              (nntp-port-number 119))))

(setq
 gnus-large-newsgroup 2000
 gnus-fetch-old-headers nil)

;; close idle connections for 30 minutes
;  (gnus-demon-add-handler 'gnus-group-get-new-news 2 t)
;  (gnus-demon-init)

;; add the topic groups
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;TODO: add something similar when writing to different people
;or use auto-dictionary otherwise to try to understand the rightthing
(add-hook 'gnus-select-group-hook
          (lambda ()
            (cond
             ((string-match
               "^it\\." (gnus-group-real-name gnus-newsgroup-name))
              (if (member "italian" (ispell-valid-dictionary-list))
                  (ispell-change-dictionary "italian")))
             (t
              (ispell-change-dictionary "english")))))

(add-hook 'message-setup-hook
          (lambda () (flyspell-mode t)))

;; add an additional window buffer with the tree
(setq gnus-use-trees t
      gnus-generate-tree-function 'gnus-generate-horizontal-tree
      gnus-tree-minimize-window nil)

(gnus-add-configuration
 '(article
   (vertical 1.0
             (horizontal 0.25
                         (summary 0.75 point)
                         (tree 1.0))
             (article 1.0))))

(setq gnus-thread-hide-subtree 'gnus-article-unread-p)

(setq gnus-posting-styles
      '((".*"
         (name "Andrea Crotti")
         (address "andrea.crotti.0@gmail.com"))
        (".*rwth.*"
         (address "andrea.crotti@rwth-aachen.de"))
        ("emacs"
         (signature (emacs-version)))))

;; Changing modeline to include also the date of the message
(setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s--%d\n")

;; enable smileys
(setq gnus-treat-display-smileys t)

(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 70 (group 1.0))
               (vertical 1.0
                         (summary 0.3 point)
                         (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 70 (group 1.0))
               (vertical 1.0 (summary 1.0 point)))))

(add-hook 'gnus-started-hook
          (lambda ()
            (when (buffer-live-p gnus-dribble-buffer)
              (with-current-buffer gnus-dribble-buffer
                (setq buffer-save-without-query t)))))

;; this variable has to be set globally somewhere else
(when (boundp 'local-machine-is-gnus-slave)
  (add-hook 'gnus-before-startup-hook
            (lambda () (ca-gnussync 'from-remote)))
  (add-hook 'gnus-after-exiting-gnus-hook
            (lambda () (ca-gnussync 'to-remote))))

; automatically start offlineimap gnus starts
(require 'offlineimap)
(add-hook 'gnus-before-startup-hook 'offlineimap)

(require 'notmuch)
(add-hook 'gnus-group-mode-hook 'lld-notmuch-shortcut)
(require 'org-gnus)

(defun lld-notmuch-shortcut ()
  (define-key gnus-group-mode-map "GG" 'notmuch-search))

(defun lld-notmuch-file-to-group (file)
  "Calculate the Gnus group name from the given file name.
    "
  (let ((group (file-name-directory (directory-file-name (file-name-directory file)))))
    (setq group (replace-regexp-in-string ".*/Maildir/" "nnimap+local:" group))
    (setq group (replace-regexp-in-string "/$" "" group))
    (if (string-match ":$" group)
        (concat group "INBOX")
      (replace-regexp-in-string ":\\." ":" group))))

(defun lld-notmuch-goto-message-in-gnus ()
  "Open a summary buffer containing the current notmuch
    article."
  (interactive)
  (let ((group (lld-notmuch-file-to-group (notmuch-show-get-filename)))
        (message-id (replace-regexp-in-string
                     "^id:" "" (notmuch-show-get-message-id))))
    (setq message-id (replace-regexp-in-string "\"" "" message-id))
    (if (and group message-id)
        (progn
          (switch-to-buffer "*Group*")
          (org-gnus-follow-link group message-id))
      (message "Couldn't get relevant infos for switching to Gnus."))))

(define-key notmuch-show-mode-map (kbd "C-c C-c") 'lld-notmuch-goto-message-in-gnus)

(require 'org)

(setq org-struct-hooks
      '(message-mode-hook
        mail-mode-hook))

(dolist (hook org-struct-hooks)
  (add-hook hook 'orgstruct++-mode 'append)
  (add-hook hook 'orgtbl-mode 'append)
  (add-hook hook 'turn-on-auto-fill 'append))

(provide 'ca-mail)
