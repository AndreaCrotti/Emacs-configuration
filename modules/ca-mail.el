;; setting where the mail is coming from
(setq mail-setup-with-from t)

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

(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnir-search-engine imap)
               (nnimap-stream ssl)))

;TODO: if it was received on gmail it's not copied twice
(setq gnus-message-archive-group "nnimap+gmail:Sent")

(setq gnus-secondary-select-methods
      '(
        ;; (nnimap "xype internal"
        ;;         (nnimap-address "localhost")
        ;;         (nnimap-server-port "1143"))
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

(add-hook 'gnus-select-group-hook
          (lambda ()
            (cond
             ((string-match
               "^it\\." (gnus-group-real-name gnus-newsgroup-name))
              (if (member "italian" (ispell-valid-dictionary-list))
                  (ispell-change-dictionary "italian")))
             (t
              (ispell-change-dictionary "english")))))

(add-hook 'message-setup-hook (lambda () (flyspell-mode t)))

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

(provide 'ca-mail)
