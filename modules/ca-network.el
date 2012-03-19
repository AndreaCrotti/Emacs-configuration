
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

(setq compose-mail-user-agent-warnings nil)
;; message-mode is a superset of mail-mode and nicer to use
(setq mail-user-agent 'message-user-agent)

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

(setq ca-gnussync-rsync-binary "/opt/local/bin/rsync")
(setq ca-gnussync-rsync-options "-auRvzp --delete")
(setq ca-gnussync-extra-files nil)

(setq ca-gnussync-files "~/News ~/Mail .newsrc .newsrc.eld")

(defun ca-gnussync-to-remote ()
  (format "%s %s %s %s:" ca-gnussync-rsync-binary ca-gnussync-rsync-options ca-gnussync-files ca-gnussync-remote))

(defun ca-gnussync-from-remote ()
  (format "%s %s %s:'%s' ~/" ca-gnussync-rsync-binary ca-gnussync-rsync-options ca-gnussync-remote ca-gnussync-files))

(defun ca-gnussync (direction)
  "Synchronize to or from the server"
  (interactive "Sdirection:")
  (let
      ((bufname (get-buffer-create "*GNUS SYNC*"))
       (command
        (cond
         ((eq direction 'to-remote) (ca-gnussync-to-remote))
         ((eq direction 'from-remote) (ca-gnussync-from-remote)))))
    (cd "~")
    (when (y-or-n-p (format "running command %s" command))
      (switch-to-buffer bufname)
      (shell-command command bufname))))

;; this variable has to be set globally somewhere else
(when (boundp 'local-machine-is-gnus-slave)
  (add-hook 'gnus-before-startup-hook (lambda () (ca-gnussync 'from-remote)))
  (add-hook 'gnus-after-exiting-gnus-hook (lambda () (ca-gnussync 'to-remote))))

;; This is an example of how to make a new command.  Type "/uptime" to
;; use it.
(defun erc-cmd-UPTIME (&rest ignore)
  "Display the uptime of the system, as well as some load-related
     stuff, to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          ", load average: " "] {Load average} ["
          ;; Collapse spaces, remove
          (replace-regexp-in-string
           " +" " "
           ;; Remove beginning and trailing whitespace
           (replace-regexp-in-string
            "^ +\\|[ \n]+$" ""
            (shell-command-to-string "uptime"))))))
    (erc-send-message
     (concat "{Uptime} [" uname-output "]"))))

;; Join the #emacs and #erc channels whenever connecting to Freenode.
(setq erc-autojoin-channels-alist
      '(("freenode.net"
         "#mailman" "#pelican"
         "#emacs" "#erc" "#ruby-lang"
         "#python" "#git" "#github" "#c" "#c++" "#conkeror"
         "#android" "#latex" "#org-mode" "#postfix" "#procmail"
         "#android-devel" "#libav-devel" "#archlinux" "#xmonad" "#ledger"
         "#scipy" "#haskell" "#macosx" "#scala" "#ubuntu" "#clojure")))

;; highlight in the modeline only when my nick is cited
(setq erc-current-nick-highlight-type 'nick)

;; enable logging (by default logs everything to ~/log
(setq erc-log-mode t)

;TODO: add something to remove also the renames
(erc-replace-mode t)
(setq erc-replace-alist
      '(("\\*.*? has changed.*?to \\+v \\(\\w+\\)" . "\\1 is online")
        ("\\*.*? has changed.*?to \\-v \\(\\w+\\)" . "\\1 is away")))

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; ignore the useless messages
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(autoload 'erc-nicklist "erc-nicklist" "erc nicklist" t)

(setq
 rfc-url-save-directory "~/rfc"
 rfc-insert-content-url-hook '(rfc-url-save)
 rfc-index-url "http://www.ietf.org/iesg/1rfc_index.txt"
 rfc-archive-alist (list (concat rfc-url-save-directory "/rfc.zip")
                         rfc-url-save-directory
                         "http://www.ietf.org/rfc/"))

(require 'irfc)
(setq irfc-directory "~/rfcs")
(add-to-list 'auto-mode-alist
             '("/rfc[0-9]+\\.txt\\'" . irfc-mode))


(provide 'ca-network)
