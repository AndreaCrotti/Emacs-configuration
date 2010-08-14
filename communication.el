(setq mail-user-agent 'sendmail-user-agent)

(setq gnus-select-method '(nntp "localhost"))
;; ;; Set also comp.* hierarchy
;; (setq gnus-secondary-select-methods
;;       '(
;;         ;; Configuration for http://www.eternal-september.org/
;;         (nntp "eternal"
;;               (nntp-authinfo-file "~/.authinfo")
;;               (nntp-address "news.eternal-september.org")
;;               (nntp-port-number 119))))

(setq gnus-large-newsgroup 500)
(setq gnus-fetch-old-headers nil)


;; Changing modeline to include also the date of the message
(setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s--%d\n")

(add-hook 'gnus-started-hook
          (lambda ()
            (when (buffer-live-p gnus-dribble-buffer)
              (with-current-buffer gnus-dribble-buffer
                (setq buffer-save-without-query t)))))
