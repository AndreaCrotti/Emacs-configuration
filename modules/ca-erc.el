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

(provide 'ca-erc)
