(require 'desktop)

(desktop-save-mode -1)
;; save always
(setq desktop-save nil)
(defun ca-print-desktop ()
  (interactive)
  (message "current desktop is %s" desktop-dirname))

(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)

;; name and mode of buffers to forget
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))

(defun ca-dired-git-current ()
  (ca-dired-git-files  desktop-dirname))

(defcustom ca-dired-git-after-desktop
  nil
  "asking to open a dired buffer every time"
  :group 'ca
  :type 'boolean)

;; first we have to pass to the right dir
(if ca-dired-git-after-desktop
    (add-hook 'desktop-after-read-hook 'ca-dired-git-current))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))

(setq desktop-clear-preserve-buffers
      (append '("\\.newsrc-dribble" "\\.org$" "eternal" "\\*shell\\*" "\\*group\\*" "\\*ielm\\*") desktop-clear-preserve-buffers))

(provide 'ca-desktop)
