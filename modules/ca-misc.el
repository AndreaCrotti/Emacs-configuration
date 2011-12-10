
(require 'saveplace)

(setq visible-bell nil) ; Turn beep off
(setq ring-bell-function 'ignore)
(savehist-mode t) ; save also minibuffer history, very useful

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)

(defun ca-toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(defun ca-full (&optional f)
  (interactive)
  (if
      ca-mac
      ;; included in emacs 23.2
      (ns-toggle-ca-fullscreen)
    (ca-toggle-fullscreen)))

;; this toogle the ca-fullscreen for every new frame (window) created
;; (add-hook 'after-make-frame-functions 'ca-full)

;; enabling winner mode for window reconfiguration
(winner-mode t)

(autoload 'google-search-selection "google_search" "google search" t)
(autoload 'google-it "google_search" "google search" t)

(require 'browse-kill-ring)

(setq babel-preferred-from-language "German")
(setq babel-preferred-to-language "English")

(autoload 'babel-region-default "babel" "translating default" t)
(autoload 'babel-region "babel" "translating a region" t)
(autoload 'babel "babel" "translating interactively" t)
(autoload 'babel-buffer "babel" "translate buffer" t)

(setq text-translator-display-popup t)
(setq text-translator-default-engine "google.com_deen")

(defun ca-text-translator-region-or-thing-at-point (&optional prompt)
  (interactive)
  "If mark is active, return the region, otherwise, thing at point."
  (cond
   (mark-active
    (buffer-substring-no-properties (region-beginning) (region-end)))
   (t
    (thing-at-point 'symbol ))))

(autoload 'dict "dict" "dictionary mode" t)
; TODO: add some thing-at-point to guess the current word
(defun ca-dictionary-search ()
  "look for a word here"
  (interactive)
  (let ((word (current-word))
        (enable-recursive-minibuffers t)
        (val))
    (setq val (read-from-minibuffer
               (concat "Word"
                       (when word
                         (concat " (" word ")"))
                       ": ")))
    (dictionary-new-search
     (cons (cond
            ((and (equal val "") word)
             word)
            ((> (length val) 0)
             val)
            (t
             (error "No word to lookup")))
           dictionary-default-dictionary))))

(setq conkeror-file-path "~/scripts/conkeror")

(when (file-exists-p conkeror-file-path)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program conkeror-file-path))

(defadvice comint-send-eof (around warn-me-please compile activate)
  "Confirm EOF when called interactively, because accidental EOF sucks."
  (when (or (not (member this-command '(comint-send-eof
                                        comint-delchar-or-maybe-eof)))
            (y-or-n-p "Really exit? "))
    ad-do-it))

(if window-system
    (progn
      (require 'server)
      (if
          (not (server-running-p))
          (server-start)
        (message "server already running, check your emacser"))))

;; TODO: make it a defcustom also, or better locate it automatically
(setq fortune-dir "/opt/local/share/games/fortune/")

(defun ca-do-applescript (str)
  "Synchronously run applescript STR."
  (with-temp-buffer
    (insert str)
    (shell-command-on-region (point-min) (point-max) "osascript" nil t)
    (buffer-string)))

(defun ca-mac-open-terminal ()
  (interactive)
  (let ((dir ""))
    (cond
     ((and (local-variable-p 'dired-directory) dired-directory)
      (setq dir dired-directory))
     ((stringp (buffer-file-name))
      (setq dir (file-name-directory (buffer-file-name))))
     )
    (ca-do-applescript
     (format "
tell application \"Terminal\"
  activate
  try
    do script with command \"cd %s\"
  on error
    beep
  end try
end tell" dir))))

(defun ca-growl-popup (msg)
  "Pop up a growl notification with MSG, or display an Emacs message.
The \"ca-growlnotify\" program is used if `window-system' is non-nil and
the program is found in `exec-path'; otherwise `message' is used."
  (interactive)
  (if (and window-system (executable-find "ca-growlnotify"))
      (shell-command (concat "growlnotify -a /Applications/Emacs.app/ -m "
                             (shell-quote-argument msg)))
    (message msg)))

(defun ca-popup-last ()
  (interactive)
  (let
      ((last-key (key-description (this-command-keys))))
    ;; check if we don't have a "stupid" sequence
    (unless
        (= (length (this-command-keys-vector)) 1)
        (ca-growl-popup last-key))))

;TODO: make it an external package and better a minor-mode, switching would also be much easier

(setq ca-growl-mode nil)

(defun ca-growl ()
  (interactive)
  (if (not ca-growl-mode)
      (progn
        (message "enabling growl mode notification")
        (add-hook 'pre-command-hook 'ca-popup-last)
        (setq ca-growl-mode t))
    (progn
      (setq-default pre-command-hook (remq 'ca-popup-last pre-command-hook))
      (message "disabling growl mode notification")
      (setq ca-growl-mode nil))))

(when ca-linux
  (progn
    (defun ca-vol-down ()
      (interactive)
      (shell-command "amixer -c 0 set PCM 3%- >/dev/null"))

    (defun ca-vol-up ()
      (interactive)
      (shell-command "amixer -c 0 set PCM 3%+ >/dev/null"))))

(defun ca-presentation-mode ()
  "what to enable in a presentation mode"
  ;TODO: also add a function to cancel all this changes
  (interactive)
  (ca-growl)
  (color-theme-high-contrast)
  (global-semantic-decoration-mode -1)
  (global-semantic-idle-summary-mode -1)
  (add-hook 'python-mode-hook (function ca-activate-flymake)))
