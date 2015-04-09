(require 'ca-environment)

(require 'saveplace)

(setq visible-bell nil) ; Turn beep off
(setq ring-bell-function 'ignore)
(savehist-mode t) ; save also minibuffer history, very useful

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)

;; this toogle the ca-fullscreen for every new frame (window) created
;; (add-hook 'after-make-frame-functions 'ca-full)

;; enabling winner mode for window reconfiguration
(winner-mode t)

(autoload 'google-search-selection "google_search" "google search" t)
(autoload 'google-it "google_search" "google search" t)

(require 'browse-kill-ring)

(setq babel-preferred-from-language ca-preferred-from-language)
(setq babel-preferred-to-language ca-preferred-to-language)

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
;; (require 'powerline)
;; (powerline-default-theme)
(provide 'ca-misc)
