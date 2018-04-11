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

(autoload 'dict "dict" "dictionary mode" t)
; TODO: add some thing-at-point to guess the current word

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

(require 'powerline)
(setq powerline-arrow-shape 'curve)
(setq powerline-default-separator-dir '(right . left))
;; These two lines you really need.
(setq sml/theme 'powerline)
(sml/setup)

(provide 'ca-misc)
