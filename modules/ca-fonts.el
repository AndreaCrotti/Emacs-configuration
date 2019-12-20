(require 'ca-customs)

(defun ca-cycle-font ()
  "Change font in current frame"
  (interactive)

  (let (fontToUse currentState)
    ;; states starts from 1.
    (setq currentState (if (get this-command 'state) (get this-command 'state) 1))
    (setq fontToUse (nth (1- currentState) ca-font-list))

    (set-frame-parameter nil 'font fontToUse)
    (message "Current font is: %s" fontToUse)
    (put this-command 'state (1+ (% currentState (length ca-font-list))))
    (redraw-frame (selected-frame))))

;FIXME: this is not set automatically for new frames
;; (set-default-font ca-preferred-font)

;; ubuntu fonts to set on various screens
(defun ca-big ()
  (interactive)
  (set-default-font "-unknown-Ubuntu Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1"))

(defun ca-small ()
  (interactive)
  (set-default-font "-unknown-Ubuntu Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"))

(defun ca-inconsolata ()
  (interactive)
  (set-default-font "-PfEd-Inconsolata-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1"))

(defun ca-inconsolata-big ()
  (interactive)
  (set-default-font "-PfEd-Inconsolata-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1"))


(defun menlo-big ()
  (interactive)
  (set-default-font "-*-Menlo-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"))

(set-default-font "-*-Menlo-normal-normal-normal-*-22-*-*-*-m-0-iso10646-1")

(defun ca-inconsolata-small ()
  (interactive)
  (set-default-font "-PfEd-Inconsolata-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")2)

(defun ca-inconsolata-linux-big ()
  (interactive)
  (set-default-font "Inconsolata:pixelsize=17:foundry=CYRE:weight=normal:slant=normal:width=normal:spacing=100:scalable=true"))

(defun ca-inconsolata-linux-small ()
  (interactive)
  (set-default-font "Inconsolata:pixelsize=14:foundry=CYRE:weight=normal:slant=normal:width=normal:spacing=100:scalable=true"))

(provide 'ca-fonts)
