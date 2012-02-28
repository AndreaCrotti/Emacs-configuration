;TODO: add a custom setting for the font, and find a way to check if they are available
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

(frame-parameter nil 'font)
(set-frame-font ca-preferred-font)

(provide 'ca-fonts)
