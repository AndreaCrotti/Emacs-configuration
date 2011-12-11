(setq current "monaco-12")
(setq font-list
      (list "monaco-12" "inconsolata-14" "courier-13"))

(defun ca-cycle-font ()
  "Change font in current frame"
  (interactive)

  (let (fontToUse currentState)
    ;; states starts from 1.
    (setq currentState (if (get this-command 'state) (get this-command 'state) 1))
    (setq fontToUse (nth (1- currentState) font-list))

    (set-frame-parameter nil 'font fontToUse)
    (message "Current font is: %s" fontToUse)
    (put this-command 'state (1+ (% currentState (length font-list))))
    (redraw-frame (selected-frame))))

(frame-parameter nil 'font)

(provide 'ca-fonts)
