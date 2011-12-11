
;TODO: make it automatically the right size (check why frame-width does not work)
(defun ca-make-org-agenda-buffer ()
  "Generates a small frame below for showing the agenda"
  (interactive)
  (let ((org-agenda-frame
         (make-frame
          '((name . "org-agenda")
            (width . 200)
            (heigth . 10)
            (minibuffer . t)))))
    (with-selected-frame org-agenda-frame
      (set-frame-position org-agenda-frame 0 400)
      (bury-buffer)
      (org-agenda 0 "t"))))

(provide 'ca-frames)