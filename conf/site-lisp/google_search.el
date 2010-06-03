(require 'url)

(defvar google-search-maxlen 50
  "Maximum string length of search term.  This prevents you from accidentally
sending a five megabyte query string to Netscape.")

(defun google-it (search-string)
  "Search for SEARCH-STRING on google."
  (interactive "sSearch for: ")
  (browse-url (concat "http://www.google.com/search?client=emacs&q="
                      (url-hexify-string
                       (encode-coding-string search-string 'utf-8)))))

(defun google-search-selection ()
  "Create a Google search URL and send it to your web browser."
  (interactive)
  (let (start end term url)
    (if (or (not (fboundp 'region-exists-p)) (region-exists-p))
        (progn
          (setq start (region-beginning)
                end   (region-end))
          (if (> (- start end) google-search-maxlen)
              (setq term (buffer-substring start (+ start google-search-maxlen)))
            (setq term (buffer-substring start end)))
          (google-it term))
      (beep)
      (message "Region not active"))))

(provide 'google-search)