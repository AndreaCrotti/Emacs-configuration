(defvar base-url "https://www.terraform.io/docs/providers/aws/r/%s.html")

(defun tg-doc (module)
  "Open terraform documentation"
  (interactive "sModule:\n")
  (browse-url-chromium (format base-url module)))

(provide 'ca-devops)
