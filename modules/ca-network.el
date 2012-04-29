(require 'ca-erc)
(require 'ca-mail)

(require 'irfc)
(setq irfc-directory "~/rfcs")

(when (not (file-directory-p irfc-directory))
  (message "creating directory %s to store the rfc files" irfc-directory)
  (make-directory irfc-directory))

(add-to-list 'auto-mode-alist
             '("/rfc[0-9]+\\.txt\\'" . irfc-mode))

(provide 'ca-network)
