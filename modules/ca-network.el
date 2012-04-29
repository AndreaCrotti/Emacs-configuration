(require 'ca-erc)
(require 'ca-mail)

(setq
 rfc-url-save-directory "~/rfc"
 rfc-insert-content-url-hook '(rfc-url-save)
 rfc-index-url "http://www.ietf.org/iesg/1rfc_index.txt"
 rfc-archive-alist (list (concat rfc-url-save-directory "/rfc.zip")
                         rfc-url-save-directory
                         "http://www.ietf.org/rfc/"))

(require 'irfc)
(setq irfc-directory "~/rfcs")
(add-to-list 'auto-mode-alist
             '("/rfc[0-9]+\\.txt\\'" . irfc-mode))


(provide 'ca-network)
