(require 'company)

;; TODO: something like this but this binds it too much, so need something
;; that would only bind to the company mode sub environment
;; (define-key company-mode-map "\C-n" 'company-select-next)

;; (define-key company-mode-map "\C-p" 'company-select-previous)

(setq company-tooltip-align-annotations t
      company-minimum-prefix-length 1)

(provide 'ca-company-mode)
