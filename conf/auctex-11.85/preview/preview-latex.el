;;; preview-latex.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (preview-report-bug LaTeX-preview-setup preview-install-styles)
;;;;;;  "preview" "preview.el" (18341 54635))
;;; Generated autoloads from preview.el

(autoload (quote preview-install-styles) "preview" "\
Installs the TeX style files into a permanent location.
This must be in the TeX search path.  If FORCE-OVERWRITE is greater
than 1, files will get overwritten without query, if it is less
than 1 or nil, the operation will fail.  The default of 1 for interactive
use will query.

Similarly FORCE-SAVE can be used for saving
`preview-TeX-style-dir' to record the fact that the uninstalled
files are no longer needed in the search path.

\(fn DIR &optional FORCE-OVERWRITE FORCE-SAVE)" t nil)

(autoload (quote LaTeX-preview-setup) "preview" "\
Hook function for embedding the preview package into AUCTeX.
This is called by `LaTeX-mode-hook' and changes AUCTeX variables
to add the preview functionality.

\(fn)" nil nil)
 (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)

(autoload (quote preview-report-bug) "preview" "\
Report a bug in the preview-latex package.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; preview-latex.el ends here

(add-to-list 'load-path (expand-file-name "auctex" (file-name-directory load-file-name)))
(defvar preview-datadir (expand-file-name "auctex" (file-name-directory load-file-name)))

