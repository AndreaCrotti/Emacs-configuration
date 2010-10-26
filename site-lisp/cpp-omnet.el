;;; cpp-omnet.el --- C++ mode for programming with omnetpp

;; Copyright (C) 2010  Andrea Crotti

;; Author: Andrea Crotti <andrea.crotti.0@gmail.com>
;; Keywords: unix

(defvar omnet-modeline-indicator " OMN"
  "call (omnet-install-mode) again if this is changed")

(defvar omnet-mode nil) 
(make-variable-buffer-local 'omnet-mode)
(put 'omnet-mode 'permanent-local t)

(defun omnet-mode (&optional arg)
  "minor mode for working with omnetpp c++ code"
  (interactive "P")
  (setq omnet-mode
	(if (null arg) (not omnet-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(provide 'omnet-mode)