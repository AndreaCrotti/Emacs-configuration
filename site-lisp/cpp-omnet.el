;;; cpp-omnet.el --- C++ mode for programming with omnetpp

;; Copyright (C) 2010  Andrea Crotti

;; Author: Andrea Crotti <andrea.crotti.0@gmail.com>
;; Keywords: unix

(require 'derived)

(define-derived-mode cpp-omnet-mode c++-mode "C++ Omnet mode"
  "Major mode for editing c++ files used with omnet++"
  )

(provide 'cpp-omnet-mode)