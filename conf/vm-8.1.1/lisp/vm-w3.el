;;; vm-w3.el --- additional functions to make VM use w3 for HTML mails

;; Copyright (C) 2008 Robert Widhopf-Fenk

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; You need to have w3 installed for this module to work.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'advice)
  (require 'vm-version)
  (require 'vm-mime)
  (require 'vm-vars))

(eval-and-compile
  (vm-load-features '(w3)))

(defvar vm-w3-text/html-message nil
  "The currently displayed message.")

(defvar url-working-buffer)
(defvar url-current-content-length)
(defvar url-current-mime-encoding)
(defvar url-current-mime-type)
(defvar url-current-mime-headers)

(defun vm-w3-cid-retrieve (url)
  "Insert content of URL."
  (set-buffer (get-buffer-create url-working-buffer))
  (let ((part (vm-mime-cid-retrieve url vm-w3-text/html-message))
        type encoding)
    (setq type (car (vm-mm-layout-type part)))
    (setq encoding (vm-mm-layout-encoding part))
    (if (= 0 (length type)) (setq type "text/plain"))
    (if (= 0 (length encoding)) (setq encoding "8bit"))
    (setq url-current-content-length (point-max)
          url-current-mime-type type
          url-current-mime-encoding encoding
          url-current-mime-headers (list (cons "content-type" type)
                                         (cons "content-encoding" encoding)))))

(defadvice url-cid (around vm-w3 activate)
  (if nil;(not vm-w3-text/html-message)
      ad-do-it
    (vm-w3-cid-retrieve (ad-get-arg 0))))

;;;###autoload
(defun vm-mime-display-internal-w3-text/html (start end layout)
  (setq vm-w3-text/html-message (vm-mm-layout-message layout))
  (let nil;((vm-w3-text/html-message (vm-mm-layout-message layout)))
    (w3-region start (1- end)))
  ;; remove read-only text properties
  (let ((inhibit-read-only t))
    (remove-text-properties start end '(read-only nil))))
