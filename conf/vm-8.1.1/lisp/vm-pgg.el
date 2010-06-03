;;; vm-pgg.el --- PGP/MIME support for VM by pgg.el
;; 
;; Copyright (C) 2006 Robert Widhopf-Fenk
;;
;; Author:      Robert Widhopf-Fenk, Jens Gustedt
;; Status:      Tested with XEmacs 21.4.19 & VM 7.19
;; Keywords:    VM helpers
;; X-URL:       http://www.robf.de/Hacking/elisp

;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; This is a replacement for mailcrypt adding PGP/MIME support to VM.
;;
;; It requires PGG which is a standard package for XEmacs and is a part
;; of Gnus for GNU Emacs.  On Debian "apt-get install gnus" should do the
;; trick.
;;
;; It is still in BETA state thus you must explicitly load it by
;; 
;;      (and (locate-library "vm-pgg") (require 'vm-pgg))
;;
;; If you set `vm-auto-displayed-mime-content-types' and/or
;; `vm-mime-internal-content-types' make sure that they contain
;; "application/pgp-keys" or set them before loading vm-pgg.
;; Otherwise public  keys are not detected automatically .
;;
;; To customize vm-pgg use: M-x customize-group RET vm-pgg RET
;;
;; Displaying of messages in the PGP(/MIME) format will automatically trigger:
;;  * decrypted of encrypted MIME parts
;;  * verification of signed MIME parts
;;  * snarfing of public keys
;;
;; The status of the current message will also be displayed in the modeline.
;;
;; To create messages according to PGP/MIME you should use:
;;  * M-x vm-pgg-encrypt       for encrypting
;;  * M-x vm-pgg-sign          for signing
;;  * C-u M-x vm-pgg-encrypt   for encrypting + signing
;;
;; All these commands are also available in the menu PGP/MIME which is
;; activated by the minor mode `vm-pgg-compose-mode'.  There are also
;; commands for the old style clear text format as MC had them.
;;
;; If you get annoyed by answering password prompts you might want to set the
;; variable `pgg-cache-passphrase' to t and `pgg-passphrase-cache-expiry' to a
;; higher value or nil!
;;

;;; References:
;;
;; Code partially stems from the sources:
;; * mml2015.el (Gnus)
;; * mc-toplev.el (Mailcrypt)
;;
;; For PGP/MIME see:
;; * http://www.faqs.org/rfcs/rfc2015.html
;; * http://www.faqs.org/rfcs/rfc2440.html
;; * http://www.faqs.org/rfcs/rfc3156.html
;;

;;; TODO:
;;
;; * add annotation see to signed/encrypted regions.  XEmacs has annotations
;;   and GNU Emacs?  Maybe I simply use overlays at the line start without eys
;;   candy.
;; * allow attaching of other keys from key-ring
;;

;;; Code:

;; handle missing pgg.el gracefully
(eval-and-compile
  (if (and (boundp 'byte-compile-current-file) byte-compile-current-file)
      (condition-case nil
          (require 'pgg)
        (error (message "WARNING: Cannot load pgg.el, related functions may not work!")))
    (require 'pgg))

  (require 'easymenu)
  (require 'vm-version)
  (require 'vm-misc)
  (require 'vm-page)
  (require 'vm-vars)
  (require 'vm-mime)
  (require 'vm-reply)

  (require 'advice))
  

(eval-when-compile
  (require 'cl)
  ;; avoid warnings
  (defvar vm-mode-line-format)
  (defvar vm-message-pointer)
  (defvar vm-presentation-buffer)
  (defvar vm-summary-buffer)
  ;; avoid bytecompile warnings
  (defvar vm-pgg-cleartext-state nil "For interfunction communication.")
)

(defgroup vm nil
  "VM"
  :group 'mail)

(defgroup vm-pgg nil
  "PGP and PGP/MIME support for VM by PGG."
  :group  'vm)

(defface vm-pgg-bad-signature
  '((((type tty) (class color))
     (:foreground "red" :bold t))
    (((type tty))
     (:bold t))
    (((background light))
     (:foreground "red" :bold t))
    (((background dark))
     (:foreground "red" :bold t)))
  "The face used to highlight bad signature messages."
  :group 'vm-pgg
  :group 'faces)

(defface vm-pgg-good-signature
  '((((type tty) (class color))
     (:foreground "green" :bold t))
    (((type tty))
     (:bold t))
    (((background light))
     (:foreground "green4"))
    (((background dark))
     (:foreground "green")))
  "The face used to highlight good signature messages."
  :group 'vm-pgg
  :group 'faces)

(defface vm-pgg-unknown-signature-type
  '((((type tty) (class color))
     (:bold t))
    (((type tty))
     (:bold t)))
  "The face used to highlight unknown signature types."
  :group 'vm-pgg
  :group 'faces)

(defface vm-pgg-error
  '((((type tty) (class color))
     (:foreground "red" :bold t))
    (((type tty))
     (:bold t))
    (((background light))
     (:foreground "red" :bold t))
    (((background dark))
     (:foreground "red" :bold t)))
  "The face used to highlight error messages."
  :group 'vm-pgg
  :group 'faces)

(defface vm-pgg-bad-signature-modeline
  '((((type tty) (class color))
     (:inherit modeline :foreground "red" :bold t))
    (((type tty))
     (:inherit modeline :bold t))
    (((background light))
     (:inherit modeline :foreground "red" :bold t))
    (((background dark))
     (:inherit modeline :foreground "red" :bold t)))
  "The face used to highlight bad signature messages."
  :group 'vm-pgg
  :group 'faces)

(defface vm-pgg-good-signature-modeline
  '((((type tty) (class color))
     (:inherit modeline :foreground "green" :bold t))
    (((type tty))
     (:inherit modeline :bold t))
    (((background light))
     (:inherit modeline :foreground "green4"))
    (((background dark))
     (:inherit modeline :foreground "green")))
  "The face used to highlight good signature messages."
  :group 'vm-pgg
  :group 'faces)

(defface vm-pgg-unknown-signature-type-modeline
  '((((type tty) (class color))
     (:inherit modeline :bold t))
    (((type tty))
     (:inherit modeline :bold t)))
    "The face used to highlight unknown signature types."
  :group 'vm-pgg
  :group 'faces)

(defface vm-pgg-error-modeline
  '((((type tty) (class color))
     (:inherit modeline :foreground "red" :bold t))
    (((type tty))
     (:inherit modeline :bold t))
    (((background light))
     (:inherit modeline :foreground "red"))
    (((background dark))
     (:inherit modeline :foreground "red")))
  "The face used to highlight error messages."
  :group 'vm-pgg
  :group 'faces)

;; hack to work around the missing support for :inherit in XEmacs
(when (featurep 'xemacs)
  (let ((faces '(vm-pgg-bad-signature-modeline
                 vm-pgg-good-signature-modeline
                 vm-pgg-unknown-signature-type-modeline
                 vm-pgg-error-modeline))
        (faces-list (face-list))
        f)
    (while faces
      (setq f (car faces))
      (set-face-parent f 'modeline)
      (face-display-set f (custom-face-get-spec f) nil '(custom))
      (setq faces (cdr faces)))))

(defcustom vm-pgg-fetch-missing-keys t
  "*If t, PGP will try to fetch missing keys from `pgg-default-keyserver-address'."
  :group 'vm-pgg
   :type 'boolean)

(defcustom vm-pgg-auto-snarf t
  "*If t, snarfing of keys will happen automatically."
  :group 'vm-pgg
   :type 'boolean)

(defcustom vm-pgg-auto-decrypt t
  "*If t, decrypting will happen automatically."
  :group 'vm-pgg
   :type 'boolean)

(defcustom vm-pgg-get-author-headers '("From:" "Sender:")
  "*The list of headers to get the author of a mail that is to be send.
If nil, `pgg-default-user-id' is used as a fallback."
  :group 'vm-pgg
  :type '(repeat string))

(defcustom vm-pgg-sign-text-transfer-encoding 'quoted-printable
  "*The encoding used for signed MIME parts of type text.
See `vm-pgg-sign' for details."
  :group 'vm-pgg
  :type '(choice (const quoted-printable) (const base64)))

(defvar vm-pgg-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c#s" 'vm-pgg-sign)
    (define-key map "\C-c#e" 'vm-pgg-encrypt)
    (define-key map "\C-c#E" 'vm-pgg-sign-and-encrypt)
    (define-key map "\C-c#a" 'vm-pgg-ask-hook)
    (define-key map "\C-c#k" 'vm-pgg-attach-public-key)
    map))

(defvar vm-pgg-compose-mode-menu nil
  "The composition menu of vm-pgg.")

(easy-menu-define
 vm-pgg-compose-mode-menu (if (featurep 'xemacs) nil (list vm-pgg-compose-mode-map))
 "PGP/MIME compose mode menu."
 '("PGP/MIME"
   ["Sign"              vm-pgg-sign t]
   ["Encrypt"           vm-pgg-encrypt t]
   ["Sign+Encrypt"      vm-pgg-sign-and-encrypt t]
   ["Ask For An Action" vm-pgg-ask-hook t]
   "----"
   ["Attach Public Key" vm-pgg-attach-public-key t]
   ["Insert Public Key" pgg-insert-key t]))

(defvar vm-pgg-compose-mode nil
  "None-nil means PGP/MIME composition mode key bindings and menu are available.")

(make-variable-buffer-local 'vm-pgg-compose-mode)

(defun vm-pgg-compose-mode (&optional arg)
  "\nMinor mode for interfacing with cryptographic functions.

Switch mode on/off according to ARG.

\\<vm-pgg-compose-mode-map>"
  (interactive)
  (setq vm-pgg-compose-mode
	(if (null arg) (not vm-pgg-compose-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if vm-pgg-compose-mode
      (easy-menu-add vm-pgg-compose-mode-menu)
    (easy-menu-remove vm-pgg-compose-mode-menu)))

(defvar vm-pgg-compose-mode-string " vm-pgg"
  "*String to put in mode line when function `vm-pgg-compose-mode' is active.")

(defcustom vm-pgg-ask-function 'vm-pgg-prompt-for-action
  "*The function to use in `vm-pgg-ask-hook'."
  :group 'vm-pgg
  :type '(choice
          (const 
           :tag "do nothing" 
           :doc "Disable `vm-pgg-ask-hook'"
           nil)
          (const
           :tag "sign" 
           :doc "Ask whether to sign the message before sending"
           sign)
          (const
           :tag "encrypt" 
           :doc "Ask whether to encryt the message before sending"
           encrypt)
          (const
           :tag "encrypt and sign" 
           :doc "Ask whether to encrypt and sign the message before sending"
           encrypt-and-sign)
          (function
           :tag "ask for the action"
           :doc "Will prompt for an action by calling `vm-pgg-prompt-for-action'"
           vm-pgg-prompt-for-action)
          (function
           :tag "your own function" 
           :doc "It should returning one of the other const values.")))


(if (not (assq 'vm-pgg-compose-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons (cons 'vm-pgg-compose-mode vm-pgg-compose-mode-map)
		minor-mode-map-alist)))

(if (not (assq 'vm-pgg-compose-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(vm-pgg-compose-mode vm-pgg-compose-mode-string) minor-mode-alist)))

(defun vm-pgg-compose-mode-activate ()
  "Activate function `vm-pgg-compose-mode'."
  (vm-pgg-compose-mode 1))

(add-hook 'vm-mail-mode-hook 'vm-pgg-compose-mode-activate t)

(defun vm-pgg-get-emails (headers)
  "Return email addresses found in the given HEADERS."
  (let (content recipients)
    (while headers
      (setq content (vm-mail-mode-get-header-contents (car headers)))
      (when content
        (setq recipients (append (rfc822-addresses content) recipients)))
      (setq headers (cdr headers)))
    recipients))

(defvar vm-pgg-get-recipients-headers '("To:" "CC:" "BCC:")
  "The list of headers to get recipients from.")
  
(defun vm-pgg-get-recipients ()
  "Return a list of recipients."
  (vm-pgg-get-emails vm-pgg-get-recipients-headers))

(defun vm-pgg-get-author ()
  "Return the author of the message."
  (car (vm-pgg-get-emails vm-pgg-get-author-headers)))

(defun vm-pgp-goto-body-start ()
  "Goto the start of the body and return point."
  (goto-char (point-min))
  (search-forward (concat "\n" mail-header-separator "\n"))
  (goto-char (match-end 0))
  (point))

(defun vm-pgp-prepare-composition ()
  "Prepare the composition for encrypting or signing."
  ;; encode message
  (unless (vm-mail-mode-get-header-contents "MIME-Version:")
    (vm-mime-encode-composition))
  (vm-mail-mode-show-headers)
  ;; ensure newline at the end
  (goto-char (point-max))
  (skip-chars-backward " \t\r\n\f")
  (delete-region (point) (point-max))
  (insert "\n")
  ;; skip headers
  (vm-pgp-goto-body-start)
  ;; guess the author
  (make-local-variable 'pgg-default-user-id)
  (setq pgg-default-user-id
        (or
         (and vm-pgg-get-author-headers (vm-pgg-get-author))
         pgg-default-user-id)))

;;; ###autoload
(defun vm-pgg-cleartext-encrypt (sign)
  "*Encrypt the composition as cleartext and with a prefix also SIGN it."
  (interactive "P")
  (save-excursion
    (vm-pgp-prepare-composition)
    (let ((start (point)) (end   (point-max)))
      (unless (pgg-encrypt-region start end (vm-pgg-get-recipients) sign)
        (pop-to-buffer pgg-errors-buffer)
        (error "Encrypt error"))
      (delete-region start end)
      (insert-buffer-substring pgg-output-buffer))))

(defun vm-pgg-make-presentation-copy ()
  "Make a presentation copy also for cleartext PGP messages."
  (let* ((m (car vm-message-pointer))
         (layout (vm-mm-layout m)))
    ;; make a presentation copy
    (vm-make-presentation-copy m)
    (vm-save-buffer-excursion
     (vm-replace-buffer-in-windows (current-buffer)
                                   vm-presentation-buffer))
    (set-buffer vm-presentation-buffer)
    
    ;; remove From line
    (goto-char (point-min))
    (forward-line 1)
    (let ((buffer-read-only nil))
      (delete-region (point-min) (point))
      (vm-reorder-message-headers nil vm-visible-headers
                                  vm-invisible-header-regexp)
      (vm-decode-mime-message-headers m)
      (when (vectorp layout)
        ;; skip headers otherwise they get removed 
        (goto-char (point-min))
        (search-forward "\n\n")
        (vm-decode-mime-layout layout)
        (delete-region (point) (point-max)))
      (vm-energize-urls-in-message-region)
      (vm-highlight-headers-maybe)
      (vm-energize-headers-and-xfaces))))
    
(defvar vm-pgg-state nil
  "State of the currently viewed message.")

(make-variable-buffer-local 'vm-pgg-state)

(defvar vm-pgg-state-message nil
  "The message for `vm-pgg-state'.")

(make-variable-buffer-local 'vm-pgg-state-message)

(defvar vm-pgg-mode-line-items
  (let ((items '((error " ERROR" vm-pgg-error-modeline)
                 (unknown " unknown" vm-pgg-unknown-signature-type-modeline)
                 (verified " verified" vm-pgg-good-signature-modeline)))
        mode-line-items
        x i s f)
    (while (and (featurep 'xemacs) items)
      (setq x (car items)
            i (car x)
            s (cadr x)
            f (caddr x)
            x (vm-make-extent 0 (length s) s))
      (vm-set-extent-property x 'face f)
      (setq items (cdr items))
      (setq mode-line-items (append mode-line-items (list (list i x s)))))
    mode-line-items)
  "An alist mapping states to modeline strings.")

(if (not (member 'vm-pgg-state vm-mode-line-format))
    (setq vm-mode-line-format (append '("" vm-pgg-state) vm-mode-line-format)))

(defun vm-pgg-state-set (&rest states)
  "Set the message state  displayed in the modeline acording to STATES.
If STATES is nil, clear it."
  ;; clear state for a new message
  (save-excursion
    (vm-select-folder-buffer-if-possible)
    (when (not (equal (car vm-message-pointer) vm-pgg-state-message))
      (setq vm-pgg-state-message (car vm-message-pointer))
      (setq vm-pgg-state nil)
      (when vm-presentation-buffer
        (save-excursion
          (set-buffer vm-presentation-buffer)
          (setq vm-pgg-state nil)))
      (when vm-summary-buffer
        (save-excursion
          (set-buffer vm-summary-buffer)
          (setq vm-pgg-state nil))))
    ;; add prefix
    (if (and states (not vm-pgg-state))
        (setq vm-pgg-state '("PGP:")))
    ;; add new states
    (let (s)
      (while states
        (setq s (car states)
              vm-pgg-state (append vm-pgg-state
                                   (list (or (cdr (assoc s vm-pgg-mode-line-items))
                                             (format " %s" s))))
              states (cdr states))))
    ;; propagate state
    (setq states vm-pgg-state)
    (when vm-presentation-buffer
      (save-excursion
        (set-buffer vm-presentation-buffer)
        (setq vm-pgg-state states)))
    (when vm-summary-buffer
      (save-excursion
        (set-buffer vm-summary-buffer)
        (setq vm-pgg-state states)))))

(defvar vm-pgg-cleartext-begin-regexp
  "^-----BEGIN PGP \\(\\(SIGNED \\)?MESSAGE\\|PUBLIC KEY BLOCK\\)-----$"
    "Regexp used to match PGP armor.")

(defvar vm-pgg-cleartext-end-regexp
  "^-----END PGP %s-----$"
    "Regexp used to match PGP armor.")

(defcustom vm-pgg-cleartext-search-limit 4096
  "Number of bytes to peek into the message for a PGP clear text armor."
   :group 'vm-pgg
   :group 'faces)

(defun vm-pgg-cleartext-automode-button (label action)
  "Cleartext thing by a button with text LABEL and associate ACTION with it.
When the button is pressed ACTION is called."
  (save-excursion
    (unless (eq major-mode 'vm-presentation-mode)
      (vm-pgg-make-presentation-copy))
    (goto-char (match-beginning 0))
    (let ((buffer-read-only nil)
          (start (point))
          o)
      (if (re-search-forward (format vm-pgg-cleartext-end-regexp
                                     (match-string 0))
                             (point-max) t)
          (delete-region start (match-end 0)))
      (insert label)
      (setq o (make-overlay start (point)))
      (overlay-put o 'vm-pgg t)
      (overlay-put o 'face vm-mime-button-face)
      (overlay-put o 'vm-button t)
      (overlay-put o 'mouse-face 'highlight)
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap [mouse-2] action)
        (define-key keymap "\r"  action)
        (overlay-put o 'local-map keymap)))))

(defvar vm-pgg-cleartext-decoded nil
  "State of the cleartext message.")
(make-variable-buffer-local 'vm-pgg-cleartext-decoded)

(defun vm-pgg-set-cleartext-decoded ()
   (save-excursion
    (vm-select-folder-buffer)
    (setq vm-pgg-cleartext-decoded (car vm-message-pointer))))

(defun vm-pgg-cleartext-automode ()
  "Check for PGP ASCII armor and triggers automatic verification/decryption."
  (save-excursion
    (vm-select-folder-buffer-if-possible)
    (if (equal vm-pgg-cleartext-decoded (car vm-message-pointer))
        (setq vm-pgg-cleartext-decoded nil)
      (setq vm-pgg-cleartext-decoded nil)
      (if vm-presentation-buffer
          (set-buffer vm-presentation-buffer))
      (goto-char (point-min))
      (when (and (vm-mime-plain-message-p (car vm-message-pointer))
                 (re-search-forward vm-pgg-cleartext-begin-regexp
                                    (+ (point) vm-pgg-cleartext-search-limit)
                                    t))
        (cond ((string= (match-string 1) "SIGNED MESSAGE")
               (vm-pgg-set-cleartext-decoded)
               (vm-pgg-cleartext-verify))
              ((string= (match-string 1) "MESSAGE")
               (vm-pgg-set-cleartext-decoded)
               (if vm-pgg-auto-decrypt
                   (vm-pgg-cleartext-decrypt)
                 (vm-pgg-cleartext-automode-button
                  "Decrypt PGP message\n"
                  (lambda ()
                    (interactive)
                    (let ((vm-pgg-auto-decrypt t))
                      (vm-pgg-cleartext-decrypt))))))
              ((string= (match-string 1) "PUBLIC KEY BLOCK")
               (vm-pgg-set-cleartext-decoded)
               (if vm-pgg-auto-snarf
                   (vm-pgg-snarf-keys)
                 (vm-pgg-cleartext-automode-button
                  "Snarf PGP key\n"
                  (lambda ()
                    (interactive)
                    (let ((vm-pgg-auto-snarf t))
                      (vm-pgg-snarf-keys))))))
              (t
               (error "This should never happen!")))))))

(defadvice vm-preview-current-message (after vm-pgg-cleartext-automode activate)
  "Decode or check signature on clear text messages."
  (vm-pgg-state-set)
  (when (and vm-pgg-cleartext-decoded
             (not (equal vm-pgg-cleartext-decoded (car vm-message-pointer))))
    (setq vm-pgg-cleartext-decoded nil))
  (when (and (not (eq vm-system-state 'previewing))
             (not vm-mime-decoded))
    (vm-pgg-cleartext-automode)))

(defadvice vm-scroll-forward (around vm-pgg-cleartext-automode activate)
  "Decode or check signature on clear text messages."
  (let ((vm-system-state-was
         (save-excursion
           (vm-select-folder-buffer-if-possible)
           vm-system-state)))
    ad-do-it
    (vm-pgg-state-set)
    (when (and (eq vm-system-state-was 'previewing)
               (not vm-mime-decoded))
      (vm-pgg-cleartext-automode))))

;;; ###autoload
(defun vm-pgg-cleartext-sign ()
  "*Sign the message."
  (interactive)
  (save-excursion
    (vm-pgp-prepare-composition)
    (let ((start (point)) (end (point-max)))
      (unless (pgg-sign-region start end t)
        (pop-to-buffer pgg-errors-buffer)
        (error "Signing error"))
      (delete-region start end)
      (insert-buffer-substring pgg-output-buffer))))

(defun vm-pgg-cleartext-cleanup (status)
  "Removed ASCII armor and insert PGG output depending on STATUS."
  (let (start end)
    (setq start (and (re-search-forward "^-----BEGIN PGP SIGNED MESSAGE-----$")
                     (match-beginning 0))
          end   (and (search-forward "\n\n")
                     (match-end 0)))
    (delete-region start end)
    (setq start (and (re-search-forward "^-----BEGIN PGP SIGNATURE-----$")
                     (match-beginning 0))
          end (and (re-search-forward "^-----END PGP SIGNATURE-----$")
                   (match-end 0)))
    (delete-region start end)
    ;; add output from PGP
    (insert "\n")
    (let ((start (point)) end)
      (if (eq status 'error)
          (insert-buffer-substring pgg-errors-buffer)
        (insert-buffer-substring pgg-output-buffer)
        (vm-pgg-crlf-cleanup start (point)))
      (setq end (point))
      (put-text-property start end 'face
                         (if (eq status 'error)
			     'vm-pgg-bad-signature
			   'vm-pgg-good-signature)))))
  
(defadvice vm-mime-transfer-decode-region (around vm-pgg-cleartext-automode activate)
  "Decode or check signature on clear text messages parts."
  (let ((vm-pgg-part-start (point)))
    ad-do-it
    ;; BUGME should we use marks here?
    (when (and (vm-mime-text-type-layout-p (ad-get-arg 0))
               (< vm-pgg-part-start (point)))
      (save-excursion
        (save-restriction
          (narrow-to-region vm-pgg-part-start (point))
          (vm-pgg-cleartext-automode)
          (widen)
;          (set-window-start (selected-window) 0)
          ;(scroll-down 1000)
          )))))
  
(defadvice vm-mime-display-internal-text/plain (around vm-pgg-cleartext-automode activate)
  "Decode or check signature on clear text messages parts.
We use the advice here in order to avoid overwriting VMs internal text display
function.  Faces will get lost if a charset conversion happens thus we do the
cleanup here after verification and decoding took place."
  (let ((vm-pgg-cleartext-state nil)
        (start (point))
        end)
    ad-do-it
    (when vm-pgg-cleartext-state
      (setq end (point))
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (vm-pgg-cleartext-cleanup vm-pgg-cleartext-state)
        (widen)))))
    
;;; ###autoload
(defun vm-pgg-cleartext-verify ()
  "*Verify the signature in the current message."
  (interactive)
  (message "Verifying PGP cleartext message...")
  (when (interactive-p)
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer-if-possible)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty))
  
  ;; make a presentation copy
  (unless (eq major-mode 'vm-presentation-mode)
    (vm-pgg-make-presentation-copy))
  
  ;; verify
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-read-only nil)
          (status (pgg-verify-region (point) (point-max) nil 
				     vm-pgg-fetch-missing-keys)))
      
      (vm-pgg-state-set 'signed)
      (setq status (if (not status) 'error 'verified))
      (vm-pgg-state-set status)
      (if (boundp 'vm-pgg-cleartext-state)
          (setq vm-pgg-cleartext-state status)
        (vm-pgg-cleartext-cleanup status)))))

;;; ###autoload
(defun vm-pgg-cleartext-decrypt ()
  "*Decrypt the contents of the current message."
  (interactive)
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
    
  ;; make a presentation copy
  (unless (eq major-mode 'vm-presentation-mode)
    (vm-pgg-make-presentation-copy))
  (goto-char (point-min))
  
  ;; decrypt
  (let (state start end)
    (setq start (and (re-search-forward "^-----BEGIN PGP MESSAGE-----$")
                     (match-beginning 0))
          end   (and (re-search-forward "^-----END PGP MESSAGE-----$")
                     (match-end 0))
          state (condition-case nil
                    (pgg-decrypt-region start end)
                  (error nil)))
    
    (vm-pgg-state-set 'encrypted)
    
    (if (not state)
        ;; insert the error message
        (let ((buffer-read-only nil))
          (vm-pgg-state-set 'error)
          (goto-char start)
          (insert-buffer-substring pgg-errors-buffer)
          (put-text-property start (point) 'face 'vm-pgg-error))
      ;; replace it with decrypted message
      (let ((buffer-read-only nil))
        (delete-region start end)
        (insert-buffer-substring pgg-output-buffer))
      ;; if it signed then also verify it
      (goto-char start)
      (if (looking-at "^-----BEGIN PGP \\(SIGNED \\)?MESSAGE-----$")
          (vm-pgg-cleartext-verify)))))

(defun vm-pgg-crlf-cleanup (start end)
  "Convert CRLF to LF in region from START to END."
  (save-excursion
    (goto-char start)
    (while (search-forward "\r\n" end t)
      (replace-match "\n" t t))))

(defun vm-pgg-make-crlf (start end)
  "Convert CRLF to LF in region from START to END."
  (save-excursion
    (goto-char end)
    (while (search-backward "\n" start t)
      (replace-match "\r\n" t t)
      (backward-char))))

(defvar vm-pgg-mime-decoded nil
  "Saves decoded state for later use, i.e. decoding to buttons.")
(make-variable-buffer-local 'vm-pgg-mime-decoded)

(defun vm-pgg-get-mime-decoded ()
  "Return `vm-pgg-mime-decoded'."
  (save-excursion
    (vm-select-folder-buffer)
    vm-pgg-mime-decoded))

(defvar vm-pgg-recursion nil
  "Detect recursive calles.")

(defadvice vm-decode-mime-message (around vm-pgg-clear-state activate)
  "Clear the modeline state before decoding."
  (vm-select-folder-buffer)
  (when (not vm-pgg-recursion)
    (setq vm-pgg-mime-decoded vm-mime-decoded))
  (setq vm-pgg-state-message nil)
  (setq vm-pgg-state nil)
  (if (vm-mime-plain-message-p (car vm-message-pointer))
      (if vm-pgg-cleartext-decoded
          (vm-preview-current-message))
    (let ((vm-pgg-recursion t))
      ad-do-it)))

(defun vm-pgg-mime-decrypt (button)
  "Replace the BUTTON with the output from `pgg-snarf-keys'."
  (let ((vm-pgg-auto-decrypt t)
        (layout (copy-sequence (vm-extent-property button 'vm-mime-layout))))
    (vm-set-extent-property button 'vm-mime-disposable t)
    (vm-set-extent-property button 'vm-mime-layout layout)
    (goto-char (vm-extent-start-position button))
    (let ((buffer-read-only nil))
      (vm-decode-mime-layout button t))))

;;; ###autoload
(defun vm-mime-display-internal-multipart/encrypted (layout)
  "Display multipart/encrypted LAYOUT."
  (vm-pgg-state-set 'encrypted)
  (let* ((part-list (vm-mm-layout-parts layout))
         (header (car part-list))
         (message (car (cdr part-list)))
         status)
    (cond ((eq (vm-pgg-get-mime-decoded) 'decoded)
           ;; after decode the state of vm-mime-decoded is 'buttons
           nil)
          ((not (and (= (length part-list) 2)
                     (vm-mime-types-match (car (vm-mm-layout-type header))
                                          "application/pgp-encrypted")
                     ;; TODO: check version and protocol here?
                     (vm-mime-types-match (car (vm-mm-layout-type message))
                                          "application/octet-stream")))
           (insert "Unknown multipart/encrypted format."))
          ((not vm-pgg-auto-decrypt)
           ;; add a button
           (let ((buffer-read-only nil))
             (vm-mime-insert-button
              (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
              'vm-pgg-mime-decrypt
              layout nil)))
          (t
           ;; decode the message now
           (save-excursion
             (set-buffer (vm-buffer-of (vm-mm-layout-message message)))
             (save-restriction
               (widen)
               (setq status (pgg-decrypt-region (vm-mm-layout-body-start message)
                                                (vm-mm-layout-body-end message)))))
           (if (not status)
               (let ((start (point)))
                 (vm-pgg-state-set 'error)
                 (insert-buffer-substring pgg-errors-buffer)
                 (put-text-property start (point) 'face 'vm-pgg-error))
             (save-excursion
               (set-buffer pgg-output-buffer)
               (vm-pgg-crlf-cleanup (point-min) (point-max))
               (setq message (vm-mime-parse-entity-safe nil nil nil t)))
             (if message
                 (vm-decode-mime-layout message)
               (insert-buffer-substring pgg-output-buffer))
             (setq status (save-excursion
                            (set-buffer pgg-errors-buffer)
                            (goto-char (point-min))
                            ;; TODO: care for BADSIG
                            (when (re-search-forward "GOODSIG [^\n\r]+" (point-max) t)
                              (vm-pgg-state-set 'signed 'verified)
                              (buffer-substring (match-beginning 0) (match-end 0)))))
             (if status
                 (let ((start (point)))
                   (insert "\n" status "\n")
                   (put-text-property start (point) 'face 'vm-pgg-good-signature))))
           t))))

;;; ###autoload
(defun vm-mime-display-internal-multipart/signed (layout)
  "Display multipart/signed LAYOUT."
  (vm-pgg-state-set 'signed)
  (let* ((part-list (vm-mm-layout-parts layout))
         (message (car part-list))
         (signature (car (cdr part-list)))
         status signature-file start end)
    (cond ((eq (vm-pgg-get-mime-decoded) 'decoded)
           ;; after decode the state of vm-mime-decoded is 'buttons
           nil)
          ((not (and (= (length part-list) 2)
		     signature
                     ;; TODO: check version and protocol here?
                     (vm-mime-types-match (car (vm-mm-layout-type signature))
                                          "application/pgp-signature")))
           ;; insert the message
           (vm-decode-mime-layout message)
           (let (start end)
             (vm-pgg-state-set 'unknown)
             (setq start (point))
             (insert
              (format
               "******* unknown signature type %s *******\n"
               (car (and signature (vm-mm-layout-type signature)))))
             (setq end (point))
	     (when signature
	       (vm-decode-mime-layout signature))
             (put-text-property start end 'face 'vm-pgg-unknown-signature-type))
           t)
          (t 
           ;; insert the message
           (vm-decode-mime-layout message)
           ;; write signature to a temp file
           (setq start (point))
           (vm-mime-insert-mime-body signature)
           (setq end (point))
           (write-region start end
                         (setq signature-file (pgg-make-temp-file "vm-pgg-signature")))
           (delete-region start end)
           (setq start (point))
           (vm-insert-region-from-buffer (marker-buffer (vm-mm-layout-header-start
                                                         message))
                                         (vm-mm-layout-header-start message)
                                         (vm-mm-layout-body-end message))
           (setq end (point-marker))
           (vm-pgg-make-crlf start end)
           (setq status (pgg-verify-region start end signature-file
                                           vm-pgg-fetch-missing-keys))
           (delete-file signature-file)
           (delete-region start end)
           ;; now insert the content
           (insert "\n")
           (let ((start (point)) end)
             (if (not status)
                 (progn
                   (vm-pgg-state-set 'error)
                   (insert-buffer-substring pgg-errors-buffer))
               (vm-pgg-state-set 'verified)
               (insert-buffer-substring 
                (if vm-fsfemacs-p pgg-errors-buffer pgg-output-buffer))
               (vm-pgg-crlf-cleanup start (point)))
             (setq end (point))
             (put-text-property start end 'face
                                (if status 'vm-pgg-good-signature
                                  'vm-pgg-bad-signature)))
           t))))

;; we must add these in order to force VM to call our handler
(eval-and-compile
;; (if (listp vm-auto-displayed-mime-content-types)
;;       (add-to-list 'vm-auto-displayed-mime-content-types "application/pgp-keys"))
  (if (listp vm-mime-internal-content-types)
      (add-to-list 'vm-mime-internal-content-types "application/pgp-keys"))
  (add-to-list 'vm-mime-button-format-alist
	       '("application/pgp-keys" . "Snarf %d"))
  (add-to-list 'vm-mime-button-format-alist
               '("multipart/encrypted" . "Decrypt PGP/MIME message")))

(defun vm-pgg-mime-snarf-keys (button)
  "Replace the BUTTON with the output from `pgg-snarf-keys'."
  (let ((vm-pgg-auto-snarf t)
        (layout (copy-sequence (vm-extent-property button 'vm-mime-layout))))
    (vm-set-extent-property button 'vm-mime-disposable t)
    (vm-set-extent-property button 'vm-mime-layout layout)
    (goto-char (vm-extent-start-position button))
    (let ((buffer-read-only nil))
      (vm-decode-mime-layout button t))))

;;; ###autoload
(defun vm-mime-display-internal-application/pgp-keys (layout)
  "Snarf keys in LAYOUT and display result of snarfing."
  (vm-pgg-state-set 'public-key)
  ;; insert the keys
  (if vm-pgg-auto-snarf
      (let ((start (point)) end status)
        (vm-mime-insert-mime-body layout)
        (setq end (point-marker))
        (vm-mime-transfer-decode-region layout start end)
        (save-excursion
          (setq status (pgg-snarf-keys-region start end)))
        (delete-region start end)
        ;; now insert the result of snafing
        (if status
            (insert-buffer-substring pgg-output-buffer)
          (insert-buffer-substring pgg-errors-buffer)))
    (let ((buffer-read-only nil))
      (vm-mime-insert-button
       (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
       'vm-pgg-mime-snarf-keys
       layout nil)))
  t)

;;; ###autoload
(defun vm-pgg-snarf-keys ()
  "*Snarf keys from the current message."
  (interactive)
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (save-restriction
    ;; ensure we are in the right buffer
    (if vm-presentation-buffer
        (set-buffer vm-presentation-buffer))
    ;; skip headers
    (goto-char (point-min))
    (search-forward "\n\n")
    (goto-char (match-end 0))
    ;; verify
    (unless (pgg-snarf-keys)
      (error "Snarfing failed"))
    (save-excursion
      (set-buffer (if vm-fsfemacs-p pgg-errors-buffer pgg-output-buffer))
      (message (buffer-substring (point-min) (point-max))))))

;;; ###autoload
(defun vm-pgg-attach-public-key ()
  "Attach your public key to a composition."
  (interactive)
  (let* ((pgg-default-user-id
          (or
           (and vm-pgg-get-author-headers (vm-pgg-get-author))
           pgg-default-user-id))
         (description (concat "public key of " pgg-default-user-id))
         (buffer (get-buffer-create (concat " *" description "*")))
         start)
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (setq start (point))
      (pgg-insert-key)
      (if (= start (point))
          (error "%s has no public key!" pgg-default-user-id)))
    (save-excursion
      (goto-char (point-max))
      (insert "\n")
      (setq start (point))
      (vm-mime-attach-object buffer
                             "application/pgp-keys"
                             (list (concat "name=\"" pgg-default-user-id ".asc\""))
                             description
                             nil)
      ;; a crude hack to set the disposition
      (let ((disposition (list "attachment"
                               (concat "filename=\"" pgg-default-user-id ".asc\"")))
            (end (point)))
        (if (featurep 'xemacs)
            (set-extent-property (extent-at start nil 'vm-mime-disposition)
                                 'vm-mime-disposition disposition)
          (put-text-property start end 'vm-mime-disposition disposition))))))

(defun vm-pgg-make-multipart-boundary (word)
  "Create a mime part boundery starting with WORD and return it.

We cannot use `vm-mime-make-multipart-boundary' as it uses the current time as
seed and thus creates the same boundery when called twice in a short period."
  (if word (setq word (concat word "+")))
  (let ((boundary (concat word (make-string 15 ?a)))
	(i (length word)))
    (random)
    (while (< i (length boundary))
      (aset boundary i (aref vm-mime-base64-alphabet
			     (% (vm-abs (lsh (random) -8))
				(length vm-mime-base64-alphabet))))
      (vm-increment i))
    boundary))

(defun vm-pgg-save-work (function &rest args)
  "Call FUNCTION with ARGS without messing up the composition in case of an error."
  (let ((composition-buffer (current-buffer))
        (undo-list-backup buffer-undo-list)
        (work-buffer (get-buffer-create " *VM-PGG-WORK*")))
    (save-excursion
      (set-buffer work-buffer)
      (buffer-disable-undo)
      (erase-buffer)
      (insert-buffer-substring composition-buffer)
      (setq major-mode 'mail-mode)
      (apply function args))
    (vm-mail-mode-show-headers)
    (erase-buffer)
    (insert-buffer-substring work-buffer)
    (kill-buffer work-buffer)))

;;; ###autoload
(defun vm-pgg-sign ()
  "Sign the composition with PGP/MIME.

If the composition is not encoded so far, it is encoded before signing.
Signing of already encoded messages is discouraged.

RFC 2015 and its successor 3156 forbid the use of 8bit encoding for signed
messages, but require to use quoted-printable or base64 instead.  Also lines
starting with \"From \" cause trouble and should be quoted.

Thus signing of encoded messages may cause an error.  To avoid this you must
set `vm-mime-8bit-text-transfer-encoding' to something different than 8bit and
`vm-mime-composition-armor-from-lines' to t.

The transfer encoding done by `vm-pgg-sign' can be controlled by the variable
`vm-pgg-sign-text-transfer-encoding'."
  (interactive)

  (when (vm-mail-mode-get-header-contents "MIME-Version:")
    ;; do a simple sanity check ... too simple as we should walk the MIME part
    ;; hierarchy and only check the MIME headers ...
    (goto-char (point-min))
    (when (re-search-forward "Content-Transfer-Encoding:\\s-*8bit" nil t)
      (describe-function 'vm-pgg-sign)
      (error "Signing is broken for 8bit encoding!"))
    (goto-char (point-min))
    (when (re-search-forward "^From\\s-+" nil t)
      (describe-function 'vm-pgg-sign)
      (error "Signing is broken for lines starting with \"From \"!")))
  
  (vm-pgg-save-work 'vm-pgg-sign-internal))

(defun vm-pgg-sign-internal ()
  "Do the signing."
  ;; prepare composition
  (let ((vm-mime-8bit-text-transfer-encoding
         vm-pgg-sign-text-transfer-encoding)
        (vm-mime-composition-armor-from-lines t))
    (vm-pgp-prepare-composition))
  
  (let ((content-type (vm-mail-mode-get-header-contents "Content-Type:"))
        (encoding (vm-mail-mode-get-header-contents "Content-Transfer-Encoding:"))
        (boundary (vm-pgg-make-multipart-boundary "pgp+signed"))
        (pgg-text-mode t) ;; For GNU Emacs PGG
        (micalg "sha1")
        entry
        body-start)
    ;; fix the body
    (setq body-start (vm-marker (vm-pgp-goto-body-start)))
    (insert "Content-Type: " (or content-type "text/plain") "\n")
    (insert "Content-Transfer-Encoding: " (or encoding "7bit") "\n")
    (if (not (looking-at "\n"))
        (insert "\n"))
    ;; now create the signature
    (save-excursion
      ;; BUGME do we need the CRLF conversion?
;      (vm-pgg-make-crlf (point) (point-max))
      (unless (pgg-sign-region body-start (point-max) nil)
        (pop-to-buffer pgg-errors-buffer)
        (error "Signing error"))
      (and (setq entry (assq 2 (pgg-parse-armor
                                (with-current-buffer pgg-output-buffer
                                  (buffer-string)))))
           (setq entry (assq 'hash-algorithm (cdr entry)))
           (if (cdr entry)
               (setq micalg (downcase (format "%s" (cdr entry)))))))
    ;; insert mime part bounderies
    (goto-char body-start)
    (insert "This is an OpenPGP/MIME signed message (RFC 2440 and 3156)\n")
    (insert "--" boundary "\n")
    (goto-char (point-max))
    (insert "\n--" boundary "\n")
    ;; insert the signature
    (insert "Content-Type: application/pgp-signature\n\n")
    (goto-char (point-max))
    (insert-buffer-substring pgg-output-buffer)
    (insert "\n--" boundary "--\n")
    ;; fix the headers
    (vm-mail-mode-remove-header "MIME-Version:")
    (vm-mail-mode-remove-header "Content-Type:")
    (vm-mail-mode-remove-header "Content-Transfer-Encoding:")
    (mail-position-on-field "MIME-Version")
    (insert "1.0")
    (mail-position-on-field "Content-Type")
    (insert "multipart/signed; boundary=\"" boundary "\";\n"
            "\tmicalg=pgp-" micalg "; protocol=\"application/pgp-signature\"")))

;;; ###autoload
(defun vm-pgg-encrypt (&optional sign)
  "Encrypt the composition as PGP/MIME.  With a prefix arg SIGN also sign it."
  (interactive "P")
  (vm-pgg-save-work 'vm-pgg-encrypt-internal sign))

(defun vm-pgg-encrypt-internal (sign)
  "Do the encrypting, if SIGN is t also sign it."
  (unless (vm-mail-mode-get-header-contents "MIME-Version:")
    (vm-mime-encode-composition))
  (let ((content-type (vm-mail-mode-get-header-contents "Content-Type:"))
        (encoding (vm-mail-mode-get-header-contents "Content-Transfer-Encoding:"))
        (boundary (vm-pgg-make-multipart-boundary "pgp+encrypted"))
        (pgg-text-mode t) ;; For GNU Emacs PGG
        body-start)
    ;; fix the body
    (setq body-start (vm-marker (vm-pgp-goto-body-start)))
    (insert "Content-Type: " (or content-type "text/plain") "\n")
    (insert "Content-Transfer-Encoding: " (or encoding "7bit") "\n")
    (insert "\n")
    (goto-char (point-max))
    (insert "\n")
    (vm-pgg-cleartext-encrypt sign)
    (goto-char body-start)
    (insert "This is an OpenPGP/MIME encrypted message (RFC 2440 and 3156)\n")
    (insert "--" boundary "\n")
    (insert "Content-Type: application/pgp-encrypted\n\n")
    (insert "Version: 1\n\n")
    (insert "--" boundary "\n")
    (insert "Content-Type: application/octet-stream\n\n")
    (goto-char (point-max))
    (insert "\n--" boundary "--\n")
    ;; fix the headers
    (vm-mail-mode-remove-header "MIME-Version:")
    (vm-mail-mode-remove-header "Content-Type:")
    (vm-mail-mode-remove-header "Content-Transfer-Encoding:")
    (mail-position-on-field "MIME-Version")
    (insert "1.0")
    (mail-position-on-field "Content-Type")
    (insert "multipart/encrypted; boundary=\"" boundary "\";\n"
            "\tprotocol=\"application/pgp-encrypted\"")))

(defun vm-pgg-sign-and-encrypt ()
  "*Sign and encrypt the composition as PGP/MIME."
  (interactive)
  (vm-pgg-encrypt t))

(defvar vm-pgg-prompt-last-action nil
  "The action last taken in `vm-pgg-prompt-for-action'.")

(defvar vm-pgg-prompt-action-alist
  '((?s sign "Sign")
    (?e encrypt "encrypt") 
    (?E sign-and-encrypt "both")
    (?n nil "nothing")
    (?q quit "quit"))
  "Alist of (KEY ACTION LABEL) elements.")

(defun vm-pgg-prompt-for-action ()
  "Prompt for an action and return it. See also `vm-pgg-prompt-action-alist'."
  (interactive)
  (let (prompt event action)
    (setq prompt (mapconcat (lambda (a)
                              (format "%s (%c)" (nth 2 a) (car a)))
                            vm-pgg-prompt-action-alist ", ")
          action (mapcar (lambda (a)
                           (if (eq (nth 1 a)
                                   vm-pgg-prompt-last-action)
                               (downcase (nth 2 a))))
                         vm-pgg-prompt-action-alist)
          prompt (format "%s (default %s)?"
                         prompt
                         (car (delete nil action)))
          action nil)
    (while (not event)
      (setq event (read-key-sequence prompt))
      (if (featurep 'xemacs)
          (setq event (event-to-character (aref event 0)))
        (setq event (if (stringp event) (aref event 0))))
      (if (eq event ?\r)
          (setq action vm-pgg-prompt-last-action)
        (setq action (assoc event vm-pgg-prompt-action-alist))
        (if action
            (setq action (nth 1 action))
          (setq event nil))))
    (when (eq action 'quit)
      (error "Sending aborted!"))
    (if action
        (message "Action is %s." action)
      (message "No action selected."))
    (setq vm-pgg-prompt-last-action action)
    action))

;;; ###autoload
(defun vm-pgg-ask-hook ()
  "Hook to automatically ask for signing or encrypting outgoing messages with PGP/MIME.

Put this function into `vm-mail-send-hook' to be asked each time you
send a message whether or not you want to sign or encrypt the
message. See `vm-pgg-ask-function' to determine which function is
proposed.

This hook should probably be the last of your hooks if you have several
other functions there.  Signing crucially relies on the fact that the
message is not altered afterwards. To put it into `vm-mail-send-hook'
put something like

       (add-hook 'vm-mail-send-hook 'vm-pgg-ask-hook t)

into your VM init file."
  (interactive)

  ;; ensure we are the last hook
  (when (and (member 'vm-pgg-ask-hook vm-mail-send-hook)
             (cdr (member 'vm-pgg-ask-hook vm-mail-send-hook)))
    (describe-function 'vm-pgg-ask-hook)
    (error "`vm-pgg-ask-function' must be the last hook in `vm-mail-send-hook'!"))
  
  (let ((handler vm-pgg-ask-function)
        action)
    (when handler
      (setq action (if (fboundp handler)
                       (funcall handler)
                     (if (y-or-n-p (format "%s the composition? " handler))
                         handler)))
      (when action 
        (funcall (intern (format "vm-pgg-%s" action)))))))

(provide 'vm-pgg)

;;; vm-pgg.el ends here
