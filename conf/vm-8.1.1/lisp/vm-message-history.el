;;; vm-message-history.el --- Move backward & forward through selected messages
;; -*-unibyte: t; coding: iso-8859-1;-*-

;; Copyright © 2003 Kevin Rodgers, 2008 Robert Widhopf-Fenk

;; Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; Created: 6 Oct 2003
;; Keywords: mail, history

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; VM defines the `vm-goto-message-last-seen' command (bound to TAB) to
;; toggle between 2 messages, but doesn't provide a general history
;; mechanism.  This library allows the user to move backward and forward
;; through the messages that have already been selected in each folder.
;; It mimics a web browser in that selecting a message causes more
;; recently selected messages in the history list to be forgotten
;; (except when using `vm-goto-message-last-seen' or one of the
;; vm-message-history.el commands).

;;; Usage:
;;
;; Add the follwoing line to your ~/.vm
;;
;; (require 'vm-message-history)
;;
;; Visit a folder, move around and the use the key bindings or menu items for
;; the moving and browsing the history.
;; C-c p, Motion -> Backward in History
;; C-c n, Motion -> Forward in History
;; C-c b, Motion -> Browse History

;;; TODO: Handle Expunged messages in the history list?

;;; Code:

(eval-and-compile
  (require 'easymenu)
  (require 'vm-version)
  (require 'vm-menu)
  (require 'vm-vars))

(defgroup vm-message-history nil
  "Message history for VM folders."
  :group 'vm)

(defcustom vm-message-history-max 32
  "The number of read or previewed messages in each folder's history."
  :type 'integer
  :group 'vm-message-history)

(defvar vm-message-history nil
  "A list of messages in the current folder.")

(make-variable-buffer-local 'vm-message-history)

(defvar vm-message-history-pointer nil
  "The cons in `vm-message-history' whose car is the current message.")

(make-variable-buffer-local 'vm-message-history-pointer)

(define-key vm-mode-map "\C-cp" 'vm-message-history-backward)
(define-key vm-mode-map "\C-cn" 'vm-message-history-forward)
(define-key vm-mode-map "\C-cb" 'vm-message-history-browse)

(setq vm-menu-motion-menu
      (append vm-menu-motion-menu
	      '(["Backward in History" vm-message-history-backward t]
		["Forward in History" vm-message-history-forward t]
		["Browse History" vm-message-history-browse
		 :active (save-excursion
			   (vm-select-folder-buffer)
			   vm-message-history)])))

;;;###autoload
(defun vm-message-history-add ()
  "Add the selected message to `vm-message-history'.
\(Unless the message was selected via \\[vm-message-history-backward] or
\\[vm-message-history-forward].)"
  (when (not (memq this-command '(vm-goto-message-last-seen
                                  vm-message-history-backward
                                  vm-message-history-forward
                                  vm-message-history-browse-select)))
    ;; remove message if it was there already
    (when (memq (car vm-message-pointer) vm-message-history)
      (setq vm-message-history (delq (car vm-message-pointer)
                                     vm-message-history)
            vm-message-history-pointer vm-message-history))
    ;; add new message to head
    (setq vm-message-history-pointer
          ;; Discard messages selected after the current message:
          (setq vm-message-history
                (cons (car vm-message-pointer)
                      vm-message-history-pointer)))
    ;; Discard oldest messages:
    (setcdr (or (nthcdr (1- vm-message-history-max) vm-message-history)
                '(t))		; hack!
            nil)))

;;;###autoload
(defun vm-message-history-backward (&optional arg)
  "Select the previous message in the current folder's history.
With prefix ARG, select the ARG'th previous message."
  (interactive "p")
  (or arg (setq arg 1))
  (vm-select-folder-buffer)
  (or vm-message-history
      (error "No message history"))
  (cond ((> arg 0)
	 (setq vm-message-history-pointer
	       (or (nthcdr arg vm-message-history-pointer)
		   ;; wrap around to newest message:
		   vm-message-history)))
	((< arg 0)
	 (let ((pointer vm-message-history))
	   (while (and pointer
		       (not (eq (nthcdr (- arg) pointer)
				vm-message-history-pointer)))
	     (setq pointer (cdr pointer)))
	   (setq vm-message-history-pointer
		 (or pointer
		     ;; wrap around to oldest message:
		     (if (fboundp 'last)
			 (last vm-message-history) ; Emacs 21, or cl.el
		       (progn
			 (setq pointer vm-message-history)
			 (while (consp (cdr pointer))
			   (setq pointer (cdr pointer)))
			 pointer)))))))
  (if (eq (car vm-message-pointer) (car vm-message-history-pointer))
      (vm-preview-current-message)
    (vm-record-and-change-message-pointer
     vm-message-pointer
     (memq (car vm-message-history-pointer)
           vm-message-list))
    (vm-preview-current-message))
  (vm-message-history-browse))

;;;###autoload
(defun vm-message-history-forward (&optional arg)
  "Select the next message in the current folder's history.
With prefix ARG, select the ARG'th next message."
  (interactive "p")
  (vm-message-history-backward (- arg)))

(defvar vm-message-history-menu nil
  "A popup menu of messages, generated from `vm-message-history'.")

(defun vm-message-history-browse-select ()
  "Select the message below the cursor."
  (interactive)
  (let ((mp (get-text-property (point) 'vm-message-pointer)))
    (vm-select-folder-buffer)
    (vm-record-and-change-message-pointer vm-message-pointer mp)
    (vm-preview-current-message)
    (vm-display nil nil '(vm-goto-message-last-seen)
                '(vm-goto-message-last-seen))
    (vm-message-history-browse)))

(defvar vm-message-history-browse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'vm-message-history-browse-select)
    (define-key map "="  'vm-summarize)
    (define-key map "q"  'bury-buffer)
    (define-key map "p" 'vm-message-history-backward)
    (define-key map "n" 'vm-message-history-forward)
    map))
      
;;;###autoload
(defun vm-message-history-browse ()
  "Select a message from a popup menu of the current folder's history."
  (interactive)
  (vm-select-folder-buffer)
  (or vm-message-history
      (error "No message history"))
  (let ((history vm-message-history)
        (folder (current-buffer))
        (selected-message (car vm-message-pointer))
        (buf (get-buffer-create (concat (buffer-name) " Message History")))
        mp)
    ;; replace summary window if possible
    (let ((window (get-buffer-window vm-summary-buffer)))
      (if window (select-window window)))
    ;; or existing one
    (let ((window (get-buffer-window buf)))
      (if window (select-window window)))
    ;; now switch to new buffer and set it up
    (switch-to-buffer buf)
    (let ((buffer-read-only nil))
      (erase-buffer))
    (abbrev-mode 0)
    (auto-fill-mode 0)
    (vm-fsfemacs-nonmule-display-8bit-chars)
    (if (fboundp 'buffer-disable-undo)
        (buffer-disable-undo (current-buffer))
      ;; obfuscation to make the v19 compiler not whine
      ;; about obsolete functions.
      (let ((x 'buffer-flush-undo))
        (funcall x (current-buffer))))
    (setq vm-mail-buffer folder
          mode-name "VM Message History"
          major-mode 'vm-message-history-mode
          mode-line-format vm-mode-line-format
          buffer-read-only t
          truncate-lines t)
    (use-local-map vm-message-history-browse-mode-map)
    ;; fill in the entries for each item
    (let ((buffer-read-only nil)
          (selected (point-min))
          start)
      (while history
        (setq mp (car history) start (point))
        (if (not (eq mp selected-message))
            (insert vm-summary-no-=>)
          (setq selected (point))
          (insert vm-summary-=>))
        (vm-tokenized-summary-insert mp (vm-su-summary mp))
        (set-text-properties start (point)
                             (list 'vm-message-pointer history))
        (setq history (cdr history)))
      ;; jump to selected message or last.
      (goto-char selected))))

(add-hook 'vm-select-message-hook 'vm-message-history-add)

(provide 'vm-message-history)

;;; vm-message-history.el ends here
