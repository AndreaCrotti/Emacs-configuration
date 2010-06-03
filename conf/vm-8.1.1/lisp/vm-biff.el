;;; vm-biff.el --- a xlbiff like tool for VM
;; 
;; Copyright (C) 2001 Robert Fenk
;;
;; Author:      Robert Fenk
;; Status:      Tested with XEmacs 21.4.15 & VM 7.18
;; Keywords:    VM helpers
;; X-URL:       http://www.robf.de/Hacking/elisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:
;;
;; Put this file into your load path and add the following line to your .vm
;; file
;;
;; (require 'vm-biff)
;;
;; Try: M-x customize-group vm-biff RET
;;
;; You should set `vm-auto-get-newmail', since otherwise this package 
;; does not make any sense!  If getting mail is slow, use fetchmail to
;; retrieve it to a local file and uses that file as VM spool file!
;; 


(eval-when-compile 
  (require 'cl))

(when vm-xemacs-p
  (require 'overlay))

(when vm-fsfemacs-p
  (defvar horizontal-scrollbar-visible-p nil))

(defgroup vm nil
  "VM"
  :group 'mail)

(defgroup vm-biff nil
  "The VM biff lib"
  :group 'vm)

(defcustom vm-biff-position 'center
  "*Position of the popup-frame."
  :group 'vm-biff
  :type '(choice (const :tag "center the popup frame" center)
                 (list  :tag "Position of the top-left corner."
                        :value (1 1)
                        (integer :tag "X")
                        (integer :tag "Y"))))


(defcustom vm-biff-width 120
  "*Width of the popup-frame."
  :group 'vm-biff
  :type 'integer)

(defcustom vm-biff-max-height 10
  "*Maximum hight of the popup window."
  :group 'vm-biff
  :type 'integer)

(defcustom vm-biff-body-peek 50
  "*Maximum number of chractes to peek into the body of a message."
  :group 'vm-biff
  :type 'integer)


(defcustom vm-biff-focus-popup nil
  "*t if popup window should get the focus after an update."
  :group 'vm-biff
  :type 'boolean)

(defcustom vm-biff-auto-remove nil
  "*Number of seconds after the popup window is automatically removed."
  :group 'vm-biff
  :type '(choice (integer :tag "Number of seconds" 10)
                 (const   :tag "Disable remove" nil)))

(defcustom vm-biff-summary-format nil
  "*Like `vm-summary-format' but for popup buffers."
  :group 'vm-biff
  :type '(choice (string :tag "Summary format")
                 (const  :tag "Disable own format" nil)))

(defcustom vm-biff-selector '(and (new)
                                  (not (deleted))
                                  (not (outgoing)))
  "*virtual folder selector matching messages to display in the pop-up."
  :group 'vm-biff
  :type 'sexp)

(defcustom vm-biff-place-frame-function 'vm-biff-place-frame
  "*Function that sets the popup frame position and size."
  :group 'vm-biff
  :type 'function)

(defcustom vm-biff-select-hook nil
  "*List of hook functions to be run when selection a message."
  :group 'vm-biff
  :type '(repeat (function)))

(defcustom vm-biff-select-frame-hook nil
  "*List of hook functions to be run when selection a message.
You may want to add `vm-biff-fvwm-focus-vm-folder-frame'.
"
  :group 'vm-biff
  :type '(repeat (function)))

(defcustom vm-biff-folder-list nil
  "*List of folders to generate a popup for.
The default is all spool files listed in `vm-spool-files'.
Testing is done by string-matching it against the current buffer-file-name.

Another form is an alist of elements (FODERNAME SELECTOR),
where SELECTOR is a virtual folder selector matching the
messges which should be displayed.  See `vm-biff-selector'
for an example and `vm-virtual-folder-alist' on how virtual
folder selectors work."
  :group 'vm-biff
  :type '(repeat (string)))

(defvar vm-biff-keymap nil
  "Keymap for vm-biff popup buffers.")

(when (not vm-biff-keymap)
  (setq vm-biff-keymap (make-sparse-keymap "VM Biff"))
  (define-key vm-biff-keymap "q" 'vm-biff-delete-popup)
  (define-key vm-biff-keymap " " 'vm-biff-delete-popup)
  (define-key vm-biff-keymap [(space)] 'vm-biff-delete-popup)
  (define-key vm-biff-keymap [(button1)] 'vm-biff-delete-popup)
  (define-key vm-biff-keymap [(mouse-1)] 'vm-biff-delete-popup)
  (define-key vm-biff-keymap [(return)] 'vm-biff-select-message)
  (define-key vm-biff-keymap [(button2)] 'vm-biff-select-message-mouse)
  (define-key vm-biff-keymap [(mouse-2)] 'vm-biff-select-message-mouse))

(defun vm-summary-function-V (msg)
  (let ((body-start (vm-text-of msg))
        (body-end (vm-end-of msg))
        peek)
    (if (< vm-biff-body-peek (- body-end body-start))
        (setq body-end (+ vm-biff-body-peek body-start)))
    (save-excursion
      (save-restriction
        (set-buffer (vm-buffer-of msg))
        (widen)
        (goto-char body-end)
        (re-search-forward "$" (point-max) t)
        (setq peek (vm-decode-mime-encoded-words-in-string
                    (buffer-substring body-start (point))))
        (let ((pos 0))
          (if (string-match "^\n+" peek pos)
              (setq peek (replace-match "" t t peek)))
          (while (setq pos (string-match "\n\n+" peek pos))
            (setq peek (replace-match "\n" t t peek)))
          (setq pos 0)
          (while (setq pos (string-match "\n" peek pos))
            (setq peek (replace-match "\n\t" t t peek)
                  pos (+ 2 pos))))
        (setq peek (concat "\t" peek))
        (put-text-property 0 (length peek) 'face 'bold peek)
        peek))))

(defun vm-biff-place-frame (&optional f)
  "Centers the frame and limits it to `vm-biff-max-height' lines."
  (let ((f (or f (selected-frame)))
        (height (1+ (count-lines (point-min) (point-max)))))
    (if (> height vm-biff-max-height)
        (setq height vm-biff-max-height))
    (set-frame-size f vm-biff-width height)

    (if (eq 'center vm-biff-position)
        (set-frame-position
         f
         (/ (- (x-display-pixel-width) (frame-pixel-width f)) 2)
         (/ (- (x-display-pixel-height) (frame-pixel-height f)) 2))
      (apply 'set-frame-position f vm-biff-position))))

(defconst vm-biff-frame-properties
  '(;; common properties
    (name . "New Mail")
    (unsplittable . t)
    (minibuffer . nil)
    (user-position . t)    
    (menubar-visible-p . nil)
    (default-toolbar-visible-p . nil)
;    (has-modeline-p . nil)
    (top . 1)
    (left . 1)
    ;; Xemacs properties
    (initially-unmapped . t)
    (modeline-shadow-thickness . 0)
    (vertical-scrollbar . nil)
    ;; GNU Emacs properties
    (vertical-scroll-bars . nil)
    (menu-bar-lines . 0)   
    (tool-bar-lines . 0)   
    (visibility . nil)
    )
  "Default properties for popup frame.")

(defvar vm-biff-message-pointer nil)
(defvar vm-biff-folder-buffer nil)
(defvar vm-biff-message-number nil)
(defvar vm-biff-folder-frame nil)
(defvar vm-biff--folder-window nil)

(defun vm-biff-x-p ()
  (if vm-xemacs-p
      (memq (console-type) '(x mswindows))
    t))

(defun vm-biff-get-buffer-window (buf)
  (if vm-xemacs-p
      (get-buffer-window buf (vm-biff-x-p) (frame-device))
    (get-buffer-window buf (vm-biff-x-p))))

(defun  vm-biff-find-folder-window (msg)
  (let ((buf (vm-buffer-of msg)))
    (save-excursion
      (set-buffer buf)
      (or (vm-biff-get-buffer-window buf)
          (and vm-presentation-buffer
               (vm-biff-get-buffer-window  vm-presentation-buffer))
          (and vm-summary-buffer
               (vm-biff-get-buffer-window vm-summary-buffer))))))

(defun  vm-biff-find-folder-frame (msg)
  (let ((ff (vm-biff-find-folder-window msg)))
    (if ff (window-frame ff))))

;;;###autoload
(defun vm-biff-select-message ()
  "Put focus on the folder frame and select the appropiate message."
  (interactive)
  (let* ((vm-biff-message-pointer
          (or (get-text-property (point) 'vm-message-pointer)
              vm-biff-message-pointer))
         (msg (car vm-biff-message-pointer))
         (vm-biff-message-number (vm-number-of msg))
         (vm-biff-folder-buffer (vm-buffer-of msg))
         (vm-biff-folder-window (vm-biff-find-folder-window msg))
         vm-biff-folder-frame)

    (if vm-biff-folder-window
        (setq vm-biff-folder-frame (window-frame vm-biff-folder-window)))

    (setq vm-biff-message-pointer nil)
    (vm-biff-delete-popup)
    
    (cond ((and vm-biff-folder-frame (vm-biff-x-p))
           (select-frame vm-biff-folder-frame)
           (focus-frame vm-biff-folder-frame)
           (raise-frame vm-biff-folder-frame)
           (run-hooks 'vm-biff-select-frame-hook)
           (select-window vm-biff-folder-window))
          (vm-biff-folder-window
           (select-window vm-biff-folder-window))
          (t 
           (bury-buffer)
           (switch-to-buffer vm-biff-folder-buffer)))

    (sit-for 0)
    
    (if vm-biff-message-number
        (vm-goto-message (string-to-number (vm-number-of msg))))
    
    (run-hooks 'vm-biff-select-hook)))

;;;###autoload
(defun vm-biff-select-message-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (vm-biff-select-message))

(defcustom vm-biff-FvwmCommand-path "/usr/bin/FvwmCommand"
  "Full qualified path to FvwmCommand."
  :group 'vm-biff
  :type 'file)

;;;###autoload
(defun vm-biff-fvwm-focus-vm-folder-frame ()
  "Jumps to the frame containing the folder for the selected message.

1) Your Emacs frame needs to have the folder name in its title, see the
   variable `frame-title-format' on how to set this up.

2) You need to define the FVWM2 function SelectWindow and start the
   FvwmCommandS module.  Therefore, you will need the following lines
   in your .fvwm2rc file. 

AddToFunc InitFunction
+ I Module FvwmCommandS

AddToFunc RestartFunction
+ I Module FvwmCommandS

AddToFunc SelectWindow
+ I Next ($0) Iconify false
+ I Next ($0) Raise
+ I Next ($0) WarpToWindow 10p 10p
"
  (interactive)
  (let ((p (start-process "FvwmCommand"
                          " *FvwmCommand*"
                          vm-biff-FvwmCommand-path
                          "-c")))
    (process-send-string p (concat "SelectWindow *"
                                   (buffer-name vm-biff-folder-buffer)
                                   "*\n"))
    (process-send-eof p)))
  
;;;###autoload
(defun vm-biff-delete-popup (&optional wf)
  (interactive)
  (if (vm-biff-x-p)
      (delete-frame wf)
    (if (not (one-window-p))
        (delete-window wf)))
  (sit-for 0))

(defun vm-biff-timer-delete-popup (wf)
  (if (featurep 'itimer)
      (delete-itimer current-itimer))
  (vm-biff-delete-popup wf))

(defvar vm-biff-message-pointer nil)
(make-variable-buffer-local 'vm-biff-message-pointer)

(defvar horizontal-scrollbar-visible-p)	; defined for XEmacs only

;;;###autoload
(defun vm-biff-popup (&optional force)
  "Scan the current VM folder for new messages and popup a summary frame."
  (interactive (list current-prefix-arg))

  (save-excursion
    (vm-select-folder-buffer)

    (when (not vm-biff-folder-list)
      (setq vm-biff-folder-list
            (if (stringp (car vm-spool-files))
                (list (expand-file-name
                       vm-primary-inbox
                       vm-folder-directory))
              (mapcar (lambda (f)
                        (expand-file-name
                         (car f)
                         vm-folder-directory))
                      vm-spool-files))))

    (let* ((mp vm-message-pointer)
           (folder (buffer-name))
           (do-mouse-track
            (and vm-mouse-track-summary
                 (vm-mouse-support-possible-p)))
           (buf (get-buffer-create
                 (concat " *new messages in VM folder: " folder "*")))
           selector msg new-messages wf)
      
      (let ((fl vm-biff-folder-list))
        (while fl
          (if (stringp (car fl))
              (if (string-match (car fl) (or (buffer-file-name)
                                             (buffer-name)))
                  (setq selector (list vm-biff-selector) fl nil))
            (if (string-match (caar fl) (or (buffer-file-name)
                                            (buffer-name)))
                (setq selector (cdar fl) fl nil)))
          (setq fl (cdr fl))))

      (when selector
        ;; collect the new messages 
        (set-buffer buf)
        (setq buffer-read-only nil)
        (erase-buffer)
      
        (let (start)
          (while mp
            (setq msg (car mp))
            (when (apply 'vm-vs-or msg selector)
              (setq start (point))
              (vm-tokenized-summary-insert msg
                                           (vm-summary-sprintf
                                            (or vm-biff-summary-format
                                                vm-summary-format)
                                            msg t))
              (put-text-property start (point) 'vm-message-pointer mp)

              (vm-summary-highlight-region start (point)
                                           vm-summary-highlight-face)

              (when do-mouse-track
                (vm-mouse-set-mouse-track-highlight
                 start (point)))
              
              (if (not new-messages) (setq new-messages mp)))
            (setq mp (cdr mp))))
    
        (when (and new-messages
                   (or force
                       (not (equal new-messages vm-biff-message-pointer))))
          (setq msg (car new-messages))
          (backward-delete-char 1)
          (goto-char (point-min))
	  
          (setq truncate-lines t
                buffer-read-only t)
          (use-local-map vm-biff-keymap)
          (setq vm-biff-message-pointer new-messages)
          
          ;; if in the minibuffer then seletc a different window
          (if (active-minibuffer-window)
              (other-window))
        
          ;; generate a own window/frame showing the messages
          (if (vm-biff-x-p)
              ;; X Window System or MS Windows
              (let* ((sf (selected-frame))
                     (ff (vm-biff-find-folder-frame msg))
                     (props (if ff
                                (cons (cons 'popup ff)
                                      vm-biff-frame-properties)
                              vm-biff-frame-properties))
                     (mf (or (and (if vm-xemacs-p
                                      (get-buffer-window buf t (frame-device))
                                    (get-buffer-window buf t))
                                  (window-frame
                                   (vm-biff-get-buffer-window buf)))
                             (make-frame props))))

                (select-frame mf)
                (switch-to-buffer buf)
                (if vm-xemacs-p
                    (set-specifier horizontal-scrollbar-visible-p nil))
            
                (if (functionp vm-biff-place-frame-function)
                    (funcall vm-biff-place-frame-function))
            
                (make-frame-visible mf)
                (setq wf mf)
              
                (if vm-biff-focus-popup (focus-frame mf)
                  (select-frame sf)))

            ;; Terminal
            (let ((w (vm-get-buffer-window buf))
                  (window-min-height 2)
                  (h (count-lines (point-min) (point-max))))
              (if w
                  (if vm-biff-focus-popup (select-window w))
                (setq wf (split-window (selected-window))))
              (sit-for 0)
              (switch-to-buffer buf)
              (if (> h vm-biff-max-height)
                  (setq h vm-biff-max-height))
              (setq h (- (window-displayed-height) h))
              (if (not (one-window-p))
                  (shrink-window h)))))

        (if vm-biff-auto-remove
            (cond
             	((condition-case nil
                     (progn (require 'itimer) t)
                   (error nil))
                 (start-itimer (buffer-name)
                               'vm-biff-timer-delete-popup
                               vm-biff-auto-remove
                               nil t t wf))
                ((condition-case nil
                     (progn (require 'timer) t)
                   (error nil))
                 (run-at-time vm-biff-auto-remove nil
                              'vm-biff-timer-delete-popup wf))))))))

(add-hook 'vm-arrived-messages-hook 'vm-biff-popup t)

(provide 'vm-biff)
