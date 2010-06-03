;;; vm-serial.el --- automatic creation of personalized message bodies
;;                   and sending of personalized serial mails
;; 
;; Copyright (C) 2000-2005 Robert Widhopf-Fenk
;;
;; Author:      Robert Widhopf-Fenk
;; Status:      Tested with XEmacs 21.4.15 & VM 7.19
;; Keywords:    sending mail, default mail, multiple recipients, serial mails
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
;;
;;
;;; Commentary:
;; 
;; Are you lazy on the one hand, but you like salutations and greetings?
;; 
;;  YES?
;; 
;; If so you got the right package here!  The idea is similar to those of
;; autoinsert.el, tempo.el, template.el etc., but specialized for composing
;; mails with VM.
;; 
;; You may want to use the following into your .vm file after adding other
;; vm-mail-mode-hooks ...
;; 
;;   (require 'vm-serial)
;;   (add-hook 'vm-mail-mode-hook 'vm-serial-auto-yank-mail t)
;;   (define-key vm-mail-mode-map "\C-c\C-t" 'vm-serial-expand-tokens)
;; 
;; and check out what happens if you reply to a message or what happens after
;; specifying a recipient in the to header and typing [C-c C-t].
;;
;; Isn't it cool?
;;
;; Now add multiple recipients to a mail before pressing [C-c C-t] and call
;; [M-x vm-serial-send-mail] in order to see what happens.  If you are a
;; trustful guy you may add a prefix arg [C-u].
;;
;; In order to learn more about valid tokens you should have a look at the
;; documentation mail template.
;;
;; Go to an newly mail buffer add a From and To header and type:
;;    C-u M-x vm-serial-yank-mail RET doc RET
;;    M-x vm-serial-expand-tokens RET
;;
;;; KNOWN PROBLEMS:
;;
;; - mail-signature: instead of using this variable, you should use
;;   `vm-serial-mail-signature' with exaclty the same semantics.
;;
;;; Thanks:
;;
;; Ivan Kanis has contributed some bugfixes & enhancements.
;; 
;;; Code:

(defgroup vm nil
  "VM"
  :group 'mail)

(defgroup vm-serial nil
  "Sending personalized serial mails and getting message templates."
  :group  'vm)

(eval-when-compile
  (require 'cl))

(require 'vm-reply)

(eval-and-compile
  (require 'vm-pine)
  (require 'mail-utils)
  (require 'mail-extr)
  (require 'advice))

(let ((feature-list '(bbdb bbdb-sc)))
  (while feature-list
    (condition-case nil
        (require (car feature-list))
      (error
       (if (load (format "%s" (car feature-list)) t)
           (message "Library %s loaded!" (car feature-list))
         (message "Could not load feature %S.  Related functions may not work correctly!" (car feature-list)))))
    (setq feature-list (cdr feature-list))))

(defvar vm-reply-list nil)
(defvar vm-redistribute-list nil)
(defvar vm-forward-list)

;;-----------------------------------------------------------------------------
(defcustom vm-serial-token-alist
  '(;; standard tokens you should not change (or need not)
    ("to"       (vm-serial-get-to)
     "to header of the mail")
    
    ("sir"      (vm-serial-get-name 'last)
     "the last name of the recipient")
    ("you"      (vm-serial-get-name 'first)
     "the first name of the recipient")
    ("mr"       (vm-serial-get-name)
     "the full name of the recipient")

    ("bbdbsir"  (vm-serial-get-bbdb-name 'last)
     "the last name of the recipient as returned by the BBDB")
    ("bbdbyou"  (vm-serial-get-bbdb-name 'first)
     "the first name of the recipient as returned by the BBDB")
    ("bbdbmr"   (vm-serial-get-bbdb-name)
     "the full name of the recipient as returned by the BBDB")

    ("me"       (user-full-name)
     "your full name")
    ("i"        (vm-serial-get-name 'first (user-full-name))
     "your first name")
    ("I"        (vm-serial-get-name 'last (user-full-name))
     "your last name")
    ("point"    (and (setq vm-serial-point (point)) nil)
     "the position of point after expanding tokens")
    ("reply"    (if (and vm-reply-list vm-serial-body-contents)
                    (insert vm-serial-body-contents))
     "set to the message body when replying")
    ("forward"  (if (and vm-forward-list vm-serial-body-contents)
                    (insert vm-serial-body-contents))
     "set to the message body when forwarding")
    ("body"     (if vm-serial-body-contents
                    (insert vm-serial-body-contents))
     "set to the message body before yanking a mail template")
    ("sig"      (cond
                 ((not vm-serial-mail-signature)
                  nil)
                 ((stringp vm-serial-mail-signature)
                  vm-serial-mail-signature)
                 ((eq t vm-serial-mail-signature)
                  (insert-file mail-signature-file))
                 ((functionp vm-serial-mail-signature)
                  (funcall vm-serial-mail-signature))
                 (t
                  (eval vm-serial-mail-signature)))
     "the signature obtained from `vm-serial-mail-signature'")
    ("fifosig"   (concat "-- \n"
                         (shell-command-to-string
                          (concat "cat " mail-signature-file)))
     "a signature read from a FIFO")
    ;; english
    ("hi"       ("Hi" "Hello" "Dear")
     "a randomly selected hi-style salutation")
    ("dear"     ("Lovely" "Hello" "Dear" "Sweetheart")
     "a randomly selected dear-style salutation")
    ("bye"      ("" "Bye " "Cheers " "CU ")
     "a randomly selected bye-style greeting")
    ("br"       ("Best regards" "Sincerly" "Yours")
     "a randomly selected best-regards-style greeting")
    ("babe"     ("honey" "sugar pie" "darling" "babe")
     "a randomly selected honey-style salutation")
    ("inlove"   ("In love" "Dreaming of you" "1 billion kisses")
     "a randomly selected inlove-style greeting")
    ("your"     ("honey" "sugar pie" "darling" "babe"
                 (vm-serial-get-name 'first (user-full-name)))
     "a randomly selected your-style greeting")
    ;; german
    ("hallo"    ("Hi" "Griass di" "Servus" "Hallo")
     "ein Hallo-Gruß")
    ("mausl"    ("Mausl" "Liebling" "Schatzi" "Hallo")
     "die Freundin")
    ("ciao"     ("" "Ciao " "Tschüß " "Servus " "Mach's gut " "Bis denn "
                 "Bis die Tage mal ")
     "Verabschiedung")
    ("sg"      ("Sehr geehrte Frau/Herr")
     "förmliche Anrede")
    ("mfg"     ("Mit freundlichen Grüßen")
     "förmliche Verabschiedung")
    ;; french
    ("salut" ("Salut" "Bonjour")
     "Une salutation au hasard")
    ("merci" ("Merci" "Au revoir" "A+" "Amicalement")
     "Un au revoir au hasard")
    )
  "*Alist for mapping tokens to real things, i.e., strings.
Set this by calling `vm-serial-set-tokens'!

The format of each record is:

        (TOKENNAME SEXPRESSION DOCUMENTATION)

TOKENNAME and DOCUMENTATION have to be strings.
SEXPRESSION one of
- a list starting with a string, which might be followed by other
  string, functions or Lisp expressions
- a function returning a string
- a Lisp expression which evaluates to a string

When a list starting with a string then `vm-serial-expand-tokens' will
randomly select one of them during expansion."
  :group 'vm-serial
  :type '(repeat (list (string :tag "Tagname")
                       (choice (repeat :tag "List of strings" (string))
                               (sexp :tag "SExp evaluating to a string"))
                       (string :tag "Documentation"))))

(defcustom vm-serial-mails-alist
  '(("honey"
     "girlfriend"
     "$dear $babe,

$point$reply

$inlove $your
$forward")
    ("german-reply"
     (and vm-reply-list
          (string-match "\\.\\(de\\|at\\|ch\\)>?$"
                        (vm-mail-mode-get-header-contents "To:")))
          "$reply
$point
$ciao$i")
    ("german-default"
     "\\.\\(de\\|at\\|ch\\)>?$"
     "$hallo $you,

$point$reply

$ciao$i

$forward
$sig")
    ("german-serious"
     "\\.\\(de\\|at\\|ch\\)>?$"
     "$sg $sir,

$point$reply

$mfg
$me

$forward
$sig")
    ("english-reply"
     vm-reply-list
     "$reply
$point
$bye$i")
    ("english-default"
     t
     "$hi $you,

$point$reply

$bye$i

$forward
$sig
")
    ;; A test mail for showing what's possible
    ("doc"
     nil
     "
                            A LECTURE ON VM-SERIAL

The `vm-serial-mails-alist' contains a list of templates and associated
conditions and names for these templates.

When doing a `vm-serial-yank-mail' it will check for the first condition
which matches and inserts this template.  Tokens in the template are
expanded by the function called `vm-serial-expand-tokens'.

There are default tokens for various things.  Tokens start with the
string specified in `vm-serial-cookie' which is \"$(eval vm-serial-cookie)\" followed by a
string matching the regexp \\([a-zA-Z][a-zA-Z0-9_-]*\\) which may be
enclosed by {} or a lisp expressions.  The first type is a named token
and has to be listed in the variable `vm-serial-token-alist'.  It will be
expanded and if evaluating to a non nil object then it is inserted.  In
order to get just the `vm-serial-cookie' \"$(eval vm-serial-cookie)\" simply write it twice.

You may also embed any kind of lisp expression.  If they return a string, it
will be inserted.

Do [M-x vm-serial-expand-tokens] in order to see how things change ...

Example of a embedded lisp expression:

 the current date is $$(format-time-string \"%D %r\").

 $$(center-line) Center this line

 $$$no expansion
  
The following tokens are currently defined:

Token   Documentation  (the example follows in the next line)
$(mapconcat
  (function (lambda (tk)
      (concat (car tk) \"\\t\" (caddr tk) \"\n\t$\" (car tk))))
  vm-serial-token-alist  \"\n\")


If you thing there are other tokens which should be added to this list, please
let me know!

mailto:Robert Fenk"))
  "*Alist of default mail templates.
Set this by calling `vm-serial-set-mail'!

Format:
   ((SYMBOLIC-NAME CONDITION MAIL-FORM)
    ...)
    
When calling `vm-serial-yank-mail' interactively one will be prompted for
a SYMBOLIC-NAME of a mail from.  If called non interactively it will
search for the first condition which evaluates to true and inserts the
corresponding mail.  If CONDITION is a string it is matched against the
To-header otherwise it is evaluated."
  :group 'vm-serial
  :type '(repeat (list (string :tag "Name")
                       (choice :tag "Condition"
                               (const :tag "NEVER" nil)
                               (const :tag "ALWAYS" t)
                               (string :tag "Regexp" "emailaddress")
                               (variable-item :tag "Relpy" vm-reply-list)
                               (variable-item :tag "Forward" vm-forward-list)
                               (variable-item :tag "Redistribute" vm-redistribute-list)
                               (sexp :tag "SEXP"))
                       (string :tag "Message-Template"))))

(defcustom vm-serial-cookie "$"
  "*The string which begins a token or Lisp expression.
See `vm-serial-expand-tokens' for information about valid tokens."
  :group 'vm-serial
  :type 'string)

(defcustom vm-serial-fcc  nil
  "*Whether to keep a FCC from the source mail within each serial mail.
If the function `vm-postpone-message' (from vm-pine) is present it will
also save the source message in the specified folder otherwise there is
no way to save the source message."
  :group 'vm-serial
  :type 'boolean)

(defcustom vm-serial-mail-signature nil
  "*Text inserted at the `sig'-token of a mail buffer.
The semantics are equal to those of variable `mail-signature', however you
should disable variable `mail-signature', since it interacts badly with
vm-serial, i.e. set vm-serial-mail-signature to the value of variable
`mail-signature' and set variable `mail-signature' to nil!"
  :group 'vm-serial
  :type '(choice (const :tag "None" nil)
                 (const :tag "The content of `mail-signature-file'" t)
                 (function-item :tag "Function")
                 (sexp :tag "Lisp-Form")))

(defvar vm-serial-to  nil
  "The recipient of the currently expanded message.")

(defvar vm-serial-body-contents nil
  "The message body of the currently replied or forwarded message.")

(defcustom vm-serial-unknown-to  "unknown"
  "*The string displayed for recipients without a real name.
If set to something different than a string it will be evaluated in order to
return a string."
  :group 'vm-serial
  :type 'string)

(defvar vm-serial-source-buffer
  nil
  "The source buffer of the currently expanded template.
When doing a `vm-serial-send-mail' this will point to the source
buffer containing the original message.")

(defvar vm-serial-send-mail-buffer "*vm-serial-mail*"
  "*Name of the buffer use by `vm-serial-send-mail' for expanded template.")

(defvar vm-serial-send-mail-jobs
  nil
  "Remaining list of addresses which have to be processed after editing.")

(make-variable-buffer-local 'vm-serial-source-buffer)
(make-variable-buffer-local 'vm-serial-send-mail-jobs)

;;-----------------------------------------------------------------------------
(defun vm-serial-get-completing-list (alist)
  "Return cars from ALIST for completion."
  (mapcar (lambda (e) (list (car e))) alist))

;;-----------------------------------------------------------------------------
(defvar vm-serial-token-history nil)

(defun vm-serial-set-token (&optional token newvalue doc)
  "Set vm-serial TOKEN to NEWVALUE with DOC.
You may remove a token by specifying just the TOKEN as argument."
  (interactive
   (let* ((token (completing-read "Token: "
                                (vm-serial-get-completing-list
                                 vm-serial-token-alist)
                                nil nil nil
                                vm-serial-token-history))
          (value (read-expression
                  "Value: "
                  (format "%S" (cdr (assoc var vm-serial-token-alist))))))
     (list token value)))
  (let ((tk (assoc token vm-serial-token-alist)))
    (if tk
        (if newvalue
            (setcdr tk (list newvalue doc))
          (setq vm-serial-token-alist (delete tk vm-serial-token-alist)))
      (setq vm-serial-token-alist
            (nconc vm-serial-token-alist
                   (list (list token newvalue doc)))))))

(defun vm-serial-set-tokens (token-list)
  "Set `vm-serial-token-alist' according to TOKEN-LIST.
Is a list of (TOKEN NEWVALUE DOC) elements"
  (let (token-value)
  (while token-list
    (setq token-value (car token-list))
    (vm-serial-set-token (car token-value) (cadr token-value)
                         (caddr token-value))
    (setq token-list (cdr token-list)))))

(defun vm-serial-get-token (&optional token)
  "Return value of vm-serial TOKEN."
  (interactive (list (completing-read "Token: "
                                      (vm-serial-get-completing-list
                                       vm-serial-token-alist)
                                      nil nil nil
                                      vm-serial-token-history)))
  (let ((value (assoc token vm-serial-token-alist)))
    (if value
        (cadr value)
      (warn "There is no vm-serial token `%s'" token)
      nil)))

(defun vm-serial-eval-token-value (&optional token-value)
  "Return string value by evaluation TOKEN-VALUE."
  (if (stringp token-value)
      token-value
    (condition-case err
      (cond ((and (listp token-value) (stringp (car token-value)))
             (setq token-value (vm-serial-random-string token-value)))
            ((functionp token-value)
             (setq token-value (funcall  token-value)))
          (t
           (setq token-value (eval token-value))))
      (error (setq token-value nil)
             (warn (format "Token `%s' caused a %S"
                           token-value err))
             nil))
    token-value))

;;-----------------------------------------------------------------------------
(defun vm-serial-get-emails (&optional header)
  "Return the recipient of current message.
Optional argument HEADER is the header to get the recipients from."
  (setq header (or header "To:"))
  (let ((to (vm-mail-mode-get-header-contents header)))
    (if (functionp 'bbdb-extract-address-components)
        (car (bbdb-extract-address-components to))
      (mail-extract-address-components to))))

(defun vm-serial-get-to ()
  "Return the recipient of current message."
  (or vm-serial-to
      (vm-serial-get-emails "To:")))

(defun vm-serial-get-name (&optional part name)
  (let ((name (or name
                  (and vm-serial-to (car vm-serial-to))
                  (let ((to (vm-serial-get-to)))
                    (and to (or (car to)
                                (cadr to))))
                  (eval vm-serial-unknown-to)))
        (part (cond ((stringp part) part)
                    ((equal part 'first) "^\\(\\w+\\)[\t ._]")
                    ((equal part 'last) "^\\w+[\t ._]+\\(.+\\)$"))))
    
    (if (and part (string-match part name))
        (match-string 1 name)
      name)))

(defun vm-serial-get-bbdb-name (&optional part name)
  (let* ((to (vm-serial-get-to))
         (rec (bbdb-search-simple nil (cadr to))))
    (if rec
        (cond ((equal part 'first) (or (bbdb/sc-consult-attr (cadr to))
                                       (bbdb-record-firstname rec)))
              ((equal part 'last)  (bbdb-record-lastname rec)))
      (vm-serial-get-name part name))))

;;-----------------------------------------------------------------------------
(defun vm-serial-set-mails (mail-alist)
  "Set `vm-serial-mails-alist' according to MAIL-ALIST."
  (let (m)
    (setq mail-alist (reverse mail-alist))
    (while mail-alist
      (setq m (assoc (caar mail-alist) vm-serial-mails-alist))
      (if m
          (setq vm-serial-mails-alist (delete m vm-serial-mails-alist)))
      (add-to-list 'vm-serial-mails-alist (car mail-alist))
      (setq mail-alist (cdr mail-alist)))))

(defun vm-serial-get-mail (&optional mail)
  "Return the mail body associated with MAIL."
  (let ((value (assoc mail vm-serial-mails-alist)))
    (if value (car (last value)) nil)))

(defvar vm-serial-mail-history nil
  "History for `vm-serial-yank-mail'.")

(defun vm-serial-find-default-mail ()
  "Return the first recipient."
  (let ((to (vm-decode-mime-encoded-words-in-string
             (or (vm-mail-mode-get-header-contents "To:")
                 (vm-mail-mode-get-header-contents "CC:")
                 (vm-mail-mode-get-header-contents "BCC:")
                 "")))
        (mails-alist vm-serial-mails-alist)
        m mail)
    (setq mail nil)
    (if (string-match "^\\s-*\\(.*[^ \t]\\)\\s-*$" to)
        (setq to (match-string 1 to)))
    (while mails-alist
      (setq m (car mails-alist))
      (if (and (> (length m) 2)
               (cond ((stringp (cadr m))
                      (let ((case-fold-search t))
                        (string-match (cadr m) to)))
                     ((functionp (cadr m))
                      (funcall (cadr m)))
                     ((equal (cadr m) t))
                     (t
                      (eval (cadr m)))))
          (setq mail (car m)
                mails-alist nil))
      (setq mails-alist (cdr mails-alist)))
    mail))

(defun vm-serial-auto-yank-mail (&optional mail no-expand)
  "Yank the mail associated with MAIL.
If MAIL is nil search for a default mail, i.e. the first which evaluates its
condition to true.  When called with a prefix argument or if NO-EXPAND is non
nil no tokens will be expanded after yanking.

This is like `vm-serial-yank-mail', but it ensures to yank only if the buffer
is no serial mail buffer and if there was no yank-mail before!"
  (if (and (not vm-serial-source-buffer)
           (not vm-redistribute-list)
           (not (local-variable-p 'vm-serial-body-contents (current-buffer)))
           (boundp 'vm-postponed-message-folder-buffer)
           (not vm-postponed-message-folder-buffer))
      (vm-serial-yank-mail (or mail (vm-serial-find-default-mail))
                           no-expand)))

(defvar vm-serial-yank-mail-choice nil)
(make-variable-buffer-local 'vm-serial-yank-mail-choice)

(defun vm-serial-yank-mail (&optional mail no-expand)
  "Yank the template associated with MAIL.

If MAIL is nil search for a default template, i.e. the first one which
evaluates its condition to true.  When called with a prefix argument ask for
a template and with another prefix argument or if NO-EXPAND is non nil
no tokens will be expanded after yanking.

You may bind this to [C-c C-t] in mail-mode in order to automatically yank
the right mail into the composition buffer and move the cursor to the
editing point.

I try to be clever when to delete the existing buffer contents and when to
expand the tokens, however if this does not satisfy you please report it to
me."

  (interactive "p")
  
  (if (numberp mail)
      (if (= mail 1)
          (setq mail nil)
        (setq no-expand (if (= mail 16) '(t))
              mail (completing-read
                    "Mail: "
                    (vm-serial-get-completing-list
                     vm-serial-mails-alist)
                    nil
                    t;; exact match
                    (cons (vm-serial-find-default-mail)
                          0)
                    vm-serial-mail-history)
              vm-serial-yank-mail-choice mail)))

  (setq mail (or mail vm-serial-yank-mail-choice (vm-serial-find-default-mail)))

  (let ((save-point (point)))
    (if (not mail)
        (message "There is no matching mail form!")
      (if (local-variable-p 'vm-serial-body-contents (current-buffer))
          (progn (delete-region (mail-text) (point-max))
                 (setq no-expand (if (and no-expand (listp no-expand))
                                     no-expand 'not))))
      
      (if (or (interactive-p)
              (local-variable-p 'vm-serial-body-contents (current-buffer)))
          (message "Inserting serial mail `%S'." mail)
        (let ((start (mail-text)) (end (goto-char (point-max))))
          (make-local-variable 'vm-serial-body-contents)
          (make-local-variable 'vm-serial-to)
          (setq vm-serial-to nil
                vm-serial-body-contents nil)
          (if (not (or vm-reply-list vm-forward-list))
              (setq no-expand (if (equal no-expand 'not) nil
                                (if (and no-expand (listp no-expand))
                                    no-expand t)))
            (setq vm-serial-body-contents (buffer-substring start end))
            (delete-region start end))))

      (let ((value (vm-serial-get-mail mail)))
      (save-excursion
        (insert value)))

      (if (or (and (not vm-forward-list) (not no-expand))
              (equal no-expand 'not))
          (vm-serial-expand-tokens)
        (goto-char save-point)))))

;;-----------------------------------------------------------------------------
(defun vm-serial-random-string (string-list)
  "Randomly return one of the strings in STRING-LIST."
  (let ((value (nth (mod (random) (length string-list)) string-list)))
    (cond ((stringp value)
           value)
          ((functionp value)
           (funcall value))
          (t
           (eval value)))))

(defun vm-serial-expand-tokens (&optional rstart rend)
  "Expand all tokens within the current mail.
This means we search for the `vm-serial-cookie' and if it is followed by a
regexp of \"[a-zA-Z][a-zA-Z0-9_-]\" we treat this as a symbol to look up in
our `vm-serial-token-alist'.  Optionally one may enclose the symbol by curly
parenthesis.  See the test mail in `vm-serial-mails-alist' for examples.
If the cookie is followed by a parenthesis then it is treated as a lisp
expression which is evaluated

Results evaluating to a string are inserted all other return values are
ignored.  For non existing tokens or errors during evaluation one will get
a warning."
  (interactive)
  
  (let ((token-regexp (concat (regexp-quote vm-serial-cookie)
                       "\\(" (regexp-quote vm-serial-cookie) "\\)*"
                       "[{\(a-zA-Z]"))
        start end expr result vm-serial-point)
    (if (and vm-xemacs-p
             (region-exists-p)
             (eq (zmacs-region-buffer) (current-buffer)))
        (setq rstart (goto-char (region-beginning)) rend (region-end))
      (setq rstart (mail-text) rend (point-max)))

    (narrow-to-region rstart rend)
    (while (re-search-forward token-regexp (point-max) t)
      (backward-char 1)
      (setq start (- (match-end 0) 1)
            result nil)
      (cond ((> (length (match-string 1)) 0)
             (delete-region (match-beginning 1) (match-end 1)))
            ((looking-at "(")
             (setq end (scan-sexps start 1))
             (goto-char start)
             (setq expr (read (current-buffer)))
             (delete-region (- start 1) end)
             (setq result (vm-serial-eval-token-value expr)))
            ((looking-at "\\({\\)?\\([a-zA-Z][a-zA-Z0-9_-]*\\)\\(}\\)?")
             (setq start (match-beginning 2))
             (setq end (match-end 2))
             (setq expr (buffer-substring start end))
             (if (and (not (and (match-end 1) (match-end 3)))
                      (or (match-end 1) (match-end 3)))
                 (error "Invalid token expression `%s'"
                        (match-string 0)))
             (delete-region (- (match-beginning 0) 1) (match-end 0))
             (setq result (vm-serial-eval-token-value
                           (vm-serial-get-token expr))))
            )
      (if (and result (stringp result))
          (insert (format "%s" result))))
    (widen)
    (if vm-serial-point
        (goto-char vm-serial-point))))

(defvar vm-serial-insert-token-history nil)

(defun vm-serial-insert-token (token)
  "Reads a valid token, inserts it at point and expands it."
  (interactive (list
                (completing-read
                 (format "Token%s: "
                         (if vm-serial-insert-token-history
                             (concat " (default: "
                                     (car vm-serial-insert-token-history)
                                     ")")
                           ""))
                 (mapcar (lambda (tok) (list (car tok)))
                         vm-serial-token-alist)
                 nil
                 t
                 nil
                 'vm-serial-insert-token-history)))
  (setq vm-serial-insert-token-history
        (delete "" vm-serial-insert-token-history))
  (if (string= "" token)
      (setq token (car vm-serial-insert-token-history)))
  (if (null token)
      (error "Error: you have to enter a toke name!"))
  (let ((start (point)))
    (insert vm-serial-cookie token)
    (vm-serial-expand-tokens start (point))))

;;-----------------------------------------------------------------------------
(defvar vm-serial-sent-cnt nil)
(defvar vm-serial-edited-cnt nil)
(defvar vm-serial-killed-cnt nil)
(defvar vm-serial-send-mail-exit nil)

(defun vm-serial-send-mail-increment (variable)
  (save-excursion
    (set-buffer vm-serial-source-buffer)
    (eval (list 'vm-increment variable))))


(defun vm-serial-send-mail-and-exit (&optional non-interactive)
  "Like `vm-serial-send-mail' but kills the buffer after sending all."
  (interactive "P")
  (make-local-variable 'vm-serial-send-mail-exit)
  (setq vm-serial-send-mail-exit t)
  (vm-serial-send-mail non-interactive))

(defun vm-serial-send-mail (&optional non-interactive done)
  "Send an expanded mail to each recipient listed in the To-header.
This will create a new buffer for expanding the tokens and user interaction.
You may send each mail interactively, that means you may send the message as
it is, or you may edit it before sending or you may skip it.

If called with a prefix argument or NON-INTERACTIVE set to non nil, no
questions will bother you!"
  (interactive "P")

  (remove-hook 'kill-buffer-hook 'vm-serial-send-mail t)

  (if vm-serial-source-buffer
      (progn (set-buffer vm-serial-source-buffer)
             (setq done t)))

  (if (get-buffer vm-serial-send-mail-buffer)
      (save-excursion
        (kill-buffer (get-buffer vm-serial-send-mail-buffer))))
  
  (let* ((work-buffer
          (save-excursion
            (let ((vm-frame-per-composition nil))
              (flet ((vm-display (buffer display commands configs
                                         &optional do-not-raise)
                                 nil))
                (vm-mail-internal vm-serial-send-mail-buffer))
              (get-buffer vm-serial-send-mail-buffer))))
         (source-buffer (current-buffer))
         work to to-string)

    (if (and (not vm-serial-send-mail-jobs) (not done))
        (if (not (setq to (mail-fetch-field "To" nil t)))
            (error "There are no recipients in %s!" (buffer-name))
          (setq vm-serial-send-mail-jobs
                (if (functionp 'bbdb-extract-address-components)
                    (bbdb-extract-address-components to)
                  (mapcar 'mail-extract-address-components
                          (bbdb-split to ","))))
          (make-local-variable 'vm-serial-sent-cnt)
          (make-local-variable 'vm-serial-edited-cnt)
          (make-local-variable 'vm-serial-killed-cnt)
          (setq vm-serial-sent-cnt 0
                vm-serial-edited-cnt 0
                vm-serial-killed-cnt 0)))

    ;; mail-extract-address-components isn't good at all! Fix it!
    (save-excursion
      (set-buffer work-buffer)
      (setq major-mode 'mail-mode))
    
    (while (and (not work) vm-serial-send-mail-jobs)
      (setq to (car vm-serial-send-mail-jobs)
            to-string (if (car to)
                          (concat (car to) " <" (cadr to) ">")
                        (cadr to)))
      (copy-to-buffer work-buffer (point-min) (point-max))
      (save-excursion
        (set-buffer work-buffer)
        (goto-char (point-min))
        (vm-mail-mode-remove-header "To:")
        (mail-position-on-field "To")
        (insert to-string)
        (if (not vm-serial-fcc)
            (vm-mail-mode-remove-header "FCC:"))
        (setq vm-serial-to to
              vm-serial-source-buffer source-buffer)
        (setq buffer-undo-list t)
        (vm-serial-expand-tokens)
        
        (if (not non-interactive)
            (let (command)
              (switch-to-buffer work-buffer)
              (while (not command)
                (message "(q)uit session or (e)dit, (s)end or (k)ill this mail to `%s'?"
                         to)
                (setq command (read-char-exclusive))
                (cond ((= command ?e)
                       (vm-serial-send-mail-increment 'vm-serial-edited-cnt)
                       (setq work 'edit))
                      ((= command ?s)
                       (vm-serial-send-mail-increment 'vm-serial-sent-cnt)
                       (vm-mail-send))
                      ((= command ?k)
                       (vm-serial-send-mail-increment 'vm-serial-killed-cnt))
                      ((= command ?q)
                       (setq work 'quit))
                      (t (message "Invalid command!")
                         (sit-for 1)
                         (setq command nil)))))
          (vm-mail-send)
          (vm-serial-send-mail-increment 'vm-serial-sent-cnt)))
    
      (setq vm-serial-send-mail-jobs (cdr vm-serial-send-mail-jobs)))

    ;; ok there was an exit or the like
    (if (equal work 'edit)
        (progn ;; and we want to edit the outgoing mail before sending
          (switch-to-buffer work-buffer)
          (run-hooks 'vm-mail-hook)
          (run-hooks 'vm-mail-mode-hook)
          (setq buffer-undo-list nil)
          (make-local-hook 'kill-buffer-hook)
          (add-hook 'kill-buffer-hook
                    '(lambda ()
                       (vm-serial-send-mail-increment 'vm-serial-killed-cnt))
                    t t)
          (add-hook 'kill-buffer-hook 'vm-serial-send-mail t t)
          (make-local-hook 'mail-send-hook)
          (add-hook 'mail-send-hook
                    '(lambda ()
                       (vm-serial-send-mail-increment 'vm-serial-sent-cnt))
                    t t)
          (remove-hook 'kill-buffer-hook 'vm-save-killed-message-hook t)
          (message "Kill or send this mail to get to the next mail!"))

      ;; get rid of the work buffer and go back to the source
      (kill-buffer work-buffer)
      (switch-to-buffer source-buffer)

      (if (not (equal work 'quit))
          (let ((fcc (vm-mail-mode-get-header-contents "FCC:")))
            ;; some statistics
            (message "%s mail%s sent, %s edited and %s killed by vm-serial!"
                     (if (= vm-serial-sent-cnt 1) "One" vm-serial-sent-cnt)
                     (if (= vm-serial-sent-cnt 1) "" "s")
                     vm-serial-edited-cnt vm-serial-killed-cnt)

            ;; this was the last mail so is there some FCC work to do?
            (if (and fcc (not vm-serial-send-mail-jobs))
                (if (not (functionp 'vm-postpone-message))
                    (error "vm-pine.el is needed to save source messages!")
                  ;; no postponed header for this!!
                  (vm-mail-mode-remove-header "FCC:")
                  (vm-postpone-message fcc vm-serial-send-mail-exit t))
              (if vm-serial-send-mail-exit
                  (kill-this-buffer))))))))

(defadvice vm-mail-send-and-exit (after vm-serial-send-mail activate)
  (if vm-serial-source-buffer
      (kill-this-buffer)))

;;-----------------------------------------------------------------------------
(provide 'vm-serial)
 
;;; vm-serial.el ends here
