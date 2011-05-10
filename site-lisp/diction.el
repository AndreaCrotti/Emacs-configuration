;;; diction.el --- minor mode to interface the command diction

;;; Copyright Sven Utcke <Sven.Utcke@gmx.de>

;; Emacs Lisp Archive Entry
;; Filename:      diction.el
;; Version:       $Id: diction.el,v 1.8 2005/09/29 12:55:52 utcke Exp $
;; Keywords:      diction, style
;; Author:        Sven Utcke <Sven.Utcke@gmx.de>
;; Maintainer:    Sven Utcke <Sven.Utcke@gmx.de>
;; Description:   minor mode to interface the command diction
;; URL:           http://www.informatik.uni-freiburg.de/~utcke/English/Software/index.html#diction
;; Compatibility: Emacs20
;; License:       Public Domain

;; This program was placed in the public domain on 13.2.2002 by the
;; Author. The program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:
;;
;; Put diction.el somewhere in your load-path and the following 
;; into your .emacs:
;;
;; (require 'diction)
;;
;; You can then call one of diction-buffer or diction-region.
;;
;; diction currently supports only English and German, although it
;; would be easy to write rulesets for other languages.  You can set the
;; language by specifying the ruleset, doing one of
;;
;; M-x set-variable RET diction-ruleset RET "en"
;; M-x set-variable RET diction-ruleset RET "de"
;;
;; which sets the (buffer local) ruleset to use.  However, if you do
;; not set the ruleset explicitly, then diction will try to guess the
;; ruleset by analysing the dictionary used when running ispell (provided
;; you already used ispell on that buffer) or by a simple (but usually
;; very good) heuristic.

(require 'compile)

(defvar diction-prefix-alist 
  '(((latex-mode tex-mode plain-tex-mode slitex-mode ams-tex-mode) . "detex -C | ")
    ((hm--html-mode html-mode html-helper-mode) . "dehtml -C | ")
    ((nroff-mode) . "deroff -C | ")
    ((text-mode) . "")
    )
  "*Association between buffer major-modes and preprocessors used to
remove language constructs specific to this mode.  The preprocessor
should be able to generate #line directives!")

(defvar diction-language-alist
  '((("american" "british" "english") . "en")
    (("deutsch-ascii" "deutsch-tex" "deutsch8" "neu-deutsch-ascii" "neu-deutsch-tex" "neu-deutsch8") . "de")
    )
  "*Association between ispell-local-dictionary and the language used
for diction.  Currently, only rulesets for German (de) and English
(en) are provided.")

;; This should really pic one out of a list of possibilities
;; also, it should be buffer-local
(defvar diction-ruleset nil
  "* The ruleset to be used.  Currently only \"de\" (Deutsch) and
\"en\" (English) are provided.")
(make-variable-buffer-local 'diction-ruleset)

;; the prefix-command
(defvar diction-prefix nil
  "* Used to remove language-specific constructs from the file.")
(make-variable-buffer-local 'diction-prefix)

;; the actual diction-command
(defvar diction-diction '"diction -L"
  "* The command calling diction.")

;; A function which sets a variable from an association list
(defun diction-associate (l from-var to-var)
  (cond ((null l)
         nil)
        ((member (symbol-value from-var) (first (first l)))
         (set to-var (rest (first l))))
        (t (diction-associate (rest l) 'from-var 'to-var))))

;; A function which guesses the buffer's language if not specified
;; borrowed from Jean-Philippe Theberge <jphil@godzilla.act.oda.fr>
(defun diction-guess-buffer-language ()
;  (interactive)
  (block xxx
    (save-excursion 
      (goto-char (point-min))
      (let* ((LangL '(("French" . " \\(et\\|ou\\|[ld]es\\|que\\) ")
                      ("English" . " \\(of\\|the\\|and\\|or\\) ")
                      ("German" . " \\(und\\|oder\\|der\\|das\\) ") 
                      ("Spanish" . " \\(el\\|una\\|l[oa]s\\|y\\|del\\) ")))
             (countL (map 'list (lambda (x) (cons (count-matches (cdr x)) 
                                                  (car x))) LangL))
             (winner (cdr (assoc (car (sort (map 'list 'car countL) '>)) countL))))
        (message (concat "using " winner))
        (cond ((string-equal winner "English") (return-from xxx "en"))
              ((string-equal winner "German")  (return-from xxx "de"))
              ((string-equal winner "French")  (return-from xxx "en")) ; French is not supported
              ((string-equal winner "Spanish") (return-from xxx "en")) ; Spanish is not supported
              )))))

;; Patterns for hilit19.  This also defines the patterns normally used 
;; in compilation-mode, since I'm going to replace the standard ones.  
;; Naughty me...
(defvar diction-hilit19-patterns
  '(
    ("^[-_./\"A-Za-z0-9]+:[0-9]+: diction" 0 string)
    ("^[-_./\"A-Za-z0-9]+:[0-9]+: diction:.*$" 0 default)
    ("\\[[^]]+->" 0 error)		;maybe that's overdoing it?
    ("\\[[^]]+\\]" 0 rule)
    ("^[-_.\"A-Za-z0-9]+\\(:\\|, line \\)[0-9]+: warning:.*$" 0 warning)
    ("^[-_.\"A-Za-z0-9]+\\(:\\|, line \\)[0-9]+:.*$" 0 error)
    )
  )

;;
(defun diction-hilit ()
  "Set up hilit19 support for diction in compilation-mode, 
   but only if hilit19 has been pre-loaded."
;  (interactive)
  (cond ((and (boundp 'hilit-default-face-table) hilit-default-face-table)
	 ;; This is replacing the original patterns.  Naughty...
	 (hilit-set-mode-patterns 'compilation-mode diction-hilit19-patterns)
	 )))

;; this is doing all the work
(defun diction-delimited (diction-start diction-end)
  ;; make sure diction-start comes before diction-end
  ;; Do I still need this?
  (cond ((< diction-end diction-start)
	 (setq swap diction-start)
	 (setq diction-start diction-end)
	 (setq diction-end swap)
	 ))
  ;; find the current line-number:
  (let ((opoint diction-start) start)
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (widen)
        (beginning-of-line)
        (setq start (point))
        (goto-char opoint)
        (beginning-of-line)
	(setq diction-start-line (1+ (count-lines 1 (point))))
	)
      )
    )
  ;; identify the language if necessary
  (if (null diction-ruleset)
      (if (or (not (boundp 'ispell-local-dictionary)) (null ispell-local-dictionary))
          (setq diction-ruleset (diction-guess-buffer-language))  ;then
        (diction-associate diction-language-alist 'ispell-local-dictionary 'diction-ruleset) ;else
        ))
  ;; identify the prefix if necessary
  (if (null diction-prefix)
      (diction-associate diction-prefix-alist 'major-mode 'diction-prefix)
    )
  ;; this is the entire command we are running
  (setq diction-command (concat diction-prefix diction-diction diction-ruleset ))
  ;; run diction, and put the output into a buffer *Diction-Output*
  (message (concat "running " diction-command " ..."))
  (shell-command-on-region diction-start diction-end diction-command "*Diction-Output*")
  ;; remember the current filename
  (setq diction-buffer (concat buffer-file-name))
  ;; change to the *Diction-Output* buffer
  (set-buffer "*Diction-Output*")
  ;; replace (stdin):number with buffer:number + diction-start-line
  ;; this way we can work on regions too
  (message "Reformating buffer... 1st pass")
  (while (re-search-forward "^(stdin):\\([0-9]+\\):" nil t)
    (let ((n (string-to-int (match-string 1))))
      (replace-match (concat diction-buffer ":" (number-to-string (+ n diction-start-line)) ":") t nil)))
  ;; replace file:number: with file:number: diction:
  ;; This way we can do some clever highlighting
  (message "Reformating buffer... 2nd pass")
  (goto-char (point-min))
  (while (re-search-forward "^[-_./\"A-Za-z0-9]+:[0-9]+:" nil t)
    (replace-match "\\& diction:" t nil))
  ;; switch on highlighting --- this might just do nothing, 
  ;; if hilit19 isn't used
  (diction-hilit)
  ;; put the buffer into compilation-mode --- does all the nifty mouse-stuff
  (compilation-mode)
  (compilation-forget-errors)
  ;; highlight
  (cond ((and (boundp 'hilit-default-face-table) hilit-default-face-table)
	 (hilit-highlight-buffer)))
)  

(defun diction-region ()
  "Run \"detex -l | diction\" on region and display output in buffer \"*Diction-Output\"."
  (interactive)
  ;; make sure diction-start comes before diction-end
  (cond ((< (mark) (point))
	 (exchange-point-and-mark)))
  (diction-delimited (mark) (point))
)

(defun diction-buffer ()
  "Run \"detex -l | diction\" on buffer and display output in buffer \"*Diction-Output\"."
  (interactive)
  (diction-delimited (point-min) (point-max))
)

(provide 'diction)

; Changes so far
;
; $Log: diction.el,v $
; Revision 1.8  2005/09/29 12:55:52  utcke
; Applied a patch by Aaron S. Hawley:
;
; Attached is a patch that fixes some type errors that occur when using
; diction.el in Emacs 21 and Emacs 22 (the development "CVS" version).
;
; Revision 1.8  2005/09/28 19:00:31  ashawley
; (diction-guess-buffer-language) Fix type error.
; (diction-delimited) Fix type error.
;
; Revision 1.7  2002/02/14 10:10:43  utcke
; New Emacs Lisp Archive Entry and License (Public Domain).
;
; Revision 1.6  2000/11/21 09:52:45  utcke
; Fixed bug in documentation.
;
; Revision 1.5  2000/09/13 10:00:06  utcke
; * added Emacs Lisp Archive Entry
; * updated commentary
; * This now requires compile
; * variables diction-prefix-alist and diction-language-alist to guess
;   preprocessing and language, as well as the corresponding function
;   diction-associate.
; * diction-guess-buffer-language to guess the language.
; * should now work without ispell/hilit19
; * works with compile.el again.
;
; Revision 1.4  1998/05/08 21:35:16  utcke
; somewhat shorter, thanks to an idea by Kai Grossjohann.
;
; Revision 1.3  1998/05/08 20:57:09  utcke
; Improved documentation some.
;
; Revision 1.2  1998/05/08 20:37:49  utcke
; works (mainly)
;
; TODO: The first occurrence found doesn't work.  Why?
;
; Revision 1.1  1998/05/08 14:43:01  utcke
; Initial revision
;
;

;; TODO
;;
;; * convert umlauts based on major mode / some variable
;; * use Gnu recode / tcl??? to convert umlauts
;; * mechanism similar to ispell-words-keyword?
;; * allow for an additional ruleset (buffer-local, or maybe using an association again?)
;;
