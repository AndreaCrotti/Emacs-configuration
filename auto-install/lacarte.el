;;; lacarte.el --- Execute menu items as commands, with completion.
;;
;; Filename: lacarte.el
;; Description: Execute menu items as commands, with completion.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005-2010, Drew Adams, all rights reserved.
;; Created: Fri Aug 12 17:18:02 2005
;; Version: 22.0
;; Last-Updated: Fri Jun 25 21:05:15 2010 (-0700)
;;           By: dradams
;;     Update #: 632
;; URL: http://www.emacswiki.org/cgi-bin/wiki/lacarte.el
;; Keywords: menu-bar, menu, command, help, abbrev, minibuffer, keys,
;;           completion, matching, local, internal, extensions,
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Q. When is a menu not a menu?  A. When it's a la carte.
;;
;;  Library La Carte lets you execute menu items as commands, with
;;  completion.  You can use it as an alternative to standard library
;;  `tmm.el'.
;;
;;  Type a menu item.  Completion is available.  Completion candidates
;;  are of the form menu > submenu > subsubmenu > ... > menu item.
;;  For example:
;;
;;    File > Open Recent > Cleanup list
;;    File > Open Recent > Edit list...
;;
;;  When you choose a menu-item candidate, the corresponding command
;;  is executed.
;;
;;  Put this in your init file (~/.emacs):
;;
;;    (require 'lacarte)
;;
;;  Suggested key bindings:
;;
;;    (global-set-key [?\e ?\M-x] 'lacarte-execute-command)
;;    (global-set-key [?\M-`]     'lacarte-execute-menu-command)
;;    (global-set-key [f10]       'lacarte-execute-menu-command)
;;
;;  (The latter two replace standard bindings for `tmm-menubar'.  On
;;  MS Windows, `f10' is normally bound to `menu-bar-open', which uses
;;  the Windows native keyboard access to menus.)
;;
;;  To really take advantage of La Carte, use it together with
;;  Icicles.  Icicles is not required to be able to use La Carte, but
;;  it enhances the functionality of `lacarte.el' considerably.
;;  (Note: `lacarte.el' was originally called `icicles-menu.el'.)
;;
;;  If you use MS Windows keyboard accelerators, consider using
;;  `lacarte-remove-w32-keybd-accelerators' as the value of
;;  `lacarte-convert-menu-item-function'.  It removes any unescaped
;;  `&' characters (indicating an accelerator) from the menu items.
;;  One library that adds keyboard accelerators to your menu items is
;;  `menuacc.el', by Lennart Borgman (< l e n n a r t . b o r g m a n
;;  @ g m a i l . c o m >).
;;
;;
;;  Commands defined here:
;;
;;    `lacarte-execute-command', `lacarte-execute-menu-command'.
;;
;;  Options defined here: `lacarte-convert-menu-item-function'.
;;
;;  Non-interactive functions defined here:
;;
;;    `lacarte-escape-w32-accel', `lacarte-get-a-menu-item-alist',
;;    `lacarte-get-a-menu-item-alist-1',
;;    `lacarte-get-overall-menu-item-alist', `lacarte-menu-first-p',
;;    `lacarte-remove-w32-keybd-accelerators'.
;;
;;  Internal variables defined here:
;;
;;    `lacarte-history', `lacarte-menu-items-alist'.
;;
;;
;;  Getting Started
;;  ---------------
;;
;;  In your init file (`~/.emacs'), bind `ESC M-x' as suggested above:
;;
;;    (global-set-key [?\e ?\M-x] 'lacarte-execute-command)
;;
;;  Type `ESC M-x' (or `ESC ESC x', which is the same thing).  You are
;;  prompted for a command or menu command to execute.  Just start
;;  typing its name.  Each menu item's full name, for completion, has
;;  its parent menu names as prefixes.
;;
;;  ESC M-x
;;  Command:
;;  Command: t [TAB]
;;  Command: Tools >
;;  Command: Tools > Compa [TAB]
;;  Command: Tools > Compare (Ediff) > Two F [TAB]
;;  Command: Tools > Compare (Ediff) > Two Files... [RET]
;;
;;
;;  Not Just for Wimps and Noobs Anymore
;;  ------------------------------------
;;
;;  *You* don't use menus.  Nah, they're too slow!  Only newbies and
;;  wimps use menus.  Well not any more.  Use the keyboard to access
;;  any menu item, without knowing where it is or what its full name
;;  is.  Type just part of its name and use completion to get the
;;  rest: the complete path and item name.
;;
;;
;;  Commands and Menu Commands
;;  --------------------------
;;
;;  You can bind either `lacarte-execute-menu-command' or
;;  `lacarte-execute-command' to a key such as `ESC M-x'.
;;
;;  `lacarte-execute-menu-command' uses only menu commands.
;;  `lacarte-execute-command' lets you choose among ordinary Emacs
;;  commands, in addition to menu commands.  You can use a prefix arg
;;  with `lacarte-execute-command' to get the same effect as
;;  `lacarte-execute-menu-command'.
;;
;;  Use `lacarte-execute-command' if you don't care whether a command
;;  is on a menu.  Then, if you want a command that affects a buffer,
;;  just type `buf'.  This is especially useful if you use Icicles -
;;  see below.
;;
;;  By default, in Icicle mode, `ESC M-x' is bound to
;;  `lacarte-execute-command', and `M-`' is bound to
;;  `lacarte-execute-menu-command'.
;;
;;
;;  Icicles Enhances Dining A La Carte
;;  ----------------------------------
;;
;;  Use Icicles with La Carte to get more power and convenience.
;;
;;  It is Icicles that lets you choose menu items a la carte, in fact.
;;  That is, you can access them directly, wherever they might be in
;;  the menu hierachy.  Without Icicles, you are limited to choosing
;;  items by their menu-hierarchy prefixes, and you must complete the
;;  entire menu prefix to the item, from the top of the menu on down.
;;  With Icicles, you can directly match any parts of a menu item and
;;  its hierarchy path.  Icicles is here:
;;  http://www.emacswiki.org/cgi-bin/wiki/Icicles.
;;
;;  Type any part of a menu-item, then use the Page Up and Page Down
;;  keys (`prior' and `next') to cycle through all menu commands that
;;  contain the text you typed somewhere in their name.  You can match
;;  within any menu or within all menus; that is, you can match any
;;  part(s) of the menu-hierachy prefix.
;;
;;  You can use `S-TAB' to show and choose from all such "apropos
;;  completions", just as you normally use `TAB' to show all prefix
;;  completions (that is, ordinary completions).  Vanilla, prefix
;;  completion is still available using `TAB', and you can cycle
;;  through the prefix completions using the arrow keys.
;;
;;  You can use Icicles "progressive completion" to match multiple
;;  parts of a menu item separately, in any order.  For example, if
;;  you want a menu command that has to do with buffers and
;;  highlighting, type `buf M-SPC hig S-TAB'.
;;
;;  Icicles apropos completion also lets you type a regular expression
;;  (regexp) - it is matched against all of the possible menu items.
;;  So, for instance, you could type `^e.+buff [next] [next]...' to
;;  quickly cycle to menu command `Edit > Go To > Goto End of Buffer'.
;;  Or type `.*print.*buf S-TAB' to choose from the list of all menu
;;  commands that match `print' followed somewhere by `buf'.
;;
;;  If you know how to use regexps, you can easily and quickly get to
;;  a menu command you want, or at least narrow the list of candidates
;;  for completion and cycling.
;;
;;  Additional benefits of using Icicles with La Carte:
;;
;;  * When you cycle to a candidate menu item, or you complete to one
;;    (entirely), the Emacs command associated with the menu item is
;;    shown in the mode line of buffer `*Completions*'.
;;
;;  * You can use `M-h' to complete your minibuffer input against
;;    commands, including menu-item commands, that you have entered
;;    previously.  You can also use the standard history keys
;;    (e.g. `M-p', `M-r') to access these commands.
;;
;;
;;  Menu Organization Helps You Find a Command
;;  ------------------------------------------
;;
;;  Unlike commands listed in a flat `*Apropos*' page, menu items are
;;  organized, grouped logically by common area of application
;;  (`File', `Edit',...).  This grouping is also available when
;;  cycling completion candidates using Icicles, and you can take
;;  advantage of it to hasten your search for the right command.
;;
;;  You want to execute a command that puts the cursor at the end of a
;;  buffer, but you don't remember its name, what menu it might be a
;;  part of, or where it might appear in that (possibly complex) menu.
;;  With Icicles and La Carte, you type `ESC M-x' and then type
;;  `buffer' at the prompt.  You use the Page Up and Page Down keys to
;;  cycle through all menu items that contain the word `buffer'.
;;
;;  There are lots of such menu items.  But all items from the same
;;  menu (e.g. `File') are grouped together.  You cycle quickly (not
;;  reading) to the `Edit' menu, because you guess that moving the
;;  cursor has more to do with editing than with file operations, tool
;;  use, buffer choice, help, etc.  Then you cycle more slowly among
;;  the `buffer' menu items in the `Edit' menu.  You quickly find
;;  `Edit > Go To > Goto End of Buffer'.  QED.
;;
;;
;;  Learn About Menu Items By Exploring Them
;;  ----------------------------------------
;;
;;  With Icicles, you can display the complete documentation (doc
;;  string) for the command corresponding to each menu item, as the
;;  item appears in the minibuffer.  To do this, just cycle menu-item
;;  candidates using `C-down' or `C-next', instead of `[down]' or
;;  `[next]'.  The documentation appears in buffer `*Help*'.
;;
;;  In sum, if you use La Carte, you will want to use it with Icicles
;;  - enjoy!
;;
;;
;;  To Do?
;;  ------
;;
;;  1. Provide sorting by menu-bar order, instead of alphabetically.
;;  2. Echo key bindings for each completed menu item.
;;
;;  3. Maybe use tmm-get-bind?
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Change log")
;;  (@> "User Options")
;;  (@> "Internal Variables")
;;  (@> "Functions")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;(@* "Change log")
;;
;; 2010/06/26 dadams
;;    lacarte-execute-command: Protected Icicles vars with boundp.  Thx to Alexey Romanov.
;; 2010/05/11 dadams
;;     lacarte-get-a-menu-item-alist-1: Add keyboard shortcuts to item names.
;;     Applied Icicles renamings (belatedly):
;;       icicle-sort-functions-alist to icicle-sort-orders-alist,
;;       icicle-sort-function to icicle-sort-comparer.
;; 2009/12/25 dadams
;;     Added: lacarte-execute-command, lacarte-menu-first-p.
;;     lacarte-get-a-menu-item-alist-1: Handle :filter (e.g. File > Open Recent submenus).
;;     lacarte-execute-menu-command:
;;       Just let-bind lacarte-menu-items-alist - don't use unwind-protect.
;;     lacarte-get-overall-menu-item-alist: Reset lacarte-menu-items-alist to nil.
;;     lacarte-get-a-menu-item-alist: Set to the return value.
;; 2009/07/29 dadams
;;     Added: lacarte-history.
;;     lacarte-execute-menu-command:
;;       Use lacarte-history as the history list.  Use strict completion.
;; 2009/07/26 dadams
;;     lacarte-execute-menu-command: Use icicle-interactive-history as the history list.
;; 2008/08/28 dadams
;;     Renamed from alacarte to lacarte.  Confusion with alacarte Ubuntu source package.
;; 2008/05/21 dadams
;;     Renamed library icicles-menu.el to alacarte.el.
;;     alacarte-execute-menu-command: Case-insensitive completion, by default.
;; 2008/05/20 dadams
;;     icicle-get-a-menu-item-alist-1: Don't add non-selectable item to alist.
;; 2006/12/22 dadams
;;     icicle-convert-menu-item-function: Use choice as :type, allowing nil.
;;     :group 'icicles -> :group 'Icicles.
;; 2006/10/16 dadams
;;     icicle-get-overall-menu-item-alist: Include minor-mode keymaps.
;; 2006/03/16 dadams
;;     Added to Commentary.
;; 2006/02/18 dadams
;;     icicle-execute-menu-command: \s -> \\s.  (Thx to dslcustomer-211-74.vivodi.gr.)
;; 2006/01/07 dadams
;;     Added :link for sending bug reports.
;; 2006/01/06 dadams
;;     Changed defgroup to icicles-menu from icicles.
;;     Added :link.
;; 2005/11/08 dadams
;;     icicle-execute-menu-command:
;;       Reset icicle-menu-items-alist in unwind-protect.
;;       Fix for dynamic menus Select and Paste, Buffers, and Frames:
;;         Treat special cases of last-command-event.
;;     icicle-get-overall-menu-item-alist: setq result of sort.
;; 2005/11/05 dadams
;;     Replaced icicle-menu-items with icicle-menu-items-alist (no need for both).
;;     icicle-execute-menu-command: Set, don't bind icicle-menu-items-alist.
;; 2005/08/23 dadams
;;     icicle-execute-menu-command: renamed alist to icicle-menu-items-alist, so can
;;       refer to it unambiguously in icicle-help-on-candidate (in icicles.el).
;; 2005/08/19 dadams
;;     Added: icicle-convert-menu-item-function, icicle-remove-w32-keybd-accelerators,
;;            icicle-escape-w32-accel.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(unless (fboundp 'replace-regexp-in-string) (require 'subr-21 nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "User Options")

;;; User Options -------------------------------------------

(defgroup lacarte nil
  "Execute menu items as commands, with completion."
  :prefix "lacarte-" :group 'menu
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=
lacarte.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/lacarte.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/cgi-bin/wiki/LaCarte")
  :link '(emacs-commentary-link :tag "Commentary" "lacarte.el")
  )

(defcustom lacarte-convert-menu-item-function nil
  "*Function to call to convert a menu item.
Used by `lacarte-execute-menu-command'.  A typical use would be to
remove the `&' characters used in MS Windows menus to define keyboard
accelerators.  See `lacarte-remove-w32-keybd-accelerators'."
  :type '(choice (const :tag "None" nil) function) :group 'lacarte)

;; $$$ NOT YET IMPLEMENTED
;; (defcustom lacarte-sort-menu-bar-order-flag nil
;;   "*Non-nil means that `lacarte-execute-menu-command' uses menu-bar order.
;; Nil means use alphabetic order.
;; The order is what is used for completion.
;; Note: Using a non-nil value imposes an extra sorting operation, which
;;       slows down the creation of the completion-candidates list."
;;   :type 'boolean :group 'lacarte)
 
;;; Internal Variables -------------------------------------

(defvar lacarte-history nil "History for menu items read using La Carte completion.")

;; This is used also in `icicle-help-on-candidate', which is defined in Icicles
;; (library `icicles-mcmd.el').
(defvar lacarte-menu-items-alist nil
  "Alist of pairs (MENU-ITEM . COMMAND).
The pairs are defined by the current local and global keymaps.
MENU-ITEM is a menu item, with ancestor-menu prefixes.
  Example: `(\"Files > Insert File...\" . insert-file)'.
COMMAND is the command  bound to the menu item.")
 
;;; Functions -------------------------------

(defun lacarte-execute-command (&optional no-commands-p)
  "Execute a menu-bar menu command or an ordinary command.
Type a menu item or a command name.  Completion is available.
With a prefix arg, only menu items are available.
Completion is not case-sensitive.  However, if you use Icicles, then
you can use `C-A' in the minibuffer to toggle case-sensitivity.

If you use Icicles, then you can also sort the completion candidates
in different ways, using `C-,'.  With Icicles, by default menu items
are sorted before non-menu commands, and menu items are highlighted
using face `icicle-special-candidate'."
  (interactive "P")
  (let ((lacarte-menu-items-alist         (lacarte-get-overall-menu-item-alist))
        (completion-ignore-case           t) ; Not case-sensitive, by default.
        (icicle-special-candidate-regexp  (and (not no-commands-p) ".* > \\(.\\|\n\\)*"))
        (icicle-sort-orders-alist         (and (boundp 'icicle-sort-orders-alist)
                                               (if no-commands-p
                                                   icicle-sort-orders-alist
                                                 (cons '("menu items first"
                                                         .  lacarte-menu-first-p)
                                                       icicle-sort-orders-alist))))
        (icicle-sort-comparer             (and (boundp 'icicle-sort-comparer)
                                               (if no-commands-p
                                                   icicle-sort-comparer
                                                 'lacarte-menu-first-p)))
        choice cmd)
    (unless no-commands-p
      (mapatoms (lambda (symb)
                  (when (commandp symb)
                    (push (cons (symbol-name symb) symb) lacarte-menu-items-alist)))))
    (setq choice  (completing-read (if no-commands-p "Menu command: " "Command: ")
                                   lacarte-menu-items-alist nil t nil 'lacarte-history)
          cmd     (cdr (assoc choice lacarte-menu-items-alist)))
    (unless cmd (error "No such menu command"))
    ;; Treat special cases of `last-command-event', reconstructing it for
    ;; menu items that get their meaning from the click itself.
    (cond ((eq cmd 'menu-bar-select-buffer)
           (string-match " >\\s-+\\(.+\\)\\s-+\\*?%?\\s-+\\S-*\\s-*$" choice)
           (setq choice (substring choice (match-beginning 1) (match-end 1)))
           (when (string-match "  \\*?%?" choice)
             (setq choice (substring choice 0 (match-beginning 0))))
           (setq last-command-event choice))
          ((eq cmd 'menu-bar-select-yank)
           (string-match "Edit > Select and Paste > \\(.*\\)$" choice)
           (setq last-command-event
                 (substring choice (match-beginning 1) (match-end 1))))
          ((eq cmd 'menu-bar-select-frame)
           (string-match " >\\s-[^>]+>\\s-+\\(.+\\)$" choice)
           (setq choice (substring choice (match-beginning 1) (match-end 1)))
           (setq last-command-event choice)))
    (call-interactively cmd)))

(defun lacarte-menu-first-p (s1 s2)
  "Return non-nil if S1 is a menu item and S2 is not."
  (save-match-data
    (and (string-match " > " s1) (not (string-match " > " s2)))))    

(defun lacarte-execute-menu-command ()
  "Execute a menu-bar menu command.
Type a menu item.  Completion is available.
Completion is not case-sensitive.  However, if you use Icicles, then
you can use `C-A' in the minibuffer to toggle case-sensitivity.
If you use Icicles, then you can also sort the completion candidates
in different ways, using `C-,'."
  (interactive)
  (let* ((lacarte-menu-items-alist  (lacarte-get-overall-menu-item-alist))
         (completion-ignore-case    t) ; Not case-sensitive, by default.
         (menu-item                 (completing-read "Menu command: "
                                                     lacarte-menu-items-alist
                                                     nil t nil 'lacarte-history))
         (cmd                       (cdr (assoc menu-item lacarte-menu-items-alist))))
    (unless cmd (error "No such menu command"))
    ;; Treat special cases of `last-command-event', reconstructing it for
    ;; menu items that get their meaning from the click itself.
    (cond ((eq cmd 'menu-bar-select-buffer)
           (string-match " >\\s-+\\(.+\\)\\s-+\\*?%?\\s-+\\S-*\\s-*$"
                         menu-item)
           (setq menu-item (substring menu-item (match-beginning 1) (match-end 1)))
           (when (string-match "  \\*?%?" menu-item)
             (setq menu-item (substring menu-item 0 (match-beginning 0))))
           (setq last-command-event menu-item))
          ((eq cmd 'menu-bar-select-yank)
           (string-match "Edit > Select and Paste > \\(.*\\)$" menu-item)
           (setq last-command-event
                 (substring menu-item (match-beginning 1) (match-end 1))))
          ((eq cmd 'menu-bar-select-frame)
           (string-match " >\\s-[^>]+>\\s-+\\(.+\\)$" menu-item)
           (setq menu-item (substring menu-item (match-beginning 1) (match-end 1)))
           (setq last-command-event menu-item)))
    (call-interactively cmd)))

(defun lacarte-get-overall-menu-item-alist ()
  "Alist formed from menu items in current active keymaps.
See `lacarte-get-a-menu-item-alist' for the structure.
As a side effect, this modifies `lacarte-get-a-menu-item-alist' and
then resets it to ()"
  (let ((alist
         (apply #'nconc
                (lacarte-get-a-menu-item-alist (assq 'menu-bar (current-local-map)))
                (lacarte-get-a-menu-item-alist (assq 'menu-bar (current-global-map)))
                (mapcar (lambda (map) (lacarte-get-a-menu-item-alist (assq 'menu-bar map)))
                        (current-minor-mode-maps)))))
    (setq lacarte-menu-items-alist ())
    (if nil;; `lacarte-sort-menu-bar-order-flag' ; Not yet implemented.
        (setq alist (sort alist SOME-PREDICATE))
      alist)))

(defun lacarte-get-a-menu-item-alist (keymap)
  "Alist of pairs (MENU-ITEM . COMMAND) defined by KEYMAP.
KEYMAP is any keymap that has menu items.
MENU-ITEM is a menu item, with ancestor-menu prefixes.
  Example: `(\"Files > Insert File...\" . insert-file)'.
COMMAND is the command  bound to the menu item.
Returns `lacarte-menu-items-alist' which it modifies."
  (setq lacarte-menu-items-alist ())
  (lacarte-get-a-menu-item-alist-1 keymap)
  (setq lacarte-menu-items-alist (nreverse lacarte-menu-items-alist)))

(defun lacarte-get-a-menu-item-alist-1 (keymap &optional root)
  "Helper function for `lacarte-get-a-menu-item-alist'.
This calls itself recursively, to process submenus.
Returns `lacarte-menu-items-alist', which it modifies."
  (let ((scan keymap))
    (setq root (or root))               ; nil, for top level.
    (while (consp scan)
      (if (atom (car scan))
          (setq scan (cdr scan))
        (let ((defn (cdr (car scan)))
              composite-name)
          ;; Get REAL-BINDING for the menu item.
          (cond
            ;; (menu-item ITEM-STRING): non-selectable item - skip it.
            ((and (eq 'menu-item (car-safe defn))
                  (null (cdr-safe (cdr-safe defn))))
             (setq defn nil))           ; So `keymapp' test, below, fails.

            ;; (ITEM-STRING): non-selectable item - skip it.
            ((and (stringp (car-safe defn)) (null (cdr-safe defn)))
             (setq defn nil))           ; So `keymapp' test, below, fails.

            ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES), with `:filter'
            ((and (eq 'menu-item (car-safe defn))
                  (member :filter (cdr (cddr defn))))
             (let ((filt  (cadr (member :filter (cdr (cddr defn))))))
               (setq composite-name
                     (concat root (and root " > ") (eval (cadr defn))
                             (let ((keys  (car-safe (cdr-safe (cdr-safe (cdr-safe defn))))))
                             (and (consp keys) (stringp (cdr keys)) (cdr keys)))))
               (setq defn (if (functionp filt) ; Apply the filter to REAL-BINDING.
                              (funcall filt (car (cddr defn)))
                            (car (cddr defn))))))

            ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES)
            ((eq 'menu-item (car-safe defn))
             (setq composite-name
                   (concat root (and root " > ") (eval (cadr defn))
                           (let ((keys  (car-safe (cdr-safe (cdr-safe (cdr-safe defn))))))
                             (and (consp keys) (stringp (cdr keys)) (cdr keys)))))
             (setq defn (car (cddr defn))))

            ;; (ITEM-STRING . REAL-BINDING) or
            ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
            ((stringp (car-safe defn))
             (setq composite-name (concat root (and root " > ") (eval (car defn))))
             (setq defn (cdr defn))
             ;; Skip HELP-STRING
             (when (stringp (car-safe defn)) (setq defn (cdr defn)))
             ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
             ;; But first add shortcuts to composite name.
             (when (and (consp defn) (consp (car defn)))
               (when (stringp (cdar defn)) ; Add shortcuts to name.
                 (setq composite-name (concat composite-name (cdar defn))))
               (setq defn (cdr defn)))))

          ;; If REAL-BINDING is a keymap, then recurse on it.
          (when (keymapp defn)
            ;; Follow indirections to ultimate symbol naming a command.
            (while (and (symbolp defn) (fboundp defn) (keymapp (symbol-function defn)))
              (setq defn (symbol-function defn)))
            (if (eq 'keymap (car-safe defn))
                (lacarte-get-a-menu-item-alist-1 (cdr defn) composite-name)
              (lacarte-get-a-menu-item-alist-1 (symbol-function defn) composite-name)))

          ;; Add menu item + command pair to `lacarte-menu-items-alist' alist.
          ;; Don't add it if `composite-name' is nil - that's a non-selectable item.
          (when (and root composite-name (not (keymapp defn)))
            (setq lacarte-menu-items-alist
                  (cons
                   (cons (if (and (functionp lacarte-convert-menu-item-function)
                                  (stringp composite-name)) ; Could be nil
                             (funcall lacarte-convert-menu-item-function composite-name)
                           composite-name)
                         defn)
                   lacarte-menu-items-alist))))
        (when (consp scan) (setq scan (cdr scan)))))
    lacarte-menu-items-alist))

(defun lacarte-remove-w32-keybd-accelerators (menu-item)
  "Remove `&' characters that define keyboard accelerators in MS Windows.
\"&&\" is an escaped `&' - it is replaced by a single `&'.
This is a candidate value for `lacarte-convert-menu-item-function'."
  (replace-regexp-in-string "&&?" 'lacarte-escape-w32-accel menu-item))

(defun lacarte-escape-w32-accel (match-string)
  "If STRING is \"&&\", then return \"&\".  Else return \"\"."
  (if (> (length match-string) 1) "&" ""))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'lacarte)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lacarte.el ends here
