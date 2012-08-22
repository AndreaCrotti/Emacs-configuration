;;; magit-svn.el --- git-svn plug-in for Magit

;; Copyright (C) 2008  Alex Ott
;; Copyright (C) 2009  Alexey Voinov
;; Copyright (C) 2009  John Wiegley
;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2008  Marcin Bachry
;; Copyright (C) 2008, 2009  Marius Vollmer
;; Copyright (C) 2010  Yann Hodique
;;
;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This plug-in provides git-p4 functionality as a separate component of Magit

;;; Code:

(require 'magit)
(eval-when-compile
  (require 'cl))

;; git p4 commands

(defun magit-p4-find-rev (rev &optional branch)
  (interactive
   (list (read-string "P4 revision: ")
         (if current-prefix-arg
             (read-string "In branch: "))))
  (let* ((sha (apply 'magit-git-string
                     `("p4"
                       "find-rev"
                       ,(concat "r" rev)
                       ,@(when branch (list branch))))))
    (if sha
        (magit-show-commit
         (magit-with-section sha 'commit
           (magit-set-section-info sha)
           sha))
      (error "Revision %s could not be mapped to a commit" rev))))

(defun magit-p4-create-branch (name)
  (interactive "sBranch name: ")
  (magit-run-git "p4" "branch" name))

(defun magit-p4-rebase ()
  (interactive)
  (magit-run-git-async "p4" "rebase"))

(defun magit-p4-dcommit ()
  (interactive)
  (magit-run-git-async "p4" "dcommit"))

(defun magit-p4-enabled ()
  (not (null (magit-p4-get-ref-info t))))

(defun magit-p4-expand-braces-in-branches (branch)
  (if (not (string-match "\\(.+\\){\\(.+,.+\\)}\\(.*\\):\\(.*\\)\\\*" branch))
      (list branch)
    (let ((prefix (match-string 1 branch))
          (suffix (match-string 3 branch))
          (rhs (match-string 4 branch))
          (pieces (split-string (match-string 2 branch) ",")))
      (mapcar (lambda (p) (concat prefix p suffix ":" rhs p)) pieces))))

(defun magit-p4-get-local-ref (url)
  (let* ((branches (cons (magit-get "p4-remote" "p4" "fetch")
                        (magit-get-all "p4-remote" "p4" "branches")))
         (branches (apply 'nconc
                          (mapcar 'magit-p4-expand-braces-in-branches
                                  branches)))
        (base-url (magit-get "p4-remote" "p4" "url"))
        (result nil))
    (while branches
      (let* ((pats (split-string (pop branches) ":"))
             (src (replace-regexp-in-string "\\*" "\\\\(.*\\\\)" (car pats)))
             (dst (replace-regexp-in-string "\\*" "\\\\1" (cadr pats)))
             (base-url (replace-regexp-in-string "\\+" "\\\\+" base-url))
             (base-url (replace-regexp-in-string "//.+@" "//" base-url))
             (pat1 (concat "^" src "$"))
             (pat2 (cond ((equal src "") (concat "^" base-url "$"))
                         (t (concat "^" base-url "/" src "$")))))
        (cond ((string-match pat1 url)
               (setq result (replace-match dst nil nil url))
               (setq branches nil))
              ((string-match pat2 url)
               (setq result (replace-match dst nil nil url))
               (setq branches nil)))))
    result))

(defvar magit-p4-get-ref-info-cache nil
  "A cache for p4-ref-info.
As `magit-get-p4-ref-info' might be considered a quite
expensive operation a cache is taken so that `magit-status'
doesn't repeatedly call it.")

(defun magit-p4-get-ref-info (&optional use-cache)
  "Gather details about the current git-p4 repository.
Return nil if there isn't one.  Keys of the alist are ref-path,
trunk-ref-name and local-ref-name.
If USE-CACHE is non-nil then return the value of `magit-get-p4-ref-info-cache'."
  (if (and use-cache magit-p4-get-ref-info-cache)
      magit-p4-get-ref-info-cache
    (let* ((fetch (magit-get "p4-remote" "p4" "fetch"))
           (url)
           (revision))
      (when fetch
        (let* ((ref (cadr (split-string fetch ":")))
               (ref-path (file-name-directory ref))
               (trunk-ref-name (file-name-nondirectory ref)))
          (set (make-local-variable
                'magit-p4-get-ref-info-cache)
                (list
                 (cons 'ref-path ref-path)
                 (cons 'trunk-ref-name trunk-ref-name)
                 ;; get the local ref from the log. This is actually
                 ;; the way that git-p4 does it.
                 (cons 'local-ref
                       (with-temp-buffer
                         (insert (or (magit-git-string "log" "--first-parent"
                                                       "--grep" "git-p4" "-1")
                                     ""))
                         (goto-char (point-min))
                         (cond ((re-search-forward "git-p4-id: \\(.+/.+?\\)@\\([0-9]+\\)" nil t)
                                (setq url (match-string 1)
                                      revision (match-string 2))
                                (magit-p4-get-local-ref url))
                               (t
                                (setq url (magit-get "p4-remote" "p4" "url"))
                                nil))))
                 (cons 'revision revision)
                 (cons 'url url))))))))

(defun magit-p4-get-ref (&optional use-cache)
  "Get the best guess remote ref for the current git-p4 based branch.
If USE-CACHE is non nil, use the cached information."
  (let ((info (magit-p4-get-ref-info use-cache)))
    (cdr (assoc 'local-ref info))))

(magit-define-inserter p4-unpulled (&optional use-cache)
  (when (magit-p4-enabled)
    (apply #'magit-git-section
           'p4-unpulled "Unpulled commits (P4):" 'magit-wash-log "log"
           (append magit-git-log-options
                   (list
                    (format "HEAD..%s" (magit-p4-get-ref use-cache)))))))

(magit-define-inserter p4-unpushed (&optional use-cache)
  (when (magit-p4-enabled)
    (apply #'magit-git-section
           'p4-unpushed "Unpushed commits (P4):" 'magit-wash-log "log"
           (append magit-git-log-options
                   (list
                    (format "%s..HEAD" (magit-p4-get-ref use-cache)))))))

(magit-define-section-jumper p4-unpushed  "Unpushed commits (P4)")

(defun magit-p4-remote-string ()
  (let ((p4-info (magit-p4-get-ref-info)))
    (when p4-info
      (concat (cdr (assoc 'url p4-info))
              " @ "
              (cdr (assoc 'revision p4-info))))))

(defun magit-p4-remote-update ()
  (interactive)
  (when (magit-p4-enabled)
    (magit-run-git-async "p4" "fetch")))

(easy-menu-define magit-p4-extension-menu
  nil
  "Git P4 extension menu"
  '("Git P4"
    :visible magit-p4-mode
    ["Create branch" magit-p4-create-branch (magit-p4-enabled)]
    ["Rebase" magit-p4-rebase (magit-p4-enabled)]
    ["Fetch" magit-p4-remote-update (magit-p4-enabled)]
    ["Commit" magit-p4-dcommit (magit-p4-enabled)]))

(easy-menu-add-item 'magit-mode-menu
                    '("Extensions")
                    magit-p4-extension-menu)

;; add the group and its keys
(progn
  ;; (re-)create the group
  (magit-key-mode-add-group 'p4)

  (magit-key-mode-insert-action 'p4 "r" "Rebase" 'magit-p4-rebase)
  (magit-key-mode-insert-action 'p4 "c" "DCommit" 'magit-p4-dcommit)
  (magit-key-mode-insert-action 'p4 "f" "Fetch" 'magit-p4-remote-update)
  (magit-key-mode-insert-action 'p4 "s" "Find rev" 'magit-p4-find-rev)
  (magit-key-mode-insert-action 'p4 "B" "Create branch" 'magit-p4-create-branch)

  ;; generate and bind the menu popup function
  (magit-key-mode-generate 'p4))

(defvar magit-p4-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "N") 'magit-key-mode-popup-p4)
    map))

;;;###autoload
(define-minor-mode magit-p4-mode "P4 support for Magit"
  :lighter " P4" :require 'magit-p4 :keymap 'magit-p4-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (let ((unpulled-hook (lambda () (magit-insert-p4-unpulled t)))
        (unpushed-hook (lambda () (magit-insert-p4-unpushed t)))
        (remote-hook 'magit-p4-remote-string))
    (if magit-p4-mode
        (progn
          (add-hook 'magit-after-insert-unpulled-commits-hook unpulled-hook nil t)
          (add-hook 'magit-after-insert-unpushed-commits-hook unpushed-hook nil t)
          (add-hook 'magit-remote-string-hook remote-hook nil t))
      (progn
        (remove-hook 'magit-after-insert-unpulled-commits-hook unpulled-hook t)
        (remove-hook 'magit-after-insert-unpushed-commits-hook unpushed-hook t)
        (remove-hook 'magit-remote-string-hook remote-hook t)))
    (when (called-interactively-p 'any)
      (magit-refresh))))

;;;###autoload
(defun turn-on-magit-p4 ()
  "Unconditionally turn on `magit-p4-mode'."
  (magit-p4-mode 1))

(provide 'magit-p4)
;;; magit-p4.el ends here
