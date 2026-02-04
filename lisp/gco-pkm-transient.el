;;; gco-pkm-transient.el --- Transient menu for PKM operations -*- lexical-binding: t; -*-

;; Author: Gary Oberbrunner
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (gco-pkm "0.1"))
;; Keywords: outlines, convenience, org, pkm

;;; Commentary:
;; Transient menu interface for PKM operations.
;; Provides quick access to all PKM functions through a modern menu system.

;;; Code:

(require 'transient)
(require 'gco-pkm)
(require 'org)
(require 'org-capture)

;;;; Quick Insert Functions (for transient menu)

(defun gco-pkm--insert-todo ()
  "Insert TODO keyword."
  (interactive)
  (insert "TODO "))

(defun gco-pkm--insert-done ()
  "Insert DONE keyword."
  (interactive)
  (insert "DONE "))

(defun gco-pkm--insert-waiting ()
  "Insert WAITING keyword."
  (interactive)
  (insert "WAITING "))

(defun gco-pkm--insert-timestamp ()
  "Insert current timestamp."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a %H:%M>")))

(defun gco-pkm--insert-date ()
  "Insert current date."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

;;;; Transient Definitions

;;;###autoload
(transient-define-prefix gco-pkm-menu ()
  "Transient menu for PKM operations."
  [:description "Personal Knowledge Management"
   ["Journal"
    ("jj" "Today" gco-pkm-journal-today)
    ("jy" "Yesterday" gco-pkm-journal-yesterday)
    ("jt" "Tomorrow" gco-pkm-journal-tomorrow)
    ("jr" "Recent" gco-pkm-open-recent-journal)
    ("jR" "Show Recent" gco-pkm-show-recent-journals)
    ("jC" "Calendar" gco-pkm-calendar-browse)
    ("jc" "Capture to journal" (lambda () (interactive) (org-capture nil "j")))]
   
   ["Create"
    ("cP" "New page (jump)" (lambda () (interactive) (gco-pkm-create-page (read-string "Page title: ") t)))
    ("cp" "Wiki link" gco-pkm-insert-wiki-link 
     :if-derived org-mode)
    ("cn" "Note (capture)" org-capture)
    ("ct" "TODO" (lambda () (interactive) (org-capture nil "t")))
    ("cb" "Block reference" gco-pkm-create-block-reference 
     :if-derived org-mode)
    ("ce" "Embed block" gco-pkm-embed-block 
     :if-derived org-mode)]]
  
  [:description ""
   ["Search"
    ("sp" "Pages (by title)" gco-pkm-search-pages)
    ("sc" "Content (full-text)" gco-pkm-search-content)
    ("st" "Tags" (lambda () (interactive) 
                  (if (fboundp 'gco-inline-tags-search)
                      (gco-inline-tags-search)
                    (org-tags-view))))
    ("sd" "TODOs" gco-pkm-search-todos)
    ("sr" "Recent files" gco-pkm-recent-files)]
   
   ["Insert"
    :if-derived org-mode
    ("it" "TODO" gco-pkm--insert-todo :transient t)
    ("id" "DONE" gco-pkm--insert-done :transient t)
    ("iw" "WAITING" gco-pkm--insert-waiting :transient t)
    ("i#" "Tag" (lambda () (interactive)
                  (if (fboundp 'gco-inline-tags-insert)
                      (gco-inline-tags-insert)
                    (org-set-tags-command))))
    ("is" "Schedule" org-schedule)
    ("iD" "Deadline" org-deadline)
    ("iT" "Timestamp" gco-pkm--insert-timestamp)
    ("ia" "Date" gco-pkm--insert-date)]]
  
  [:description ""
   ["Navigate"
    ("nf" "Find node" (lambda () (interactive)
                       (if (fboundp 'org-roam-node-find)
                           (org-roam-node-find)
                         (gco-pkm-search-pages))))
    ("ni" "Insert link" (lambda () (interactive)
                         (if (fboundp 'org-roam-node-insert)
                             (org-roam-node-insert)
                           (org-insert-link)))
     :if-derived org-mode)
    ("nb" "Backlinks" gco-pkm-show-backlinks 
     :if-derived org-mode)
    ("na" "Agenda" org-agenda)]
   
   ["Organize"
    :if-derived org-mode
    ("or" "Refile" org-refile)
    ("oa" "Archive" org-archive-subtree)
    ("ot" "Set tags" org-set-tags-command)
    ("op" "Set property" org-set-property)]]
  
  [:description ""
   ["" 
    ("/" "Slash commands" gco-pkm-slash 
     :if-derived org-mode)
    ("q" "Quit" transient-quit-one)]])

;;;; Quick slash command for insert operations

;;;###autoload
(transient-define-prefix gco-pkm-slash ()
  "Quick slash command menu for insertions."
  [:description "Quick Insert (Slash Commands)"
   :if-derived org-mode
   ["Keywords"
    ("t" "TODO" gco-pkm--insert-todo :transient t)
    ("d" "DONE" gco-pkm--insert-done :transient t) 
    ("w" "WAITING" gco-pkm--insert-waiting :transient t)
    ("n" "NOTE" (lambda () (interactive) (insert "NOTE ")) :transient t)]
   
   ["Elements"
    ("b" "Block ref" gco-pkm-create-block-reference)
    ("e" "Embed" gco-pkm-embed-block)
    ("l" "Link" org-insert-link)
    ("p" "Wiki link" gco-pkm-insert-wiki-link)]
   
   ["Meta"
    ("#" "Tag" (lambda () (interactive)
                (if (fboundp 'gco-inline-tags-insert)
                    (gco-inline-tags-insert)
                  (org-set-tags-command))))
    ("s" "Schedule" org-schedule)
    ("D" "Deadline" org-deadline)
    ("T" "Timestamp" gco-pkm--insert-timestamp)
    ("a" "Date" gco-pkm--insert-date)]
   
   [""
    ("/" "Main menu" gco-pkm-menu)
    ("q" "Quit" transient-quit-one)]])

(provide 'gco-pkm-transient)
;;; gco-pkm-transient.el ends here
