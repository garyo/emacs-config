;;; gco-pkm-transient.el --- Transient menu for PKM operations -*- lexical-binding: t; -*-

;; Author: Gary Oberbrunner
;; Version: 0.3
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (gco-pkm "0.1"))
;; Keywords: outlines, convenience, org, markdown, pkm

;;; Commentary:
;; Transient menu interface for PKM operations.
;;
;; Notes come in org and markdown while the corpus is converted, so entries
;; are gated by what they actually need rather than by major mode:
;;
;;   (no gate)          works in both formats
;;   :if gco-pkm-transient--note-p    needs a note buffer, either format
;;   :if-derived org-mode             genuinely org-only (transclusion,
;;                                    org-node linking, agenda scheduling)

;;; Code:

(require 'transient)
(require 'gco-pkm)
(require 'gco-pkm-format)
(require 'org)
(require 'org-capture)

(declare-function consult-ripgrep "consult")
(declare-function my/pkm-md-toggle-frontmatter "init-org")
(declare-function gco-inline-tags-insert "gco-inline-tags")
(declare-function gco-inline-tags-search "gco-inline-tags")

(defun gco-pkm-transient--note-p ()
  "Non-nil in a note buffer of either format."
  (gco-pkm-format-note-buffer-p))

(defun gco-pkm-transient--md-p ()
  "Non-nil in a markdown note buffer."
  (eq (gco-pkm-format-current) 'md))

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

(defun gco-pkm--insert-checkbox ()
  "Insert an unchecked checkbox item, which reads the same in both formats."
  (interactive)
  (unless (bolp) (insert "\n"))
  (insert "- [ ] "))

(defun gco-pkm--insert-timestamp ()
  "Insert the current timestamp, bracketed only in org.
The converter strips org's angle brackets, so markdown notes carry plain
dates and this keeps new text consistent with converted text."
  (interactive)
  (insert (if (gco-pkm-transient--md-p)
              (format-time-string "%Y-%m-%d %a %H:%M")
            (format-time-string "<%Y-%m-%d %a %H:%M>"))))

(defun gco-pkm--insert-date ()
  "Insert the current date, bracketed only in org."
  (interactive)
  (insert (if (gco-pkm-transient--md-p)
              (format-time-string "%Y-%m-%d %a")
            (format-time-string "<%Y-%m-%d %a>"))))

(defun gco-pkm--insert-image-width ()
  "Constrain the width of the image link on this line.
Uses `#+ATTR_ORG:' in org and an HTML width attribute in markdown, which
is the only thing CommonMark renderers honour."
  (interactive)
  (let ((width (read-string "Image width (pixels): " "400")))
    (if (gco-pkm-transient--md-p)
        (save-excursion
          (beginning-of-line)
          (if (re-search-forward "!\\[\\([^]]*\\)\\](\\([^)]+\\))" (line-end-position) t)
              (replace-match
               (format "<img src=\"%s\" alt=\"%s\" width=\"%s\">"
                       (match-string 2) (match-string 1) width)
               t t)
            (user-error "No image link on this line")))
      (beginning-of-line)
      (insert (format "#+ATTR_ORG: :width %s\n" width))
      (forward-line -1))))

(defun gco-pkm-grep-notes ()
  "Full-text search across notes in either format."
  (interactive)
  (if (fboundp 'consult-ripgrep)
      (consult-ripgrep gco-pkm-directory)
    (rgrep (read-string "Search notes for: ") "*.org *.md" gco-pkm-directory)))

(defun gco-pkm-insert-note-link ()
  "Insert a link to another note, in the current buffer's format."
  (interactive)
  (let* ((by-title (make-hash-table :test #'equal))
         titles)
    (dolist (file (gco-pkm--all-notes))
      (let ((title (gco-pkm--file-title file)))
        (unless (gethash title by-title)
          (puthash title file by-title)
          (push title titles))))
    (let* ((choice (completing-read "Link to note: " (nreverse titles) nil t))
           (target (gethash choice by-title))
           (rel (file-relative-name
                 target (file-name-directory (or (buffer-file-name)
                                                 gco-pkm-directory)))))
      (insert (if (gco-pkm-transient--md-p)
                  (format "[%s](%s)" choice rel)
                (format "[[file:%s][%s]]" rel choice))))))

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
    ("jc" "Capture to journal" (lambda () (interactive) (org-capture nil "j"))
     :if-derived org-mode)]

   ["Create"
    ("cP" "New page" gco-pkm-create-page)
    ("cn" "Note (capture)" org-capture)
    ("ct" "TODO" (lambda () (interactive) (org-capture nil "t"))
     :if-derived org-mode)
    ("cb" "Block reference" gco-pkm-create-block-reference
     :if gco-pkm-transient--note-p)
    ("ce" "Embed block (transclusion)" gco-pkm-embed-block
     :if-derived org-mode)
    ("cy" "Paste image" yank-media
     :if gco-pkm-transient--note-p)]]

  [:description ""
   ["Search"
    ("sp" "Pages (by title)" gco-pkm-find-page)
    ("sc" "Content (full-text)" gco-pkm-grep-notes)
    ("st" "Tags" (lambda () (interactive)
                   (if (fboundp 'gco-inline-tags-search)
                       (gco-inline-tags-search)
                     (gco-pkm-search-tag (read-string "Tag: ")))))
    ("sd" "TODOs" gco-pkm-search-todos)
    ("sr" "Recent files" gco-pkm-recent-files)]

   ["Insert"
    :if gco-pkm-transient--note-p
    ("ix" "Checkbox" gco-pkm--insert-checkbox :transient t)
    ("it" "TODO" gco-pkm--insert-todo :transient t)
    ("id" "DONE" gco-pkm--insert-done :transient t)
    ("iw" "WAITING" gco-pkm--insert-waiting :transient t)
    ("i#" "Tag" (lambda () (interactive)
                  (if (fboundp 'gco-inline-tags-insert)
                      (gco-inline-tags-insert)
                    (insert "#"))))
    ("il" "Link to note" gco-pkm-insert-note-link)
    ("if" "Fold frontmatter" my/pkm-md-toggle-frontmatter
     :if gco-pkm-transient--md-p)
    ("is" "Schedule" org-schedule :if-derived org-mode)
    ("iD" "Deadline" org-deadline :if-derived org-mode)
    ("iT" "Timestamp" gco-pkm--insert-timestamp)
    ("ia" "Date" gco-pkm--insert-date)
    ("ii" "Image width" gco-pkm--insert-image-width)]]

  [:description ""
   ["Navigate"
    ("nf" "Find note" gco-pkm-find-page)
    ("ni" "Insert link" gco-pkm-insert-note-link
     :if gco-pkm-transient--note-p)
    ("nb" "Backlinks" org-node-context-dwim
     :if-derived org-mode)
    ("nc" "Context panel" gco-pkm-context-toggle)
    ("ng" "Grep notes" gco-pkm-grep-notes)
    ("ns" "Sequence nav" org-node-seq-dispatch
     :if-derived org-mode)
    ("nr" "Refile to node" org-node-refile
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
     :if gco-pkm-transient--note-p)
    ("q" "Quit" transient-quit-one)]])

;;;; Quick slash command for insert operations

;;;###autoload
(transient-define-prefix gco-pkm-slash ()
  "Quick slash command menu for insertions."
  [:description "Quick Insert (Slash Commands)"
   :if gco-pkm-transient--note-p
   ["Keywords"
    ("x" "Checkbox" gco-pkm--insert-checkbox :transient t)
    ("t" "TODO" gco-pkm--insert-todo :transient t)
    ("d" "DONE" gco-pkm--insert-done :transient t)
    ("w" "WAITING" gco-pkm--insert-waiting :transient t)
    ("n" "NOTE" (lambda () (interactive) (insert "NOTE ")) :transient t)]

   ["Elements"
    ("b" "Block ref" gco-pkm-create-block-reference)
    ("e" "Embed" gco-pkm-embed-block :if-derived org-mode)
    ("l" "Link to note" gco-pkm-insert-note-link)
    ("L" "Link (org)" org-insert-link :if-derived org-mode)]

   ["Meta"
    ("#" "Tag" (lambda () (interactive)
                 (if (fboundp 'gco-inline-tags-insert)
                     (gco-inline-tags-insert)
                   (insert "#"))))
    ("s" "Schedule" org-schedule :if-derived org-mode)
    ("D" "Deadline" org-deadline :if-derived org-mode)
    ("T" "Timestamp" gco-pkm--insert-timestamp)
    ("a" "Date" gco-pkm--insert-date)]

   [""
    ("/" "Main menu" gco-pkm-menu)
    ("q" "Quit" transient-quit-one)]])

(provide 'gco-pkm-transient)
;;; gco-pkm-transient.el ends here
