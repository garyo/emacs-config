;;; gco-pkm-transient.el --- Transient menu for PKM operations -*- lexical-binding: t; -*-

;; Author: Gary Oberbrunner
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (org "9"))
;; Keywords: outlines, convenience, org, pkm

;;; Commentary:
;; Transient menu for PKM operations, combining Logseq-like slash commands
;; with other common operations for org-mode based note-taking.

;;; Code:

(require 'transient)
(require 'org)
(require 'org-roam nil t)
(require 'org-ql nil t)
(require 'consult nil t)

;;;; Helper Functions

(defun gco-pkm--journal-today ()
  "Jump to today's journal entry."
  (interactive)
  (if (fboundp 'my/org-journal-today)
      (my/org-journal-today)
    (message "Journal function not configured")))

(defun gco-pkm--journal-yesterday ()
  "Jump to yesterday's journal entry."
  (interactive)
  (find-file org-default-notes-file)
  (org-datetree-find-date-create 
   (calendar-gregorian-from-absolute 
    (1- (calendar-absolute-from-gregorian (calendar-current-date))))))

(defun gco-pkm--journal-tomorrow ()
  "Jump to tomorrow's journal entry."
  (interactive)
  (find-file org-default-notes-file)
  (org-datetree-find-date-create
   (calendar-gregorian-from-absolute
    (1+ (calendar-absolute-from-gregorian (calendar-current-date))))))

(defun gco-pkm--quick-page-link ()
  "Insert a [[wiki:page]] link with completion."
  (interactive)
  (let ((page-name (gco-pkm-wiki-complete)))
    (insert (format "[[wiki:%s]]" page-name))))

(defun gco-pkm--create-new-page ()
  "Create a new org page/file and jump to it."
  (interactive)
  (let* ((title (read-string "Page title: "))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title)))
         (slug (replace-regexp-in-string "^-\\|-$" "" slug))  ; trim hyphens
         (filename (expand-file-name (concat slug ".org") org-directory)))
    ;; Check if file exists
    (when (file-exists-p filename)
      (if (yes-or-no-p (format "File %s already exists. Open it? " slug))
          (find-file filename)
        (error "Cancelled")))
    ;; Create new file
    (unless (file-exists-p filename)
      (find-file filename)
      (insert (format "#+title: %s\n" title))
      (when (fboundp 'org-id-get-create)
        (org-id-get-create))
      ;; If org-roam is available, update its database
      (when (fboundp 'org-roam-db-update-file)
        (save-buffer)
        (org-roam-db-update-file)))))

(defun gco-pkm--create-block-reference ()
  "Create a reference to current block with ID."
  (interactive)
  (let ((id (org-id-get-create)))
    (kill-new (format "[[id:%s]]" id))
    (message "Block reference copied: [[id:%s]]" id)))

(defun gco-pkm--embed-block ()
  "Insert a transclusion/embed directive."
  (interactive)
  (if (require 'org-transclusion nil t)
      (progn
        (insert "#+transclude: [[id:")
        (insert "]] ")
        (backward-char 3))
    (message "org-transclusion not installed")))

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

(defun gco-pkm--search-pages ()
  "Search pages by title."
  (interactive)
  (cond
   ((fboundp 'org-roam-node-find) (org-roam-node-find))
   ((fboundp 'consult-find) (consult-find org-directory))
   (t (find-file (read-file-name "Find file: " org-directory)))))

(defun gco-pkm--search-content ()
  "Full-text search across all notes."
  (interactive)
  (cond
   ((fboundp 'consult-ripgrep) (consult-ripgrep org-directory))
   ((fboundp 'org-ql-search) (org-ql-search org-directory))
   (t (grep (read-string "Search for: ") org-directory))))

(defun gco-pkm--search-tags ()
  "Search for tags."
  (interactive)
  (if (fboundp 'gco-inline-tags-search)
      (gco-inline-tags-search)
    (org-tags-view)))

(defun gco-pkm--search-todos ()
  "Search for TODO items."
  (interactive)
  (if (fboundp 'org-ql-search)
      (org-ql-search org-directory '(todo))
    (org-todo-list)))

(defun gco-pkm--recent-files ()
  "Show recently modified org files."
  (interactive)
  (let* ((files (directory-files-recursively org-directory "\\.org$" nil))
         (sorted (sort files (lambda (a b)
                              (time-less-p
                               (nth 5 (file-attributes b))
                               (nth 5 (file-attributes a))))))
         (recent (seq-take sorted 20))
         (choice (completing-read "Recent file: " 
                                 (mapcar (lambda (f) (file-relative-name f org-directory))
                                        recent))))
    (find-file (expand-file-name choice org-directory))))

(defun gco-pkm--show-backlinks ()
  "Show backlinks for current node."
  (interactive)
  (cond
   ((fboundp 'org-roam-buffer-toggle) (org-roam-buffer-toggle))
   ((fboundp 'consult-org-roam-backlinks) (consult-org-roam-backlinks))
   (t (message "Backlinks not configured"))))

(defun gco-pkm--insert-timestamp ()
  "Insert current timestamp."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a %H:%M>")))

(defun gco-pkm--insert-date ()
  "Insert current date."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

;;;; Transient Definition

;;;###autoload
(transient-define-prefix gco-pkm-menu ()
  "Transient menu for PKM operations."
  [:description "Personal Knowledge Management"
   ["Journal"
    ("jj" "Today" gco-pkm--journal-today)
    ("jy" "Yesterday" gco-pkm--journal-yesterday)
    ("jt" "Tomorrow" gco-pkm--journal-tomorrow)
    ("jc" "Capture to journal" (lambda () (interactive) (org-capture nil "j")))]
   
   ["Create"
    ("cP" "New page (jump)" gco-pkm--create-new-page)
    ("cp" "Wiki link" gco-pkm--quick-page-link 
     :if-derived org-mode)
    ("cn" "Note (capture)" org-capture)
    ("ct" "TODO" (lambda () (interactive) (org-capture nil "t")))
    ("cb" "Block reference" gco-pkm--create-block-reference 
     :if-derived org-mode)
    ("ce" "Embed block" gco-pkm--embed-block 
     :if-derived org-mode)]]
  
  [:description ""
   ["Search"
    ("sp" "Pages (by title)" gco-pkm--search-pages)
    ("sc" "Content (full-text)" gco-pkm--search-content)
    ("st" "Tags" gco-pkm--search-tags)
    ("sd" "TODOs" gco-pkm--search-todos)
    ("sr" "Recent files" gco-pkm--recent-files)]
   
   ["Insert"
    :if-derived org-mode
    ("it" "TODO" gco-pkm--insert-todo :transient t)
    ("id" "DONE" gco-pkm--insert-done :transient t)
    ("iw" "WAITING" gco-pkm--insert-waiting :transient t)
    ("i#" "Tag" gco-inline-tags-insert)
    ("is" "Schedule" org-schedule)
    ("iD" "Deadline" org-deadline)
    ("iT" "Timestamp" gco-pkm--insert-timestamp)
    ("ia" "Date" gco-pkm--insert-date)]]
  
  [:description ""
   ["Navigate"
    ("nf" "Find node" org-roam-node-find)
    ("ni" "Insert link" org-roam-node-insert 
     :if-derived org-mode)
    ("nb" "Backlinks" gco-pkm--show-backlinks 
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
    ("b" "Block ref" gco-pkm--create-block-reference)
    ("e" "Embed" gco-pkm--embed-block)
    ("l" "Link" org-insert-link)
    ("p" "Wiki link" gco-pkm--quick-page-link)]
   
   ["Meta"
    ("#" "Tag" gco-inline-tags-insert)
    ("s" "Schedule" org-schedule)
    ("D" "Deadline" org-deadline)
    ("T" "Timestamp" gco-pkm--insert-timestamp)
    ("a" "Date" gco-pkm--insert-date)]
   
   [""
    ("/" "Main menu" gco-pkm-menu)
    ("q" "Quit" transient-quit-one)]])

;;;; Wiki-style links

(defun gco-pkm-wiki-follow (name)
  "Follow a wiki link - tries file first, then org-roam node, then creates new."
  (let ((filename (expand-file-name (concat name ".org") org-directory)))
    (cond
     ;; First: Check if file exists
     ((file-exists-p filename)
      (find-file filename))
     
     ;; Second: Try to find org-roam node with this title
     ((and (fboundp 'org-roam-node-from-title-or-alias)
           (org-roam-node-from-title-or-alias name))
      (org-roam-node-visit (org-roam-node-from-title-or-alias name)))
     
     ;; Third: Search for partial matches in org-roam
     ((fboundp 'org-roam-node-find)
      (org-roam-node-find nil name))
     
     ;; Last resort: Offer to create new file
     (t
      (if (y-or-n-p (format "Create new page '%s'? " name))
          (progn
            (find-file filename)
            (insert (format "#+title: %s\n" name))
            (when (fboundp 'org-id-get-create)
              (org-id-get-create)))
        (error "Page not found: %s" name))))))

(defun gco-pkm-wiki-complete ()
  "Completion for wiki links - combines files and org-roam nodes."
  (let* ((files (mapcar (lambda (f)
                         (file-name-sans-extension 
                          (file-name-nondirectory f)))
                       (directory-files org-directory nil "\\.org$")))
         (nodes (when (fboundp 'org-roam-node-list)
                  (mapcar #'org-roam-node-title (org-roam-node-list))))
         (all (delete-dups (append files nodes))))
    (completing-read "Wiki page: " all)))

;; Register the wiki link type
(org-link-set-parameters "wiki"
                        :follow #'gco-pkm-wiki-follow
                        :complete #'gco-pkm-wiki-complete)

(provide 'gco-pkm-transient)
;;; gco-pkm-transient.el ends here