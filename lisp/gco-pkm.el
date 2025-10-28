;;; gco-pkm.el --- Personal Knowledge Management core for Org -*- lexical-binding: t; -*-

;; Author: Gary Oberbrunner
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org "9"))
;; Keywords: outlines, convenience, org, pkm
;; URL: https://github.com/garyo/emacs-config

;;; Commentary:
;; Core PKM functionality for org-mode based knowledge management.
;; Provides journal, page creation, wiki links, and search functions.
;; Designed to work with org-roam when available but doesn't require it.

;; Main menu is on C-c C-/ (see init-org)

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-element)
(require 'org-datetree)
(require 'org-capture)
(require 'calendar)
(require 'org-ql)
(require 'org-ql-search)

(require 'gco-pkm-consult)

(use-package org-transclusion)


;; Prevent warnings when running org-ql sexp queries
(setq org-ql-ask-unsafe-queries nil)

;;;; Customization

(defgroup gco-pkm nil
  "Personal Knowledge Management for Org."
  :group 'org
  :prefix "gco-pkm-")

(defcustom gco-pkm-directory (or (bound-and-true-p org-directory)
                                  (expand-file-name "~/Documents/org-agenda"))
  "Directory for PKM notes and files."
  :type 'directory
  :group 'gco-pkm)

(defcustom gco-pkm-journal-file "journal.org"
  "Filename for the journal file (relative to `gco-pkm-directory')."
  :type 'string
  :group 'gco-pkm)

(defcustom gco-pkm-auto-commit nil
  "Whether to auto-commit changes to git."
  :type 'boolean
  :group 'gco-pkm)

;;;; Journal Functions

(defun gco-pkm-journal-file ()
  "Return the full path to the journal file."
  (expand-file-name gco-pkm-journal-file gco-pkm-directory))

(defun gco-pkm-journal-timestamp-target ()
  "Return position at the end of today's journal entry body, creating it if needed.
Suitable for use in `org-capture-templates'. Uses org-datetree and ensures an ID."
  (let* ((ts (format-time-string "<%Y-%m-%d %a>"))
         (date (calendar-current-date))
         pos)
    (with-current-buffer (find-file-noselect (gco-pkm-journal-file))
      (org-with-wide-buffer
        (widen)
        ;; Find/create the day headline
        (setq pos (or (org-find-exact-headline-in-buffer ts)
                      (save-excursion
                        (org-datetree-find-date-create date)
                        (org-edit-headline ts)
                        (point)))))
      ;; Ensure ID (creates drawer immediately under the heading if missing)
      (goto-char pos)
      (org-id-get-create)

      ;; Jump to end of this headline's contents (after drawer & any existing items)
      (org-back-to-heading t)
      (let* ((el (org-element-at-point))
             (append-pos (or (- (org-element-property :contents-end el) 1)
                             (org-element-property :contents-begin el)
                             (save-excursion (org-end-of-subtree t t) (point)))))
        (goto-char append-pos)
        ;; Ensure exactly one line for the new item
        (unless (bolp) (insert "\n"))
        (point)))))

;;;###autoload
(defun gco-pkm-journal-today ()
  "Jump to today's journal entry, creating if needed."
  (interactive)
  (find-file (gco-pkm-journal-file))
  (goto-char (point-min))
  (let ((today (format-time-string "<%Y-%m-%d %a>")))
    (cond ((search-forward today nil t)
           (org-fold-show-subtree)
           (org-end-of-subtree)
           (or (bolp) (insert "\n"))
           (unless (looking-at-p "^$")
             (insert "\n"))
           )
          (t
           (org-datetree-find-date-create (calendar-current-date) nil)
           (org-edit-headline today)
           (org-id-get-create)
           (org-end-of-subtree)
           (or (bolp) (insert "\n"))
           (unless (looking-at-p "^$")
             (insert "\n"))
           (org-fold-show-subtree)))))

;;;###autoload
(defun gco-pkm-journal-yesterday ()
  "Jump to yesterday's journal entry."
  (interactive)
  (find-file (gco-pkm-journal-file))
  (org-datetree-find-date-create
   (calendar-gregorian-from-absolute
    (1- (calendar-absolute-from-gregorian (calendar-current-date))))))

;;;###autoload
(defun gco-pkm-journal-tomorrow ()
  "Jump to tomorrow's journal entry."
  (interactive)
  (find-file (gco-pkm-journal-file))
  (org-datetree-find-date-create
   (calendar-gregorian-from-absolute
    (1+ (calendar-absolute-from-gregorian (calendar-current-date))))))

;;;; Page Creation and Management

(defun gco-pkm--slugify (title)
  "Convert TITLE to a filename-safe slug."
  (let* ((slug (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title)))
         (slug (replace-regexp-in-string "^-\\|-$" "" slug)))
    slug))

;;;###autoload
(defun gco-pkm-create-page (title &optional jump)
  "Create a new org page with TITLE. If JUMP is non-nil, visit the file."
  (interactive (list (read-string "Page title: ") t))
  (let* ((slug (gco-pkm--slugify title))
         (filename (expand-file-name (concat slug ".org") gco-pkm-directory)))
    (when (and (file-exists-p filename) jump)
      (if (yes-or-no-p (format "File %s already exists. Open it? " slug))
          (find-file filename)
        (user-error "Cancelled")))
    (unless (file-exists-p filename)
      (if jump
          (find-file filename)
        (with-temp-file filename))
      (with-current-buffer (find-file-noselect filename)
        (when (= (buffer-size) 0)
          (insert (format "#+title: %s\n" title))
          (org-id-get-create)
          ;; Update org-roam if available
          (when (fboundp 'org-roam-db-update-file)
            (save-buffer)
            (org-roam-db-update-file)))))
    filename))

;;;###autoload
(defun gco-pkm-create-tag-page (tag)
  "Create a dynamic tag page for TAG."
  (interactive "sTag: ")
  (let ((filename (expand-file-name (format "tag-%s.org" tag) gco-pkm-directory)))
    (find-file filename)
    (when (= (buffer-size) 0)
      (insert (format "#+title: #%s\n\n" tag))
      (when (fboundp 'org-ql)
        (insert (format "#+BEGIN: org-ql :query (tags \"%s\")\n\n#+END:\n\n" tag))
        (insert "Press C-c C-c on the block above to refresh.\n"))
      (org-id-get-create)
      (save-buffer))))

;;;; Wiki Links

(defun gco-pkm-wiki-follow (name)
  "Follow a wiki link NAME - tries file first, then org-roam node, then creates new."
  (let ((filename (expand-file-name (concat name ".org") gco-pkm-directory)))
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
          (gco-pkm-create-page name t)
        (user-error "Page not found: %s" name))))))

(defun gco-pkm-wiki-complete ()
  "Completion for wiki links - combines files and org-roam nodes."
  (let* ((files (mapcar (lambda (f)
                         (file-name-sans-extension
                          (file-name-nondirectory f)))
                       (directory-files gco-pkm-directory nil "\\.org$")))
         (nodes (when (fboundp 'org-roam-node-list)
                  (mapcar #'org-roam-node-title (org-roam-node-list))))
         (all (delete-dups (append files nodes))))
    (completing-read "Wiki page: " all)))

;;;###autoload
(defun gco-pkm-insert-wiki-link ()
  "Insert a [[wiki:page]] link with completion."
  (interactive)
  (let ((page-name (gco-pkm-wiki-complete)))
    (insert (format "[[wiki:%s]]" page-name))))

;;;; Block References

;;;###autoload
(defun gco-pkm-create-block-reference ()
  "Create a reference to current block/paragraph with ID."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Block references only work in org-mode"))
  (let ((id (org-id-get-create)))
    (kill-new (format "[[id:%s]]" id))
    (message "Block reference copied: [[id:%s]]" id)))

;;;###autoload
(defun gco-pkm-embed-block ()
  "Insert a transclusion/embed directive at point."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Block embedding only works in org-mode"))
  (if (require 'org-transclusion nil t)
      (progn
        (insert "#+transclude: [[id:")
        (insert "]] ")
        (backward-char 3))
    (insert "#+TRANSCLUDE: [[id:]] ")
    (backward-char 3)))

;;;; Search Functions

;;;###autoload
(defun gco-pkm-search-pages ()
  "Search pages by title."
  (interactive)
  (cond
   ((fboundp 'org-roam-node-find)
    (org-roam-node-find))
   ((fboundp 'consult-find)
    (consult-find gco-pkm-directory))
   (t
    (find-file (read-file-name "Find file: " gco-pkm-directory)))))

;;;###autoload
(defun gco-pkm-search-content ()
  "Full-text search across all notes."
  (interactive)
  (cond
   ((fboundp 'consult-ripgrep)
    (consult-ripgrep gco-pkm-directory))
   ((fboundp 'org-ql-search)
    (org-ql-search gco-pkm-directory))
   (t
    (grep (read-string "Search for: ") gco-pkm-directory))))

;;;###autoload
(defun gco-pkm-search-todos ()
  "Search for TODO items."
  (interactive)
  (if (fboundp 'org-ql-search)
      (org-ql-search gco-pkm-directory '(todo))
    (org-todo-list)))

;;;###autoload
(defun gco-pkm-recent-files (&optional num)
  "Show NUM recently modified org files (default 20)."
  (interactive)
  (let* ((num (or num 20))
         (files (directory-files-recursively gco-pkm-directory "\\.org$" nil))
         (sorted (sort files (lambda (a b)
                              (time-less-p
                               (nth 5 (file-attributes b))
                               (nth 5 (file-attributes a))))))
         (recent (seq-take sorted num))
         (choice (completing-read "Recent file: "
                                 (mapcar (lambda (f)
                                          (file-relative-name f gco-pkm-directory))
                                        recent))))
    (find-file (expand-file-name choice gco-pkm-directory))))

;;;; Utility Functions

;;;###autoload
(defun gco-pkm-auto-commit ()
  "Auto-commit org files after save."
  (when (and gco-pkm-auto-commit
             (buffer-file-name)
             (string-prefix-p (expand-file-name gco-pkm-directory)
                             (buffer-file-name)))
    (shell-command-to-string
     (format "cd %s && git add -A && git commit -m 'Auto-commit: %s'"
             (shell-quote-argument gco-pkm-directory)
             (format-time-string "%Y-%m-%d %H:%M")))))

;;;###autoload
(defun gco-pkm-show-backlinks ()
  "Show backlinks for current node."
  (interactive)
  (cond
   ((fboundp 'org-roam-buffer-toggle)
    (org-roam-buffer-toggle))
   ((fboundp 'consult-org-roam-backlinks)
    (consult-org-roam-backlinks))
   (t
    (message "Backlinks not configured"))))

(defun org-dblock-write:gco-pkm-query (params)
  "Dynamic block for querying across all org files in gco-pkm-directory.
FORMAT is a function that takes (marker title file query) and returns a string to insert."
  (let* ((query (plist-get params :query))
         (format-fn (or (plist-get params :format) 'gco-pkm-query-format-default))
         (files (directory-files-recursively gco-pkm-directory "\\.org$"))
         (results (org-ql-select files query :action 'element-with-markers))
         (lines '()))
    (dolist (result results)
      (when result
        (let* ((marker (org-element-property :org-marker result))
               (title (org-element-property :raw-value result))
               (file (when marker (buffer-file-name (marker-buffer marker)))))
          (when file
            (let ((line (funcall format-fn marker title file query)))
              (when line
                (push line lines)))))))
    ;; Insert all lines
    (dolist (line (nreverse lines))
      (insert line))))

(defun gco-pkm-query-format-default (marker title file query)
  "Default formatter: show file and outline path."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (let* ((outline-path (org-get-outline-path))
             (file-base (file-name-sans-extension
                        (file-name-nondirectory file)))
             (context (if outline-path
                         (format "%s > %s"
                                (mapconcat 'identity outline-path " > ")
                                title)
                       title)))
        (format "- [[file:%s::*%s][%s: %s]]\n"
               file title file-base context)))))

(defun gco-pkm-query-format-with-preview (marker title file query)
  "Formatter with content preview, showing match context for regexp queries."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (let* ((file-base (file-name-sans-extension
                        (file-name-nondirectory file)))
             (element (org-element-at-point))
             ;; Get headline's content boundaries (before parsing section)
             (contents-begin (org-element-property :contents-begin element))
             (contents-end (org-element-property :contents-end element))
             (preview
              (when (and contents-begin contents-end)
                (let ((content (buffer-substring-no-properties
                               contents-begin contents-end)))
                  ;; If it's a regexp query, find and show context around the match
                  (if (and (listp query) (eq (car query) 'regexp))
                      (let ((pattern (cadr query)))
                        (when (string-match pattern content)
                          (let* ((match-pos (match-beginning 0))
                                 (context-start (max 0 (- match-pos 40)))
                                 (context-end (min (length content) (+ match-pos 60)))
                                 (excerpt (substring content context-start context-end))
                                 ;; Clean up whitespace
                                 (cleaned (replace-regexp-in-string "[\n\r]+" " " excerpt)))
                            (concat
                             (if (> context-start 0) "…" "")
                             (string-trim cleaned)
                             (if (< context-end (length content)) "…" "")))))
                    ;; Not a regexp, just show beginning
                    (let ((preview-text (substring content 0 (min 100 (length content)))))
                      (string-trim
                       (replace-regexp-in-string "[\n\r]+" " " preview-text))))))))
        (if (or (null preview) (string-empty-p preview))
            (format "- [[file:%s::*%s][%s: %s]]\n"
                   file title file-base title)
          (format "- [[file:%s::*%s][%s: %s]] — %s\n"
                 file title file-base title preview))))))

(defun gco-pkm-query-format-org-transclusion (marker title file query)
  "Formatter that creates an org-transclusion link for the heading."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (let* ((id (org-id-get-create))  ; Ensure heading has an ID
             (file-base (file-name-sans-extension
                        (file-name-nondirectory file)))
             (outline-path (org-get-outline-path))
             (context (if outline-path
                         (format "%s > %s"
                                (mapconcat 'identity outline-path " > ")
                                title)
                       title)))
        ;; Return an org-transclusion link
        (format "- [[file:%s::*%s][%s: %s]]\n  #+transclude: [[id:%s]] :only-contents\n"
               file title file-base context id)))))


(defun gco-pkm-query-format-simple (marker title file query)
  "Simple formatter: just title and filename."
  (format "- [[file:%s::*%s][%s]]\n" file title title))

(defun my/insert-title-query-block ()
  "Insert query block for references to this file's title."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name)
             (string-match-p (regexp-quote gco-pkm-directory)
                           (buffer-file-name))
             (= (buffer-size) 0))  ; Only in new files
    (let* ((title (file-name-base (buffer-file-name)))
           (query (format "(regexp \"#%s\")" title)))
      (insert (format "#+TITLE: %s\n\n" title))
      (insert (format "#+BEGIN: gco-pkm-query :query %s\n" query))
      (insert "#+END:\n\n"))))

;; (add-hook 'org-mode-hook #'my/insert-title-query-block)

(defun gco-pkm-query-link-follow (tag)
  "Open org-ql-view for TAG."
  (org-ql-search (directory-files-recursively gco-pkm-directory "\\.org$")
    `(regexp ,(format "#%s" tag))
    :title (format "References to #%s" tag)))

(defun gco-pkm-query-link-export (tag desc format)
  "Export the query link."
  (pcase format
    ('html (format "<a href='#'>%s</a>" (or desc tag)))
    (_ (or desc tag))))

(org-link-set-parameters "query"
                         :follow #'gco-pkm-query-link-follow
                         :export #'gco-pkm-query-link-export
                         :face '(:foreground "purple" :underline t))


;;;; Setup

;;;###autoload
(defun gco-pkm-setup ()
  "Set up PKM system."
  (interactive)
  ;; Ensure directory exists
  (unless (file-exists-p gco-pkm-directory)
    (make-directory gco-pkm-directory t))

  ;; Register wiki link type
  (org-link-set-parameters "wiki"
                          :follow #'gco-pkm-wiki-follow
                          :complete #'gco-pkm-wiki-complete)

  ;; Set up auto-commit hook if enabled
  (when gco-pkm-auto-commit
    (add-hook 'after-save-hook #'gco-pkm-auto-commit))

  (message "GCO PKM system initialized in %s" gco-pkm-directory))

(provide 'gco-pkm)
;;; gco-pkm.el ends here
