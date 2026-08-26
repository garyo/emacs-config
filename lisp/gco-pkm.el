;;; gco-pkm.el --- Personal Knowledge Management core for Org -*- lexical-binding: t; -*-

;; Author: Gary Oberbrunner
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org "9"))
;; Keywords: outlines, convenience, org, pkm
;; URL: https://github.com/garyo/emacs-config

;;; Commentary:
;; Core PKM functionality for org-mode based knowledge management.
;; Provides journal, page creation, and search functions.
;; Uses org-node for node finding, linking, and backlinks.

;; Main menu is on C-c C-/ (see init-org)

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-element)
(require 'org-datetree)
(require 'org-capture)
(require 'calendar)
;; org-ql/org-node index org files only, so they are optional: the PKM has
;; to keep working once the corpus is markdown and these are uninstalled.
(require 'org-ql nil t)
(require 'org-ql-search nil t)

(require 'gco-pkm-consult)
(require 'gco-pkm-format)

(declare-function org-node-create "org-node")
(declare-function consult-ripgrep "consult")

(use-package org-transclusion)


;; Prevent warnings when running org-ql sexp queries
(with-eval-after-load 'org-ql (setq org-ql-ask-unsafe-queries nil))

;;;; Customization

(defgroup gco-pkm nil
  "Personal Knowledge Management for Org."
  :group 'org
  :prefix "gco-pkm-")

(defcustom gco-pkm-directory my/notes-dir
  "Directory for PKM notes and files."
  :type 'directory
  :group 'gco-pkm)

(defcustom gco-pkm-journal-subdir "journals"
  "Subdirectory, relative to `gco-pkm-directory`, for daily journal files."
  :type 'string
  :group 'gco-pkm)

(defun gco-pkm-journal-dir ()
  "Effective journal directory (absolute), ensured to exist."
  (let ((dir (expand-file-name gco-pkm-journal-subdir gco-pkm-directory)))
    (unless (file-directory-p dir) (make-directory dir t))
    dir))

(defcustom gco-pkm-auto-commit nil
  "Whether to auto-commit changes to git."
  :type 'boolean
  :group 'gco-pkm)

;;;; Journal Functions

(defun gco-pkm-journal--path-for-date (&optional date)
  "Return full path for journal file for DATE (default today).
DATE is a list (month day year) as used by calendar functions.
An existing journal for DATE wins in either format; a new one follows
whatever format the journals directory already holds."
  (let* ((date (or date (calendar-current-date)))
         (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
         (dir (gco-pkm-journal-dir))
         (stem (format-time-string "%Y-%m-%d" time))
         (existing (seq-find #'file-exists-p
                             (list (expand-file-name (concat stem ".md") dir)
                                   (expand-file-name (concat stem ".org") dir)))))
    (or existing
        (expand-file-name
         (format "%s.%s" stem
                 (gco-pkm-format-extension (gco-pkm-format-for-dir dir)))
         dir))))

;;;###autoload
(defun gco-pkm-journal-today ()
  "Open today's daily journal file, creating it if necessary."
  (interactive)
  (let ((file (gco-pkm-journal--path-for-date)))
    (find-file file)
    (when (= (buffer-size) 0)
      (if (eq (gco-pkm-format-of file) 'md)
          ;; The org date wrapper collapses into frontmatter.
          (insert (format "---\ntitle: \"%s\"\nid: %s\ndate: %s\n---\n\n"
                          (format-time-string "%Y-%m-%d")
                          (upcase (org-id-new))
                          (format-time-string "%Y-%m-%d")))
        (insert (format "#+title: %s\n\n* <%s>\n"
                        (format-time-string "%Y-%m-%d")
                        (format-time-string "%Y-%m-%d %a")))
        (org-id-get-create))
      (save-buffer))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (message "Opened journal: %s" file)))

(defun gco-pkm-journal--shift-day (n)
  "Return calendar date N days from today."
  (calendar-gregorian-from-absolute
   (+ n (calendar-absolute-from-gregorian (calendar-current-date)))))

;;;###autoload
(defun gco-pkm-journal-yesterday ()
  "Open yesterday’s journal file."
  (interactive)
  (find-file (gco-pkm-journal--path-for-date (gco-pkm-journal--shift-day -1))))

;;;###autoload
(defun gco-pkm-journal-tomorrow ()
  "Open tomorrow’s journal file."
  (interactive)
  (find-file (gco-pkm-journal--path-for-date (gco-pkm-journal--shift-day 1))))

(defun gco-pkm-open-recent-journal ()
  "Quickly open a recent journal file."
  (interactive)
  (let* ((files (directory-files (gco-pkm-journal-dir) nil gco-pkm-format-file-regexp))
         (vertico-sort-function nil)    ; display in provided order (most recent first)
         (choice (completing-read "Recent journal: "
                                  (sort files #'string>))))
    (find-file (expand-file-name choice (gco-pkm-journal-dir)))))

(defun gco-pkm-show-recent-journals (&optional n)
  "Show recent journal entries in a single buffer.
N defaults to 7. Each file may contribute multiple top-level entries."
  (interactive "P")
  (let* ((n (or n 7))
         (journal-dir (gco-pkm-journal-dir))
         (files (sort (directory-files journal-dir t gco-pkm-format-file-regexp) #'string>))
         (recent-files (seq-take files n))
         (buffer (get-buffer-create "*Recent Journals*")))
    (with-current-buffer buffer
      (setq default-directory gco-pkm-directory)
      (read-only-mode -1)
      (erase-buffer)
      ;; Show the digest in the mode matching the journals it collects.
      (if (eq (gco-pkm-format-for-dir journal-dir) 'md)
          (progn (when (fboundp 'markdown-mode) (markdown-mode))
                 (insert (format "# Recent Journal Entries (%d)\n\n" n)))
        (org-mode)
        (insert (format "#+title: Recent Journal Entries (%d)\n\n" n)))
      (dolist (file recent-files)
        (let* ((base (file-name-base file))
               (start (point))
               (content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
          (insert (format "%s Journal for %s (click to open)"
                          (if (eq (gco-pkm-format-for-dir journal-dir) 'md) "#" "*")
                          base))
          (make-text-button
           start (point)
           'help-echo "RET or click to open this journal"
           'mouse-face 'highlight
           'follow-link t
           'action (lambda (_btn) (find-file file)))
          (insert "\n")
          ;; Insert top-level headings from the file (plain text search, no org-mode needed)
          (with-temp-buffer
            (insert content)
            (goto-char (point-min))
            (while (re-search-forward "^\\(?:\\*\\|#\\) " nil t)
              (let ((hl-start (line-beginning-position))
                    (hl-end (or (save-excursion
                                  (and (re-search-forward "^\\(?:\\*\\|#\\) " nil t)
                                       (line-beginning-position)))
                                (point-max))))
                (insert-into-buffer buffer hl-start hl-end)
                (with-current-buffer buffer (insert "\n")))))))
      ;; "More" button
      (let ((start (point)))
        (insert (format "\nShow %d more..." (+ n 7)))
        (make-text-button
         start (point)
         'help-echo "RET or click to show more history"
         'mouse-face 'highlight
         'follow-link t
         'action (lambda (_btn) (gco-pkm-show-recent-journals (+ n 7)))))
      (goto-char (point-min))
      (when (derived-mode-p 'org-mode) (org-hide-drawer-all))
      (view-mode 1))
    (switch-to-buffer buffer)))


;;;; Page Creation and Management

;;;###autoload
(defun gco-pkm-create-tag-page (tag)
  "Create a dynamic tag page for TAG."
  (interactive "sTag: ")
  (let* ((fmt (gco-pkm-format-for-dir gco-pkm-directory))
         (filename (expand-file-name
                    (format "tag-%s.%s" tag (gco-pkm-format-extension fmt))
                    gco-pkm-directory)))
    (find-file filename)
    (when (= (buffer-size) 0)
      (if (eq fmt 'md)
          ;; No dblock equivalent in markdown; the tag itself is the query,
          ;; and `gco-pkm-search-tag' lists live hits.
          (insert (format "---\ntitle: \"#%s\"\nid: %s\n---\n\nNotes tagged #%s. Use `gco-pkm-search-tag' for the live list.\n\n"
                          tag (upcase (org-id-new)) tag))
        (insert (format "#+title: #%s\n\n" tag))
        (when (fboundp 'org-ql)
          (insert (format "#+BEGIN: org-ql :query (tags \"%s\")\n\n#+END:\n\n" tag))
          (insert "Press C-c C-c on the block above to refresh.\n"))
        (gco-pkm-format-ensure-file-id))
      (save-buffer))))

;;;###autoload
(defun gco-pkm-search-tag (tag)
  "List notes carrying inline #TAG, in either format."
  (interactive "sTag: ")
  (if (fboundp 'consult-ripgrep)
      (consult-ripgrep gco-pkm-directory (format "#%s\\b" tag))
    (rgrep (format "#%s" tag) "*.org *.md" gco-pkm-directory)))

;;;; Block References

;;;###autoload
(defun gco-pkm--anchor-slug (text)
  "Return the `^anchor' slug the converter would give heading TEXT."
  (let* ((s (downcase (string-trim text)))
         (s (replace-regexp-in-string "\\[\\([^]]*\\)\\]([^)]*)" "\\1" s))
         (s (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" s))
         (s (replace-regexp-in-string "[[:space:]_-]+" "-" s)))
    (substring (string-trim s "-" "-") 0 (min 48 (length (string-trim s "-" "-"))))))

;;;###autoload
(defun gco-pkm-create-block-reference ()
  "Copy a link to the current heading.
In org this mints an :ID: and copies an id: link.  In markdown it adds a
`^anchor' to the heading -- the same convention the converter emits -- and
copies a relative link to it."
  (interactive)
  (pcase (gco-pkm-format-current)
    ('org
     (let ((id (org-id-get-create)))
       (kill-new (format "[[id:%s]]" id))
       (message "Block reference copied: [[id:%s]]" id)))
    ('md
     (save-excursion
       (goto-char (gco-pkm-format-heading-start))
       (unless (looking-at (gco-pkm-format-heading-regexp))
         (user-error "Point is not under a heading"))
       (let* ((text (match-string 2))
              (existing (and (string-match "\\^\\([[:alnum:]_-]+\\)[ \t]*$" text)
                             (match-string 1 text)))
              (anchor (or existing
                          (gco-pkm--anchor-slug
                           (gco-pkm-format-heading-text text)))))
         (unless existing
           (end-of-line)
           (insert " ^" anchor))
         (let ((link (format "[%s](%s#^%s)"
                             (gco-pkm-format-heading-text text)
                             (file-name-nondirectory (buffer-file-name))
                             anchor)))
           (kill-new link)
           (message "Block reference copied: %s" link)))))
    (_ (user-error "Not a PKM note buffer"))))

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
(defun gco-pkm--file-title (file)
  "Return FILE's note title, falling back to its basename.
Reads only the head of the file, so scanning the whole PKM is cheap."
  (or (with-temp-buffer
        (insert-file-contents file nil 0 2048)
        (setq buffer-file-name file)
        (prog1 (ignore-errors (gco-pkm-format-buffer-title))
          (setq buffer-file-name nil)))
      (file-name-base file)))

(defun gco-pkm--all-notes ()
  "Return every note file under `gco-pkm-directory', newest first.
Scans the directory rather than consulting org-mem, which only indexes
org files and would go blind to the markdown half of the corpus."
  (sort (directory-files-recursively
         gco-pkm-directory gco-pkm-format-file-regexp nil
         (lambda (dir) (not (string-prefix-p "." (file-name-nondirectory dir)))))
        (lambda (a b)
          (time-less-p (file-attribute-modification-time (file-attributes b))
                       (file-attribute-modification-time (file-attributes a))))))

;;;###autoload
(defun gco-pkm-find-page ()
  "Find any note file by title, in either format.
Lists every note under `gco-pkm-directory' by its title (or basename),
newest first.  If the input matches no existing page, create one."
  (interactive)
  (let ((by-title (make-hash-table :test #'equal))
        titles)
    (dolist (file (gco-pkm--all-notes))
      (let ((title (gco-pkm--file-title file)))
        (unless (gethash title by-title)   ; keep the newest of a duplicate title
          (puthash title file by-title)
          (push title titles))))
    (setq titles (nreverse titles))
    (let* ((choice (completing-read
                    "Page: "
                    (lambda (str pred action)
                      (if (eq action 'metadata)
                          '(metadata (display-sort-function . identity)
                                     (cycle-sort-function . identity))
                        (complete-with-action action titles str pred)))))
           (file (gethash choice by-title)))
      (if file
          (find-file file)
        (gco-pkm-create-page choice)))))

;;;###autoload
(defun gco-pkm-create-page (title)
  "Create a new page called TITLE, in the PKM's current format."
  (interactive "sPage title: ")
  (let* ((fmt (gco-pkm-format-for-dir gco-pkm-directory))
         (slug (replace-regexp-in-string
                "[^[:alnum:]]+" "-" (downcase (string-trim title))))
         (file (expand-file-name
                (format "%s.%s" (string-trim slug "-" "-")
                        (gco-pkm-format-extension fmt))
                gco-pkm-directory)))
    (find-file file)
    (when (= (buffer-size) 0)
      (if (eq fmt 'md)
          (insert (format "---\ntitle: \"%s\"\nid: %s\n---\n\n"
                          title (upcase (org-id-new))))
        (insert (format "#+title: %s\n\n" title))
        (gco-pkm-format-ensure-file-id))
      (save-buffer))
    (goto-char (point-max))))

;;;###autoload
(defun gco-pkm-search-todos ()
  "Search for open TODO items across the PKM, in either format.
Unchecked checkboxes are the corpus's actual convention (org TODO
keywords appear in a handful of files), and they read the same in both
formats, so a ripgrep over them covers everything.  Falls back to
`org-ql-search' only when consult is unavailable."
  (interactive)
  (cond
   ((fboundp 'consult-ripgrep)
    (consult-ripgrep gco-pkm-directory "^\\s*[-+*] \\[ \\]|^\\*+ TODO "))
   ((fboundp 'org-ql-search)
    (org-ql-search gco-pkm-directory '(todo)))
   (t (org-todo-list))))

;;;###autoload
(defun gco-pkm-recent-files (&optional num)
  "Show NUM recently modified org files (default 20)."
  (interactive)
  (let* ((num (or num 20))
         (files (directory-files-recursively gco-pkm-directory gco-pkm-format-file-regexp nil))
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

(defun gco-pkm-ensure-file-id ()
  "Give the current note file a file-level ID if it has none.
For `before-save-hook': makes every note saved under
`gco-pkm-directory' a full org-node citizen (findable, linkable,
backlink-capable).  Journal files are skipped since their IDs
belong on day headings."
  (when (and (derived-mode-p 'org-mode)
             buffer-file-name
             (file-in-directory-p buffer-file-name gco-pkm-directory)
             (not (file-in-directory-p buffer-file-name (gco-pkm-journal-dir))))
    (org-with-wide-buffer
     (goto-char (point-min))
     (gco-pkm-format-ensure-file-id))))

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

;; Register query link type; gco-pkm-consult overrides :follow if loaded
(org-link-set-parameters "query"
                         :follow #'gco-pkm-query-link-follow
                         :export #'gco-pkm-query-link-export
                         :face '(:foreground "purple" :underline t))
;; If consult integration was loaded (require'd above), let it override the follow handler
(when (fboundp 'gco-pkm-query-link-follow-consult)
  (org-link-set-parameters "query"
                           :follow #'gco-pkm-query-link-follow-consult))


;;;; Setup

;;;###autoload
(defun gco-pkm-setup ()
  "Set up PKM system."
  (interactive)
  ;; Ensure directory exists
  (unless (file-exists-p gco-pkm-directory)
    (make-directory gco-pkm-directory t))

  ;; Set up auto-commit hook if enabled
  (when gco-pkm-auto-commit
    (add-hook 'after-save-hook #'gco-pkm-auto-commit))

  ;; Every saved note gets an ID, so org-node can always find and link it
  (add-hook 'before-save-hook #'gco-pkm-ensure-file-id)

  (message "GCO PKM system initialized in %s" gco-pkm-directory))

(provide 'gco-pkm)
;;; gco-pkm.el ends here
