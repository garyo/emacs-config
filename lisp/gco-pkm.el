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
DATE is a list (month day year) as used by calendar functions."
  (let* ((date (or date (calendar-current-date)))
         (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
         (fname (format-time-string "%Y-%m-%d.org" time))) ; YYYY-MM-DD
    (expand-file-name fname (gco-pkm-journal-dir))))

;;;###autoload
(defun gco-pkm-journal-today ()
  "Open today's daily journal file, creating it if necessary."
  (interactive)
  (let ((file (gco-pkm-journal--path-for-date)))
    (find-file file)
    (when (= (buffer-size) 0)
      (insert (format "#+title: %s\n\n* <%s>\n"
                      (format-time-string "%Y-%m-%d")
                      (format-time-string "%Y-%m-%d %a")))
      (org-id-get-create)
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
  (let* ((files (directory-files (gco-pkm-journal-dir) nil "\\.org$"))
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
         (files (sort (directory-files journal-dir t "\\.org$") #'string>))
         (recent-files (seq-take files n))
         (buffer (get-buffer-create "*Recent Journals*")))
    (with-current-buffer buffer
      (setq default-directory gco-pkm-directory)
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (insert (format "#+title: Recent Journal Entries (%d)\n\n" n))
      (dolist (file recent-files)
        (let* ((base (file-name-base file))
               (start (point))
               (content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
          (insert (format "* Journal for %s (click to open)" base))
          (make-text-button
           start (point)
           'help-echo "RET or click to open this journal"
           'mouse-face 'highlight
           'follow-link t
           'action (lambda (_btn) (find-file file)))
          (insert "\n")
          ;; Insert top-level headings from the file
          (with-temp-buffer
            (insert content)
            (org-mode)
            (goto-char (point-min))
            (while (re-search-forward "^\\* " nil t)
              (let ((hl-start (line-beginning-position))
                    (hl-end (or (save-excursion
                                  (and (re-search-forward "^\\* " nil t)
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
      (org-hide-drawer-all)
      (view-mode 1))
    (switch-to-buffer buffer)))


;;;; Page Creation and Management

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

  ;; Set up auto-commit hook if enabled
  (when gco-pkm-auto-commit
    (add-hook 'after-save-hook #'gco-pkm-auto-commit))

  (message "GCO PKM system initialized in %s" gco-pkm-directory))

(provide 'gco-pkm)
;;; gco-pkm.el ends here
