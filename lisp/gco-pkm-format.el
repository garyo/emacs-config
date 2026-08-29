;;; gco-pkm-format.el --- Format primitives for the PKM -*- lexical-binding: t; -*-

;; Author: Gary Oberbrunner
;; Version: 0.1
;; Keywords: outlines, convenience, org, markdown, pkm

;;; Commentary:
;; The PKM is being converted from org-mode to CommonMark + YAML frontmatter.
;; Both formats coexist while that happens, and the markdown corpus is the
;; destination.  Everything that differs between the two lives here, so the
;; rest of gco-pkm can be written once:
;;
;;   org                          markdown
;;   ---------------------------  -----------------------------------
;;   * Heading                    # Heading
;;   #+title: Foo                 --- / title: Foo / ---  (frontmatter)
;;   :PROPERTIES: :ID: … :END:    id: … in frontmatter
;;   [[file:x.org][desc]]         [desc](x.md)
;;
;; Which format a *new* file gets follows whatever the target directory
;; already holds, so the cutover needs no flag day here either.

;;; Code:

(require 'subr-x)
(require 'org-id)   ; org-id-new: UUID minting, needed for both formats

(defgroup gco-pkm-format nil
  "Note format handling for the PKM."
  :group 'gco-pkm
  :prefix "gco-pkm-format-")

(defconst gco-pkm-format-file-regexp "\\.\\(org\\|md\\)\\'"
  "Matches any note file, in either format.")

(defun gco-pkm-format-of (file)
  "Return `org' or `md' for FILE, based on its extension."
  (pcase (and file (file-name-extension file))
    ("org" 'org)
    ("md" 'md)
    (_ nil)))

(defun gco-pkm-format-current ()
  "Return the note format of the current buffer, or nil."
  (or (gco-pkm-format-of (buffer-file-name))
      (cond ((derived-mode-p 'org-mode) 'org)
            ((derived-mode-p 'markdown-mode) 'md))))

(defun gco-pkm-format-note-buffer-p ()
  "Non-nil when the current buffer is a note in either format."
  (and (gco-pkm-format-current) t))

(defun gco-pkm-format-for-dir (dir)
  "Return the format new notes in DIR should use.
Follows whatever DIR already contains, so this tracks the conversion
without needing to be switched by hand.  Markdown wins a tie, since
that is where the corpus is heading."
  (if (and (file-directory-p dir)
           (directory-files dir nil "\\.md\\'" t))
      'md
    (if (and (file-directory-p dir)
             (directory-files dir nil "\\.org\\'" t))
        'org
      'md)))

(defun gco-pkm-format-extension (format)
  "File extension string for FORMAT."
  (if (eq format 'md) "md" "org"))

;;;; Headings

(defun gco-pkm-format-heading-regexp (&optional format)
  "Regexp matching a heading line in FORMAT (default: current buffer)."
  (if (eq (or format (gco-pkm-format-current)) 'md)
      "^\\(#+\\)[ \t]+\\(.*\\)$"
    "^\\(\\*+\\)[ \t]+\\(.*\\)$"))

(defun gco-pkm-format-any-heading-regexp ()
  "Regexp matching a heading line in either format."
  "^\\(\\*+\\|#+\\)[ \t]+\\(.*\\)$")

(defun gco-pkm-format-heading-text (raw &optional format)
  "Strip FORMAT-specific decoration from heading text RAW.
Removes org trailing :tags: and the markdown `^anchor' link target,
so the two formats yield comparable text."
  (let ((s (string-trim raw)))
    (if (eq (or format (gco-pkm-format-current)) 'md)
        (replace-regexp-in-string "[ \t]+\\^[[:alnum:]_-]+\\'" "" s)
      (replace-regexp-in-string "[ \t]+:[[:alnum:]_@#%:]+:\\'" "" s))))

(defun gco-pkm-format-current-heading ()
  "Return the enclosing heading's text, or nil.
Works in both formats without requiring org-mode to be active."
  (save-excursion
    (let ((re (gco-pkm-format-heading-regexp)))
      (beginning-of-line)
      (if (or (looking-at re) (re-search-backward re nil t))
          (gco-pkm-format-heading-text (match-string 2))
        nil))))

(defun gco-pkm-format-heading-start ()
  "Return point at the start of the enclosing heading, or `point-min'."
  (save-excursion
    (let ((re (gco-pkm-format-heading-regexp)))
      (beginning-of-line)
      (if (or (looking-at re) (re-search-backward re nil t))
          (line-beginning-position)
        (point-min)))))

;;;; Titles and IDs

(defun gco-pkm-format-buffer-title ()
  "Return the current buffer's note title, or nil.
Reads `#+title:' in org; in markdown, the frontmatter `title:', falling
back to the first level-1 heading, which is the de facto title of the
many notes that carry no frontmatter."
  (save-excursion
    (goto-char (point-min))
    (if (eq (gco-pkm-format-current) 'md)
        (or (when (looking-at "^---[ \t]*$")
              (let ((end (save-excursion
                           (forward-line 1)
                           (and (re-search-forward "^---[ \t]*$" nil t) (point)))))
                (when (and end (re-search-forward "^title:[ \t]*\\(.*\\)$" end t))
                  (string-trim (match-string 1) "\"" "\""))))
            (progn
              (goto-char (point-min))
              (when (re-search-forward "^#[ \t]+\\(.+\\)$" nil t)
                (string-trim (match-string 1)))))
      (when (re-search-forward "^#\\+title:[ \t]*\\(.*\\)$" nil t)
        (string-trim (match-string 1))))))

(defun gco-pkm-format--frontmatter-bounds ()
  "Return (START . END) of the markdown frontmatter block, or nil.
END is the position just after the closing delimiter line."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^---[ \t]*$")
      (let ((start (point)))
        (forward-line 1)
        (when (re-search-forward "^---[ \t]*$" nil t)
          (cons start (line-beginning-position 2)))))))

(defun gco-pkm-format-file-id ()
  "Return the current buffer's file-level ID, or nil."
  (if (eq (gco-pkm-format-current) 'md)
      (when-let* ((bounds (gco-pkm-format--frontmatter-bounds)))
        (save-excursion
          (goto-char (car bounds))
          (when (re-search-forward "^id:[ \t]*\\(\\S-+\\)[ \t]*$" (cdr bounds) t)
            (match-string 1))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
        (match-string 1)))))

(defun gco-pkm-format-ensure-file-id ()
  "Ensure the current buffer has a file-level ID; return it.
In org this defers to `org-id-get-create'.  In markdown the id lives in
frontmatter, which is created if the buffer has none."
  (if (eq (gco-pkm-format-current) 'md)
      (or (gco-pkm-format-file-id)
          (let ((id (upcase (org-id-new))))
            (save-excursion
              (if-let* ((bounds (gco-pkm-format--frontmatter-bounds)))
                  (progn (goto-char (cdr bounds))
                         (forward-line -1)
                         (insert (format "id: %s\n" id)))
                (goto-char (point-min))
                (insert (format "---\ntitle: \"%s\"\nid: %s\n---\n\n"
                                (file-name-base (or (buffer-file-name) "untitled"))
                                id))))
            id))
    (save-excursion
      (goto-char (point-min))
      (org-id-get-create))))

(provide 'gco-pkm-format)
;;; gco-pkm-format.el ends here
