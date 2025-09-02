;;; gco-inline-tags.el --- Inline #tag UI for Org -*- lexical-binding: t; -*-

;; Author: Gary Oberbrunner
;; Version: 0.2
;; Package-Requires: ((emacs "27.1") (org "9"))
;; Keywords: outlines, convenience, org, pkm
;; URL: https://github.com/garyo/emacs-config

;;; Commentary:
;; - Fontifies inline #tags (e.g., "#emacs") in Org buffers.
;; - RET or click on a tag triggers search (Consult UI if available).
;; - `gco-inline-tags-search` offers completion of known tags via ripgrep.
;; - Uses PCRE2 lookarounds for tight boundary matching.

;;; Code:

(require 'org)
(require 'org-element)
(require 'subr-x)

;;;; Customization

(defgroup gco-inline-tags nil
  "Inline #tags UI for Org."
  :group 'org
  :prefix "gco-inline-tags-")

(defface gco-inline-tags-face
  '((t :inherit link))
  "Face used to display inline #tags."
  :group 'gco-inline-tags)

(defcustom gco-inline-tags-rg-exe "rg"
  "Path to ripgrep executable."
  :type 'string
  :group 'gco-inline-tags)

(defcustom gco-inline-tags-roots
  (list (or (bound-and-true-p org-directory) default-directory))
  "Directories searched for inline tags."
  :type '(repeat directory)
  :group 'gco-inline-tags)

(defcustom gco-inline-tags-collect-rg-args
  '("--only-matching" "--no-filename" "-N" "--glob" "*.org")
  "Extra ripgrep args used when collecting tag names."
  :type '(repeat string)
  :group 'gco-inline-tags)

(defcustom gco-inline-tags-search-rg-args
  '("--null" "--line-buffered" "--color=never"
    "--max-columns=1000" "--path-separator=/" "--smart-case"
    "--no-heading" "--with-filename" "--line-number" "--pcre2"
    "--glob" "*.org")
  "Extra ripgrep args used for interactive searches."
  :type '(repeat string)
  :group 'gco-inline-tags)

(defcustom gco-inline-tags-ignore-in-src-and-example t
  "When non-nil, do not fontify inline tags inside src/example blocks."
  :type 'boolean
  :group 'gco-inline-tags)

;;;; Regex utilities

;; Font-lock regexp: group 1 = full #tag, group 2 = tag text
(defconst gco-inline-tags--re
  "\\(?:^\\|[^[:alnum:]_]\\)\\(#\\([[:alnum:]_:-]+\\)\\)")

(defun gco-inline-tags--pcre-pattern (tag)
  "PCRE2 pattern matching inline #TAG with tight boundaries."
  (format "(?<![[:alnum:]_])#%s(?![[:alnum:]_:-])" (regexp-quote tag)))

(defun gco-inline-tags--rg-command (pat &optional for-collect)
  "Build ripgrep command list for PAT.
If FOR-COLLECT is non-nil, use collector args."
  (append (list gco-inline-tags-rg-exe
                "-P" "-e" pat)
          (if for-collect
              gco-inline-tags-collect-rg-args
            gco-inline-tags-search-rg-args)
          gco-inline-tags-roots))

;;;; Fontification helpers

(defun gco-inline-tags--skip-directive-at (pos)
  "Return non-nil if text at POS begins an Org #+DIRECTIVE."
  (save-excursion
    (goto-char pos)
    (looking-at (rx "#+" (+ (any "A-Z_")) ":"))))

(defun gco-inline-tags--in-ignored-block-p ()
  "Return non-nil if point is inside a block we should ignore."
  (when gco-inline-tags-ignore-in-src-and-example
    (let ((el (org-element-at-point)))
      (memq (org-element-type el)
            '(src-block example-block verse-block code)))))

;;;; Keymap + actions

(defvar gco-inline-tags--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gco-inline-tags-act-at-point)
    (define-key map [mouse-1]  #'gco-inline-tags-act-at-point)
    map)
  "Keymap put on inline #tag text.")

(defun gco-inline-tags--tag-at (pos)
  "Return tag (string, no leading '#') at POS, or nil."
  (save-excursion
    (goto-char pos)
    (when (or (looking-at gco-inline-tags--re)
              (and (> pos (point-min))
                   (goto-char (1- pos))
                   (looking-at gco-inline-tags--re)))
      (match-string-no-properties 2)))) ;; group 2 = tag only

;;;###autoload
(defun gco-inline-tags-act-at-point (&optional pos)
  "Action for tag at POS/point: open a search for that tag."
  (interactive)
  (let* ((p (or pos (point)))
         (tag (or (get-text-property p 'gco-inline-tag)
                  (gco-inline-tags--tag-at p))))
    (if tag
        (gco-inline-tags-search tag)
      (message "No inline #tag here"))))

;;;; Ripgrep helpers

(defun gco-inline-tags--collect-tags ()
  "Return unique sorted list of inline tags (no leading '#')."
  (when (executable-find gco-inline-tags-rg-exe)
    (let* ((pat "(?<=#)([[:alnum:]_:-]+)") ;; capture group
           (cmd (gco-inline-tags--rg-command pat t))
           (out (with-temp-buffer
                  (apply #'process-file (car cmd) nil t nil (cdr cmd))
                  (buffer-string))))
      (sort (delete-dups (split-string out "\n" t)) #'string-lessp))))

;;;###autoload
(defun gco-inline-tags-read (&optional prompt)
  "Read an inline tag with completion using optional PROMPT."
  (let* ((prompt (or prompt "Inline tag: #"))
         (cands  (or (gco-inline-tags--collect-tags) '())))
    (if cands
        (completing-read prompt cands nil nil)
      (read-string prompt))))

;;; Tag search utilities
(defun gco-inline-tags--direct-ripgrep (command title)
  "Run ripgrep with a pre-built COMMAND using consult's formatting. Use TITLE for heading."
  (let ((builder (lambda (_input) (cons command nil))))
    ;; This uses internal consult functions. I'd love to use only public ones
    ;; but there's no public way to pass a direct PCRE regex to ripgrep
    ;; that I can find.
    (unless (and (fboundp 'consult--read) (fboundp 'consult--async-pipeline))
      (error "Required consult internal functions not available"))
    (consult--read
     (consult--async-pipeline
      (consult--async-min-input 0)
      (consult--async-process builder)
      (consult--grep-format (lambda (_) nil)))
     :prompt title
     :lookup #'consult--lookup-member
     :state (consult--grep-state)
     :require-match t
     :category 'consult-grep
     :group #'consult--prefix-group
     :sort nil
     :initial "#"))) ; Start with # to trigger immediately

(defun gco-inline-tags--consult-ripgrep-search (pattern title)
  "Search for PATTERN with \"consult-ripgrep\", using TITLE for search results title."
  (let ((cmd `(,gco-inline-tags-rg-exe
               ,@gco-inline-tags-search-rg-args
               "-e" ,pattern
               ,@gco-inline-tags-roots)))
    (gco-inline-tags--direct-ripgrep cmd title)))

;;; Example usage:
;; To test low-level ripgrep search:
;; (gco-inline-tags--consult-ripgrep-search "(?<![[:alnum:]])#emacs(?![[:alnum:]])" "Results:")

;;;###autoload
(defun gco-inline-tags-search (&optional tag)
  "Search for inline #TAG. With Consult live preview if available."
  (interactive)
  (let* ((tag (or tag (gco-inline-tags-read)))
         (pat (gco-inline-tags--pcre-pattern tag)))
    (cond
     ;; Preferred: Consult async UI with raw rg command
     ((and (featurep 'consult) (fboundp 'consult--grep))
      (gco-inline-tags--consult-ripgrep-search pat (format "Search results for %s: " tag)))
     ;; Fallback: plain rg via compilation
     (t
      (let ((cmd (mapconcat #'shell-quote-argument
                            (gco-inline-tags--rg-command pat nil) " ")))
        (compilation-start cmd 'grep-mode))))))

;;;###autoload
(defun gco-inline-tags-insert (tag)
  "Insert an inline #TAG (with completion)."
  (interactive (list (gco-inline-tags-read "Insert inline tag: #")))
  (insert "#" tag))

;;;; Minor mode (font-lock integration)

;;;###autoload
(define-minor-mode gco-inline-tags-mode
  "Fontify and interact with inline #tags in Org."
  :lighter " #tag"
  (if gco-inline-tags-mode
      (progn
        (font-lock-add-keywords
         nil
         `((,gco-inline-tags--re
            (0
             (let* ((beg (match-beginning 1))
                    (end (match-end 1))
                    (tag (match-string-no-properties 2)))
               (unless (or (gco-inline-tags--skip-directive-at beg)
                           (save-excursion
                             (goto-char beg)
                             (gco-inline-tags--in-ignored-block-p)))
                 (add-text-properties
                  beg end
                  `(face gco-inline-tags-face
                         mouse-face highlight
                         gco-inline-tag ,tag
                         help-echo ,(format "Inline tag: #%s (RET/click to search)" tag)
                         keymap ,gco-inline-tags--keymap))))
             nil))))
        (font-lock-flush) (font-lock-ensure))
    (font-lock-remove-keywords nil `((,gco-inline-tags--re (0 nil))))
    (font-lock-flush) (font-lock-ensure)))

;;;###autoload
(define-globalized-minor-mode gco-inline-tags-global-mode
  gco-inline-tags-mode
  (lambda () (when (derived-mode-p 'org-mode) (gco-inline-tags-mode 1))))

(provide 'gco-inline-tags)
;;; gco-inline-tags.el ends here
