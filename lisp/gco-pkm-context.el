;;; gco-pkm-context.el --- Live context sidebar for PKM -*- lexical-binding: t; -*-

;; Author: Gary Oberbrunner
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (gco-pkm "0.1") (gco-inline-tags "0.2"))
;; Keywords: outlines, convenience, org, pkm

;;; Commentary:
;; A side window that surfaces material related to the heading at point:
;;   - identical headings elsewhere in the PKM,
;;   - other headings sharing significant words with the current one,
;;   - inline #tags found within a few lines of point.
;;
;; Results are sorted latest-first (by date in filename, falling back to
;; mtime) and deduplicated.  The sidebar refreshes after a short idle
;; delay only when the heading or nearby tag set actually changes, so it
;; is cheap to keep on while editing.
;;
;; Navigation: focus the side window with `C-x o' and use `n'/`p' to
;; walk entries.  Each move previews the source location in the editing
;; window without stealing focus (consult-style).  `RET' commits the
;; jump for real.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'seq)
(require 'text-property-search)
(require 'gco-pkm)
(require 'gco-inline-tags)

;;;; Customization

(defgroup gco-pkm-context nil
  "Live context sidebar for PKM."
  :group 'gco-pkm
  :prefix "gco-pkm-context-")

(defcustom gco-pkm-context-idle-delay 0.5
  "Seconds of idle time before recomputing context."
  :type 'number)

(defcustom gco-pkm-context-max-results 30
  "Maximum number of entries to display."
  :type 'integer)

(defcustom gco-pkm-context-tag-window-lines 5
  "Number of lines around point scanned for inline #tags."
  :type 'integer)

(defcustom gco-pkm-context-min-word-length 3
  "Minimum length for a heading word to be considered significant."
  :type 'integer)

(defcustom gco-pkm-context-stop-words
  '("about" "after" "again" "against" "also" "another" "because" "been"
    "before" "being" "between" "both" "could" "does" "doing" "done"
    "during" "each" "either" "every" "from" "further" "have" "having"
    "here" "into" "just" "like" "more" "most" "much" "myself" "only"
    "other" "ours" "over" "same" "should" "some" "such" "than" "that"
    "their" "them" "then" "there" "these" "they" "this" "those" "through"
    "today" "very" "was" "were" "what" "when" "where" "which" "while"
    "with" "would" "your"
    "todo" "done" "waiting" "note"
    "monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday"
    "january" "february" "march" "april" "june" "july" "august"
    "september" "october" "november" "december")
  "Words excluded from heading word-overlap matching."
  :type '(repeat string))

(defcustom gco-pkm-context-stop-headings
  '("Today" "Yesterday" "Tomorrow" "Notes" "TODO" "Tasks" "Log")
  "Headings whose exact match should be suppressed.
A heading whose trimmed text matches one of these (case-insensitively)
will not generate `exact' search patterns, so duplicate `** Today'
entries don't fill the sidebar.  Word and tag matching are unaffected."
  :type '(repeat string))

(defcustom gco-pkm-context-side 'right
  "Side of the frame on which to display the context buffer."
  :type '(choice (const left) (const right) (const top) (const bottom)))

(defcustom gco-pkm-context-window-width 50
  "Width in columns for the context side window."
  :type 'integer)

(defcustom gco-pkm-context-rg-args
  '("--no-heading" "--with-filename" "--line-number"
    "--color=never" "--max-columns=500" "--max-columns-preview"
    "--pcre2" "-A" "1" "--glob" "*.org")
  "Ripgrep arguments for context queries."
  :type '(repeat string))

;;;; Faces

(defface gco-pkm-context-exact-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the [exact] badge.")

(defface gco-pkm-context-word-face
  '((t :inherit font-lock-variable-name-face))
  "Face for the [word] badge.")

(defface gco-pkm-context-tag-face
  '((t :inherit gco-inline-tags-face :weight bold))
  "Face for the [#tag] badge.")

(defface gco-pkm-context-date-face
  '((t :inherit font-lock-comment-face))
  "Face for the date column.")

(defface gco-pkm-context-path-face
  '((t :inherit link))
  "Face for the file:line path column.")

(defface gco-pkm-context-heading-face
  '((t :inherit org-level-3 :weight bold))
  "Face for the heading line in a snippet.")

(defface gco-pkm-context-snippet-face
  '((t :inherit shadow))
  "Face for the trailing context line.")

;;;; Internal state

(defconst gco-pkm-context--buffer-name "*PKM Context*")

(defvar gco-pkm-context--proc nil
  "In-flight ripgrep process, or nil.")

(defvar gco-pkm-context--last-key nil
  "Last `(file heading sorted-tags)' key for which results were rendered.")

(defvar gco-pkm-context--idle-timer nil)

(defvar gco-pkm-context--source-window nil
  "Window the user is editing in; preview targets this when live.")

(defvar-local gco-pkm-context--last-preview-pos nil
  "Buffer-local: last point at which we previewed a source location.")

;;;; Heading & tag extraction (sync)

(defun gco-pkm-context--in-pkm-buffer-p (&optional buf)
  "Non-nil if BUF (or current buffer) visits a file under `gco-pkm-directory'."
  (let* ((buf (or buf (current-buffer)))
         (file (buffer-file-name buf)))
    (and file
         (string-prefix-p (file-name-as-directory
                           (expand-file-name gco-pkm-directory))
                          (expand-file-name file)))))

(defun gco-pkm-context--current-heading-text ()
  "Return cleaned heading text at point, or the file `#+title:' if none.
Returns nil if no usable text is available."
  (when (derived-mode-p 'org-mode)
    (or (ignore-errors
          (save-excursion
            (org-back-to-heading t)
            (let ((h (org-get-heading t t t t)))
              (and h (not (string-empty-p (string-trim h)))
                   (string-trim h)))))
        (let ((kw (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))))
          (and kw (not (string-empty-p (string-trim kw)))
               (string-trim kw))))))

(defun gco-pkm-context--timestamp-only-p (heading)
  "Non-nil if HEADING is just an org timestamp like `<2026-05-09 Sat>'."
  (and heading
       (string-match-p
        "\\`<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [A-Za-z]\\{3\\}\\)?\\(?: [0-9:]+\\)?>\\'"
        heading)))

(defun gco-pkm-context--significant-words (heading)
  "Return list of significant lowercased words in HEADING."
  (when heading
    (let* ((tokens (split-string (downcase heading)
                                 "[^[:alnum:]_-]+" t))
           (stops gco-pkm-context-stop-words))
      (cl-delete-duplicates
       (cl-remove-if
        (lambda (w)
          (or (< (length w) gco-pkm-context-min-word-length)
              (member w stops)
              (string-match-p "\\`[0-9]+\\'" w)))
        tokens)
       :test #'string=))))

(defun gco-pkm-context--nearby-tags ()
  "Return sorted list of inline tags within a window around point."
  (when (derived-mode-p 'org-mode)
    (let* ((lines gco-pkm-context-tag-window-lines)
           (start (save-excursion
                    (forward-line (- lines))
                    (line-beginning-position)))
           (end (save-excursion
                  (forward-line lines)
                  (line-end-position)))
           (tags '()))
      (save-excursion
        (goto-char start)
        (while (re-search-forward gco-inline-tags--re end t)
          (push (match-string-no-properties 2) tags)))
      (sort (cl-delete-duplicates tags :test #'string=) #'string<))))

(defun gco-pkm-context--key (file heading words tags)
  "Build a change-detection key tuple."
  (list file heading words tags))

;;;; Pattern construction

(defun gco-pkm-context--pcre-quote (s)
  "Wrap S in PCRE2 \\Q...\\E for literal matching.
Any literal `\\E' in S is broken across two quoted spans."
  (concat "\\Q"
          (replace-regexp-in-string "\\\\E" "\\E\\E\\Q" s nil t)
          "\\E"))

(defun gco-pkm-context--exact-pattern (heading)
  "PCRE2 pattern matching a heading line whose text equals HEADING."
  (format "(?:^\\*+\\s+(?:TODO\\s+|DONE\\s+|WAITING\\s+|NOTE\\s+)?%s\\s*$)"
          (gco-pkm-context--pcre-quote heading)))

(defun gco-pkm-context--word-pattern (word)
  "PCRE2 pattern matching a heading line containing WORD on a word boundary."
  (format "(?:^\\*+\\s+.*\\b%s\\b.*$)"
          (gco-pkm-context--pcre-quote word)))

(defun gco-pkm-context--tag-pattern (tag)
  "PCRE2 pattern matching inline #TAG with tight boundaries."
  (format "(?:%s)" (gco-inline-tags--pcre-pattern tag)))

(defun gco-pkm-context--tag-classifier-re (tag)
  "Emacs regex matching an inline `#TAG' occurrence."
  (concat "\\(?:^\\|[^[:alnum:]_]\\)#" (regexp-quote tag)
          "\\(?:[^[:alnum:]_:-]\\|$\\)"))

(defun gco-pkm-context--heading-word-classifier-re (word)
  "Emacs regex matching a heading line containing WORD on word boundaries."
  (concat "^\\*+\\s-+.*\\b" (regexp-quote word) "\\b"))

(defun gco-pkm-context--build-pattern (heading words tags)
  "Return (PCRE-STRING . CLASSIFIERS) for the combined query.
Each nearby tag drives both `#tag' matches and headings containing the
tag text; each significant heading word drives both heading matches and
`#word' matches.  CLASSIFIERS is an ordered list of (REGEX . KIND); the
first one whose REGEX matches the hit text wins, so order = priority
(exact > tag > word)."
  (let* ((pieces '())
         (classifiers '())
         (stop-headings (mapcar #'downcase gco-pkm-context-stop-headings)))
    (when (and heading
               (not (gco-pkm-context--timestamp-only-p heading))
               (not (member (downcase heading) stop-headings)))
      (push (gco-pkm-context--exact-pattern heading) pieces)
      (push (cons (concat "^\\*+\\s-+\\(?:TODO\\s-+\\|DONE\\s-+\\|WAITING\\s-+\\|NOTE\\s-+\\)?"
                          (regexp-quote heading) "\\s-*$")
                  'exact)
            classifiers))
    (dolist (tag tags)
      (push (gco-pkm-context--tag-pattern tag) pieces)
      (push (gco-pkm-context--word-pattern tag) pieces)
      (let ((kind (intern (format "tag:%s" tag))))
        (push (cons (gco-pkm-context--tag-classifier-re tag) kind) classifiers)
        (push (cons (gco-pkm-context--heading-word-classifier-re tag) kind)
              classifiers)))
    (dolist (word words)
      (push (gco-pkm-context--word-pattern word) pieces)
      (push (gco-pkm-context--tag-pattern word) pieces)
      (push (cons (gco-pkm-context--heading-word-classifier-re word) 'word)
            classifiers)
      (push (cons (gco-pkm-context--tag-classifier-re word) 'word)
            classifiers))
    (when pieces
      (cons (concat "(?i)"
                    (mapconcat #'identity
                               (delete-dups (nreverse pieces))
                               "|"))
            (nreverse classifiers)))))

;;;; Async rg driver

(defun gco-pkm-context--cancel ()
  "Kill any in-flight ripgrep process."
  (when (and gco-pkm-context--proc
             (process-live-p gco-pkm-context--proc))
    (delete-process gco-pkm-context--proc))
  (setq gco-pkm-context--proc nil))

(defun gco-pkm-context--launch (pattern classifiers root on-done)
  "Run rg asynchronously for PATTERN under ROOT.
CLASSIFIERS is the list to attach for hit classification.  ON-DONE is
called with the parsed entry list when the process exits cleanly."
  (gco-pkm-context--cancel)
  (let* ((bin (or (executable-find gco-inline-tags-rg-exe)
                  (user-error "Ripgrep (`%s') not found" gco-inline-tags-rg-exe)))
         (out-buf (generate-new-buffer " *gco-pkm-context-rg-out*"))
         (cmd (append (list bin "-P" "-e" pattern)
                      gco-pkm-context-rg-args
                      (list (expand-file-name root))))
         proc)
    (setq proc
          (make-process
           :name "gco-pkm-context-rg"
           :buffer out-buf
           :command cmd
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (p _event)
             (when (memq (process-status p) '(exit signal))
               (let ((this-buf (process-buffer p)))
                 (unwind-protect
                     (when (and (eq p gco-pkm-context--proc)
                                (= 0 (or (process-exit-status p) 1))
                                (buffer-live-p this-buf))
                       (let* ((output (with-current-buffer this-buf
                                        (buffer-string)))
                              (entries (gco-pkm-context--parse-rg-output
                                        output classifiers)))
                         (setq gco-pkm-context--proc nil)
                         (funcall on-done entries)))
                   (when (buffer-live-p this-buf)
                     (kill-buffer this-buf))))))))
    (setq gco-pkm-context--proc proc)))

;;;; Output parsing

(defun gco-pkm-context--classify (text classifiers)
  "Return the KIND symbol for TEXT, given CLASSIFIERS in priority order."
  (let ((case-fold-search t)
        (kind nil))
    (cl-loop for (re . k) in classifiers
             when (string-match-p re text)
             do (setq kind k) and return nil)
    (or kind 'word)))

(defun gco-pkm-context--entry-date (file)
  "Return a sortable date for FILE.
Prefers a YYYY-MM-DD prefix in the basename, falls back to mtime."
  (let* ((base (file-name-base file)))
    (if (string-match "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" base)
        (cons :date (match-string 1 base))
      (cons :mtime (file-attribute-modification-time
                    (file-attributes file))))))

(defun gco-pkm-context--date-key (date-cell)
  "Convert a DATE-CELL from `--entry-date' to a comparable Lisp-time."
  (pcase date-cell
    (`(:date . ,s)
     (let ((parts (split-string s "-")))
       (encode-time 0 0 0
                    (string-to-number (nth 2 parts))
                    (string-to-number (nth 1 parts))
                    (string-to-number (nth 0 parts)))))
    (`(:mtime . ,time) time)))

(defun gco-pkm-context--parse-rg-output (output classifiers)
  "Parse OUTPUT from rg into a list of plists."
  (let ((lines (split-string output "\n"))
        (entries '())
        (current nil))
    (dolist (raw lines)
      (cond
       ((string= raw "")
        nil)
       ((string= raw "--")
        (when current
          (push current entries)
          (setq current nil)))
       ((string-match "\\`\\(.+\\):\\([0-9]+\\):\\(.*\\)\\'" raw)
        (when current (push current entries))
        (let* ((file (match-string 1 raw))
               (line (string-to-number (match-string 2 raw)))
               (text (match-string 3 raw))
               (kind (gco-pkm-context--classify text classifiers))
               (date (gco-pkm-context--entry-date file)))
          (setq current (list :file file :line line :match text
                              :context "" :kind kind :date date))))
       ((and current
             (string-match "\\`\\(.+\\)-\\([0-9]+\\)-\\(.*\\)\\'" raw))
        (let ((ctx (match-string 3 raw)))
          (setq current (plist-put current :context ctx))))))
    (when current (push current entries))
    (nreverse entries)))

;;;; Post-processing

(defun gco-pkm-context--filter-self (entries source-file source-line)
  "Drop ENTRIES that point at SOURCE-FILE:SOURCE-LINE."
  (cl-remove-if
   (lambda (e)
     (and source-file source-line
          (equal (expand-file-name (plist-get e :file))
                 (expand-file-name source-file))
          (= (plist-get e :line) source-line)))
   entries))

(defun gco-pkm-context--kind-priority (kind)
  "Lower number = higher priority."
  (cond
   ((eq kind 'exact) 0)
   ((and (symbolp kind) (string-prefix-p "tag:" (symbol-name kind))) 1)
   (t 2)))

(defun gco-pkm-context--dedup (entries)
  "Keep one entry per (file . line); higher-priority KIND wins."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (e entries)
      (let* ((key (cons (plist-get e :file) (plist-get e :line)))
             (existing (gethash key table)))
        (if (or (null existing)
                (< (gco-pkm-context--kind-priority (plist-get e :kind))
                   (gco-pkm-context--kind-priority (plist-get existing :kind))))
            (puthash key e table))))
    (hash-table-values table)))

(defun gco-pkm-context--sort (entries)
  "Sort ENTRIES by date descending, then by file."
  (sort entries
        (lambda (a b)
          (let ((ta (gco-pkm-context--date-key (plist-get a :date)))
                (tb (gco-pkm-context--date-key (plist-get b :date))))
            (cond
             ((time-less-p tb ta) t)
             ((time-less-p ta tb) nil)
             (t (string< (plist-get a :file) (plist-get b :file))))))))

(defun gco-pkm-context--truncate (entries n)
  (if (> (length entries) n) (seq-take entries n) entries))

;;;; Renderer

(defun gco-pkm-context--badge (kind)
  "Return propertized badge string for KIND."
  (cond
   ((eq kind 'exact)
    (propertize "≡" 'face 'gco-pkm-context-exact-face))
   ((and (symbolp kind) (string-prefix-p "tag:" (symbol-name kind)))
    (propertize (format "#%s" (substring (symbol-name kind) 4))
                'face 'gco-pkm-context-tag-face))
   (t
    (propertize "~" 'face 'gco-pkm-context-word-face))))

(defun gco-pkm-context--format-date (date-cell)
  "Format DATE-CELL for display."
  (pcase date-cell
    (`(:date . ,s) s)
    (`(:mtime . ,time) (format-time-string "%Y-%m-%d" time))))

(defun gco-pkm-context--render (buffer entries root header)
  "Render ENTRIES into BUFFER, with HEADER at top."
  (with-current-buffer buffer
    (setq gco-pkm-context--last-preview-pos nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize header 'face 'mode-line-emphasis) "\n")
      (insert (make-string (length header) ?-) "\n\n")
      (if (null entries)
          (insert (propertize "  (no related entries)\n"
                              'face 'gco-pkm-context-snippet-face))
        (let ((idx 0))
          (dolist (e entries)
            (cl-incf idx)
            (let* ((file (plist-get e :file))
                   (line (plist-get e :line))
                   (kind (plist-get e :kind))
                   (match (string-trim-right (plist-get e :match)))
                   (ctx (string-trim-right (or (plist-get e :context) "")))
                   (date (gco-pkm-context--format-date (plist-get e :date)))
                   (rel (file-relative-name file root))
                   (begin (point)))
              (insert (gco-pkm-context--badge kind) " "
                      (propertize date 'face 'gco-pkm-context-date-face) " "
                      (propertize (format "%s:%d" rel line)
                                  'face 'gco-pkm-context-path-face)
                      "\n")
              (insert "    "
                      (propertize match 'face 'gco-pkm-context-heading-face)
                      "\n")
              (when (and ctx (not (string-empty-p ctx)))
                (insert "    "
                        (propertize ctx 'face 'gco-pkm-context-snippet-face)
                        "\n"))
              (insert "\n")
              (add-text-properties
               begin (point)
               `(gco-pkm-context-entry ,idx
                                       gco-pkm-context-file ,file
                                       gco-pkm-context-line ,line
                                       gco-pkm-context-kind ,kind))))))
      (goto-char (point-min)))))

;;;; Window placement

(defun gco-pkm-context--get-buffer ()
  "Return the context buffer, creating it (and the major mode) once."
  (let ((buf (get-buffer gco-pkm-context--buffer-name)))
    (unless buf
      (setq buf (get-buffer-create gco-pkm-context--buffer-name))
      (with-current-buffer buf
        (gco-pkm-context-mode)))
    buf))

(defun gco-pkm-context--display (buffer)
  "Display BUFFER in the configured side window, returning the window."
  (display-buffer
   buffer
   `((display-buffer-in-side-window)
     (side . ,gco-pkm-context-side)
     (slot . 0)
     (window-width . ,gco-pkm-context-window-width)
     (preserve-size . (t . nil))
     (window-parameters . ((no-other-window . nil)
                           (no-delete-other-windows . t))))))

(defun gco-pkm-context--side-window ()
  "Return the live window currently showing the context buffer, or nil."
  (let ((buf (get-buffer gco-pkm-context--buffer-name)))
    (and buf (get-buffer-window buf))))

;;;; Follow-mode preview

(defun gco-pkm-context--reveal ()
  "Reveal the org subtree at point if applicable."
  (when (derived-mode-p 'org-mode)
    (ignore-errors
      (cond
       ((fboundp 'org-fold-show-context) (org-fold-show-context 'agenda))
       ((fboundp 'org-show-context) (org-show-context 'agenda))))))

(defun gco-pkm-context--pick-preview-window ()
  "Pick a window in which to preview a source location.
Prefers `gco-pkm-context--source-window' when live and not the side
window."
  (let ((side (gco-pkm-context--side-window)))
    (cond
     ((and (window-live-p gco-pkm-context--source-window)
           (not (eq gco-pkm-context--source-window side)))
      gco-pkm-context--source-window)
     (t
      (catch 'found
        (walk-windows
         (lambda (w)
           (unless (or (eq w side)
                       (window-dedicated-p w)
                       (window-parameter w 'window-side))
             (throw 'found w)))
         nil 'visible)
        nil)))))

(defun gco-pkm-context--preview-at-point ()
  "Show the source location of the entry at point, without stealing focus."
  (let ((file (get-text-property (point) 'gco-pkm-context-file))
        (line (get-text-property (point) 'gco-pkm-context-line)))
    (when (and file line
               (not (equal (cons file line) gco-pkm-context--last-preview-pos)))
      (setq gco-pkm-context--last-preview-pos (cons file line))
      (let* ((src-buf (find-file-noselect file))
             (target (or (gco-pkm-context--pick-preview-window)
                         (display-buffer
                          src-buf
                          '((display-buffer-pop-up-window)
                            (inhibit-same-window . t))))))
        (when (window-live-p target)
          (with-selected-window target
            (switch-to-buffer src-buf nil t)
            (goto-char (point-min))
            (forward-line (1- line))
            (gco-pkm-context--reveal)
            (recenter)))))))

(defvar gco-pkm-context-follow-mode)

(defun gco-pkm-context--follow-post-command ()
  "post-command-hook in the context buffer: preview after a tiny idle."
  (when gco-pkm-context-follow-mode
    (run-with-idle-timer
     0.05 nil
     (let ((buf (current-buffer)))
       (lambda ()
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (gco-pkm-context--preview-at-point))))))))

(define-minor-mode gco-pkm-context-follow-mode
  "Buffer-local: when on, point movement previews source locations."
  :lighter " Follow"
  (if gco-pkm-context-follow-mode
      (add-hook 'post-command-hook
                #'gco-pkm-context--follow-post-command nil t)
    (remove-hook 'post-command-hook
                 #'gco-pkm-context--follow-post-command t)))

;;;; Major mode

(defun gco-pkm-context-next-entry ()
  "Move point to the next entry."
  (interactive)
  (let ((m (text-property-search-forward
            'gco-pkm-context-entry nil
            (lambda (_target value) value) t)))
    (when m (goto-char (prop-match-beginning m)))))

(defun gco-pkm-context-previous-entry ()
  "Move point to the previous entry."
  (interactive)
  (let ((m (text-property-search-backward
            'gco-pkm-context-entry nil
            (lambda (_target value) value) t)))
    (when m (goto-char (prop-match-beginning m)))))

(defun gco-pkm-context-jump ()
  "Jump fully to the source location of the entry at point."
  (interactive)
  (let ((file (get-text-property (point) 'gco-pkm-context-file))
        (line (get-text-property (point) 'gco-pkm-context-line)))
    (unless (and file line)
      (user-error "No entry at point"))
    (let* ((buf (find-file-noselect file))
           (target (or (and (window-live-p gco-pkm-context--source-window)
                            (not (eq gco-pkm-context--source-window
                                     (gco-pkm-context--side-window)))
                            gco-pkm-context--source-window)
                       (gco-pkm-context--pick-preview-window))))
      (if (window-live-p target)
          (progn
            (select-window target)
            (switch-to-buffer buf))
        (pop-to-buffer buf))
      (goto-char (point-min))
      (forward-line (1- line))
      (gco-pkm-context--reveal))))

(defun gco-pkm-context-display-entry ()
  "Like RET but keep focus in the context buffer."
  (interactive)
  (gco-pkm-context--preview-at-point))

(defun gco-pkm-context-toggle-follow ()
  "Toggle follow-mode in the context buffer."
  (interactive)
  (gco-pkm-context-follow-mode 'toggle)
  (message "PKM context follow-mode %s"
           (if gco-pkm-context-follow-mode "on" "off")))

(defvar gco-pkm-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n")     #'gco-pkm-context-next-entry)
    (define-key map (kbd "p")     #'gco-pkm-context-previous-entry)
    (define-key map (kbd "TAB")   #'gco-pkm-context-next-entry)
    (define-key map (kbd "<backtab>") #'gco-pkm-context-previous-entry)
    (define-key map (kbd "RET")   #'gco-pkm-context-jump)
    (define-key map (kbd "o")     #'gco-pkm-context-display-entry)
    (define-key map (kbd "g")     #'gco-pkm-context-refresh-now)
    (define-key map (kbd "f")     #'gco-pkm-context-toggle-follow)
    (define-key map (kbd "q")     #'quit-window)
    map)
  "Keymap for `gco-pkm-context-mode'.")

(define-derived-mode gco-pkm-context-mode special-mode "PKM-Ctx"
  "Major mode for the PKM context sidebar."
  (setq-local truncate-lines t)
  (setq-local cursor-in-non-selected-windows nil)
  (gco-pkm-context-follow-mode 1))

;;;; Tracking & lifecycle

(defvar gco-pkm-context--last-source-file nil)
(defvar gco-pkm-context--last-source-line nil)

(defun gco-pkm-context--maybe-refresh ()
  "Idle-timer entry point: refresh if the heading or nearby tags changed."
  (when (and (bound-and-true-p gco-pkm-context-track-mode)
             (gco-pkm-context--in-pkm-buffer-p)
             (not (eq (current-buffer)
                      (get-buffer gco-pkm-context--buffer-name))))
    (let* ((file (buffer-file-name))
           (heading (gco-pkm-context--current-heading-text))
           (words (gco-pkm-context--significant-words heading))
           (tags (gco-pkm-context--nearby-tags))
           (key (gco-pkm-context--key file heading words tags)))
      (setq gco-pkm-context--source-window (selected-window)
            gco-pkm-context--last-source-file file
            gco-pkm-context--last-source-line
            (line-number-at-pos
             (if (derived-mode-p 'org-mode)
                 (or (ignore-errors
                       (save-excursion (org-back-to-heading t) (point)))
                     (point))
               (point))))
      (unless (equal key gco-pkm-context--last-key)
        (setq gco-pkm-context--last-key key)
        (gco-pkm-context--refresh heading words tags file)))))

(defun gco-pkm-context--refresh (heading words tags source-file)
  "Run a refresh for the given HEADING/WORDS/TAGS."
  (let* ((buf (gco-pkm-context--get-buffer))
         (root (expand-file-name gco-pkm-directory))
         (header (format "PKM context — %s"
                         (or heading "(no heading)")))
         (built (gco-pkm-context--build-pattern heading words tags)))
    (gco-pkm-context--display buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize header 'face 'mode-line-emphasis) "\n")
        (insert (make-string (length header) ?-) "\n\n")
        (insert (propertize "  Computing context..."
                            'face 'gco-pkm-context-snippet-face))
        (goto-char (point-min))))
    (if (null built)
        (gco-pkm-context--render buf nil root header)
      (let ((src-line gco-pkm-context--last-source-line))
        (gco-pkm-context--launch
         (car built) (cdr built) root
         (lambda (entries)
           (let* ((cleaned
                   (gco-pkm-context--truncate
                    (gco-pkm-context--sort
                     (gco-pkm-context--dedup
                      (gco-pkm-context--filter-self
                       entries source-file src-line)))
                    gco-pkm-context-max-results)))
             (gco-pkm-context--render buf cleaned root header))))))))

(defun gco-pkm-context-refresh-now ()
  "Force a refresh of the context buffer using the current selection."
  (interactive)
  (setq gco-pkm-context--last-key nil)
  (gco-pkm-context--maybe-refresh))

;;;###autoload
(define-minor-mode gco-pkm-context-track-mode
  "Global minor mode: ambient PKM context sidebar."
  :global t
  :lighter " PKM-Ctx"
  (cond
   (gco-pkm-context-track-mode
    (unless (executable-find gco-inline-tags-rg-exe)
      (setq gco-pkm-context-track-mode nil)
      (user-error "Ripgrep (`%s') not found; cannot enable context tracking"
                  gco-inline-tags-rg-exe))
    (gco-pkm-context--display (gco-pkm-context--get-buffer))
    (setq gco-pkm-context--last-key nil)
    (when gco-pkm-context--idle-timer
      (cancel-timer gco-pkm-context--idle-timer))
    (setq gco-pkm-context--idle-timer
          (run-with-idle-timer gco-pkm-context-idle-delay t
                               #'gco-pkm-context--maybe-refresh))
    (gco-pkm-context--maybe-refresh))
   (t
    (when gco-pkm-context--idle-timer
      (cancel-timer gco-pkm-context--idle-timer)
      (setq gco-pkm-context--idle-timer nil))
    (gco-pkm-context--cancel)
    (let ((win (gco-pkm-context--side-window)))
      (when (window-live-p win)
        (delete-window win)))
    (setq gco-pkm-context--last-key nil
          gco-pkm-context--source-window nil))))

;;;###autoload
(defun gco-pkm-context-toggle ()
  "Toggle the PKM context sidebar."
  (interactive)
  (gco-pkm-context-track-mode 'toggle))

(defvar gco-pkm-context--auto-enable-attempted nil
  "Non-nil once auto-enable has fired this session.")

;;;###autoload
(defun gco-pkm-context-maybe-enable ()
  "Enable `gco-pkm-context-track-mode' on first PKM org buffer this session.
Intended for `org-mode-hook'.  Once it has fired, the user remains in
control: toggling off will not be undone by visiting another PKM file."
  (when (and (not gco-pkm-context--auto-enable-attempted)
             (derived-mode-p 'org-mode)
             (gco-pkm-context--in-pkm-buffer-p))
    (setq gco-pkm-context--auto-enable-attempted t)
    (gco-pkm-context-track-mode 1)))

(provide 'gco-pkm-context)
;;; gco-pkm-context.el ends here
