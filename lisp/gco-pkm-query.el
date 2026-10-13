;;; gco-pkm-query.el --- Query blocks: journal views kept inside notes -*- lexical-binding: t; -*-

;; Author: Gary Oberbrunner
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (gco-pkm "0.1"))
;; Keywords: outlines, convenience, markdown, pkm

;;; Commentary:
;; A query block is a live view of matching journal sections, kept in a note:
;;
;;   <!-- query: Egypt OR eclipse -->
;;   ## [2026-10-13 Tue · Travel Egypt](../journals/2026-10-13.md#travel-egypt)
;;   ...copied journal text...
;;   <!-- /query 3f9a2c1e -->
;;
;; The query line is yours.  Everything after it, through the closing line,
;; is a copy that the PKM server rewrites whenever the journals change; see
;; doc/query-blocks-design.md in gco-pkm-llm.  `gco-pkm-query-mode' shades
;; those results and makes them read-only, so an edit there can't be lost at
;; the next rewrite, and C-c C-c on a block refreshes it locally by piping
;; the note through the server's query engine.  Like an Org dynamic block
;; update, that leaves the buffer modified rather than saving it.
;;
;; Ticking a task anywhere in a note stamps it: checked, its line ends
;; with a ✅ and today's date; reopened, it loses the stamp.  On a task
;; copied into a block's results, C-c C-c ticks the item it is a copy
;; of instead, in its journal, and the copy with it; see
;; doc/query-todos-design.md in gco-pkm-llm.
;;
;; Markers are recognised exactly as the engine (pkm_bridge/queries)
;; recognises them, fences included, so what is shaded here is precisely
;; what the server will overwrite.  The context sidebar uses the same
;; scanner to skip the copies, which would otherwise turn up twice in every
;; search.
;;
;; Each result's heading links back to its source section.  Following such
;; a link needs two repairs to markdown-ts-mode, made under "Heading
;; anchors" below.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'gco-pkm)
(require 'gco-pkm-format)

;;;; Customization

(defgroup gco-pkm-query nil
  "Query blocks: journal views kept inside PKM notes."
  :group 'gco-pkm
  :prefix "gco-pkm-query-")

(defcustom gco-pkm-query-python "python3"
  "Python interpreter that runs the query engine (3.9 or later)."
  :type 'string)

(defcustom gco-pkm-query-engine-directory "~/src/gco-pkm-llm"
  "Checkout of gco-pkm-llm, whose pkm_bridge/queries is the query engine.
It goes on PYTHONPATH; the engine needs nothing beyond the standard
library, so no virtualenv is involved."
  :type 'directory)

(defcustom gco-pkm-query-heading "Journal"
  "Heading `gco-pkm-insert-query' puts over a new query block."
  :type 'string)

(defcustom gco-pkm-query-generic-words
  '("a" "an" "and" "or" "not" "the" "of" "for" "to" "in" "on" "at" "by"
    "with" "from" "about" "my" "our" "vs"
    "notes" "note" "page" "list" "log" "misc" "info" "ideas" "plan" "plans"
    "planning" "project" "setup" "trip" "todo" "tasks" "draft" "old" "new"
    "today" "tonight" "tomorrow" "yesterday")
  "Words left out when `gco-pkm-insert-query' seeds a query from a title.
Compared case-insensitively.  Numbers are always left out.  Keep \"or\"
and \"not\" here: as query terms they would read as operators."
  :type '(repeat string))

(defface gco-pkm-query-results-face
  '((((class color) (background light)) :background "#f1f4f8" :extend t)
    (((class color) (background dark)) :background "#1b1f25" :extend t)
    (t :inherit shadow))
  "Face shading a query block's results, which are a read-only copy.")

;;;; Finding blocks

;; These mirror OPEN_RE, CLOSE_RE and FENCE_RE in pkm_bridge/queries/notes.py.
;; Python's \s becomes `blank' plus the three control characters it also
;; covers; matching is case-sensitive, as there.

(defconst gco-pkm-query--open-re
  (rx (** 0 3 (any " \t")) "<!--" (* (any blank ?\r ?\f ?\v)) "query:"
      (* (any blank ?\r ?\f ?\v)) (group (*? nonl))
      (* (any blank ?\r ?\f ?\v)) "-->" (* (any blank ?\r ?\f ?\v)) eol)
  "Matches a query line from its start; group 1 is the query.")

(defconst gco-pkm-query--close-re
  (rx (** 0 3 (any " \t")) "<!--" (* (any blank ?\r ?\f ?\v)) "/query"
      (? (+ (any blank ?\r ?\f ?\v)) (= 8 (any "0-9a-f")))
      (* (any blank ?\r ?\f ?\v)) "-->" (* (any blank ?\r ?\f ?\v)) eol)
  "Matches a query block's closing line from its start.")

(defconst gco-pkm-query--fence-re
  (rx (* (any " \t")) (group (or (>= 3 "`") (>= 3 "~"))))
  "Matches a code fence line from its start; group 1 is the fence itself.
Looser than CommonMark, as the engine is: any indentation, and a closing
fence may carry text after it.")

(defconst gco-pkm-query--heading-re
  (rx (group (** 1 6 "#")) (? (+ (any " \t")) (group (*? nonl)))
      (? (+ (any " \t")) (+ "#")) (* (any " \t")) eol)
  "Matches an ATX heading line from its start, as the engine's HEADING_RE.
Group 1 is the marker; group 2 the text, without any closing #s.")

(cl-defstruct (gco-pkm-query-block
               (:constructor gco-pkm-query--block)
               (:copier nil))
  "A query block: its query line, then any results and its closing line.
Positions belong to the buffer it was found in.  Lines count from 1, as
ripgrep's do.  A block with no closing line yet is just its query line."
  (start nil :documentation "Start of the query line.")
  (query-end nil :documentation "End of the query line, before its newline.")
  (end nil :documentation "End of the block's last line, after its newline.")
  (first-line nil :documentation "Line number of the query line.")
  (last-line nil :documentation "Line number of the block's last line.")
  (query nil :documentation "The query text."))

(defun gco-pkm-query-block-closed-p (block)
  "Non-nil if BLOCK has a closing line, and so a region of results."
  (> (gco-pkm-query-block-last-line block)
     (gco-pkm-query-block-first-line block)))

(defun gco-pkm-query--fence-closer ()
  "If the line at point opens a code fence, return a regexp that closes it."
  (when (looking-at gco-pkm-query--fence-re)
    (let ((fence (match-string 1)))
      (concat "[ \t]*" (regexp-quote fence)
              (regexp-quote (substring fence 0 1)) "*"))))

(defun gco-pkm-query--scan-lines (regexp fn)
  "Call FN at the start of each line that REGEXP matches, outside code fences.
Fenced lines are skipped whole, fence lines included, so a note that
explains the syntax holds no live markers.  Matching is case-sensitive."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search nil)
            (candidate (concat "^\\(?:[ \t]*\\(?:```\\|~~~\\)\\|" regexp "\\)"))
            closer)
        (while (re-search-forward candidate nil t)
          (forward-line 0)
          (cond (closer (when (looking-at closer) (setq closer nil)))
                ((setq closer (gco-pkm-query--fence-closer)))
                ((looking-at regexp) (save-excursion (funcall fn))))
          (forward-line 1))))))

(defun gco-pkm-query--make-block (open close)
  "Make a block from OPEN, (START . QUERY) for its query line, and CLOSE.
CLOSE is the start of the closing line, or nil if there is none."
  (save-excursion
    (let ((start (car open))
          (last (or close (car open))))
      (goto-char start)
      (gco-pkm-query--block
       :start start
       :query-end (line-end-position)
       :end (progn (goto-char last) (line-beginning-position 2))
       :first-line (line-number-at-pos start t)
       :last-line (line-number-at-pos last t)
       :query (cdr open)))))

(defun gco-pkm-query-blocks ()
  "Return the query blocks in the current buffer, in order.
A block runs from a query line to the next closing line.  A query line
followed by another query line, or by the end of the buffer, before any
closing line is a block whose results haven't been filled in yet."
  (save-excursion
    (save-restriction
      (widen)
      (let (open blocks)
        (gco-pkm-query--scan-lines
         "[ \t]\\{0,3\\}<!--"
         (lambda ()
           (cond ((looking-at gco-pkm-query--open-re)
                  (when open
                    (push (gco-pkm-query--make-block open nil) blocks))
                  (setq open (cons (point) (match-string-no-properties 1))))
                 ((and open (looking-at gco-pkm-query--close-re))
                  (push (gco-pkm-query--make-block open (point)) blocks)
                  (setq open nil)))))
        (when open
          (push (gco-pkm-query--make-block open nil) blocks))
        (nreverse blocks)))))

(defun gco-pkm-query-block-at (&optional pos)
  "Return the query block whose lines include POS (default point), or nil."
  (let ((line (line-number-at-pos pos t)))
    (seq-find (lambda (block)
                (<= (gco-pkm-query-block-first-line block)
                    line
                    (gco-pkm-query-block-last-line block)))
              (gco-pkm-query-blocks))))

;;;; Shading and protection

(defconst gco-pkm-query--update-delay 0.3
  "Idle seconds after an edit before blocks are found again.")

(defvar-local gco-pkm-query--update-timer nil
  "Pending idle timer that finds this buffer's blocks again, or nil.")

(defvar gco-pkm-query-mode)

(defun gco-pkm-query--remove-overlays ()
  "Remove this buffer's query block shading and protection."
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'gco-pkm-query)
        (delete-overlay ov)))))

(defun gco-pkm-query--guard (overlay after beg end &rest _)
  "Refuse a change to the query results OVERLAY protects.
A modification hook, called before (AFTER nil) and after each change to
BEG..END inside the results, and for an insertion just after the query
line.  Allowed anyway: changes made under `inhibit-read-only', which
refresh, revert and undo all bind, and a deletion that takes the whole
block, query line included -- killing a section, or moving it, carries
its block along."
  (unless (or after
              inhibit-read-only
              (not gco-pkm-query-mode)
              (and (<= beg (save-excursion
                             (goto-char (overlay-start overlay))
                             (line-beginning-position)))
                   (>= end (overlay-end overlay))))
    (signal 'text-read-only
            (list "Query results are a copy: edit their source, or C-c C-c to refresh"))))

(defun gco-pkm-query--update-overlays ()
  "Shade and protect the results of every query block in the buffer.
Rebuilt from scratch, so it is right whatever happened to the text.  The
protected region starts at the query line's newline, so nothing can be
inserted between the query and its results; it ends with the closing
line's newline, so typing at the start of the next line is fine."
  (when gco-pkm-query--update-timer
    (cancel-timer gco-pkm-query--update-timer)
    (setq gco-pkm-query--update-timer nil))
  (gco-pkm-query--remove-overlays)
  (when gco-pkm-query-mode
    (dolist (block (gco-pkm-query-blocks))
      (when (gco-pkm-query-block-closed-p block)
        (let* ((query-end (gco-pkm-query-block-query-end block))
               (end (gco-pkm-query-block-end block))
               (guard (make-overlay query-end end))
               (shade (make-overlay (1+ query-end) end)))
          (overlay-put guard 'gco-pkm-query 'guard)
          (overlay-put guard 'evaporate t)
          (overlay-put guard 'modification-hooks '(gco-pkm-query--guard))
          (overlay-put guard 'insert-in-front-hooks '(gco-pkm-query--guard))
          (overlay-put shade 'gco-pkm-query 'shade)
          (overlay-put shade 'evaporate t)
          (overlay-put shade 'face 'gco-pkm-query-results-face))))))

(defun gco-pkm-query--run-update (buffer)
  "Find BUFFER's blocks again; the idle timer's callback."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq gco-pkm-query--update-timer nil)
      (gco-pkm-query--update-overlays))))

(defun gco-pkm-query--schedule-update (&rest _)
  "Find the blocks again once the user pauses: an edit may add or drop markers.
For `after-change-functions'."
  (unless gco-pkm-query--update-timer
    (setq gco-pkm-query--update-timer
          (run-with-idle-timer gco-pkm-query--update-delay nil
                               #'gco-pkm-query--run-update (current-buffer)))))

;;;; Running the engine

(defun gco-pkm-query--note-path ()
  "Return the visited note's path relative to `gco-pkm-directory'."
  (let ((file (or buffer-file-name (user-error "This buffer isn't visiting a note")))
        (root (file-truename gco-pkm-directory)))
    (unless (file-in-directory-p file root)
      (user-error "%s isn't under %s" (abbreviate-file-name file) gco-pkm-directory))
    (file-relative-name (file-truename file) root)))

(defun gco-pkm-query--buffer-text (&optional buffer)
  "Return all of BUFFER's text (default the current buffer's), without properties."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun gco-pkm-query--run-engine (command &rest args)
  "Pipe the buffer through the query engine's COMMAND with ARGS; return its JSON.
The engine works on the notes in `gco-pkm-directory'.  Its JSON comes
back as an alist, with lists for arrays and nil for null and false.  An
`error' key is the engine declining, which becomes a `user-error' with
its message."
  (let ((python (or (executable-find gco-pkm-query-python)
                    (user-error "Can't find Python (`%s'); set `gco-pkm-query-python'"
                                gco-pkm-query-python)))
        (engine (file-name-as-directory
                 (expand-file-name gco-pkm-query-engine-directory))))
    (unless (file-directory-p (expand-file-name "pkm_bridge/queries" engine))
      (user-error "No query engine in %s; set `gco-pkm-query-engine-directory'"
                  (abbreviate-file-name engine)))
    (let ((out (generate-new-buffer " *gco-pkm-query-out*" t))
          (err (make-temp-file "gco-pkm-query-err")))
      (unwind-protect
          (let ((status
                 (let ((default-directory engine) ; keeps the note's folder off sys.path
                       (process-environment
                        (cons (concat "PYTHONPATH=" engine
                                      (when-let* ((old (getenv "PYTHONPATH")))
                                        (concat path-separator old)))
                              process-environment))
                       (coding-system-for-write 'utf-8-unix)
                       (coding-system-for-read 'utf-8-unix))
                   (save-restriction
                     (widen)
                     (apply #'call-process-region (point-min) (point-max) python nil
                            (list out err) nil
                            "-m" "pkm_bridge.queries" command
                            (append args (list "--root"
                                               (expand-file-name gco-pkm-directory))))))))
            (unless (eql status 0)
              (let ((lines (with-temp-buffer
                             (insert-file-contents err)
                             (split-string (buffer-string) "\n" t "[ \t]+"))))
                (error "Query engine failed (%s): %s"
                       status (or (car (last lines)) "no output"))))
            (let ((report (with-current-buffer out
                            (goto-char (point-min))
                            (condition-case nil
                                (json-parse-buffer :object-type 'alist :array-type 'list
                                                   :null-object nil :false-object nil)
                              (json-error (error "Query engine returned no JSON: %s"
                                                 (string-limit (buffer-string) 200)))))))
              (when-let* ((refusal (alist-get 'error report)))
                (user-error "%s" refusal))
              report))
        (kill-buffer out)
        (delete-file err)))))

(defun gco-pkm-query--replace-text (text)
  "Make the buffer's text TEXT, editing only where they differ.
Point, markers and everything outside the changes survive."
  (let ((inhibit-read-only t)
        (source (generate-new-buffer " *gco-pkm-query-text*" t)))
    (unwind-protect
        (save-restriction
          (widen)
          (with-current-buffer source (insert text))
          (if (>= emacs-major-version 31)
              (replace-region-contents (point-min) (point-max) source)
            ;; Obsolete since 31.1, in favour of the call above.
            (with-suppressed-warnings ((obsolete replace-buffer-contents))
              (replace-buffer-contents source))))
      (kill-buffer source))))

(defun gco-pkm-query--describe (report)
  "Describe one block's REPORT from the engine, e.g. \"`Egypt' 7 entries\"."
  (let-alist report
    (concat (format-message "`%s' " .query)
            (if .error
                (format "error: %s" .error)
              (format "%d %s" .entries (if (eql .entries 1) "entry" "entries")))
            (if .hand_edited " (hand edits replaced)" ""))))

(defun gco-pkm-query--summary (reports changed)
  "Summarize a refresh: REPORTS from the engine; CHANGED if the text changed."
  (if (null reports)
      "No query blocks in this note"
    (concat (if changed "Query results updated: " "Query results unchanged: ")
            (mapconcat #'gco-pkm-query--describe reports "; "))))

;;;###autoload
(defun gco-pkm-query-refresh ()
  "Re-run every query block in this note, as Org updates dynamic blocks.
The buffer goes through the query engine's command line, so this works
offline.  Only the changed text is replaced, so point stays put, and the
buffer is left modified rather than saved."
  (interactive)
  (let* ((report (gco-pkm-query--run-engine "refresh-text" "--json"
                                            "--path" (gco-pkm-query--note-path)))
         (text (alist-get 'text report))
         (changed (not (string= text (gco-pkm-query--buffer-text)))))
    (when changed
      (gco-pkm-query--replace-text text))
    (gco-pkm-query--update-overlays)
    (message "%s" (gco-pkm-query--summary (alist-get 'blocks report) changed))))

;;;; Editing blocks

;;;###autoload
(defun gco-pkm-query-delete-block ()
  "Delete the query block at point: its query line, results and closing line."
  (interactive)
  (let ((block (or (gco-pkm-query-block-at) (user-error "Not in a query block")))
        (inhibit-read-only t))
    (delete-region (gco-pkm-query-block-start block) (gco-pkm-query-block-end block))
    (gco-pkm-query--update-overlays)))

(defun gco-pkm-query-newline-after-block ()
  "With point at the end of a block's closing line, open a line below it.
A plain newline there would split the read-only closing line."
  (interactive)
  (forward-line 1)
  (save-excursion (insert "\n")))

(defun gco-pkm-query--title-query ()
  "Return a query seeded from this note's title: its telling words, ORed."
  (let ((title (or (ignore-errors (gco-pkm-format-buffer-title))
                   (and buffer-file-name (file-name-base buffer-file-name))
                   "")))
    (string-join
     (seq-uniq (seq-remove (lambda (word)
                             (or (< (length word) 2)
                                 (string-match-p "\\`[0-9]+\\'" word)
                                 (member-ignore-case word gco-pkm-query-generic-words)))
                           (split-string title "[^[:alnum:]]+" t))
               #'string-equal-ignore-case)
     " OR ")))

(defun gco-pkm-query--headings (&optional include-copies)
  "Return (POS LEVEL TEXT) for each heading, found as the engine finds them.
Lines in the frontmatter or in code fences only look like headings, so
they don't count.  Neither do the copies in query blocks, unless
INCLUDE-COPIES: editors number repeated anchors counting those too."
  (save-restriction
    (widen)
    (let ((body (or (and (save-excursion  ; frontmatter starts with a key, not a rule
                           (goto-char (point-min))
                           (forward-line 1)
                           (looking-at-p "[[:alnum:]_-]+[ \t]*:"))
                         (cdr (gco-pkm-format--frontmatter-bounds)))
                    (point-min)))
          (blocks (unless include-copies (gco-pkm-query-blocks)))
          headings)
      (gco-pkm-query--scan-lines
       gco-pkm-query--heading-re
       (lambda ()
         (unless (or (< (point) body)
                     (seq-some (lambda (block)
                                 (and (<= (gco-pkm-query-block-start block) (point))
                                      (< (point) (gco-pkm-query-block-end block))))
                               blocks))
           (looking-at gco-pkm-query--heading-re)
           (push (list (point) (length (match-string 1))
                       (or (match-string-no-properties 2) ""))
                 headings))))
      (nreverse headings))))

(defun gco-pkm-query--new-section-place ()
  "Return (POS . LEVEL): where a new section goes, and its heading level.
That is just after the subtree of the section at point, as its sibling.
Above the first heading, it is the end of the note, at the note's
shallowest heading level."
  (let* ((headings (gco-pkm-query--headings))
         (current (car (last (seq-take-while (lambda (h) (<= (car h) (point)))
                                             headings)))))
    (if current
        (cons (or (car (seq-find (lambda (h) (and (> (car h) (car current))
                                                  (<= (cadr h) (cadr current))))
                                 headings))
                  (point-max))
              (cadr current))
      (cons (point-max) (if headings (apply #'min (mapcar #'cadr headings)) 1)))))

;;;###autoload
(defun gco-pkm-insert-query (query)
  "Add a journal view to this note: a heading over a query block for QUERY.
The heading, `gco-pkm-query-heading', becomes a sibling of the section
at point, placed after that section's subtree; above the first heading,
it goes at the end of the note.  QUERY is read with the note's title as
a starting point, minus `gco-pkm-query-generic-words'.  The block is
filled in at once."
  (interactive
   (progn
     (unless (eq (gco-pkm-format-current) 'md)
       (user-error "Query blocks live in Markdown notes"))
     (list (read-string "Query: " (gco-pkm-query--title-query)))))
  (when (string-blank-p query)
    (user-error "Empty query"))
  (save-restriction
    (widen)
    (pcase-let ((`(,pos . ,level) (gco-pkm-query--new-section-place)))
      (goto-char pos)
      (unless (bolp) (insert "\n"))
      (unless (or (bobp) (save-excursion (forward-line -1) (looking-at-p "[ \t]*$")))
        (insert "\n"))
      (insert (make-string level ?#) " " gco-pkm-query-heading "\n\n")
      (save-excursion
        (insert "<!-- query: " (string-trim query) " -->\n<!-- /query -->\n")
        (unless (eobp) (insert "\n")))))
  (gco-pkm-query-refresh))

;;;; Tasks
;;
;; A task is a list item that starts with a checkbox.  Emacs, the web
;; editor and the server all set a task's state by the same rule,
;; set_task_state in pkm_bridge/queries/notes.py, so a tick comes out
;; the same bytes wherever it is made.

(declare-function markdown-ts-toggle-checkbox "markdown-ts-mode" ())

(defconst gco-pkm-query--task-re
  (rx (* (any " \t")) (or (any "-*+") (seq (** 1 9 (any "0-9")) (any ".)")))
      (+ (any " \t")) "[" (group (any " xX")) "]" (or (any " \t") eol))
  "Matches a task line from its start, as the engine's TASK_RE.
Group 1 is its state: a space when open, x or X when done.")

(defconst gco-pkm-query--stamp-re
  (rx (group (+ (any " \t")) "✅" (* (any " \t"))
             (= 4 (any "0-9")) "-" (= 2 (any "0-9")) "-" (= 2 (any "0-9")))
      (* (any " \t")) eol)
  "Matches a completion stamp as group 1, as the engine's _STAMP_RE.
A stamp ends its line, but for any spaces and tabs.")

(defun gco-pkm-query--task-state ()
  "Return `open' or `done' for the task line at point, nil for any other line."
  (save-excursion
    (forward-line 0)
    (let ((case-fold-search nil))
      (and (looking-at gco-pkm-query--task-re)
           (if (eq (char-after (match-beginning 1)) ?\s) 'open 'done)))))

(defun gco-pkm-query-set-task-state (checked &optional today)
  "Make the task line at point CHECKED, or open if CHECKED is nil.
Checking sets the task's state to x, takes any ✅ stamp and then any
trailing spaces and tabs off its line, and ends it with \" ✅ TODAY\";
TODAY is a YYYY-MM-DD string, by default the local date.  Opening sets
the state to a space and takes off any stamp.  Nothing else on the line
changes.  Return nil, changing nothing, if the line isn't a task."
  (save-excursion
    (forward-line 0)
    (let ((case-fold-search nil))
      (when (looking-at gco-pkm-query--task-re)
        (let ((state (match-beginning 1))
              (stamp (and (re-search-forward gco-pkm-query--stamp-re (line-end-position) t)
                          (cons (match-beginning 1) (match-end 1)))))
          (if checked
              (let ((tail (concat " ✅ " (or today (format-time-string "%F"))))
                    (end (progn (goto-char (or (car stamp) (line-end-position)))
                                (skip-chars-backward " \t")
                                (point))))
                (unless (string= (buffer-substring-no-properties end (line-end-position)) tail)
                  (delete-region end (line-end-position))
                  (insert tail)))
            (when stamp
              (delete-region (car stamp) (cdr stamp))))
          ;; Inserting before deleting keeps markers on both sides in place.
          (goto-char state)
          (unless (eq (char-after) (if checked ?x ?\s))
            (insert (if checked "x" " "))
            (delete-char 1))
          t)))))

(defun gco-pkm-query-toggle-checkbox ()
  "Toggle the task checkbox at point, and stamp the task to match.
This is `markdown-ts-toggle-checkbox', then `gco-pkm-query-set-task-state'
on the task it toggled: one it checks gets today's ✅ stamp, one it
unchecks loses its stamp.  One undo takes back both."
  (interactive)
  (let (toggled)
    (with-undo-amalgamate
      ;; Watch where the toggle edits: from a continuation line, the
      ;; checkbox it toggles is on the item's first line.
      (let ((note (lambda (beg _end _length) (unless toggled (setq toggled beg)))))
        (add-hook 'after-change-functions note nil t)
        (unwind-protect
            (markdown-ts-toggle-checkbox)
          (remove-hook 'after-change-functions note t)))
      (when toggled
        (save-excursion
          (goto-char toggled)
          (when-let* ((state (gco-pkm-query--task-state)))
            (gco-pkm-query-set-task-state (eq state 'done))))))))

;;;###autoload
(defun gco-pkm-query-check-task ()
  "Tick the task at point, a copy in query results, where it came from.
An open copy checks off the item it is a copy of, adding a ✅ stamp in
its journal; a done copy reopens it.  The copy changes the same way at
once, and the next refresh brings the rest of the page in line.

The query engine finds the item and makes both changes, working from
the text of the buffers visiting the two notes, so unsaved edits in
either count.  Each of the two is saved unless it had unsaved changes
already.  A journal that wasn't being visited is visited for this, and
killed again afterwards."
  (interactive)
  (let* ((checked (eq (or (gco-pkm-query--task-state) (user-error "Not on a task"))
                      'open))
         (page (current-buffer))
         (rel (gco-pkm-query--note-path))
         (line (number-to-string (line-number-at-pos nil t)))
         (source-path (alist-get 'source_path
                                 (gco-pkm-query--run-engine "locate-task"
                                                            "--path" rel "--line" line)))
         (file (expand-file-name source-path gco-pkm-directory))
         (visited (find-buffer-visiting file))
         (request `((page . ,(gco-pkm-query--buffer-text))
                    (source . ,(if visited (gco-pkm-query--buffer-text visited) :null))))
         (change (with-temp-buffer
                   (json-insert request)
                   (gco-pkm-query--run-engine "check-task" "--path" rel "--line" line
                                              (if checked "--check" "--uncheck"))))
         (save-page (not (buffer-modified-p page)))
         (save-source (not (and visited (buffer-modified-p visited))))
         (source (or visited (find-file-noselect file))))
    (unwind-protect
        (progn
          (with-current-buffer source
            (gco-pkm-query--replace-text (alist-get 'source change))
            (when save-source (save-buffer)))
          (with-current-buffer page
            (gco-pkm-query--replace-text (alist-get 'page change))
            (gco-pkm-query--update-overlays)
            (when save-page (save-buffer))))
      (unless (or visited (buffer-modified-p source))
        (kill-buffer source)))
    (message (if checked "Checked off in %s" "Reopened in %s") source-path)))

;;;; Heading anchors
;;
;; Entry headings link to their source section by its GitHub anchor, taken
;; from the heading as displayed: `# [OpenFX](../pages/openfx.md) TSC
;; meeting' is `#openfx-tsc-meeting', as `gco-pkm-create-block-reference'
;; also has it.  markdown-ts-mode slugs the raw markup instead, so it can't
;; find those anchors, and it visits `note.md#anchor' as a file of that
;; name.  Two guarded advices fix both; should either internal function go
;; away or change its contract, they quietly stand aside.

(defun gco-pkm-query--heading-anchors ()
  "Return a hash table from this buffer's heading anchors to their positions.
An anchor is the heading's GitHub slug with links flattened to their
text, by `gco-pkm--heading-slug'; a repeat gets -1, -2, ... in document
order, and the first of two equal anchors wins.  Headings are counted as
the engine counts them when it mints entry links, copies included."
  (let ((anchors (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal)))
    (pcase-dolist (`(,pos ,_level ,text) (gco-pkm-query--headings t))
      (let* ((slug (gco-pkm--heading-slug (gco-pkm-format-heading-text text 'md)))
             (n (gethash slug seen 0))
             (anchor (if (zerop n) slug (format "%s-%d" slug n))))
        (puthash slug (1+ n) seen)
        (unless (or (string-empty-p slug) (gethash anchor anchors))
          (puthash anchor pos anchors))))
    anchors))

(defun gco-pkm-query--add-heading-anchors (ids)
  "Add this buffer's flattened heading anchors to IDS, keeping its entries.
Filter-return advice for `markdown-ts--build-heading-ids'.  IDS comes
back untouched unless it is the table of positions this was written for."
  (when (and (hash-table-p ids)
             (catch 'unexpected
               (maphash (lambda (_id pos)
                          (unless (integer-or-marker-p pos) (throw 'unexpected nil)))
                        ids)
               t))
    (condition-case err
        (maphash (lambda (anchor pos)
                   (unless (gethash anchor ids) (puthash anchor pos ids)))
                 (gco-pkm-query--heading-anchors))
      (error (message "gco-pkm-query: heading anchors: %s" (error-message-string err)))))
  ids)

(defun gco-pkm-query--visit-anchor (file anchor)
  "Visit FILE, relative to the current buffer, at its heading ANCHOR."
  (find-file file)
  (if-let* ((pos (or (and (derived-mode-p 'markdown-ts-mode)
                          (fboundp 'markdown-ts--heading-ids)
                          (ignore-errors (gethash anchor (markdown-ts--heading-ids))))
                     (gethash anchor (gco-pkm-query--heading-anchors)))))
      (progn
        (push-mark)
        (goto-char pos)
        (when (eq (window-buffer) (current-buffer))
          (recenter)))
    (message "No heading for #%s in %s" anchor (file-name-nondirectory file))))

(defun gco-pkm-query--anchor-link-button (beg end url &rest _)
  "Make the link button from BEG to END land on URL's heading in its note.
After advice for `markdown-ts--make-link-button', which follows a bare
`#anchor' itself but visits `note.md#anchor' as a file of that name."
  (when (and (stringp url)
             (string-match "\\`<?\\([^#:<>]+\\)#\\([^#>]+\\)>?\\'" url))
    (let ((file (match-string 1 url))
          (anchor (match-string 2 url)))
      (put-text-property beg end 'action
                         (lambda (_button) (gco-pkm-query--visit-anchor file anchor))))))

(with-eval-after-load 'markdown-ts-mode
  (when (fboundp 'markdown-ts--build-heading-ids)
    (advice-add 'markdown-ts--build-heading-ids :filter-return
                #'gco-pkm-query--add-heading-anchors))
  (when (fboundp 'markdown-ts--make-link-button)
    (advice-add 'markdown-ts--make-link-button :after
                #'gco-pkm-query--anchor-link-button)))

;;;; Minor mode

(defun gco-pkm-query--at-block-p ()
  "Non-nil when point is on a query line or among a block's results.
Cheap enough for a key filter: overlays answer inside results, and only
a line that looks like a marker pays for a scan of the buffer."
  (or (seq-some (lambda (ov) (overlay-get ov 'gco-pkm-query)) (overlays-at (point)))
      (and (save-excursion
             (forward-line 0)
             (let ((case-fold-search nil))
               (or (looking-at gco-pkm-query--open-re)
                   (looking-at gco-pkm-query--close-re))))
           (gco-pkm-query-block-at))))

(defun gco-pkm-query--at-closing-eol-p ()
  "Non-nil when point ends the closing line of a block, with text after it."
  (and (eolp) (not (eobp))
       (seq-some (lambda (ov) (and (eq (overlay-get ov 'gco-pkm-query) 'guard)
                                   (= (overlay-end ov) (1+ (point)))))
                 (overlays-at (point)))))

(defun gco-pkm-query--block-filter (cmd)
  "Key filter: CMD on a query block, else nil to let C-c C-c fall through.
On a task among a block's results, `gco-pkm-query-check-task' instead."
  (and (gco-pkm-query--at-block-p)
       (if (gco-pkm-query--task-state) #'gco-pkm-query-check-task cmd)))

(defun gco-pkm-query--newline-filter (cmd)
  "Key filter: CMD at the end of a closing line, else nil to let RET fall through."
  (and (gco-pkm-query--at-closing-eol-p) cmd))

(defvar gco-pkm-query-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Conditional bindings: elsewhere the filters return nil and the key
    ;; falls through to whatever else binds it -- markdown-ts-mode's
    ;; checkbox toggle and newline, or markdown-mode's C-c C-c prefix.
    (define-key map (kbd "C-c C-c")
                '(menu-item "" gco-pkm-query-refresh
                            :filter gco-pkm-query--block-filter))
    (define-key map (kbd "RET")
                '(menu-item "" gco-pkm-query-newline-after-block
                            :filter gco-pkm-query--newline-filter))
    ;; From C-c C-c or the menu, the checkbox toggle stamps what it ticks.
    (define-key map [remap markdown-ts-toggle-checkbox] #'gco-pkm-query-toggle-checkbox)
    map)
  "Keymap for `gco-pkm-query-mode'.")

;;;###autoload
(define-minor-mode gco-pkm-query-mode
  "Shade the results of a note's query blocks and keep them read-only.
Results are a copy that the server rewrites, so edit the query line, or
the note a result came from.  On a block, C-c C-c refreshes it; on a
task among its results, it checks that task off where it came from, or
reopens it.  At the end of a closing line, RET opens a line below the
block.  Delete a whole block with `gco-pkm-query-delete-block', or kill
a region that spans it.

Elsewhere, C-c C-c on a task, `markdown-ts-toggle-checkbox', stamps it
with today's date when it checks it off, and takes the stamp off when
it reopens it."
  :keymap gco-pkm-query-mode-map
  (gco-pkm-query--update-overlays)
  (if gco-pkm-query-mode
      (progn
        (add-hook 'after-change-functions #'gco-pkm-query--schedule-update nil t)
        (add-hook 'after-revert-hook #'gco-pkm-query--update-overlays nil t)
        ;; Overlays outlive a change of major mode; the mode itself doesn't.
        (add-hook 'change-major-mode-hook #'gco-pkm-query--remove-overlays nil t))
    (remove-hook 'after-change-functions #'gco-pkm-query--schedule-update t)
    (remove-hook 'after-revert-hook #'gco-pkm-query--update-overlays t)
    (remove-hook 'change-major-mode-hook #'gco-pkm-query--remove-overlays t)))

;;;###autoload
(defun gco-pkm-query-maybe-enable ()
  "Enable `gco-pkm-query-mode' if this buffer visits a note.
For `markdown-mode-hook' and `markdown-ts-mode-hook'."
  (when (and buffer-file-name
             (file-in-directory-p buffer-file-name gco-pkm-directory))
    (gco-pkm-query-mode 1)))

(provide 'gco-pkm-query)
;;; gco-pkm-query.el ends here
