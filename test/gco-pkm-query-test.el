;;; gco-pkm-query-test.el --- Tests for query blocks and the sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for gco-pkm-query.el and the query-block parts of
;; gco-pkm-context.el.  Run from the config directory:
;;
;;   emacs -Q --batch -l test/gco-pkm-query-test.el -f ert-run-tests-batch-and-exit
;;
;; Packages come from elpaca's build directories, which this file puts on
;; the load path itself.  Refreshes and check-offs run against a stub
;; engine that honors the CLI's JSON contract; a few more tests run the
;; real engine when `gco-pkm-query-engine-directory' has it, and the
;; markdown-ts-mode tests need this configuration's tree-sitter grammars.

;;; Code:

(require 'ert)

(eval-and-compile
  (defconst gco-pkm-query-test--config-dir
    (file-name-directory (directory-file-name (file-name-directory (macroexp-file-name))))
    "The Emacs configuration these tests belong to.")
  (add-to-list 'load-path (expand-file-name "lisp" gco-pkm-query-test--config-dir))
  (dolist (dir (file-expand-wildcards
                (expand-file-name "var/elpaca/builds/*" gco-pkm-query-test--config-dir)))
    (add-to-list 'load-path dir)))

;; gco-pkm's defcustoms read this; init-org.el normally provides it.
(defvar my/notes-dir (make-temp-file "gco-pkm-query-test-notes" t))

(require 'autorevert)
(require 'gco-pkm-query)
(require 'gco-pkm-context)

(declare-function markdown-ts--heading-ids "markdown-ts-mode")
(declare-function markdown-ts-mode "markdown-ts-mode")
(declare-function markdown-ts-toggle-checkbox "markdown-ts-mode")

;; As init-org.el hooks it.
(add-hook 'markdown-ts-mode-hook #'gco-pkm-query-maybe-enable)

;;;; Fixtures

(defconst gco-pkm-query-test--page
  "---
title: \"Egypt Eclipse Trip 2027\"
---

# Overview

Totality in Luxor.

# Journal

<!-- query: Egypt OR eclipse -->
## [2026-10-13 Tue · Travel Egypt](../journals/2026-10-13.md#travel-egypt)
We talked about #Egypt plans.

<!-- /query 3f9a2c1e -->

# After

Mine.
"
  "A page with a filled query block on lines 11-15.")

(defvar gco-pkm-query-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'gco-pkm-query-test-toggle)
    (define-key map (kbd "RET") #'gco-pkm-query-test-newline)
    map)
  "Keymap for `gco-pkm-query-test-mode'.")

(defun gco-pkm-query-test-toggle ()
  "Stand-in for markdown-ts-mode's C-c C-c."
  (interactive))

(defun gco-pkm-query-test-newline ()
  "Stand-in for markdown-ts-mode's RET."
  (interactive)
  (newline))

(define-derived-mode gco-pkm-query-test-mode text-mode "QTest"
  "Stand-in for markdown-ts-mode, which also binds C-c C-c and RET.")

(add-hook 'gco-pkm-query-test-mode-hook #'gco-pkm-query-maybe-enable)

(defmacro gco-pkm-query-test--with-buffer (text &rest body)
  "Run BODY in a buffer holding TEXT, with `gco-pkm-query-mode' on."
  (declare (indent 1))
  `(with-temp-buffer
     (gco-pkm-query-test-mode)
     (insert ,text)
     (gco-pkm-query-mode 1)
     (goto-char (point-min))
     ,@body))

(defmacro gco-pkm-query-test--with-notes (files &rest body)
  "Run BODY with `gco-pkm-directory' bound to a new directory holding FILES.
FILES is an alist of (RELATIVE-PATH . CONTENTS).  Notes visited in BODY,
or reverted, come up in `gco-pkm-query-test-mode'."
  (declare (indent 1))
  `(let ((gco-pkm-directory (file-name-as-directory
                             (make-temp-file "gco-pkm-query-notes" t)))
         (auto-mode-alist '(("\\.md\\'" . gco-pkm-query-test-mode)))
         (coding-system-for-write 'utf-8-unix))
     (unwind-protect
         (progn
           (pcase-dolist (`(,rel . ,text) ,files)
             (gco-pkm-query-test--write rel text))
           ,@body)
       (dolist (buf (buffer-list))
         (when-let* ((file (buffer-file-name buf)))
           (when (file-in-directory-p file gco-pkm-directory)
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))
       (delete-directory gco-pkm-directory t))))

(defun gco-pkm-query-test--write (rel text)
  "Write TEXT to REL under `gco-pkm-directory'; return the file name."
  (let ((file (expand-file-name rel gco-pkm-directory)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert text))
    file))

(defun gco-pkm-query-test--visit (rel)
  "Visit REL under `gco-pkm-directory' and make it current."
  (set-buffer (find-file-noselect (expand-file-name rel gco-pkm-directory))))

(defun gco-pkm-query-test--goto (string)
  "Move to the start of the first STRING in the buffer; return point."
  (goto-char (point-min))
  (search-forward string)
  (goto-char (match-beginning 0)))

(defun gco-pkm-query-test--lines ()
  "Return (FIRST-LINE LAST-LINE QUERY) for each block in the buffer."
  (mapcar (lambda (b) (list (gco-pkm-query-block-first-line b)
                            (gco-pkm-query-block-last-line b)
                            (gco-pkm-query-block-query b)))
          (gco-pkm-query-blocks)))

(defun gco-pkm-query-test--line ()
  "Return the text of the line at point."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun gco-pkm-query-test--file-text (rel)
  "Return the text of REL under `gco-pkm-directory', as it is on disk."
  (with-temp-buffer
    (insert-file-contents (expand-file-name rel gco-pkm-directory))
    (buffer-string)))

(defmacro gco-pkm-query-test--read-only (&rest body)
  "Assert that BODY is refused as a change to read-only text."
  `(should-error (progn ,@body) :type 'text-read-only))

(defmacro gco-pkm-query-test--with-markdown-ts (&rest body)
  "Run BODY with markdown-ts-mode and this configuration's grammars, or skip."
  (declare (indent 0))
  `(let ((treesit-extra-load-path
          (cons (expand-file-name "tree-sitter" gco-pkm-query-test--config-dir)
                treesit-extra-load-path)))
     (skip-unless (and (require 'markdown-ts-mode nil t)
                       (treesit-language-available-p 'markdown)
                       (treesit-language-available-p 'markdown-inline)))
     ,@body))

;;;; Finding blocks

(ert-deftest gco-pkm-query-test-blocks ()
  (with-temp-buffer
    (insert gco-pkm-query-test--page)
    (should (equal (gco-pkm-query-test--lines) '((11 15 "Egypt OR eclipse"))))
    (let ((block (car (gco-pkm-query-blocks))))
      (should (gco-pkm-query-block-closed-p block))
      (should (= (gco-pkm-query-block-start block)
                 (gco-pkm-query-test--goto "<!-- query")))
      (should (= (gco-pkm-query-block-end block)
                 (progn (gco-pkm-query-test--goto "<!-- /query") (forward-line 1) (point)))))))

(ert-deftest gco-pkm-query-test-blocks-unclosed ()
  ;; An opening line with another before any closing line is a block of
  ;; its own, with no results yet; so is one at the end of the buffer.
  (with-temp-buffer
    (insert "<!-- query: A -->\ntext\n<!-- query: B -->\nr\n<!-- /query -->\n"
            "after\n<!-- query: C -->\nmore")
    (should (equal (gco-pkm-query-test--lines)
                   '((1 1 "A") (3 5 "B") (7 7 "C"))))
    (should-not (gco-pkm-query-block-closed-p (car (gco-pkm-query-blocks))))))

(ert-deftest gco-pkm-query-test-blocks-fences ()
  (with-temp-buffer
    (insert "```markdown\n<!-- query: example -->\n<!-- /query -->\n```\n"
            "~~~~\n```\n<!-- query: still fenced -->\n~~~\n~~~~~ closes\n"
            "  - item\n      ```\n      x\n<!-- query: indented fence -->\n      ```\n"
            "<!-- query: live -->\n<!-- /query -->\n")
    (should (equal (gco-pkm-query-test--lines) '((15 16 "live"))))))

(ert-deftest gco-pkm-query-test-blocks-strict-markers ()
  ;; Case-sensitive, hashes lowercase and eight digits, at most three
  ;; columns of indentation -- as in the engine.
  (with-temp-buffer
    (insert "<!-- QUERY: no -->\n    <!-- query: indented -->\n"
            "<!--query:tight-->\n<!-- /query ABCDEF12 -->\n<!-- /query abcdef1 -->\n"
            "\t<!-- /query abcdef12 -->\n")
    (should (equal (gco-pkm-query-test--lines) '((3 6 "tight"))))))

(ert-deftest gco-pkm-query-test-block-at ()
  (with-temp-buffer
    (insert gco-pkm-query-test--page)
    (dolist (spot '("<!-- query" "We talked" "<!-- /query"))
      (gco-pkm-query-test--goto spot)
      (should (equal (gco-pkm-query-block-query (gco-pkm-query-block-at))
                     "Egypt OR eclipse")))
    (gco-pkm-query-test--goto "# After")
    (should-not (gco-pkm-query-block-at))))

;;;; Read-only results

(ert-deftest gco-pkm-query-test-results-read-only ()
  (gco-pkm-query-test--with-buffer gco-pkm-query-test--page
    (gco-pkm-query-test--goto "We talked")
    (gco-pkm-query-test--read-only (insert "x"))
    (gco-pkm-query-test--read-only (delete-char 3))
    (gco-pkm-query-test--read-only (kill-line))
    ;; Nothing may land between the query line and its results...
    (gco-pkm-query-test--goto "<!-- query")
    (end-of-line)
    (gco-pkm-query-test--read-only (insert "\n"))
    (gco-pkm-query-test--read-only (delete-char 1))
    ;; ...nor after the closing marker, on its line.
    (gco-pkm-query-test--goto "<!-- /query")
    (end-of-line)
    (gco-pkm-query-test--read-only (insert "x"))
    (gco-pkm-query-test--read-only (delete-char 1))
    ;; A partial region fails whole.
    (gco-pkm-query-test--read-only
     (delete-region (gco-pkm-query-test--goto "Totality")
                    (gco-pkm-query-test--goto "We talked")))
    (should (equal (buffer-string) gco-pkm-query-test--page))))

(ert-deftest gco-pkm-query-test-query-line-editable ()
  (gco-pkm-query-test--with-buffer gco-pkm-query-test--page
    (gco-pkm-query-test--goto "Egypt OR")
    (delete-char 5)
    (insert "Nile")
    (gco-pkm-query-test--goto "<!-- query")
    (insert "Before\n")
    ;; The line after the block is the user's again.
    (gco-pkm-query-test--goto "<!-- /query")
    (forward-line 1)
    (insert "Below\n")
    (should (string-match-p "Before\n<!-- query: Nile OR eclipse -->\n" (buffer-string)))
    (should (string-match-p "3f9a2c1e -->\nBelow\n" (buffer-string)))))

(ert-deftest gco-pkm-query-test-shading ()
  (gco-pkm-query-test--with-buffer gco-pkm-query-test--page
    (let ((shade (seq-find (lambda (ov) (eq (overlay-get ov 'gco-pkm-query) 'shade))
                           (overlays-in (point-min) (point-max)))))
      (should (eq (overlay-get shade 'face) 'gco-pkm-query-results-face))
      (should (= (overlay-start shade) (gco-pkm-query-test--goto "## [2026")))
      (should (= (overlay-end shade)
                 (progn (gco-pkm-query-test--goto "<!-- /query") (forward-line 1) (point)))))
    (gco-pkm-query-mode -1)
    (should-not (seq-some (lambda (ov) (overlay-get ov 'gco-pkm-query))
                          (overlays-in (point-min) (point-max))))
    (gco-pkm-query-test--goto "We talked")
    (insert "x")))

(ert-deftest gco-pkm-query-test-whole-block-deletion ()
  ;; Killing a region that spans the whole block works, so a section can
  ;; be cut or moved with its block.
  (gco-pkm-query-test--with-buffer gco-pkm-query-test--page
    (kill-region (gco-pkm-query-test--goto "# Journal")
                 (gco-pkm-query-test--goto "# After"))
    (should (string-match-p "Luxor.\n\n# After\n" (buffer-string))))
  (gco-pkm-query-test--with-buffer gco-pkm-query-test--page
    (gco-pkm-query-test--goto "We talked")
    (gco-pkm-query-delete-block)
    (should (string-match-p "# Journal\n\n\n# After\n" (buffer-string)))
    (should-not (gco-pkm-query-blocks))
    (should-error (gco-pkm-query-delete-block) :type 'user-error)))

(ert-deftest gco-pkm-query-test-programmatic-changes ()
  (gco-pkm-query-test--with-buffer gco-pkm-query-test--page
    (buffer-enable-undo)
    (undo-boundary)
    (gco-pkm-query-test--goto "We talked")
    (let ((inhibit-read-only t))
      (insert "Refreshed. "))
    (undo-boundary)
    ;; Undo puts back what a refresh or revert replaced.
    (let ((last-command nil))
      (undo))
    (should (equal (buffer-string) gco-pkm-query-test--page))))

(ert-deftest gco-pkm-query-test-rescan-after-edits ()
  (gco-pkm-query-test--with-buffer gco-pkm-query-test--page
    ;; Taking away the query line's marker frees its old results...
    (gco-pkm-query-test--goto "Egypt OR")
    (delete-region (line-beginning-position) (line-end-position))
    (should gco-pkm-query--update-timer)
    (gco-pkm-query--run-update (current-buffer))
    (should-not gco-pkm-query--update-timer)
    (gco-pkm-query-test--goto "We talked")
    (insert "Editable. ")
    ;; ...and markers added around text protect it.
    (gco-pkm-query-test--goto "Mine.")
    (insert "<!-- query: mine -->\n")
    (end-of-line)
    (insert "\n<!-- /query -->")
    (gco-pkm-query--run-update (current-buffer))
    (gco-pkm-query-test--goto "Mine.")
    (gco-pkm-query-test--read-only (insert "x"))))

(ert-deftest gco-pkm-query-test-revert ()
  (let ((two-blocks (string-replace
                     "Mine.\n"
                     "Mine.\n\n<!-- query: Pelorus -->\n## [P](x.md)\n<!-- /query 0123abcd -->\n"
                     gco-pkm-query-test--page))
        ;; Poll the file's time, as without file notification.
        (auto-revert-use-notify nil))
    (gco-pkm-query-test--with-notes `(("pages/egypt.md" . ,gco-pkm-query-test--page))
      (gco-pkm-query-test--visit "pages/egypt.md")
      (should gco-pkm-query-mode)
      ;; The server rewrites the page: auto-revert picks it up, and the
      ;; protection follows the new layout.
      (auto-revert-mode 1)
      (gco-pkm-query-test--write "pages/egypt.md" two-blocks)
      (set-file-times buffer-file-name (time-add nil 10))
      (auto-revert-handler)
      (should (equal (buffer-string) two-blocks))
      (gco-pkm-query-test--goto "## [P]")
      (gco-pkm-query-test--read-only (insert "x"))
      (gco-pkm-query-test--goto "Mine.")
      (insert "Still mine. ")
      ;; A revert that re-runs the major mode re-enables the mode.
      (set-buffer-modified-p nil)
      (revert-buffer t t)
      (should gco-pkm-query-mode)
      (gco-pkm-query-test--goto "We talked")
      (gco-pkm-query-test--read-only (insert "x"))
      ;; Leaving the mode behind leaves no shading or protection behind.
      (fundamental-mode)
      (should-not (seq-some (lambda (ov) (overlay-get ov 'gco-pkm-query))
                            (overlays-in (point-min) (point-max))))
      (insert "x"))))

;;;; Keys

(ert-deftest gco-pkm-query-test-keys ()
  (gco-pkm-query-test--with-buffer
      (concat gco-pkm-query-test--page "\n```\n<!-- query: example -->\n```\n")
    (dolist (spot '("<!-- query: Egypt" "Egypt OR" "We talked" "<!-- /query"))
      (gco-pkm-query-test--goto spot)
      (should (eq (key-binding (kbd "C-c C-c")) #'gco-pkm-query-refresh)))
    (dolist (spot '("Totality" "Mine." "<!-- query: example"))
      (gco-pkm-query-test--goto spot)
      (should (eq (key-binding (kbd "C-c C-c")) #'gco-pkm-query-test-toggle)))
    ;; RET at the end of the closing line opens a line below the block.
    (gco-pkm-query-test--goto "Mine.")
    (should (eq (key-binding (kbd "RET")) #'gco-pkm-query-test-newline))
    (gco-pkm-query-test--goto "<!-- /query")
    (end-of-line)
    (should (eq (key-binding (kbd "RET")) #'gco-pkm-query-newline-after-block))
    (call-interactively (key-binding (kbd "RET")))
    (insert "Typed")
    (should (string-match-p "3f9a2c1e -->\nTyped\n\n# After" (buffer-string)))))

;;;; Refreshing

(defconst gco-pkm-query-test--stub-main
  "import json, re, sys
args = sys.argv[1:]
assert args[:2] == ['refresh-text', '--json'], args
path = args[args.index('--path') + 1]
lines = sys.stdin.read().split('\\n')
out, blocks, i = [], [], 0
while i < len(lines):
    out.append(lines[i])
    m = re.match(r'<!-- query: (.*) -->$', lines[i])
    i += 1
    if m:
        while not lines[i].startswith('<!-- /query'):
            i += 1
        i += 1
        out += ['## [Stub: %s](%s)' % (m.group(1), path), '<!-- /query 0123abcd -->']
        blocks.append({'query': m.group(1), 'entries': 1, 'error': None,
                       'changed': True, 'hand_edited': m.group(1) == 'edited'})
json.dump({'text': '\\n'.join(out), 'changed': True, 'blocks': blocks}, sys.stdout)
"
  "A stand-in for the engine's CLI that honors its JSON contract.
Each block's results become one entry naming the query and --path.")

(defmacro gco-pkm-query-test--with-engine (main &rest body)
  "Run BODY with `gco-pkm-query-engine-directory' holding a stub engine.
MAIN is the Python source of its pkm_bridge/queries/__main__.py."
  (declare (indent 1))
  `(let ((gco-pkm-query-engine-directory (make-temp-file "gco-pkm-query-engine" t)))
     (skip-unless (executable-find gco-pkm-query-python))
     (unwind-protect
         (let ((package (expand-file-name "pkm_bridge/queries" gco-pkm-query-engine-directory)))
           (make-directory package t)
           (with-temp-file (expand-file-name "../__init__.py" package))
           (with-temp-file (expand-file-name "__init__.py" package))
           (with-temp-file (expand-file-name "__main__.py" package) (insert ,main))
           ,@body)
       (delete-directory gco-pkm-query-engine-directory t))))

(ert-deftest gco-pkm-query-test-refresh ()
  (gco-pkm-query-test--with-engine gco-pkm-query-test--stub-main
    (gco-pkm-query-test--with-notes `(("pages/egypt.md" . ,gco-pkm-query-test--page))
      (gco-pkm-query-test--visit "pages/egypt.md")
      (let ((mine (copy-marker (gco-pkm-query-test--goto "Mine."))))
        (gco-pkm-query-test--goto "We talked")
        (should (equal (call-interactively (key-binding (kbd "C-c C-c")))
                       (format-message "Query results updated: `Egypt OR eclipse' 1 entry")))
        (should (string-match-p
                 "<!-- query: Egypt OR eclipse -->\n## \\[Stub: Egypt OR eclipse\\](pages/egypt.md)\n<!-- /query 0123abcd -->\n\n# After"
                 (buffer-string)))
        ;; Edited in place: markers survive, and the buffer is modified, not saved.
        (should (equal (buffer-substring mine (+ mine 5)) "Mine."))
        (should (buffer-modified-p))
        (should (equal (with-temp-buffer
                         (insert-file-contents (buffer-file-name (marker-buffer mine)))
                         (buffer-string))
                       gco-pkm-query-test--page))
        ;; The new results are protected like the old.
        (gco-pkm-query-test--goto "## [Stub")
        (gco-pkm-query-test--read-only (insert "x"))))))

(ert-deftest gco-pkm-query-test-refresh-summary ()
  (should (equal (gco-pkm-query--summary nil nil) "No query blocks in this note"))
  (should (equal (gco-pkm-query--summary
                  '(((query . "Egypt") (entries . 7) (error) (changed . t) (hand_edited))
                    ((query . "foo (") (entries . 0) (error . "unbalanced (")
                     (changed) (hand_edited))
                    ((query . "x") (entries . 1) (error) (changed . t) (hand_edited . t)))
                  t)
                 (format-message "Query results updated: `Egypt' 7 entries; \
`foo (' error: unbalanced (; `x' 1 entry (hand edits replaced)"))))

(ert-deftest gco-pkm-query-test-refresh-errors ()
  (gco-pkm-query-test--with-notes `(("pages/egypt.md" . ,gco-pkm-query-test--page))
    (gco-pkm-query-test--visit "pages/egypt.md")
    (let ((gco-pkm-query-python "no-such-python-gco"))
      (should-error (gco-pkm-query-refresh) :type 'user-error))
    (let ((gco-pkm-query-engine-directory (make-temp-file "no-engine" t)))
      (unwind-protect
          (should-error (gco-pkm-query-refresh) :type 'user-error)
        (delete-directory gco-pkm-query-engine-directory t)))
    (gco-pkm-query-test--with-engine
        "import sys\nsys.stderr.write('Traceback\\nValueError: boom\\n')\nsys.exit(1)\n"
      (should (string-match-p "boom" (cadr (should-error (gco-pkm-query-refresh))))))
    (should (equal (buffer-string) gco-pkm-query-test--page)))
  (with-temp-buffer
    (should-error (gco-pkm-query-refresh) :type 'user-error)))

(ert-deftest gco-pkm-query-test-title-query ()
  (dolist (case '(("Egypt Eclipse Trip 2027" . "Egypt OR Eclipse")
                  ("Long Now Boston Audio Setup" . "Long OR Now OR Boston OR Audio")
                  ("Travel or not, travel" . "Travel")
                  ("2026-10-13" . "")))
    (with-temp-buffer
      (insert (format "---\ntitle: \"%s\"\n---\n" (car case)))
      (let ((buffer-file-name "/tmp/note.md"))
        (should (equal (gco-pkm-query--title-query) (cdr case)))))))

(ert-deftest gco-pkm-query-test-insert-query ()
  (gco-pkm-query-test--with-engine gco-pkm-query-test--stub-main
    (gco-pkm-query-test--with-notes
        '(("pages/egypt.md" . "---\ntitle: \"Egypt\"\n---\n\n# Overview\n\nText.\n\n## Details\n\nMore.\n\n# Packing\n\nList.\n"))
      (gco-pkm-query-test--visit "pages/egypt.md")
      ;; A sibling of the section at point, after its subtree.
      (gco-pkm-query-test--goto "More.")
      (gco-pkm-insert-query "Egypt")
      (should (string-match-p
               "More.\n\n## Journal\n\n<!-- query: Egypt -->\n## \\[Stub: Egypt\\](pages/egypt.md)\n<!-- /query 0123abcd -->\n\n# Packing\n"
               (buffer-string)))
      (should (looking-at-p "<!-- query: Egypt -->"))
      ;; Above the first heading: the end of the note, at its top level.
      (goto-char (point-min))
      (gco-pkm-insert-query "Nile")
      (should (string-match-p
               "List.\n\n# Journal\n\n<!-- query: Nile -->\n## \\[Stub: Nile\\](pages/egypt.md)\n<!-- /query 0123abcd -->\n\\'"
               (buffer-string)))
      (should-error (gco-pkm-insert-query " ") :type 'user-error))))

(defun gco-pkm-query-test--real-engine-p ()
  "Non-nil if the real query engine is where `gco-pkm-query-engine-directory' says."
  (and (executable-find gco-pkm-query-python)
       (file-directory-p (expand-file-name "pkm_bridge/queries"
                                           gco-pkm-query-engine-directory))))

(ert-deftest gco-pkm-query-test-real-engine ()
  "Refresh against the real engine, when it is installed."
  (skip-unless (gco-pkm-query-test--real-engine-p))
  (gco-pkm-query-test--with-notes
      '(("journals/2026-10-13.md" . "---\ntitle: \"2026-10-13\"\n---\n\n# Travel Egypt\nWe booked the Luxor hotel.\n")
        ("pages/egypt.md" . "---\ntitle: \"Egypt\"\n---\n\n# Journal\n\n<!-- query: Egypt -->\n<!-- /query -->\n"))
    (gco-pkm-query-test--visit "pages/egypt.md")
    (gco-pkm-query-test--goto "<!-- query")
    (gco-pkm-query-refresh)
    (should (string-match-p
             (concat "<!-- query: Egypt -->\n\n?"
                     "## \\[2026-10-13 Tue · Travel Egypt\\](\\.\\./journals/2026-10-13\\.md#travel-egypt)\n"
                     "We booked the Luxor hotel\\.\n\n?"
                     "<!-- /query [0-9a-f]\\{8\\} -->\n")
             (buffer-string)))
    ;; A second refresh finds nothing to change.
    (let ((text (buffer-string)))
      (gco-pkm-query-refresh)
      (should (equal (buffer-string) text)))))

;;;; Tasks

(defconst gco-pkm-query-test--task-lines
  '(("- [ ] Buy milk" "- [x] Buy milk ✅ 2026-10-13" "- [ ] Buy milk")
    ("- [x] Buy milk ✅ 2026-10-01" "- [x] Buy milk ✅ 2026-10-13" "- [ ] Buy milk")
    ("- [X] Shout" "- [x] Shout ✅ 2026-10-13" "- [ ] Shout")
    ("- [ ] Spaces after   " "- [x] Spaces after ✅ 2026-10-13" "- [ ] Spaces after   ")
    ("- [x] Tabs\t✅2026-10-01 \t" "- [x] Tabs ✅ 2026-10-13" "- [ ] Tabs \t")
    ("- [ ] Trailing tab\t" "- [x] Trailing tab ✅ 2026-10-13" "- [ ] Trailing tab\t")
    ("- [ ] No-break space\u00a0" "- [x] No-break space\u00a0 ✅ 2026-10-13"
     "- [ ] No-break space\u00a0")
    ("- [x] No-break\u00a0 ✅ 2026-10-01" "- [x] No-break\u00a0 ✅ 2026-10-13" "- [ ] No-break\u00a0")
    ("    * [ ] Indented" "    * [x] Indented ✅ 2026-10-13" "    * [ ] Indented")
    ("\t+ [x] Tab-indented ✅ 2026-10-01" "\t+ [x] Tab-indented ✅ 2026-10-13"
     "\t+ [ ] Tab-indented")
    ("12. [x] Ordered ✅ 2026-01-02" "12. [x] Ordered ✅ 2026-10-13" "12. [ ] Ordered")
    ("3) [ ]\tParenthesized" "3) [x]\tParenthesized ✅ 2026-10-13" "3) [ ]\tParenthesized")
    ("- [ ]" "- [x] ✅ 2026-10-13" "- [ ]")
    ("- [ ] See ✅ 2026-10-01 there" "- [x] See ✅ 2026-10-01 there ✅ 2026-10-13"
     "- [ ] See ✅ 2026-10-01 there")
    ("- [x] Twice ✅ 2026-10-01 ✅ 2026-10-02" "- [x] Twice ✅ 2026-10-01 ✅ 2026-10-13"
     "- [ ] Twice ✅ 2026-10-01"))
  "Task lines, each followed by itself checked on 2026-10-13, then opened.")

(defconst gco-pkm-query-test--not-tasks
  '("- [ ]x" "-[ ] Tight" "- [y] Other" "- [] Empty" "> - [ ] Quoted" "Text [ ]"
    "1234567890. [ ] Too long")
  "Lines that only look like tasks.")

(defun gco-pkm-query-test--set-state (line checked)
  "Return LINE set to CHECKED on 2026-10-13, or nil if it isn't a task."
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    (and (gco-pkm-query-set-task-state checked "2026-10-13")
         (buffer-string))))

(ert-deftest gco-pkm-query-test-set-task-state ()
  (pcase-dolist (`(,line ,checked ,open) gco-pkm-query-test--task-lines)
    (should (equal (gco-pkm-query-test--set-state line t) checked))
    (should (equal (gco-pkm-query-test--set-state line nil) open)))
  (dolist (line gco-pkm-query-test--not-tasks)
    (should-not (gco-pkm-query-test--set-state line t)))
  (with-temp-buffer
    (insert "# PKM\n- [ ] Buy milk  \n  at the shop\n")
    ;; From anywhere on the line, and only the line changes; point stays put.
    (gco-pkm-query-test--goto "milk")
    (should (gco-pkm-query-set-task-state t "2026-10-13"))
    (should (equal (buffer-string) "# PKM\n- [x] Buy milk ✅ 2026-10-13\n  at the shop\n"))
    (should (looking-at-p "milk ✅"))
    ;; Already so, nothing changes, not even the modified flag.
    (set-buffer-modified-p nil)
    (gco-pkm-query-set-task-state t "2026-10-13")
    (should-not (buffer-modified-p))
    ;; Today, by default.
    (gco-pkm-query-set-task-state t)
    (should (string-match-p (concat "Buy milk ✅ " (format-time-string "%F") "\n")
                            (buffer-string)))))

(defun gco-pkm-query-test--python (script input)
  "Run Python SCRIPT, able to import the engine; return the JSON it prints.
INPUT goes to its standard input as JSON."
  (with-temp-buffer
    (json-insert input)
    (let* ((default-directory (file-name-as-directory
                               (expand-file-name gco-pkm-query-engine-directory)))
           (process-environment (cons (concat "PYTHONPATH=" default-directory)
                                      process-environment))
           (coding-system-for-write 'utf-8-unix)
           (coding-system-for-read 'utf-8-unix)
           (status (call-process-region (point-min) (point-max) gco-pkm-query-python
                                        t t nil "-c" script)))
      (unless (eql status 0)
        (error "Python failed: %s" (buffer-string)))
      (goto-char (point-min))
      (json-parse-buffer :array-type 'list :null-object nil))))

(ert-deftest gco-pkm-query-test-set-task-state-engine ()
  "Ticks come out byte for byte as the engine's `set_task_state' makes them."
  (skip-unless (gco-pkm-query-test--real-engine-p))
  (let ((lines (append (mapcar #'car gco-pkm-query-test--task-lines)
                       gco-pkm-query-test--not-tasks)))
    (should (equal (gco-pkm-query-test--python "\
import json, sys
from datetime import date
from pkm_bridge.queries import set_task_state
def state(line, checked):
    try:
        return set_task_state(line, checked, date(2026, 10, 13))
    except ValueError:
        return None
json.dump([[state(l, True), state(l, False)] for l in json.load(sys.stdin.buffer)], sys.stdout)
"
                                               (vconcat lines))
                   (mapcar (lambda (line)
                             (list (gco-pkm-query-test--set-state line t)
                                   (gco-pkm-query-test--set-state line nil)))
                           lines)))))

(ert-deftest gco-pkm-query-test-toggle-stamps ()
  "Ticking a task in a note stamps it, and one undo takes the tick back."
  (gco-pkm-query-test--with-markdown-ts
    (gco-pkm-query-test--with-notes
        '(("journals/2026-10-12.md"
           . "# PKM\n- [ ] Buy milk\n  at the shop\n- [X] Call Bob ✅ 2026-10-01\n"))
      (let ((auto-mode-alist '(("\\.md\\'" . markdown-ts-mode)))
            (today (format-time-string "%F")))
        (gco-pkm-query-test--visit "journals/2026-10-12.md")
        (should gco-pkm-query-mode)
        (gco-pkm-query-test--goto "Buy milk")
        (should (eq (key-binding (kbd "C-c C-c")) #'gco-pkm-query-toggle-checkbox))
        (let ((before (buffer-string)))
          (undo-boundary)
          (call-interactively (key-binding (kbd "C-c C-c")))
          (should (equal (gco-pkm-query-test--line) (concat "- [x] Buy milk ✅ " today)))
          (undo-boundary)
          (let ((last-command nil))
            (undo))
          (should (equal (buffer-string) before)))
        ;; From a continuation line, the stamp goes on the item it toggles.
        (gco-pkm-query-test--goto "at the shop")
        (call-interactively (key-binding (kbd "C-c C-c")))
        (should (string-match-p (concat "^- \\[x\\] Buy milk ✅ " today "\n  at the shop\n")
                                (buffer-string)))
        ;; Reopening takes the stamp off, and X is as done as x.
        (gco-pkm-query-test--goto "Call Bob")
        (call-interactively (key-binding (kbd "C-c C-c")))
        (should (equal (gco-pkm-query-test--line) "- [ ] Call Bob"))))))

(ert-deftest gco-pkm-query-test-toggle-elsewhere ()
  "Outside the notes, C-c C-c is markdown-ts-mode's own toggle, with no stamp."
  (gco-pkm-query-test--with-markdown-ts
    (with-temp-buffer
      (insert "- [ ] Buy milk\n")
      (markdown-ts-mode)
      (goto-char (point-min))
      (should-not gco-pkm-query-mode)
      (should (eq (key-binding (kbd "C-c C-c")) #'markdown-ts-toggle-checkbox))
      (call-interactively (key-binding (kbd "C-c C-c")))
      (should (equal (buffer-string) "- [x] Buy milk\n")))))

(defconst gco-pkm-query-test--tasks-journal
  "---
title: \"2026-10-12\"
---

# PKM
- [ ] Auto-updating queries on pages
- [x] Write the design ✅ 2026-10-11
"
  "A journal whose tasks the to-do page copies.")

(defconst gco-pkm-query-test--todo
  "---
title: \"ToDo\"
---

# This week
<!-- query: PKM is:todo -->

## [2026-10-12 Mon · PKM](../journals/2026-10-12.md#pkm)
- [ ] Auto-updating queries on pages
- [x] Write the design ✅ 2026-10-11
- [ ] Gone from the journal
Not a task.

<!-- /query 86ba1cd4 -->

# Mine
- [ ] Buy milk
"
  "A to-do page holding copies of tasks, the first on line 9.")

(defconst gco-pkm-query-test--task-stub-main
  (concat "import json, os, re, sys
sys.stdin.reconfigure(encoding='utf-8')
args = sys.argv[1:]
if args[0] in ('locate-task', 'check-task'):
    opt = lambda name: args[args.index(name) + 1]
    request = json.load(sys.stdin) if args[0] == 'check-task' else {'page': sys.stdin.read()}
    with open(os.path.join(os.path.dirname(__file__), 'request.json'), 'w') as f:
        json.dump(dict(request, args=args), f)
    page, n = request['page'].split('\\n'), int(opt('--line')) - 1
    stamp = r'(?: \\u2705 [-0-9]+)?$'
    task = re.match(r'- \\[[ xX]\\] (.*?)' + stamp, page[n])
    entry = re.compile(r'## \\[.*\\]\\(\\.\\./([^#)]+)')
    links = [m.group(1) for m in map(entry.match, page[:n]) if m]
    if not (task and links):
        result = {'error': 'not a copied task'}
    elif args[0] == 'locate-task':
        result = {'source_path': links[-1]}
    else:
        source = request['source']
        if source is None:
            with open(os.path.join(opt('--root'), links[-1]), encoding='utf-8') as f:
                source = f.read()
        lines, item = source.split('\\n'), task.group(1)
        same = [i for i, line in enumerate(lines)
                if re.match(r'- \\[[ xX]\\] ' + re.escape(item) + stamp, line)]
        if not same:
            result = {'error': 'this copy is out of date; refresh it'}
        else:
            ticked = ('- [x] %s \\u2705 2026-10-13' if '--check' in args else '- [ ] %s') % item
            lines[same[0]] = page[n] = ticked
            result = {'page': '\\n'.join(page), 'source_path': links[-1],
                      'source': '\\n'.join(lines), 'source_line': same[0] + 1, 'item': item}
    json.dump(result, sys.stdout)
    sys.exit(0)
" gco-pkm-query-test--stub-main)
  "The stub engine, answering `locate-task' and `check-task' too.
It finds a copy's source by the entry heading above it, ticks the first
source item with the copy's text, stamping it 2026-10-13, and keeps the
last request it got, with its arguments, in request.json beside it.")

(defun gco-pkm-query-test--stub-request ()
  "Return the last request the stub engine got, with :null for null."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "pkm_bridge/queries/request.json"
                                            gco-pkm-query-engine-directory))
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(defun gco-pkm-query-test--visiting (rel)
  "Return the buffer visiting REL under `gco-pkm-directory', or nil."
  (find-buffer-visiting (expand-file-name rel gco-pkm-directory)))

(ert-deftest gco-pkm-query-test-check-task ()
  "Checking a copy off ticks its source too, saving the notes that were clean."
  (gco-pkm-query-test--with-engine gco-pkm-query-test--task-stub-main
    (gco-pkm-query-test--with-notes
        `(("journals/2026-10-12.md" . ,gco-pkm-query-test--tasks-journal)
          ("pages/todo.md" . ,gco-pkm-query-test--todo))
      (let ((journal "journals/2026-10-12.md"))
        (gco-pkm-query-test--visit "pages/todo.md")
        ;; A clean page and an unvisited journal: the engine reads the
        ;; journal itself, both are saved, and the journal isn't left open.
        (gco-pkm-query-test--goto "Auto-updating")
        (should (equal (gco-pkm-query-check-task) "Checked off in journals/2026-10-12.md"))
        (should (looking-at-p "Auto-updating queries on pages ✅ 2026-10-13$"))
        (let-alist (gco-pkm-query-test--stub-request)
          (should (eq .source :null))
          (should (equal .page gco-pkm-query-test--todo))
          (should (equal (cadr (member "--line" .args)) "9"))
          (should (member "--check" .args)))
        (should-not (buffer-modified-p))
        (should (equal (gco-pkm-query-test--file-text "pages/todo.md") (buffer-string)))
        (should (string-match-p "^- \\[x\\] Auto-updating queries on pages ✅ 2026-10-13\n"
                                (gco-pkm-query-test--file-text journal)))
        (should-not (gco-pkm-query-test--visiting journal))
        ;; A copy the engine can't place is refused, and nothing changes.
        (let ((page (buffer-string)))
          (gco-pkm-query-test--goto "Gone from")
          (should (equal (cadr (should-error (gco-pkm-query-check-task) :type 'user-error))
                         "this copy is out of date; refresh it"))
          (gco-pkm-query-test--goto "Not a task")
          (should-error (gco-pkm-query-check-task) :type 'user-error)
          (should (equal (buffer-string) page))
          (should-not (gco-pkm-query-test--visiting journal)))
        ;; Unsaved edits in both: the engine gets the journal's buffer, and
        ;; neither note is saved.
        (let ((saved-page (buffer-string))
              (saved-journal (gco-pkm-query-test--file-text journal)))
          (with-current-buffer (find-file-noselect (expand-file-name journal gco-pkm-directory))
            (goto-char (point-max))
            (insert "Unsaved.\n"))
          (gco-pkm-query-test--goto "Buy milk")
          (insert "Unsaved: ")
          (gco-pkm-query-test--goto "Write the design")
          (should (equal (gco-pkm-query-check-task) "Reopened in journals/2026-10-12.md"))
          (should (equal (gco-pkm-query-test--line) "- [ ] Write the design"))
          (let-alist (gco-pkm-query-test--stub-request)
            (should (string-suffix-p "\nUnsaved.\n" .source))
            (should (member "--uncheck" .args)))
          (should (buffer-modified-p))
          (should (equal (gco-pkm-query-test--file-text "pages/todo.md") saved-page))
          (with-current-buffer (gco-pkm-query-test--visiting journal)
            (should (buffer-modified-p))
            (should (string-suffix-p "- [ ] Write the design\nUnsaved.\n" (buffer-string))))
          (should (equal (gco-pkm-query-test--file-text journal) saved-journal)))))))

(ert-deftest gco-pkm-query-test-task-keys ()
  "C-c C-c checks off a copied task, and still refreshes elsewhere in a block."
  (gco-pkm-query-test--with-buffer gco-pkm-query-test--todo
    (dolist (spot '("Auto-updating" "Write the design"))
      (gco-pkm-query-test--goto spot)
      (should (eq (key-binding (kbd "C-c C-c")) #'gco-pkm-query-check-task)))
    (dolist (spot '("<!-- query" "## [2026" "Not a task" "<!-- /query"))
      (gco-pkm-query-test--goto spot)
      (should (eq (key-binding (kbd "C-c C-c")) #'gco-pkm-query-refresh)))
    (gco-pkm-query-test--goto "Buy milk")
    (should (eq (key-binding (kbd "C-c C-c")) #'gco-pkm-query-test-toggle))))

(ert-deftest gco-pkm-query-test-task-dispatch ()
  "In markdown-ts-mode, C-c C-c checks off a copy, refreshes a block, or ticks."
  (gco-pkm-query-test--with-markdown-ts
    (gco-pkm-query-test--with-engine gco-pkm-query-test--task-stub-main
      (gco-pkm-query-test--with-notes
          `(("journals/2026-10-12.md" . ,gco-pkm-query-test--tasks-journal)
            ("pages/todo.md" . ,gco-pkm-query-test--todo))
        (let ((auto-mode-alist '(("\\.md\\'" . markdown-ts-mode))))
          (gco-pkm-query-test--visit "pages/todo.md")
          (gco-pkm-query-test--goto "Auto-updating")
          (call-interactively (key-binding (kbd "C-c C-c")))
          (should (equal (gco-pkm-query-test--line)
                         "- [x] Auto-updating queries on pages ✅ 2026-10-13"))
          (should (string-match-p "^- \\[x\\] Auto-updating queries on pages ✅ 2026-10-13\n"
                                  (gco-pkm-query-test--file-text "journals/2026-10-12.md")))
          (gco-pkm-query-test--goto "Buy milk")
          (call-interactively (key-binding (kbd "C-c C-c")))
          (should (equal (gco-pkm-query-test--line)
                         (concat "- [x] Buy milk ✅ " (format-time-string "%F"))))
          (gco-pkm-query-test--goto "## [2026")
          (call-interactively (key-binding (kbd "C-c C-c")))
          (should (string-match-p (concat "<!-- query: PKM is:todo -->\n"
                                          "## \\[Stub: PKM is:todo\\](pages/todo.md)\n"
                                          "<!-- /query 0123abcd -->\n")
                                  (buffer-string))))))))

(ert-deftest gco-pkm-query-test-real-engine-tasks ()
  "Check a task off through a page and reopen it, with the real engine."
  (skip-unless (gco-pkm-query-test--real-engine-p))
  (gco-pkm-query-test--with-notes
      '(("journals/2026-10-12.md"
         . "---\ntitle: \"2026-10-12\"\n---\n\n# PKM\n- [ ] Auto-updating queries on pages\n- [ ] Try markdown-ts-appear\n")
        ("pages/todo.md"
         . "---\ntitle: \"ToDo\"\n---\n\n# This week\n<!-- query: PKM is:todo -->\n\n# Flat\n<!-- query: PKM is:todo show:list -->\n"))
    (let ((today (format-time-string "%F"))
          (journal "journals/2026-10-12.md"))
      (gco-pkm-query-test--visit "pages/todo.md")
      (gco-pkm-query-refresh)
      (save-buffer)
      (gco-pkm-query-test--goto "- [ ] Auto-updating")
      (should (equal (call-interactively (key-binding (kbd "C-c C-c")))
                     "Checked off in journals/2026-10-12.md"))
      (should (equal (gco-pkm-query-test--line)
                     (concat "- [x] Auto-updating queries on pages ✅ " today)))
      (should (string-match-p (concat "^- \\[x\\] Auto-updating queries on pages ✅ " today "\n")
                              (gco-pkm-query-test--file-text journal)))
      (should-not (buffer-modified-p))
      ;; In a show:list item, the stamp goes before the source link.
      (gco-pkm-query-test--goto "Try markdown-ts-appear · [")
      (call-interactively (key-binding (kbd "C-c C-c")))
      (should (string-prefix-p
               (concat "- [x] Try markdown-ts-appear ✅ " today " · [2026-10-12 PKM](")
               (gco-pkm-query-test--line)))
      (should (string-match-p (concat "^- \\[x\\] Try markdown-ts-appear ✅ " today "\n")
                              (gco-pkm-query-test--file-text journal)))
      (gco-pkm-query-test--goto "- [x] Auto-updating")
      (should (equal (call-interactively (key-binding (kbd "C-c C-c")))
                     "Reopened in journals/2026-10-12.md"))
      (should (string-match-p "^- \\[ \\] Auto-updating queries on pages\n"
                              (gco-pkm-query-test--file-text journal)))
      ;; Once its item is gone from the journal, a copy is refused.
      (gco-pkm-query-test--write journal "---\ntitle: \"2026-10-12\"\n---\n\n# PKM\n")
      (gco-pkm-query-test--goto "- [ ] Auto-updating")
      (should (string-match-p "out of date"
                              (cadr (should-error (call-interactively
                                                   (key-binding (kbd "C-c C-c")))
                                                  :type 'user-error)))))))

;;;; Heading anchors

(defconst gco-pkm-query-test--journal
  "---
title: \"2026-10-13\"
# a YAML comment, not a heading
---

# [OpenFX](../pages/openfx.md) TSC meeting

## Notes ^n1

```sh
# fenced, not a heading
```

# Notes

# C# ##

<!-- query: travel -->
## [2026-10-01 Thu · Notes](x.md#notes)
<!-- /query 0123abcd -->
"
  "A journal whose headings test anchor making.")

(ert-deftest gco-pkm-query-test-heading-anchors ()
  (with-temp-buffer
    (insert gco-pkm-query-test--journal)
    (let (anchors)
      (maphash (lambda (anchor pos) (push (cons anchor (line-number-at-pos pos)) anchors))
               (gco-pkm-query--heading-anchors))
      ;; Links flattened, `^anchor' targets dropped, repeats numbered;
      ;; frontmatter and fences don't count, but query results do, as
      ;; editors count them when numbering repeats.
      (should (equal (sort anchors (lambda (a b) (< (cdr a) (cdr b))))
                     '(("openfx-tsc-meeting" . 6) ("notes" . 8)
                       ("notes-1" . 14) ("c" . 16)
                       ("2026-10-01-thu-·-notes" . 19)))))))

(ert-deftest gco-pkm-query-test-heading-ids-advice ()
  (with-temp-buffer
    (insert gco-pkm-query-test--journal)
    (let ((ids (make-hash-table :test #'equal)))
      (puthash "notes" 1 ids)               ; markdown-ts's own entries win
      (should (eq (gco-pkm-query--add-heading-anchors ids) ids))
      (should (equal (gethash "notes" ids) 1))
      (should (equal (line-number-at-pos (gethash "openfx-tsc-meeting" ids)) 6)))
    ;; Anything but a table of positions comes back untouched.
    (should (equal (gco-pkm-query--add-heading-anchors '(a b)) '(a b)))
    (let ((odd (make-hash-table :test #'equal)))
      (puthash "x" '(1 . 2) odd)
      (gco-pkm-query--add-heading-anchors odd)
      (should (= (hash-table-count odd) 1)))))

(ert-deftest gco-pkm-query-test-anchor-links ()
  "Links to headings with links in them land there, in markdown-ts-mode."
  (gco-pkm-query-test--with-markdown-ts
    (gco-pkm-query-test--with-notes
        `(("journals/2026-10-13.md" . ,gco-pkm-query-test--journal)
          ("pages/x.md" . "# Page\n\nSee [the meeting](../journals/2026-10-13.md#openfx-tsc-meeting).\n"))
      (let ((auto-mode-alist '(("\\.md\\'" . markdown-ts-mode))))
        (gco-pkm-query-test--visit "journals/2026-10-13.md")
        (should (equal (line-number-at-pos (gethash "openfx-tsc-meeting"
                                                    (markdown-ts--heading-ids)))
                       6))
        (gco-pkm-query-test--visit "pages/x.md")
        (font-lock-ensure)
        (push-button (gco-pkm-query-test--goto "the meeting"))
        (should (equal (file-name-nondirectory buffer-file-name) "2026-10-13.md"))
        (should (looking-at-p "# \\[OpenFX\\]"))))))

;;;; Sidebar

(defconst gco-pkm-query-test--corpus
  `(("pages/egypt.md" . ,gco-pkm-query-test--page)
    ("journals/2026-10-13.md"
     . "---\ntitle: \"2026-10-13\"\n---\n\n# Travel Egypt\nWe booked the Luxor hotel.\n")
    ("journals/2026-10-14.md"
     . "---\ntitle: \"2026-10-14\"\n---\n\n# Travel Egypt\nVisa paperwork.\n")
    ("pages/travel.md" . "---\ntitle: \"Travel\"\n---\n\n# Places\n\nSomewhere.\n")
    ("pages/syntax.md"
     . "---\ntitle: \"Query syntax\"\n---\n\n```\n<!-- query: Egypt -->\n```\n\ntitle: Egypt, in prose\n"))
  "Notes for the sidebar tests: a page with a block, two journals, a page
without a block, and a page that only describes the syntax.")

(defun gco-pkm-query-test--entry (rel line kind)
  "Make a parsed rg entry for LINE of REL, of KIND."
  (let ((file (expand-file-name rel gco-pkm-directory)))
    (list :file file :line line :match "" :context "" :kind kind
          :date (gco-pkm-context--entry-date file))))

(defun gco-pkm-query-test--summarize (entries)
  "Return (REL LINE KIND MATCH) for each of ENTRIES."
  (mapcar (lambda (e) (list (file-relative-name (plist-get e :file) gco-pkm-directory)
                            (plist-get e :line) (plist-get e :kind) (plist-get e :match)))
          entries))

(ert-deftest gco-pkm-query-test-sidebar-classifier ()
  (let ((classifiers (cdr (gco-pkm-context--build-pattern
                           "Travel Egypt" '("travel" "egypt") nil))))
    (dolist (case '(("<!-- query: Egypt OR eclipse -->" . page)
                    ("title: \"Egypt Eclipse Trip 2027\"" . page)
                    ("  <!-- query: text:travel -->" . page)
                    ("# Travel Egypt" . exact)
                    ("## Travel plans" . word)
                    ("<!-- query: Pelorus -->" . word)))
      (should (eq (gco-pkm-context--classify (car case) classifiers) (cdr case))))))

(ert-deftest gco-pkm-query-test-sidebar-filter-copies ()
  (gco-pkm-query-test--with-notes gco-pkm-query-test--corpus
    (let ((entries (list (gco-pkm-query-test--entry "pages/egypt.md" 6 'word)
                         (gco-pkm-query-test--entry "pages/egypt.md" 11 'word)
                         (gco-pkm-query-test--entry "pages/egypt.md" 12 'word)
                         (gco-pkm-query-test--entry "pages/egypt.md" 13 'tag:egypt)
                         (gco-pkm-query-test--entry "pages/egypt.md" 15 'word)
                         (gco-pkm-query-test--entry "pages/egypt.md" 17 'word)
                         (gco-pkm-query-test--entry "pages/egypt.md" 11 'page)
                         (gco-pkm-query-test--entry "journals/2026-10-13.md" 5 'exact))))
      (should (equal (mapcar #'cdr (mapcar #'butlast (gco-pkm-query-test--summarize
                                                      (gco-pkm-context--filter-copies entries))))
                     '((6 word) (17 word) (11 page) (5 exact)))))
    ;; Block ranges are cached until the file changes.
    (let ((file (expand-file-name "pages/egypt.md" gco-pkm-directory)))
      (should (gco-pkm-context--in-query-block-p file 12))
      (gco-pkm-query-test--write "pages/egypt.md" "No blocks now.\n")
      (set-file-times file (time-add nil 10))
      (should-not (gco-pkm-context--in-query-block-p file 12)))))

(ert-deftest gco-pkm-query-test-sidebar-page-hits ()
  (gco-pkm-query-test--with-notes gco-pkm-query-test--corpus
    (let* ((entries (list (gco-pkm-query-test--entry "pages/egypt.md" 2 'page)
                          (gco-pkm-query-test--entry "pages/egypt.md" 11 'page)
                          (gco-pkm-query-test--entry "pages/travel.md" 2 'page)
                          (gco-pkm-query-test--entry "pages/syntax.md" 6 'page)
                          (gco-pkm-query-test--entry "pages/syntax.md" 9 'page)
                          (gco-pkm-query-test--entry "journals/2026-10-13.md" 5 'exact)))
           (hits (gco-pkm-context--page-hits entries nil))
           (egypt (seq-find (lambda (e) (string-suffix-p "egypt.md" (plist-get e :file))) hits)))
      ;; One hit per real page, titled, landing on the query line if any.
      (should (equal (sort (gco-pkm-query-test--summarize hits)
                           (lambda (a b) (string< (car a) (car b))))
                     '(("journals/2026-10-13.md" 5 exact "")
                       ("pages/egypt.md" 11 page "Egypt Eclipse Trip 2027")
                       ("pages/travel.md" 2 page "Travel"))))
      (should (equal (plist-get egypt :context) "query: Egypt OR eclipse"))
      ;; Never the page you are on.
      (should-not (seq-some (lambda (e) (eq (plist-get e :kind) 'page))
                            (gco-pkm-context--page-hits
                             (list (car entries))
                             (expand-file-name "pages/egypt.md" gco-pkm-directory)))))))

(ert-deftest gco-pkm-query-test-sidebar-end-to-end ()
  "One rg run, as the sidebar makes it, through to the groups it shows."
  (skip-unless (executable-find gco-inline-tags-rg-exe))
  (gco-pkm-query-test--with-notes gco-pkm-query-test--corpus
    (let* ((built (gco-pkm-context--build-pattern "Travel Egypt" '("travel" "egypt") nil))
           (source (expand-file-name "journals/2026-10-14.md" gco-pkm-directory))
           (done nil)
           (groups nil))
      ;; Both pages are older than the journal hit, yet they come first.
      (set-file-times (expand-file-name "pages/egypt.md" gco-pkm-directory)
                      (encode-time '(0 0 0 2 1 2020 nil -1 nil)))
      (set-file-times (expand-file-name "pages/travel.md" gco-pkm-directory)
                      (encode-time '(0 0 0 1 1 2020 nil -1 nil)))
      (gco-pkm-context--launch (car built) (cdr built) gco-pkm-directory
                               (lambda (entries)
                                 (setq groups (gco-pkm-context--process entries source 5)
                                       done t)))
      (with-timeout (10 (error "rg did not finish"))
        (while (not done) (accept-process-output nil 0.05)))
      ;; Pages first, newest first, then the rest.  Nothing from inside the
      ;; Egypt block, the fenced example, or the heading being edited.
      (should (equal (gco-pkm-query-test--summarize groups)
                     '(("pages/egypt.md" 11 page "Egypt Eclipse Trip 2027")
                       ("pages/travel.md" 2 page "Travel")
                       ("journals/2026-10-13.md" 5 exact "# Travel Egypt")))))))

(provide 'gco-pkm-query-test)
;;; gco-pkm-query-test.el ends here
