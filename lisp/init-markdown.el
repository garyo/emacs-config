;;; init-markdown.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package visual-fill-column)  ; fill to column in visual-line-mode
(use-package adaptive-wrap)  ; "adaptive" wrapping in visual-line-mode

(defun my-markdown-mode-setup ()
  "Common setup for markdown-mode and markdown-ts-mode."
  (mixed-pitch-mode 1)
  (visual-line-mode 1)
  (setq line-spacing 3)
  (adaptive-wrap-prefix-mode 1))

(defun my-markdown-run-pandoc (begin end output-buffer)
  "Run pandoc on the region, surfacing stderr on failure.
Used as `markdown-command' so `markdown-export' reports pandoc's actual
error message (e.g. parse error with line/column) instead of just an
opaque exit code."
  (let ((stderr-file (make-temp-file "pandoc-stderr-")))
    (unwind-protect
        (let ((exit-code (call-process-region
                          begin end "pandoc" nil
                          (list output-buffer stderr-file) nil)))
          (unless (eq exit-code 0)
            (let ((stderr (with-temp-buffer
                            (insert-file-contents stderr-file)
                            (string-trim (buffer-string)))))
              (error "pandoc exited %s: %s"
                     exit-code
                     (if (string-empty-p stderr) "(no stderr)" stderr)))))
      (ignore-errors (delete-file stderr-file)))))

;; markdown-ts-mode owns .md (below).  markdown-mode stays installed as the
;; fallback when the tree-sitter grammar is missing, and for M-x use only.
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command #'my-markdown-run-pandoc)
  :config
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'markdown-table-face)
  :hook ((markdown-mode . my-markdown-mode-setup)
         (markdown-ts-mode . my-markdown-mode-setup))
  )

;; markdown-ts-mode declares markdown-mode only via
;; `derived-mode-extra-parents': `derived-mode-p' answers yes, but
;; `markdown-mode-hook' does NOT run. Anything hooked for markdown has to
;; name markdown-ts-mode too, or it silently never fires there.

;; Tree-sitter markdown mode for ALL markdown: colored embedded code blocks,
;; inline images, org-like folding and navigation, and a native GFM table
;; mode.  markdown-ts-mode-x adds export/preview and TOC generation.
;;
;; Keep tables and code monospaced under mixed-pitch.
;;
;; The existing entry covers `markdown-table-face', which belongs to
;; markdown-mode; markdown-ts-mode fontifies with its own faces, so tables
;; rendered in the variable-pitch body font and stopped lining up even
;; though the text was character-aligned.  Column alignment is only
;; meaningful in a fixed-pitch font.
(with-eval-after-load 'mixed-pitch
  (dolist (face '(;; fixed-pitch itself: otherwise it inherits the height
                  ;; scale mixed-pitch gives the default face and comes out
                  ;; larger than every other monospaced face in the buffer,
                  ;; which threw off md-render's pixel-measured table padding
                  fixed-pitch
                  ;; tables -- alignment depends on this
                  markdown-ts-table
                  markdown-ts-table-cell
                  markdown-ts-table-delimiter-cell
                  markdown-ts-table-header
                  markdown-ts-in-table
                  ;; code, where proportional glyphs are just wrong
                  markdown-ts-code-block
                  markdown-ts-code-span
                  markdown-ts-indented-code-block
                  markdown-ts-in-code-block
                  markdown-ts-code-block-markup-hidden
                  markdown-ts-language-keyword
                  ;; the rule drawn for a thematic break, so it is continuous
                  markdown-ts-thematic-break))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;; Folded headings get org's chevron rather than "...", which reads as
;; truncation.  Consulted when the mode sets up outline folding.
(setopt markdown-ts-ellipsis " ⌄")

;; An edit can change how the text *around* it parses -- typing the first
;; word of a sub-list item turns the line above back from a setext heading
;; into a list item -- but treesit only refontifies the wider region when
;; `treesit--pre-redisplay' is the call that performs the reparse.  Anything
;; that touches the tree first consumes the changed regions, and the line
;; above keeps the face it had, until a manual `font-lock-update'.  A
;; notifier fires on every reparse, whoever triggered it.
(defun gco-treesit-refontify-changed (ranges parser)
  "Mark RANGES touched by PARSER's reparse as needing refontification."
  (with-current-buffer (treesit-parser-buffer parser)
    (with-silent-modifications
      (dolist (range ranges)
        (put-text-property (car range) (cdr range) 'fontified nil)))))

;; `markdown-ts-mode' has one face for both setext levels, and it inherits
;; heading 1: bold black at 1.2x.  That is loud for a real heading and
;; alarming for the transient one that a half-typed "  -" sub-item makes of
;; the line above it.  Demote it -- ---- underlines are level 2 anyway, and
;; ==== underlines appear nowhere in my notes.
(custom-set-faces
 '(markdown-ts-setext-heading ((t (:inherit markdown-ts-heading-2)))))

;; The grammar only builds a task_list_marker node once the item has
;; content, so an item still at "- [ ]" parses as a shortcut link and is
;; fontified as one -- brackets in the delimiter face around a link-faced
;; space, which looks nothing like the checkbox it becomes a keystroke
;; later.  GitHub and pandoc both read it as a checkbox, so fontify it as
;; one.  Appended to the settings so it wins over the link rule.
(defun gco-markdown-ts-empty-checkbox-p (node)
  "Return non-nil if NODE is a bare `[ ]' or `[x]' opening a list item."
  (and (string-match-p "\\`\\[[ xX]\\]\\'" (treesit-node-text node t))
       (save-excursion
         (goto-char (treesit-node-start node))
         (looking-back "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+"
                       (line-beginning-position)))))

(defun gco-markdown-ts-fontify-empty-checkbox (node override start end &rest _)
  "Fontify NODE, a checkbox the grammar did not recognize, as one.
Mirrors `markdown-ts--fontify-checkbox', including the symbol shown
when `markdown-ts-hide-markup' is on, so an empty checkbox keeps its
appearance once text follows it.  OVERRIDE, START and END are passed
through to `treesit-fontify-with-override'."
  (let* ((beg (treesit-node-start node))
         (fin (treesit-node-end node))
         (checked (memq (char-after (1+ beg)) '(?x ?X)))
         (value (if checked markdown-ts-checked-checkbox
                  markdown-ts-unchecked-checkbox))
         (symbol (if (eq value 'icon)
                     (icon-string (if checked
                                      'markdown-ts-checked-checkbox-icon
                                    'markdown-ts-unchecked-checkbox-icon))
                   (markdown-ts--resolve-display-value value))))
    (treesit-fontify-with-override
     beg fin (if checked 'markdown-ts-task-checked 'markdown-ts-task-unchecked)
     override start end)
    (if (and markdown-ts-hide-markup symbol)
        (put-text-property beg fin 'display
                           (or (and (stringp symbol)
                                    (get-text-property 0 'display symbol))
                               symbol))
      (remove-text-properties beg fin '(display nil)))))

(defun gco-markdown-ts-font-lock-setup ()
  "Fix up `markdown-ts-mode' fontification: empty checkboxes, stale faces."
  (setq-local treesit-font-lock-settings
              (append treesit-font-lock-settings
                      (treesit-font-lock-rules
                       :language 'markdown-inline
                       :feature 'paragraph-inline
                       :override t
                       '(((shortcut_link) @gco-markdown-ts-fontify-empty-checkbox
                          (:pred gco-markdown-ts-empty-checkbox-p
                                 @gco-markdown-ts-fontify-empty-checkbox))))))
  (treesit-parser-add-notifier treesit-primary-parser
                               #'gco-treesit-refontify-changed))

(add-hook 'markdown-ts-mode-hook #'gco-markdown-ts-font-lock-setup)

;; Claimed by remapping, not by auto-mode-alist: markdown-mode registers
;; ".md" in its own autoloads, which elpaca loads asynchronously after init.
;; Any entry we add during init is therefore prepended *before* that one
;; arrives, and loses. major-mode-remap-alist sidesteps the ordering
;; entirely -- whatever decides on markdown-mode, we get markdown-ts-mode --
;; while leaving M-x markdown-mode reachable as the fallback.
;;
;; Only when the grammar is actually available; otherwise markdown-mode
;; stays in charge rather than dropping the buffer into fundamental-mode.
(when (and (fboundp 'markdown-ts-mode)
           ;; treesit-available-p and treesit-language-available-p are C
           ;; primitives, always defined. treesit-ready-p is not: it lives in
           ;; treesit.el, so guarding on it silently skipped this whole block
           ;; during init, before anything had loaded that file.
           (fboundp 'treesit-available-p)
           (treesit-available-p)
           (treesit-language-available-p 'markdown)
           (treesit-language-available-p 'markdown-inline))
  (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
  (add-to-list 'major-mode-remap-alist '(gfm-mode . markdown-ts-mode)))

(when (fboundp 'markdown-ts-mode)
  (use-package markdown-ts-mode
    :ensure nil
    :defer t
    :config
    (require 'markdown-ts-mode-x nil t)
    ;; markdown-ts-mode has no `markdown-mode-command-map', so the preview
    ;; commands bound into that map are re-bound here directly.
    (with-eval-after-load 'grip-mode
      (define-key markdown-ts-mode-map (kbd "C-c C-x g") #'grip-mode))
    (with-eval-after-load 'markdown-xwidget
      (define-key markdown-ts-mode-map (kbd "C-c C-x x")
                  #'markdown-xwidget-preview-mode))
    ;; markdown-mode installs the save/kill hooks that drive
    ;; `markdown-live-preview-mode' from its major-mode body, so in
    ;; markdown-ts-mode the preview renders once and never refreshes.
    (with-eval-after-load 'markdown-mode
      (defun my-markdown-ts-live-preview-hooks (&rest _)
        "Add the live-preview save and kill hooks in `markdown-ts-mode'."
        (when (and markdown-live-preview-mode (derived-mode-p 'markdown-ts-mode))
          (add-hook 'after-save-hook #'markdown-live-preview-if-markdown t t)
          (add-hook 'kill-buffer-hook #'markdown-live-preview-remove-on-kill t t)))
      (advice-add 'markdown-live-preview-mode :after
                  #'my-markdown-ts-live-preview-hooks))
    (when (fboundp 'markdown-ts-convert)
      (define-key markdown-ts-mode-map (kbd "C-c C-x c") #'markdown-ts-convert))
    (define-key markdown-ts-mode-map (kbd "C-c C-x C-t") #'gco-md-tables-mode)))

;; Box-drawn tables from yibie/md-mode's renderer.  Only the md-render
;; files are taken from that repo: md-mode.el's autoloads would claim .md
;; files for its own major mode.  Since 0.5 md-render needs textui, which
;; only md-mode.el declares, so elpaca can't infer it.
(use-package textui
  :ensure (:host github :repo "yibie/textui")
  :defer t)

(use-package md-render
  :ensure (:host github :repo "yibie/md-mode" :files ("md-render*.el"))
  :defer t)

;; Tables show rendered while point is elsewhere and as source while point
;; is inside them (lisp/gco-md-tables.el).  Toggle with C-c C-x C-t.
(use-package gco-md-tables
  :ensure nil
  :hook (markdown-ts-mode . gco-md-tables-mode))

;; Live preview in an xwidget-webkit buffer with GitHub styling, MathJax,
;; Mermaid, and highlight.js. Toggle with C-c C-c x in markdown-mode.
;; Forces a string `markdown-xwidget-command' because `markdown-command'
;; here is a function, which the package can't invoke directly.
(use-package markdown-xwidget
  :after markdown-mode
  :ensure (:host github :repo "cfclrk/markdown-xwidget"
           :files (:defaults "resources"))
  :bind (:map markdown-mode-command-map
              ("x" . markdown-xwidget-preview-mode))
  :config
  (setq markdown-xwidget-command "pandoc"
        markdown-xwidget-github-theme "light"
        markdown-xwidget-code-block-theme "default"
        markdown-xwidget-mermaid-theme "default")
  ;; Pandoc emits <pre class="mermaid"><code>...</code></pre>, but mermaid
  ;; reads the element's innerHTML, so the nested <code> tag breaks parsing
  ;; ("no diagram type detected"). Unwrap it on DOMContentLoaded, which fires
  ;; before the load event that triggers mermaid's startOnLoad render.
  (advice-add 'markdown-xwidget-header-html :filter-return
              (lambda (html)
                (concat html "
<script type=\"text/javascript\">
  document.addEventListener(\"DOMContentLoaded\", () => {
    document.querySelectorAll(\"pre.mermaid > code\").forEach((code) => {
      code.parentElement.textContent = code.textContent;
    });
  });
</script>
"))))

;; Live preview in the system browser via grip (good for dual monitors,
;; scroll-locked side-by-side review). Uses the Python `grip' backend,
;; which goes through GitHub's API: handles YAML frontmatter correctly
;; and renders exactly like github.com. Install: `uv tool install grip'.
;;
;; Credentials are read from ~/.authinfo. Without an entry grip still
;; works at the 60 req/hr unauthenticated limit; to lift it, add:
;;   machine api.github.com login YOUR_GH_USER password ghp_YOUR_PAT
;; (a token with no scopes is sufficient for rate-limit purposes).
;;
;; Toggle with C-c C-c g in markdown-mode.
(use-package grip-mode
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :config
  (require 'auth-source)
  (setq grip-command 'grip
        grip-preview-use-webkit nil)
  (when-let* ((entry (car (auth-source-search :host "api.github.com"
                                              :require '(:user :secret))))
              (user (plist-get entry :user))
              (secret (plist-get entry :secret))
              (pass (if (functionp secret) (funcall secret) secret)))
    (setq grip-github-user user
          grip-github-password pass)))

(provide 'init-markdown)
