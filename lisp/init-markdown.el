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
;; fallback when the tree-sitter grammar is missing, and for M-x use, but it
;; no longer claims a file extension -- two packages racing for the same
;; auto-mode-alist entry is exactly the bug that made PKM notes open in the
;; wrong mode, since elpaca activates asynchronously and order is not the
;; order of the require calls.
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
  (dolist (face '(;; tables -- alignment depends on this
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
    (when (fboundp 'markdown-ts-convert)
      (define-key markdown-ts-mode-map (kbd "C-c C-x c") #'markdown-ts-convert))))

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
