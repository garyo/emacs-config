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

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("M-RET" . completion-at-point))
  :init (setq markdown-command #'my-markdown-run-pandoc)
  :config
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'markdown-table-face)
  :hook ((markdown-mode . my-markdown-mode-setup)
         (markdown-ts-mode . my-markdown-mode-setup))
  )

;; Emacs 31 ships an experimental tree-sitter markdown mode with colored
;; embedded code blocks, inline image viewing, and org-like navigation.
;; markdown-mode (above) still owns `.md' files; this just makes
;; `markdown-ts-mode' available to switch into, and lets eglot render LSP
;; docs via `markdown-ts-view-mode' (see init-language-server.el).
(when (fboundp 'markdown-ts-mode)
  (use-package markdown-ts-mode
    :ensure nil
    :defer t))

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
        markdown-xwidget-mermaid-theme "default"))

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
