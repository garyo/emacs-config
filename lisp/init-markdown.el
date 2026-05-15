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

(provide 'init-markdown)
