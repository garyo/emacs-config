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

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("M-RET" . completion-at-point))
  :init (setq markdown-command "multimarkdown")
  :hook ((markdown-mode . my-markdown-mode-setup)
         (markdown-ts-mode . my-markdown-mode-setup))
  )

(provide 'init-markdown)
