;;; init-mac.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Mac default setup has Command (⌘, clover) = meta
;; Also set Option (⌥) to be super
(when-mac
 (setq mac-option-modifier 'super)
 )

;; The Cocoa build's xwidget-webkit search primitives are stubs; this
;; backs C-s in xwidget-webkit buffers with the DOM's window.find.
(use-package gco-xwidget-search
  :ensure nil
  :after xwidget)

;; Copy as rich text (HTML) for pasting into mail: C-c C-x w renders the
;; Markdown region with pandoc; M-w copies an xwidget-webkit page selection.
(use-package gco-rich-copy
  :ensure nil
  :commands (gco-rich-copy-markdown gco-rich-copy-xwidget-selection)
  :init
  (with-eval-after-load 'markdown-ts-mode
    (define-key markdown-ts-mode-map (kbd "C-c C-x w") #'gco-rich-copy-markdown))
  (with-eval-after-load 'xwidget
    (define-key xwidget-webkit-mode-map (kbd "M-w")
                #'gco-rich-copy-xwidget-selection)))



(provide 'init-mac)
