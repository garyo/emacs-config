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



(provide 'init-mac)
