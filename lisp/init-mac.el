;;; init-mac.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Mac default setup has Command (⌘, clover) = meta
;; Also set Option (⌥) to be super
(when-mac
 (setq mac-option-modifier 'super)
 )



(provide 'init-mac)
