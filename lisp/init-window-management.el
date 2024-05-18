;;; init-window-management.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Tabs, Buffers and Window Management

;; There's a good article at https://www.masteringemacs.org/article/demystifying-emacs-window-manager about using tab-bar mode and ~display-buffer-alist~ to manage windows and set up tabs.
;; Tab-bar mode commands are on ~C-x t~.

;; Here are some configs that help me:

(setopt
 switch-to-buffer-obey-display-actions t ; treat manual buffer switching same as programmatic
 switch-to-buffer-in-dedicated-window 'pop ; pop up somewhere else if user switches buffer in dedicated window
 tab-bar-show 1                            ; show tabs if more than 1
 tab-bar-format '(tab-bar-format-history tab-bar-format-tabs-groups tab-bar-separator tab-bar-format-add-tab) ;
 )
(tab-bar-mode t) ; enable tab bar (won't show unless there's more than one tab)
(add-to-list 'display-buffer-alist
             '("\\*Calendar*"
               (display-buffer-at-bottom)))
(add-to-list 'display-buffer-alist
             '("\\*Warnings*"
               (display-buffer-at-bottom)
               (window-height . 8)))
(add-to-list 'display-buffer-alist
             '("\\*shell:"
               (display-buffer-below-selected)
               (window-height . 12)))
(add-to-list 'display-buffer-alist
             '("\\*shell\\*"
               (display-buffer-at-bottom)
               (window-height . 30)))
(add-to-list 'display-buffer-alist
             '("\\magit:"
               (display-buffer-same-window)))
(add-to-list 'display-buffer-alist
             '("\\*Man"
               (display-buffer-same-window)))
(add-to-list 'display-buffer-alist
             '("\\*Help"
               (display-buffer-same-window)))
(add-to-list 'display-buffer-alist
               '("\\*rg\\*"
                 (display-buffer-same-window) ; try this; see how I like it
                 ))


;;; Window management

;; I have a hard time training myself to use anything more modern than
;; ~C-x o~ (other-window) which I've used for decades. I have
;; repeat-mode turned on, so I can do ~C-x o o o ...~ to keep going,
;; but I rarely do it.

;; Recently I've been using ~windmove~ with ~shift-↑~ etc. for more
;; visual movement, trying to get that into my fingers. But I recently
;; ran across ~winum~ which puts a window number in the modeline, and
;; lets you switch directly to any window -- O(1). That really works for me!

;; Note: the place I got this recommended adding ~:suppress 'nodigits~
;; to the keymap, but that suppresses self-insert for everything which
;; makes emacs unusable. This version seems OK though.

(use-package winum
  :init
  (defvar-keymap winum-keymap
    :doc "Keymap for winum-mode actions."
    "M-0" 'winum-select-window-0-or-10
    "M-1" 'winum-select-window-1
    "M-2" 'winum-select-window-2
    "M-3" 'winum-select-window-3
    "M-4" 'winum-select-window-4
    "M-5" 'winum-select-window-5
    "M-6" 'winum-select-window-6
    "M-7" 'winum-select-window-7
    "M-8" 'winum-select-window-8
    "M-9" 'winum-select-window-9)
  :config
  (winum-mode)
  )


(provide 'init-window-management)
