;;; initial-utils.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Only load this file on Android.
(when (not (eq system-type 'android))
 (error "This init-android.el configuration is for Android only"))

;;; Set up use-package; no straight/elpaca on Android
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Auto-install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; Tool bar
(tool-bar-mode +1)
(setq tool-bar-position 'bottom)
(setq tool-bar-button-margin 27)

;; Bigger default font for touch
(set-face-attribute 'default nil :height 160)

;; Bigger minibuffer for touch keyboard
(setq resize-mini-windows t)
(setq max-mini-window-height 0.35)

;; Better touch scrolling
(setq scroll-margin 3)
(setq scroll-conservatively 101)


(provide 'init-android)
