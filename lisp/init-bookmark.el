;;; init-bookmark.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Set up bookmarks subsystem

;; - bookmark+ can do a lot of things; among them is saving named configs as bookmarks.
;;   It uses desktop.


;; Try bookmark+ -- can use it just for saving/restoring named desktops.
;; It can do a lot more than that but bkmp-set-desktop-bookmark,
;; bkmp-jump-desktop, and bkmp-bmenu-list should be good enough for a
;; start.

(defun my/bmkp-list-all ()
  "Open a bookmark list that has no filter applied (i.e., show all bookmarks)."
  (interactive)
  ;; Clear any existing filter/pattern.
  (setq bmkp-bmenu-filter-function  nil
        bmkp-bmenu-filter-pattern   nil
        bmkp-bmenu-title            "All Bookmarks")
  ;; Rebuild and display the bookmark list buffer.
  (bookmark-bmenu-list 'NO-MSG-P)
  (switch-to-buffer "*Bookmark List*"))


(use-package bookmark+
  :after desktop
  :ensure (:host github :repo "emacsmirror/bookmark-plus")
  :config
  ;; Show my bookmarks at startup, instead of the *scratch* buffer
  (setq bmkp-desktop-jump-save-before-flag t) ;; Auto-save before switching
  (add-hook 'emacs-startup-hook
            (lambda () (my/bmkp-list-all)))
  )

(provide 'init-bookmark)
