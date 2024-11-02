;;; init-casual.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; All the casual packages

;;; Packages that casual hooks into:
(use-package ibuffer
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :ensure nil                           ; built-in
  :defer t)

(use-package re-builder
  :ensure nil
  :defer t)
(use-package bookmark
  :ensure nil
  :defer t)

(use-package casual
  :after (ibuffer re-builder bookmark)
  :bind
  (:map calc-mode-map ("C-o" . casual-calc-tmenu))
  (:map Info-mode-map ("C-o" . casual-info-tmenu))
  (:map dired-mode-map ("C-o" . casual-dired-tmenu))
  ("M-G" . casual-avy-tmenu)
  (:map isearch-mode-map ("<f2>" . casual-isearch-tmenu))
  (:map
   ibuffer-mode-map
   ("C-o" . casual-ibuffer-tmenu)
   ("F" . casual-ibuffer-filter-tmenu)
   ("s" . casual-ibuffer-sortby-tmenu)
   ("<double-mouse-1>" . ibuffer-visit-buffer) ; optional
   ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window) ; optional
   ("{" . ibuffer-backwards-next-marked)       ; optional
   ("}" . ibuffer-forward-next-marked)         ; optional
   ("[" . ibuffer-backward-filter-group)       ; optional
   ("]" . ibuffer-forward-filter-group)        ; optional
   ("$" . ibuffer-toggle-filter-group))
  (:map
   reb-mode-map ("C-o" . casual-re-builder-tmenu)
   :map
   reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu))
  (:map bookmark-bmenu-mode-map
        ("C-o" . casual-bookmarks-tmenu)
        ("S" . casual-bookmarks-sortby-tmenu)
        ("J" . bookmark-jump))
  )

(provide 'init-casual)
