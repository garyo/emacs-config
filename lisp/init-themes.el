;;; init-themes.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package modus-themes
  :init
  (setopt modus-vivendi-palette-overrides
        '(
          (bg-removed "#661119") ; more visible to me vs. green bg-added color
          ))
  (setopt modus-themes-common-palette-overrides
          '(
            (bg-mode-line-active bg-blue-subtle)
            (border-mode-line-inactive bg-mode-line-inactive)
            (line-height-heading-1 1.5)
            (fg-heading-1 fg-main)
            (fg-heading-2 red-warmer)
            (fg-heading-3 yellow)
            (fg-heading-4 green-warmer)
            (fg-heading-6 blue)
            (fg-heading-7 white)
          ))
  (setopt modus-themes-italic-constructs t) ; italic comments, doc strings
  ;; for org-mode headings
  (setopt modus-themes-headings
        '((1 . (1.2 weight bold))
          (2 . (1.05))
          (3 . (1.01))
          (t . (semilight)))
        )
  :config
  ;; Customizations to override theme defaults
  ;; Do this in `enable-theme-functions` hook to ensure the customizations
  ;; are re-applied when switching themes.
  (defun customize-theme-faces (&rest _)
    (modus-themes-with-colors
      ;; Customize header line format
      (set-face-attribute 'header-line nil
                          :height 1.15 :underline t :background bg-active))
      (set-face-attribute 'modus-themes-heading-1 nil
                          :underline t)
    )
  (add-hook 'enable-theme-functions #'customize-theme-faces)

  ;(load-theme 'modus-vivendi)           ; load dark theme
  (load-theme 'modus-operandi)           ; load light theme
  )

(provide 'init-themes)
