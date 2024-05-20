;;; init-themes.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package modus-themes
  :init
  (setq modus-vivendi-palette-overrides
        '(
          (bg-mode-line-active bg-blue-subtle)
          (border-mode-line-inactive bg-mode-line-inactive)
          (bg-removed "#661119") ; more visible to me vs. green bg-added color
          )
        modus-themes-italic-constructs t ; italic comments, doc strings
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
    )
  (add-hook 'enable-theme-functions #'customize-theme-faces)

  (load-theme 'modus-vivendi)           ; load dark theme
  )

(provide 'init-themes)
