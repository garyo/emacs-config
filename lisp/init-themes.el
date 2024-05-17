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
  (load-theme 'modus-vivendi)           ; dark theme
  )

(provide 'init-themes)
