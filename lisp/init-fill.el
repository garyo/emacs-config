;;; init-fill.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; unfill fills or unfills para, toggling each time you press M-q
(use-package unfill
  :bind
  (([remap fill-paragraph] . unfill-toggle)
   :map org-mode-map
   ("M-q" . unfill-toggle)
  ))

;; adaptive fill mode
(use-package filladapt
  ;; to enable only in certain modes:
  ;; :hook (('text-mode-hook . 'filladapt-mode))
  :diminish filladapt-mode
  :config
  (setq-default filladapt-mode t))      ; turn on by default everywhere



(provide 'init-fill)
