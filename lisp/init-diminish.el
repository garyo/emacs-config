;;; init-diminish.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; This enables the :diminish keyword which I use in later package
;; setups.
(use-package diminish
  :config
  (diminish 'eldoc-mode))

(provide 'init-diminish)
