;;; init-helpful.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;; Nicer *Help* buffer for Emacs commands, functions, variables, keys.

;;; Code:

(use-package helpful
  :bind (
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-command] . helpful-command)
         ("C-h F" . helpful-function)   ; normally Info-goto-emacs-command-node
         )
  )

(provide 'init-helpful)
