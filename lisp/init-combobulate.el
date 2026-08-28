;;; init-combobulate.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Combobulate: tree-sitter code motion & editing

(use-package combobulate
    :ensure (:host github :repo "mickeynp/combobulate")
    :after treesit
    :hook ((prog-mode . combobulate-mode))
)

(provide 'init-combobulate)
