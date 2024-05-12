;;; init-grep.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;; I use ripgrep: fast recursive grep, wgrep-capable.
;; "ag" (aka "Silver Searcher" is an alternative.)

;;; Code:

(use-package rg
  :config
  (setq rg-executable "rg") ; defaults to (executable-find "rg") which can be wrong on Windows
  (rg-enable-menu)          ; start w/ C-c s p, "rg-project"
  ;; rg-mode binds C-n and C-p to go to next/prev file rather than by line
  ;; which is a bit jarring.
  (define-key rg-mode-map (kbd "C-n") nil)
  (define-key rg-mode-map (kbd "C-p") nil)
  (rg-define-search rg-search-all       ; C-c s a: search all in project
    "Search all files in project with rg"
    :files "everything"
    :dir project
    :menu ("Search" "a" "All in project")
    )
  (rg-define-search rg-search-dir       ; C-c s d: search in current dir
    "Search in current dir with rg"
    :files "everything"
    :dir current
    :menu ("Search" "C" "All in current dir")
    )
  )

  ;;; wgrep-change-to-wgrep-mode to edit right in a grep buffer (or ag/ripgrep)
  ;;; Use C-c C-e to apply.
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t)
  )

;; ripgrep seems better and works better on Windows, but could switch to 'ag' (silver searcher):

;; M-x ag-project
;; (use-package ag)
  ;;; Need this for wgrep to understand ag-search buffers
;; (use-package wgrep-ag
;;   :hook (ag-mode . wgrep-ag-setup)
;; )



(provide 'init-grep)
