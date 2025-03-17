;;; init-grep.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;; I use ripgrep: fast recursive grep, wgrep-capable.
;; March 2025: maybe switch to deadgrep. Simpler, faster.

;;; Code:

;; (use-package rg
;;   :config
;;   (setq rg-executable "rg") ; defaults to (executable-find "rg") which can be wrong on Windows
;;   (rg-enable-menu)          ; start w/ C-c s p, "rg-project"
;;   ;; rg-mode binds C-n and C-p to go to next/prev file rather than by line
;;   ;; which is a bit jarring.
;;   (define-key rg-mode-map (kbd "C-n") nil)
;;   (define-key rg-mode-map (kbd "C-p") nil)
;;   (rg-define-search rg-search-all       ; C-c s a: search all in project
;;     "Search all files in project with rg"
;;     :files "everything"
;;     :dir project
;;     :menu ("Search" "a" "All in project")
;;     )
;;   (rg-define-search rg-search-dir       ; C-c s d: search in current dir
;;     "Search in current dir with rg"
;;     :files "everything"
;;     :dir current
;;     :menu ("Search" "C" "All in current dir")
;;     )
;;   )

;; March 2025: deadgrep is faster/simpler than rg

(use-package deadgrep
  :config
  :bind ("C-c s" . deadgrep))

  ;;; wgrep-change-to-wgrep-mode to edit right in a grep buffer (or ag/ripgrep)
  ;;; Use C-c C-e to apply.
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t)
  )

;; deadgrep + wgrep
(use-package wgrep-deadgrep
  :after deadgrep wgrep)

(provide 'init-grep)
