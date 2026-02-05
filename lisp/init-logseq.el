;;; init-logseq.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Logseq

;; Logseq PKM system keeps its files as Markdown, so they can be
;; edited by Emacs as well. Here's a simple integration: a transient
;; menu for going to today's journal entry.

(defvar logseq-root
  (cond ((eq system-type 'windows-nt)
         (expand-file-name "~/Documents/Logseq Notes"))
        ((eq system-type 'darwin)
         (expand-file-name "~/Logseq Notes"))
        ))
(defvar logseq-personal (concat logseq-root "/Personal"))
(defvar logseq-work (concat logseq-root "/DSS"))

(defun logseq-goto-today-personal ()
  "Go to today's Logseq personal journal"
  (interactive)
  (let
      ((today (format-time-string "%Y_%m_%d")))
    (find-file (concat logseq-personal "/journals/" today ".md"))
    ))
(defun logseq-goto-today-work ()
  (interactive)
  (let
      ((today (format-time-string "%Y_%m_%d")))
    (find-file (concat logseq-work "/journals/" today ".md"))
    ))

(defun logseq-search (query &optional root)
  "Search all of Logseq Notes with ripgrep"
  (interactive "sQuery: ")
  (let ((rg-command-line-flags '("--glob=!version-files/" "--glob=!bak/")))
    (deadgrep query (or root logseq-root))))

(defun logseq-search-personal (query)
  "Search personal Logseq Notes with ripgrep"
  (interactive "sQuery: ")
  (logseq-search query logseq-personal))
(defun logseq-search-work (query)
  "Search work Logseq Notes with ripgrep"
  (interactive "sQuery: ")
  (logseq-search query logseq-work))


;; F9: open or search Logseq notes
(with-eval-after-load 'transient
  (transient-define-prefix logseq-menu ()
    "Logseq Menu"
    [["Capture"
      ("tp" "Today (personal)" logseq-goto-today-personal)
      ("tw" "Today (work)" logseq-goto-today-work)]
     ["Search"
      ("a" "All" logseq-search)
      ("p" "Personal" logseq-search-personal)
      ("w" "Work" logseq-search-work)
      ]])
  (define-key global-map (kbd "<f9>") 'logseq-menu)
  )


(provide 'init-logseq)
