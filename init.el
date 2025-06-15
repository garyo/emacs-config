;;; package --- Gary Oberbrunner's Emacs init file  -*- lexical-binding: t; -*-
;; garyo@oberbrunner.com

;;; Commentary:
;; This is my Emacs config file. Parts of it are from the 1980s but
;; most of it is fairly up to date. I intend for it to work on any
;; recent Emacs, on Windows, Linux, and Mac. These days, I usually
;; use a bleeding-edge Emacs so I haven't kept it back-compatible.

;;; Startup debugging:

;; (setopt debug-on-error t)
;; Prefer .el file if newer than .elc
;(setq load-prefer-newer t)

;;; Code:

(message "Loading init.el: ~ is %S, user-emacs-directory is %S"
         (expand-file-name "~") user-emacs-directory)

;; Most of the actual config is in sections in "lisp/"
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Custom settings:
;; Save ~M-x customize~ results into a separate file. I do this
;; because I prefer to do my customizations explicitly in this file,
;; but sometimes it's convenient to use ~M-x customize~ temporarily,
;; and I don't want that to affect my actual init file.
;; Note that I explicitly DO NOT load the custom file (commented out here)!
;; Anything changed in there by customize should be copied into
;; init-settings or placed with its package.
(setq custom-file (locate-user-emacs-file "custom-settings.el"))
;(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

;;; Select high-level emacs packages:

(defconst use-flycheck-mode nil
  "Which checker to use: t means use flycheck; nil means use flymake.")

(defconst use-lsp-mode nil
  "Which LSP service to use: t means use lsp-mode; nil means use eglot.
Always uses eglot if this Emacs doesn't have fast JSON.")

(defconst modeline-package 'doom
  "Modeline package to use: sml or doom.")

;; The rest of the config is broken into individual files for specific
;; packages, features, and concepts.
;; For Android, we only load a few things.

(pcase system-type
  ("android"
   (require 'init-android)
   )
  (_
   (require 'initial-utils)
   (require 'initial-setup)
   (require 'elpaca-bootstrap)

   (require 'init-no-littering)         ; should be early

;;; Profile emacs init
                                        ; (use-package esup)

   (require 'init-options)

   (require 'init-diminish)
   (require 'init-windows)              ; Win32/WSL setup
   (require 'init-mac)

   (require 'init-fonts-and-frames)
   (require 'init-desktop)
   (require 'init-bookmark)
   (require 'init-system-env)
   (require 'init-shell)
   (require 'init-grep)
   (require 'init-dired)
   (require 'init-completion)
   (require 'init-project)
   (require 'init-python)
   (require 'init-languages)
   (require 'init-elisp-mode)
   (require 'init-markdown)
   (require 'init-snippets)
   (require 'init-error-checker)
   (require 'init-language-server)
   (require 'init-org)
   (require 'init-llm)
   ;;(require 'init-ekg)                     ; emacs knowledge graph (not working 2024-06-23)
   (require 'init-version-control)
   (require 'init-window-management)
   ;; Not yet working well
   ;; (require 'init-combobulate)             ; structured editing & movement
   (require 'init-logseq)
   (require 'init-modeline)
   (require 'init-fill)
   (require 'init-system-open)
   (require 'init-avy)
   (require 'init-casual)
   (require 'init-misc-packages)
   (require 'init-transient)
   (require 'init-helpful)
   (require 'init-misc)                 ; uncategorized stuff
   (require 'init-keybindings)
   (require 'init-settings)
   (require 'init-themes)

   ;; My own packages
   (elpaca '(dup-transform :host github :repo "garyo/dup-transform.el")
	   (use-package dup-transform
	     :hook ((prog-mode . dup-transform-mode))
	     )
     )

   ;; My local packages (in development)

   ;; contextum:
   (add-to-list 'trusted-content "~/src/contextum/")
   ;; Note: requires elpaca of 2025-06-15 or later, with local repo support
   (elpaca '(contextum :repo "/Users/garyo/src/contextum")
     :after 'org-ml
     )
  ))



;;; Enable a few global commands
(put 'set-goal-column 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;; Trust all of my elisp files (this enables flymake byte-compile checking)
(add-to-list 'trusted-content "~/.config/emacs/init.el")
(add-to-list 'trusted-content "~/.config/emacs/early-init.el")
(add-to-list 'trusted-content "~/.config/emacs/lisp/")

(print-time-since-init "init.el")
(add-hook 'elpaca-after-init-hook (lambda () (print-time-since-init "init.el, after all init complete")))

;; Show my bookmarks at startup, instead of the *scratch* buffer
(defun my/bmkp-list-all (&rest args)
  "Open a bookmark list that has no filter applied (i.e., show all bookmarks)."
  (interactive)
  ;; Clear any existing filter/pattern.
  (setq bmkp-bmenu-filter-function  nil
        bmkp-bmenu-filter-pattern   nil
        bmkp-bmenu-title            "All Bookmarks")
  ;; Rebuild and display the bookmark list buffer.
  (condition-case err
      (bookmark-bmenu-list 'NO-MSG-P)
    (wrong-number-of-arguments
     (bookmark-bmenu-list)))
  (switch-to-buffer "*Bookmark List*"))

(add-hook 'emacs-startup-hook
          #'my/bmkp-list-all)
(provide 'emacs)

;;; init.el ends here
