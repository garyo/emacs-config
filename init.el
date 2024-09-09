;; -*- lexical-binding: t; -*-
;;; Gary Oberbrunner's Emacs init file
;; garyo@oberbrunner.com

;; This is my Emacs config file. Parts of it are from the 1980s but
;; most of it is fairly up to date. I intend for it to work on any
;; recent Emacs, on Windows, Linux, and Mac. These days, I usually
;; use a bleeding-edge Emacs so I haven't kept it back-compatible.

;;; Startup debugging:

; (setopt debug-on-error nil)
;; Prefer .el file if newer than .elc
;(setq load-prefer-newer t)

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
  "Modeline package to use: sml or doom")

;; The rest of the config is broken into individual files for specific
;; packages, features, and concepts.

(require 'initial-utils)
(require 'initial-setup)
(require 'elpaca-bootstrap)

(require 'init-no-littering)            ; should be early

;;; Profile emacs init
; (use-package esup)

(require 'init-options)

(require 'init-diminish)
(require 'init-windows)                 ; Win32/WSL setup
(require 'init-mac)

(require 'init-fonts-and-frames)
(require 'init-desktop)
(require 'init-system-env)
(require 'init-shell)
(require 'init-grep)
(require 'init-dired)
(require 'init-completion)
(require 'init-python)
(require 'init-languages)
(require 'init-elisp-mode)
(require 'init-markdown)
(require 'init-snippets)
(require 'init-error-checker)
(require 'init-language-server)
(require 'init-org)
(require 'init-llm)
;(require 'init-ekg)                     ; emacs knowledge graph (not working 2024-06-23)
(require 'init-version-control)
(require 'init-window-management)
(require 'init-logseq)
(require 'init-modeline)
(require 'init-fill)
(require 'init-system-open)
(require 'init-avy)
(require 'init-casual)
(require 'init-misc-packages)
(require 'init-transient)
(require 'init-helpful)
(require 'init-misc)                    ; uncategorized stuff
(require 'init-keybindings)
(require 'init-settings)
(require 'init-themes)

;;; Enable a few global commands
(put 'set-goal-column 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(print-time-since-init "init.el")
(add-hook 'elpaca-after-init-hook (lambda () (print-time-since-init "init.el, after all init complete")))

(provide 'emacs)

;;; init.el ends here
