;;; init-misc-packages.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

  ;;; Save minibuffer histories -- important w/ vertico, useful always
(use-package savehist
  :ensure nil
  :config
  (savehist-mode)
  )

  ;;; Ediff: split horizontally (A|B, like C-x 3) and
  ;;; don't use the little floating control frame.
(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  )

(use-package ztree)                     ; file tree browser

;; White space cleanup, without obtrusive white space removal.
;; Whitespaces at EOL and EOF are trimmed upon file save, and only for lines modified by you.
;; Much better than globally removing EOL whitespace on save, especially when
;; editing collaboratively with others.
(use-package ws-butler
  :ensure (:host github
           :repo "lewang/ws-butler"
           :branch "master")
  :hook (prog-mode . ws-butler-mode)
  )

;;; Looks cool but requires helm (?)
; (use-package filetree)
(use-package treemacs)

;;; Temporarily highlight undo, yank, find-tag and a few other things
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t)
  :diminish volatile-highlights-mode
  )

(use-package all-the-icons)

;; better visual paren matching
(use-package mic-paren
  :hook ((c-mode-common .
                       (lambda ()
                        (paren-toggle-open-paren-context 1)))
         (c-ts-base-mode .
                       (lambda ()
                        (paren-toggle-open-paren-context 1)))
         )
  :config
  (paren-activate)
  )

;;; Useful for folding, manipulating and navigating indented languages like yaml
;;; (or even python)
(use-package indent-tools
  :bind (("C-c >" . indent-tools-hydra/body))
  )

;;; show keybindings following prefix in a popup
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  )

;;; Use M-x mc/<TAB> to see commands
(use-package phi-search)                ; to make Ctrl-S work with mc
(use-package multiple-cursors
  :after phi-search
  :bind (("C-S-<mouse-1>" . mc/add-cursor-on-click)) ; activate multiple cursors with Ctrl-shift-click
  )

;;; Useful when switching git branches
(use-package revert-buffer-all
  :commands (revert-buffer-all))

;;; Show breadcrumbs in top line (uses imenu and/or project; useful with eglot since lsp-mode does this itself)
(use-package breadcrumb
         :ensure (:host github
                          :repo "joaotavora/breadcrumb"
                          :branch "master")
  :hook ((prog-mode . breadcrumb-mode)
         (org-mode . breadcrumb-mode))
  :config
  ;; Workaround for breadcrumb crash (args-out-of-range "" 0 1):
  ;; When project-current returns "~/Documents/org-notes/" (or org-agenda/) but
  ;; buffer-file-name is "/Users/garyo/Documents/org-notes/..." (or org-agenda/),
  ;; breadcrumb--project-crumbs-1 computes a relative path that
  ;; starts with "/" (the full absolute path treated as relative).
  ;; Splitting on "/" produces an empty string "" component, and
  ;; breadcrumb--summarize crashes calling (aref "" 0) on it.
  (advice-add 'breadcrumb--project-crumbs-1 :filter-return
              (lambda (crumbs)
                (cl-remove-if (lambda (c) (and (stringp c) (string-empty-p c)))
                              crumbs))
              '((name . breadcrumb--filter-empty-crumbs)))
  )

;; Transient-based menu for calc mode on "C-o"
;;; Not working as of 2024-06-24
;; (use-package casual-calc
;;  :config
;;  (define-key calc-mode-map (kbd "C-o") 'casual-main-menu)
;;  )

;; Nice little package to make a QR Code from the region or URL under point.
;; Could be useful to send small stuff to phone.
(use-package qrencode
  )

;;; Recentf: remember recently visited files.

;; I like to have it save the list periodically in case of crashes or
;; when using emacs server.

;; Make recentf save silently
(defun recentf-save-silently-advice (original &rest args)
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply original args)))
(advice-add 'recentf-save-list :around #'recentf-save-silently-advice)

(recentf-mode t)
(setq-default
 recentf-exclude '("semantic.cache"
                   "\\.completions"
                   "\\.projects\\.ede"
                   "\\.ido\\.last"
                   "recentf"
                   "\\.config/emacs/var/"
                   "ido\\.last"
                   ".tmp.babel-"
                   "/[a-z]+:.*:"       ;don't save tramp files
                   )
 recentf-max-menu-items 30
 recentf-max-saved-items 50)
;; emacs doesn't save recentf list until you "exit normally"
;; which never really happens with emacs-server. So just save every 10
;; min, and do it silently.
(run-at-time nil 600 (lambda () (let ((save-silently t))
                                  (recentf-save-list))))

;;; Timezone converter: functions are like `tzc-convert-*` and `tzc-world-clock`
(use-package tzc)

;; Edit server for Chrome (browser extension) (but only if installed):
;; Install this Chrome extension: https://chromewebstore.google.com/detail/chrome-emacs/dabdpcafiblbndpoadckibiaojbdnpjg
(use-package atomic-chrome
  :demand t
  :ensure (:host github
           :repo "KarimAziev/atomic-chrome"
           :flavor nil)
  :commands (atomic-chrome-start-server)
  :defines atomic-chrome-create-file-strategy
  :config
  (atomic-chrome-start-server)
  (setq-default atomic-chrome-extension-type-list '(atomic-chrome))
  (setq-default atomic-chrome-buffer-open-style 'frame)
  (setq-default atomic-chrome-auto-remove-file t)
  (setq-default atomic-chrome-url-major-mode-alist
                '(("github.com" . gfm-mode)
                  ("gitlab.com" . gfm-mode)
                  ("leetcode.com" . typescript-ts-mode)
                  ("codesandbox.io" . js-ts-mode)
                  ("typescriptlang.org" . typescript-ts-mode)
                  ("jsfiddle.net" . js-ts-mode)
                  ("w3schools.com" . js-ts-mode)))
  )

(use-package package-lint)

(use-package csv-mode)

(use-package popper
  :ensure t
  ;; XXX better key bindings?
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Org QL View\\*"
          help-mode
          compilation-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints


(provide 'init-misc-packages)
