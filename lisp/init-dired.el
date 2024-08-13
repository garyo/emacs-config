;;; init-dired.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(put 'dired-find-alternate-file 'disabled nil)

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lah")       ; long, add dotfiles, human-readable sizes
  (dired-kill-when-opening-new-dired-buffer t "Only one Dired buffer")
  (image-dired-thumb-margin 5)
  (dired-dwim-target t "Suggest already opened Dired buffers for file operations")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (wdired-allow-to-change-permissions t)
  (dired-mouse-drag-files t "Enable mouse dragging to another apps")
  :config
  (progn
    (defun mydired-sort ()
      "Sort dired listings with directories first."
      (save-excursion
        (let (buffer-read-only)
          (forward-line 2) ;; beyond dir. header
          (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
        (set-buffer-modified-p nil)))

    (defadvice dired-readin
        (after dired-after-updating-hook first () activate)
      "Sort dired listings with directories first before adding marks."
      (mydired-sort))))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map
        ("H" . dired-hide-dotfiles-mode)))

(use-package dired-rsync
  :demand t
  :after dired
  :bind
  (:map dired-mode-map
        ("r" . dired-rsync))
  :config
  (add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append)))

;; Dired-x (extra functions for dired mode)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            (setq dired-omit-localp nil) ; match full pathname (slower)
            (setq dired-omit-files "/\\.svn/\\|\\.svn-base$\\|/SBuild/\\|/\\.?#\\|/\\.$\\|/\\.\\.$")
            ))

;; Transient-based menu for dired mode on "C-o"
;;; Not working as of 2024-06-24
;; (use-package casual-dired
;;  :config
;;  (define-key dired-mode-map (kbd "C-o") 'casual-dired-tmenu)
;;  )

(provide 'init-dired)
