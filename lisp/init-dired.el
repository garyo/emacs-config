;;; init-dired.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(put 'dired-find-alternate-file 'disabled nil)

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lah")       ; long, add dotfiles, human-readable sizes
  ;(dired-kill-when-opening-new-dired-buffer t "Only one Dired buffer")
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
  ;; Uncomment this to hide by default
  ;; :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map
        ("H" . dired-hide-dotfiles-mode)))

(use-package dired-rsync
  :demand t
  :after dired
  :bind
  (:map dired-mode-map
        ("r" . dired-rsync)
        ("I" . dired-kill-subdir)
        )
  :config
  (add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append)))

;; Expand subtrees inline using TAB
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)
              ("M-<down>" . dired-subtree-insert)
              ("M-<up>" . dired-subtree-remove))
  :config
  ;(setq dired-subtree-use-backgrounds nil) ;; Disable background colorization
  )

;; When a dir has only one child, show it inline -- nice.
(use-package dired-collapse
  :after dired
  :config
  (global-dired-collapse-mode)
)

(use-package dired-filter
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-filter-map)      ;; Open filter menu
              ("C-/" . dired-filter-pop-all)) ;; Quickly clear filters ("/ /" also works)
  :config
  ;; Enable persistent filtering across Dired buffers
  (setq dired-filter-persistent t))

;; From https://oremacs.com/2017/03/18/dired-ediff/
(defun ora-ediff-files ()
  "Ediff marked files in Dired."
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "No more than 2 files should be marked"))))
(define-key dired-mode-map "e" 'ora-ediff-files)


;; Transient-based menu for dired mode on "C-o"
;;; Not working as of 2024-06-24
;; (use-package casual-dired
;;  :config
;;  (define-key dired-mode-map (kbd "C-o") 'casual-dired-tmenu)
;;  )

(provide 'init-dired)
