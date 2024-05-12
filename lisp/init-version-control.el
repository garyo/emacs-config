;;; init-version-control.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Version control and magit

(autoload 'vc-git-root "vc-git" nil t)
(autoload 'vc-git-grep "vc-git" nil t)

(use-package git-modes)

(use-package transient)
(use-package magit
  :bind (("C-x v =" . magit-status)
         ("C-x v b" . magit-blame)
  	 ("C-x v l" . magit-log-current))
  :config
  (add-hook 'magit-status-mode-hook 'delete-other-windows)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  ;; speed up on Windows
  (when-windows
   ;; Without this, magit-show-refs-popup ('y') is very slow, late 2014
   (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
   ;; try to speed up status on Windows
   (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
   ;;(remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
   (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
   (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
   (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
   (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
   )
  :custom
  (magit-backup-mode nil)
  (magit-cygwin-mount-points '(("/c" . "c:")))
  (magit-diff-expansion-threshold 999.0)
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-expand-staged-on-commit 'full)
  (magit-log-format-graph-function 'magit-log-format-unicode-graph)
  (magit-log-format-unicode-graph-alist '((47 . 9585) (92 . 9586) (42 . 9642)))
  (magit-pull-arguments '("--rebase"))
  (magit-refresh-status-buffer nil)
  :custom-face
  (magit-item-highlight ((t (:background "floral white"))))
  (magit-section-highlight ((t (:background "floral white"))))
  )

;; (use-package diff-hl
;;   :config
;;   (global-diff-hl-mode))

;; Git-gutter mode hangs in tramp (remote) buffers, so I disable it there.
;; See hook below.
(defun my-disable-git-gutter-in-tramp-buffers ()
  "Disable git-gutter mode in TRAMP buffers."
  (when (and (fboundp 'git-gutter-mode) (tramp-tramp-file-p (buffer-file-name)))
    (git-gutter-mode -1)))

(use-package git-gutter
  :config
  (dolist (p '((git-gutter:added    . "#0c0")
               (git-gutter:deleted  . "#c88")
               (git-gutter:modified . "#df0")))
    (set-face-background (car p) (cdr p)))
  (global-git-gutter-mode +1)
  (require 'tramp)                      ; for tramp-tramp-file-p
  (add-hook 'find-file-hook 'my-disable-git-gutter-in-tramp-buffers)
  :diminish git-gutter-mode
  )

;; Like vc-git-grep from Emacs 25, but without the semi-useless "files" arg.
(defun git-grep (regexp &optional dir)
  "Run git grep, searching for REGEXP in directory DIR.

    With \\[universal-argument] prefix, you can edit the constructed shell command line
    before it is executed.
    With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

    Collect output in a buffer.  While git grep runs asynchronously, you
    can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
    in the grep output buffer,
    to go to the lines where grep found matches.

    This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
                                   nil nil 'grep-history)
             nil))
      (t (let* ((regexp (grep-read-regexp))
                (dir (read-directory-name "In directory: "
                                          (vc-git-root default-directory) nil t)))
           (list regexp dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (progn
        (setq dir (file-name-as-directory (expand-file-name dir)))
        (setq command
              (grep-expand-template "git --no-pager grep -n -e <R>"
                                    regexp))
        (when command
          (if (equal current-prefix-arg '(4))
              (setq command
                    (read-from-minibuffer "Confirm: "
                                          command nil nil 'grep-history))
            (add-to-history 'grep-history command))))
      (when command
        (let ((default-directory dir)
              (compilation-environment (cons "PAGER=" compilation-environment)))
          ;; Setting process-setup-function makes exit-message-function work
          ;; even when async processes aren't supported.
          (compilation-start command 'grep-mode))
        (if (eq next-error-last-buffer (current-buffer))
            (setq default-directory dir))))))


(provide 'init-version-control)
