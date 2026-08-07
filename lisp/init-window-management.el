;;; init-window-management.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Tabs, Buffers and Window Management

;; There's a good article at https://www.masteringemacs.org/article/demystifying-emacs-window-manager about using tab-bar mode and ~display-buffer-alist~ to manage windows and set up tabs.
;; Tab-bar mode commands are on ~C-x t~.

;; Here are some configs that help me:

(setopt
 switch-to-buffer-obey-display-actions t ; treat manual buffer switching same as programmatic
 switch-to-buffer-in-dedicated-window 'pop ; pop up somewhere else if user switches buffer in dedicated window
 tab-bar-show 1                            ; show tabs if more than 1
 tab-bar-format '(tab-bar-format-history tab-bar-format-tabs-groups tab-bar-separator tab-bar-format-add-tab) ;
 )
(tab-bar-mode t) ; enable tab bar (won't show unless there's more than one tab)
(add-to-list 'display-buffer-alist
             '("\\*Calendar\\*"
               (display-buffer-at-bottom)))
(add-to-list 'display-buffer-alist
             '("\\*Warnings\\*"
               (display-buffer-at-bottom)
               (window-height . 8)))
(add-to-list 'display-buffer-alist
             '("\\*shell:"
               (display-buffer-below-selected)
               (window-height . 12)))
(add-to-list 'display-buffer-alist
             '("\\*shell\\*"
               (display-buffer-at-bottom)
               (window-height . 30)))
(add-to-list 'display-buffer-alist
             '("magit:"
               (display-buffer-same-window)))
(add-to-list 'display-buffer-alist
             '("\\*Man"
               (display-buffer-same-window)))
(add-to-list 'display-buffer-alist
             '("\\*Help"
               (display-buffer-same-window)))
(add-to-list 'display-buffer-alist
               '("\\*rg\\*"
                 (display-buffer-same-window) ; try this; see how I like it
                 ))

;; default compilation window height to smallish
(setq compilation-window-height 15)

;; Compilation buffers:
;; - Open at the bottom.
;; - Reuse the same window if possible.
;; - Stay under 15 lines high.
;; - Split the right-hand window if the frame is split vertically.

(defun my/rightmost-window ()
  "Return the rightmost live window in the current frame, or nil if there's only one window.
‘Rightmost’ is determined by the largest left-edge X-coordinate."
  (let* ((all-windows (window-list))
         ;; We only consider a 'rightmost' window if there's more than one window total.
         (window-count (length all-windows)))
    (cond
     ((< window-count 2)
      nil)
     (t
      (car (sort (copy-sequence all-windows)
                 (lambda (w1 w2)
                   (let ((left1 (nth 0 (window-edges w1)))
                         (left2 (nth 0 (window-edges w2))))
                     (> left1 left2)))))))))

(defun my/split-bottom-right (buffer _alist)
  "Ensure *compilation* opens at the bottom, reusing the same window if possible.
If the frame is split side by side, ensure *compilation* appears in the bottom
of the right-hand column. Otherwise, open a bottom side window."
  (let ((existing (get-buffer-window buffer 'visible))
        (rightmost (my/rightmost-window)))
    (cond
     ;; 1) If *compilation* is already visible, reuse it.
     (existing
      (display-buffer-reuse-window
       buffer
       `((reusable-frames . t))))

     ;; 2) If there's a distinct 'rightmost' window (a left-right split),
     ;;    split that horizontally to put compilation at the bottom-right.
     ((window-live-p rightmost)
      (let ((new-win (split-window rightmost (- compilation-window-height) 'below)))
        (window--display-buffer buffer new-win 'window)))

     ;; 3) Otherwise, open at the bottom side window.
     (t
      (display-buffer-in-side-window
       buffer
       `((side . bottom)
         (slot . 0)
         (window-height . ,compilation-window-height)
         (reusable-frames . t)))))))

(add-to-list 'display-buffer-alist
             '("\\*compilation\\*" my/split-bottom-right))

;; Speedbar: Emacs 31 can put it in a side window of the current frame
;; instead of its own tiny frame.
(when (>= emacs-major-version 31)
  (setopt speedbar-prefer-window t
          speedbar-window-side 'left
          speedbar-window-default-width 36))

;; Speedbar re-roots itself at the current file's directory as you move
;; around, so in a deep tree it never shows the project. Root it at the
;; project instead, and expand the tree down to whatever file is current.
(eval-when-compile (require 'speedbar) (require 'project))
(declare-function speedbar-update-contents "speedbar")
(declare-function speedbar-find-selected-file "speedbar" (file))
(declare-function speedbar-expand-line "speedbar" (&optional arg))

(defun gco-project-root ()
  "Root directory of the project containing `default-directory', or nil."
  (when-let* ((proj (project-current nil)))
    (expand-file-name (project-root proj))))

(defun gco-speedbar-use-project-root (orig &rest args)
  "Apply ORIG to ARGS with `default-directory' bound to the project root."
  (let ((default-directory (or (gco-project-root) default-directory)))
    (apply orig args)))

(defun gco-speedbar-reveal-current-file ()
  "Expand speedbar's tree down to the file visited in the current buffer."
  (when-let* (((buffer-live-p speedbar-buffer))
              (file (and buffer-file-name (expand-file-name buffer-file-name))))
    (with-current-buffer speedbar-buffer
      (when (and (string-prefix-p (expand-file-name default-directory) file)
                 (not (save-excursion (speedbar-find-selected-file file))))
        (goto-char (point-min))
        (dolist (dir (split-string (file-relative-name (file-name-directory file)
                                                       default-directory)
                                   "/" t))
          (when (re-search-forward
                 (concat "<\\([-+]\\)> " (regexp-quote dir) "$") nil t)
            ;; Expanding moves point (speedbar recenters), so search for the
            ;; next component from this directory's line, not from wherever
            ;; we were left.
            (let ((line (line-beginning-position)))
              (when (equal (match-string 1) "+")
                (speedbar-expand-line))
              (goto-char line))))
        (when-let* (((speedbar-find-selected-file file))
                    (win (get-buffer-window speedbar-buffer)))
          (set-window-point win (point)))))))

(with-eval-after-load 'speedbar
  ;; Only the follow-the-current-buffer path is redirected;
  ;; `speedbar-update-contents' is left alone so the directory buttons
  ;; still navigate wherever you click them.
  (advice-add 'speedbar-update-localized-contents :around
              #'gco-speedbar-use-project-root)
  (add-hook 'speedbar-timer-hook #'gco-speedbar-reveal-current-file))

(defun gco-speedbar-project ()
  "Open speedbar in a side window, rooted at the current project."
  (interactive)
  (require 'speedbar)
  (let ((root (or (gco-project-root) default-directory)))
    (speedbar 1)
    (with-current-buffer speedbar-buffer
      (setq default-directory root)
      (speedbar-update-contents))
    (gco-speedbar-reveal-current-file)))

;;; Window management

;; I have a hard time training myself to use anything more modern than
;; ~C-x o~ (other-window) which I've used for decades. I have
;; repeat-mode turned on, so I can do ~C-x o o o ...~ to keep going,
;; but I rarely do it.

;; Recently I've been using ~windmove~ with ~shift-↑~ etc. for more
;; visual movement, trying to get that into my fingers. But I recently
;; ran across ~winum~ which puts a window number in the modeline, and
;; lets you switch directly to any window -- O(1). That really works for me!

;; Note: the place I got this recommended adding ~:suppress 'nodigits~
;; to the keymap, but that suppresses self-insert for everything which
;; makes emacs unusable. This version seems OK though.

(use-package winum
  :init
  (defvar-keymap winum-keymap
    :doc "Keymap for winum-mode actions."
    "M-0" 'winum-select-window-0-or-10
    "M-1" 'winum-select-window-1
    "M-2" 'winum-select-window-2
    "M-3" 'winum-select-window-3
    "M-4" 'winum-select-window-4
    "M-5" 'winum-select-window-5
    "M-6" 'winum-select-window-6
    "M-7" 'winum-select-window-7
    "M-8" 'winum-select-window-8
    "M-9" 'winum-select-window-9)
  :config
  (winum-mode)
  )

;; Window manipulation keymap.
;; Emacs 31 (window-x.el) renamed several layout commands and added a new
;; `window-layout-transpose'. Resolve to whichever names this Emacs provides
;; so the bindings work on both pre-31 and 31+:
;;   flip-window-layout-horizontally    -> window-layout-flip-leftright
;;   flip-window-layout-vertically      -> window-layout-flip-topdown
;;   rotate-window-layout-clockwise     -> window-layout-rotate-clockwise
;;   rotate-window-layout-anticlockwise -> window-layout-rotate-counterclockwise
;;   (new in 31)                           window-layout-transpose
(defun gco-resolve-command (&rest names)
  "Return the first bound command among NAMES, or nil."
  (seq-find #'fboundp names))

(defvar-keymap window-manipulation-command-map
  :doc "Keymap for window manipulation commands.")

(let ((bindings
       `(("r" . ,(gco-resolve-command 'rotate-windows))
         ("R" . ,(gco-resolve-command 'rotate-windows-back))
         ("f" . ,(gco-resolve-command 'window-layout-flip-leftright
                                      'flip-window-layout-horizontally))
         ("F" . ,(gco-resolve-command 'window-layout-flip-topdown
                                      'flip-window-layout-vertically))
         ("w" . ,(gco-resolve-command 'window-layout-rotate-clockwise
                                      'rotate-window-layout-clockwise))
         ("W" . ,(gco-resolve-command 'window-layout-rotate-counterclockwise
                                      'rotate-window-layout-anticlockwise))
         ("t" . ,(gco-resolve-command 'window-layout-transpose)))))
  (pcase-dolist (`(,key . ,cmd) bindings)
    (when cmd
      (keymap-set window-manipulation-command-map key cmd)
      (put cmd 'repeat-map 'window-manipulation-command-map))))

;; Bind the window-manipulation keymap to C-c w
(keymap-set global-map "C-c w" window-manipulation-command-map)

(provide 'init-window-management)
