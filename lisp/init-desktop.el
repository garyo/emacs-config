;;; init-desktop.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Save desktops, frames etc. to disk
;; - frameset just saves/loads framesets (window config) which I like.
;; - desktop can save everything: frames, buffers, and global vars.
;; - bookmark+ can do a lot of things; among them is saving named configs as bookmarks.
;;   It uses desktop.

;; Choose 'frameset or 'desktop
;; Frameset just saves/restores frames which is mostly what I'm looking for.
(defconst window-save-restore-method 'frameset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop

;; Use `desktop-save-in-desktop-dir' or just `desktop-save' to save anywhere

(use-package desktop
  :ensure nil
  :init
  (setq desktop-base-file-name "emacs.desktop")
  :config
  ;; Override stale desktop-file locks (from emacswiki)
  (defun garyo/desktop-owner-advice (original &rest args)
    (let ((owner (apply original args)))
      (if (and owner (/= owner (emacs-pid)))
          (and (car (member owner (list-system-processes)))
               (let (cmd (attrlist (process-attributes owner)))
                 (if (not attrlist) owner
                   (dolist (attr attrlist)
                     (and (car attr) (string= "comm" (car attr))
                          (setq cmd (cdr attr))))
                   (and cmd (string-match-p "[Ee]macs" cmd) owner))))
        owner)))
  ;; Ensure that dead system processes don't own it.
  (advice-add #'desktop-owner :around #'garyo/desktop-owner-advice)

  (when (eq window-save-restore-method 'desktop)
    ;; Doesn't seem to work in wsl2 for now
    (when (not wsl2-p)
      ;; don't save any files; just the window configuration
      ;; and don't load if the desktop file is locked
      (setq desktop-files-not-to-save ".*"
            desktop-buffers-not-to-save ".*"
            desktop-load-locked-desktop nil)
      (desktop-save-mode t)
      ;; Read the desktop now
      (message (format "Reading desktop from %s, %s" desktop-base-file-name desktop-path))
      (desktop-read)
      )
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frameset version

(defun my/saved-frames-file ()
  "Return the path to the saved frameset file."
  (expand-file-name "var/saved-frames.frameset" user-emacs-directory))

(defun my/save-frame-position ()
  "Save the current frame layout to a file."
  (let ((frameset (frameset-save nil)))
    (with-temp-file (my/saved-frames-file)
      (prin1 frameset (current-buffer)))))

(defun my/restore-frame-position ()
  "Restore the last saved frame layout from file."
  (let ((file (my/saved-frames-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((frameset (read (current-buffer))))
          (frameset-restore frameset :reuse-frames t))))))

(when (eq window-save-restore-method 'frameset)
  (add-hook 'kill-emacs-hook #'my/save-frame-position)
  (add-hook 'after-init-hook #'my/restore-frame-position)
  )

;; Try bookmark+ -- can use it just for saving/restoring named desktops.
;; It can do a lot more than that but bkmp-set-desktop-bookmark,
;; bkmp-jump-desktop, and bkmp-bmenu-list should be good enough for a
;; start.
(use-package bookmark+
  :after desktop
  :ensure (:host github :repo "emacsmirror/bookmark-plus")
  :config
  (setq bmkp-desktop-jump-save-before-flag t)) ;; Auto-save before switching


(provide 'init-desktop)
