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
;; Frameset version (actually I don't even need frameset -- just save/load frame props)

(defvar my-frame-geometry-file
  (expand-file-name "var/saved-frame-geometry.el" user-emacs-directory)
  "File for saving only the top-level frame geometry (no subwindows).")

(defun my/get-frame-geometry ()
  "Return an alist of just the geometry-related frame parameters for the current frame."
  (let ((parms (frame-parameters nil)))  ; nil => current (selected) frame
    (list (cons 'left       (cdr (assq 'left       parms)))
          (cons 'top        (cdr (assq 'top        parms)))
          (cons 'width      (cdr (assq 'width      parms)))
          (cons 'height     (cdr (assq 'height     parms)))
          (cons 'fullscreen (cdr (assq 'fullscreen parms))))))

(defun my/save-frame-geometry ()
  "Write the current frame geometry to `my-frame-geometry-file`."
  (with-temp-file my-frame-geometry-file
    (prin1 (my/get-frame-geometry) (current-buffer))))

(defun check-frame-position-sanity (frame-params &optional min max)
  "Check if frame position parameters are within a reasonable range.
FRAME-PARAMS is an alist with frame parameters.
MIN is the minimum acceptable value (default 0).
MAX is the maximum acceptable value (default 10000).
This helps prevent the window being completely off-screen."
  (let ((min-val (or min -10))
        (max-val (or max 10000))
        (result t))
    (dolist (param '(left top))
      (when-let* ((pair (assq param frame-params))
                  (val (cdr pair)))
        (when (or (< val min-val) (> val max-val))
          (message "Frame parameter %s has suspicious value: %s" param val)
          (setq result nil))))
    result))

(defun my/restore-frame-geometry ()
  "Restore frame geometry from `my-frame-geometry-file`, if it exists."
  (when (file-exists-p my-frame-geometry-file)
    (condition-case err
        (let ((geom (with-temp-buffer
                      (insert-file-contents my-frame-geometry-file)
                      (read (current-buffer)))))
          (when (check-frame-position-sanity geom)
            (message (format "Restoring frame geometry %s" geom))
            (modify-frame-parameters nil geom)))
      (error
       (message "Error restoring frame geometry: %S" err)))))

(when (eq window-save-restore-method 'frameset)
  (add-hook 'kill-emacs-hook #'my/save-frame-geometry)
  (add-hook 'after-init-hook #'my/restore-frame-geometry)
  )


(provide 'init-desktop)
