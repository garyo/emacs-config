;;; init-desktop.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; save/restore window configs to disk automatically
(use-package desktop
  :ensure nil
  :init
  (setq desktop-base-file-name "emacs.desktop")
  :config
  ;; Doesn't seem to work in wsl2 for now
  (when (not wsl2-p)
    ;; don't save any files; just the window configuration
    ;; and don't load if the desktop file is locked
    (setq desktop-files-not-to-save ".*"
          desktop-buffers-not-to-save ".*"
          desktop-load-locked-desktop nil)
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
    (desktop-save-mode t)
    ;; Read the desktop now
    (message (format "Reading desktop from %s, %s" desktop-base-file-name desktop-path))
    (desktop-read)
    ))

(provide 'init-desktop)
