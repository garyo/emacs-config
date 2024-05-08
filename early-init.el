;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:

(defconst emacs-start-time (float-time))
(defun print-time-since-init (loc)
  (let* ((now (float-time))
         (elapsed (- now emacs-start-time)))
    (message "Init file %s: time=%.4f sec" loc elapsed)))

(message "Starting emacs... early-init.el")

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Disable `package' in favor of `straight' or `elpaca'.
(setq package-enable-at-startup nil)


;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
(setq load-prefer-newer t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be an expensive part of changing the
;; font. Inhibit this to reduce startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; These are good notes on optimizing startup performance:
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast

(print-time-since-init "early-init.el")
(provide 'early-init)

;;; early-init.el ends here
