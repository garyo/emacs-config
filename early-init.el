;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:

(when (eq system-type 'android)
  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
    (setq exec-path (append exec-path (list termuxpath)))))


(defconst emacs-start-time (float-time)
  "Time when Emacs was started (early-init loaded).")

(defun print-time-since-init (loc)
  "Print elapsed time since Emacs started.
LOC is the init-file name to print in the message."
  (let* ((now (float-time))
         (elapsed (- now emacs-start-time)))
    (message "Init file %s: time=%.4f sec" loc elapsed)))

(message "Starting emacs... early-init.el")

(defun dir-in-path-p (directory)
  "Return non-nil if DIRECTORY is in PATH environment variable.
Handles path normalization and OS-specific path separators."
  (let* ((normalized-dir (directory-file-name
                         (expand-file-name directory)))
         (path (getenv "PATH"))
         (path-dirs (split-string path path-separator t)))
    (catch 'found
      (dolist (path-dir path-dirs)
        (when (string= normalized-dir
                      (directory-file-name
                       (expand-file-name path-dir)))
          (throw 'found t)))
      nil)))

(defun gco-add-invocation-dir-to-path ()
  "Add Emacs bin directory to path, for native-compilation.
Beware of resetting $PATH later though! If you do, just
call ~gco-add-invocation-dir-to-path~ again."
  (add-to-list 'exec-path invocation-directory)
  (unless (dir-in-path-p invocation-directory)
    (setenv "PATH" (concat invocation-directory path-separator (getenv "PATH")))
    )
  )

(when (eq system-type 'windows-nt)
  (gco-add-invocation-dir-to-path)
  )

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Disable `package' in favor of `straight' or `elpaca'.
(setq package-enable-at-startup nil)

(startup-redirect-eln-cache (expand-file-name "var/eln-cache" user-emacs-directory))

;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
(setq load-prefer-newer t)

;; Faster to disable these here (before they've been initialized)
(unless (eq system-type 'android)
   (push '(menu-bar-lines . 0) default-frame-alist)
   (push '(tool-bar-lines . 0) default-frame-alist)
   (push '(vertical-scroll-bars) default-frame-alist)
   )

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
