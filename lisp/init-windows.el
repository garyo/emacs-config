;;; init-windows.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;; Settings to determine system type, including WSL 1 and 2 on Windows.

;;; Code:

(defun scoop-msys-root ()
  (let*
      ((scoop-root (expand-file-name "~/scoop/apps/msys2/current"))
       )
    scoop-root
  ))

(defvar msys-root
  (cond ((file-exists-p "c:/tools/msys64/msys64")
         "c:/tools/msys64/msys64")
        ((file-exists-p "c:/tools/msys64")
         "c:/tools/msys64")
        ((file-exists-p (scoop-msys-root))
         (scoop-msys-root))
        (t
         "NO_MSYS"))
  "Root of Msys64 install; should contain e.g. usr/bin/zsh.exe")

(defmacro msys-path (file)
  "Path within msys dir of FILE. FILE should be relative (no leading /)."
  `(expand-file-name ,file msys-root))

(if (eq system-type 'windows-nt)
    (push (msys-path "usr/bin") exec-path) ; for msys/linux "find", needed by straight.el
  )
(defconst msys-p (not (equal msys-root "NO_MSYS")))

(require 'rx)       ; not sure why this is needed but it is, for pcase
;;; detect whether running under WSL 1 or 2, using /proc/version
;;; Sets constants "wsl-p", "wsl1-p", and "wsl2-p"
(defconst wsl-version
  (let* ((subproc-output
	(condition-case nil
            (with-temp-buffer
              (list (call-process "cat" nil (current-buffer) nil
				  "/proc/version")
                    (buffer-string)))
	    (error '(-1 ""))))
       (status (car subproc-output))
       (output (cadr subproc-output))
       (wsl-version (if (= status 0)    ;/proc/version found; check string
                        (pcase output
                          ((rx "WSL2") 'wsl2)
                          ((rx "Microsoft@Microsoft.com") 'wsl1))
                      nil)))
  (message "WSL version is %s" wsl-version)
  wsl-version)
  "If running under WSL, the WSL version as wsl1 or wsl2, else nil")
(defconst wsl-p (or (eq wsl-version 'wsl1) (eq wsl-version 'wsl2))
  "Running under Windows WSL (any version)")
(defconst wsl1-p (eq wsl-version 'wsl1) "Running under Windows WSL (1, not 2)")
(defconst wsl2-p (eq wsl-version 'wsl2) "Running under Windows WSL2")

;; Stupid workaround for WSLg (Windows/WSL2 GUI mode) 1.0.28 as of Nov 2021
;; see https://github.com/microsoft/wslg/issues/207
(when wsl2-p
  (defun delay-exit ()
    (interactive)
    (save-some-buffers)
    (sit-for 0.4)
    (kill-emacs))
  (global-set-key (kbd "C-x C-c") 'delay-exit)
  )

(when-windows
 (setq tramp-use-ssh-controlmaster-options nil))

(when-windows
 ;; make PC keyboard's Windows key be Super or Hyper (Windows only)
 ;; (There are a few that aren't rebindable: Win-L, Win-G at least)
 ;; This is nice because e.g. Super-p is the prefix for Projectile
 (setq w32-pass-lwindow-to-system nil)
 (setq w32-lwindow-modifier 'super)     ; Left Windows key
 (w32-register-hot-key [s-]) ; disable all Windows shortcuts while Emacs has focus

 ;; I don't usually use right-windows but why not
 (setq w32-pass-rwindow-to-system nil)
 (setq w32-rwindow-modifier 'super)     ; Right Windows key

 ;; The menu/app key (to the right of the right Windows key) is
 ;; pretty hard to reach with right pinky, so it's less useful, and
 ;; only on certain keyboards, but my Das Keyboard has it, so why not.
 (setq w32-apps-modifier 'hyper)        ; Menu/App key

 ;; Program paths
 (setq
  find-dired-find-program "/bin/find"
  find-program "/bin/find"
  grep-program "/bin/grep"
  )
 )

(provide 'init-windows)
