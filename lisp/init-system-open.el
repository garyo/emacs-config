;;; init-system-open.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun open-with-system-browser (url &optional _new-window)
  (interactive "sURL: ")
  "Open the given file (may be a file, dir or URL) in the system's file explorer/finder/browser"
  (let ((path (cond ((string-match "^file:///[a-z]:/" url)
                     (substring url 8)) ; remove scheme, expose drive (Win32)
                    ((string-match "^file://" url)
                     (substring url 7)) ; remove scheme
                    (t url))))
    (cond (wsl-p
           (browse-url-xdg-open url))
          ((eq system-type 'windows-nt)
           (start-process "system-start" nil "cmd" "/c" "start" path))
          ((eq system-type 'darwin)
           (shell-command (concat "open " (shell-quote-argument path))))
          (t
           (browse-url-xdg-open url))
          )))

;; Make browse-url open files with system browser (explorer/Finder/etc.), not emacs
(with-eval-after-load 'browse-url
  (add-to-list 'browse-url-handlers '("^file:///" . open-with-system-browser))
  )

(defun open-folder-in-explorer ()
  "Call when editing a file in a buffer.

      Open windows explorer in the current directory and select the current file"
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))
(global-set-key [f12] 'open-folder-in-explorer)


(provide 'init-system-open)
