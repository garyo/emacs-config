;;; Gary Oberbrunner's Emacs init file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bootstrap: load straight.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set up package system -- straight.el (better than built-in package.el)
(defvar bootstrap-version)
(or (boundp 'native-comp-deferred-compilation-deny-list)
    (setq native-comp-deferred-compilation-deny-list '()))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Use latest org-mode. Do this early, to use when loading config
(straight-use-package
 '(org
   :mode (("\\.org$" . org-mode))
   :commands org-mode
   :config
   (require 'org-mouse)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load main emacs-config.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I use this file-local var in emacs-config.org, so allow it without prompting
(setq safe-local-variable-values
   '((org-src-preserve-indentation . t))
)
(org-babel-load-file (concat user-emacs-directory "emacs-config.org"))

;;; init.el ends here
