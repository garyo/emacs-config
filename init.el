;; -*- lexical-binding: t; -*-
;;; Gary Oberbrunner's Emacs init file

(message "User-emacs-directory is %S" user-emacs-directory)
(message "Loading init.el, ~ is %S" (expand-file-name "~"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bootstrap: load straight.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set up package system -- straight.el (better than built-in package.el)
(defvar bootstrap-version)
;; Don't check for modifications on every emacs startup -- takes too long
(defvar straight-check-for-modifications '(find-when-checking) "When should straight.el check for modified packages")
(or (boundp 'native-comp-deferred-compilation-deny-list)
    (setq native-comp-deferred-compilation-deny-list '()))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
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
   :commands '(org-mode org-babel-load-file)
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
