;;; Set up package system -- straight.el
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

;;; Meta-package system: use-package. Auto-installs and configures packages.
(straight-use-package 'use-package)
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t) ; make use-package use straight

(use-package org
 :config
 (require 'org-mouse)
)

(set-variable 'debug-on-error t)
