;;; init-options.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setopt user-full-name "Gary Oberbrunner"
        user-mail-address "garyo@darkstarsystems.com")

;; typescript language server, more RAM:
(setenv "NODE_OPTIONS" "--max-old-space-size=8192")

;;; Prefer utf-8 coding system everywhere, with LF line endings
(prefer-coding-system 'utf-8-unix)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))



(provide 'init-options)
