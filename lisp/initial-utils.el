;;; initial-utils.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun maybe-require (feature)
  "Try to require FEATURE (symbol); return feature or nil."
  (require feature nil t))

;; I want to avoid any evaluation of code for other OSes, even by the
;; byte compiler, to avoid byte-compiler warnings (and thus flycheck
;; squiggles).
(eval-when-compile
  (defmacro when-mac (&rest body)
    "Evaluate BODY only when `system-type' is `darwin'."
    (if (eq system-type 'darwin)
        `(progn ,@body)
      nil)
    )
  (defmacro when-windows (&rest body)
    "Evaluate BODY only when `system-type' is `windows-nt'."
    (if (eq system-type 'windows-nt)
        `(progn ,@body)
      nil)
    )
  (defmacro when-android (&rest body)
    "Evaluate BODY only when `system-type' is `android'."
    (if (eq system-type 'android)
        `(progn ,@body)
      nil)
    )
  )

(provide 'initial-utils)
