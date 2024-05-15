;;; init-elisp-mode.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs Lisp mode setup
;; Got this from https://github.com/holgerschurig/emacs-doom-config/blob/master/config.el#L387

;;; Code:

(defun my-emacs-lisp-mode-setup ()
    (interactive)
    "My emacs lisp mode setup function."
    ;; "-" is almost always part of a function- or variable-name
    (modify-syntax-entry ?- "w")

    ;; make sure we cannot save syntax errors
    (add-hook 'local-write-file-hooks 'check-parens)

    ;; Modify completions, elisp-completion-at-point wouldn't allow me to
    ;; complete elisp things in comments.
    (defalias 'my-elisp-capf (cape-capf-super #'elisp-completion-at-point
                                              #'cape-dabbrev
                                              #'cape-file
                                              #'cape-dict
                                              #'cape-elisp-symbol))
    (setq-local completion-at-point-functions '(my-elisp-capf t))

    ;; The following changes the imenu "M-g i" to care most about ";;;" comments as sections
    (setq lisp-imenu-generic-expression '())
    (setq imenu-generic-expression
          (list
           (list (purecopy "Type")
                 (purecopy (concat "^\\s-*("
                                   (eval-when-compile
                                     (regexp-opt
                                      '(;; Elisp
                                        "defgroup" "deftheme"
                                        "define-widget" "define-error"
                                        "defface" "cl-deftype" "cl-defstruct"
                                        ;; CL
                                        "deftype" "defstruct"
                                        "define-condition" "defpackage"
                                        ;; CLOS and EIEIO
                                        "defclass")
                                      t))
                                   "\\s-+'?\\(" lisp-mode-symbol-regexp "\\)"))
                 2)
           (list (purecopy "Variable")
                 (purecopy (concat "^\\s-*("
                                   (eval-when-compile
                                     (regexp-opt
                                      '(;; Elisp
                                        "defconst" "defcustom"
                                        ;; CL
                                        "defconstant"
                                        "defparameter" "define-symbol-macro")
                                      t))
                                   "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                 2)
           ;; For `defvar'/`defvar-local', we ignore (defvar FOO) constructs.
           (list (purecopy "Variable")
                 (purecopy (concat "^\\s-*(defvar\\(?:-local\\)?\\s-+\\("
                                   lisp-mode-symbol-regexp "\\)"
                                   "[[:space:]\n]+[^)]"))
                 1)
           (list "Function"
                 (purecopy (concat "^\\s-*("
                                   (eval-when-compile
                                     (regexp-opt
                                      '("defun" "defmacro"
                                        ;; Elisp.
                                        "defun*" "defsubst" "define-inline"
                                        "define-advice" "defadvice" "define-skeleton"
                                        "define-compilation-mode" "define-minor-mode"
                                        "define-global-minor-mode"
                                        "define-globalized-minor-mode"
                                        "define-derived-mode" "define-generic-mode"
                                        "ert-deftest"
                                        "cl-defun" "cl-defsubst" "cl-defmacro"
                                        "cl-define-compiler-macro" "cl-defgeneric"
                                        "cl-defmethod"
                                        ;; CL.
                                        "define-compiler-macro" "define-modify-macro"
                                        "defsetf" "define-setf-expander"
                                        "define-method-combination"
                                        ;; CLOS and EIEIO
                                        "defgeneric" "defmethod")
                                      t))
                                   "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                 2)
           (list "require"
                 (concat "^\\s-*(require\\s-+'\\(" lisp-mode-symbol-regexp "\\)")
                 1)
           (list "use-package"
                 (concat "^\\s-*(use-package\\s-+\\(" lisp-mode-symbol-regexp "\\)")
                 1)
           (list "Section"
                 "^;;[;]\\{1,8\\} \\(.*$\\)"
                 1))))
(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-setup)


(provide 'init-elisp-mode)
