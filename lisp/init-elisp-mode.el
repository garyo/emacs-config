;;; init-elisp-mode.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs Lisp mode setup
;; Got this from https://github.com/holgerschurig/emacs-doom-config/blob/master/config.el#L387

;;; Code:

(defun my-emacs-lisp-mode-setup ()
    "My Emacs Lisp mode setup function."
    (interactive)
    ;; "-" is almost always part of a function- or variable-name
    (modify-syntax-entry ?- "w")

    ;; make sure we cannot save syntax errors
    ;(add-hook 'write-file-functions 'check-parens)

    ;; Modify completions, elisp-completion-at-point wouldn't allow me to
    ;; complete elisp things in comments.
    (defalias 'my-elisp-capf
      (if (functionp #'cape-capf-super)
          (cape-capf-super #'elisp-completion-at-point
                           #'cape-dabbrev
                           #'cape-file
                           #'cape-dict
                           #'cape-elisp-symbol)
        #'elisp-completion-at-point))
    (setq-local completion-at-point-functions '(my-elisp-capf t))
)
(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-setup)
(customize-set-variable 'elisp-fontify-semantically t)


(provide 'init-elisp-mode)
