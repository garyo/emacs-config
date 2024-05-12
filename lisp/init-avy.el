;;; init-avy.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Avy: jump (or act) on any visible char or string
;;; ⮕ Use ~M-j~ to start searching
;;; See Karthik's article:
;;;  https://karthinks.com/software/avy-can-do-anything
(use-package avy
  :ensure nil
  ;; :bind (("C-:" . avy-goto-char-timer)) -- avy-recommended binding, but it's hard to type
  :bind (("M-j" . avy-goto-char-timer))
  :config
  (define-key isearch-mode-map (kbd "M-j") 'avy-isearch) ; also avy in isearch
  (defun avy-show-dispatch-help ()
    "Prettier help display for avy"
    (let* ((len (length "avy-action-"))
           (fw (frame-width))
           (raw-strings (mapcar
                         (lambda (x)
                           (format "%2s: %-19s"
                                   (propertize
                                    (char-to-string (car x))
                                    'face 'aw-key-face)
                                   (substring (symbol-name (cdr x)) len)))
                         avy-dispatch-alist))
           (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
           (strings-len (length raw-strings))
           (per-row (floor fw max-len))
           display-strings)
      (cl-loop for string in raw-strings
               for N from 1 to strings-len do
               (push (concat string " ") display-strings)
               (when (= (mod N per-row) 0) (push "\n" display-strings)))
      (message "%s" (apply #'concat (nreverse display-strings)))))
  )


(provide 'init-avy)
