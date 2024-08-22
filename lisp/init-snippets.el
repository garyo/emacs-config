;;; init-snippets.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; File skeletons and snippets

;;;; Yasnippet -- autocomplete various language snippets
;; TAB expands snippet "keys" (abbrevs) and moves to next field
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  ;; This is a bit questionable: during an expansion, yasnippet normally uses
  ;; TAB to accept a field and move to the next field. But company also binds
  ;; TAB (to advance to common completion), so when a completion is in
  ;; progress _and_ it has a snippet to expand, TAB doesn't work.
  ;; So this uses a function bound to C-o to either expand an active snippet,
  ;; or else do the usual open-line.
  (global-set-key (kbd "C-o") 'yasnippet-or-open-line)
  (defun yasnippet-or-open-line ()
    "Call `open-line', unless there are abbrevs or snippets at point.
In that case expand them.  If there's a snippet expansion in progress,
move to the next field. Call `open-line' if nothing else applies."
    (interactive)
    (cond ((expand-abbrev))
          ((yas-active-snippets)
           (yas-next-field-or-maybe-expand))
          ((ignore-errors
             (yas-expand)))
          (t
           (open-line 1))))
  )


;; all the snippets -- this is big!
(use-package yasnippet-snippets
  :defer 5)

;; I define some yasnippets using ~company-name~ which is intended to be
;; set in a .dir-locals.el file, so mark it as safe for all string values
(defvar company-name
  "Dark Star Systems, Inc." "Company name, for use in file snippets")
(put 'company-name 'safe-local-variable #'stringp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto insert snippet into new files

;; Define an alist mapping file extensions to yasnippet names (*not* keys!)
(defvar my-extension-snippet-alist
  '(("^\\(cpp\\|cxx\\)$" . "C++ CPP source file header")
    ("^\\(h\\|hpp\\|hxx\\)$" . "C++ CPP header file header")
    )
  "Alist mapping file extensions to yasnippet keys.")

(defun my-auto-insert-snippet ()
  "Automatically insert a specific yasnippet when creating a new file based on its extension."
  (when (and buffer-file-name
             (zerop (buffer-size))) ;; Ensure the buffer is empty (new file)
    (let* ((file-extension (file-name-extension buffer-file-name))
           (snippet-name (assoc-default file-extension my-extension-snippet-alist 'string-match-p)))
      (condition-case err
          (when snippet-name
            (yas-expand-snippet (yas-lookup-snippet snippet-name)))
        (error
         ;; maybe no such snippet
         (message (error-message-string err))
         nil)
        ))))

(add-hook 'find-file-hook 'my-auto-insert-snippet)


(provide 'init-snippets)
