;;; gco-pkm-consult.el --- Consult integration for gco-pkm -*- lexical-binding: t; -*-

;; This file adds consult integration to gco-pkm for live preview

;;; Code:

;(require 'gco-pkm)
(require 'consult)

;;;; Consult Integration

(defun gco-pkm-consult-format-results (results)
  "Format org-ql RESULTS for consult."
  (mapcar (lambda (element)
            (let* ((marker (org-element-property :org-marker element))
                   (title (org-element-property :raw-value element))
                   (file (buffer-file-name (marker-buffer marker)))
                   (file-base (file-name-sans-extension
                              (file-name-nondirectory file))))
              (propertize (format "%s: %s" file-base title)
                         'consult--candidate marker)))
          results))

;;;###autoload
(defun gco-pkm-consult-search (tag)
  "Search PKM files for TAG using consult with live preview.
As you navigate results with C-n/C-p, preview automatically shows the location."
  (interactive
   (list (or (and (derived-mode-p 'org-mode)
                  (let* ((title (cadr (assoc "TITLE" (org-collect-keywords '("TITLE")))))
                         (filename (file-name-sans-extension
                                   (file-name-nondirectory (buffer-file-name)))))
                    ;; Try to extract tag from title like "Emacs (#emacs)" or "tag-emacs"
                    (or (when title
                          (if (string-match "#\\([a-zA-Z0-9_-]+\\)" title)
                              (match-string 1 title)
                            title))
                        ;; If filename starts with "tag-", extract the tag part
                        (when (string-match "^tag-\\(.+\\)$" filename)
                          (match-string 1 filename))
                        filename)))
             (read-string "Tag to search: "))))
  (let* ((results (gco-pkm-consult-format-results
                   (org-ql-select
                     (directory-files-recursively gco-pkm-directory "\\.org$")
                     `(regexp ,(format "#%s" tag))
                     :action 'element-with-markers
                     :sort '(date priority))))
         (marker (when results
                   (consult--read
                    results
                    :state (consult--jump-state)  ; This gives you the preview!
                    :category 'consult-org-heading
                    :prompt (format "References to #%s: " tag)
                    :sort nil
                    :lookup #'consult--lookup-candidate))))
    (cond
     ((null results)
      (message "No results found for #%s" tag))
     ((null marker)
      (message "Cancelled"))
     (t
      (let ((buffer (marker-buffer marker))
            (pos (marker-position marker)))
        (unless buffer (user-error "No buffer"))
        (pop-to-buffer-same-window buffer)
        (goto-char pos)
        (when (derived-mode-p 'org-mode)
          (org-show-context 'agenda)))))))

;; Override the query link follow to use consult
(defun gco-pkm-query-link-follow-consult (tag)
  "Open consult search for TAG with live preview."
  (gco-pkm-consult-search tag))

;; Re-register query link to use consult version
(with-eval-after-load 'gco-pkm
  (org-link-set-parameters "query"
                           :follow #'gco-pkm-query-link-follow-consult
                           :export #'gco-pkm-query-link-export
                           :face '(:foreground "purple" :underline t)))

(provide 'gco-pkm-consult)
;;; gco-pkm-consult.el ends here
