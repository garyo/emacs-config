;;; init-project.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; A "homedir" project back-end for dotfiles managed by yadm

(defun project-try-home (dir)
  "Return a special project type if the project *is* the user's home dir.
This is used for managing dotfiles."
  (let ((dir1 (directory-file-name (expand-file-name dir)))
        (home (directory-file-name (expand-file-name "~"))))
    (cond ((string-equal dir1 home)
         `(homedir ,dir))
        (t nil))))

(defun yadm-list-files ()
  "Run 'yadm ls-files' and return the result as a list of filenames."
  (let* ((default-directory (expand-file-name "~"))
         (output (shell-command-to-string "yadm ls-files"))
         (file-list (split-string output "\n" t)))
    file-list))


(with-eval-after-load 'project
  (add-hook 'project-find-functions 'project-try-home)

  (cl-defmethod project-name ((project (head homedir)))
    "homedir")

  (cl-defmethod project-root ((project (head homedir)))
    (cadr project))

  (cl-defmethod project-files ((project (head homedir)) &optional dirs)
    "Ignore dirs for now, just list all homedir project files."
    (cond ((executable-find "yadm")
           (yadm-list-files))
          (t nil))
    )

  (cl-defmethod project-buffers ((project (head homedir)))
    nil)
  )

(provide 'init-project)
