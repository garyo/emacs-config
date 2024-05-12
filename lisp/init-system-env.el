;;; init-system-env.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; System-environment runs a shell command that prints VAR=VALUE lines, then
;; imports those into emacs's environment.

(use-package system-environment
  :ensure (:host github
                 :repo "bwachter/system-environment"
                 :branch "master"
                 :fork (:repo "garyo/emacs-system-environment"))
  :config
  (defun gco-update-exec-path-from-PATH ()
    "Update emacs's exec-path from PATH env var. Tries to determine
    path separator by looking for Windows drive letter. Used as a hook for system-environment."
    (let* ((path (getenv "PATH"))
           (pathsep (if (string-match-p "\\b[cdCD]:[\\/]" path) ";" ":")))
      (setq exec-path (seq-uniq
                       (append (split-string (getenv "PATH") pathsep) exec-path)
                       'string=))))
  (add-hook 'system-environment-import-hook 'gco-update-exec-path-from-PATH)
  (add-hook 'system-environment-import-async-hook 'gco-update-exec-path-from-PATH)
  (let ((verbose nil)
        (keep-buffer nil)
        (env-vars '("SSH_AUTH_SOCK" "LANG" "LC_ALL" "CLANGD_FLAGS" "GIT_SSH_COMMAND")))
    ;; Try zsh, then bash. Windows is special.
    (cond ((and (eq system-type 'windows-nt) (executable-find "zsh"))
           (message "Updating emacs env vars from Windows zsh")
           (system-environment-import-from-command
            "zsh -i -c env"
            env-vars
            nil keep-buffer verbose)
           ;; Handle $PATH specially; translate from cygwin style to Windows style
           (message "Updating emacs path from Windows zsh")
           (system-environment-import-from-async-command
            '("zsh" "-i" "-c" "echo PATH=$(cygpath -p -m $PATH)")
            '("PATH")
            nil keep-buffer verbose "*system-environment-2*"))
          ((executable-find "zsh")
           (message "Updating emacs path from zsh")
           (system-environment-import-from-async-command
            "zsh -i -c env"
            (cons "PATH" env-vars)
            nil keep-buffer verbose))
          ((executable-find "bash")
           (message "Updating emacs path from bash")
           (system-environment-import-from-async-command
            "bash -i -c env"
            (cons "PATH" env-vars)
            nil keep-buffer verbose))
          (t
           (message "No shell found; not importing system environment"))))
  )


(provide 'init-system-env)
