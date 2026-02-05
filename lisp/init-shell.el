;;; init-shell.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'shell)

;; use zsh or bash.  Do this early on before loading any git stuff,
;; otherwise that will try to use cmdproxy.exe.

(cond ((file-exists-p (msys-path "usr/bin/zsh.exe"))
       (setq explicit-shell-file-name (msys-path "usr/bin/zsh.exe")))
      ((executable-find "zsh")
       (setq explicit-shell-file-name "zsh"))
      ((executable-find "bash")
       (setq explicit-shell-file-name "bash"))
      (t
       (message "Can't find zsh!")))

;; Setting this will make emacs use this shell for subprocesses
;; (shell-command, start-file-process, compilations, etc.)
;; Beware: on Windows with msys zsh, it'll translate paths
;; which might be what you want sometimes, but not others!
;; (so "grep /foo" will turn into "grep c:/tools/msys64/msys64/foo")
;; In that case you can double the initial slash (maybe!).
(setq shell-file-name explicit-shell-file-name)

;; Set up f7 to start or switch to shell.
;; Repeat presses switch to next shell buffer.
;; Would be nice if it worked with eshell.
(defun sh-buf-filter (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
(defun shell-dwim (&optional create)
  "Start or switch to an inferior shell process, in a smart way.
    If a buffer with a running shell process exists, simply switch
    to that buffer.  If a shell buffer exists, but the shell
    process is not running, restart the shell.  If already in an
    active shell buffer, switch to the next one, if any.  With
    prefix argument CREATE always start a new
    shell."
  (interactive "P")
  (let ((next-shell-buffer) (buffer)
        (shell-buf-list (identity ;;used to be reverse
                         (sort
                          (sh-buf-filter (lambda (x) (string-match "^\\*shell\\*" (buffer-name x))) (buffer-list))
                          #'(lambda (a b) (string< (buffer-name a) (buffer-name b)))))))
    (setq next-shell-buffer
          (if (string-match "^\\*shell\\*" (buffer-name buffer))
              (get-buffer (cadr (member (buffer-name) (mapcar (function buffer-name) (append shell-buf-list shell-buf-list)))))
            nil))
    (setq buffer
          (if create
              (generate-new-buffer-name "*shell*")
            next-shell-buffer))
    (shell buffer)
    ))
(global-set-key [f7] 'shell-dwim)
(global-set-key [f8] 'eshell)

;; Dirtrack mode in shell buffers; finds prompts with dir name
;; which should be better with msys2/cygwin where I can emit a
;; Windows-style dir name in the prompt.
(require 'dirtrack)
(add-hook 'shell-mode-hook
          #'(lambda ()
              (setq dirtrack-list '("(\\(.*?\\)\\( \\|) \\)" 1 t))
              (dirtrack-mode 1)))

(defface shell-hilight-face
  '((t (:background "grey80")))
  "Used for marking significant items in shell buffers."
  :group 'shell)
  ;; Hilight compiler and linker output filenames so I can see them more easily
(defvar my-shell-extra-keywords
  '(("/OUT:[^ ]+" 0 shell-hilight-face)
    ("/Fo[^ ]+" 0 shell-hilight-face)
    ))
(add-hook 'shell-mode-hook
          (lambda ()
            (font-lock-add-keywords nil my-shell-extra-keywords)))
(add-hook 'comint-output-filter-functions 'ansi-color-process-output)
(ignore-errors
  (pcomplete-shell-setup)	; set up emacs24 programmable completion for shell mode; not that great but OK
  )


(setq
 shell-pushd-regexp "pushd\\|1\\|2"
 shell-pushd-dextract t
 shell-pushd-dunique t
                                        ;shell-cd-regexp nil			; autopushd in zsh
 shell-chdrive-regexp "[a-z]:")		;

;;This is from Voelker's emacs NT page:
(defvar explicit-zsh-args)
(setq explicit-bash-args '("--login" "--noediting" "-i")
                                        ; explicit-zsh-args '("-i" "-o" "emacscygwinhack")
      explicit-zsh-args '("-i")
      comint-completion-addsuffix t
                                        ; comint-process-echoes nil ;; t for command.com, nil for bash
      comint-eol-on-send t
      comint-input-ignoredups t
      comint-input-ring-size 256
      )

(make-variable-buffer-local 'comint-completion-addsuffix)

;;; eshell (shell implemented entirely in emacs, sometimes useful):
(add-hook 'eshell-mode-hook
          (function
           (lambda ()
             ;; This prevents vertical bars between letters in typed-in text
             ;; (probably an emacs 21.1 bug?)
             (setq cursor-type '(bar . 10))
             )))

;;; EAT: Emulate A Terminal. Nice new (2024) terminal emulator.
;; See my bug report: https://codeberg.org/akib/emacs-eat/issues/167#issuecomment-2078670
(use-package eat
  :ensure (:host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (setq eat-tic-path "/usr/bin/tic") ; Needed on MacOS with homebrew, to use system terminfo compiler
  )

(provide 'init-shell)
