;;; init-error-checker.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Set up flycheck or flymake.
;; As of 2024 I'm using built-in flymake.

;; flycheck/flymake are syntax/error checkers for many languages.
;; Languages with LSP support override the checkers, but this can
;; still be useful for other languages.

;; Bindings begin with ~C-c !~ or, better, use Consult ~M-g f~

;; I may switch between flycheck and flymake; the community seems to
;; be moving to flymake since it got a rewrite some time ago. So this
;; code allows using either one. See top-level init use-flycheck-mode.

;; This file also sets up some compilation mode settings especially
;; related to finding errors.

(defun setup-flycheck ()
  "Set up flycheck as the checker"
  (use-package flycheck
    :config (global-flycheck-mode)
    )

  ;; show flycheck errors in popup, not in minibuffer. This is important
  ;; because minibuffer may be showing documentation or something else,
  ;; and without this flycheck errors/warnings overwrite that info.
  (use-package flycheck-posframe
    :after flycheck
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
    (flycheck-posframe-configure-pretty-defaults))

  ;; for Windows, especially for emacs-lisp checker which passes
  ;; lots of cmd-line args to emacs
  (cond ((eq system-type 'windows-nt)
         (setq flycheck-command-wrapper-function
               (lambda (cmd)
                 (list "bash" "-c" (format "%s"
                                           (mapconcat 'shell-quote-argument cmd " ")))))))

  ;; On Windows, commands run by flycheck may have CRs (\r\n line endings).
  ;; Strip them out before parsing.
  (defun flycheck-parse-output (output checker buffer)
    "Parse OUTPUT from CHECKER in BUFFER.

OUTPUT is a string with the output from the checker symbol
CHECKER.  BUFFER is the buffer which was checked.

Return the errors parsed with the error patterns of CHECKER."
    (let ((sanitized-output (replace-regexp-in-string "\r" "" output))
          )
      (funcall (flycheck-checker-get checker 'error-parser) sanitized-output checker buffer)))

  ;; Set flycheck list window to be small -- fit to content
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote "*Flycheck errors*")
                 (display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (window-height . fit-window-to-buffer)))
  )

(defun setup-flymake ()
  "Set up built-in flymake as the checker"

  (use-package flymake
    :ensure nil                         ; use emacs built-in version
    )

  ;; NOTE: don't need flymake-posframe with eglot, at least in C++,
  ;; because eglot gets errors from its LSP server and displays them
  ;; in the eldoc-box (see below).
  (defun flymake-posframe-mode-if-not-eglot (&rest args)
    (unless eglot--managed-mode
      (apply 'flymake-posframe-mode args)))

  (use-package flymake-posframe
    ;; Note: this is a fork of the main flymake-posframe, with a fix for eglot
    :ensure (:host github
                   :repo "articuluxe/flymake-posframe"
                   :branch "feature/eglot")
    :demand t
    :hook (flymake-mode . flymake-posframe-mode-if-not-eglot)
    )
  ;; This is what eglot uses to show popup doc windows on hover
  (use-package eldoc-box
    ;; note: I've set 'variable-pitch to be quite large, too large for
    ;; doc boxes, so best to explicitly set height here.
    :custom-face (eldoc-box-body ((t (:inherit 'variable-pitch :height 110))))
    :hook (eglot-managed-mode . eldoc-box-hover-mode)
    :config
    (setq eldoc-box-max-pixel-width 500)
    :diminish eldoc-box-hover-mode
    )

  (add-to-list 'display-buffer-alist
               `(,(regexp-quote "*eldoc*")
                 (display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (window-height . fit-window-to-buffer)))
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote "*Flymake")
                 (display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (window-height . fit-window-to-buffer)))
  )

(if use-flycheck-mode
    (setup-flycheck)
  (setup-flymake))

;; Customize process-error-filename to find source files more reliably
;; in certain conditions.
(defun process-error-filename (filename &optional spec-directory)
  "Process compile errors from FILENAME, looking for sources in SPEC-DIRECTORY.

      SCons (with -D) starts builds from the top of the source tree,
      and it builds into an 'SBuild' subdir. But we want to find the
      original errors in the regular source dir, regardless of the
      current directory when we run \\[compile]. Note
      \"default-directory\" may not be what you expect here, and the
      filenames are absolute, so need to remove surgically."

  (let ((case-fold-search t)
        (topdir (car (project-roots (project-current))))
        )
    ;; prepend dir
    (if (and spec-directory
             (not (file-name-absolute-p filename)))
        (setq filename (concat spec-directory "/" filename)))

    (let ((candidates (get_src_from_build_path (fix-win-path filename)))
          (result nil))
      ;; (message (format "In process-error-filename: %s in %s: candidates = %s" filename spec-directory candidates))
      (dolist (f candidates)
        (cond ((file-exists-p f)
               (setq result f))
              ((file-exists-p (concat topdir f))
               (setq result (concat topdir f)))))
      (if result result filename))))

(defun fix-win-path (p)
  "Convert backslashes to forward slashes in P so path-handling functions don't get confused."
  (cond (p (replace-regexp-in-string "\\\\" "/" p)))
  )

(defun get_src_from_build_path (p)
  "Strip Sbuild dirs from a pathname P."
  (list
   (replace-regexp-in-string
    "[Ss]?[Bb]uild/.*\\(final\\|release\\|dbg\\|debug\\)[^/]*/" "" p)
   (replace-regexp-in-string
    "[Ss]?[Bb]uild/.*\\(final\\|release\\|dbg\\|debug\\)[^/]*/" "src/" p)
   )
  )

;; For emacs 21.1, this requires a patch to compile.el, which is in
;; Gary's email in the emacs folder (date around 10/25/2001).  Later
;; versions should already have it.
(setq compilation-parse-errors-filename-function 'process-error-filename)

(setq compilation-mode-font-lock-keywords
      '(("^\"\\([^\"]*\", line [0-9]+:[ \t]*warning:[ \t]*\\)\\(.*$\\)"
         2 font-lock-keyword-face)
        ("^\"\\([^\"]*\", line [0-9]+:[ \t]*\\)\\(.*$\\)"
         2 font-lock-function-name-face)))

;; clang compiler error messages are like "edg-1" but the line number has ,COL after it
(with-eval-after-load 'compile
  (let ((clang-err-elt
         '(clang-1 "^\\([^ \n]+\\)(\\([0-9]+\\),\\([0-9]+\\)): \\(?:fatal\\|error\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
                   1 2 3 (4 . 5))))
    (add-to-list 'compilation-error-regexp-alist-alist clang-err-elt)
    (add-to-list 'compilation-error-regexp-alist 'clang-1)))

;; For emacs23, long lines in buffers make emacs really slow.
;; This seems to ameliorate it a little.
(add-hook 'compilation-mode-hook (lambda () (line-number-mode nil)))


(provide 'init-error-checker)
