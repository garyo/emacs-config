;;; init-python.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Python setup using the Astral toolchain (uv, ruff, ty).
;;; Uses uv directly for running tools - no slow virtualenv detection needed.

(use-package pytest)

;;; Ruff formatting via uvx (no installation needed)
(use-package reformatter
  :config
  (reformatter-define ruff-format
    :program "uvx"
    :args `("ruff" "format" "--stdin-filename" ,buffer-file-name "-")))

;;; rassumfrassum: LSP multiplexer by eglot's author (João Távora).
;;; Runs both ty (type checker) and ruff (linter) as a single
;;; multiplexed LSP server for eglot.  Zero-install via uvx --with.
;;; useUv "scripts" has ty ask uv for each PEP 723 script's environment
;;; (needs uv >= 0.12.3); rass passes the option to ruff too, which ignores it.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("uvx" "--from" "rassumfrassum"
                    "--with" "ty" "--with" "ruff"
                    "rass" "python"
                    :initializationOptions
                    (:experimental (:useUv "scripts"))))))

(defun my-python-pep723-script-p ()
  "Return non-nil if this buffer has PEP 723 inline script metadata."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^# /// script$" nil t)))

;;; Simple uv-based Python setup (no slow pet virtualenv detection)
(add-hook 'python-base-mode-hook
          (lambda ()
            (let ((script (and buffer-file-name
                               (my-python-pep723-script-p)
                               (shell-quote-argument
                                (file-local-name buffer-file-name)))))
              ;; Use uv to run python/pytest in the correct environment,
              ;; adding a PEP 723 script's own dependencies to the REPL
              (setq-local python-shell-interpreter "uv")
              (setq-local python-shell-interpreter-args
                          (if script
                              (concat "run --with-requirements " script " python")
                            "run python"))
              (setq-local pytest-global-name "uv run pytest")
              ;; M-x compile type-checks with the lockfile-pinned ty
              (setq-local compile-command
                          (if script
                              (concat "uv check --script " script)
                            "uv check")))
            ;; ruff formatting on save
            (ruff-format-on-save-mode)))


(provide 'init-python)
