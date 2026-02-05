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

;;; eglot-python-preset: bridges uv environments with LSP servers
;;; Automatically detects uv environments and configures ty
(use-package eglot-python-preset
  :ensure (:host github :repo "mwolson/eglot-python-preset")
  :after eglot
  :custom
  (eglot-python-preset-lsp-server 'ty)
  :config
  (eglot-python-preset-setup)
  ;; Override to use uvx (runs ty without needing it installed)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("uvx" "ty" "server"))))

;;; Simple uv-based Python setup (no slow pet virtualenv detection)
(add-hook 'python-base-mode-hook
          (lambda ()
            ;; Use uv to run python/pytest in the correct environment
            (setq-local python-shell-interpreter "uv")
            (setq-local python-shell-interpreter-args "run python")
            (setq-local python-pytest-executable "uv run pytest")
            ;; ruff formatting on save
            (ruff-format-on-save-mode)))


(provide 'init-python)
