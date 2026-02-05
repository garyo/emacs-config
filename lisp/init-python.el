;;; init-python.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Ruff is a new 2024 much-faster code linter/formatter
(use-package ruff-format)

(use-package pytest)

(use-package python-isort)


;;; Pet is Python Executable Tracker.
;;; Supports all kinds of virtualenvs, especially "uv"
(use-package pet
  :config
  ;;; Master python hook
  (add-hook 'python-base-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))

              (pet-eglot-setup)
              (eglot-ensure)

              (setq-local dap-python-executable python-shell-interpreter)

              (setq-local python-pytest-executable (pet-executable-find "pytest"))

              (when-let ((ruff-executable (pet-executable-find "ruff")))
                (setq-local ruff-format-command ruff-executable)
                (ruff-format-on-save-mode))

              (when-let ((isort-executable (pet-executable-find "isort")))
                (setq-local python-isort-command isort-executable)
                (python-isort-on-save-mode)))))


(provide 'init-python)
