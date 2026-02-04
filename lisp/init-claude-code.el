;;; init-claude-code.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Integrate Emacs with Claude Code

(use-package claude-code-ide
  :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :custom
  (claude-code-ide-cli-path "~/.claude/local/claude")
  (claude-code-ide-terminal-backend 'eat)
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(provide 'init-claude-code)
;;; init-claude-code.el ends here
