;;; init-claude-code.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;; Run Claude Code sessions inside Emacs.  claude-code.el drives the
;;; real Claude CLI in ghostel terminals, one or more per project, and
;;; Monet gives each session an IDE connection back to Emacs: Claude
;;; sees the current selection, shows its diffs here, opens files and
;;; reads diagnostics.

;;; Code:

(use-package monet
  :ensure (:host github :repo "stevemolitor/monet"))

(defun gco-claude-code-environment (_buffer-name _directory)
  "Return environment settings for Claude sessions run inside Emacs.
The classic renderer leaves the whole conversation in ghostel's
scrollback, where copy mode and isearch reach it; the fullscreen one
holds only the visible screen.  Prompts and transcripts Claude opens in
an editor open in this Emacs."
  '("CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1" "EDITOR=emacsclient"))

(use-package claude-code
  :ensure (:host github :repo "stevemolitor/claude-code.el")
  :bind-keymap ("C-c C" . claude-code-command-map)
  ;; After C-c C M, keep pressing M to cycle auto-accept/plan/confirm.
  :bind (:repeat-map claude-code-cycle-repeat-map
                     ("M" . claude-code-cycle-mode))
  :custom
  (claude-code-terminal-backend 'ghostel)
  :config
  (add-hook 'claude-code-process-environment-functions
            #'gco-claude-code-environment)
  (add-hook 'claude-code-process-environment-functions
            #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode 1))

(declare-function claude-code--directory "claude-code")
(declare-function claude-code--find-claude-buffers-for-directory "claude-code")

(defun gco-claude-code--has-conversation-p (directory)
  "Return non-nil if DIRECTORY has a Claude conversation to continue.
`claude --continue' exits when DIRECTORY has no transcript from an
interactive session; Claude Code keeps transcripts in a folder under
~/.claude/projects named after the path, and marks interactive ones
with entrypoint \"cli\"."
  (let ((folder (expand-file-name
                 (replace-regexp-in-string
                  "[^A-Za-z0-9]" "-" (directory-file-name (expand-file-name directory)))
                 (expand-file-name "projects" (or (getenv "CLAUDE_CONFIG_DIR") "~/.claude")))))
    (and (file-directory-p folder)
         (seq-some (lambda (file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (search-forward "\"entrypoint\":\"cli\"" nil t)))
                   (sort (directory-files folder t "\\.jsonl\\'")
                         :key (lambda (file) (file-attribute-modification-time
                                              (file-attributes file)))
                         :reverse t)))))

(defun gco-claude-code-project ()
  "Show the current project's Claude session, starting one if none runs.
A new session continues the project's latest conversation, if it has
one.  From \\[project-switch-project] this acts on the chosen project."
  (interactive)
  (require 'claude-code)
  (let ((directory (claude-code--directory)))
    (if-let* ((buffer (car (claude-code--find-claude-buffers-for-directory directory))))
        (pop-to-buffer buffer)
      (if (gco-claude-code--has-conversation-p directory)
          (claude-code-continue '(4))
        (claude-code '(4))))))

(with-eval-after-load 'project
  (keymap-set project-prefix-map "C" #'gco-claude-code-project)
  (add-to-list 'project-switch-commands '(gco-claude-code-project "Claude") t))

;;; Ghostel backend fixes.  claude-code.el's ghostel support predates
;;; ghostel's native PTY and input-mode rework (0.22+), so replace the
;;; methods that broke.  Remove once upstream catches up; see
;;; stevemolitor/claude-code.el issues #139 and #147.

(defvar ghostel--input-mode)
(defvar ghostel-query-before-killing)
(defvar ghostel-semi-char-mode-map)
(declare-function ghostel-copy-mode "ghostel")
(declare-function ghostel-readonly-exit "ghostel")
(declare-function claude-code--buffer-p "claude-code")
(declare-function claude-code--term-setup-keymap "claude-code")

(with-eval-after-load 'claude-code
  ;; The native PTY's process object is an event pipe that
  ;; `kill-process' rejects; killing the buffer stops the child.
  ;; claude-code has already asked for confirmation.
  (cl-defmethod claude-code--term-kill-process ((_backend (eql ghostel)) buffer)
    (when (buffer-live-p buffer)
      (let ((ghostel-query-before-killing nil))
        (kill-buffer buffer))))

  (cl-defmethod claude-code--term-in-read-only-p ((_backend (eql ghostel)))
    (memq ghostel--input-mode '(copy emacs)))

  (cl-defmethod claude-code--term-read-only-mode ((_backend (eql ghostel)))
    (unless (claude-code--term-in-read-only-p 'ghostel)
      (ghostel-copy-mode)))

  (cl-defmethod claude-code--term-interactive-mode ((_backend (eql ghostel)))
    (ghostel-readonly-exit))

  (advice-add 'ghostel-semi-char-mode :after #'gco-claude-ghostel-restore-keys))

(defun gco-claude-ghostel-restore-keys (&rest _)
  "Reinstall claude-code's keys after ghostel returns to semi-char mode.
Every ghostel input-mode switch replaces the local map, which drops
claude-code's C-g and RET bindings; clicking or scrolling enters copy
mode, so this happens in normal use."
  (when (and (eq (current-local-map) ghostel-semi-char-mode-map)
             (claude-code--buffer-p (current-buffer)))
    (claude-code--term-setup-keymap 'ghostel)))

(provide 'init-claude-code)
;;; init-claude-code.el ends here
