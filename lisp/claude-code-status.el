;;; claude-code-status.el --- See which Claude Code sessions are waiting for you  -*- lexical-binding: t -*-

;; Author: Gary Oberbrunner <garyo@oberbrunner.com>
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (claude-code "0.4"))
;; Keywords: tools, convenience

;;; Commentary:

;; Tracks the state of each claude-code.el session -- working, waiting
;; for you, or needing input such as a permission decision -- and shows
;; it in a session list and in the mode line.  It works with any
;; claude-code.el terminal backend.
;;
;; Claude Code reports state changes through its hooks, which call back
;; into Emacs with emacsclient.  To set up:
;;
;;   1. Run an Emacs server (`server-start').
;;   2. Enable `claude-code-status-mode'.  Sessions started from then on
;;      carry what their hooks need to reach this Emacs.
;;   3. Run M-x claude-code-status-install-hooks once, to add the hooks
;;      to ~/.claude/settings.json.  They do nothing in Claude sessions
;;      that Emacs didn't start.
;;
;; M-x claude-code-status-list shows every session with its state; RET
;; switches to one.  The mode line summarizes sessions that are waiting
;; or need input; click it to open the list.

;;; Code:

(require 'json)
(require 'seq)
(require 'server)
(require 'tabulated-list)

(defvar claude-code-process-environment-functions)

(defgroup claude-code-status nil
  "Show which Claude Code sessions are waiting for you."
  :group 'tools
  :prefix "claude-code-status-")

(defface claude-code-status-input '((t :inherit error))
  "Face for sessions that need input, such as a permission decision.")

(defface claude-code-status-waiting '((t :inherit warning))
  "Face for sessions that have finished and are waiting for a prompt.")

(defface claude-code-status-working '((t :inherit shadow))
  "Face for sessions that are working.")

(defcustom claude-code-status-mode-line t
  "Whether `claude-code-status-mode' summarizes sessions in the mode line."
  :type 'boolean)

(defcustom claude-code-status-display-action
  '(display-buffer-in-side-window (side . right) (window-width . 40))
  "Display action `claude-code-status-list' uses for the session list."
  :type 'sexp)

(defcustom claude-code-status-settings-file "~/.claude/settings.json"
  "Claude Code settings file that `claude-code-status-install-hooks' edits."
  :type 'file)

(defconst claude-code-status--states
  '((input   "needs input" claude-code-status-input)
    (waiting "waiting"     claude-code-status-waiting)
    (working "working"     claude-code-status-working))
  "Session states, most urgent first: (STATE LABEL FACE).")

(defconst claude-code-status--hooks
  '(("SessionStart" nil waiting)
    ("UserPromptSubmit" nil working)
    ("PostToolUse" nil working)
    ("PermissionRequest" nil input)
    ("Notification"
     "permission_prompt|elicitation_dialog|elicitation_url_dialog|agent_needs_input"
     input)
    ("Stop" nil waiting))
  "Claude Code hook events that change a session's state: (EVENT MATCHER STATE).")

(defconst claude-code-status--list-buffer "*Claude Sessions*")

(defvar claude-code-status--sessions (make-hash-table)
  "Map from session id to the name of its claude-code.el buffer.")

(defvar claude-code-status--last-id 0)

(defvar claude-code-status--mode-line-string nil
  "Mode-line summary, recomputed whenever a session's state changes.")
(put 'claude-code-status--mode-line-string 'risky-local-variable t)

(defvar-local claude-code-status--state nil
  "State of this buffer's Claude session, a key of `claude-code-status--states'.")

(defvar-local claude-code-status--since nil
  "Time this buffer's session entered its current state.")

;;; Session state

(defun claude-code-status--environment (buffer-name _directory)
  "Return environment entries that let a session's hooks reach this Emacs.
BUFFER-NAME is the session's claude-code.el buffer.  For
`claude-code-process-environment-functions'."
  (let ((id (setq claude-code-status--last-id (1+ claude-code-status--last-id))))
    (puthash id buffer-name claude-code-status--sessions)
    (list (format "CLAUDE_CODE_STATUS_ID=%d" id)
          (if server-use-tcp
              (format "EMACS_SERVER_FILE=%s" (expand-file-name server-name server-auth-dir))
            (format "EMACS_SOCKET_NAME=%s" (expand-file-name server-name server-socket-dir))))))

(defun claude-code-status-update (id state)
  "Record that Claude session ID has entered STATE.
Claude Code hooks call this through emacsclient."
  (when-let* ((name (gethash id claude-code-status--sessions))
              (buffer (get-buffer name))
              ((assq state claude-code-status--states)))
    (with-current-buffer buffer
      (setq claude-code-status--state state
            claude-code-status--since (current-time))
      (add-hook 'kill-buffer-hook #'claude-code-status--refresh-soon nil t))
    (claude-code-status--refresh))
  nil)

(defun claude-code-status--session-buffers ()
  "Return claude-code.el session buffers, most urgent state first."
  (let ((rank (lambda (buffer)
                (or (seq-position (mapcar #'car claude-code-status--states)
                                  (buffer-local-value 'claude-code-status--state buffer))
                    (length claude-code-status--states)))))
    (sort (seq-filter (lambda (buffer) (string-prefix-p "*claude:" (buffer-name buffer)))
                      (buffer-list))
          :key rank)))

(defun claude-code-status--describe (buffer)
  "Return (LABEL . DIRECTORY) for the session in BUFFER.
claude-code.el names session buffers *claude:DIRECTORY:INSTANCE*."
  (let ((name (buffer-name buffer)))
    (if (string-match "\\`\\*claude:\\(.*\\):\\([^:]*\\)\\*\\'" name)
        (let ((directory (match-string 1 name))
              (instance (match-string 2 name)))
          (cons (concat (file-name-nondirectory (directory-file-name directory))
                        (unless (equal instance "default") (concat ":" instance)))
                directory))
      (cons name nil))))

(defun claude-code-status--refresh ()
  "Update the mode-line summary and the session list after a change."
  (setq claude-code-status--mode-line-string (claude-code-status--summary))
  (force-mode-line-update t)
  (when-let* ((buffer (get-buffer claude-code-status--list-buffer)))
    (with-current-buffer buffer
      (revert-buffer))))

(defun claude-code-status--refresh-soon ()
  "Refresh once the current command finishes, for `kill-buffer-hook'."
  (run-at-time 0 nil #'claude-code-status--refresh))

;;; Mode line

(defvar-keymap claude-code-status--mode-line-map
  "<mode-line> <mouse-1>" #'claude-code-status-list)

(defun claude-code-status--summary ()
  "Return the mode-line summary of sessions needing attention, or nil."
  (when-let* ((states (mapcar (lambda (buffer)
                                (buffer-local-value 'claude-code-status--state buffer))
                              (claude-code-status--session-buffers)))
              (parts
               (delq nil
                     (mapcar
                      (pcase-lambda (`(,state ,label ,face))
                        (let ((count (seq-count (lambda (s) (eq s state)) states)))
                          (unless (or (zerop count) (eq state 'working))
                            (propertize (format "%d %s" count label) 'face face))))
                      claude-code-status--states))))
    (propertize (concat " Claude: " (string-join parts ", ") " ")
                'help-echo "mouse-1: list Claude sessions"
                'mouse-face 'mode-line-highlight
                'local-map claude-code-status--mode-line-map)))

;;; Session list

(defvar-keymap claude-code-status-list-mode-map
  "RET" #'claude-code-status-visit)

(define-derived-mode claude-code-status-list-mode tabulated-list-mode "Claude Sessions"
  "Major mode listing claude-code.el sessions and their states."
  (setq tabulated-list-format [("State" 12 nil) ("Since" 6 nil) ("Session" 0 nil)]
        tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'claude-code-status--list-entries nil t)
  (tabulated-list-init-header))

(defun claude-code-status--list-entries ()
  "Fill `tabulated-list-entries' with the current sessions."
  (setq tabulated-list-entries
        (mapcar
         (lambda (buffer)
           (pcase-let* ((state (buffer-local-value 'claude-code-status--state buffer))
                        (`(,label ,face) (alist-get state claude-code-status--states))
                        (since (buffer-local-value 'claude-code-status--since buffer))
                        (`(,session . ,directory) (claude-code-status--describe buffer)))
             (list buffer
                   (vector (if state (propertize label 'face face) "")
                           (if since (format-time-string "%H:%M" since) "")
                           (propertize session 'help-echo directory)))))
         (claude-code-status--session-buffers))))

;;;###autoload
(defun claude-code-status-list ()
  "Show claude-code.el sessions and their states."
  (interactive)
  (let ((buffer (get-buffer-create claude-code-status--list-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'claude-code-status-list-mode)
        (claude-code-status-list-mode))
      (revert-buffer))
    (select-window (display-buffer buffer claude-code-status-display-action))))

(defun claude-code-status-visit ()
  "Switch to the session on this line."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id))
              ((buffer-live-p buffer)))
    (pop-to-buffer buffer)))

;;; Setup

;;;###autoload
(define-minor-mode claude-code-status-mode
  "Track the state of claude-code.el sessions.
Needs an Emacs server and the hooks from
`claude-code-status-install-hooks'; affects sessions started after
the mode is enabled."
  :global t
  (if claude-code-status-mode
      (progn
        (add-hook 'claude-code-process-environment-functions
                  #'claude-code-status--environment)
        (when claude-code-status-mode-line
          (add-to-list 'global-mode-string 'claude-code-status--mode-line-string t))
        (unless (server-running-p)
          (display-warning 'claude-code-status
                           "No Emacs server is running, so Claude hooks can't report session state; see `server-start'")))
    (remove-hook 'claude-code-process-environment-functions
                 #'claude-code-status--environment)
    (setq global-mode-string
          (delq 'claude-code-status--mode-line-string global-mode-string))))

(defun claude-code-status--hook-command (state)
  "Return the shell command a Claude Code hook runs to report STATE."
  ;; The id check keeps the hook inert in sessions Emacs didn't start, and
  ;; ensures only a number is ever interpolated into the Lisp.  `-a false'
  ;; stops emacsclient from starting an Emacs when no server answers.
  (format "case \"${CLAUDE_CODE_STATUS_ID:-x}\" in *[!0-9]*) ;; *) emacsclient -n -a false --eval \"(claude-code-status-update $CLAUDE_CODE_STATUS_ID '%s)\" >/dev/null 2>&1 ;; esac; exit 0"
          state))

(defun claude-code-status--own-hook-p (handler)
  "Return non-nil if hook HANDLER, a parsed JSON object, is one of ours."
  (string-match-p "claude-code-status-update" (or (alist-get 'command handler) "")))

;;;###autoload
(defun claude-code-status-install-hooks ()
  "Add the hooks that report session state to Claude Code's settings.
Edits `claude-code-status-settings-file', replacing hooks a previous
call added and leaving all others alone."
  (interactive)
  (let* ((file (expand-file-name claude-code-status-settings-file))
         (settings (if (file-exists-p file)
                       (with-temp-buffer
                         (insert-file-contents file)
                         ;; Arrays as vectors, so empty ones survive the round trip.
                         (json-parse-buffer :object-type 'alist :array-type 'array
                                            :null-object :null :false-object :false))
                     nil))
         (hooks (alist-get 'hooks settings)))
    ;; Drop handlers from earlier installs, then any groups left empty.
    (setq hooks
          (mapcar (pcase-lambda (`(,event . ,groups))
                    (cons event
                          (vconcat
                           (seq-filter
                            (lambda (group) (length> (alist-get 'hooks group) 0))
                            (seq-map (lambda (group)
                                       (setf (alist-get 'hooks group)
                                             (vconcat (seq-remove #'claude-code-status--own-hook-p
                                                                  (alist-get 'hooks group))))
                                       group)
                                     groups)))))
                  hooks))
    (pcase-dolist (`(,event ,matcher ,state) claude-code-status--hooks)
      (let ((group `(,@(and matcher `((matcher . ,matcher)))
                     (hooks . [((type . "command")
                                (command . ,(claude-code-status--hook-command state))
                                (timeout . 2))]))))
        (setf (alist-get (intern event) hooks)
              (vconcat (alist-get (intern event) hooks) (list group)))))
    (setf (alist-get 'hooks settings)
          (seq-filter (lambda (entry) (length> (cdr entry) 0)) hooks))
    (when (yes-or-no-p (format "Add Claude session-state hooks to %s? " file))
      (with-temp-file file
        (let ((json-encoding-pretty-print t)
              (json-encoding-default-indentation "  ")
              (json-false :false)
              (json-null :null))
          (insert (json-encode settings) "\n")))
      (message "Installed hooks for %d events in %s"
               (length claude-code-status--hooks) file))))

(provide 'claude-code-status)
;;; claude-code-status.el ends here
