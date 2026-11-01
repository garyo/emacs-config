;;; claude-code-status.el --- See which Claude Code sessions are waiting for you  -*- lexical-binding: t -*-

;; Author: Gary Oberbrunner <garyo@oberbrunner.com>
;; Version: 0.2
;; Package-Requires: ((emacs "30.1") (claude-code "0.4"))
;; Keywords: tools, convenience

;;; Commentary:

;; Tracks the state of each claude-code.el session -- working, waiting
;; for you, or needing input such as a permission decision -- and shows
;; it in a session list and in the mode line.  Each session in the list
;; also shows a line or two of context: what Claude is asking, the tool
;; it last ran, or the end of its last reply.  It works with any
;; claude-code.el terminal backend.
;;
;; Claude Code reports state changes through its hooks, which save the
;; hook's JSON input to a per-session temporary file and call back into
;; Emacs with emacsclient.  To set up:
;;
;;   1. Run an Emacs server (`server-start').
;;   2. Enable `claude-code-status-mode'.  Sessions started from then on
;;      carry what their hooks need to reach this Emacs.
;;   3. Run M-x claude-code-status-install-hooks once, to add the hooks
;;      to ~/.claude/settings.json.  They do nothing in Claude sessions
;;      that Emacs didn't start.
;;
;; M-x claude-code-status-list shows the sessions, most urgent first:
;; RET visits one, k kills it, r renames it, n and p move between them.
;; The mode line summarizes sessions that need you; click it to open the
;; list.

;;; Code:

(require 'json)
(require 'seq)
(require 'server)

(defvar claude-code-process-environment-functions)
(declare-function claude-code--kill-buffer "claude-code")

(defgroup claude-code-status nil
  "Show which Claude Code sessions are waiting for you."
  :group 'tools
  :prefix "claude-code-status-")

(defface claude-code-status-input '((t :inherit error))
  "Face for sessions that need input, such as a permission decision.")

(defface claude-code-status-waiting '((t :inherit warning))
  "Face for sessions that have finished and are waiting for a prompt.")

(defface claude-code-status-working '((t :inherit success))
  "Face for sessions that are working.")

(defface claude-code-status-detail '((t :inherit shadow))
  "Face for the context line under each session in the list.")

(defcustom claude-code-status-icons
  '((input . "▶") (waiting . "○") (working . "●") (untracked . "?"))
  "Icon for each session state in the list and the mode line.
`untracked' is for sessions started before `claude-code-status-mode'."
  :type '(alist :key-type symbol :value-type string))

(defcustom claude-code-status-detail-lines 2
  "Maximum number of context lines shown under each session in the list."
  :type 'natnum)

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
  "Map from session id to (BUFFER-NAME . PAYLOAD-FILE).")

(defvar claude-code-status--last-id 0)

(defvar claude-code-status--mode-line-string nil
  "Mode-line summary, recomputed whenever a session's state changes.")
(put 'claude-code-status--mode-line-string 'risky-local-variable t)

(defvar-local claude-code-status--state nil
  "State of this buffer's Claude session, a key of `claude-code-status--states'.")

(defvar-local claude-code-status--since nil
  "Time this buffer's session entered its current state.")

(defvar-local claude-code-status--detail nil
  "Context for this buffer's session state, such as Claude's question.")

(defvar-local claude-code-status--detail-from-end nil
  "Non-nil if the end of `claude-code-status--detail' matters most.")

;;; Session state

(defun claude-code-status--environment (buffer-name _directory)
  "Return environment entries that let a session's hooks reach this Emacs.
BUFFER-NAME is the session's claude-code.el buffer.  For
`claude-code-process-environment-functions'."
  (let ((id (setq claude-code-status--last-id (1+ claude-code-status--last-id)))
        ;; Created owner-only and exclusively, so it's safe in a shared /tmp.
        (file (make-temp-file "claude-code-status-" nil ".json")))
    (puthash id (cons buffer-name file) claude-code-status--sessions)
    (list (format "CLAUDE_CODE_STATUS_ID=%d" id)
          (format "CLAUDE_CODE_STATUS_FILE=%s" file)
          (if server-use-tcp
              (format "EMACS_SERVER_FILE=%s" (expand-file-name server-name server-auth-dir))
            (format "EMACS_SOCKET_NAME=%s" (expand-file-name server-name server-socket-dir))))))

(defun claude-code-status--read-payload (file)
  "Return the hook input saved in FILE, as an alist, or nil."
  (when (and file (file-readable-p file))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file)
        (json-parse-buffer :object-type 'alist :null-object nil :false-object nil)))))

(defun claude-code-status--tool-summary (payload)
  "Describe the tool call in hook PAYLOAD, such as \"Bash: make test\"."
  (let* ((input (alist-get 'tool_input payload))
         (cwd (alist-get 'cwd payload))
         (path (alist-get 'file_path input))
         (target (or (alist-get 'command input)
                     (and path (if cwd (file-relative-name path cwd) path))
                     (alist-get 'pattern input)
                     (alist-get 'url input)
                     (alist-get 'query input)
                     (alist-get 'description input))))
    (concat (alist-get 'tool_name payload)
            (and (stringp target) (concat ": " (car (split-string target "\n")))))))

(defun claude-code-status--last-reply (transcript)
  "Return the text of the last assistant message in TRANSCRIPT, or nil.
TRANSCRIPT is a Claude Code session file with one JSON record per line."
  (when (and transcript (file-readable-p transcript))
    (with-temp-buffer
      (let ((size (file-attribute-size (file-attributes transcript))))
        (insert-file-contents transcript nil (max 0 (- size 500000)) size))
      (goto-char (point-max))
      (let (text)
        (while (and (not text) (zerop (forward-line -1)))
          (when-let* ((record (ignore-errors
                                (json-parse-string
                                 (buffer-substring (point) (line-end-position))
                                 :object-type 'alist :array-type 'list
                                 :null-object nil :false-object nil)))
                      ((equal (alist-get 'type record) "assistant"))
                      (parts (seq-filter
                              (lambda (part) (equal (alist-get 'type part) "text"))
                              (alist-get 'content (alist-get 'message record)))))
            (setq text (mapconcat (lambda (part) (alist-get 'text part)) parts "\n"))))
        text))))

(defun claude-code-status--last-paragraph (text)
  "Return the last non-blank paragraph of TEXT."
  (car (last (seq-remove #'string-blank-p (split-string text "\n[ \t]*\n")))))

(defun claude-code-status--describe-event (payload)
  "Return (DETAIL . FROM-END) describing hook PAYLOAD for the session list.
DETAIL is nil when the event says nothing new."
  (let ((event (alist-get 'hook_event_name payload)))
    (pcase event
      ("UserPromptSubmit" (cons (alist-get 'prompt payload) nil))
      ("PostToolUse" (cons (claude-code-status--tool-summary payload) nil))
      ("PermissionRequest"
       (cons (if (equal (alist-get 'tool_name payload) "AskUserQuestion")
                 (alist-get 'question (seq-first (alist-get 'questions
                                                            (alist-get 'tool_input payload))))
               (concat "Allow " (claude-code-status--tool-summary payload) "?"))
             nil))
      ;; A permission notification just repeats PermissionRequest, less precisely.
      ("Notification"
       (unless (equal (alist-get 'notification_type payload) "permission_prompt")
         (cons (alist-get 'message payload) nil)))
      ("Stop"
       (when-let* ((reply (or (alist-get 'last_assistant_message payload)
                              (claude-code-status--last-reply
                               (alist-get 'transcript_path payload)))))
         (cons (claude-code-status--last-paragraph reply) t)))
      ("SessionStart" (cons nil nil)))))

(defun claude-code-status-update (id state)
  "Record that Claude session ID has entered STATE.
Claude Code hooks call this through emacsclient, after saving their
input to the session's payload file."
  (when-let* ((entry (gethash id claude-code-status--sessions))
              (buffer (get-buffer (car entry)))
              ((assq state claude-code-status--states)))
    (let ((described (claude-code-status--describe-event
                      (claude-code-status--read-payload (cdr entry)))))
      (with-current-buffer buffer
        (setq claude-code-status--state state
              claude-code-status--since (current-time))
        (when described
          (setq claude-code-status--detail (car described)
                claude-code-status--detail-from-end (cdr described)))
        (add-hook 'kill-buffer-hook #'claude-code-status--forget-buffer nil t)))
    (claude-code-status--refresh))
  nil)

(defun claude-code-status--forget-buffer ()
  "Drop the current buffer's session, for `kill-buffer-hook'."
  (let ((name (buffer-name)))
    (maphash (lambda (id entry)
               (when (equal (car entry) name)
                 (ignore-errors (delete-file (cdr entry)))
                 (remhash id claude-code-status--sessions)))
             claude-code-status--sessions))
  (run-at-time 0 nil #'claude-code-status--refresh))

(defun claude-code-status--delete-payload-files ()
  "Delete every session's payload file, for `kill-emacs-hook'."
  (maphash (lambda (_id entry) (ignore-errors (delete-file (cdr entry))))
           claude-code-status--sessions))

(defun claude-code-status--tracked-p (buffer)
  "Return non-nil if BUFFER's session was started with tracking on."
  (let ((name (buffer-name buffer)) found)
    (maphash (lambda (_id entry) (when (equal (car entry) name) (setq found t)))
             claude-code-status--sessions)
    found))

(defun claude-code-status--display-state (buffer)
  "Return BUFFER's state for display: a key of the states, `untracked' or nil."
  (or (buffer-local-value 'claude-code-status--state buffer)
      (and (not (claude-code-status--tracked-p buffer)) 'untracked)))

(defun claude-code-status--session-buffers ()
  "Return claude-code.el session buffers, most urgent state first."
  (let ((rank (lambda (buffer)
                (or (seq-position (mapcar #'car claude-code-status--states)
                                  (buffer-local-value 'claude-code-status--state buffer))
                    (length claude-code-status--states)))))
    (sort (seq-filter (lambda (buffer) (string-prefix-p "*claude:" (buffer-name buffer)))
                      (buffer-list))
          :key rank)))

(defun claude-code-status--parse-name (buffer)
  "Return (DIRECTORY . INSTANCE) from BUFFER's name, or nil.
claude-code.el names session buffers *claude:DIRECTORY:INSTANCE*."
  (let ((name (buffer-name buffer)))
    (when (string-match "\\`\\*claude:\\(.*\\):\\([^:]*\\)\\*\\'" name)
      (cons (match-string 1 name) (match-string 2 name)))))

(defun claude-code-status--label (buffer)
  "Return a short name for the session in BUFFER: its project, and instance."
  (pcase (claude-code-status--parse-name buffer)
    (`(,directory . ,instance)
     (concat (file-name-nondirectory (directory-file-name directory))
             (unless (equal instance "default") (concat ":" instance))))
    (_ (buffer-name buffer))))

(defun claude-code-status--icon (state)
  "Return STATE's icon, in its face, with its label as help."
  (let ((spec (alist-get state claude-code-status--states)))
    (propertize (alist-get state claude-code-status-icons " ")
                'face (or (nth 1 spec) 'shadow)
                'help-echo (cond (spec (car spec))
                                 ((eq state 'untracked)
                                  "Started before tracking; restart the session to track it")
                                 (t "starting")))))

(defun claude-code-status--refresh ()
  "Update the mode-line summary and the session list after a change."
  (setq claude-code-status--mode-line-string (claude-code-status--summary))
  (force-mode-line-update t)
  (when-let* ((buffer (get-buffer claude-code-status--list-buffer)))
    (with-current-buffer buffer
      (claude-code-status--render))))

;;; Mode line

(defvar-keymap claude-code-status--mode-line-map
  "<mode-line> <mouse-1>" #'claude-code-status-list)

(defun claude-code-status--summary ()
  "Return the mode-line summary of sessions that need you, or nil."
  (let* ((states (mapcar (lambda (buffer)
                           (buffer-local-value 'claude-code-status--state buffer))
                         (claude-code-status--session-buffers)))
         (counts (delq nil
                       (mapcar (pcase-lambda (`(,state ,label . ,_))
                                 (let ((count (seq-count (lambda (s) (eq s state)) states)))
                                   (unless (or (zerop count) (eq state 'working))
                                     (list state count label))))
                               claude-code-status--states))))
    (when counts
      (propertize
       (concat " Claude "
               (mapconcat (pcase-lambda (`(,state ,count . ,_))
                            (concat (claude-code-status--icon state) (number-to-string count)))
                          counts " ")
               " ")
       'help-echo (concat (mapconcat (pcase-lambda (`(,_ ,count ,label))
                                       (format "%d %s" count label))
                                     counts ", ")
                          "\nmouse-1: list Claude sessions")
       'mouse-face 'mode-line-highlight
       'local-map claude-code-status--mode-line-map))))

;;; Session list

(defvar-keymap claude-code-status-list-mode-map
  "RET" #'claude-code-status-visit
  "<mouse-2>" #'claude-code-status-visit
  "k" #'claude-code-status-kill
  "r" #'claude-code-status-rename
  "n" #'claude-code-status-next
  "p" #'claude-code-status-previous)

(define-derived-mode claude-code-status-list-mode special-mode "Claude Sessions"
  "Major mode listing claude-code.el sessions and their states.
\\{claude-code-status-list-mode-map}"
  (setq truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (claude-code-status--render)))
  (add-hook 'window-size-change-functions #'claude-code-status--resized nil t))

(defun claude-code-status--resized (window)
  "Re-render the session list in WINDOW to fit its new width."
  (with-current-buffer (window-buffer window)
    (claude-code-status--render)))

(defun claude-code-status--fit (text width lines from-end)
  "Fill TEXT to WIDTH and return at most LINES lines of it.
When it's longer, keep the end if FROM-END is non-nil, else the start."
  (let* ((text (replace-regexp-in-string "\\*\\*\\|`" "" (string-trim text)))
         (filled (with-temp-buffer
                   (insert (replace-regexp-in-string "[ \t\n]+" " " text))
                   (let ((fill-column width)) (fill-region (point-min) (point-max)))
                   (split-string (buffer-string) "\n" t)))
         (excess (- (length filled) lines)))
    (cond ((<= excess 0) filled)
          (from-end (let ((kept (last filled lines)))
                      (cons (concat "…" (car kept)) (cdr kept))))
          (t (let ((kept (take lines filled)))
               (append (butlast kept) (list (concat (car (last kept)) "…"))))))))

(defun claude-code-status--insert-entry (buffer width)
  "Insert BUFFER's session into the list, filling context lines to WIDTH."
  (let* ((start (point))
         (state (claude-code-status--display-state buffer))
         (since (buffer-local-value 'claude-code-status--since buffer))
         (time (if since (format-time-string "%H:%M" since) ""))
         (detail (buffer-local-value 'claude-code-status--detail buffer)))
    (insert (claude-code-status--icon state) " "
            (propertize (claude-code-status--label buffer)
                        'help-echo (car (claude-code-status--parse-name buffer))
                        'mouse-face 'highlight
                        'follow-link t)
            (propertize " " 'display `(space :align-to (- right ,(1+ (length time)))))
            time "\n")
    (when (and detail (> claude-code-status-detail-lines 0))
      (dolist (line (claude-code-status--fit
                     detail (- width 3) claude-code-status-detail-lines
                     (buffer-local-value 'claude-code-status--detail-from-end buffer)))
        (insert "  " (propertize line 'face 'claude-code-status-detail) "\n")))
    (put-text-property start (point) 'claude-code-status-session buffer)))

(defun claude-code-status--render ()
  "Redraw the session list in the current buffer, keeping point's session."
  (let* ((inhibit-read-only t)
         (window (get-buffer-window (current-buffer) t))
         (width (if window (window-body-width window) 40))
         (here (get-text-property (point) 'claude-code-status-session))
         (sessions (claude-code-status--session-buffers)))
    (erase-buffer)
    (if sessions
        (dolist (buffer sessions)
          (claude-code-status--insert-entry buffer width))
      (insert (propertize "No Claude sessions" 'face 'shadow)))
    (goto-char (or (and here (text-property-any (point-min) (point-max)
                                                'claude-code-status-session here))
                   (point-min)))
    (when window (set-window-point window (point)))))

(defun claude-code-status--session-at-point (&optional event)
  "Return the session buffer at point, or at mouse EVENT's position."
  (let ((pos (if event (posn-point (event-start event)) (point))))
    (get-text-property pos 'claude-code-status-session)))

;;;###autoload
(defun claude-code-status-list ()
  "Show claude-code.el sessions and their states."
  (interactive)
  (let ((buffer (get-buffer-create claude-code-status--list-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'claude-code-status-list-mode)
        (claude-code-status-list-mode)))
    (select-window (display-buffer buffer claude-code-status-display-action))
    (claude-code-status--render)))

(defun claude-code-status-visit (&optional event)
  "Switch to the session at point, or at mouse EVENT."
  (interactive (list last-nonmenu-event))
  (when-let* ((buffer (claude-code-status--session-at-point
                       (and (mouse-event-p event) event)))
              ((buffer-live-p buffer)))
    (pop-to-buffer buffer)))

(defun claude-code-status-kill ()
  "Kill the session at point, after confirming."
  (interactive)
  (when-let* ((buffer (claude-code-status--session-at-point))
              ((y-or-n-p (format "Kill Claude session %s? "
                                 (claude-code-status--label buffer)))))
    (if (fboundp 'claude-code--kill-buffer)
        (claude-code--kill-buffer buffer)
      (kill-buffer buffer))
    (claude-code-status--refresh)))

(defun claude-code-status-rename (name)
  "Rename the session at point to instance NAME.
claude-code.el shows the instance name after the project, as in
`project:NAME', and offers it when you choose among sessions."
  (interactive
   (let ((buffer (or (claude-code-status--session-at-point)
                     (user-error "No session here"))))
     (list (read-string "Rename session to: "
                        (cdr (claude-code-status--parse-name buffer))))))
  (let* ((buffer (claude-code-status--session-at-point))
         (old (buffer-name buffer))
         (new (format "*claude:%s:%s*" (car (claude-code-status--parse-name buffer)) name)))
    (when (or (string-empty-p name) (string-match-p "[:*]" name))
      (user-error "A session name can't be empty or contain `:' or `*'"))
    (when (get-buffer new)
      (user-error "There is already a session named %s" name))
    (with-current-buffer buffer
      (rename-buffer new))
    (maphash (lambda (_id entry) (when (equal (car entry) old) (setcar entry new)))
             claude-code-status--sessions)
    (claude-code-status--refresh)))

(defun claude-code-status--entry-start (pos)
  "Return where the session entry containing POS begins."
  (if (or (= pos (point-min))
          (not (eq (get-text-property (1- pos) 'claude-code-status-session)
                   (get-text-property pos 'claude-code-status-session))))
      pos
    (or (previous-single-property-change pos 'claude-code-status-session) (point-min))))

(defun claude-code-status-next (n)
  "Move to the Nth next session in the list."
  (interactive "p")
  (dotimes (_ (abs n))
    (if (> n 0)
        (let ((next (next-single-property-change (point) 'claude-code-status-session)))
          (when (and next (< next (point-max)))
            (goto-char next)))
      (let ((start (claude-code-status--entry-start (point))))
        (goto-char (if (> start (point-min))
                       (claude-code-status--entry-start (1- start))
                     start))))))

(defun claude-code-status-previous (n)
  "Move to the Nth previous session in the list."
  (interactive "p")
  (claude-code-status-next (- n)))

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
        (add-hook 'kill-emacs-hook #'claude-code-status--delete-payload-files)
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
  ;; The id check keeps the hook inert in sessions Emacs didn't start,
  ;; and ensures only a number is ever interpolated into the Lisp; the
  ;; hook's JSON input goes through a file Emacs created.  `-a false'
  ;; stops emacsclient from starting an Emacs when no server answers.
  (format "case \"${CLAUDE_CODE_STATUS_ID:-x}\" in *[!0-9]*) ;; *) [ -n \"${CLAUDE_CODE_STATUS_FILE:-}\" ] && cat > \"$CLAUDE_CODE_STATUS_FILE\"; emacsclient -n -a false --eval \"(claude-code-status-update $CLAUDE_CODE_STATUS_ID '%s)\" >/dev/null 2>&1 ;; esac; exit 0"
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
