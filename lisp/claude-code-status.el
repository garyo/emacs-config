;;; claude-code-status.el --- See which Claude Code sessions are waiting for you  -*- lexical-binding: t -*-

;; Author: Gary Oberbrunner <garyo@oberbrunner.com>
;; Version: 0.4
;; Package-Requires: ((emacs "30.1") (claude-code "0.4"))
;; Keywords: tools, convenience

;;; Commentary:

;; Tracks the state of each claude-code.el session -- working, finished
;; with a reply you haven't seen, waiting for you, or needing input: a
;; permission decision, a question dialog, or a finished reply that ends
;; with a question -- and shows it in a session list and in the mode
;; line.  Each session in the list also shows a line or two of context:
;; what Claude is asking, the tool it last ran, or the end of its last
;; reply.  It works with any claude-code.el terminal backend.
;;
;; Claude Code reports state changes through its hooks, which save the
;; hook's JSON input to a per-session temporary file and call back into
;; Emacs with emacsclient.  In ghostel terminals, Claude Code also
;; reports its status directly (OSC 7501), which catches what hooks
;; miss, such as compacting or waiting out a rate limit; there, that
;; report decides the state and hooks supply the context.  To set up:
;;
;;   1. Run an Emacs server (`server-start').
;;   2. Enable `claude-code-status-mode'.  Sessions started from then on
;;      carry what their hooks need to reach this Emacs.
;;   3. Run M-x claude-code-status-install-hooks once, to add the hooks
;;      to ~/.claude/settings.json.  They do nothing in Claude sessions
;;      that Emacs didn't start.
;;
;; M-x claude-code-status-list shows the sessions, most urgent first:
;; RET visits one, k kills it, r renames it, n and p move between them,
;; and s sorts them by status, time or name instead.
;; The mode line summarizes sessions that need you; click it to open the
;; list.
;;
;; With `claude-code-status-remember-sessions' on, sessions still
;; running when Emacs exits stay in the list, greyed out, after a
;; restart; RET resumes one's conversation and k forgets it.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'server)

(defvar claude-code-process-environment-functions)
(defvar claude-code-status-mode)
(defvar ghostel-program-status)
(defvar ghostel-program-status-functions)
(declare-function claude-code--kill-buffer "claude-code")
(declare-function claude-code--start "claude-code")

(defgroup claude-code-status nil
  "Show which Claude Code sessions are waiting for you."
  :group 'tools
  :prefix "claude-code-status-")

(defface claude-code-status-input '((t :inherit error))
  "Face for sessions that need input, such as a permission decision or an answer.")

(defface claude-code-status-done '((t :inherit (bold warning)))
  "Face for sessions that have finished a reply you haven't seen yet.")

(defface claude-code-status-waiting '((t :inherit warning))
  "Face for sessions that have finished and are waiting for a prompt.")

(defface claude-code-status-working '((t :inherit success))
  "Face for sessions that are working.")

(defface claude-code-status-detail '((t :inherit shadow))
  "Face for the context line under each session in the list.")

(defcustom claude-code-status-icons
  '((input . "▶") (done . "✓") (waiting . "○") (working . "●")
    (untracked . "?") (ended . "◌"))
  "Icon for each session state in the list and the mode line.
`untracked' is for sessions started before `claude-code-status-mode',
and `ended' for remembered sessions from an earlier Emacs."
  :type '(alist :key-type symbol :value-type string))

(defcustom claude-code-status-detail-lines 2
  "Maximum number of context lines shown under each session in the list."
  :type 'natnum)

(defcustom claude-code-status-mode-line t
  "Whether `claude-code-status-mode' summarizes sessions in the mode line."
  :type 'boolean)

(defcustom claude-code-status-remember-sessions nil
  "Whether to remember sessions across Emacs restarts.
When non-nil, sessions still running when Emacs exits appear in the
session list after a restart, and RET resumes one's conversation.
Sessions you end yourself are forgotten."
  :type 'boolean)

(defcustom claude-code-status-sessions-file
  (locate-user-emacs-file "claude-code-status-sessions.eld")
  "File where `claude-code-status-remember-sessions' keeps sessions."
  :type 'file)

(defcustom claude-code-status-sort 'status
  "How the session list orders sessions.
`status' puts the sessions that need you first, `time' the most
recently changed, and `name' sorts by name.  Ties go to the most
recently changed.  Sessions from an earlier Emacs always follow
running ones.  \\<claude-code-status-list-mode-map>\\[claude-code-status-cycle-sort] in the list switches among these."
  :type '(choice (const status) (const time) (const name)))

(defcustom claude-code-status-display-action
  '(display-buffer-in-side-window (side . right) (window-width . 40))
  "Display action `claude-code-status-list' uses for the session list."
  :type 'sexp)

(defcustom claude-code-status-settings-file "~/.claude/settings.json"
  "Claude Code settings file that `claude-code-status-install-hooks' edits."
  :type 'file)

(defconst claude-code-status--states
  '((input   "needs input" claude-code-status-input)
    (done    "finished"    claude-code-status-done)
    (waiting "waiting"     claude-code-status-waiting)
    (working "working"     claude-code-status-working))
  "Session states, most urgent first: (STATE LABEL FACE).")

(defconst claude-code-status--program-states
  '((working . working) (blocked . input) (error . input) (done . done) (idle . waiting))
  "Session state for each state of Claude Code's own status report.")

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

(defvar claude-code-status--remembered nil
  "Sessions from an earlier Emacs, as plists; see `claude-code-status--record'.")

(defvar claude-code-status--save-timer nil)

(defvar claude-code-status--exiting nil
  "Non-nil once Emacs is exiting, so dying sessions are still remembered.")

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

(defvar-local claude-code-status--reports-status nil
  "Non-nil once this buffer's session has reported its own status.")

(defvar-local claude-code-status--session-id nil
  "Claude Code's id for this buffer's conversation, for resuming it.")

(defvar-local claude-code-status--transcript nil
  "File where Claude Code keeps this buffer's conversation.")

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

(defun claude-code-status--asks-p (text)
  "Return non-nil if TEXT ends with a question.
Ignores trailing Markdown emphasis, quotes and closing parentheses."
  (and text (string-match-p "\\?[]*_`)\"'”’ \t\n]*\\'" text)))

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
    (let* ((payload (claude-code-status--read-payload (cdr entry)))
           (described (claude-code-status--describe-event payload)))
      (with-current-buffer buffer
        (when described
          (setq claude-code-status--detail (car described)
                claude-code-status--detail-from-end (cdr described)))
        (when-let* ((session-id (alist-get 'session_id payload)))
          (setq claude-code-status--session-id session-id
                claude-code-status--transcript (alist-get 'transcript_path payload)))
        (claude-code-status--set-state
         (if claude-code-status--reports-status claude-code-status--state state))))
    (claude-code-status--refresh)
    (claude-code-status--schedule-save))
  nil)

(defun claude-code-status--set-state (state)
  "Put the current buffer's session in STATE; return non-nil if it changed."
  ;; A finished turn that ends by asking something needs you as much as
  ;; a permission prompt does.
  (when (and (memq state '(done waiting))
             claude-code-status--detail-from-end
             (claude-code-status--asks-p claude-code-status--detail))
    (setq state 'input))
  (add-hook 'kill-buffer-hook #'claude-code-status--forget-buffer nil t)
  (unless (eq state claude-code-status--state)
    (setq claude-code-status--state state
          claude-code-status--since (current-time))))

(defun claude-code-status--program-status (id old new)
  "Follow a Claude session's own status report, from OLD record to NEW.
Only the record with ID \"\", for the session itself, counts.  For
`ghostel-program-status-functions', in the terminal's buffer."
  (when (and (string-empty-p id)
             (equal (plist-get (or new old) :app) "claude-code")
             (claude-code-status--parse-name (current-buffer)))
    (setq claude-code-status--reports-status t)
    (when-let* ((state (if new
                           (alist-get (plist-get new :state)
                                      claude-code-status--program-states)
                         ;; ghostel drops a finished record once you've seen it.
                         (and (memq (plist-get old :state) '(done error)) 'waiting))))
      (let* ((msg (plist-get new :msg))
             (new-detail (and msg (not (equal msg claude-code-status--detail)))))
        (when new-detail
          (setq claude-code-status--detail msg
                claude-code-status--detail-from-end nil))
        ;; This runs on ghostel's output path; redraw only on a change.
        (when (or (claude-code-status--set-state state) new-detail)
          (claude-code-status--refresh)
          (claude-code-status--schedule-save))))))

(defun claude-code-status--follow-program-status (follow)
  "Start following Claude's status reports in ghostel if FOLLOW, else stop.
Picks up what running sessions have already reported."
  (if (not follow)
      (when (boundp 'ghostel-program-status-functions)
        (remove-hook 'ghostel-program-status-functions
                     #'claude-code-status--program-status))
    (with-eval-after-load 'ghostel-status
      (when claude-code-status-mode
        (add-hook 'ghostel-program-status-functions
                  #'claude-code-status--program-status)
        (dolist (buffer (claude-code-status--session-buffers))
          (with-current-buffer buffer
            (when-let* ((record (cdr (assoc "" (bound-and-true-p ghostel-program-status)))))
              (claude-code-status--program-status "" nil record))))))))

(defun claude-code-status--forget-buffer ()
  "Drop the current buffer's session, for `kill-buffer-hook'."
  (unless claude-code-status--exiting
    (let ((name (buffer-name)))
      (maphash (lambda (id entry)
                 (when (equal (car entry) name)
                   (ignore-errors (delete-file (cdr entry)))
                   (remhash id claude-code-status--sessions)))
               claude-code-status--sessions))
    (claude-code-status--schedule-save)
    (run-at-time 0 nil #'claude-code-status--refresh)))

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
  "Return claude-code.el session buffers."
  (seq-filter (lambda (buffer) (string-prefix-p "*claude:" (buffer-name buffer)))
              (buffer-list)))

(defun claude-code-status--get (session var key)
  "Return SESSION's value of buffer-local VAR, or of KEY if it's remembered.
SESSION is a session buffer or a remembered session's plist."
  (if (bufferp session) (buffer-local-value var session) (plist-get session key)))

(defun claude-code-status--sort-key (session)
  "Return SESSION's sort key for `claude-code-status-sort', for `value<'."
  (let* ((since (claude-code-status--get session 'claude-code-status--since :since))
         (recent (- (if since (float-time since) 0))))
    (pcase claude-code-status-sort
      ('name (list (downcase (claude-code-status--label session)) recent))
      ('time (list recent))
      (_ (list (or (and (bufferp session)
                        (seq-position (mapcar #'car claude-code-status--states)
                                      (buffer-local-value 'claude-code-status--state session)))
                   (length claude-code-status--states))
               recent)))))

(defun claude-code-status--parse-name (buffer)
  "Return (DIRECTORY . INSTANCE) from BUFFER's name, or nil.
claude-code.el names session buffers *claude:DIRECTORY:INSTANCE*."
  (let ((name (buffer-name buffer)))
    (when (string-match "\\`\\*claude:\\(.*\\):\\([^:]*\\)\\*\\'" name)
      (cons (match-string 1 name) (match-string 2 name)))))

(defun claude-code-status--format-label (directory instance)
  "Return a short session name from its DIRECTORY and INSTANCE."
  (concat (file-name-nondirectory (directory-file-name directory))
          (unless (equal instance "default") (concat ":" instance))))

(defun claude-code-status--label (session)
  "Return a short name for SESSION, a session buffer or remembered plist."
  (if (bufferp session)
      (pcase (claude-code-status--parse-name session)
        (`(,directory . ,instance) (claude-code-status--format-label directory instance))
        (_ (buffer-name session)))
    (claude-code-status--format-label (plist-get session :directory)
                                      (plist-get session :instance))))

(defun claude-code-status--icon (state)
  "Return STATE's icon, in its face, with its label as help."
  (let ((spec (alist-get state claude-code-status--states)))
    (propertize (alist-get state claude-code-status-icons " ")
                'face (or (nth 1 spec) 'shadow)
                'help-echo (cond (spec (car spec))
                                 ((eq state 'untracked)
                                  "Started before tracking; restart the session to track it")
                                 ((eq state 'ended) "Ended with Emacs; RET resumes it")
                                 (t "starting")))))

(defun claude-code-status--refresh ()
  "Update the mode-line summary and the session list after a change."
  (setq claude-code-status--mode-line-string (claude-code-status--summary))
  (force-mode-line-update t)
  (when-let* ((buffer (get-buffer claude-code-status--list-buffer)))
    (with-current-buffer buffer
      (claude-code-status--render))))

;;; Remembering sessions

(defun claude-code-status--record (buffer)
  "Return a plist describing BUFFER's session, or nil if it can't be resumed."
  (with-current-buffer buffer
    (when-let* ((session-id claude-code-status--session-id)
                (name (claude-code-status--parse-name buffer)))
      (list :directory (car name) :instance (cdr name)
            :session-id session-id :transcript claude-code-status--transcript
            :state claude-code-status--state :since claude-code-status--since
            :detail claude-code-status--detail
            :detail-from-end claude-code-status--detail-from-end))))

(defun claude-code-status--save ()
  "Write the running and remembered sessions to the sessions file."
  (when claude-code-status-remember-sessions
    (let ((records (append (delq nil (mapcar #'claude-code-status--record
                                             (claude-code-status--session-buffers)))
                           claude-code-status--remembered))
          (file claude-code-status-sessions-file))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert ";; -*- lisp-data -*-  Sessions for claude-code-status.el\n")
        (let ((print-length nil) (print-level nil))
          (prin1 records (current-buffer)))
        (insert "\n")))))

(defun claude-code-status--schedule-save ()
  "Save sessions once Emacs is idle, coalescing bursts of changes."
  (when (and claude-code-status-remember-sessions
             (not claude-code-status--exiting)
             (not claude-code-status--save-timer))
    (setq claude-code-status--save-timer
          (run-with-idle-timer 2 nil (lambda ()
                                       (setq claude-code-status--save-timer nil)
                                       (claude-code-status--save))))))

(defun claude-code-status--save-on-exit ()
  "Remember the running sessions before Emacs exits and they die."
  (unless claude-code-status--exiting
    (setq claude-code-status--exiting t)
    (claude-code-status--save)))

(defun claude-code-status--load ()
  "Read remembered sessions, leaving out any that are running now."
  (let ((file claude-code-status-sessions-file)
        (running (delq nil (mapcar (lambda (buffer)
                                     (buffer-local-value 'claude-code-status--session-id buffer))
                                   (claude-code-status--session-buffers)))))
    (setq claude-code-status--remembered
          (and (file-readable-p file)
               (seq-filter (lambda (record)
                             (and (plistp record)
                                  (stringp (plist-get record :session-id))
                                  (stringp (plist-get record :directory))
                                  (not (member (plist-get record :session-id) running))))
                           (ignore-errors
                             (with-temp-buffer
                               (insert-file-contents file)
                               (read (current-buffer)))))))))

(defun claude-code-status--resume (record)
  "Start a session that resumes remembered session RECORD's conversation."
  (let* ((directory (expand-file-name (plist-get record :directory)))
         (transcript (plist-get record :transcript))
         (taken (mapcar #'cdr
                        (seq-filter (lambda (name)
                                      (and name (file-equal-p (car name) directory)))
                                    (mapcar #'claude-code-status--parse-name
                                            (claude-code-status--session-buffers)))))
         (base (plist-get record :instance))
         (instance base))
    (unless (file-directory-p directory)
      (user-error "%s no longer exists" directory))
    (when (and transcript (not (file-exists-p transcript)))
      (user-error "Claude no longer has this conversation"))
    (cl-loop for n from 2 while (member instance taken)
             do (setq instance (format "%s-%d" base n)))
    (require 'claude-code)
    ;; Start through claude-code.el's own path, in the remembered
    ;; directory and under the remembered instance name.
    (cl-letf (((symbol-function 'claude-code--directory) (lambda () directory))
              ((symbol-function 'claude-code--prompt-for-instance-name)
               (lambda (&rest _) instance)))
      (claude-code--start '(4) (list "--resume" (plist-get record :session-id))))
    (setq claude-code-status--remembered (delq record claude-code-status--remembered))
    (claude-code-status--schedule-save)
    (claude-code-status--refresh)))

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
  "s" #'claude-code-status-cycle-sort
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

(defun claude-code-status--format-time (time)
  "Return TIME as a clock time if it's today, else as a date."
  (cond ((null time) "")
        ((equal (format-time-string "%F" time) (format-time-string "%F"))
         (format-time-string "%H:%M" time))
        (t (format-time-string "%b %e" time))))

(defun claude-code-status--insert-entry (session width)
  "Insert SESSION into the list, filling context lines to WIDTH.
SESSION is a session buffer or a remembered session's plist."
  (let* ((start (point))
         (live (bufferp session))
         (time (claude-code-status--format-time
                (claude-code-status--get session 'claude-code-status--since :since)))
         (detail (claude-code-status--get session 'claude-code-status--detail :detail))
         (from-end (claude-code-status--get session 'claude-code-status--detail-from-end
                                            :detail-from-end))
         (label (claude-code-status--label session)))
    (insert (claude-code-status--icon
             (if live (claude-code-status--display-state session) 'ended))
            " "
            (propertize label
                        'face (unless live 'shadow)
                        'help-echo (if live
                                       (car (claude-code-status--parse-name session))
                                     (format "%s\nEnded with Emacs; RET resumes it, k forgets it"
                                             (plist-get session :directory)))
                        'mouse-face 'highlight
                        'follow-link t)
            (propertize " " 'display `(space :align-to (- right ,(1+ (length time)))))
            (propertize time 'face (unless live 'shadow))
            "\n")
    (when (and detail (> claude-code-status-detail-lines 0))
      (dolist (line (claude-code-status--fit
                     detail (- width 3) claude-code-status-detail-lines from-end))
        (insert "  " (propertize line 'face 'claude-code-status-detail) "\n")))
    (put-text-property start (point) 'claude-code-status-session session)))

(defun claude-code-status--render ()
  "Redraw the session list in the current buffer, keeping point's session."
  (let* ((inhibit-read-only t)
         (window (get-buffer-window (current-buffer) t))
         (width (if window (window-body-width window) 40))
         (here (get-text-property (point) 'claude-code-status-session))
         (sessions (append (sort (claude-code-status--session-buffers)
                                 :key #'claude-code-status--sort-key)
                           (sort claude-code-status--remembered
                                 :key #'claude-code-status--sort-key))))
    (setq mode-name (format "Claude Sessions by %s" claude-code-status-sort))
    (force-mode-line-update)
    (erase-buffer)
    (if sessions
        (dolist (session sessions)
          (claude-code-status--insert-entry session width))
      (insert (propertize "No Claude sessions" 'face 'shadow)))
    (goto-char (or (and here (text-property-any (point-min) (point-max)
                                                'claude-code-status-session here))
                   (point-min)))
    (when window (set-window-point window (point)))))

(defun claude-code-status--session-at-point (&optional event)
  "Return the session at point, or at mouse EVENT's position.
That's a session buffer, or a remembered session's plist."
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
  "Switch to the session at point, or at mouse EVENT.
For a remembered session, start one that resumes its conversation."
  (interactive (list last-nonmenu-event))
  (let ((session (claude-code-status--session-at-point
                  (and (mouse-event-p event) event))))
    (cond ((bufferp session) (when (buffer-live-p session) (pop-to-buffer session)))
          (session (claude-code-status--resume session)))))

(defun claude-code-status-kill ()
  "Kill the session at point after confirming, or forget a remembered one."
  (interactive)
  (let ((session (claude-code-status--session-at-point)))
    (cond ((bufferp session)
           (when (y-or-n-p (format "Kill Claude session %s? "
                                   (claude-code-status--label session)))
             (if (fboundp 'claude-code--kill-buffer)
                 (claude-code--kill-buffer session)
               (kill-buffer session))
             (claude-code-status--refresh)))
          (session
           (when (y-or-n-p (format "Forget session %s? (Claude keeps its history) "
                                   (claude-code-status--label session)))
             (setq claude-code-status--remembered
                   (delq session claude-code-status--remembered))
             (claude-code-status--schedule-save)
             (claude-code-status--refresh))))))

(defun claude-code-status-rename (name)
  "Rename the session at point to instance NAME.
claude-code.el shows the instance name after the project, as in
`project:NAME', and offers it when you choose among sessions."
  (interactive
   (let ((session (claude-code-status--session-at-point)))
     (unless (bufferp session)
       (user-error (if session "Resume the session to rename it" "No session here")))
     (list (read-string "Rename session to: "
                        (cdr (claude-code-status--parse-name session))))))
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
    (claude-code-status--schedule-save)
    (claude-code-status--refresh)))

(defun claude-code-status-cycle-sort ()
  "Sort the session list the next way: by status, time, or name.
See `claude-code-status-sort'."
  (interactive)
  (let ((orders '(status time name)))
    (setq claude-code-status-sort
          (or (cadr (memq claude-code-status-sort orders)) (car orders))))
  (claude-code-status--render)
  (message "Sorted by %s" claude-code-status-sort))

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
        (claude-code-status--follow-program-status t)
        (add-hook 'kill-emacs-hook #'claude-code-status--delete-payload-files)
        ;; Ahead of other exit hooks, which may kill the sessions.
        (add-hook 'kill-emacs-hook #'claude-code-status--save-on-exit -90)
        (when claude-code-status-remember-sessions
          (claude-code-status--load))
        (when claude-code-status-mode-line
          (add-to-list 'global-mode-string 'claude-code-status--mode-line-string t))
        (unless (server-running-p)
          (display-warning 'claude-code-status
                           "No Emacs server is running, so Claude hooks can't report session state; see `server-start'")))
    (remove-hook 'claude-code-process-environment-functions
                 #'claude-code-status--environment)
    (claude-code-status--follow-program-status nil)
    (remove-hook 'kill-emacs-hook #'claude-code-status--save-on-exit)
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
