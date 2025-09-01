;; init-org.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; XXX Windows dir
;; XXX don't call it org-agenda, maybe org-pkm
(defcustom my/notes-dir (file-truename "~/Documents/org-agenda")
  "Top-level org-mode notes dir, used for unified agenda, org-roam and PKM."
  :type '(string)
  :group 'pkm)
(unless (file-exists-p my/notes-dir)
  (make-directory my/notes-dir))

;; Org mode

;; Basic org-mode config
(use-package org
  :ensure t          ; use latest even though org is included in emacs
  :hook
  (org-mode . (lambda ()
                (mixed-pitch-mode 1)
                (visual-line-mode 1)
                (setq line-spacing 0.4) ; seems loose, but it looks good
                ;; I don't use ispell, no need for this
                (setq completion-at-point-functions
                      (delete #'ispell-completion-at-point completion-at-point-functions))
                ))
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda))
  :config
  (require 'org-tempo)
  (setopt
   ;; Directories and files
   org-directory my/notes-dir
   org-agenda-files (list org-directory)
   org-default-notes-file (concat org-directory "/journal.org")

   ;; TODO keywords and logging
   org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
   org-log-done 'time

   ;; Navigation and links
   org-return-follows-link t
   org-use-speed-commands t

   ;; Agenda behavior
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-start-on-weekday nil

   ;; Tags and refiling
   org-tags-exclude-from-inheritance '("project")
   org-tag-faces '(("@work" . "#0066ff")
                   ("@home" . "#bb0000")
                   ("volunteer" . "#005500"))
   org-refile-targets '((nil :maxlevel . 4)
                        (org-agenda-files :maxlevel . 4))

   ;; Display and formatting
   org-startup-folded 'nofold
   org-startup-indented t
   org-src-fontify-natively t
   org-list-allow-alphabetical t
   org-use-sub-superscripts '{}
   org-indent-mode-turns-on-hiding-stars nil
   org-startup-with-inline-images t
   org-image-actual-width nil

   ;; Babel
   org-confirm-babel-evaluate nil

   ;; Export settings
   org-export-backends '(ascii html icalendar latex koma-letter)
   org-export-coding-system 'utf-8
   org-export-with-sub-superscripts '{}
   org-export-with-toc nil

   ;; LaTeX export
   org-latex-listings t
   org-latex-packages-alist '(("cm" "fullpage" nil)
                              ("compact" "titlesec" nil)
                              ("" "paralist" nil)
                              ("" "color" nil)
                              ("" "tabularx" nil)
                              ("" "enumitem" nil))

   ;; Table settings
   org-table-convert-region-max-lines 9999
   )

   ;; Babel (code execution)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (python . t)
     (js . t)
     (dot . t)
     (ditaa . t)
     (latex . t)
     (sql . t)
     (shell . t)))
  (setq org-babel-python-command "uv run python")

  ;; when modifying agenda files make sure to update appt
  (when (file-exists-p org-directory)
    (require 'filenotify)
    (file-notify-add-watch org-directory '(change) #'gco-org-agenda-file-notify))
  )

(defun my/org-refresh-faces ()
  "Refresh mixed-pitch after tweaking faces."
  (when (derived-mode-p 'org-mode)
    (mixed-pitch-mode -1)
    (mixed-pitch-mode 1)))

(add-hook 'after-setting-font-hook #'my/org-refresh-faces)

;; Show full links for editing when point is over them
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  ;; Reveal links and emphasis markers at point
  (setq org-appear-autolinks t     ;; expand [[links][desc]] when point enters
        org-appear-autosubmarkers t ;; also expand *bold*, /italic/ markers
        org-appear-autoentities t   ;; show \alpha etc.
        org-appear-delay 0.1))      ;; small delay so it feels smooth


;; Org query language. Searches in ~org-agenda-files~.
(use-package org-ql
  :config
  ;; Create org-ql view for recent journals
  (require 'org-ql-view)
  (setopt org-ql-views
          (append org-ql-views
                  '(("Recent Journal Entries"
                     :buffers-files org-agenda-files
                     :query (and (path "journal.org")
                                 (level 3)
                                 (ts-active :from -30))
                     :title "Recent Journal Entries (Last 30 Days)"
                     :sort (date reverse)))))
  )

;; Modern functional API for org-mode
(use-package org-ml)

;; Images
(use-package org-remoteimg
  :ensure (:host github :repo "gaoDean/org-remoteimg")
  :after org
  :config
  (setopt org-display-remote-inline-images 'cache)
  )

(use-package org-imgtog    ; toggle images off when cursor enters them
  :ensure (:host github :repo "gaoDean/org-imgtog")
  :hook org-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org agenda/supertag PKM/second-brain/note-taking setup:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Exporting source blocks to HTML needs this
(use-package htmlize)
;; Live preview of HTML exports:
;; (Use org-preview-html-mode)
(use-package org-preview-html
  :commands org-preview-html-mode
  :config
  (setq org-preview-html-viewer 'xwidget))

(defun go/verify-refile-target ()
  "Exclude TODOS as refile targets."
  (not (member (nth 2 (org-heading-components)) (list "TODO" "DONE"))))
(setq org-refile-target-verify-function 'go/verify-refile-target)

(defun go/journal-timestamp-target ()
  "Return position at the end of today's journal entry body, creating it if needed.
Suitable for use in `org-capture-templates'. Uses org-datetree and ensures an ID."
  (require 'org)
  (require 'org-datetree)
  (let* ((ts (format-time-string "<%Y-%m-%d %a>"))
         (date (calendar-current-date))
         pos)
    (with-current-buffer (find-file-noselect org-default-notes-file)
      (org-with-wide-buffer
        (widen)
        ;; Find/create the day headline
        (setq pos (or (org-find-exact-headline-in-buffer ts)
                      (save-excursion
                        (org-datetree-find-date-create date)
                        (org-edit-headline ts)
                        (point)))))
      ;; Ensure ID (creates drawer immediately under the heading if missing)
      (goto-char pos)
      (org-id-get-create)

      ;; Jump to end of this headline's contents (after drawer & any existing items)
      (org-back-to-heading t)
      (let* ((el (org-element-at-point))
             (append-pos (or (- (org-element-property :contents-end el) 1)
                             ;; If no contents yet, use contents-begin (start of body)
                             (org-element-property :contents-begin el)
                             ;; Fallback: end of subtree
                             (save-excursion (org-end-of-subtree t t) (point)))))
        (goto-char append-pos)
        ;; Ensure exactly one line for the new item: only add a newline
        ;; if we're not already at BOL (avoid creating extra blank lines)
        (unless (bolp) (insert "\n"))
        (point)))))

;; C-c c j/n/t/f
(setq org-capture-templates
  '(("j" "Journal Note" item
     (file+function org-default-notes-file go/journal-timestamp-target)
     "- %?\n")
    ("n" "Note" entry
     (file org-default-notes-file)
     "* %U - %?\n")
    ("t" "TODO" entry
     (file+headline org-default-notes-file "Tasks")
     "* TODO %?\n  SCHEDULED: %t\n")
    ("f" "New File Note" plain
     (file (lambda () (read-file-name "Note file: " org-directory nil nil ".org")))
     "#+title: %^{Title}\n\n%?\n")
    )
)

(setq org-agenda-custom-commands        ; C-a a <cmd>
      '(("w" "At work"
         ((agenda "" ((org-agenda-span 2)))
          (tags-todo "+PRIORITY=\"A\"") ; top priority
          (tags-todo "@work")
          )
         ((org-agenda-compact-blocks t)))
        ("h" "At home"
         ((agenda "" ((org-agenda-span 4)))
          (tags-todo "+PRIORITY=\"A\"") ; top priority
          (tags-todo "@home")
          )
         ((org-agenda-compact-blocks t)))
        ("i" "Inbox"
         ((tags-todo "+CATEGORY=\"Inbox\"")
          )
         )
        ("u" "Uncategorized"
         ((tags-todo "-{.*}"
                     ((org-agenda-overriding-header "Uncategorized TODOs")))
          )
         )
        ("U" "Unscheduled"
         ((todo ""
                ((org-agenda-overriding-header "Unscheduled TODOs")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
          )
         )
        ;; other commands here
        ))

;; this is a "sexp diary" function -- "date" is provided by dynamic scoping.
;; It's a list of (month day year).
(defun first-of-month-unless-weekend ()
  "Return t if date (provided dynamically) is the first of the month.
  Unless the first falls on a weekend, in which case return t if
  this is the first Monday of the month."
  (let ((dayname (calendar-day-of-week date)) ; dayname is 0=Sun, 1=Mon, ...
        (day (cadr date)))
    (or (and (= day 1) (memq dayname '(1 2 3 4 5)))
        (and (memq day '(2 3)) (= dayname 1)))
    ))
(defun first-of-quarter-unless-weekend ()
  "Return t if date (provided dynamically) is the first day of the quarter.
  Unless the first falls on a weekend, in which case return t if
  this is the first Monday of the month."
  (let ((month (car date)))
    (and (memq month '(1 4 7 10))
         (first-of-month-unless-weekend))
    ))

;; Auto regenerate agenda when files change - use inotify
(defun gco-org-agenda-file-notify (_event)
  "Rebuild all agenda buffers when _EVENT specifies any org agenda files change."
  ;; XXX this causes a backtrace sometimes?
  ;(org-agenda-to-appt t)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-redo t)))))

;; Add notifications for appointments
(use-package appt
  :ensure nil
  :config
  (appt-activate t)
  (setq appt-display-mode-line t
        appt-display-interval 5
        appt-message-warning-time 10)
  (add-hook 'org-agenda-finalize-hook 'org-agenda-to-appt)
  )


;;; Prettify org-mode buffers

;; Use variable-pitch mode and use bullet symbols for bullet lists
;; with ~+~ and ~-~.

;; Tried this but it de-indents content when using indent mode
;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode))

;; Use utf-8 bullets for bullet lists -- this isn't great, but a bit nicer than nothing.
;; Ideally should use monospace font for spaces before bullet item, and use different bullets by list level.
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

;; De-emphasize the tildes org-mode uses for source snippets by making them small
;; This doesn't work with modus-themes, which seems to override it.
(defface org-tilde-face
  '((t :inherit default :height 0.5))
  "Face for highlighting tildes in org-mode")
(font-lock-add-keywords 'org-mode '(("~" . ''org-tilde-face)))

;; Make property drawers less obtrusive
(custom-set-faces
 '(org-drawer ((t (:inherit shadow :height 0.7))))
 '(org-property-value ((t (:inherit shadow :height 0.7)))))



;;; Org-roam

;; Org-roam adds nice backlinks and IDs everywhere.
;; It can be used with a file-per-day journal setup, but I don't use that.
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory my/notes-dir)
  (org-roam-completion-everywhere t)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id) ; auto-create IDs
  ;; Nice completion UI: show title + (short path)
  (org-roam-node-display-template
   (concat "${title:*}  "
           (propertize "${tags:10}" 'face 'org-tag)
           (propertize "  ·  ${file-title}" 'face 'shadow)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (message "Set up org-roam in %s" org-roam-directory)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(use-package consult-org-roam
  :after (org-roam consult)
  :custom (consult-org-roam-grep-func #'consult-ripgrep)
  :bind (("C-c n f" . org-roam-node-find) ;; find nodes by heading
         ("C-c n i" . org-roam-node-insert)      ;; insert ID link for heading
         ("C-c n s" . consult-org-roam-search))) ; search & open node; uses ripgrep

;;; org-supertag: logseq-like org management
(use-package deferred
      :ensure t)
(use-package epc
      :ensure t)

;; ;;; org-supertag: inline tags, transclusion and db-based searching
;; (use-package org-supertag
;;   :ensure (:host github :repo "yibie/org-supertag")
;;   :after org deferred epc
;;   :config
;;   (setq org-supertag-bridge-enable-ai nil) ; keep nil til I get uv python working
;;   (setq org-supertag-sync-directories (list org-directory)) ;; Configure sync folders
;;   (org-supertag-setup))

;;; Helper functions for workflows

(defun my/org-journal-today ()
  "Jump to today's journal entry, creating if needed."
  (interactive)
  (find-file org-default-notes-file)
  (goto-char (point-min))
  (let ((today (format-time-string "<%Y-%m-%d %a>")))
    (if (search-forward today nil t)
        (org-show-subtree)
      (org-datetree-find-date-create (calendar-current-date) 'subtree-at-point)
      (org-show-subtree))))

(defun my/org-auto-commit ()
  "Auto-commit org files after save."
  (when (and (buffer-file-name)
             (string-prefix-p (expand-file-name org-directory) (buffer-file-name)))
    (shell-command-to-string
     (format "cd %s && git add -A && git commit -m 'Auto-commit: %s'"
             (shell-quote-argument org-directory)
             (format-time-string "%Y-%m-%d %H:%M")))))

(defun my/create-tag-page (tag)
  "Create a dynamic tag page for TAG."
  (interactive "sTag: ")
  (let ((filename (format "%s/tag-%s.org" org-directory tag)))
    (find-file filename)
    (when (= (buffer-size) 0)
      (insert (format "#+title: #%s\n\n" tag))
      (insert (format "#+BEGIN: org-ql :query (tags \"%s\")\n\n#+END:\n\n" tag))
      (insert "Press C-c C-c on the block above to refresh.\n")
      (org-dblock-update))))

;; Enable auto-commit (comment out if you don't want this)
;; (add-hook 'after-save-hook #'my/org-auto-commit)

;; Add keybinding for jumping to today
(global-set-key (kbd "C-c j") #'my/org-journal-today)

(provide 'init-org)
