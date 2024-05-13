;;; init-org.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Org mode

;; Basic org-mode config
(use-package org
  :ensure nil                           ; already installed in init.el
  :hook
  (org-mode . (lambda ()
                (mixed-pitch-mode 1)
                (visual-line-mode 1)
                ;; I don't use ispell, no need for this
                (setq completion-at-point-functions
                      (delete #'ispell-completion-at-point completion-at-point-functions))
                ))
  :bind
  (:map org-mode-map
        ("M-RET" . completion-at-point)
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org agenda setup:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-directory "~/Documents/org-agenda") ; inbox.org, gtd.org, tickler.org ...
(setq org-agenda-files (list org-directory)) ; all .org files in these dirs
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-log-done 'time)
(setq org-return-follows-link t)        ; Enter key to follow links
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-start-on-weekday nil)  ; start on today
;; Projects are headings with the :project: tag, shouldn't be inherited.
(setq org-tags-exclude-from-inheritance '("project"))
(setq org-tag-faces
      '(("@work" . "#0066ff")
        ("@home" . "#bb0000")
        ("volunteer" . "#005500")))
(setq org-refile-targets (quote ((nil :maxlevel . 4)
                                 (org-agenda-files :maxlevel . 4))))

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
                                        ;(add-hook 'auto-save-hook 'org-save-all-org-buffers)            ; autosave always
                                        ;(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers) ; autosave on quit agenda

;;; Used these when I was trying org agenda
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "<f9>") 'org-agenda) ; faster, one keystroke
;; (global-set-key (kbd "<f8>") 'org-capture) ; faster, one keystroke
;; (global-set-key (kbd "C-c c") 'org-capture)

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

;; agenda template expansions: (e.g. C-c c t to capture a todo)
;; ^G: prompt for tags
;; ^t: prompt for timestamp
;; %U: add inactive timestamp (creation time)
;; (defvar org-capture-templates
;;   '(("t" "Todo [inbox]" entry
;;      (file+headline "inbox.org" "Tasks")
;;      "* TODO %i%?\n  %U"
;;      :prepend t)
;;     ("." "Today" entry
;;      (file+headline "inbox.org" "Tasks")
;;      "* TODO %^{Task}\nSCHEDULED: %t\n"
;;      :immediate-finish t)
;;     ("s" "Scheduled TODO" entry
;;      (file+headline "inbox.org" "Tasks") ;prompts for tags and schedule date (^G, ^t)
;;      "* TODO %? %^G \nSCHEDULED: %^t\n  %U")
;;     ("d" "Deadline" entry
;;      (file+headline "inbox.org" "Tasks")
;;      "* TODO %? %^G \n  DEADLINE: %^t"
;;      :empty-lines 1)
;;     ("w" "Work" entry
;;      (file+headline "gtd.org" "Work")
;;      "* TODO %i%?\n  %U"
;;      :prepend t)
;;     ("h" "Home" entry
;;      (file+headline "gtd.org" "Home")
;;      "* TODO %i%?\n  %U"
;;      :prepend t)
;;     ("T" "Tickler" entry
;;      (file+headline "tickler.org" "Tickler")
;;      "* TODO %i%? \n %U")
;;     ))
;; (defun gtd ()
;;   (interactive)
;;   (find-file (concat org-directory "/gtd.org")))

;; Auto regenerate agenda when files change - use inotify
(defun gco-org-agenda-file-notify (_event)
  "Rebuild all agenda buffers when _EVENT specifies any org agenda files change."
  (org-agenda-to-appt t)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-redo t)))))
;; when modifying agenda files make sure to update appt
(if (file-exists-p org-directory)
    (progn
      (require 'filenotify)
      (dolist (file org-agenda-files)
        (file-notify-add-watch file '(change) #'gco-org-agenda-file-notify))
      ))

(require 'org-tempo)

(setq
 org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t)
     (ditaa . t)
     (latex . t)
     (sql . t)
     (shell . t))
 org-confirm-babel-evaluate nil
 org-export-backends '(ascii html icalendar latex odt koma-letter)
 org-export-coding-system 'utf-8
 org-export-with-sub-superscripts '{}
 org-export-with-toc nil
 org-latex-listings t
 org-latex-packages-alist
   '(("cm" "fullpage" nil)
     ("compact" "titlesec" nil)
     ("" "paralist" nil)
     ("" "enumitem" nil)
     ("" "color" nil)
     ("" "tabularx" nil)
     ("" "enumitem" nil))
 org-list-allow-alphabetical t
 org-odt-convert-processes
   '(("LibreOffice" "\"c:/Program Files (x86)/LibreOffice 5/program/soffice\" --headless --convert-to %f%x --outdir %d %i")
     ("unoconv" "unoconv -f %f -o %d %i"))
 org-odt-preferred-output-format "docx"
 org-src-fontify-natively t
 org-startup-folded nil
 org-startup-indented t                 ; indent content
 org-table-convert-region-max-lines 9999
 org-use-sub-superscripts '{}
 org-use-speed-commands t)

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
(defface org-tilde-face
  '((t :inherit default :height 0.7))
  "Face for highlighting tildes in org-mode")
(font-lock-add-keywords 'org-mode '(("~" . ''org-tilde-face)))


;;; Org-roam

(if (file-exists-p "~/Documents/org-roam")
    (use-package org-roam
      :init
      (setq org-roam-v2-ack t)
      :custom
      (org-roam-directory "~/Documents/org-roam")
      (org-roam-completion-everywhere t)
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
      (require 'org-roam-dailies) ;; Ensure the keymap is available
      (org-roam-db-autosync-mode)))


(provide 'init-org)
