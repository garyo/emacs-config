;;; init-misc.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; In WSL2, browse to URLs using Windows cmd.exe which will open
;; default browser.
(cond (wsl2-p
       (setq
        browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args     '("/c" "start" "")
        browse-url-browser-function 'browse-url-generic)
       ))

(winner-mode 1)	; restore window config w/ C-c left (C-c right to redo)

(repeat-mode 1) ;; allow C-x ^^^^ to enlarge window with each press of ^ (same for C-x },{,v})

;; windmove: shift+arrow keys to move between windows.
;; Should be available since emacs 21.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;; Turn off visual-line-mode
(visual-line-mode nil) ; next-line go to real next line, see also line-move-visual
(global-visual-line-mode 0)
(setq line-move-visual nil)			; C-n go to next real line

(setq-default cache-long-scans t) ; speed up redisplay with very long lines, e.g. compilation buffers

;; always enable electric-pair-mode to insert matching parens & braces
(electric-pair-mode t)
(defun gco-inhibit-electric-pair-predicate (c)
  (or
   ;; if within a string started by the same char, inhibit pair insertion
   (save-excursion
     (let ((s (syntax-ppss (- (point) 1))))
       (eq (nth 3 s) c)))
   ;; inhibit when it helps balance
   (save-excursion
     (electric-pair-inhibit-if-helps-balance c))
   ;; inhibit when same char is next, or 2nd "" or ((, or next to a word
   (save-excursion
     (electric-pair-conservative-inhibit c))
   ))

(setq electric-pair-inhibit-predicate 'gco-inhibit-electric-pair-predicate)


(blink-cursor-mode -1)	;this is annoying
;;(mouse-avoidance-mode 'animate)
(global-font-lock-mode 1)

(add-hook 'sql-mode-hook
          (lambda () (sql-highlight-mysql-keywords)))

;; Show blank lines
(setq-default indicate-empty-lines t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Automatically revert files that change on disk
;; (but only when the buffer is unmodified, so it's safe)
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t) ; dired

;; Save all backup(~) files and auto-save files in /tmp
;; This keeps clutter down.
(defconst emacs-tmp-dir
  (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(set-variable 'create-lockfiles nil)     ; dangerous but useful for file-watching recompiles

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
  http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
  this.  The function inserts linebreaks to separate tags that have
  nothing but whitespace between them.  It then indents the markup
  by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ibuffer: I don't use this anymore; consult-buffer is better.
(setq ibuffer-formats '((mark modified read-only " " (name 16 16) " "
                              (size 6 -1 :right) " " (mode 16 16 :center)
                              " " (process 8 -1) " " filename)
                        (mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")
(require 'ibuffer)

;; Trying this out; not sure if I'll ever use it.
(require 'misc) ; has forward-to-word
(global-set-key (kbd "M-<right>") 'forward-to-word)

(defun end-of-buffer-right-way ()
  "Put point at the end of the buffer and also at the bottom of the window."
  (interactive nil)
  (push-mark)
  (goto-char (point-max))
  (recenter -2))

(defun copy-line (arg)
  "Copy lines (as many as prefix ARG) into the kill ring.

          Ease of use features:
          - Move to start of next line.
          - Appends the copy on sequential calls.
          - Use newline as last char even on the last line of the buffer.
          - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

    ;;; We use .cp for C source files, but emacs ignores them by default.
(setq completion-ignored-extensions
      (remove nil
              (remove ".log"
                      (remove ".cp" completion-ignored-extensions))))

;; This fixes the slow startup of query-replace when using Droid Sans Mono Dotted font
;; The default value of this isn't in that font I guess? (In fact, even pasting it
;; in here makes redisplay slow down!)
(setq-default query-replace-from-to-separator " -> ")


;; This is very important to speed up display of long lines.
;; It's not perfect but it should help.
(setq-default bidi-display-reordering nil)

;; Always use '(foo) rather than (quote (foo)) in customize
;; (custom-set-variables below)
(advice-add 'custom-save-all :around
            (lambda (orig)
              (let ((print-quoted t))
                (funcall orig))))

;;; Start emacs server
(require 'server)
(unless (server-running-p)
  (server-start))


(provide 'init-misc)
