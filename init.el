;;; -*-mode: emacs-lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gary's .emacs file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For this to work, HOME must be set to the dir of this file.

(add-to-list 'load-path
	     (expand-file-name "~/emacs"))

(server-start)
(require 'cl)

;; edit server for Chrome (browser extension):
(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (message "Starting edit server for Chrome...")
  (edit-server-start))

;;;; FONTS ;;;;;;
;; Notes:
;; use M-x describe-font RET to describe current font
;; C-u C-x = describes font under point (and lots of other goodies).
(setq preferred-fonts
      '(
	;; Droid Sans Mono: quite nice.
	;; 15 pixels total height at 10 point.  Clear & crisp.
	;; (e.g. http://www.fontex.org/download/Droid-sans-mono.ttf)
	"Droid Sans Mono Dotted-10"
	"Droid Sans Mono-10"
	;; Consolas: download installer from Microsoft.
	;; Quite beautiful and renders nicely, but a little light.
	;; Pretty similar to Droid Sans Mono.
	;; The slanted verticals on the capital M annoy me a little.
	;; (16 pixels height)
	"Consolas-10.5"
	;; Inconsolata: lots of people like this.
	;; http://www.levien.com/type/myfonts/inconsolata.html:
	;; about same size as Consolas-10.5, but thicker and less leading
	;; (17 pixels height) and not as smooth lines.  Feels chunky.
	"Inconsolata-12"
	;; default
	"Courier New-10.5"
        "Courier-10"))
(defun find-first-font (list)
  (cond ((null list)
	 nil)
	((x-list-fonts (car list))
	 (message (concat "Using font " (car list)))
	 (car list))
	(t				; recurse
	 (find-first-font (cdr list)))
	))
; set default font attributes (for all frames)
(if window-system
    (progn
      (set-face-attribute 'default nil :font (find-first-font preferred-fonts))

      (cond ((> (x-display-pixel-height) 1000)
	     (setq initial-frame-alist (list
					'(top . 15)
					'(left . 1000)
					'(width . 98)
					'(height . 64)
					)))
	    (t
	     (setq initial-frame-alist (list
					'(top . 15)
					'(left . 20)
					'(width . 98)
					'(height . 40)
					))))
      ))

;;; TMC Completion
(setq *cdabbrev-radius* nil		; search whole buffer
      *print-next-completion-does-cdabbrev-search-p* t ;show next even if cdabbrev
      *separator-character-uses-completion-p* t	       ; save after typing separator
      )
(ignore-errors
 (condition-case nil
     (load-library "completion-11-4")
   (error
    (load-library "completion-11-2")))
 (initialize-completions)
 )

;; use zsh or bash.  Do this early on before loading any git stuff,
;; otherwise that will try to use cmdproxy.exe.
(cond ((executable-find "zsh")
       (setq explicit-shell-file-name "zsh"))
      ((executable-find "bash")
       (setq explicit-shell-file-name "bash"))
      (t nil))

(setq shell-file-name explicit-shell-file-name)

(load-library "paren")
(show-paren-mode)
(recentf-mode t)
(if (> emacs-major-version 22)
    (progn
      (visual-line-mode nil) ; next-line go to real next line, see also line-move-visual
      (global-visual-line-mode 0)
      (setq line-move-visual nil)))

;;; as of 21-Oct-10, git as a vc backend makes saving files really slow (a second or more).
;;; I think egg is fine, don't really need vc.
(setq vc-handled-backends (remq 'Git vc-handled-backends))
(setq vc-handled-backends (remq 'git vc-handled-backends))

(ignore-errors
    (require 'git-emacs-autoloads) ; an emacs GIT interface (one of many); try M-x git-status
    (require 'git-emacs)
    (require 'git-status)
    (autoload 'mo-git-blame-file "mo-git-blame" nil t)
    (autoload 'mo-git-blame-current "mo-git-blame" nil t)
    (require 'egg) ; another emacs GIT interface; try M-x egg-log or egg-status
  )

(add-to-list 'exec-path "c:/Program Files (x86)/Git/cmd") ; for Git
; (add-to-list 'exec-path "c:/Program Files/TortoiseHg") ; for Hg/Mercurial

(add-to-list 'exec-path "c:/bin")
(add-to-list 'exec-path "c:/bin2")
(add-to-list 'exec-path "c:/Program Files/R/R-2.14.0/bin") ; for R (statistics pkg)
(setq python-command (or (executable-find "python") "c:/Python27/python"))

(defvar dc-auto-insert-directory "~/.emacs.d/Insert/")
(ignore-errors
  (require 'defaultcontent)
  (require 'filladapt)			;adaptive fill mode
)
(setq-default filladapt-mode t)

(autoload 'taskjuggler-mode "taskjuggler-mode" "TaskJuggler mode." t)

;;; C-c C-e to edit right in a grep buffer, C-c C-s to save.  Nice!
(ignore-errors
  (load-library "grep-ed"))

(ignore-errors
 (load-file "~/.emacs-orgmode")
)

;; Emacs Speaks Statistics (for R):
(ignore-errors
  (require 'ess-site)
  (ess-toggle-underscore nil)		; no annoying magic underscore
)

;; use M-x idb to run the Intel debugger inside emacs (looks like 'dbx')
(setq idbpath "c:/Program Files/Intel/IDB/10.0/IA32/Bin")
(if (file-readable-p (concat idbpath "/idb.el"))
    (progn (load-file (concat idbpath "/idb.el"))
	   (add-to-list 'exec-path idbpath))
  )

;; This makes [f8] insert a template in the current mode for a new (empty) file
;; and binds some C-c cmds in C and C++ mode.  (Try C-c SPC after "if" etc.)
(ignore-errors
  (require 'templates)
  )

(if window-system
    (tool-bar-mode 0))
(blink-cursor-mode -1)	;this is annoying
;;(mouse-avoidance-mode 'animate)
(global-font-lock-mode 1)

;; Set up font.
;; Use Shift-mouse-1 to select fonts interactively.
;; Then use M-x describe-font to see the full name of the current font
;; for use in set-frame-font (in emacs23 set-default-font is deprecated, use set-frame-font).
;; As of emacs23 we can use <name>-<size> to select fonts, much easier!
;; (cond ((eq system-type 'windows-nt)

(defface shell-hilight-face
  '((t (:background "grey80")))
  "Used for marking significant items in shell buffers.")
;;; Hilight compiler and linker output filenames so I can see them more easily
(defvar my-shell-extra-keywords
  '(("/OUT:[^ ]+" 1 shell-hilight-face)
    ("/Fo[^ ]+" 1 shell-hilight-face)
    ))
(add-hook 'shell-mode-hook
          (lambda ()
            (font-lock-add-keywords nil my-shell-extra-keywords)))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(ignore-errors
  (pcomplete-shell-setup)	; set up emacs24 programmable completion for shell mode; not that great but OK
  )
(add-hook 'sql-mode-hook
	  (lambda () (sql-highlight-mysql-keywords)))

;;; whitespace and blank lines:
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indicate-empty-lines t)
;(setq-default show-trailing-whitespace t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; Do NOT use standard-display-european: it breaks comint (shell)
;;; modes by adding an extra CR to every command, causing syntax
;;; errors from the shell etc.
;(standard-display-european 1)		; allow european chars
;(display-time)

(setq auto-mode-alist
      ;;; this selects . or \ or / or beginning-of-string
      ;;; followed by "diff" and maybe "s",
      ;;; then end of string.
      (cons '("\\(\\.\\|\\\\\\|/\\|\\`\\)diffs?\\'" . diff-mode)
	    auto-mode-alist))

(setq auto-mode-alist (cons '("\\.pl\\'" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SCons\\(truct\\|cript\\)\\'" . python-mode) auto-mode-alist))
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq visual-basic-mode-indent 4)
(setq auto-mode-alist (cons '("\\(\\.vb\\|\\.bas\\)\\'" . visual-basic-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cu$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cp$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tjp$" . taskjuggler-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))

;; nicer interface to git-blame, kinda -- not very great since the left/right buffers
;; easily get out of sync.
;(autoload 'mo-git-blame-file "mo-git-blame" nil t)
;(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;;; CEDET: emacs tools for C development
;; ;(ignore-errors
;;   (load-file "c:/emacs/site-lisp/cedet/common/cedet.el")
;;   (require 'semanticdb)			;save parse tree to file
;;   (global-semanticdb-minor-mode 1)
;;   ;; new suggestions
;;   ;;(global-ede-mode 1)                      ; Enable the Project management system
;;   ;;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;;   ;;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;;   (semantic-load-enable-minimum-features)
;;   ;;(semantic-idle-summary-mode) ; show doc for token at point in modeline
;;   (semantic-load-enable-code-helpers)
;;   ;;(semantic-idle-completions-mode)	;may be too weird?
;;   ;;(global-semantic-stickyfunc-mode 1)     ;header line in buffer w/ current func's header
;;   (which-function-mode 1)		;show func name in modeline

;;   ;; to get Intellisense completion:
;;   (global-ede-mode 1)		; Enable the Project management system
;;   (semantic-load-enable-code-helpers) ; Enable prototype help and smart completion
;;   ;; then in C mode, turn on global-semantic-idle-completions-mode

;;   (defun my-semantic-hook ()
;;     "Don't parse really large buffers"
;;     (cond ((string-match "^/usr/include" (buffer-file-name))
;; 	   nil)
;; 	  ((string-match "^/Progra" (buffer-file-name))
;; 	   nil)
;; 	  ((string-match "^c:/Progra" (buffer-file-name))
;; 	   nil)
;; 	  ((> (point-max) 1000000)
;; 	   nil)
;; 	  ;; only parse C and h files
;; 	  ((string-match "\\(\\.c\\|\\.cxx\\|\\.cpp\\|\\.h\\)$" (buffer-file-name))
;; 					;(message (concat "my-semantic-hook: OK to parse " (buffer-file-name)))
;; 	   t)
;; 	  (t
;; 					; (message (concat "my-semantic-hook: unknown file: " (buffer-file-name)))
;; 	   t)))
;;   (add-hook 'semantic--before-fetch-tags-hook
;; 	    'my-semantic-hook)

;;   ;; ECB: Emacs Code Browser
;;   ;(add-to-list 'load-path "c:/Program Files/emacs/site-lisp/ecb-2.40")
;;   ;(require 'ecb)
;; ;  )

;; ;(ignore-errors
;;   (require 'ede-load)
;;   (global-ede-mode t)
;;   (ede-cpp-root-project "Sapphire"
;; 		      :name "GenArts Sapphire"
;; 		      :file "/genarts/sapphire/SConstruct"
;; 		      :include-path '("/")
;; 		      :spp-table '(("WIN32" . "1")
;; 				   ("_WIN32" . "1")))
;; ;  )


;; (custom-set-variables
;;  '(global-semantic-stickyfunc-mode t nil (semantic-util-modes))
;;  '(semantic-default-c-path (quote ("c:/genarts/sapphire")))
;; )


;; Printing via GhostScript/GhostView, e.g. to color Epson C40UX
;; (non-PS) on Gary's desk.
(require 'ps-print)
(setq ps-lpr-command "c:\\Program Files\\Ghostgum\\gsview\\gsprint.exe")
;; -query causes ghostscript to query which printer to use.
;; other options: -twoup, -landscape, ... (see c:/Ghostgum/gsview/csprint.htm)
(setq ps-lpr-switches '("-query"))
(setq ps-print-color-p t)		; or t or 'black-white
(setq ps-printer-name t)
(setq ps-left-margin 30)
(setq ps-right-margin 10)
(setq ps-header-lines 1)
(setq ps-print-header-frame nil)
(setq ps-font-size '(7 . 9))

(setq user-full-name "Gary Oberbrunner"
      user-mail-address "garyo@genarts.com")
(defun end-of-buffer-right-way ()
  "Put point at the end of the buffer and also at the bottom of the window."
  (interactive nil)
  (push-mark)
  (goto-char (point-max))
  (recenter -2))

;; Copy a line into the kill ring, like C-k but without killing.
;; Moves point just like kill-line.
;; Works by temporarily making the buffer read-only.
(defun copy-line (&optional arg)
  "Copy a line (or LINES) into the killbuffer.
       Like kill-line (\\[kill-line]) but only copies, doesn't kill."
  (interactive "P")
  (let ((buffer-read-only t)
	(kill-read-only-ok t))
    (kill-line arg)))

(add-hook 'text-mode-hook
	  (lambda ()
	    (auto-fill-mode)))

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

;;; prevent newlines from being inserted after semicolons when there
;;; is a non-blank following line.
(defun my-semicolon-criteria ()
  (save-excursion
    (if (and (eq last-command-event ?\;)
             (zerop (forward-line 1))
             (not (looking-at "^[ \t]*$")))
        'stop
      nil)))

(defun my-c-mode-hook ()
  ;(load-library "cc-cmds")
  (setq c-basic-offset 2)
  (setq c-hanging-comment-ender-p nil)
  (setq c-hanging-comment-start-p nil)
  ;; Labels offset by 1 from parent, but keep case stmts
  ;; offset by c-basic-offset.
  (c-set-offset 'label 1)
  (c-set-offset 'case-label 1)
  (c-set-offset 'innamespace 0)		;don't indent in namespaces
  (c-set-offset 'inextern-lang 0)	;don't indent in extern "C"
  (c-set-offset 'statement-case-intro (lambda (in)
					(- c-basic-offset 1)))
  (c-set-offset 'statement-case-open (lambda (in)
					(- c-basic-offset 1)))
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-cont 'c-lineup-math)
  ; prevent arg lists from going off right side of page:
  ; longnamed_function(
  ;     arg_t arg1,
  ;     arg_t 2);
  (c-set-offset 'arglist-intro '++)	; 1st line in arg list (after open)
  (c-set-offset 'arglist-close '--)
  ; you might think auto-fill in C mode is a bad idea, but
  ; cc-mode is clever and only does it while in comments.
  ; see c-ignore-auto-fill.
  ; On the other hand, even doing it in comments can be annoying,
  ; so I have it turned off now.
  ; (turn-on-auto-fill)
  ;(c-toggle-hungry-state 1)
  (setq fill-column 77)
  (setq c-hanging-semi&comma-criteria
	(cons 'my-semicolon-criteria
	      c-hanging-semi&comma-criteria))
  (setq c-hanging-braces-alist
	'((brace-list-open)
	  (brace-list-close)
	  (brace-list-intro)
	  (brace-list-entry)
	  (substatement-open after)
	  (topmost-intro after)
	  (inline-open after)
	  (block-close . c-snug-do-while)
	  (extern-lang-open after)))

  (setq c-cleanup-list (cons 'defun-close-semi c-cleanup-list)))

(add-hook 'c-mode-common-hook
	  'my-c-mode-hook)

(add-hook 'java-mode-hook
	  (function
   (lambda ()
	     (make-variable-buffer-local 'c-basic-offset)
	     (setq c-basic-offset 2)
	     (local-set-key "\C-cc" 'compile)
	     )))

;; always hilight XXX in programming modes
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<XXX\\|TODO\\>" 0 font-lock-warning-face prepend)
	   )))
      '(c-mode c++-mode java-mode lisp-mode emacs-lisp-mode))

;; M-x align support for def-effects param options
(require 'align)
(setq orig-align-rules-list align-rules-list)
;;; add a new rule to line up param options, and remove the
;;; lisp-alist-dot which is bad for def-effects (screws up any decimal
;;; numbers).
(setq align-rules-list
      (cons '(def-effects-param
	       ;; match things like these, and align the open paren
	       ;;  foo = 0     (boolean)
	       ;;  foo = [0 0] (popup ...)
	      (regexp . "=\\s-*[[0-9.]?[ 0-9.]*[]0-9.]\\(\\s-*\\)(")
	      (group 1)
	      (modes . align-lisp-modes))
	    (remove* 'lisp-alist-dot orig-align-rules-list :key 'car)))



;;----------------------------------------------------------------------------
;; Programming commands (from ElijahDaniel.emacs)
;;----------------------------------------------------------------------------
;; If point is in a class definition, return the name of the class. Otherwise,
;; return nil.
(defun ewd-classname ()
  "If the point is in a class definition, get the name of the class.  Return
nil otherwise."
  (save-excursion
    (let ((brace (assoc 'inclass (c-guess-basic-syntax))))
      (if (null brace) '()
        (goto-char (cdr brace))
        (let ((class-open (assoc 'class-open (c-guess-basic-syntax))))
          (if class-open (goto-char (cdr class-open)))
          (if (looking-at "^class[ \t]+\\([A-Za-z_][^ \t:{]*\\)")
              (buffer-substring (match-beginning 1) (match-end 1))
            (error "Error parsing class definition!")))))))




(setq
 shell-pushd-regexp "pushd\\|1\\|2"
 shell-pushd-dextract t
 shell-pushd-dunique t
 ;shell-cd-regexp nil			; autopushd in zsh
 shell-chdrive-regexp "[a-z]:")		;

;; ;(add-hook 'comint-output-filter-functions
;; ;          'comint-strip-ctrl-m)

;; Not needed
;; ;(setenv "CYGWIN" "nobinmode")

;;This is from Voelker's emacs NT page:
(setq explicit-bash-args '("--login" "--noediting" "-i")
      ; explicit-zsh-args '("-i" "-o" "emacscygwinhack")
      explicit-zsh-args '("-i")
      ; explicit-sh-args '("-login" "-i") (only needed for bash)
      ; comint-scroll-show-maximum-output 'this
      comint-completion-addsuffix t
      ; comint-process-echoes nil ;; t for command.com, nil for bash
      comint-eol-on-send t
      comint-input-ignoredups t
      comint-input-ring-size 256
      w32-quote-process-args ?\"
      )

(make-variable-buffer-local 'comint-completion-addsuffix)

;;; eshell (shell implemented entirely in emacs, sometimes useful):
(add-hook 'eshell-mode-hook
  (function
   (lambda ()
     ;; This prevents vertical bars between letters in typed-in text
     ;; (probably an emacs 21.1 bug?)
     (setq cursor-type '(bar . 10))
     )))

(load-library "shell")

(defun open-folder-in-explorer ()
  "Call when editing a file in a buffer. Open windows explorer in the current directory and select the current file"
  (interactive)
  (cond (buffer-file-name
	 (w32-shell-execute
	  "open" "explorer"
	  (concat "/e,/select," (convert-standard-filename buffer-file-name))
	  ))
	(t
	  (w32-shell-execute
	   "open" "explorer"
	   (concat "/e,/root," (convert-standard-filename default-directory))
	  ))
	))
(defun w32-shell-dos-semantics() t)	;needed for above, to convert filenames to backslash form
(global-set-key [f12] 'open-folder-in-explorer)

;;; Now that I use more window splitting, I have to teach myself not to use C-x 1 all the time
;;; just to expand the current window.  Right now, make it just beep.
;;; Ideally, could check if any side-by-side windows exist, and do nothing only
;;; in that case.
(global-set-key "\C-x1" (lambda ()
			  (interactive)
			  (beep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GenArts tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-debug (key &optional level)
   "Set a debug flag in /tmp/sapphire.dbg, overwriting the previous
    value if any.  When called with a numeric prefix argument, set
    the debug level to the prefix value (a value of 0 deletes the
    flag entirely).  The default level, with no prefix argument,
    is 1."

   (interactive "MSet Debug Flag: \np")
   (with-current-buffer (find-file-noselect "/tmp/sapphire.dbg" 't)
     ; Revert to saved version of file without prompting
     (if (buffer-modified-p)
        (revert-buffer t t))

     ;; Clear old value (if any)
     (save-excursion
       (beginning-of-buffer)
       (if (search-forward-regexp (concat "^" key "=") nil t)
          (progn
            (beginning-of-line)
            (kill-line 1))))

     ;; Write new value
     (save-excursion
       (beginning-of-buffer)
       (if level
          (if (not (= level 0))
              (insert (format "%s=%d\n" key level)))
        (insert key "=1\n")))

     ;; Save file
     (save-buffer)))

(defun clear-debug ()
   (interactive)

   (with-current-buffer (find-file-noselect "/tmp/sapphire.dbg" 't)
     (if (buffer-modified-p)
        (revert-buffer t t))
     (mark-whole-buffer)
     (kill-region)
     (save-buffer)))

(global-set-key "\C-cd" 'set-debug)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SCons (with -D) starts builds from the top of the source tree, and it builds into
;;; an 'SBuild' subdir. But we want to find the original errors in the regular source dir,
;;; regardless of the current directory when we run M-x compile.
;;; Note default-directory may not be what you expect here, and
;;; the filenames are absolute, so need to remove surgically.
(defun process-error-filename (filename &optional spec-directory)
  (let ((case-fold-search t)
	(topdir (svn-base-dir nil))
	)
    ;; prepend dir
    (if (and spec-directory
	     (not (file-name-absolute-p filename)))
	(setq filename (concat spec-directory "/" filename)))

    (setq f (strip-sbuild (fix-win-path filename)))
    (save-current-buffer
      (set-buffer "*Messages*")
      (insert (format "process-error-filename: filename %s in %s -> %s %S (my dir=%s, top=%s)" filename spec-directory f (file-exists-p f) default-directory topdir)))
    (cond ((file-exists-p f)
	   f)
	  ((file-exists-p (concat topdir f))
	   (concat topdir f))
	  (t filename))))

;;; Move up the directory hierarchy from dir (nil for default-directory)
;;; until we find the top of the SVN working dir
(defun svn-base-dir (dir)
  (or (cdr (file-find-upwards dir ".git"))
      (cdr (file-find-upwards dir ".hg"))
      (cdr (file-find-upwards dir ".svn"))
      (cdr (file-find-upwards dir "primitives")) ; Sapphire only!
      dir
      ))

;;; Convert "\" to "/" so path-handling functions don't get confused
(defun fix-win-path (p)
  (cond (p (replace-regexp-in-string "\\\\" "/" p)))
  )

;;; Strip Sbuild dirs from a pathname
(defun strip-sbuild (p)
  (replace-regexp-in-string
   "[Ss]?[Bb]uild/.*\\(final\\|dbg\\)[^/]*/" "" p))

;;; found this on the web.  Useful!
;;; Modified to return (dir . file)
;;; pass nil for startfile to use default-directory.
(defun file-find-upwards (startfile file-name)
  ;; Chase links in the source file and search in the dir where it points.
  (setq dir-name (or (and startfile
                          (file-name-directory (file-chase-links startfile)))
                     default-directory))
  (setq dir-name (expand-file-name (file-chase-links dir-name)))
  ;; Move up in the dir hierarchy till we find the given file-name
  (let ((file1 (concat dir-name file-name))
        parent-dir)
    (while (and (not (file-exists-p file1))
                (progn (setq parent-dir
                             (file-name-directory
                              (directory-file-name
                               (file-name-directory file1))))
                       ;; Give up if we are already at the root dir.
                       (not (string= (file-name-directory file1)
                                     parent-dir))))
      ;; Move up to the parent dir and try again.
      (setq file1 (expand-file-name file-name parent-dir)))
    ;; If we found the file in a parent dir, use that.  Otherwise,
    ;; return nil
    (if (or (get-file-buffer file1) (file-exists-p file1))
        (cons file1 parent-dir)
      (cons nil nil))))

;;; For emacs 21.1, this requires a patch to compile.el, which is in
;;; Gary's email in the emacs folder (date around 10/25/2001).  Later
;;; versions should already have it.
(setq compilation-parse-errors-filename-function 'process-error-filename)

(setq compilation-mode-font-lock-keywords
      '(("^\"\\([^\"]*\", line [0-9]+:[ \t]*warning:[ \t]*\\)\\(.*$\\)"
	 2 font-lock-keyword-face)
	("^\"\\([^\"]*\", line [0-9]+:[ \t]*\\)\\(.*$\\)"
	 2 font-lock-function-name-face)))

;;; For emacs23, long lines in buffers make emacs really slow.
;;; This seems to ameliorate it a little.
;(add-hook 'compilation-mode-hook (lambda () (setq truncate-lines t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; These let you send mail from emacs on NT.
(setq smtp-service "smtp"
      smtpmail-default-smtp-server "penguin"
      smtp-default-server "penguin"
      smtpmail-local-domain "genarts.com"
      smtp-debug-info t
      )
(setq send-mail-function 'smtpmail-send-it)
(autoload 'mail "Send mail message" "smtpmail")
;(load-library "message")
(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'smtpmail-send-it) ;for gnus
(setq user-full-name "Gary Oberbrunner"
      user-mail-address "garyo@genarts.com"
      message-syntax-checks '((sender . disabled)))
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively
      gnus-message-archive-group "INBOX.Sent"
      gnus-message-archive-method '(nnimap "mail.hq.genarts.com")
      nnimap-authinfo-file '((""
			      ("login" . "garyo")))
      nnimap-list-pattern
      '("INBOX" "*"))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-select-method
      '(nnimap "mail.hq.genarts.com"
	       (nnimap-stream ssl)
	       (nnimap-server-port 993)
	       (nnimap-authenticator login)))
(setq mail-sources '((file :plugged t)))


;(custom-set-variables
; '(message-default-headers "Mime-Version: 1.0
;Content-type: text/plain; charset=ISO-8859-1
;Content-Transfer-Encoding: 8bit
;"))

;; NT emacs needs this for command.com to work as shell
;(setq process-coding-system-alist
;      (append '(("bash" . raw-text-unix)
;		("cmdproxy" . (raw-text-dos . raw-text-dos)))
;	      process-coding-system-alist))


;;; GNUserv stuff (for Windows, see http://www.wyrdrune.com/index.html?gnuserv.html)
;;; use 'gnuclient' or 'gnuclientw' to connect to this emacs
;(setenv "GNUSERV_SHOW_EMACS" "1")
;(load-library "gnuserv")
;(gnuserv-start)

;;; Dired-x (extra functions for dired mode)
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    ;; Set dired-x global variables here.  For example:
	    ;; (setq dired-guess-shell-gnutar "gtar")
	    ;; (setq dired-x-hands-off-my-keys nil)
	    (setq dired-omit-localp nil) ; match full pathname (slower)
	    (setq dired-omit-files "/\\.svn/\\|\\.svn-base$\\|/SBuild/\\|/\\.?#\\|/\\.$\\|/\\.\\.$")
	    ))
;(setq dired-omit-files "/\\.svn/\\|\\.svn-base$\\|/SBuild/\\|/\\.?#\\|/\\.$\\|/\\.\\.$")
(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Set dired-x buffer-local variables here.  For example:
	    ;(dired-omit-mode 1)
	    ))


;;; Note: emacs24 has scroll-up-line and scroll-down-line, no need for these anymore.
(defun scroll-down-one-line (arg)
  "Scroll down one line, or number of lines specified by prefix arg."
  (interactive "p")
  (scroll-down 1))
(put 'scroll-down-one-line 'isearch-scroll t) ;allow use during isearch
(put 'scroll-down-one-line 'search-command t) ;allow use during isearch
(defun scroll-up-one-line (arg)
  "Scroll up one line, or number of lines specified by prefix arg."
  (interactive "p")
  (scroll-down (- 1)))
(put 'scroll-up-one-line 'isearch-scroll t) ;allow use during isearch
(put 'scroll-up-one-line 'scroll-command t) ;allow use during search

(setq ibuffer-formats '((mark modified read-only " " (name 16 16) " "
			      (size 6 -1 :right) " " (mode 16 16 :center)
			      " " (process 8 -1) " " filename)
			(mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")
;(require 'ibuffer)

;;; Interactive buffer switching using minibuffer substring completion
;(require 'iswitchb)
;(iswitchb-mode)
(setq ido-enable-tramp-completion nil)	    ; workaround tramp bug in emacs 23.1
(ido-mode)


;;; This adds an isearch-mode keybinding for C-o to run Occur
;;; with the current search string, without interrupting the isearch.
;;; Way cool!
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(cond ((fboundp 'scroll-up-line)
       (global-set-key "\C-z" 'scroll-up-line) ; use emacs24 builtins
       (global-set-key "\M-z" 'scroll-down-line))
      (t
       (global-set-key "\C-z" 'scroll-up-one-line) ; use mine
       (global-set-key "\M-z" 'scroll-down-one-line)))

(global-set-key "\M-k" 'copy-line)
(global-set-key "\M->" 'end-of-buffer-right-way)
(global-set-key "\C-X." 'goto-line)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\M-n" '(lambda ()
			  "Move down 10 lines"
			  (interactive)
			  (next-line 10)))
(global-set-key "\M-p" '(lambda ()
			  "Move up 10 lines"
			  (interactive)
			  (previous-line 10)))
(global-set-key [f5] 'compile)

;;; for Sapphire def-effects.text: indent (def ...) like a special
;;; form with one arg (the name), not like a defun.
(put 'def 'lisp-indent-function 1)

;;; We use .cp for C source files, but emacs ignores them by default.
(setq completion-ignored-extensions
      (remove nil
	      (remove ".log"
		      (remove ".cp" completion-ignored-extensions))))

(setq
 backup-by-copying-when-linked t
 font-lock-maximum-decoration t
 cache-long-line-scans t
 compilation-window-height 15
 compilation-search-path '(nil "build/quantel/dbg")
 compilation-scroll-output t
 compile-command "scons -D -j3 cuda_arch=sm_20 version="
 delete-old-versions t
 diff-switches "-up"
 enable-recursive-minibuffers t
 ffap-url-regexp nil		  ; don't find URLs with M-x find-file
 fill-column 78
 find-file-existing-other-name t
 find-dired-find-program "/bin/find" ; Win32 only
 inhibit-startup-message t
 initial-scratch-message ""   ; prevent the useless cruft in *scratch*
 Info-enable-edit t
 isearch-allow-scroll t
 kept-old-versions 1
 lazy-lock-minimum-size 5000
 line-number-mode t
 line-move-visual nil			; C-n go to next real line
 mark-even-if-inactive t
 mouse-drag-copy-region t ; default in emacs24 is nil; I like the old way.
 calendar-latitude 42.36831	  ; 8 Clinton St., Cambridge, MA 02139
 calendar-longitude -71.10613		; from www.MapsOnUs.com
 require-final-newline t
 next-line-add-newlines nil
 scroll-step 2
 scroll-conservatively 10
 search-highlight t
 smtp-local-domain "genarts.com"
 split-height-threshold (/ (frame-height) 3)
 tags-revert-without-query t
 tramp-default-method "pscp"		; for Windows; uses PuTTY
 truncate-partial-width-windows nil	; ECB needs this to avoid
					; truncating source window
					; since it's partial width.
 vc-make-backup-files t			; Make emacs backups even for
					; version-controlled files
 vc-path '("c:/bin" "c:/mingw/bin")
 version-control t
 visible-bell t
 )

; Emulate 3rd button.  Don't know why wheel doesn't work as button2 on blur.
;(setq w32-num-mouse-buttons 2)

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Initial default directory
(if (file-exists-p "c:/genarts/sapphire/SConstruct")
  (progn
    (setq default-directory "c:/genarts/sapphire")
    (cd "c:/genarts/sapphire")
  ))

(put 'narrow-to-page 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(align-to-tab-stop nil)
 '(ecb-layout-name "left1")
 '(ecb-layout-window-sizes (quote (("left1" (0.2698412698412698 . 0.30158730158730157) (0.12698412698412698 . 0.31746031746031744) (0.14285714285714285 . 0.31746031746031744) (0.2698412698412698 . 0.31746031746031744)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 30)
 '(egg-buffer-hide-section-type-on-start nil)
 '(egg-cmd-select-special-buffer t)
 '(egg-commit-box-chars [9608])
 '(egg-commit-file-select-mark 10004)
 '(egg-enable-tooltip t)
 '(egg-log-HEAD-max-len 50)
 '(egg-log-all-max-len 500)
 '(egg-log-buffer-marks [10004 9998 46 9733 62])
 '(egg-log-graph-chars [9608 124 45 47 92])
 '(egg-quit-window-actions (quote ((egg-status-buffer-mode kill restore-windows))))
 '(ido-auto-merge-delay-time 10)
 '(ido-enable-flex-matching t)
 '(ido-use-filename-at-point (quote guess))
 '(inferior-octave-program "c:/Octave/3.2.4_gcc-4.4.0/bin/octave")
 '(org-babel-load-languages (quote ((emacs-lisp . t) (R . t) (python . t) (dot . t) (ditaa . t) (latex . t) (sql . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-export-latex-hyperref-format "Sec. \\ref{%s} (%s)")
 '(org-export-odt-preferred-output-format "docx")
 '(org-export-taskjuggler-default-reports (quote ("taskreport \"Gantt Chart\" {
  headline \"Project Gantt Chart\"
  columns hierarchindex, name, start, end, effort, duration, completed, chart
  timeformat \"%Y-%m-%d\"
  hideresource 1
  formats html
  loadunit shortauto
}" "resourcereport \"Resource Graph\" {
  headline \"Resource Allocation Graph\"
  columns no, name, effortleft, freetime, chart
  loadunit shortauto
  formats html
  hidetask ~isleaf()
}")))
 '(org-export-taskjuggler-target-version 3.0)
 '(org-export-with-LaTeX-fragments (quote dvipng))
 '(org-export-with-toc nil)
 '(org-latex-listings t)
 '(org-latex-packages-alist (quote (("cm" "fullpage" nil) ("compact" "titlesec" nil) ("" "paralist" nil) ("" "color" nil))))
 '(org-list-allow-alphabetical t)
 '(org-src-fontify-natively t)
 '(org-startup-folded nil)
 '(org-startup-indented nil)
 '(ps-font-size (quote (7 . 10)))
 '(ps-paper-type (quote letter))
 '(py-python-command "c:/python25/python")
 '(recentf-exclude (quote ("semantic.cache" "\\.completions" "\\.projects\\.ede" "\\.ido\\.last" ".tmp.babel-")))
 '(recentf-max-menu-items 30)
 '(recentf-max-saved-items 50)
 '(rng-nxml-auto-validate-flag t)
 '(safe-local-variable-values (quote ((Mode . C++) (Mode . C) (test-case-name . twisted\.test\.test_protocols) (Mode . c++) (Mode . python) (Mode . perl) (Mode . cperl) (comment-new_column . 0))))
 '(speedbar-tag-hierarchy-method (quote (speedbar-prefix-group-tag-hierarchy speedbar-trim-words-tag-hierarchy speedbar-sort-tag-hierarchy)))
 '(taskjuggler-command "tj3")
 '(vc-dired-recurse nil)
 '(w32-get-true-file-attributes nil t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
