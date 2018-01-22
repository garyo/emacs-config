;;; -*-mode: emacs-lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gary's .emacs file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

;;; Like normal-top-level-add-subdirs-to-load-path except it doesn't recurse.

(setq user-full-name "Gary Oberbrunner"
      user-mail-address "garyo@darkstarsystems.com")

(defun add-dir-and-subdirs-to-load-path (dir)
  "Add DIR and its subdirs to \"load-path\"."
  (if (file-directory-p dir)
      (progn
	(add-to-list 'load-path dir)
	(dolist (f (directory-files dir))
	  (let ((name (concat dir "/" f)))
	    (when (and (file-directory-p name)
		       (string-match "\\`[[:alnum:]]" f)
		       (not (equal f ".."))
		       (not (equal f ".")))
	      (message (concat "Adding " name " to load-path"))
	      (add-to-list 'load-path name)))))))

;; For this to work, HOME must be set properly
(add-dir-and-subdirs-to-load-path "c:/emacs/site-lisp") ;Windows only
(add-dir-and-subdirs-to-load-path "~/.emacs.d/lisp")
(add-dir-and-subdirs-to-load-path "~/.emacs.d/lisp/magit/lisp") ; special case

(require 'server)
(unless (server-running-p)
  (server-start))

;;; try to load libname (string); returns t or nil.
(defun maybe-load-library (libname)
  "Try to load LIBNAME, ignore errors."
   (condition-case nil
       (load-library libname)
     (error nil)))

(defun maybe-require (feature)
  "Try to require FEATURE (symbol); return feature or nil."
  (require feature nil t))

;;; Set up package system
(condition-case error
    (progn
      (require 'package)
      (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			       ;;("marmalade" . "http://marmalade-repo.org/packages/")
			       ("melpa" . "http://melpa.org/packages/")))
      )
  ('error (message "No 'package' package found."))
  )

;;; Meta-package system: use-package. Auto-installs and configures packages.
(eval-when-compile
  (when (not (fboundp 'use-package))
    (package-list-packages)
    (package-install 'use-package))
  (require 'use-package))

;; edit server for Chrome (browser extension):
(when (maybe-require 'edit-server)
  (setq edit-server-new-frame nil)
  (message "Starting edit server for Chrome...")
  (edit-server-start))

;; Arduino mode
(add-to-list 'load-path "~/.emacs.d/arduino-mode")
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

;;; Default frame size - could make this variable depending on display params
;;; but then it would have to go in the frame setup hook.
(setq default-frame-alist (list
			   '(top . 15)
			   '(left . 200)
			   '(width . 98)
			   '(height . 66)
			   ))

;;;; FONTS ;;;;;;
;; Notes:
;; use M-x describe-font RET to describe current font
;; C-u C-x = describes font under point (and lots of other goodies).
(defvar preferred-fonts
      '(
	;; Droid Sans Mono: quite nice.
	;; 15 pixels total height at 10 point.  Clear & crisp.
	;; (e.g. http://www.fontex.org/download/Droid-sans-mono.ttf)
	("Droid Sans Mono Dotted" . 10)
	("Droid Sans Mono" . 10)
	;; Consolas: download installer from Microsoft.
	;; Quite beautiful and renders nicely, but a little light.
	;; Pretty similar to Droid Sans Mono.
	;; The slanted verticals on the capital M annoy me a little.
	;; (16 pixels height)
	("Consolas" . 10.5)
	;; Inconsolata: lots of people like this.
	;; http://www.levien.com/type/myfonts/inconsolata.html:
	;; about same size as Consolas-10.5, but thicker and less leading
	;; (17 pixels height) and not as smooth lines.  Feels chunky.
	("Inconsolata" . 12)
	;; default
	("Courier New" . 10.5)
        ("Courier" . 10)))
(cond
 ((eq window-system 'ns) ; Mac native emacs: above fonts are too small
  (setq preferred-fonts '(("Droid Sans Mono Dotted" . 13)
			  ("Courier New" . 13)))
  ))

(defun find-first-font (fonts frame)
  "Find first font for FRAME in list; FONTS is ((name . size) ...)."
  (cl-find-if (lambda (f)
	     ;(message "Checking %s..." f)
	     (find-font (font-spec :family (car f)) frame))
	   fonts))

;;; Note: display-graphic-p returns false when emacs is started in daemon mode,
;;; so we do much of the frame setup in the new-frame-setup hook, which is called
;;; after the new frame is created but before it's selected. That means we have to
;;; use 'frame' everywhere here, not assume selected-frame is valid.
(defun new-frame-setup (frame)
  "Set default font and frame attributes for FRAME."
  (when (display-graphic-p frame)
    (message "Setting up new graphic frame")
    (let ((font-info (find-first-font preferred-fonts frame)))
      (when font-info
	(let ((font (find-font (font-spec :family (car font-info)
					 :size (float (cdr font-info)))
			      frame)))
	  (message "Using font %s: %s" font-info font)
	  (set-face-attribute 'default frame :font font)
	  ;(add-to-list 'default-frame-alist `(font . ,font))
	  )))
    (tool-bar-mode 0))
  )
;;; run on existing frames (non-daemon startup)
(mapc 'new-frame-setup (frame-list))
;;; run when new frames created (daemon or server)
(add-hook 'after-make-frame-functions 'new-frame-setup)

(use-package company
  :ensure t
  :demand
  :bind (("M-RET" . company-complete))
  :config
  (global-company-mode)
  ;; dabbrev mode seems closest to TMC completion
  (setq company-backends '(company-semantic company-dabbrev-code company-dabbrev company-etags company-keywords))
  (setq company-dabbrev-downcase nil	 ;make case-sensitive
	company-dabbrev-ignore-case nil) ;make case-sensitive
)

(use-package company-statistics
  :ensure t
  :after company
  :config
  (add-hook 'after-init-hook
	    'company-statistics-mode)
  )

;; Gnu Global tags
;; package-install ggtags
(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1))))
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; wgrep-change-to-wgrep-mode to edit right in a grep buffer, C-c C-e to apply.  Nice!
(use-package wgrep
  :ensure t)

(use-package gitconfig-mode
  :ensure t)
(use-package gitignore-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x v =" . magit-status)
	 ("C-x v l" . magit-log-current))
  :config
  ;; Without this, magit-show-refs-popup ('y') is very slow, late 2014
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (add-hook 'magit-status-mode-hook 'delete-other-windows)
  )

(use-package mo-git-blame
  :ensure t
  :commands (mo-git-blame-file mo-git-blame-current)
  )

;;; Trying this. Use tabs to express indentation, spaces for
;;; alignment. It's a little odd but may be good, who knows. It sets
;;; tab-width locally to indent though, so people with tab-width set
;;; to 8 will see very deep indentation.
;;;;; NO, too weird for 2018 unless it could auto-guess. Just use spaces via indent-tabs-mode=nil.
;;;(use-package smart-tabs-mode
;;;  :ensure t
;;;  :config (smart-tabs-insinuate 'c 'c++ 'python))

;;; Temporarily highlight undo, yank, find-tag and a few other things
(use-package volatile-highlights
  :ensure t
  :config (volatile-highlights-mode t))

;;; This sets $PATH and exec-path by querying the shell.
;;; Much better than trying to keep them in sync as above.
;;; Only Mac for now, but could this be useful on Windows? Probably.
;;; Also can copy other env vars, see exec-path-from-shell-copy-env.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package org
  :ensure t)
(use-package ob-sql-mode
  :ensure t
  :after org)
;; (use-package ox-tufte
;;   :ensure t
;;   :after org)

;; string manipulation routines
(use-package s
  :ensure t)
;; better visual paren matching
(use-package mic-paren
  :config
  (paren-activate)
  (add-hook 'c-mode-common-hook
            (function (lambda ()
                        (paren-toggle-open-paren-context 1))))
  )

(winner-mode 1)	; restore window config w/ C-c left (C-c right to redo)

;;; windmove: shift+arrow keys to move between windows.
;;; Should be available since emacs 21.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;;; save/restore window configs to disk automatically
;;; (this is a little weird on Mac with emacs 25.3)
;(desktop-save-mode t)

;;; Prefer utf-8 coding system everywhere
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; use zsh or bash.  Do this early on before loading any git stuff,
;; otherwise that will try to use cmdproxy.exe.
(cond ((file-exists-p "c:/msys64/usr/bin/zsh.exe")
       (setq explicit-shell-file-name "c:/msys64/usr/bin/zsh.exe"))
      ((executable-find "zsh")
       (setq explicit-shell-file-name "zsh"))
      ((executable-find "bash")
       (setq explicit-shell-file-name "bash"))
      (t nil))

(setq shell-file-name explicit-shell-file-name)

;;; Set up f7 to start or switch to shell.
;;; Repeat presses switch to next shell buffer.
;;; Would be nice if it worked with eshell.
(defun sh-buf-filter (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
(defun shell-dwim (&optional create)
  "Start or switch to an inferior shell process, in a smart way.
  If a buffer with a running shell process exists, simply switch
  to that buffer.  If a shell buffer exists, but the shell
  process is not running, restart the shell.  If already in an
  active shell buffer, switch to the next one, if any.  With
  prefix argument CREATE always start a new
  shell."
  (interactive "P")
  (let ((next-shell-buffer) (buffer)
	(shell-buf-list (identity ;;used to be reverse
			 (sort
			  (sh-buf-filter (lambda (x) (string-match "^\\*shell\\*" (buffer-name x))) (buffer-list))
			  #'(lambda (a b) (string< (buffer-name a) (buffer-name b)))))))
    (setq next-shell-buffer
	  (if (string-match "^\\*shell\\*" (buffer-name buffer))
	      (get-buffer (cadr (member (buffer-name) (mapcar (function buffer-name) (append shell-buf-list shell-buf-list)))))
	    nil))
    (setq buffer
	  (if create
	      (generate-new-buffer-name "*shell*")
	    next-shell-buffer))
    (shell buffer)
    ))
(global-set-key [f7] 'shell-dwim)
(global-set-key [f8] 'eshell)

; Dirtrack mode in shell buffers; finds prompts with dir name
; which should be better with msys2/cygwin where I can emit a
; Windows-style dir name in the prompt.
(require 'dirtrack)
(add-hook 'shell-mode-hook
          #'(lambda ()
	      (setq dirtrack-list '("(\\(.*?\\)\\( \\|) \\)" 1 t))
              (dirtrack-mode 1)))

(recentf-mode t)
(if (> emacs-major-version 22)
    (progn
      (visual-line-mode nil) ; next-line go to real next line, see also line-move-visual
      (global-visual-line-mode 0)
      (setq line-move-visual nil)))

(autoload 'vc-git-grep "vc-git" nil t)

;; (maybe-require 'git-emacs-autoloads) ; an emacs GIT interface (one of many); try M-x git-status
;; (maybe-require 'git-emacs)	     ; Provides M-x gitk to run gitk
;; (maybe-require 'git-status)
;; (maybe-require 'egg) ; another emacs GIT interface; try M-x egg-log or egg-status

(add-to-list 'exec-path "c:/Program Files/GnuGlobal/bin") ; for Git
(add-to-list 'exec-path "c:/Program Files (x86)/Git/cmd") ; for Git
(add-to-list 'exec-path "c:/msys64/usr/bin") ; for Git (msys2)
(add-to-list 'exec-path "c:/msys64/usr/local/bin") ; for GNU global/gtags
(cond ((eq system-type 'windows-nt)
       (setenv "PATH" (concat "c:/msys64/usr/local/bin;" (getenv "PATH")))
       (setenv "PATH" (concat "/usr/local/bin;" (getenv "PATH")))))
; (add-to-list 'exec-path "c:/Program Files/TortoiseHg") ; for Hg/Mercurial

(add-to-list 'exec-path "c:/bin")
(add-to-list 'exec-path "c:/bin2")
(add-to-list 'exec-path "c:/Program Files/R/R-2.14.0/bin") ; for R (statistics pkg)
;;; Use python-shell-interpreter to set python to run from emacs, not python-command
;;; NO:(setq-default python-command (or (executable-find "python") "c:/Python27/python"))

(defun copyright-for-skel (comment-start comment-end)
  "Skeleton for corporate copyright in a comment, using COMMENT-START and COMMENT-END."
  (s-format
   (concat "${cs}----------------------------------------------------------------------${ce}\n"
	   "${cs} (c) Copyright " (substring (current-time-string) -4) ", Dark Star Systems, Inc.  All rights reserved.    ${ce}\n"
	   "${cs} This file may contain proprietary and confidential information.	${ce}\n"
	   "${cs} DO NOT COPY or distribute in any form without prior written consent. ${ce}\n"
	   "${cs}----------------------------------------------------------------------${ce}\n")
   'aget `(("cs" . ,comment-start) ("ce" . ,comment-end)))
  )

(define-skeleton cxx-skeleton
  "Default C/C++ file skeleton"
  ""
  (copyright-for-skel "/*" "*/")
  "\n"
  > _ \n
  "\n"
  "/* end of " (file-name-nondirectory (buffer-file-name)) " */" > \n)

(define-skeleton h-skeleton
  "Default C/C++ header file skeleton"
  ""
  '(setq h-guard-name
	 (replace-regexp-in-string "-" "_" (upcase (file-name-base (buffer-file-name)))))
  (copyright-for-skel "/*" "*/")
  "\n"
  "#ifndef __" h-guard-name "_H__" \n
  "#define __" h-guard-name "_H__" \n
  "\n"
  > _ \n
  "\n"
  "#endif /*__" h-guard-name "_H__ */" \n
  "/* end of " (file-name-nondirectory (buffer-file-name)) " */" > \n)

(define-skeleton sh-skeleton
  "Default shell file skeleton"
  ""
  "#! /bin/sh" \n
  "\n"
  (copyright-for-skel "#" "")
  "\n"
  > _ \n
  "\n"
  "\n"
  "# end of " (file-name-nondirectory (buffer-file-name)) \n
  )

(define-skeleton py-skeleton
  "Default Python file skeleton"
  ""
  "#! /usr/bin/env python" \n
  "\n"
  (copyright-for-skel "#" "")
  "\n"
  > _ \n
  "\n"
  "\n"
  "# end of " (file-name-nondirectory (buffer-file-name)) \n
  )

(auto-insert-mode)
(setq-default auto-insert-alist
	      '((("\\.\\(CC?\\|cc\\|c\\|cxx\\|cpp\\|c++\\)\\'" . "C/C++ skeleton")
		 . cxx-skeleton)
		(("\\.\\(HH?\\|hh\\|h\\|hxx\\|hpp\\|h++\\)\\'" . "C/C++ header skeleton")
		 . h-skeleton)
		(("\\.\\(sh\\)\\'" . "Shell script skeleton")
		 . sh-skeleton)
		(("\\.\\(py\\)\\'" . "Python script skeleton")
		 . py-skeleton)
		)
      )

(maybe-require 'filladapt)			;adaptive fill mode
(setq-default filladapt-mode t)
(setq-default cache-long-scans t) ; speed up redisplay with very long lines, e.g. compilation buffers

(autoload 'taskjuggler-mode "taskjuggler-mode" "TaskJuggler mode." t)

(ignore-errors
 (load-file "~/.emacs-orgmode")
)

;; Emacs Speaks Statistics (for R):
(if (maybe-require 'ess-site)
    (ess-toggle-underscore nil)		; no annoying magic underscore
  nil)

;; use M-x idb to run the Intel debugger inside emacs (looks like 'dbx')
(setq idbpath "c:/Program Files/Intel/IDB/10.0/IA32/Bin")
(if (file-readable-p (concat idbpath "/idb.el"))
    (progn (load-file (concat idbpath "/idb.el"))
	   (add-to-list 'exec-path idbpath))
  )

;;; Menu bar takes up space, and can sometimes hang emacs on Windows (Feb 2017):
;(menu-bar-mode -1)
(blink-cursor-mode -1)	;this is annoying
;;(mouse-avoidance-mode 'animate)
(global-font-lock-mode 1)

;; Set up faces.
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

;;; Automatically revert files that change on disk
;;; (but only when the buffer is unmodified, so it's safe)
(global-auto-revert-mode t)

;;; Save all backup(~) files and auto-save files in /tmp
;;; This keeps clutter down.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq auto-mode-alist (cons '("\\.pl\\'" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SCons\\(truct\\|cript\\)\\'" . python-mode) auto-mode-alist))
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq-default visual-basic-mode-indent 4)
(setq auto-mode-alist (cons '("\\(\\.vb\\|\\.bas\\)\\'" . visual-basic-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cu$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cp$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tjp$" . taskjuggler-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
;;; .h files: interpret as C++ (for namespace etc.)
(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))

;; Like vc-git-grep from Emacs 25, but without the semi-useless "files" arg.
(defun git-grep (regexp &optional dir)
  "Run git grep, searching for REGEXP in directory DIR.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While git grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
				   nil nil 'grep-history)
	     nil))
      (t (let* ((regexp (grep-read-regexp))
		(dir (read-directory-name "In directory: "
					  (vc-git-root default-directory) nil t)))
	   (list regexp dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (progn
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(setq command
	      (grep-expand-template "git --no-pager grep -n -e <R>"
                                    regexp))
	(when command
	  (if (equal current-prefix-arg '(4))
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-history))
	    (add-to-history 'grep-history command))))
      (when command
	(let ((default-directory dir)
	      (compilation-environment (cons "PAGER=" compilation-environment)))
	  ;; Setting process-setup-function makes exit-message-function work
	  ;; even when async processes aren't supported.
	  (compilation-start command 'grep-mode))
	(if (eq next-error-last-buffer (current-buffer))
	    (setq default-directory dir))))))

;;; CEDET: emacs tools for C development
;; (ignore-errors
;;   (load-file "/emacs/site-lisp/cedet-trunk/cedet-devel-load.el")

;;   (add-to-list 'load-path
;; 	       (expand-file-name "/emacs/site-lisp/cedet-trunk/contrib"))
;;   (require 'semantic-tag-folding)
;;   ;; Add further minor-modes to be enabled by semantic-mode.
;;   ;; See doc-string of `semantic-default-submodes' for other things
;;   ;; you can use here.
;;   (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
;;   (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
;;   (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;;   ;; Enable Semantic
;;   (semantic-mode 1)

;;   ;; Enable EDE (Project Management) features
;;   (global-ede-mode 1)

;;   (which-function-mode 1)		;show func name in modeline

;;   ;; (require 'semanticdb)			;save parse tree to file
;;   ;; (global-semanticdb-minor-mode 1)
;;   ;; (semantic-load-enable-minimum-features)
;;   ;; ;;(semantic-idle-summary-mode) ; show doc for token at point in modeline
;;   ;; (semantic-load-enable-code-helpers)
;;   ;; (semantic-load-enable-code-helpers) ; Enable prototype help and smart completion
;;   ;; ;;(semantic-idle-completions-mode)	;may be too weird?
;;   ;; ;;(global-semantic-stickyfunc-mode 1)     ;header line in buffer w/ current func's header


;;   ;; to get Intellisense completion:
;; 					;(global-ede-mode 1)		; Enable the Project management system
;;   ;; then in C mode, turn on global-semantic-idle-completions-mode

;;   ;; (defun my-semantic-hook ()
;;   ;;   "Don't parse really large buffers"
;;   ;;   (cond ((string-match "^/usr/include" (buffer-file-name))
;;   ;; 	   nil)
;;   ;; 	  ((string-match "^/Progra" (buffer-file-name))
;;   ;; 	   nil)
;;   ;; 	  ((string-match "^c:/Progra" (buffer-file-name))
;;   ;; 	   nil)
;;   ;; 	  ((> (point-max) 1000000)
;;   ;; 	   nil)
;;   ;; 	  ;; only parse C and h files
;;   ;; 	  ((string-match "\\(\\.c\\|\\.cxx\\|\\.cpp\\|\\.h\\)$" (buffer-file-name))
;;   ;; 					;(message (concat "my-semantic-hook: OK to parse " (buffer-file-name)))
;;   ;; 	   t)
;;   ;; 	  (t
;;   ;; 					; (message (concat "my-semantic-hook: unknown file: " (buffer-file-name)))
;;   ;; 	   t)))
;;   ;; (add-hook 'semantic--before-fetch-tags-hook
;;   ;; 	    'my-semantic-hook)

;;   ;; ECB: Emacs Code Browser
;; 					;(add-to-list 'load-path "c:/Program Files/emacs/site-lisp/ecb-2.40")
;; 					;(require 'ecb)
;;   )

;; (ignore-errors
;;   (require 'ede-load)
;;   (global-ede-mode t)
;;   (ede-cpp-root-project "Sapphire"
;; 			:name "GenArts Sapphire"
;; 			:file "/genarts/sapphire/SConstruct"
;; 			:include-path '("/")
;; 			:spp-table '(("WIN32" . "1")
;; 				     ("_WIN32" . "1")))
;;   (custom-set-variables
;;    '(global-semantic-stickyfunc-mode t nil (semantic-util-modes))
;;    '(semantic-default-c-path (quote ("c:/genarts/sapphire")))
;;    )
;;   )


;; Printing via GhostScript/GhostView
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
  "C style for Gary Oberbrunner."
  (setq-default c-basic-offset 2
		c-hanging-comment-ender-p nil
		c-hanging-comment-start-p nil)
  ;; Labels offset by 1 from parent, but keep case stmts
  ;; offset by c-basic-offset.
  (c-set-offset 'label 1)
  (c-set-offset 'case-label 1)
  (c-set-offset 'innamespace 0)		;don't indent in namespaces
  (c-set-offset 'inextern-lang 0)	;don't indent in extern "C"
  (c-set-offset 'inlambda 0)	; lambdas don't need any extra indent
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
  (setq-default c-hanging-braces-alist
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
	     (setq-default c-basic-offset 4)
	     (local-set-key "\C-cc" 'compile)
	     )))

;; always hilight XXX in programming modes
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<XXX\\|TODO\\>" 0 font-lock-warning-face prepend)
	   )))
      '(c-mode c++-mode java-mode lisp-mode emacs-lisp-mode python-mode))

;;----------------------------------------------------------------------------
;; Programming commands (from ElijahDaniel.emacs)
;;----------------------------------------------------------------------------
;; If point is in a class definition, return the name of the class. Otherwise,
;; return nil.
(defun ewd-classname ()
  "If the point is in a class definition, get the name of the class.

  Return nil otherwise."
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
  "Call when editing a file in a buffer.

  Open windows explorer in the current directory and select the current file"
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))
(global-set-key [f12] 'open-folder-in-explorer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-error-filename (filename &optional spec-directory)
  "Process compile errors from FILENAME, looking for sources in SPEC-DIRECTORY.

  SCons (with -D) starts builds from the top of the source tree,
  and it builds into an 'SBuild' subdir. But we want to find the
  original errors in the regular source dir, regardless of the
  current directory when we run \\[compile]. Note
  \"default-directory\" may not be what you expect here, and the
  filenames are absolute, so need to remove surgically."

  (let ((case-fold-search t)
	(topdir (svn-base-dir nil))
	)
    ;; prepend dir
    (if (and spec-directory
	     (not (file-name-absolute-p filename)))
	(setq filename (concat spec-directory "/" filename)))

    (setq f (strip-sbuild (fix-win-path filename)))
		(message (format "In process-error-filename: %s in %s -> %s" filename spec-directory f))
    (cond ((file-exists-p f)
	   f)
	  ((file-exists-p (concat topdir f))
	   (concat topdir f))
	  (t filename))))

;;; Move up the directory hierarchy from dir (nil for default-directory)
;;; until we find the top of the SVN working dir
(defun svn-base-dir (dir)
  "Find Subversion top dir, starting from DIR."
  (or (cdr (file-find-upwards dir ".git"))
      (cdr (file-find-upwards dir ".hg"))
      (cdr (file-find-upwards dir ".svn"))
      (cdr (file-find-upwards dir "primitives")) ; Sapphire only!
      dir
      ))


(defun fix-win-path (p)
  "Convert backslashes to forward slashes in P so path-handling functions don't get confused."
  (cond (p (replace-regexp-in-string "\\\\" "/" p)))
  )

(defun strip-sbuild (p)
  "Strip Sbuild dirs from a pathname P."
  (replace-regexp-in-string
   "[Ss]?[Bb]uild/.*\\(final\\|release\\|dbg\\|debug\\)[^/]*/\\(mocha-[^/]*\\)?" "" p)
)

;;; found this on the web.  Useful!
;;; Modified to return (dir . file)
;;; pass nil for startfile to use default-directory.
(defun file-find-upwards (startfile file-name)
  "Chase links in the source file and search in the dir where it points."
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
(add-hook 'compilation-mode-hook (lambda () (line-number-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(setq ibuffer-formats '((mark modified read-only " " (name 16 16) " "
			      (size 6 -1 :right) " " (mode 16 16 :center)
			      " " (process 8 -1) " " filename)
			(mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")
(require 'ibuffer)

;;; Interactive buffer switching using minibuffer substring completion
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

(add-hook 'text-mode-hook
	  (lambda ()
	    (auto-fill-mode)))

(global-set-key "\C-z" 'scroll-up-line) ; use emacs24 builtins
(global-set-key "\M-z" 'scroll-down-line)
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

;;; We use .cp for C source files, but emacs ignores them by default.
(setq completion-ignored-extensions
      (remove nil
	      (remove ".log"
		      (remove ".cp" completion-ignored-extensions))))

(setq
 backup-by-copying-when-linked t
 font-lock-maximum-decoration t
 compilation-window-height 15
 compilation-scroll-output t
 compile-command "scons -D -j8 variants="
 delete-old-versions t
 diff-switches "-up"
 egg-switch-to-buffer t
 enable-recursive-minibuffers t
 fill-column 78
 find-file-existing-other-name t
 find-dired-find-program "/bin/find" ; Win32 only
 inhibit-startup-message t
 initial-scratch-message ""   ; prevent the useless cruft in *scratch*
 Info-enable-edit t
 isearch-allow-scroll t
 kept-old-versions 1
 lazy-lock-minimum-size 5000
 line-number-mode t			; XXX: disable in compilation-mode buffers
 line-move-visual nil			; C-n go to next real line
 mark-even-if-inactive t
 mouse-drag-copy-region t ; default in emacs24 is nil; I like the old way.
 require-final-newline t
 next-line-add-newlines nil
 scroll-step 2
 scroll-conservatively 10
 search-highlight t
 split-height-threshold (/ (frame-height) 2)
 tags-revert-without-query t
 tramp-default-method "pscp"		; for Windows; uses PuTTY
 truncate-partial-width-windows nil	; ECB needs this to avoid
					; truncating source window
					; since it's partial width.
 vc-make-backup-files t			; Make emacs backups even for
					; version-controlled files
 version-control t
 ; visible-bell hangs Windows emacs, early 2017
 ; visible-bell t
 )

;;; Initial default directory
(if (file-exists-p "c:/dss/Product")
  (progn
    (setq default-directory "c:/dss/Product")
    (cd default-directory)
  ))

;;; This is very important to speed up display of long lines.
;;; It's not perfect but it should help.
(setq-default bidi-display-reordering nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(align-to-tab-stop nil)
 '(company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\)")
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode def-effects-mode)))
 '(ecb-layout-name "left1")
 '(ecb-layout-window-sizes
   (quote
    (("left1"
      (0.2698412698412698 . 0.30158730158730157)
      (0.12698412698412698 . 0.31746031746031744)
      (0.14285714285714285 . 0.31746031746031744)
      (0.2698412698412698 . 0.31746031746031744)))))
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
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(git-commit-summary-max-length 64)
 '(ido-auto-merge-delay-time 10)
 '(ido-enable-flex-matching t)
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(inferior-octave-program "c:/Octave/3.2.4_gcc-4.4.0/bin/octave")
 '(magit-backup-mode nil)
 '(magit-cygwin-mount-points (quote (("/c" . "c:"))))
 '(magit-diff-expansion-threshold 999.0)
 '(magit-diff-refine-hunk t)
 '(magit-expand-staged-on-commit (quote full))
 '(magit-log-format-graph-function (quote magit-log-format-unicode-graph))
 '(magit-log-format-unicode-graph-alist (quote ((47 . 9585) (92 . 9586) (42 . 9642))))
 '(magit-pull-arguments (quote ("--rebase")))
 '(magit-refresh-status-buffer nil)
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (R . t)
     (python . t)
     (dot . t)
     (ditaa . t)
     (latex . t)
     (sql . t)
     (sh . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-export-backends (quote (ascii html icalendar latex odt koma-letter)))
 '(org-export-coding-system (quote utf-8))
 '(org-export-with-sub-superscripts (quote {}))
 '(org-export-with-toc nil)
 '(org-latex-listings t)
 '(org-latex-packages-alist
   (quote
    (("cm" "fullpage" nil)
     ("compact" "titlesec" nil)
     ("" "paralist" nil)
     ("" "enumitem" nil)
     ("" "color" nil)
     ("" "tabularx" nil)
     ("" "enumitem" nil))))
 '(org-list-allow-alphabetical t)
 '(org-odt-convert-processes
   (quote
    (("LibreOffice" "\"c:/Program Files (x86)/LibreOffice 5/program/soffice\" --headless --convert-to %f%x --outdir %d %i")
     ("unoconv" "unoconv -f %f -o %d %i"))))
 '(org-odt-preferred-output-format "docx")
 '(org-src-fontify-natively t)
 '(org-startup-folded nil)
 '(org-startup-indented nil)
 '(org-table-convert-region-max-lines 9999)
 '(org-use-speed-commands t)
 '(org-use-sub-superscripts (quote {}))
 '(package-selected-packages
   (quote
    (mic-paren s volatile-highlights smart-tabs-mode smart-tabs mo-git-blame use-package flycheck gitconfig-mode gitignore-mode ox-tufte ob-sql-mode org exec-path-from-shell ggtags company-statistics magit company wgrep)))
 '(ps-font-size (quote (7 . 10)))
 '(ps-paper-type (quote letter))
 '(py-python-command "c:/python27/python")
 '(recentf-exclude
   (quote
    ("semantic.cache" "\\.completions" "\\.projects\\.ede" "\\.ido\\.last" ".tmp.babel-")))
 '(recentf-max-menu-items 30)
 '(recentf-max-saved-items 50)
 '(rng-nxml-auto-validate-flag t)
 '(safe-local-variable-values
   (quote
    ((Mode . C++)
     (Mode . C)
     (test-case-name . twisted\.test\.test_protocols)
     (Mode . c++)
     (Mode . python)
     (Mode . perl)
     (Mode . cperl)
     (comment-new_column . 0))))
 '(same-window-regexps (quote ("\\*shell.*\\*\\(\\|<[0-9]+>\\)")))
 '(sentence-end-double-space nil)
 '(speedbar-tag-hierarchy-method
   (quote
    (speedbar-prefix-group-tag-hierarchy speedbar-trim-words-tag-hierarchy speedbar-sort-tag-hierarchy)))
 '(taskjuggler-command "tj3")
 '(vc-dired-recurse nil)
 '(visible-bell t)
 '(w32-get-true-file-attributes nil t)
 '(warning-suppress-types (quote ((\(undo\ discard-info\)))))
 '(whitespace-style
   (quote
    (face trailing tabs spaces newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t (:background "floral white"))))
 '(magit-section-highlight ((t (:background "floral white")))))

(put 'set-goal-column 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'emacs)
;;; emacs ends here
