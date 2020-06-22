;;; -*-mode: emacs-lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gary's .emacs file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;; Performance timing for loading .emacs
(defvar gco-timer (float-time))
(defun print-elapsed-time (loc)
  "Show elapsed time since last call, at LOC (string)."
    (let* ((now (float-time))
           (elapsed (- now gco-timer)))
      (setq gco-timer now)
      (message "Elapsed time at %s: %s sec" loc elapsed)))

;; Prefer .el file if newer than .elc
;(setq load-prefer-newer t)

(setq user-full-name "Gary Oberbrunner"
      user-mail-address "garyo@darkstarsystems.com")

;; don't GC after every 800k; only when idle.
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 10 t (lambda () (garbage-collect)))

(defvar msys-root
  (cond ((file-exists-p "c:/tools/msys64/msys64")
         "c:/tools/msys64/msys64")
        ((file-exists-p "c:/tools/msys64")
         "c:/tools/msys64")
        (t
         "NO_MSYS"))
  "Root of Msys64 install; should contain e.g. usr/bin/zsh.exe")

(defmacro msys-path (file)
  "Path within msys dir of FILE. FILE should be relative (no leading /)."
  `(expand-file-name ,file msys-root))

(if (eq system-type 'windows-nt)
    (push (msys-path "usr/bin") exec-path) ; for msys/linux "find", needed by straight.el
  )

;;; Set up package system -- straight.el (better than built-in package.el)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Meta-package system: use-package. Auto-installs and configures packages.
(straight-use-package 'use-package)
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t) ; make use-package use straight
(defvar straight-check-for-modifications)
(setq straight-check-for-modifications
      '(check-on-save find-when-checking))

;;;; FONTS ;;;;;;
;; Notes:
;; use M-x describe-font RET to describe current font
;; C-u C-x = describes font under point (and lots of other goodies).
;; To list all fonts, in *scratch* buffer do (print (font-family-list))
;; To test a font, use Options menu -> Set Default Font...
(defvar preferred-fonts
      '(
        ("Hack" . 10)                   ; my new fave as of 2019 (very similar to DV Sans Mono)
	("DejaVu Sans Mono" . 10)       ; better ~ than Droid Sans Dotted Mono
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
  (setq preferred-fonts '(("Hack" . 13)
                          ("DejaVu Sans Mono" . 13)
                          ("Droid Sans Mono Dotted" . 13)
			  ("Courier New" . 13)))
  ))

(defun find-first-font (fonts &optional frame)
  "Find first font for FRAME (nil for current) in list; FONTS is ((name . size) ...)."
  (cl-find-if (lambda (f)
	        (message "Checking %s..." f)
	        (find-font (font-spec :family (car f)) frame))
	      fonts))

;;; Note: display-graphic-p returns false when emacs is started in daemon mode,
;;; so we do much of the frame setup in the new-frame-setup hook, which is called
;;; after the new frame is created but before it's selected. That means we have to
;;; use 'frame' everywhere here, not assume selected-frame is valid.
;;; Note: for testing, use (selected-frame) to get the current frame.
(defun new-frame-setup (frame)
  "Set default font and frame attributes for FRAME."
  (when (display-graphic-p frame)
    (tool-bar-mode 0))
    (message "Setting up new graphic frame %s, current geom %s" frame (frame-geometry frame))
    (let ((font-info (find-first-font preferred-fonts frame)))
      (when font-info
	(let* ((font (find-font (font-spec :family (car font-info)
                                          :slant 'normal
                                          :weight 'normal
					  :size (float (cdr font-info)))
			       frame))
              )
	  (message "Using font %s: %s" font-info font)
	  (set-face-attribute 'default frame :font font)
          (set-frame-width frame 100)
          (set-frame-height frame 48)
          ;; Now using desktop-save-mode so don't bother setting frame position/size
          ;(set-frame-parameter frame 'fullscreen 'fullheight) ; full height
          ;(if (equal window-system 'ns)
          ;    (set-frame-position frame 20 0)
          ;  (set-frame-position frame -20 0)) ; negative means right- or bottom-relative
	  )))
  )
;;; run on existing frames (non-daemon startup)
(mapc 'new-frame-setup (frame-list))
;;; run when new frames created (daemon or server)
(add-hook 'after-make-frame-functions 'new-frame-setup)

(desktop-save-mode 1)

;;; I like italic comment face as long as the actual font supports it
;;; (which Hack does)
(set-face-italic font-lock-comment-face t)

;;;
;;; Packages
;;;
(defvar helm-or-ivy 'ivy
  "Whether to use Helm mode or Ivy mode for advanced completions")

(use-package company
  :bind (("M-RET" . company-complete))
  :demand                               ; load it now (better for eglot)
  :defines company-dabbrev-downcase company-dabbrev-ignore-case
  :config
  (global-company-mode)
  ;; dabbrev mode seems closest to TMC completion
  (setq company-backends '(company-capf company-semantic company-dabbrev-code
                                        company-dabbrev company-etags
                                        company-keywords))
  (setq company-dabbrev-downcase nil	 ;make case-sensitive
	company-dabbrev-ignore-case nil) ;make case-sensitive
)

(use-package company-statistics
  :after company
  :hook (after-init . company-statistics-mode)
  )

;; Gnu Global tags
(use-package ggtags
  :config
  :hook (c-mode-common . (lambda ()
	                   (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		             (ggtags-mode 1))))
  )

;; string manipulation routines
(use-package s)

(use-package flycheck-pos-tip)
(use-package flycheck
  :config
  (global-flycheck-mode)
  ;; show flycheck errors in popup, not in minimuffer. This is important
  ;; because minibuffer may be showing documentation or something else,
  ;; and without this flycheck errors/warnings overwrite that info.
  ;; (alternately, could show doc strings in popup somehow)
  (flycheck-pos-tip-mode)
  )

;;; for Windows, especially for emacs-lisp checker which passes
;;; lots of cmd-line args to emacs
(cond ((eq system-type 'windows-nt)
       (setq flycheck-command-wrapper-function
             (lambda (cmd)
               (list "bash" "-c" (format "%s"
                                         (mapconcat 'shell-quote-argument cmd " ")))))))

;;; On Windows, commands run by flycheck may have CRs (\r\n line endings).
;;; Strip them out before parsing.
(defun flycheck-parse-output (output checker buffer)
  "Parse OUTPUT from CHECKER in BUFFER.

OUTPUT is a string with the output from the checker symbol
CHECKER.  BUFFER is the buffer which was checked.

Return the errors parsed with the error patterns of CHECKER."
  (let ((sanitized-output (replace-regexp-in-string "\r" "" output))
        )
    (funcall (flycheck-checker-get checker 'error-parser) sanitized-output checker buffer)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; instant live github markdown preview in markdown mode, C-c C-c g
;; Requires 'grip', a python package (pip install grip) installed in system python
(use-package grip-mode
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

;;; * Searching

(use-package rg                         ; ripgrep: fast find, wgrep-capable
  :config
  (rg-enable-menu)          ; start w/ C-c s p, "rg-project"
  ;; rg-mode binds C-n and C-p to go to next/prev file rather than by line
  ;; which is a bit jarring.
  (define-key rg-mode-map (kbd "C-n") nil)
  (define-key rg-mode-map (kbd "C-p") nil)
  (rg-define-search rg-search-all       ; C-c s a: search all in project
    "Search all files in project with rg"
    :files "all"
    :dir project
    :menu ("Search" "a" "All in project")
    )
  )

;;; ripgrep seems better and works better on Windows, but keep this here for now:
;; M-x ag-project
(use-package ag)

;;; wgrep-change-to-wgrep-mode to edit right in a grep buffer (or ag/ripgrep)
;;; Use C-c C-e to apply.
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t)
  )

;;; Need this for wgrep to understand ag-search buffers
(use-package wgrep-ag
  :hook (ag-mode . wgrep-ag-setup)
  )

;;; M-x helm-ag: very nice for searching through files!
;;; Requires ag or rg (silver searcher or ripgrep)
(if (eq helm-or-ivy 'helm)
    (use-package helm-ag
      :config
      ;; This is configured for ripgrep; comment out to use ag
      (setq helm-ag-base-command "rg --no-heading --vimgrep --smart-case")
      ))

(use-package gitconfig-mode
  :mode "\\.gitconfig\\'")

(use-package gitignore-mode
  :mode "\\.gitignore\\'")

(use-package magit
  :bind (("C-x v =" . magit-status)
         ("C-x v b" . magit-blame)
	 ("C-x v l" . magit-log-current))
  :config
  ;; Without this, magit-show-refs-popup ('y') is very slow, late 2014
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (add-hook 'magit-status-mode-hook 'delete-other-windows)
  )

;; Show git changes in fringe
(use-package git-gutter-fringe
  :config
  (dolist (p '((git-gutter:added    . "#0c0")
               (git-gutter:deleted  . "#c00")
               (git-gutter:modified . "#df0")))
    (set-face-foreground (car p) (cdr p))
    (set-face-background (car p) (cdr p)))
  (global-git-gutter-mode)
  )

;;; Temporarily highlight undo, yank, find-tag and a few other things
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t)
  )

;;; This sets $PATH and exec-path by querying the shell.
;;; Much better than trying to keep them in sync as above.
;;; Only Mac for now, but could this be useful on Windows? Probably.
;;; Also can copy other env vars, see exec-path-from-shell-copy-env.
;(use-package exec-path-from-shell
;  :if (memq window-system '(mac ns))
;  :config
;  (exec-path-from-shell-initialize))

(use-package org
  :mode (("\\.org$" . org-mode))
  :commands org-mode
  :config
  (require 'org-mouse)
)
;; (use-package ox-tufte
;;   :after org)

;; better visual paren matching
(use-package mic-paren
  :hook (c-mode-common .
                       (lambda ()
                        (paren-toggle-open-paren-context 1)))
  :config
  (paren-activate)
  )

(use-package gdscript-mode
  :mode ("\\.gd$")
)

(use-package typescript-mode
  :mode ("\\.ts$")
  )

(use-package js2-mode
  :mode ("\\.js$")
  )

;;; Vue mode, based on mmm-mode -- set up for .vue files (html/css/script)
(use-package vue-mode
  :mode "\\.vue$"
  :config
  (setq mmm-submode-decoration-level 0) ; don't color background of sub-modes
  ;; fix for Emacs27/mmm-mode bug (as of June 2019)
  ;; without this, TAB doesn't indent in the <script> section
  ;; remove once mmm-mode has a fix for this
  ;; (see https://github.com/purcell/mmm-mode/issues/99)
  (add-to-list 'mmm-save-local-variables '(syntax-ppss-table buffer))
  ;; Fix for mmm-mode bug https://github.com/purcell/mmm-mode/issues/100
  ;; (can remove once that's fixed & released)
  (add-to-list 'mmm-save-local-variables '(c-current-comment-prefix region))
  )

;;; Yasnippet -- autocomplete various language snippets
;;; TAB expands snippet "keys" (abbrevs) and moves to next field
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  ;; This is a bit questionable: during an expansion, yasnippet normally uses
  ;; TAB to accept a field and move to the next field. But company also binds
  ;; TAB (to advance to common completion), so when a completion is in
  ;; progress _and_ it has a snippet to expand, TAB doesn't work.
  ;; So this uses a function bound to C-o to either expand an active snippet,
  ;; or else do the usual open-line.
  (global-set-key "\C-o" 'yasnippet-or-open-line)
  (defun yasnippet-or-open-line ()
    "Call `open-line', unless there are abbrevs or snippets at point.
In that case expand them.  If there's a snippet expansion in progress,
move to the next field. Call `open-line' if nothing else applies."
    (interactive)
    (cond ((expand-abbrev))
          ((yas-active-snippets)
           (yas-next-field-or-maybe-expand))
          ((ignore-errors
             (yas-expand)))
          (t
           (open-line 1))))
  )


;;; all the snippets -- this is big!
(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

;;; multiple major modes in a buffer; like multi-web-mode but more modern.
;;; polymode may be better still but as of Sept 2018 it's still being rewritten.
;; (use-package mmm-mode
;;   :config
;;   (mmm-add-mode-ext-class 'html-mode nil 'html-js) ; set up for js in html
;;   (mmm-add-mode-ext-class 'html-mode nil 'html-css) ; set up for css in html
;;   (setq mmm-global-mode 'maybe)
;;   (setq mmm-submode-decoration-level 0) ; don't color background of sub-modes
;;   (setq mmm-major-mode-preferences
;;         '((perl cperl-mode perl-mode)
;;           (python python-mode python-mode)
;;           (javascript js-mode c++-mode) ; only here because of this -- use js-mode
;;           (java jde-mode java-mode c++-mode)
;;           (css css-mode c++-mode))
;;         )
;;   )

(defun has-fast-json ()
  "Return t if \"json-serialize\" is implemented as a C function.
This was done for Emacs 27 but not all builds include the C version,
which is a lot faster."
  (and
   (subrp (symbol-function 'json-serialize))
   ;; test that it works -- on Windows the DLL (or deps) may be missing
   (equal (json-serialize (json-parse-string "[123]")) "[123]")))

(unless (has-fast-json)
  (warn "This emacs is using older elisp json functions; maybe rebuild with libjansson?"))

(defvar vls-exe
  (cond ((file-exists-p (expand-file-name "~/src/vetur/server/bin/vls"))
         (expand-file-name "~/src/vetur/server/bin/vls"))
        (t "vls"))
  "Vue Language Server path - normally just \"vls\" but can point to local install."
  )

;; May 2019: Eglot is more responsive and simpler
;; Oct 2019: lsp-mode has more features, but it's very slow
;;           unless this Emacs has the fast C json lib (libjansson).
;;           ... and even then it's super slow for me.
(defvar use-lsp-mode nil
  "T means use lsp-mode; nil means use eglot.
Always uses eglot if this Emacs doesn't have fast JSON.")

(defvar lsp-mode-verbose nil
  "Set to t to turn on lots of logging in lsp-mode.")

(defvar vls-vetur-configuration
  `(:useWorkspaceDependencies: t
    :completion
    (:autoImport t :useScaffoldSnippets t :tagCasing "kebab")
    :grammar
    (:customBlocks
     (:docs "md" :i18n "json"))
    :validation
    (:template t :style t :script t)
    :format
    (:enable t
             :options (:tabSize 2)      ; required, believe it or not
             :defaultFormatter
             (:html "prettyhtml" :css "prettier" :postcss "prettier"
                    :scss "prettier" :less "prettier"
                    :stylus "stylus-supremacy"
                    :js "prettier" :ts "prettier")
             :defaultFormatterOptions
             (:js-beautify-html
              (:wrap_attributes "force-expand-multiline")
              :prettyhtml
              (:printWidth 100 :singleQuote :json-false :wrapAttributes :json-false :sortAttributes :json-false))
             :styleInitialIndent :json-false
             :scriptInitialIndent :json-false)
    ,@(if lsp-mode-verbose
          '(:trace
            (:server "verbose")))
    :dev
    (:vlsPath "" :logLevel: "DEBUG")
    :html
    (:suggest nil)
    :prettier :json-false
    ))

(defvar vls-workspace-configuration
  `((:vetur . ,vls-vetur-configuration)
    (:html . (:suggest ()))
    (:prettier . :json-false)
    (:javascript . (:format nil :suggest nil))
    (:typescript . (:format nil :suggest nil))
    (:emmet . ())
    (:stylusSupremacy . ())
    )
  )

(eval-after-load "eglot"
  (defun my-eglot-init ()
    """Initialize eglot."""

    (defclass eglot-vls (eglot-lsp-server) ()
      :documentation "Vue Language Server.")

    (add-hook 'eglot-server-initialized-hook
              (lambda (server)
                (if (eglot-vls-p server)
                    (setq eglot-workspace-configuration vls-workspace-configuration)
                  )))

    (add-to-list 'eglot-server-programs
                 '(vue-mode . (eglot-vls . ("vls" "--stdio"))))
    (add-to-list 'eglot-server-programs
                 '((js-mode typescript-mode) . ("typescript-language-server" "--stdio" "--tsserver-log-file" "/tmp/tsserver.log")))

    (cl-defmethod eglot-initialization-options ((server eglot-vls))
      "Passes through required vetur initialization options to VLS."
      `(:config
        (:vetur ,vls-vetur-configuration
                :css (:suggest nil)
                :html (:suggest nil)
                :prettier :json-false
                :javascript (:format nil :suggest nil)
                :typescript (:format nil :suggest nil)
                :emmet nil
                :stylusSupremacy nil
                )))
    )
  )

(cond ((and use-lsp-mode (has-fast-json))
       ;; LSP mode: language server protocol for getting completions, definitions etc.
       (use-package lsp-mode
         :commands lsp
         :hook ((vue-mode . lsp)
                (typescript-mode . lsp)
                (python-mode . lsp))
         :config
         (setq lsp-prefer-flymake nil
               lsp-log-io lsp-mode-verbose
               lsp-trace lsp-mode-verbose
               lsp-print-performance t
               lsp-response-timeout 10
               lsp-vetur-server-command vls-exe ; normally just "vls"
               )
         )
       (use-package lsp-ui
         :commands lsp-ui-mode
         :hook (lsp-mode . lsp-ui-mode)
         :config
         (setq lsp-ui-doc-enable t
               lsp-ui-doc-use-childframe t
               lsp-ui-doc-position 'top
               lsp-ui-doc-include-signature t
               lsp-ui-sideline-enable nil
               lsp-ui-flycheck-enable t
               lsp-ui-flycheck-list-position 'right
               lsp-ui-flycheck-live-reporting t
               lsp-ui-peek-enable t
               lsp-ui-peek-list-width 60
               lsp-ui-peek-peek-height 25
               )
	 )
       (use-package company-lsp
         :config
         (push 'company-lsp company-backends)
         ;; Disable client-side cache because the LSP server does a better job (?)
         (setq company-lsp-async t
               company-lsp-cache-candidates nil
               company-lsp-filter-candidates ; need to filter vls and jsts-ls
               '((bingo . nil)
                 (ccls . nil)
                 (clangd . nil)
                 (cquery . nil)
                 (go-bingo . nil)
                 (gopls . nil)
                 (javacomp . nil)
                 (jdtls . nil)
                 (pyls . nil)
                 (rls . nil)
                 (jsts-ls . t)
                 (vls . t)
                 (t . t))
               company-lsp-enable-snippet t
               )
         )
       (use-package helm-lsp
         )

       (if (not (featurep 'yasnippet))
           (warn "LSP: missing yasnippet, LSP won't work well"))
       )
      (t
       (use-package jsonrpc)
       (use-package eglot
         :straight (:host github
                          :repo "joaotavora/eglot"
                          :branch "master")
         :commands eglot-ensure
         :hook ((vue-mode . eglot-ensure)
                (typescript-mode . eglot-ensure)
                (javascript-mode . eglot-ensure))
         :config
         ;; note: company-mode must be loaded already
         ;; eglot wants to replace all company backends with 'company-capf
         ;; so I make sure that's first, but keep my other backends in case
         ;; eglot doesn't have any completions (e.g. to use yasnippet or complete
         ;; in strings and comments)
         (add-to-list 'eglot-stay-out-of "company")
         (add-to-list 'company-backends  'company-capf)
         (define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)
         (my-eglot-init)
         )
       ))

;;; Eglot uses eldoc to display docs for functions
;;; Try displaying those in a child frame:
;; (use-package eldoc-box
;;   :hook (eglot--managed-mode . eldoc-box-hover-mode)
;;   :config
;;   (set-face-background 'eldoc-box-body "#ffb")
;;   )

;;; Work with python virtualenvs
;;; M-x venv-workon (has completion), M-x venv-deactivate, M-x venv-*
;;; Looks in ~/.virtualenvs
(use-package virtualenvwrapper
  )

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package origami
  :bind (("C-c f" . origami-recursively-toggle-node)
         ("C-c F" . origami-show-only-node))
  )

(defun projectile-mode-line ()
  "Report project name (only) in the modeline."
  (let ((project-name (projectile-project-name))
        (project-type (projectile-project-type)))
    (format "%s[%s]"
            projectile-mode-line-prefix
            (or project-name "-")
            )))
(use-package projectile
  :bind (("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map))
  :demand
  :config
  (projectile-mode +1)
  (setq projectile-mode-line-prefix " Prj")
  (setq projectile-mode-line-function 'projectile-mode-line)
  )

(if (eq helm-or-ivy 'helm)
    (use-package helm
      :diminish helm-mode
      :init
      (progn
        (require 'helm-config)
        (setq helm-candidate-number-limit 100)
        ;; From https://gist.github.com/antifuchs/9238468
        (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
              helm-input-idle-delay 0.01 ; this actually updates things
                                        ; reeeelatively quickly.
              helm-yas-display-key-on-candidate t
              helm-quick-update t
              helm-buffers-fuzzy-matching t
              helm-M-x-requires-pattern nil
              projectile-completion-system 'helm
              helm-ff-skip-boring-files t)
        (helm-mode))
      :config
      (setq projectile-completion-system 'helm)
      :bind (("C-h a" . helm-apropos)
             ("C-x C-b" . helm-buffers-list)
             ("C-x b" . helm-mini)
             ("M-y" . helm-show-kill-ring)
                                        ; ("M-x" . helm-M-x)
             ("C-x C-f" . helm-find-files)
             ("C-x c o" . helm-occur)
             ("C-x c SPC" . helm-all-mark-rings)))
  ;; else Ivy
  (use-package counsel
    :config
    (counsel-mode 1)
    :bind (("C-c g" . counsel-git)
           )
    )
  (use-package swiper
    ;; use Ctrl-O to switch to swiper mode within isearch
    :bind (:map isearch-mode-map
                ("C-o" . swiper-from-isearch))
    )
  (use-package ivy
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          ivy-height 20
          ivy-count-format "(%d/%d) ")
    (setq projectile-completion-system 'ivy)
    )
  (use-package ivy-hydra)               ; C-o in ivy minibuffer to start hydra
  (use-package ivy-rich
    :config
    (ivy-rich-mode 1)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    )
)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/name-width 40)
  (setq sml/mode-width 'full)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  ;; don't show these minor modes
  (setq rm-blacklist '(" hl-p" " company" " ElDoc" " VHl" " Helm" " Fill"
                       " Filladapt" " counsel" " ivy" " yas" " GitGutter"))
  (add-to-list 'sml/replacer-regexp-list
               '("c:/dss/Product/Horizon/WebProjects/horizon-project/horizon" ":HZN:"))
  (sml/setup)
  )

;; unfill fills or unfills para, toggling each time you press M-q
(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

;;; adaptive fill mode
(use-package filladapt
  ;; to enable only in certain modes:
  ;; :hook (('text-mode-hook . 'filladapt-mode))
  :config
  (setq-default filladapt-mode t))      ; turn on by default everywhere

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maybe-require (feature)
  "Try to require FEATURE (symbol); return feature or nil."
  (require feature nil t))

;; edit server for Chrome (browser extension) (but only if installed):
(when (maybe-require 'edit-server)
  (defvar edit-server-new-frame)
  (setq edit-server-new-frame nil)
  (message "Starting edit server for Chrome...")
  (edit-server-start))


(winner-mode 1)	; restore window config w/ C-c left (C-c right to redo)

;;; windmove: shift+arrow keys to move between windows.
;;; Should be available since emacs 21.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;;; save/restore window configs to disk automatically
;;; (this is a little weird on Mac with emacs 25.3)
;(desktop-save-mode t)

;;; Prefer utf-8 coding system everywhere, with LF line endings
(prefer-coding-system 'utf-8-unix)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; use zsh or bash.  Do this early on before loading any git stuff,
;; otherwise that will try to use cmdproxy.exe.
(cond ((file-exists-p (msys-path "usr/bin/zsh.exe"))
       (setq explicit-shell-file-name (msys-path "usr/bin/zsh.exe")))
      ((executable-find "zsh")
       (setq explicit-shell-file-name "zsh"))
      ((executable-find "bash")
       (setq explicit-shell-file-name "bash"))
      (t
       (message "Can't find zsh!")))

;; Setting this will make emacs use this shell for subprocesses
;; (shell-command, start-file-process, compilations, etc.)
;; Beware: on Windows with msys zsh, it'll translate paths
;; which might be what you want sometimes, but not others!
;; (so "grep /foo" will turn into "grep c:/tools/msys64/msys64/foo")
;; In that case you can double the initial slash (maybe!).
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

;;; Recentf mode: remember recently visited files
(recentf-mode t)
(setq-default
 recentf-exclude '("semantic.cache"
                   "\\.completions"
                   "\\.projects\\.ede"
                   "\\.ido\\.last"
                   "recentf"
                   "ido\\.last"
                   ".tmp.babel-")
 recentf-max-menu-items 30
 recentf-max-saved-items 50)
;; emacs doesn't save recentf list until you "exit normally"
;; which never really happens with emacs-server. So just save every 10
;; min, and do it silently.
(run-at-time nil 600 (lambda () (let ((save-silently t))
                                  (recentf-save-list))))

(if (> emacs-major-version 22)
    (progn
      (visual-line-mode nil) ; next-line go to real next line, see also line-move-visual
      (global-visual-line-mode 0)
      (setq line-move-visual nil)))

(autoload 'vc-git-root "vc-git" nil t)
(autoload 'vc-git-grep "vc-git" nil t)

(defmacro prepend-PATH (dir)
  "Prepend DIR (abs path) to PATH env var."
  `(setenv "PATH" (concat ,dir
                          path-separator
                          (getenv "PATH"))))
(defmacro prepend-PATH-msys (dir)
  "Prepend msys DIR (path rel to msys root) to PATH env var."
  `(setenv "PATH" (concat (msys-path ,dir)
                          path-separator
                          (getenv "PATH"))))

(cond ((eq system-type 'windows-nt)
       (add-to-list 'exec-path "c:/Program Files/GnuGlobal/bin") ; for Global
       (add-to-list 'exec-path "c:/Program Files (x86)/Git/cmd") ; for Git
       (add-to-list 'exec-path "c:/Program Files/Git/cmd") ; for Git
       (add-to-list 'exec-path (msys-path "mingw64/bin")) ; for "ag"
       (add-to-list 'exec-path (msys-path "usr/bin")) ; for zsh, git, etc.
       (add-to-list 'exec-path (msys-path "usr/local/bin")) ; for GNU global/gtags
       (add-to-list 'exec-path "c:/bin")
       (add-to-list 'exec-path "c:/bin2")
       (add-to-list 'exec-path "c:/ProgramData/Chocolatey/bin") ; rg, putty, etc.
       (prepend-PATH-msys "usr/local/bin")
       (prepend-PATH-msys "usr/bin")
       (prepend-PATH-msys "mingw64/bin")
       (prepend-PATH "/usr/local/bin")
       )
      (t
       (add-to-list 'exec-path "/usr/local/bin")
       (push "/Users/garyo/python36/bin" exec-path)
       (delete-dups exec-path)
       (message "exec-path: %s" exec-path)
       ;; for SCons in compilation-mode. (emacs uses exec-path for
       ;; things it execs directly, but compilation-mode runs a shell
       ;; which invokes SCons, and doesn't seem to get my path -- that
       ;; could probably be fixed.)
       (prepend-PATH "/usr/local/bin")
       (prepend-PATH "/Users/garyo/python36/bin")
       (message "PATH: %s" (getenv "PATH"))
       ))

;;; Use python-shell-interpreter to set python to run from emacs, not python-command
;;; NO:(setq-default python-command (or (executable-find "python") "c:/Python27/python"))

(defun copyright-for-skel (comment-start comment-end)
  "Skeleton for corporate copyright in a comment, using COMMENT-START and COMMENT-END."
  (s-format
   (concat "${cs} ----------------------------------------------------------------------${ce}\n"
	   "${cs} (c) Copyright " (substring (current-time-string) -4) ", Dark Star Systems, Inc.  All rights reserved.    ${ce}\n"
	   "${cs} This file may contain proprietary and confidential information.	${ce}\n"
	   "${cs} DO NOT COPY or distribute in any form without prior written consent. ${ce}\n"
	   "${cs} ----------------------------------------------------------------------${ce}\n")
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

(define-skeleton js-skeleton
  "Default Javascript file skeleton"
  ""
  (copyright-for-skel "//" "")
  "\n"
  > _ \n
  "\n"
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
		(("\\.\\(jsx?\\|vue\\|tsx?\\)\\'" . "Javascript skeleton")
		 . js-skeleton)
		)
      )

(setq-default cache-long-scans t) ; speed up redisplay with very long lines, e.g. compilation buffers

(autoload 'taskjuggler-mode "taskjuggler-mode" "TaskJuggler mode." t)

(ignore-errors
 (load-file "~/.emacs-orgmode")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org agenda setup:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-directory "~/Dropbox/Personal/org-agenda") ; inbox.org, gtd.org, tickler.org ...
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
(defun go/verify-refile-target ()
  "Exclude TODOS as refile targets."
  (not (member (nth 2 (org-heading-components)) (list "TODO" "DONE"))))
(setq org-refile-target-verify-function 'go/verify-refile-target)
;(add-hook 'auto-save-hook 'org-save-all-org-buffers)            ; autosave always
;(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers) ; autosave on quit agenda

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f9>") 'org-agenda) ; faster, one keystroke
(global-set-key (kbd "<f8>") 'org-capture) ; faster, one keystroke
(global-set-key (kbd "C-c c") 'org-capture)

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
(defvar org-capture-templates
      '(("t" "Todo [inbox]" entry
         (file+headline "inbox.org" "Tasks")
         "* TODO %i%?\n  %U"
         :prepend t)
        ("." "Today" entry
         (file+headline "inbox.org" "Tasks")
         "* TODO %^{Task}\nSCHEDULED: %t\n"
         :immediate-finish t)
        ("s" "Scheduled TODO" entry
         (file+headline "inbox.org" "Tasks") ;prompts for tags and schedule date (^G, ^t)
         "* TODO %? %^G \nSCHEDULED: %^t\n  %U")
        ("d" "Deadline" entry
         (file+headline "inbox.org" "Tasks")
         "* TODO %? %^G \n  DEADLINE: %^t"
         :empty-lines 1)
        ("w" "Work" entry
         (file+headline "gtd.org" "Work")
         "* TODO %i%?\n  %U"
         :prepend t)
        ("h" "Home" entry
         (file+headline "gtd.org" "Home")
         "* TODO %i%?\n  %U"
         :prepend t)
        ("T" "Tickler" entry
         (file+headline "tickler.org" "Tickler")
         "* TODO %i%? \n %U")
        ))
(defun gtd ()
   (interactive)
   (find-file (concat org-directory "/gtd.org")))

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

;; use M-x idb to run the Intel debugger inside emacs (looks like 'dbx')
(defvar idbpath "c:/Program Files/Intel/IDB/10.0/IA32/Bin")
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
  "Used for marking significant items in shell buffers."
  :group 'shell)
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
(defcustom delete-trailing-whitespace-on-save
  t "Delete trailing whitespace when buffer is saved."
  :type '(boolean)
  :group 'GCO)
(make-variable-buffer-local 'delete-trailing-whitespace-on-save)
(defun maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace on save, if enabled by delete-trailing-whitespace-on-save."
  (if delete-trailing-whitespace-on-save
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'maybe-delete-trailing-whitespace)
(setq-default indicate-empty-lines t)

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

(setq
 shell-pushd-regexp "pushd\\|1\\|2"
 shell-pushd-dextract t
 shell-pushd-dunique t
 ;shell-cd-regexp nil			; autopushd in zsh
 shell-chdrive-regexp "[a-z]:")		;

;;This is from Voelker's emacs NT page:
(defvar explicit-zsh-args)
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
	(topdir (car (project-roots (project-current))))
	)
    ;; prepend dir
    (if (and spec-directory
	     (not (file-name-absolute-p filename)))
	(setq filename (concat spec-directory "/" filename)))

    (let ((candidates (get_src_from_build_path (fix-win-path filename)))
          (result nil))
      (message (format "In process-error-filename: %s in %s: candidates = %s" filename spec-directory candidates))
      (dolist (f candidates)
        (cond ((file-exists-p f)
	       (setq result f))
	    ((file-exists-p (concat topdir f))
	     (setq result (concat topdir f)))))
      (if result result filename))))

(defun fix-win-path (p)
  "Convert backslashes to forward slashes in P so path-handling functions don't get confused."
  (cond (p (replace-regexp-in-string "\\\\" "/" p)))
  )

(defun get_src_from_build_path (p)
  "Strip Sbuild dirs from a pathname P."
  (list
   (replace-regexp-in-string
    "[Ss]?[Bb]uild/.*\\(final\\|release\\|dbg\\|debug\\)[^/]*/" "" p)
   (replace-regexp-in-string
    "[Ss]?[Bb]uild/.*\\(final\\|release\\|dbg\\|debug\\)[^/]*/" "src/" p)
   )
)

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

(setq ibuffer-formats '((mark modified read-only " " (name 16 16) " "
			      (size 6 -1 :right) " " (mode 16 16 :center)
			      " " (process 8 -1) " " filename)
			(mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")
(require 'ibuffer)

;;; Interactive buffer switching using minibuffer substring completion
;;(setq ido-enable-tramp-completion nil)	    ; workaround tramp bug in emacs 23.1
;;(ido-mode)

(cond ((eq system-type 'windows-nt)
       (setq tramp-use-ssh-controlmaster-options nil)))

;; make PC keyboard's Windows key be Super or Hyper (Windows only)
;; (There are a few that aren't rebindable: Win-L, Win-G at least)
;; This is nice because Super-p is the prefix for Projectile
(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key
  (w32-register-hot-key [s-]) ; disable all Windows shortcuts while Emacs has focus

  ;; I don't usually use right-windows but why not
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key

  ;; The menu/app key (to the right of the right Windows key) is
  ;; pretty hard to reach with right pinky, so it's less useful, and
  ;; only on certain keyboards, but my Das Keyboard has it, so why not.
  (setq w32-apps-modifier 'hyper) ; Menu/App key
  )
;; Mac default setup has Command (⌘, clover) = meta
;; Also set Option (⌥) to be super
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  )

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

(global-set-key "\M- " 'cycle-spacing) ; improvement over just-one-space; repeated calls cycle 1, 0, orig
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

;; This fixes the slow startup of query-replace when using Droid Sans Mono Dotted font
;; The default value of this isn't in that font I guess? (In fact, even pasting it
;; in here makes redisplay slow down!)
(setq-default query-replace-from-to-separator " -> ")

(cond ((eq system-type 'windows-nt)
       (setq
        find-dired-find-program "/bin/find"
        find-program "/bin/find"
        grep-program "/bin/grep"
        )
       ))


(setq
 backup-by-copying-when-linked t
 font-lock-maximum-decoration t
 compilation-window-height 15
 compilation-scroll-output t
 compile-command "scons -D -j8 v=debug"
 delete-old-versions t
 diff-switches "-up"
 egg-switch-to-buffer t
 enable-recursive-minibuffers t
 fill-column 78
 find-file-existing-other-name t
 inhibit-startup-message t
 initial-scratch-message ""   ; prevent the useless cruft in *scratch*
 Info-enable-edit t
 ;; isearch-allow-scroll nil  ; t means allow scroll, but prevent scrolling if would go off screen
 kept-old-versions 1
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

;;; This is very important to speed up display of long lines.
;;; It's not perfect but it should help.
(setq-default bidi-display-reordering nil)

;;; Always use '(foo) rather than (quote (foo)) in customize
;;; (custom-set-variables below)
(advice-add 'custom-save-all :around
            (lambda (orig)
              (let ((print-quoted t))
                (funcall orig))))

;;; Start emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments '("--smart-case"))
 '(ag-highlight-search t)
 '(ag-reuse-buffers t)
 '(ag-reuse-window t)
 '(align-to-tab-stop nil)
 '(company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\)")
 '(company-dabbrev-code-modes
   '(prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode def-effects-mode))
 '(custom-safe-themes
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(ecb-layout-name "left1")
 '(ecb-layout-window-sizes
   '(("left1"
      (0.2698412698412698 . 0.30158730158730157)
      (0.12698412698412698 . 0.31746031746031744)
      (0.14285714285714285 . 0.31746031746031744)
      (0.2698412698412698 . 0.31746031746031744))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 30)
 '(edebug-print-length 500)
 '(egg-buffer-hide-section-type-on-start nil)
 '(egg-cmd-select-special-buffer t)
 '(egg-commit-box-chars [9608])
 '(egg-commit-file-select-mark 10004)
 '(egg-enable-tooltip t)
 '(egg-log-HEAD-max-len 50)
 '(egg-log-all-max-len 500)
 '(egg-log-buffer-marks [10004 9998 46 9733 62])
 '(egg-log-graph-chars [9608 124 45 47 92])
 '(egg-quit-window-actions '((egg-status-buffer-mode kill restore-windows)))
 '(exec-path-from-shell-arguments '("-l"))
 '(extended-command-suggest-shorter nil)
 '(flycheck-c/c++-cppcheck-executable "c:/Program Files/Cppcheck/cppcheck.exe")
 '(flycheck-clang-args '("--std=c++17"))
 '(flycheck-disabled-checkers '(typescript-tslint emacs-lisp-checkdoc))
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(ggtags-enable-navigation-keys nil)
 '(git-commit-summary-max-length 64)
 '(helm-autoresize-mode t)
 '(helm-buffers-fuzzy-matching t)
 '(ido-auto-merge-delay-time 10)
 '(ido-enable-flex-matching t)
 '(ido-use-filename-at-point 'guess)
 '(indent-tabs-mode nil)
 '(inferior-octave-program "c:/Octave/3.2.4_gcc-4.4.0/bin/octave")
 '(jit-lock-defer-time 0.2)
 '(js-indent-level 2)
 '(js2-strict-missing-semi-warning nil)
 '(magit-backup-mode nil)
 '(magit-cygwin-mount-points '(("/c" . "c:")))
 '(magit-diff-expansion-threshold 999.0)
 '(magit-diff-refine-hunk t)
 '(magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
 '(magit-expand-staged-on-commit 'full)
 '(magit-log-format-graph-function 'magit-log-format-unicode-graph)
 '(magit-log-format-unicode-graph-alist '((47 . 9585) (92 . 9586) (42 . 9642)))
 '(magit-pull-arguments '("--rebase"))
 '(magit-refresh-status-buffer nil)
 '(markdown-command "pandoc")
 '(mhtml-tag-relative-indent nil)
 '(ns-command-modifier 'meta)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (dot . t)
     (ditaa . t)
     (latex . t)
     (sql . t)
     (shell . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-export-backends '(ascii html icalendar latex odt koma-letter))
 '(org-export-coding-system 'utf-8)
 '(org-export-with-sub-superscripts '{})
 '(org-export-with-toc nil)
 '(org-latex-listings t)
 '(org-latex-packages-alist
   '(("cm" "fullpage" nil)
     ("compact" "titlesec" nil)
     ("" "paralist" nil)
     ("" "enumitem" nil)
     ("" "color" nil)
     ("" "tabularx" nil)
     ("" "enumitem" nil)))
 '(org-list-allow-alphabetical t)
 '(org-odt-convert-processes
   '(("LibreOffice" "\"c:/Program Files (x86)/LibreOffice 5/program/soffice\" --headless --convert-to %f%x --outdir %d %i")
     ("unoconv" "unoconv -f %f -o %d %i")))
 '(org-odt-preferred-output-format "docx")
 '(org-src-fontify-natively t)
 '(org-startup-folded nil)
 '(org-startup-indented nil)
 '(org-table-convert-region-max-lines 9999)
 '(org-use-speed-commands t)
 '(org-use-sub-superscripts '{})
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules"))
 '(ps-font-size '(7 . 10))
 '(ps-paper-type 'letter)
 '(py-python-command "c:/python27/python")
 '(rng-nxml-auto-validate-flag t)
 '(safe-local-variable-values
   '((venv-location . "~/Library/Caches/pypoetry/virtualenvs")
     (venv-location . ".")
     (indent-tabs-mode . 2)
     (eval pyvenv-activate "venv")
     (eval venv-workon "venv")
     (c-basic-offset 4)
     (Mode . C++)
     (Mode . C)
     (test-case-name . twisted\.test\.test_protocols)
     (Mode . c++)
     (Mode . python)
     (Mode . perl)
     (Mode . cperl)
     (comment-new_column . 0)))
 '(same-window-regexps '("\\*shell.*\\*\\(\\|<[0-9]+>\\)"))
 '(sentence-end-double-space nil)
 '(speedbar-tag-hierarchy-method
   '(speedbar-prefix-group-tag-hierarchy speedbar-trim-words-tag-hierarchy speedbar-sort-tag-hierarchy))
 '(taskjuggler-command "tj3")
 '(tramp-syntax 'default nil (tramp))
 '(typescript-indent-level 2)
 '(vc-dired-recurse nil)
 '(visible-bell t)
 '(w32-get-true-file-attributes nil t)
 '(warning-suppress-types '((\(undo\ discard-info\))))
 '(whitespace-style
   '(face trailing tabs spaces newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t (:background "floral white"))))
 '(magit-section-highlight ((t (:background "floral white"))))
 '(org-agenda-date-today ((t (:inherit org-agenda-date :slant italic :weight bold :height 1.1))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "deep sky blue" :weight thin))))
 '(org-level-1 ((t (:inherit default :weight bold :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :slant italic :height 1.1)))))

(put 'set-goal-column 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'emacs)
;;; emacs ends here
