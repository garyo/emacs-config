;;; init-completion.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Completion packages: consult, vertico, corfu, orderless, embark

;; Emacs has two types of completion: minibuffer with
;; ~completing-read~, and in-buffer with ~completion-at-point~.
;; ~completing-read~ is what's used in ~M-x find-file~ and anything
;; that prompts in the minibuffer. ~completion-at-point~ (which I bind
;; to ~M-RET~ for historical reasons) is more complex and interesting,
;; with lots of options springing up in the last few years. But as of
;; 2024, Corfu is built in to emacs 29, and it's quite lightweight
;; compared to company, so it makes sense to switch to that.

;; For minibuffer completion, I use vertico.
;;   With vertico, I set up vertico + consult + marginalia + orderless.

;; For capf (in-buffer completion), I use corfu + cape.
;;   See https://github.com/minad/corfu for details.
;;   This also uses orderless.

;; Embark is a departure from regular emacs; it allows selecting the
;; object and then deciding what to do with it. I don't always use it,
;; but nice to have.


;; Until 2024 I used ~Company~ for my in-buffer completion framework,
;; used for e.g. identifier completion in programming languages. It
;; supports many backends, which are sources of completion candidates.
;; It can use ~vertico~ as a UI, but its own UI is fine. The default
;; backend is ~company-capf~ which in turn redirects to the
;; completion-at-point-functions (which defaults to
;; ~tags-completion-at-point-functions~ I think, but gets rebound by
;; various modes).

;; I grew up using Jim Salem's TMC completion so M-RET is in my
;; fingers. :-). In most modes I'd like to emulate that completion
;; type, which remembered what you type and recorded contents of
;; visited buffers, and used that cache to propose completions (based
;; on initial substring match). ~dabbrev~ is the closest modern
;; version of that, so I use it as a capf.

;; Vertico is just a simple "vertical" completion UI for minibuffer
;; completion (~completing-read~) -- no new commands. Consult adds
;; completing versions of various commands, and those get presented by
;; vertico. Try ~M-x~ or ~C-x C-f~ to see what this does.

;; Why not ~selectrum~? Selectrum was the predecessor of Vertico, so
;; Vertico seems to do what selectrum does but better in most cases.

;; There's some good samples of customizations at
;; https://kristofferbalintona.me/posts/202202211546/#extensions

;; Note that org-mode redefines ~M-RET~ as something else -- we rebind
;; it in that mode so it works there too.


;;;;;;;;;;;;;;
;;; Consult
;;;;;;;;;;;;;;

;; Lots of additional completion sources for various commands. I don't
;; use most of these consult bindings, so they could use a good
;; pruning. But things like ~consult-buffer~ are vital to my workflow.
;; Probably would be better to use ~[remap ...]~ more, to replace
;; standard bindings.

;; Use the ~substring~ completion style so calling this from isearch works properly
(defun consult-line-literal ()
  (interactive)
  (let ((completion-styles '(substring))
        (completion-category-defaults nil)
        (completion-category-overrides nil))
    (consult-line)))

(use-package consult
  :defines consult-buffer-sources
  :demand t
  :init
  (setq consult-preview-key 'any)
  :bind (
         ;; !!! Replace isearch with consult-line!
         ("C-s" . consult-line)
         ("C-r" . consult-line)

         ;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ([remap switch-to-buffer] . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line) ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line-literal)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("C-o" . consult-line-literal)
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line-literal) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         )
  :config
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-narrow-key "<") ; use this to show different types of things in C-x b

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.4 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   )
  ;; Use projects as a source for consult-buffer
  ;; Works, but hides "file" sources -- use "<" to select other sources
  (setq my-consult-source-projects
        `(:name "Project.el projects"
                :narrow   ?P
                :category project
                :action   ,#'project-switch-project
                :items    ,(project-known-project-roots)))
  (add-to-list 'consult-buffer-sources my-consult-source-projects 'append)
  )

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; flycheck integration - nice if using flycheck. ~M-g f~
(if use-flycheck-mode
    (use-package consult-flycheck))

;;; Minibuffer completion: vertico + consult

(use-package vertico
  :ensure (:files (:defaults "extensions/*"))
  :after consult
  :demand t
  :config
  (vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Correct file path when changed
  )

;;  "orderless"" completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :after consult
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  )

;; show file metadata in buffer completion list (C-x b) etc.
(use-package marginalia
  :config
  (marginalia-mode))

;;; In-buffer completion (using complete-at-point-functions): corfu

(use-package corfu
  :ensure (:files (:defaults "extensions/*"))
  :demand t                      ; need this when using :bind or :hook
  :config
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.75)
  (corfu-quit-no-match t) ; quit when the popup appears and I type anything else
  ;; Might want to customize corfu-sort-function
  :bind
  (("M-RET" . completion-at-point)
   )
  )

;; corfu extension (in corfu/extensions/corfu-history.el); load after corfu
(use-package corfu-history
  :ensure nil
  :after corfu
  :config
  (corfu-history-mode)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)
)

(use-package corfu-terminal
  :after corfu
  :init
  (defvar corfu-terminal-mode)
  ;; TODO set this up, for use in non-GUI emacs
  )

;; Additional capf completion sources
(use-package cape
  :config
  ;; Note: order matters here. First one returning a result wins. Use
  ;; ~add-hook~ to add these, since it sets the global (default) value
  ;; of capf, instead of ~setq~ which would make it buffer-local
  ;; (which would be bad): capf is automatically buffer-local when
  ;; set.
  ;; The buffer-local value, which takes precedence over these, calls these as long
  ;; as it ends with ~t~.
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (message (format "Loading my capf extensions: %s" completion-at-point-functions))
  )

;; Nice icons for corfu popups
(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Completion actions: Embark

;; embark is bound to C-. and allows actions on the current thing
;; at point, or the current completion candidate in the minibuffer.
;; This is nice because you can use C-x C-f (find-file) but then
;; decide to do something besides open it in a buffer using C-.

;; C-. pops up a nice window of commands you can do on the current
;; thing, so there's no learning curve.

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   )
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(provide 'init-completion)
