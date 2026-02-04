;;; init-languages.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Language modes

;; Mostly programming-language related.

(use-package typescript-mode
  :mode ("\\.ts$")
  )

(use-package js2-mode
  :mode ("\\.js$")
  )

(use-package cuda-mode
  :mode ("\\.cu$")
  )

(use-package web-mode
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2
        web-mode-script-padding 0       ; start script in col 0
        web-mode-enable-current-column-highlight t
        )
  (defun my-web-mode-prettier-on-save ()
    (when (and buffer-file-name
               (or (derived-mode-p 'web-mode)
                   (derived-mode-p 'javascript-mode))
               (executable-find "bunx"))
      (let ((output (shell-command-to-string
                     (format "bunx prettier --write %s"
                             (shell-quote-argument buffer-file-name)))))
        (when (string-match-p (regexp-quote (file-name-nondirectory buffer-file-name)) output)
          (revert-buffer :ignore-auto :noconfirm)))))
  (add-hook 'after-save-hook #'my-web-mode-prettier-on-save)

  )
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; Vue mode: instead of the old mmm-mode one, use a custom mode derived from web-mode
;; -- set up for .vue files (html/css/script)

;; (use-package vue-mode
;;   :mode "\\.vue$"
;;   :config
;;   (setq mmm-submode-decoration-level 0) ; don't color background of sub-modes
;;   (add-to-list 'mmm-save-local-variables '(sgml--syntax-propertize-ppss))
;;   )
;; 2021: web-mode is better than vue-mode (simpler) -- also use for plain html

(define-derived-mode vue-mode web-mode "GO.Vue"
    "A major mode derived from web-mode, for editing .vue files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(define-derived-mode astro-mode web-mode "astro"
    "A major mode derived from web-mode, for editing .astro files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-mode))

(use-package php-mode
  :mode "\\.php$"
  )

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package gdscript-mode
  :mode ("\\.gd$")
)

(use-package opencl-c-mode
  :ensure (:main "opencl-c-mode.el")
  :mode ("\\.cl$" . opencl-c-mode)
)

;; instant live github markdown preview in markdown mode, C-c C-c g
;; Requires 'grip', a python package (pip install grip) installed in system python
(use-package grip-mode
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

(use-package dumb-jump
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

;;;; clojure: for logseq config (.edn files)
;; (clojure is a web language with lisp-like syntax)
(use-package clojure-mode)

;;;; Format C++ buffers with clang-format

(use-package clang-format)

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
              ;; Continue to save.
              nil)
            nil
            ;; Buffer local hook.
            t))

;; Run this for each mode you want to use the hook.
(add-hook 'c-mode-common-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'glsl-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'c-ts-base-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

;;; Language Settings

(use-package metal-mode
  :ensure (:host github
           :repo "masfj/metal-mode"
           :branch "master")
  )

;; Set up auto modes and settings

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
(setq auto-mode-alist (cons '("\\.mm$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.metal$" . metal-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cmake$" . cmake-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("CMakeLists\\.txt$" . cmake-mode) auto-mode-alist))


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
  ;; Much of this is irrelevant in modern code (as of 2023 or so) because
  ;; I use clang-format to format C/C++ buffers on save.
  ;; But this keeps things close to the proper style as I'm editing.
  ;; Also with the treesit modes, these are probably ignored.
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
(add-hook 'c-ts-base-mode-hook
          'my-c-mode-hook)

(add-hook 'java-mode-hook
          (function
           (lambda ()
             (setq-default c-basic-offset 4)
             (local-set-key "\C-cc" 'compile)
             )))

;; always hilight XXX in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<XXX\\|TODO\\|FIXME\\>" 0 font-lock-warning-face prepend)
               ))))

;;; Tree-sitter for syntax highlighting

;; Tree-sitter is a new (as of Nov 2022) multi-language parser that
;; produces a full AST. It enables faster and better syntax
;; highlighting, and other upcoming features.

;; Run `tree-sitter-langs-install-grammars` periodically to install
;; new grammars.

;; Built-in treesit (as of Jan 2023) requires compiled grammars in lib
;; path or ~~/.config/emacs/tree-sitter~. Build those using
;; https://github.com/casouri/tree-sitter-module.git.

(setq using-treesit nil)

;;; Enable built-in treesit support, or dynamically loaded tree-sitter
;;; Q: can these coexist? ts-fold wants to use tree-sitter, for instance.
(when (and (functionp 'treesit-available-p) (treesit-available-p))
  ;; Use built-in treesit -- best as of Jan 2023
  (setq using-treesit t)
  (message "★Built-in treesit is available!")

  (use-package treesit-auto
    :custom
    (treesit-auto-install 'prompt)
    :config
    ;; (setq treesit-auto-langs
    ;;       (delete 'c
    ;;               (delete 'cpp treesit-auto-langs)))
    (treesit-auto-add-to-auto-mode-alist))
  )
;; Also use tree-sitter minor mode (?)
;; Actually it doesn't play perfectly with treesit; ts-fold at least
;; doesn't work properly and that's the point of using this mode.
(when (and (functionp 'module-load) (not using-treesit)
  (use-package tree-sitter
    :diminish tree-sitter-mode
    :config
    (push '(c++-ts-mode . cpp) tree-sitter-major-mode-language-alist)
    (push '(c++-ts-mode . cxx) tree-sitter-major-mode-language-alist)
    )
  (use-package tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ))

;;; Tree-sitter based code folding

;; Very nice language-aware code folding with sidebar markers. This
;; depends on ~tree-sitter-mode~.

  ;; (cond (using-treesit
  ;;        (message "Using native treesit for ts-fold")
  ;;        (use-package ts-fold
  ;;          :ensure (:host github
  ;;                   :repo "AndrewSwerlick/ts-fold"
  ;;                   :branch "andrew-sw/treesit-el-support"
  ;;                   :fork (:host github
  ;;                          :repo "garyo/ts-fold"
  ;;                          :branch "garyo/treesit-el-patches")
  ;;                   )

  ;;          :config (global-ts-fold-indicators-mode)

  ;;          :bind (("C-c C-f" . hydra-ts-fold/body)
  ;;                 )
  ;;          )
  ;;        )
  ;;       (t
  ;;        (message "Using tree-sitter version of ts-fold")
  ;;        (use-package ts-fold
  ;;          :ensure (:host github :repo "emacs-tree-sitter/ts-fold")
  ;;          :config (global-ts-fold-indicators-mode)

  ;;          :bind (("C-c C-f" . hydra-ts-fold/body)
  ;;                 )
  ;;          )
  ;;        ))

(provide 'init-languages)
