;;; init-language-server.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Language Servers: lsp-mode or eglot

;; - May 2019: Eglot is more responsive and simpler

;; - Oct 2019: lsp-mode has more features, but it's very slow unless
;;   this Emacs has the fast C json lib (libjansson). And even then
;;   it's super slow for me.
;; - Aug 2020: lsp-mode is now faster and more reliable than eglot. Time to switch.
;; - Jun 2023: maybe switch back to eglot since it's built in and has come a long way

;; Note: use ~pylsp~ for Python, rather than ~pyright~. The latter is
;; just a type checker; pylsp is a full configurable LSP server.
;; Install like this: ~pip install python-lsp-server~. Also good to
;; install ~pyflakes~ for linting, ~pylsp-mypy~ for type checking,
;; ~pylsp-rope~ for refactoring, ~python-lsp-black~ for formatting.

;; For C++, use ~clangd~.

;;; Eglot Vue Language Server

;; When using eglot, the Vue language server has to be specially
;; configured; at least it did when I wrote this. We define some vars
;; that will only be used when using eglot.

(defvar lsp-verbose nil
  "Set to t to turn on lots of logging in lsp-mode or eglot.")

;; for Vue VLS with eglot
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
                               ,@(if lsp-verbose
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

;; Run this before loading eglot
(defun my-eglot-vue-init ()
  """Initialize Vue Language Server for eglot."""

  ;; Find the location of "typescript.js"
  ;; On my M1 Mac, it's in $(yarn global dir)/node_modules/typescript/lib
  (defun typescript-lib-dir ()
    (expand-file-name
     "node_modules/typescript/lib"
     (string-trim-right (shell-command-to-string "yarn global dir"))
     ))

  (defun vue-eglot-init-options ()
    (let ((tsdk-path (typescript-lib-dir)))
      `(:typescript (:tsdk ,tsdk-path)
                    :vue (:hybridMode :json-false)
                    :languageFeatures (:completion
                                       (:defaultTagNameCase "both"
                                                            :defaultAttrNameCase "kebabCase"
                                                            :getDocumentNameCasesRequest nil
                                                            :getDocumentSelectionRequest nil)
                                       :diagnostics
                                       (:getDocumentVersionRequest nil))
                    :documentFeatures (:documentFormatting
                                       (:defaultPrintWidth 100
                                                           :getDocumentPrintWidthRequest nil)
                                       :documentSymbol t
                                       :documentColor t))))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Set up LSP or Eglot

;; Since fast json and native compilation, lsp-mode is plenty fast, and quite nice.
;; I use it for Javascript/typescript, Vue, python, and maybe C++.

;; *** My setup for LSP mode
;; This is currently unused as of Dec 2023; eglot is built-in to emacs and works well.

;; (defun lsp-ui-doc-font ()
;;   (face-remap-add-relative 'default :family "Bitstream Charter" :height 120))

(defun setup-lsp-mode ()
  ;; LSP mode: language server protocol for getting completions, definitions etc.
  (use-package lsp-mode
    :commands lsp
    :hook ((vue-mode . lsp)
           (web-mode . lsp)
           (typescript-mode . lsp)
           (typescript-ts-mode . lsp)
           (javascript-mode . lsp)
           (javascript-ts-mode . lsp)
           (js2-mode . lsp)
           (js2-ts-mode . lsp)
           ;; python LSP; it hangs sometimes?
           (python-mode . lsp)
           (python-ts-mode . lsp)
           (c-mode-common . lsp)
           (c-ts-base-mode . lsp)
           )
    :init
    (setq lsp-keymap-prefix "C-c C-l")  ; default is super-l
    :config
    (setq lsp-log-io lsp-verbose
          lsp-clients-typescript-log-verbosity (if lsp-verbose "verbose" "normal")
          lsp-print-performance t
          lsp-response-timeout 15
          lsp-headerline-breadcrumb-enable t
          lsp-headerline-breadcrumb-segments '(file symbols)
          flycheck-checker-error-threshold 1000 ; need more than default of 400
          lsp-pylsp-plugins-pylint-enabled nil ; too much! Other pylsp checkers do enough.
          )
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
    )
  (use-package lsp-ui
    :commands lsp-ui-mode
    :hook (lsp-mode . lsp-ui-mode)
    :config
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-use-webkit t
          lsp-ui-doc-include-signature t
          lsp-ui-sideline-show-hover t ; show hover actions in the sideline
          lsp-ui-doc-use-childframe nil ; childframe has bugs (12/2020); nil works fine
          lsp-ui-sideline-actions-kind-regex "quickfix.*" ; don't show refactor actions; too many (in vue mode)
          lsp-ui-sideline-enable nil ; turn off the whole sideline (right sidebar doc & actions)
          lsp-modeline-code-actions-mode nil
          )
    )
  (use-package lsp-pyright) ; python type-checker, better than pylsp (Dec 2021)
  (use-package lsp-treemacs)
  ;; doesn't work
  ;; (add-hook 'lsp-ui-doc-mode-hook #'lsp-ui-doc-font)

  (if (not (featurep 'yasnippet))
      (warn "LSP: missing yasnippet, LSP won't work well"))
  )

;;; My setup for eglot mode

;; Eglot works well. I have it set up to use popups for errors and
;; hover doc.

;; Tested & working with C++ (clangd), Python (pyright-langserver),
;; and Typescript (typescript-language-server).

;; note: eglot uses eldoc-box for popup doc
;; and flymake for errors (I use flymake-posframe so errors show up as popups)

(defun setup-eglot-mode ()
  ;(use-package jsonrpc) NO -- leave this as built-in
  (use-package eglot
    ;; Don't get from master repo -- eglot is part of emacs now, just use the normal version.
    ;; :ensure (:host github
    ;;                  :repo "joaotavora/eglot"
    ;;                  :branch "master")
    :ensure nil
    :commands eglot-ensure eglot
    :hook ((vue-mode . eglot-ensure)
           (c-mode-common . eglot-ensure)
           (c-ts-base-mode . eglot-ensure)
           (cmake-base-mode . eglot-ensure)   ; cmake-language-server
           (sh-base-mode . eglot-ensure)      ; bash-language-server
           (yaml-base-mode . eglot-ensure)    ; yaml-language-server
           (python-base-mode . eglot-ensure)
           (dockerfile-base-mode . eglot-ensure) ; docker-langserver
           (js-base-mode . eglot-ensure)
           (typescript-ts-base-mode . eglot-ensure)
                                        ; (prog-mode . eglot-ensure) ; all prog modes: C++, python, typescript etc.
           )
    :init
    (my-eglot-vue-init)
    :config
    ;; Install like this:
    ;;  yarn global add @vue/language-server@latest @vue/typescript-plugin@latest
    ;; and make sure $(yarn global bin) is in exec-path
    (add-to-list 'eglot-server-programs
                 `(vue-mode . ("vue-language-server" "--stdio"
                                   :initializationOptions ,(vue-eglot-init-options))))
    (define-key eglot-mode-map (kbd "C-c h") 'display-local-help)

    )
  )

;;; Set up LSP service (eglot or lsp)

(if (and use-lsp-mode (has-fast-json))
  (setup-lsp-mode)
  (setup-eglot-mode))

(provide 'init-language-server)
