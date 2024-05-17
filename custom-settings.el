;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE: this is used by Custom, but I do NOT load it!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
 '(company-dabbrev-code-modes
   '(prog-mode batch-file-mode csharp-mode css-mode erlang-mode
               haskell-mode jde-mode lua-mode python-mode
               def-effects-mode))
 '(custom-safe-themes
   '("0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     default))
 '(ecb-layout-name "left1")
 '(ecb-layout-window-sizes
   '(("left1" (0.2698412698412698 . 0.30158730158730157)
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
 '(eldoc-echo-area-use-multiline-p t)
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
 '(jit-lock-chunk-size 3000)
 '(jit-lock-defer-time 0.2)
 '(js-indent-level 2)
 '(js2-strict-missing-semi-warning nil)
 '(llm-warn-on-nonfree nil)
 '(markdown-command "pandoc" t)
 '(mhtml-tag-relative-indent nil)
 '(mouse-wheel-tilt-scroll t)
 '(ns-command-modifier 'meta)
 '(ps-font-size '(7 . 10))
 '(ps-paper-type 'letter)
 '(py-python-command "c:/python27/python")
 '(rng-nxml-auto-validate-flag t)
 '(safe-local-variable-values
   '((venv-location . "~/Library/Caches/pypoetry/virtualenvs")
     (venv-location . ".") (indent-tabs-mode . 2)
     (eval pyvenv-activate "venv") (eval venv-workon "venv")
     (c-basic-offset 4) (org-src-preserve-indentation . t)
     (Mode . C++) (Mode . C)
     (test-case-name . twisted.test.test_protocols) (Mode . c++)
     (Mode . python) (Mode . perl) (Mode . cperl)
     (comment-new_column . 0)))
 '(same-window-regexps '("\\*shell.*\\*\\(\\|<[0-9]+>\\)"))
 '(sentence-end-double-space nil)
 '(speedbar-tag-hierarchy-method
   '(speedbar-prefix-group-tag-hierarchy
     speedbar-trim-words-tag-hierarchy speedbar-sort-tag-hierarchy))
 '(taskjuggler-command "tj3")
 '(typescript-indent-level 2)
 '(vc-dired-recurse nil)
 '(visible-bell t)
 '(w32-get-true-file-attributes nil t)
 '(warning-suppress-log-types '((comp) (frameset) (\(undo\ discard-info\))))
 '(warning-suppress-types '((frameset) (\(undo\ discard-info\))))
 '(what-cursor-show-names t)
 '(whitespace-style
   '(face trailing tabs spaces newline empty indentation space-after-tab
          space-before-tab space-mark tab-mark newline-mark)))

;;; Custom faces: don't use this; customize theme instead where possible.
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(flycheck-fringe-info ((t (:foreground "#00dd00"))))
;;  '(flycheck-info ((t (:underline (:color "#00ff00" :style wave)))))
;;  '(header-line ((t (:height 1.1 :slant italic :color "grey75" :foreground "grey20" :background "grey90" :inherit (mode-line)))))
;;  '(lsp-ui-doc-background ((((background light)) (:background "#ffffbb")) (t (:background "#272A36"))))
;;  '(lsp-ui-sideline-code-action ((t (:foreground "brown"))))
;;  '(org-agenda-date-today ((t (:inherit org-agenda-date :slant italic :weight bold :height 1.1))))
;;  '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "deep sky blue" :weight thin))))
;;  '(org-level-1 ((t (:inherit default :weight bold :height 1.3))))
;;  '(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.15))))
;;  '(org-level-3 ((t (:inherit outline-3 :slant italic :height 1.1))))
;;  '(tab-bar ((t (:inherit variable-pitch :background "grey85" :foreground "black" :height 0.87))))
;;  '(web-mode-current-column-highlight-face ((t (:background "#f0f0f0"))) t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
