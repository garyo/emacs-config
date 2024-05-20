;;; init-settings.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;; Various varible settings and customizations.
;; TODO Should be curated and pruned. Maybe use (use-package emacs)?

;;; Code:

(setq
 backup-by-copying-when-linked t
 font-lock-maximum-decoration t
 compilation-scroll-output 'first-error
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
 mark-even-if-inactive t
 mouse-drag-copy-region t ; default in emacs24 is nil; I like the old way.
 require-final-newline t
 next-line-add-newlines nil
 scroll-step 2
 scroll-conservatively 10
 search-highlight t
 split-height-threshold (/ (frame-height) 2)
 tags-revert-without-query t
 truncate-partial-width-windows nil	; ECB needs this to avoid
                                        ; truncating source window
                                        ; since it's partial width.
 vc-make-backup-files t			; Make emacs backups even for
                                        ; version-controlled files
 version-control t
 )

(custom-set-variables
 '(ag-arguments '("--smart-case"))
 '(ag-highlight-search t)
 '(ag-reuse-buffers t)
 '(ag-reuse-window t)
 '(align-to-tab-stop nil)
 '(custom-safe-themes
   '("0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     default))
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
 '(jit-lock-defer-time 0.2)
 '(jit-lock-chunk-size 3000)
 '(js-indent-level 2)
 '(js2-strict-missing-semi-warning nil)
 '(llm-warn-on-nonfree nil)
 '(markdown-command "pandoc")
 '(mhtml-tag-relative-indent nil)
 '(mouse-wheel-tilt-scroll t)
 '(ns-command-modifier 'meta)
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
     (org-src-preserve-indentation . t)
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
 '(typescript-indent-level 2)
 '(vc-dired-recurse nil)
 '(visible-bell t)
 '(w32-get-true-file-attributes nil t)
 '(warning-suppress-log-types '((comp) (frameset) (\(undo\ discard-info\))))
 '(warning-suppress-types '((frameset) (\(undo\ discard-info\))))
 '(what-cursor-show-names t)
 '(whitespace-style
   '(face trailing tabs spaces newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))

(provide 'init-settings)
