;;; init-keybindings.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(global-set-key (kbd "M-SPC") 'cycle-spacing) ; improvement over just-one-space; repeated calls cycle 1, 0, orig
(global-set-key (kbd "C-z") 'scroll-up-line) ; use emacs24 builtins
(global-set-key (kbd "M-z") 'scroll-down-line)
(global-set-key (kbd "M-k") 'copy-line)
(global-set-key (kbd "M->") 'end-of-buffer-right-way)
(global-set-key (kbd "C-X .") 'goto-line)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "M-n") #'(lambda ()
                               "Move down 10 lines"
                               (interactive)
                               (forward-line 10)))
(global-set-key (kbd "M-p") #'(lambda ()
                               "Move up 10 lines"
                               (interactive)
                               (forward-line -10)))
(global-set-key [f5] 'project-compile)
(global-set-key [remap count-words-region] #'count-words) ; better: uses region when active

;; Prevent accidentally changing font size with mouse wheel and pinch gestures
;; (they're *way* too fast anyway)
;; Better to use C-x C-0, C-x C-=, C-x C-- and C-x C-+.
;; They have repeat modes so they work nicely.
;; See also text-scale-mode-step.
;; (global-set-key (kbd "<pinch>") 'ignore) -- this one is OK
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)


(provide 'init-keybindings)
