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
                               (next-line 10)))
(global-set-key (kbd "M-p") #'(lambda ()
                               "Move up 10 lines"
                               (interactive)
                               (previous-line 10)))
(global-set-key [f5] 'project-compile)
(global-set-key [remap count-words-region] #'count-words) ; better: uses region when active



(provide 'init-keybindings)
