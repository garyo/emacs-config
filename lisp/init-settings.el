;;; init-settings.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq
 backup-by-copying-when-linked t
 font-lock-maximum-decoration t
 compilation-window-height 15
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


(provide 'init-settings)
