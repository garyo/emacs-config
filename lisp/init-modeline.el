;;; init-modeline.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(cond ((eq modeline-package 'sml)
       (use-package smart-mode-line
         :config
         (setq sml/no-confirm-load-theme t)
         (setq sml/name-width 40)
         (setq sml/mode-width 'full)
         (setq sml/extra-filler -4) ; Seem to need this with eglot to prevent truncation on the right
         (setq sml/shorten-directory t)
         (setq sml/shorten-modes t)
         ;; don't show these minor modes
         (setq rm-blacklist '(" hl-p" " company" " ElDoc" " VHl" " Helm" " Fill"
                              " Filladapt" " counsel" " ivy" " yas" " GitGutter"))
         (add-to-list 'sml/replacer-regexp-list
                      '("c:/dss/Product/Horizon/WebProjects/horizon-project/horizon" ":HZN:"))
         (sml/setup)
         )
       )
      ((eq modeline-package 'doom) ; Very pretty modeline -- nicer than sml.
       ;; Note: this uses nerd-icons. You may need to do ~M-x nerd-icons-install-fonts~.
       ;; Note: theme will override various colors here. Customize there rather than here.
       (use-package doom-modeline
         :config
         (doom-modeline-mode)
         ;; My mod for the "workspace-name" segment: show a Tab icon
         ;; (it's confusing if it's just sitting there)
         (doom-modeline-def-segment workspace-name
           "The current workspace name or number.
            Requires `eyebrowse-mode' to be enabled or `tab-bar-mode' tabs to be created."
           (when doom-modeline-workspace-name
             (when-let
                 ((name (cond
                         ((and (bound-and-true-p eyebrowse-mode)
                               (length> (eyebrowse--get 'window-configs) 1))
                          (setq mode-line-misc-info
                                (assq-delete-all 'eyebrowse-mode mode-line-misc-info))
                          (when-let*
                              ((num (eyebrowse--get 'current-slot))
                               (tag (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
                            (if (length> tag 0) tag (int-to-string num))))
                         ((and (fboundp 'tab-bar-mode)
                               (length> (frame-parameter nil 'tabs) 1))
                          (let* ((current-tab (tab-bar--current-tab))
                                 (tab-index (tab-bar--current-tab-index))
                                 (explicit-name (alist-get 'explicit-name current-tab))
                                 (tab-name (alist-get 'name current-tab)))
                            (if explicit-name tab-name (+ 1 tab-index)))))))
               (concat
                (propertize " "
                            'face (doom-modeline-face 'doom-modeline-buffer-major-mode))
                (propertize (doom-modeline-icon 'mdicon "nf-md-tab" "🖿" "TAB"))
                (propertize (format ":%s " name)
                            'face (doom-modeline-face 'doom-modeline-buffer-major-mode))
                ))))

         )
       )
      )


(provide 'init-modeline)
