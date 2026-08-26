;; init-org.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; XXX Windows dir
;; XXX don't call it org-notes, maybe org-pkm
(defcustom my/notes-dir
  (file-truename
   (or
    ;; Lets a throwaway Emacs run against a copy of the notes -- the only
    ;; way to exercise the markdown half of the PKM before the corpus is
    ;; converted.  See pkm-sandbox.sh.
    (getenv "GCO_PKM_DIR")
    (if (file-directory-p "~/Documents/org-notes")
        "~/Documents/org-notes"
      "~/Documents/org-agenda")))
  "Top-level notes dir, used for unified agenda, org-node and PKM.
Notes may be org or markdown; see `gco-pkm-format'."
  :type 'string
  :group 'pkm)
(unless (file-exists-p my/notes-dir)
  (make-directory my/notes-dir))

;; Org mode
(require 'cl-lib)

;; Image attachment optimization settings
(defcustom my/org-attach-image-convert-formats '("png" "heic" "tiff" "bmp")
  "Image file extensions to convert to JPG when attaching.
Set to nil to disable conversion."
  :type '(repeat string)
  :group 'pkm)

(defcustom my/org-attach-image-max-width 2000
  "Maximum width in pixels for attached images."
  :type 'integer
  :group 'pkm)

(defcustom my/org-attach-image-max-height 1000
  "Maximum height in pixels for attached images."
  :type 'integer
  :group 'pkm)

(defcustom my/org-attach-image-quality 85
  "JPEG quality (1-100) for converted attached images."
  :type 'integer
  :group 'pkm)

(defcustom my/pkm-inline-image-width 600
  "Width, in pixels, for inline image previews in notes.
Pasted images are already capped on disk at
`my/org-attach-image-max-width' x `my/org-attach-image-max-height', but
that is still far larger than a comfortable preview, so bound the display
too.  An explicit per-image width (`#+ATTR_ORG: :width' in org, the
menu's \"Image width\" in markdown) still wins."
  :type 'integer
  :group 'pkm)

(defcustom my/org-assets-dir (expand-file-name "assets" my/notes-dir)
  "Flat directory holding images pasted or dropped into notes.
Replaces org-attach's ID-hashed `data/' trees so that the path in a
file: link resolves identically in Emacs and in the web PKM."
  :type 'directory
  :group 'pkm)

;; Basic org-mode config
(use-package org
  :ensure t          ; use latest even though org is included in emacs
  :hook
  (org-mode . (lambda ()
                (mixed-pitch-mode 1)
                (visual-line-mode 1)
                (setq line-spacing 0.4) ; seems loose, but it looks good
                ;; I don't use ispell, no need for this
                (setq-local completion-at-point-functions
                            (remq #'ispell-completion-at-point completion-at-point-functions))
                ))
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   :map org-mode-map
   ("C-c C-y" . yank-media))
  :config
  (require 'org-tempo)
  (require 'org-attach)  ; register attachment: link type for inline image display

  ;; org-link-preview with arg (16) (startup, C-u C-u) skips links that
  ;; have descriptions.  Attachment links often get descriptions (e.g.
  ;; from yank-media), so change (16) → 11 which previews the whole
  ;; buffer *including* described links.
  (advice-add 'org-link-preview :around
              (lambda (orig-fn &optional arg &rest args)
                (apply orig-fn (if (equal arg '(16)) 11 arg) args)))

  ;; Auto-optimize and preview images after paste/drop.
  ;; - Formats in `my/org-attach-image-convert-formats' are converted to JPG
  ;; - ALL images (including JPG) are resized to fit max-width x max-height
  (defun my/sips-resize-to-fit (path max-w max-h)
    "Resize image at PATH to fit MAX-W x MAX-H using sips.
Only shrinks, never enlarges."
    (let* ((info (with-output-to-string
                   (call-process "sips" nil standard-output nil
                                 "-g" "pixelWidth" "-g" "pixelHeight" path)))
           (w (and (string-match "pixelWidth: \\([0-9]+\\)" info)
                   (string-to-number (match-string 1 info))))
           (h (and (string-match "pixelHeight: \\([0-9]+\\)" info)
                   (string-to-number (match-string 1 info)))))
      (when (and w h (or (> w max-w) (> h max-h)))
        (let ((new-w (max 1 (floor (* w (min (/ (float max-w) w)
                                              (/ (float max-h) h)))))))
          (call-process "sips" nil nil nil
                        "--resampleWidth" (number-to-string new-w) path)))))

  (defun my/optimize-image (path)
    "Optimize image at PATH: convert to JPG if needed, resize to fit.
Converts formats in `my/org-attach-image-convert-formats' to JPG.
Resizes all images to fit `my/org-attach-image-max-width' x
`my/org-attach-image-max-height'.  Uses magick or sips.
Returns final path (may differ from input if format changed)."
    (let* ((ext (downcase (file-name-extension path)))
           (convert-p (member ext my/org-attach-image-convert-formats))
           (max-w my/org-attach-image-max-width)
           (max-h my/org-attach-image-max-height)
           (size (format "%dx%d>" max-w max-h))
           (quality (number-to-string my/org-attach-image-quality))
           (jpg (concat (file-name-sans-extension path) ".jpg")))
      (cond
       ((executable-find "magick")
        (if convert-p
            ;; Convert format + resize
            (when (zerop (call-process "magick" nil nil nil
                                       path "-resize" size
                                       "-quality" quality jpg))
              (delete-file path) jpg)
          ;; Already JPG: resize in place
          (call-process "magick" nil nil nil
                        path "-resize" size "-quality" quality path)
          path))
       ((executable-find "sips")
        (if convert-p
            ;; Convert format, then resize
            (when (zerop (call-process "sips" nil nil nil
                                       "-s" "format" "jpeg"
                                       "-s" "formatOptions" quality
                                       path "--out" jpg))
              (my/sips-resize-to-fit jpg max-w max-h)
              (delete-file path) jpg)
          ;; Already JPG: resize in place, recompress
          (call-process "sips" nil nil nil
                        "-s" "formatOptions" quality path)
          (my/sips-resize-to-fit path max-w max-h)
          path))
       (t path))))

  (defun my/org-after-image-attach (&rest _)
    "Optimize the image just saved into `my/org-assets-dir', then preview."
    (save-excursion
      (goto-char (line-beginning-position))
      ;; Match the file: link org just inserted
      (when (re-search-forward
             "\\[\\[file:\\([^]]+\\.\\([a-zA-Z]+\\)\\)"
             (line-end-position) t)
        (let* ((link (match-string 1))
               (old-path (expand-file-name
                          link (file-name-directory (buffer-file-name))))
               (old-name (file-name-nondirectory old-path))
               (image-ext-re (image-file-name-regexp)))
          ;; Only touch images we just placed in assets/ -- never an
          ;; arbitrary file: link that happens to sit on this line.
          (when (and (file-exists-p old-path)
                     (string-match-p image-ext-re old-name)
                     (file-in-directory-p old-path my/org-assets-dir))
            (let* ((new-path (my/optimize-image old-path))
                   (new-name (file-name-nondirectory new-path)))
              ;; Update link if filename changed (format conversion)
              (unless (equal old-name new-name)
                (goto-char (line-beginning-position))
                (while (search-forward old-name (line-end-position) t)
                  (replace-match new-name t t))))))))
    (org-link-preview-region t nil (line-beginning-position) (line-end-position)))

  (dolist (fn '(org--image-yank-media-handler
                org--dnd-attach-file
                org--copied-files-yank-media-handler))
    (advice-add fn :after #'my/org-after-image-attach))

  (setopt
   ;; Directories and files
   org-directory my/notes-dir
   org-agenda-files (list org-directory)
   org-default-notes-file (concat org-directory "/journal.org")

   ;; TODO keywords and logging
   org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
   org-log-done 'time

   ;; Navigation and links
   org-return-follows-link t
   org-use-speed-commands t

   ;; Agenda behavior
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-start-on-weekday nil

   ;; Tags and refiling
   org-tags-exclude-from-inheritance '("project")
   org-tag-faces '(("@work" . "#0066ff")
                   ("@home" . "#bb0000")
                   ("volunteer" . "#005500"))
   org-refile-targets '((nil :maxlevel . 4)
                        (org-agenda-files :maxlevel . 4))

   ;; Display and formatting
   org-startup-folded 'nofold
   org-startup-indented t
   org-src-fontify-natively t
   org-list-allow-alphabetical t
   org-use-sub-superscripts '{}
   org-indent-mode-turns-on-hiding-stars nil
   org-startup-with-link-previews t
   ;; A list means: use an explicit #+ATTR width when present, else fall
   ;; back to this. Plain nil falls back to the image's actual size, which
   ;; for a 1558px paste fills the window.
   org-image-actual-width (list my/pkm-inline-image-width)
   ;; Pasted/dropped images go to one flat assets/ dir as a relative file:
   ;; link, so the same link resolves on every synced machine and in the
   ;; web PKM.  `org-yank-dnd-method' stays `attach' because that is what
   ;; routes dropped images through the save-method above; note that a
   ;; dropped *non-image* file still goes through org-attach.
   org-yank-image-save-method my/org-assets-dir
   org-yank-dnd-method 'attach
   org-yank-dnd-default-attach-method 'cp
   org-link-file-path-type 'relative

   ;; Babel
   org-confirm-babel-evaluate nil

   ;; Export settings
   org-export-backends '(ascii html icalendar latex koma-letter)
   org-export-coding-system 'utf-8
   org-export-with-sub-superscripts '{}
   org-export-with-toc nil

   ;; LaTeX export
   org-latex-src-block-backend 'listings
   org-latex-packages-alist '(("cm" "fullpage" nil)
                              ("compact" "titlesec" nil)
                              ("" "paralist" nil)
                              ("" "color" nil)
                              ("" "tabularx" nil)
                              ("" "enumitem" nil))

   ;; Table settings
   org-table-convert-region-max-lines 9999
   )

   ;; Babel (code execution)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (python . t)
     (calc . t)
     (js . t)
     (dot . t)
     (ditaa . t)
     (latex . t)
     (sql . t)
     (shell . t)))
  (setq org-babel-python-command "uv run python")

  ;; org-inlinetask for pseudo-"inline" tasks (really just deep headings)
  (require 'org-inlinetask)

  ;; when modifying agenda files make sure to update appt
  (when (file-exists-p org-directory)
    (require 'filenotify)
    (file-notify-add-watch org-directory '(change) #'gco-org-agenda-file-notify))
  )

;; Personal Knowledge Management (PKM) system
;; For local packages, ensure dependencies are loaded first
;; then use regular use-package with :ensure nil
(use-package gco-pkm
  :ensure nil
  :load-path "lisp/"
  :after org
  :demand t  ; force loading when org is ready
  :config
  (setopt gco-pkm-directory my/notes-dir)
  (gco-pkm-setup)
  :bind (("C-c j" . gco-pkm-journal-today)))

(use-package gco-inline-tags
  :ensure nil
  :load-path "lisp/"
  :after org
  :demand t
  :hook (org-mode . gco-inline-tags-mode)
  :bind (("C-c t s" . gco-inline-tags-search)
         ("C-c t i" . gco-inline-tags-insert))
  :config
  (setopt gco-inline-tags-roots (list my/notes-dir)))

;; Ensure transient is loaded first (only if not already in elpaca's queue, for me anyway)
;(use-package transient :ensure t :demand t)

;; XXX when elpaca supports loading single files, fix this to use elpaca with :type file
(use-package gco-pkm-transient
  :ensure nil
  :load-path "lisp/"
  :after (gco-pkm org transient)
  :demand t
  :bind (("C-c C-/" . gco-pkm-menu)))

(use-package gco-pkm-calendar
  :ensure nil
  :load-path "lisp/"
  :after (gco-pkm calendar)
  :commands (gco-pkm-calendar-browse))

(use-package gco-pkm-context
  :ensure nil
  :load-path "lisp/"
  :after (gco-pkm gco-inline-tags)
  :demand t
  ;; Notes come in both formats while the PKM is converted to markdown,
  ;; so the sidebar attaches to either.
  :hook ((org-mode . gco-pkm-context-maybe-enable)
         (markdown-mode . gco-pkm-context-maybe-enable)
         ;; markdown-ts-mode does not run markdown-mode-hook; see init-markdown.
         (markdown-ts-mode . gco-pkm-context-maybe-enable))
  :bind (("C-c t c" . gco-pkm-context-toggle)))

;;;; Markdown notes: the same image handling org notes get
;;
;; markdown-mode ships a yank-media handler, but it prompts for a filename
;; on every paste, saves beside the buffer, and skips the optimization
;; above.  Replace it so pasting into a markdown note behaves exactly like
;; pasting into an org one: auto-named, into assets/, optimized, linked
;; relatively.

(defun my/pkm-md-asset-link (path)
  "Return a markdown image link to PATH, relative to the current buffer."
  (format "![](%s)"
          (file-relative-name path (file-name-directory (buffer-file-name)))))

(defun my/pkm-md-save-asset (data ext)
  "Write DATA to a timestamped EXT file in `my/org-assets-dir'; return path.
Names match the corpus convention, `clipboard-<ISO stamp>.<ext>'."
  (unless (file-directory-p my/org-assets-dir)
    (make-directory my/org-assets-dir t))
  (let ((path (expand-file-name
               (format "clipboard-%s.%s" (format-time-string "%Y%m%dT%H%M%S") ext)
               my/org-assets-dir)))
    (let ((coding-system-for-write 'emacs-internal))
      (with-temp-file path (insert data)))
    path))

(defun my/markdown-refresh-images ()
  "Re-display inline images, in whichever markdown mode is active."
  (ignore-errors
    (if (derived-mode-p 'markdown-ts-mode)
        (when (fboundp 'markdown-ts--set-inline-images)
          (markdown-ts--set-inline-images t))
      (when (fboundp 'markdown-display-inline-images)
        (markdown-display-inline-images)))))

(defun my/pkm-md-image-yank-handler (mimetype data)
  "Save pasted image DATA of MIMETYPE into assets/ and link it."
  (require 'mailcap)
  (let* ((ext (symbol-name (mailcap-mime-type-to-extension mimetype)))
         (path (my/pkm-md-save-asset data ext))
         (final (my/optimize-image path)))
    (insert (my/pkm-md-asset-link final))
    (my/markdown-refresh-images)))

(defun my/pkm-md-dnd-handler (url action)
  "Copy a dropped image URL into assets/ and link it; else fall back."
  (let ((file (dnd-get-local-file-name url t)))
    (if (and file (string-match-p (image-file-name-regexp) file))
        (progn
          (unless (file-directory-p my/org-assets-dir)
            (make-directory my/org-assets-dir t))
          (let ((dest (expand-file-name (file-name-nondirectory file)
                                        my/org-assets-dir)))
            (copy-file file dest t)
            (insert (my/pkm-md-asset-link (my/optimize-image dest)))
            (my/markdown-refresh-images)
            'private))
      (dnd-insert-text (selected-window) action (or file url)))))

;;;; Frontmatter folding
;;
;; Frontmatter is the markdown counterpart of an org property drawer:
;; bookkeeping that belongs in the file but not in your face. Org folds
;; those on open, so fold this the same way.

(defcustom my/markdown-hide-markup t
  "Whether to hide markup delimiters in markdown buffers.
With this on, list markers render as bullets, checkboxes as boxes, and
thematic breaks as rules -- the org-like presentation.  It is a single
switch: emphasis and link delimiters are hidden too.  Toggle per buffer
with \[markdown-ts-toggle-hide-markup] (C-c C-x C-m)."
  :type 'boolean
  :group 'pkm)

(defcustom my/markdown-fold-frontmatter t
  "Whether to fold YAML frontmatter when opening a markdown note."
  :type 'boolean
  :group 'pkm)

(defun my/markdown-frontmatter-bounds ()
  "Return (START . END) of the buffer's YAML frontmatter, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^---[ \t]*$")
      (let ((start (point)))
        (forward-line 1)
        (when (re-search-forward "^---[ \t]*$" nil t)
          (cons start (line-end-position)))))))

(defun my/markdown-frontmatter-overlay ()
  "Return the existing frontmatter overlay, or nil."
  (seq-find (lambda (o) (overlay-get o 'my/pkm-frontmatter))
            (overlays-in (point-min) (min (point-max) 4096))))

(defun my/markdown-in-frontmatter-p ()
  "Non-nil when point is inside the frontmatter block, folded or not.
When folded the text is still there, just invisible, so the same bounds
check answers for both states -- which is what lets one key toggle it
each way."
  (when-let* ((bounds (my/markdown-frontmatter-bounds)))
    (and (>= (point) (car bounds)) (<= (point) (cdr bounds)))))

(defun my/markdown-toggle-frontmatter ()
  "Fold or unfold the YAML frontmatter block."
  (interactive)
  (if-let* ((o (my/markdown-frontmatter-overlay)))
      (delete-overlay o)
    (when-let* ((bounds (my/markdown-frontmatter-bounds)))
      (let ((o (make-overlay (car bounds) (cdr bounds))))
        (overlay-put o 'my/pkm-frontmatter t)
        (overlay-put o 'invisible t)
        (overlay-put o 'isearch-open-invisible #'delete-overlay)
        (overlay-put o 'display
                     (propertize
                      (format "--- %s ---"
                              (or (save-excursion
                                    (goto-char (car bounds))
                                    (and (re-search-forward "^title:[ \t]*\\(.*\\)$"
                                                            (cdr bounds) t)
                                         (string-trim (match-string 1) "\"" "\"")))
                                  "frontmatter"))
                      'face 'shadow))))))

(defun my/markdown--delegate (key fallback)
  "Run whatever KEY would do without `my/markdown-extras-mode' in the way.
Resolved dynamically rather than against a named keymap, so this works
whether the buffer is in `markdown-ts-mode' or `markdown-mode'."
  (let* ((my/markdown-extras-mode nil)          ; take our own map out of the lookup
         (cmd (or (key-binding key t) fallback)))
    (if (commandp cmd)
        (progn (setq this-command cmd) (call-interactively cmd))
      (call-interactively fallback))))

(defun my/markdown-tab ()
  "Fold or unfold frontmatter at point; otherwise behave as usual.
Org folds a drawer with TAB, so frontmatter answers to it too."
  (interactive)
  (if (my/markdown-in-frontmatter-p)
      (my/markdown-toggle-frontmatter)
    (my/markdown--delegate (kbd "TAB") #'indent-for-tab-command)))

(defun my/markdown-previous-line (&optional arg try-vscroll)
  "Like `previous-line', but able to escape a tall inline image.

markdown-ts-mode renders an inline image as an `after-string' on the
trailing newline of the link's line.  Moving up into such a display
confuses `line-move': it either pins point to that line or, once a goal
column is established across successive presses, lands *below* where it
started, so C-p oscillates between two lines instead of going up.  Moving
down is unaffected.

Hence the test is that point ended up strictly earlier, not merely that it
moved: fall back to a logical line move otherwise, which display strings
do not confuse."
  (interactive "^p\np")
  (let ((start (point)))
    (ignore-errors (line-move (- (or arg 1)) nil nil try-vscroll))
    (when (and (>= (point) start) (not (bobp)))
      (goto-char start)
      (forward-line (- (or arg 1))))))

(defvar my/markdown-extras-mode-map
  (let ((map (make-sparse-keymap)))
    ;; TAB only. markdown-ts-mode binds C-c C-c to its own checkbox toggle
    ;; and TAB to outline cycling; we intercept TAB purely so frontmatter
    ;; folds like an org drawer, and defer to the mode otherwise.
    (define-key map (kbd "TAB") #'my/markdown-tab)
    (define-key map (kbd "C-p") #'my/markdown-previous-line)
    (define-key map (kbd "<up>") #'my/markdown-previous-line)
    map)
  "Keymap for PKM markdown notes.")

(define-minor-mode my/markdown-extras-mode
  "Buffer-local conveniences for markdown notes in the PKM.

A minor mode rather than `local-set-key': the latter mutates
`current-local-map', which for a markdown buffer is the shared
`markdown-mode-map', so its bindings would leak into every markdown
file rather than just notes under `my/notes-dir'."
  :lighter " PKM"
  :keymap my/markdown-extras-mode-map)

;; PKM setup that applies to markdown notes the way init-org's does to org.
(defun my/markdown-setup ()
  "Conveniences for editing any markdown file.
Frontmatter folding, TAB handling and bounded inline images are useful in
a blog post or a README, not just in a note, so they are not gated on
`my/notes-dir'."
  ;; NB: not orgtbl-mode. It rewrites the GFM delimiter row `|---|---|' into
  ;; org's `|---+---|', which is not valid GFM: pandoc and GitHub then render
  ;; the table as a paragraph of literal text. It realigns on TAB, so merely
  ;; tabbing through a table would destroy it. markdown-ts-mode has a native
  ;; GFM table mode; markdown-mode's own commands keep the pipes too.
  (my/markdown-extras-mode 1)
  ;; Inline images, matching org's startup-with-link-previews behaviour,
  ;; bounded the same way. The two markdown modes spell this differently.
  (if (derived-mode-p 'markdown-ts-mode)
      (progn
        (setq-local markdown-ts-image-max-width my/pkm-inline-image-width)
        (setq-local markdown-ts-inline-images t)
        (when (fboundp 'markdown-ts--set-inline-images)
          (ignore-errors (markdown-ts--set-inline-images t))))
    (setq-local markdown-max-image-size
                (cons my/pkm-inline-image-width
                      (round (* my/pkm-inline-image-width 0.75))))
    (when (fboundp 'markdown-display-inline-images)
      (ignore-errors (markdown-display-inline-images))))
  ;; Bullets for list markers, boxes for checkboxes, a rule for thematic
  ;; breaks. markdown-ts-unordered-list-marker and friends only take effect
  ;; when markup is hidden, so it is this one switch that turns them on.
  (when (and my/markdown-hide-markup (derived-mode-p 'markdown-ts-mode))
    (setq-local markdown-ts-hide-markup t)
    (when (fboundp 'markdown-ts--set-hide-markup)
      (ignore-errors (markdown-ts--set-hide-markup t))))
  (when my/markdown-fold-frontmatter
    (my/markdown-toggle-frontmatter)))

(defun my/pkm-journal-capture-template ()
  "Capture template text matching the journal's own format."
  (if (eq (gco-pkm-format-for-dir (gco-pkm-journal-dir)) 'md)
      "- %?"
    "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"))

(defun my/pkm-markdown-setup ()
  "PKM-specific markdown setup, for notes under `my/notes-dir'.
Only the parts that depend on the PKM's layout: pasted and dropped images
belong in its flat assets/ directory, which makes no sense elsewhere."
  (when (and buffer-file-name
             (file-in-directory-p buffer-file-name my/notes-dir))
    ;; Same key markdown-mode registers under, so this replaces its handler.
    (when (fboundp 'yank-media-handler)
      (yank-media-handler "image/.*" #'my/pkm-md-image-yank-handler))
    (setq-local dnd-protocol-alist
                (append (list (cons "^file:///" #'my/pkm-md-dnd-handler)
                              (cons "^file:/[^/]" #'my/pkm-md-dnd-handler)
                              (cons "^file:[^/]" #'my/pkm-md-dnd-handler))
                        dnd-protocol-alist))))

(dolist (hook '(markdown-mode-hook markdown-ts-mode-hook))
  (add-hook hook #'my/markdown-setup)
  (add-hook hook #'my/pkm-markdown-setup))



(defun my/org-refresh-faces ()
  "Refresh mixed-pitch after tweaking faces."
  (when (derived-mode-p 'org-mode)
    (mixed-pitch-mode -1)
    (mixed-pitch-mode 1)))

(add-hook 'after-setting-font-hook #'my/org-refresh-faces)

;; Show full links for editing when point is over them
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  ;; Reveal links and emphasis markers at point
  (setq org-appear-autolinks t     ;; expand [[links][desc]] when point enters
        org-appear-autosubmarkers t ;; also expand *bold*, /italic/ markers
        org-appear-autoentities t   ;; show \alpha etc.
        org-appear-delay 0.1))      ;; small delay so it feels smooth


;; Org query language. Searches in ~org-agenda-files~.
(use-package org-ql
  :config
  ;; Create org-ql view for recent journals
  (require 'org-ql-view)
  (setopt org-ql-views
          (append org-ql-views
                  '(("Recent Journal Entries"
                     :buffers-files org-agenda-files
                     :query (and (path "journal.org")
                                 (level 3)
                                 (ts-active :from -30))
                     :title "Recent Journal Entries (Last 30 Days)"
                     :sort (date reverse)))))
  )

;; Images
(use-package org-remoteimg
  :ensure (:host github :repo "gaoDean/org-remoteimg")
  :after org
  :config
  (setopt org-display-remote-inline-images 'cache)
  )

(use-package org-imgtog    ; toggle images off when cursor enters them
  :ensure (:host github :repo "gaoDean/org-imgtog")
  :hook org-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org agenda/supertag PKM/second-brain/note-taking setup:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Exporting source blocks to HTML needs this
(use-package htmlize)
;; Live preview of HTML exports:
;; (Use org-preview-html-mode)
(use-package org-preview-html
  :commands org-preview-html-mode
  :config
  (setq org-preview-html-viewer 'xwidget))

(defun go/verify-refile-target ()
  "Exclude TODOS as refile targets."
  (not (member (nth 2 (org-heading-components)) (list "TODO" "DONE"))))
(setq org-refile-target-verify-function 'go/verify-refile-target)

;; Journal target function is now provided by gco-pkm package

;; C-c c j/n/t/f
(setq org-capture-templates
      '(("j" "Journal"
         ;; `plain', not `entry': the journal is markdown now, and an org
         ;; `entry' capture would insert a `*' heading and a :PROPERTIES:
         ;; drawer into it.  The template follows the journal's own format.
         plain
         (file (gco-pkm-journal--path-for-date))
         (function my/pkm-journal-capture-template)
         :empty-lines 1 :unnarrowed t)
        ("n" "Note" entry
         (file org-default-notes-file)
         "* %U - %?\n")
        ("t" "TODO" entry
         (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  SCHEDULED: %t\n")
        ("f" "New File Note" plain
         (file (lambda () (read-file-name "Note file: " org-directory nil nil ".org")))
         "#+title: %^{Title}\n\n%?\n")
        ("i" "ID Node" plain
         (function org-node-capture-target)
         "%?\n"
         :empty-lines-before 1)
        )
      )

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

;; Auto regenerate agenda when files change - use inotify
;; Debounce to avoid repeated expensive rebuilds on rapid saves
(defvar gco-org-agenda--notify-timer nil
  "Timer for debouncing agenda file-notify events.")

(defun gco-org-agenda-file-notify (_event)
  "Rebuild all agenda buffers after a debounce delay."
  (when gco-org-agenda--notify-timer
    (cancel-timer gco-org-agenda--notify-timer))
  (setq gco-org-agenda--notify-timer
        (run-with-idle-timer
         2 nil
         (lambda ()
           (setq gco-org-agenda--notify-timer nil)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when (derived-mode-p 'org-agenda-mode)
                 (org-agenda-redo t))))))))

;; Add notifications for appointments
(use-package appt
  :ensure nil
  :config
  (appt-activate t)
  (setq appt-display-mode-line t
        appt-display-interval 5
        appt-message-warning-time 10)
  (add-hook 'org-agenda-finalize-hook 'org-agenda-to-appt)
  )


;;; Prettify org-mode buffers

;; Use variable-pitch mode and use bullet symbols for bullet lists
;; with ~+~ and ~-~.

;; Tried this but it de-indents content when using indent mode
;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode))

;; Use utf-8 bullets for bullet lists -- this isn't great, but a bit nicer than nothing.
;; Ideally should use monospace font for spaces before bullet item, and use different bullets by list level.
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

;; De-emphasize the tildes org-mode uses for source snippets by making them
;; small.  Match only paired ~code~ delimiters, not lone tildes in prose.
(defface org-tilde-face
  '((t :inherit default :height 0.5))
  "Face for highlighting tildes in org-mode")
(font-lock-add-keywords 'org-mode
                        '(("\\(~\\)[^~ \n]\\(?:[^~\n]*[^~ \n]\\)?\\(~\\)"
                           (1 'org-tilde-face) (2 'org-tilde-face))))

;; Make property drawers less obtrusive
(custom-set-faces
 '(org-drawer ((t (:inherit shadow :height 0.7))))
 '(org-property-value ((t (:inherit shadow :height 0.7)))))



;;; org-node -- fast node-based PKM (replaces org-roam)

;; org-mem: indexing backend for org-node
(use-package org-mem
  :custom
  (org-mem-watch-dirs (list my/notes-dir))
  (org-mem-do-sync-with-org-id t)
  :config
  (org-mem-updater-mode))

;; org-node: core node navigation, linking, backlinks, completion
;; org-node-seq setup is in :config (not with-eval-after-load) so it
;; runs after org-node-cache-mode is enabled.
(use-package org-node
  :after org-mem
  :demand t
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-node-creation-fn #'org-capture)
  (org-node-slug-fn #'org-node-slugify-for-web)
  (org-node-datestamp-format "")
  (org-node-file-directory-ask t)
  (org-node-display-sort-fn #'org-node-sort-by-file-mtime)
  (org-node-backlink-lazy nil)
  :bind (("C-c n f" . org-node-find)
         ("C-c n i" . org-node-insert-link)
         ("C-c n l" . org-node-insert-link*)
         ("C-c n s" . org-node-grep)
         ("C-c n b" . org-node-context-dwim)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-node-cache-mode 1)
  (org-node-backlink-mode 1)
  (org-node-context-follow-mode 1)
  ;; Don't use the global mode -- it adds org-node capf at default
  ;; priority, so it matches every word and floods corfu with node
  ;; titles.  Instead, add it manually with depth 90 (very low
  ;; priority) so dabbrev/keyword/etc. are tried first.
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        #'org-node-complete-at-point 90 t)))
  ;; org-node-seq: sequences (daily journal navigation, calendar marks)
  ;; Bundled inside org-node package; must be set up after cache-mode
  (require 'org-node-seq)
  (setopt org-node-seq-defs
          (list (org-node-seq-def-on-filepath-sort-by-basename
                 "d" "Daily journals"
                 (expand-file-name "journals" my/notes-dir)
                 nil nil)))  ; no capture template, no date-picker (use completing-read)
  (setopt org-node-seq-that-marks-calendar "d")
  (org-node-seq-mode)
  (message "Set up org-node in %s" (car org-mem-watch-dirs))
  (add-to-list 'display-buffer-alist
               '("\\*org-node context\\*"
                 (display-buffer-in-direction)
                 (direction . bottom)
                 (window-height . 0.25))))



;;; Helper functions for workflows

;; PKM helper functions moved to gco-pkm package

;; PKM keybindings are now set up in gco-pkm package

(provide 'init-org)
