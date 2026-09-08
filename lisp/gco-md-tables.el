;;; gco-md-tables.el --- Rendered pipe tables in markdown-ts-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Each pipe table in a markdown-ts-mode buffer is shown as md-render's
;; box-drawn rendering while point is elsewhere, and as its raw source
;; while point is inside it.  The buffer text is never touched: the
;; rendering lives in `display' properties on overlays, so editing,
;; saving, undo and tree-sitter see only the source.
;;
;; Two kinds of overlay do the work.  One overlay spans the whole table:
;; it tracks the table's bounds, caches the rendering, and zeroes
;; `line-spacing' so the vertical borders join up.  While the table is
;; hidden, one overlay per source row carries that row's rendered line
;; as its `display'.  The buffer's own newlines stay between them; that
;; matters because Emacs ignores `line-spacing' inside a display string,
;; and because it lets the cursor step through the rows.
;;
;; md-render.el (from yibie/md-mode, itself derived from xenodium's
;; agent-shell-markdown.el) does the drawing: Unicode borders, header
;; and zebra faces, column alignment and wrapping to the window width.
;; Its faces are `md-render-table-header', `md-render-table-border' and
;; `md-render-table-zebra'.
;;
;;; Code:

(require 'md-render)
(require 'treesit)

(defgroup gco-md-tables nil
  "Rendered pipe tables in markdown-ts-mode."
  :group 'markdown-ts)

(defcustom gco-md-tables-refresh-delay 0.2
  "Idle seconds after an edit or window resize before tables are re-synced."
  :type 'number)

(defvar-local gco-md-tables--overlays nil
  "Overlays spanning the tables in this buffer, one per table.")

(defvar-local gco-md-tables--timer nil
  "Pending idle refresh, or nil.")

(defvar-local gco-md-tables--tick nil
  "Value of `buffer-chars-modified-tick' when overlays were last synced.
While it differs from the current tick the overlay bounds may be stale.")

(defvar gco-md-tables--query nil
  "Compiled tree-sitter query matching pipe tables.")

(defvar gco-md-tables-mode)

;;;; Locating tables

(defun gco-md-tables--bounds ()
  "Return (START . END) for every pipe table in the buffer.
END excludes the newline that terminates the last row."
  (unless gco-md-tables--query
    (setq gco-md-tables--query
          (treesit-query-compile 'markdown '((pipe_table) @table))))
  (mapcar (lambda (node)
            (cons (treesit-node-start node)
                  (save-excursion
                    (goto-char (treesit-node-end node))
                    (skip-chars-backward "\n")
                    (point))))
          (treesit-query-capture 'markdown gco-md-tables--query nil nil t)))

(defun gco-md-tables--overlay-overlapping (start end)
  "Return the table overlay overlapping START..END, if any."
  (seq-find (lambda (ov)
              (and (<= (overlay-start ov) end)
                   (>= (overlay-end ov) start)))
            gco-md-tables--overlays))

(defun gco-md-tables--sync ()
  "Give every table an overlay with current bounds; drop the rest."
  (let ((kept nil))
    (pcase-dolist (`(,start . ,end) (gco-md-tables--bounds))
      (let ((ov (gco-md-tables--overlay-overlapping start end)))
        (if ov
            (progn
              (move-overlay ov start end)
              (setq gco-md-tables--overlays (delq ov gco-md-tables--overlays)))
          (setq ov (make-overlay start end))
          (overlay-put ov 'gco-md-tables t)
          (overlay-put ov 'line-spacing 0))
        (push ov kept)))
    (dolist (ov gco-md-tables--overlays)
      (gco-md-tables--reveal ov)
      (delete-overlay ov))
    (setq gco-md-tables--overlays (nreverse kept)
          gco-md-tables--tick (buffer-chars-modified-tick))))

;;;; Rendering

(defun gco-md-tables--render (source window)
  "Return SOURCE, a pipe table, rendered as propertized text for WINDOW.
md-render measures cell widths by briefly inserting probe text into
WINDOW's buffer, which must not leave a trace in the undo list."
  (let ((buffer-undo-list t)
        (md-render-render-functions nil))
    (with-selected-window window
      (with-temp-buffer
        (insert source)
        (md-render-replace-markup :render-images nil)
        (string-trim-right (buffer-string) "\n+")))))

;; md-render decides whether a cell fits its column by summing per-char
;; widths scaled by each face's measured pixel ratio (inline code under
;; mixed-pitch is 8/9 of fixed-pitch here) and comparing that float with
;; the integer column width.  Rounding noise -- 27.000000000000004 against
;; 27 -- wraps a cell that fits exactly, so allow a hair of slack.
(defun gco-md-tables--tolerate-rounding (width)
  "Return WIDTH minus a rounding-error margin."
  (if (floatp width) (- width 1e-6) width))

(advice-add 'md-render--table-wrap-string-width :filter-return
            #'gco-md-tables--tolerate-rounding)

(defun gco-md-tables--layout-key (window)
  "Return what the rendering of a table depends on, besides its source."
  (list (window-body-width window)
        (bound-and-true-p text-scale-mode-amount)))

(defun gco-md-tables--ensure-rendered (ov window)
  "Make sure OV caches a rendering of its current source for WINDOW."
  (let ((source (buffer-substring-no-properties (overlay-start ov)
                                                (overlay-end ov)))
        (layout (gco-md-tables--layout-key window)))
    (unless (and (equal source (overlay-get ov 'gco-md-tables-source))
                 (equal layout (overlay-get ov 'gco-md-tables-layout)))
      ;; Rendering probes the buffer and so bumps the modification tick
      ;; without moving anything; keep the sync state it found.
      (let ((synced (eql gco-md-tables--tick (buffer-chars-modified-tick))))
        (overlay-put ov 'gco-md-tables-source source)
        (overlay-put ov 'gco-md-tables-layout layout)
        (overlay-put ov 'gco-md-tables-rendered
                     (gco-md-tables--render source window))
        (when synced
          (setq gco-md-tables--tick (buffer-chars-modified-tick)))))))

;;;; Showing and hiding

(defun gco-md-tables--row-strings (rendered nrows)
  "Split RENDERED into NROWS strings, one per source row.
Wrapped cells make the rendering longer than the source.  The surplus
lines ride along on the last row, which looks the same on screen."
  (let ((lines (split-string rendered "\n")))
    (append (seq-take lines (1- nrows))
            (list (string-join (seq-drop lines (1- nrows)) "\n")))))

(defun gco-md-tables--hidden-p (ov)
  "Return non-nil if the table under OV shows its rendering."
  (overlay-get ov 'gco-md-tables-rows))

(defun gco-md-tables--reveal (ov)
  "Show the source of the table under OV."
  (mapc #'delete-overlay (overlay-get ov 'gco-md-tables-rows))
  (overlay-put ov 'gco-md-tables-rows nil))

(defun gco-md-tables--hide (ov window)
  "Cover each source row of the table under OV with its rendered line."
  (gco-md-tables--ensure-rendered ov window)
  (gco-md-tables--reveal ov)
  (let ((rows nil))
    (save-excursion
      (goto-char (overlay-start ov))
      (dolist (text (gco-md-tables--row-strings
                     (overlay-get ov 'gco-md-tables-rendered)
                     (count-lines (overlay-start ov) (overlay-end ov))))
        (let ((row (make-overlay (point) (line-end-position))))
          (overlay-put row 'gco-md-tables t)
          (overlay-put row 'display text)
          (push row rows))
        (forward-line 1)))
    (overlay-put ov 'gco-md-tables-rows rows)))

(defun gco-md-tables--point-in-p (ov)
  "Return non-nil if point is within the table under OV, ends included."
  (<= (overlay-start ov) (point) (overlay-end ov)))

(defun gco-md-tables--update (&optional check-all)
  "Show the source of the table at point and the rendering of the rest.
Tables already rendered are left alone unless CHECK-ALL is non-nil,
which re-examines them after an edit or a change of window width."
  (let ((window (get-buffer-window (current-buffer) 'visible)))
    (dolist (ov gco-md-tables--overlays)
      (cond ((gco-md-tables--point-in-p ov)
             (gco-md-tables--reveal ov))
            ((and window (or check-all (not (gco-md-tables--hidden-p ov))))
             (gco-md-tables--hide ov window))))))

;;;; Scheduling

(defun gco-md-tables--refresh (buffer)
  "Sync and redisplay the tables in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq gco-md-tables--timer nil)
      (when gco-md-tables-mode
        (gco-md-tables--sync)
        (gco-md-tables--update t)))))

(defun gco-md-tables--schedule (&optional delay)
  "Refresh once Emacs has been idle for DELAY seconds.
A pending refresh is reused, so a burst of edits or resize steps costs
one refresh at the end."
  (unless gco-md-tables--timer
    (setq gco-md-tables--timer
          (run-with-idle-timer (or delay gco-md-tables-refresh-delay) nil
                               #'gco-md-tables--refresh (current-buffer)))))

(defun gco-md-tables--post-command ()
  "Reveal the table at point; hide the others once bounds are trustworthy."
  (if (eql gco-md-tables--tick (buffer-chars-modified-tick))
      (gco-md-tables--update)
    ;; Edited since the last sync: overlay bounds may be stale, so only
    ;; reveal now and leave hiding to the refresh.
    (dolist (ov gco-md-tables--overlays)
      (when (gco-md-tables--point-in-p ov)
        (gco-md-tables--reveal ov)))
    (gco-md-tables--schedule)))

(defun gco-md-tables--teardown ()
  "Remove overlays, hooks and timers."
  (when gco-md-tables--timer
    (cancel-timer gco-md-tables--timer)
    (setq gco-md-tables--timer nil))
  (remove-hook 'post-command-hook #'gco-md-tables--post-command t)
  (remove-hook 'window-configuration-change-hook #'gco-md-tables--schedule t)
  (remove-hook 'text-scale-mode-hook #'gco-md-tables--schedule t)
  (remove-hook 'change-major-mode-hook #'gco-md-tables--teardown t)
  (remove-overlays (point-min) (point-max) 'gco-md-tables t)
  (setq gco-md-tables--overlays nil
        gco-md-tables--tick nil))

;;;###autoload
(define-minor-mode gco-md-tables-mode
  "Show pipe tables rendered, except the one point is in."
  :lighter " ▦"
  (gco-md-tables--teardown)
  (when gco-md-tables-mode
    (unless (derived-mode-p 'markdown-ts-mode)
      (setq gco-md-tables-mode nil)
      (user-error "gco-md-tables-mode needs markdown-ts-mode"))
    (add-hook 'post-command-hook #'gco-md-tables--post-command nil t)
    (add-hook 'window-configuration-change-hook #'gco-md-tables--schedule nil t)
    (add-hook 'text-scale-mode-hook #'gco-md-tables--schedule nil t)
    ;; Local hooks vanish with the major mode, but overlays would not.
    (add-hook 'change-major-mode-hook #'gco-md-tables--teardown nil t)
    ;; Deferred, not immediate: from a mode hook the buffer is not yet
    ;; displayed, and rendering needs its window to measure text.
    (gco-md-tables--schedule 0)))

(provide 'gco-md-tables)
;;; gco-md-tables.el ends here
