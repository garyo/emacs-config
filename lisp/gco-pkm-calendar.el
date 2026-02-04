;;; gco-pkm-calendar.el --- Calendar-based journal browsing for PKM -*- lexical-binding: t; -*-

;; Author: Gary Oberbrunner
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (gco-pkm "0.1"))
;; Keywords: outlines, convenience, org, pkm, calendar

;;; Commentary:
;; Browse journal entries using the built-in Emacs calendar.
;; Dates with journal files are visually marked, and a journal
;; preview is shown below the calendar as you navigate.
;; Press RET to open the journal file for editing.
;;
;; Entry point: `gco-pkm-calendar-browse'
;;
;; Keybindings (active only during PKM calendar session):
;;   RET - Open/create journal for date under cursor
;;   SPC - Toggle journal window
;;   q   - Quit and clean up

;;; Code:

(require 'calendar)
(require 'gco-pkm)

;;;; Face

(defface gco-pkm-calendar-journal-mark
  '((t :inherit success :weight bold :underline t))
  "Face for calendar dates that have journal entries."
  :group 'gco-pkm)

;;;; Internal State

(defvar gco-pkm-calendar--active nil
  "Non-nil when the calendar was opened via `gco-pkm-calendar-browse'.")

(defvar gco-pkm-calendar--journal-dates-cache nil
  "Hash table mapping \"YYYY-MM-DD\" strings to t for existing journals.")

(defvar gco-pkm-calendar--saved-bindings nil
  "Alist of (KEY . ORIGINAL-BINDING) for keys overridden during PKM session.")

(defvar gco-pkm-calendar--journal-visible t
  "Non-nil when the journal window should be shown.")

(defvar gco-pkm-calendar--journal-window nil
  "The window displaying the journal preview, or nil.")

(defvar gco-pkm-calendar--journal-file nil
  "Path of the journal file currently shown in the preview, or nil.")

(defconst gco-pkm-calendar--preview-buffer-name "*PKM Journal Preview*"
  "Name of the persistent preview buffer.")

(defconst gco-pkm-calendar--calendar-height 9
  "Number of lines for the calendar window (header + 6 week rows + mode-line).")

;;;; Date Scanning

(defun gco-pkm-calendar--scan-journal-dates ()
  "Scan journal directory and return a hash table of existing dates.
Keys are \"YYYY-MM-DD\" strings, values are t."
  (let ((table (make-hash-table :test 'equal))
        (dir (gco-pkm-journal-dir)))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir nil "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$"))
        (puthash (file-name-sans-extension file) t table)))
    table))

;;;; Date Marking

(defun gco-pkm-calendar--mark-journal-dates ()
  "Mark visible calendar dates that have journal entries.
Intended for `calendar-today-visible-hook' and
`calendar-today-invisible-hook'."
  (when gco-pkm-calendar--active
    (setq gco-pkm-calendar--journal-dates-cache
          (gco-pkm-calendar--scan-journal-dates))
    (maphash
     (lambda (date-str _v)
       (let* ((parts (split-string date-str "-"))
              (year (string-to-number (nth 0 parts)))
              (month (string-to-number (nth 1 parts)))
              (day (string-to-number (nth 2 parts)))
              (date (list month day year)))
         (when (calendar-date-is-visible-p date)
           (calendar-mark-visible-date date 'gco-pkm-calendar-journal-mark))))
     gco-pkm-calendar--journal-dates-cache)))

;;;; Journal Display

(defun gco-pkm-calendar--date-to-string (date)
  "Convert calendar DATE (month day year) to \"YYYY-MM-DD\" string."
  (format "%04d-%02d-%02d" (nth 2 date) (nth 0 date) (nth 1 date)))

(defun gco-pkm-calendar--journal-exists-p (date)
  "Return non-nil if a journal file exists for calendar DATE."
  (let ((key (gco-pkm-calendar--date-to-string date)))
    (or (and gco-pkm-calendar--journal-dates-cache
             (gethash key gco-pkm-calendar--journal-dates-cache))
        (file-exists-p (gco-pkm-journal--path-for-date date)))))

(defun gco-pkm-calendar--get-preview-buffer ()
  "Return the preview buffer, creating it and setting `org-mode' once."
  (let ((buf (get-buffer gco-pkm-calendar--preview-buffer-name)))
    (unless buf
      (setq buf (get-buffer-create gco-pkm-calendar--preview-buffer-name))
      (with-current-buffer buf
        (org-mode)
        (setq buffer-read-only t)))
    buf))

(defun gco-pkm-calendar--show-journal (date)
  "Show journal preview for DATE in a window below the calendar.
Uses a single persistent `org-mode' buffer to avoid repeated mode setup."
  (let* ((file (gco-pkm-journal--path-for-date date))
         (cal-win (selected-window)))
    ;; Only update buffer contents when the file changes
    (unless (equal file gco-pkm-calendar--journal-file)
      (setq gco-pkm-calendar--journal-file file)
      (let ((buf (gco-pkm-calendar--get-preview-buffer)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-file-contents file)))))
    ;; Reuse existing journal window if live, otherwise split below
    (let ((buf (gco-pkm-calendar--get-preview-buffer)))
      (if (and gco-pkm-calendar--journal-window
               (window-live-p gco-pkm-calendar--journal-window))
          (set-window-buffer gco-pkm-calendar--journal-window buf)
        (setq gco-pkm-calendar--journal-window
              (display-buffer buf
                              '((display-buffer-below-selected)
                                (inhibit-same-window . t))))
        ;; Shrink calendar to fixed height, giving the rest to the journal
        (when (window-live-p cal-win)
          (with-selected-window cal-win
            (let ((window-min-height 1))
              (shrink-window (- (window-height) gco-pkm-calendar--calendar-height)))))))
    ;; Keep focus on calendar
    (select-window cal-win)))

(defun gco-pkm-calendar--hide-journal ()
  "Hide the journal window."
  (when (and gco-pkm-calendar--journal-window
             (window-live-p gco-pkm-calendar--journal-window))
    (delete-window gco-pkm-calendar--journal-window))
  (setq gco-pkm-calendar--journal-window nil)
  (setq gco-pkm-calendar--journal-file nil))

(defun gco-pkm-calendar--update-journal ()
  "Update the journal display based on the date under cursor.
Intended for `calendar-move-hook'."
  (when gco-pkm-calendar--active
    (let ((date (calendar-cursor-to-date t)))
      (when date
        (if (and gco-pkm-calendar--journal-visible
                 (gco-pkm-calendar--journal-exists-p date))
            (gco-pkm-calendar--show-journal date)
          (gco-pkm-calendar--hide-journal))))))

;;;; Keybindings

(defun gco-pkm-calendar--setup-keybindings ()
  "Install PKM keybindings in `calendar-mode-map', saving originals."
  (setq gco-pkm-calendar--saved-bindings nil)
  (dolist (pair '(([return]  . gco-pkm-calendar-open-journal)
                  (?\r       . gco-pkm-calendar-open-journal)
                  (?\s       . gco-pkm-calendar-toggle-journal)
                  (?q        . gco-pkm-calendar-quit)))
    (let ((key (car pair))
          (cmd (cdr pair)))
      (push (cons key (lookup-key calendar-mode-map (if (vectorp key) key (vector key))))
            gco-pkm-calendar--saved-bindings)
      (define-key calendar-mode-map (if (vectorp key) key (vector key)) cmd))))

(defun gco-pkm-calendar--restore-keybindings ()
  "Restore original `calendar-mode-map' bindings."
  (dolist (pair gco-pkm-calendar--saved-bindings)
    (let ((key (car pair))
          (cmd (cdr pair)))
      (define-key calendar-mode-map
                  (if (vectorp key) key (vector key))
                  cmd)))
  (setq gco-pkm-calendar--saved-bindings nil))

;;;; Interactive Commands

;;;###autoload
(defun gco-pkm-calendar-open-journal ()
  "Open the journal for the date under cursor.
If no journal exists, offer to create one."
  (interactive)
  (let ((date (calendar-cursor-to-date t)))
    (unless date
      (user-error "No date under cursor"))
    (let ((file (gco-pkm-journal--path-for-date date)))
      (if (file-exists-p file)
          (progn
            (gco-pkm-calendar--teardown)
            (calendar-exit)
            (find-file file))
        (when (y-or-n-p (format "No journal for %s. Create one? "
                                (gco-pkm-calendar--date-to-string date)))
          (gco-pkm-calendar--teardown)
          (calendar-exit)
          (find-file file)
          (when (= (buffer-size) 0)
            (let* ((time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
                   (date-str (format-time-string "%Y-%m-%d" time))
                   (date-day-str (format-time-string "%Y-%m-%d %a" time)))
              (insert (format "#+title: %s\n\n* <%s>\n" date-str date-day-str))
              (org-id-get-create)
              (save-buffer)))
          (goto-char (point-max))
          (unless (bolp) (insert "\n")))))))

;;;###autoload
(defun gco-pkm-calendar-toggle-journal ()
  "Toggle the journal window below the calendar."
  (interactive)
  (setq gco-pkm-calendar--journal-visible
        (not gco-pkm-calendar--journal-visible))
  (if gco-pkm-calendar--journal-visible
      (gco-pkm-calendar--update-journal)
    (gco-pkm-calendar--hide-journal)))

;;;###autoload
(defun gco-pkm-calendar-quit ()
  "Quit the PKM calendar session, cleaning up hooks and journal window."
  (interactive)
  (gco-pkm-calendar--teardown)
  (calendar-exit))

;;;; Lifecycle

(defun gco-pkm-calendar--setup ()
  "Set up hooks, keybindings, and state for PKM calendar browsing."
  ;; Guard against double-setup
  (when gco-pkm-calendar--active
    (gco-pkm-calendar--teardown))
  (setq gco-pkm-calendar--active t)
  (setq gco-pkm-calendar--journal-visible t)
  (setq gco-pkm-calendar--journal-dates-cache nil)
  (setq gco-pkm-calendar--journal-window nil)
  (setq gco-pkm-calendar--journal-file nil)
  ;; Install hooks
  (add-hook 'calendar-today-visible-hook #'gco-pkm-calendar--mark-journal-dates)
  (add-hook 'calendar-today-invisible-hook #'gco-pkm-calendar--mark-journal-dates)
  (add-hook 'calendar-move-hook #'gco-pkm-calendar--update-journal)
  ;; Keybindings installed after calendar buffer exists (deferred to entry point)
  ;; Advice calendar-exit for cleanup in case user exits via other means
  (advice-add 'calendar-exit :before #'gco-pkm-calendar--on-calendar-exit))

(defun gco-pkm-calendar--teardown ()
  "Remove hooks, restore keybindings, close journal window, clear state."
  (setq gco-pkm-calendar--active nil)
  ;; Remove hooks
  (remove-hook 'calendar-today-visible-hook #'gco-pkm-calendar--mark-journal-dates)
  (remove-hook 'calendar-today-invisible-hook #'gco-pkm-calendar--mark-journal-dates)
  (remove-hook 'calendar-move-hook #'gco-pkm-calendar--update-journal)
  ;; Restore keybindings
  (gco-pkm-calendar--restore-keybindings)
  ;; Remove advice
  (advice-remove 'calendar-exit #'gco-pkm-calendar--on-calendar-exit)
  ;; Close journal window and kill the preview buffer
  (gco-pkm-calendar--hide-journal)
  (let ((buf (get-buffer gco-pkm-calendar--preview-buffer-name)))
    (when buf (kill-buffer buf)))
  ;; Clear cache
  (setq gco-pkm-calendar--journal-dates-cache nil))

(defun gco-pkm-calendar--on-calendar-exit (&rest _args)
  "Advice for `calendar-exit' to clean up PKM state.
Only acts when `gco-pkm-calendar--active' is non-nil."
  (when gco-pkm-calendar--active
    (gco-pkm-calendar--teardown)))

;;;; Entry Point

;;;###autoload
(defun gco-pkm-calendar-browse ()
  "Browse journal entries using the Emacs calendar.
Opens the calendar with journal dates visually marked.  A preview
of the journal for the date under cursor is shown below the calendar.
Press RET to open the file for editing.  Standard calendar navigation
works as usual.

\\<calendar-mode-map>
Keybindings during PKM calendar session:
  \\[gco-pkm-calendar-open-journal] - Open/create journal for date under cursor
  \\[gco-pkm-calendar-toggle-journal] - Toggle journal window
  \\[gco-pkm-calendar-quit] - Quit and clean up"
  (interactive)
  (gco-pkm-calendar--setup)
  (calendar)
  (delete-other-windows)
  ;; Now the calendar buffer exists; install keybindings
  (gco-pkm-calendar--setup-keybindings)
  ;; Show journal for today if it exists
  (gco-pkm-calendar--update-journal))

(provide 'gco-pkm-calendar)
;;; gco-pkm-calendar.el ends here
