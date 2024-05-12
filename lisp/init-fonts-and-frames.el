;;; init-fonts-and-frames.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; frame title, with WSL indicator
(let ((base-frame-title-format '("[%b] - " system-name " - Emacs " emacs-version)))
  (cond (wsl1-p
         (setq frame-title-format (append base-frame-title-format '(" (WSL1)"))))
        (wsl2-p
         (setq frame-title-format (append base-frame-title-format '(" (WSL2)"))))
        (t
         (setq frame-title-format base-frame-title-format)))
  )

;;; FONTS

;; Notes:
;; use M-x describe-font RET to describe current font
;; C-u C-x = describes font under point (and lots of other goodies).
;; To list all fonts, in *scratch* buffer do (print (font-family-list))
;; To test a font, use Options menu -> Set Default Font...
(defvar preferred-fonts
      '(
        ("Hack" . 10.5) ; my new fave as of 2019 (very similar to DV Sans Mono)
	("DejaVu Sans Mono" . 10)       ; better ~ than Droid Sans Dotted Mono
	;; Droid Sans Mono: quite nice.
	;; 15 pixels total height at 10 point.  Clear & crisp.
	;; (e.g. http://www.fontex.org/download/Droid-sans-mono.ttf)
	("Droid Sans Mono Dotted" . 10)
	("Droid Sans Mono" . 10)
	;; Consolas: download installer from Microsoft.
	;; Quite beautiful and renders nicely, but a little light.
	;; Pretty similar to Droid Sans Mono.
	;; The slanted verticals on the capital M annoy me a little.
	;; (16 pixels height)
	("Consolas" . 10.5)
	;; Inconsolata: lots of people like this.
	;; http://www.levien.com/type/myfonts/inconsolata.html:
	;; about same size as Consolas-10.5, but thicker and less leading
	;; (17 pixels height) and not as smooth lines.  Feels chunky.
	("Inconsolata" . 12)
	;; default
	("Courier New" . 10.5)
        ("Courier" . 10)))

;; Mac: use larger fonts
(when-mac
 (setq preferred-fonts '(("Hack" . 13)
                         ("DejaVu Sans Mono" . 13)
                         ("Droid Sans Mono Dotted" . 13)
			 ("Courier New" . 13))))

(defun font-exists-p (font-name &optional frame)
  "Does this font exist? Returns font or nil."
  (find-font (font-spec :family font-name) frame))

(defun use-font (name size &optional frame)
  "Use font NAME at height SIZE (in points, float or int).
   FRAME of nil means all existing + new.
   Returns t if font exists and was set, else nil."
  (when (font-exists-p name)
    (set-face-attribute 'default frame :family name :height (round (* size 10)))
    (face-all-attributes 'default)))

(defun my-dpi (&optional frame)
  "Get the DPI of FRAME (or current if nil)."
  (cl-flet ((pyth (lambda (w h)
                    (sqrt (+ (* w w)
                             (* h h)))))
            (mm2in (lambda (mm)
                     (/ mm 25.4))))
    (let* ((atts (frame-monitor-attributes frame))
           (pix-w (cl-fourth (assoc 'geometry atts)))
           (pix-h (cl-fifth (assoc 'geometry atts)))
           (pix-d (pyth pix-w pix-h))
           (mm-w (cl-second (assoc 'mm-size atts)))
           (mm-h (cl-third (assoc 'mm-size atts)))
           (mm-d (pyth mm-w mm-h)))
      (/ pix-d (mm2in mm-d)))))

;; Note: display-graphic-p returns false when emacs is started in daemon mode,
;; so we do much of the frame setup in the new-frame-setup hook, which is called
;; after the new frame is created but before it's selected. That means we have to
;; use 'frame' everywhere here, not assume selected-frame is valid.
;; Note: for testing, use (selected-frame) to get the current frame.
(defun new-frame-setup (frame)
  "Set default font and frame attributes for FRAME."
  (when (display-graphic-p frame)
    (tool-bar-mode 0)
    ;; (message "Setting up new graphic frame %s, current geom %s" frame (frame-geometry frame))
    (let ((font-info (cl-find-if (lambda (x) (font-exists-p (car x) frame))
                                 preferred-fonts)))
      (when font-info
	(message "Using font %s, at %.2f dpi" font-info (my-dpi))
	(use-font (car font-info) (cdr font-info))
        (set-frame-width frame 100)
        (set-frame-height frame 48)
	))))

;; run on existing frames (non-daemon startup)
(mapc 'new-frame-setup (frame-list))
;;; run when new frames created (daemon or server)
(add-hook 'after-make-frame-functions 'new-frame-setup)

;; I like italic comment face as long as the actual font supports it
;; (which Hack does)
(set-face-italic font-lock-comment-face t)

(pixel-scroll-precision-mode t)

;;; Variable/Mixed Pitch fonts:

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :diminish mixed-pitch-mode
  )

;; Geneva works & looks good on Mac
;; or try Lucida Grande
(cond ((find-font (font-spec :name "Lucida Grande"))
       (set-face-attribute 'variable-pitch nil :font "Lucida Grande" :weight 'light :height 1.3))
      ((find-font (font-spec :name "Verdana"))
       (set-face-attribute 'variable-pitch nil :font "Verdana" :weight 'light :height 1.3))
      ((find-font (font-spec :name "Times New Roman"))
       (set-face-attribute 'variable-pitch nil :font "Times New Roman" :weight 'light :height 1.3))
      )


;;; to display Unicode math chars, like math A to z (𝐴 .. 𝑧, #x1D434 .. #x1D467)
;;; and pi: #1D70B = 𝜋
;;; Cambria and Segoe UI Symbol should both work on Windows, but Emacs
;;; may pick up some other inappropriate font.
(when-windows
 (set-fontset-font t 'mathematical "Segoe UI Symbol"))

;; Useful things for chars, fonts and fontsets:
;;  M-x describe-fontset
;;  C-u C-x = ; char details at point
;;  C-x 8 RET ; insert char by name or unicode (hex)
;;  var script-representative-chars: list of all (most?) Unicode script ranges with "representative" chars
;;  See https://lists.gnu.org/archive/html/help-gnu-emacs/2021-09/txtRLYx8BDBtJ.txt for useful math fontset test code
;;  As of 2024, Emacs 30 on Windows does not support color emojis, just black & white.

(provide 'init-fonts-and-frames)
