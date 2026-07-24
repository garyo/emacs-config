;;; initial-setup.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Don't use msys64 gpg on Windows -- prefer official install.
;; Msys64 version has problems setting --homedir arg with drive letter.
(if (file-directory-p "C:/Program Files (x86)/GnuPG")
    (setq
     epg-gpg-home-directory "C:/Program Files (x86)/GnuPG"
     epg-gpg-program (concat epg-gpg-home-directory "/bin/gpg.exe")
     epg-gpgconf-program  (concat epg-gpg-home-directory "/bin/gpgconf.exe")))

;;; Load treesit early.
;; Some tree-sitter package autoloads (e.g. astro-ts-mode) call
;; `treesit-ready-p' at the top level of their generated autoloads file,
;; which elpaca evaluates during package activation at startup. That
;; function lives in treesit.el and is not itself autoloaded, so it must
;; be available before packages are activated, else activation fails with
;; "void-function treesit-ready-p". treesit.el is cheap to load (it does
;; not load any grammars) and exists on any Emacs 29+ build.
(require 'treesit nil t)

(defun emacs-build-description-string ()
  "Run `emacs-build-description' in a temp buffer, return result as string."
  (with-temp-buffer
    (emacs-build-description)
    (buffer-string)))

(defvar startup-verbose-p t "More verbose messages during emacs startup")
(when startup-verbose-p
  (message "*****************
%s
config: %s
************************"
           (emacs-build-description-string)
           system-configuration-features))

;;; Check for fast JSON and native-comp

(defun has-fast-json ()
  "Return t if \"json-serialize\" is implemented as a C function.
This was done for Emacs 27 but not all builds include the C version,
which is a lot faster."
  (and
   (subrp (symbol-function 'json-serialize))
   ;; test that it works -- on Windows the DLL (or deps) may be missing
   (equal (json-serialize (json-parse-string "[123]")) "[123]")))

(unless (has-fast-json)
  (warn "❗This emacs is using older elisp json functions; maybe rebuild with libjansson?"))

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "★Native compilation is available!")
  (message "❗Native compilation is *not* available"))

;; Bug workaround, 2023-02-26:
(if (not (boundp 'comp-enable-subr-trampolines))
    (setq comp-enable-subr-trampolines native-comp-enable-subr-trampolines))

;;; GC and misc optimizations

;; Set up some low-level settings to prevent garbage collection as frequently, allow for more undos, and improve performance especially when using LSP.

;; Restore gc-cons-threshold from most-positive-fixnum (set in early-init.el)
;; to a reasonable value after startup. Also GC when idle.
(add-hook 'elpaca-after-init-hook
          (lambda () (setq gc-cons-threshold (* 100 1024 1024)))) ; 100MB
(run-with-idle-timer 10 t (lambda () (garbage-collect)))
;; undo is pruned on GC; allow more undos (10x default)
(setopt undo-limit 1600000)
(setopt undo-strong-limit (* 10 undo-limit))

(setq read-process-output-max (* 1024 1024)) ; improve LSP performance


(provide 'initial-setup)
