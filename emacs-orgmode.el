;;; -*-mode: emacs-lisp-*-
;;; Set up emacs for org-mode
;;; This can be loaded from .emacs, or used as the value of org-export-async-init-file.

;; Set up dirs
(dolist (filename '("~/emacs/org-mode"
		    "c:/emacs/site-lisp/org-mode"))
  (let ((f (expand-file-name filename)))
    (cond ((file-directory-p f)
	   (add-to-list 'load-path (concat f "/lisp"))
	   (add-to-list 'load-path (concat f "/contrib/lisp"))
	   (add-to-list 'Info-default-directory-list (concat f "/info"))
	   ))))

(condition-case ex
    (progn
      (require 'ox-reveal)
      (setq org-reveal-root "file:///c:/emacs/site-lisp/reveal.js-master")
      )
  ('error (message "No org-mode ox-reveal; OK, but no reveal.js presentation export available.")))

(custom-set-variables
 '(org-babel-load-languages (quote ((emacs-lisp . t)
				    (R . t)
				    (python . t)
				    (dot . t)
				    (ditaa . t)
				    (latex . t)
				    (sql . t)
				    (shell . t))))
 '(org-export-backends (quote (ascii html icalendar latex odt koma-letter)))
 '(org-export-with-sub-superscripts (quote {}))
 '(org-use-sub-superscripts (quote {}))
 '(org-export-with-toc nil)
 '(org-latex-packages-alist
   (quote
    (("cm" "fullpage" nil)
     ("compact" "titlesec" nil)
     ("" "paralist" nil)
     ("" "enumitem" nil)
     ("" "color" nil)
     ("" "tabularx" nil)
     ("" "enumitem" nil))))
 '(org-odt-convert-processes
   (quote
    (("LibreOffice" "\"c:/Program Files (x86)/LibreOffice 5/program/soffice\" --headless --convert-to %f%x --outdir %d %i")
     ("unoconv" "unoconv -f %f -o %d %i"))))
 '(org-table-convert-region-max-lines 9999)
 '(org-use-speed-commands t)

 '(org-confirm-babel-evaluate nil)
 '(org-odt-preferred-output-format "docx")
 '(org-latex-listings t)
 '(org-list-allow-alphabetical t)
 '(org-src-fontify-natively t)
 '(org-startup-folded nil)
 '(org-startup-indented nil)
 '(org-export-coding-system 'utf-8)
)

;; to use koma-article formatting (more modern than default LaTeX),
;; put this in org-mode file:
;;  #+LaTeX_CLASS: koma-article
(eval-after-load 'ox '(require 'ox-koma-letter))
(add-to-list 'org-latex-classes
	     '("koma-article"
	       "\\documentclass{scrartcl}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
	     '("beamer"
	       "\\documentclass\[presentation\]\{beamer\}"
	       ("\\section\{%s\}" . "\\section*\{%s\}")
	       ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
	       ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;; org-mode color
(if (fboundp 'org-add-link-type)
    (progn
      (org-add-link-type
       "color"
       ;; follow-link function:
       (lambda (path)
	 (message (concat "color "
			  (progn (add-text-properties
				  0 (length path)
				  (list 'face `((t (:foreground ,path))))
				  path) path))))
       ;; export function:
       (lambda (path desc format) ; desc is the text, path is the color, and format is html/latex/odt
	 (cond
	  ((eq format 'html)
	   (format"<span style=\"color:%s;\">%s</span>"  path desc))
	  ((eq format 'latex)
	   (format"{\\color{%s}%s}"  path desc))
	  ((eq format 'odt)
	   (format"<text:span text:style-name=\"%sColor\">%s</text:span>" path desc)) ; see ~/ooo-template.ott
	  (t
	   (format"%s{%s}" desc path)))))

      (org-add-link-type
       "bgcolor"
       ;; follow-link function:
       (lambda (path)
	 (message (concat "background color "
			  (progn (add-text-properties
				  0 (length path)
				  (list 'face `((t (:background ,path))))
				  path) path))))
       ;; export function:
       (lambda (path desc format)
	 (cond
	  ((eq format 'html)
	   (format"<span style=\"background-color:%s;\">%s</span>"  path desc))
	  ((eq format 'latex)
	   (format"\\colorbox{%s}{%s}"  path desc))
	  ((eq format 'odt)
	   (format"<text:span text:style-name=\"%sBg\">%s</text:span>" path desc))
	  (t
	   (format"%s{%s}" desc path)))))

      (setq org-export-html-style
	    "   <style type=\"text/css\">
    <![CDATA[
      .title { font-size: 1.3em; }
       h1 { font-size: 1.3em; }
       h2 { font-size: 1.2em;
            margin-bottom: 0.2em;
            -webkit-margin-after: 0.2em; }
       h3 { font-size: 1.1em; }
       ul { margin-left: -1em;
            margin-top: 0.1em;
            margin-bottom: 0.1em; }
    ]]>
   </style>
")
      ))
