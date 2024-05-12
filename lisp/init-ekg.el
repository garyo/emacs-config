;;; init-ekg.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; EKG: Emacs Knowledge Graph

;; Experimental -- logseq-like note-taking app, backed by sql rather
;; than note files. Might be great.

(defun setup-ekg-transients () "Set up Transient menus for EKG"
       (transient-define-prefix ekg-dispatch ()
         "Top level Transient menu for EKG (Emacs Knowledge Graph)"
         [["Show"
           ("st" "Today" ekg-show-notes-for-today)
           ("slc" "Latest Captured" ekg-show-notes-latest-captured)
           ("slm" "Latest Mod" ekg-show-notes-latest-modified)
           ("sx" "Trash" ekg-show-notes-in-trash)
           ("sd" "Drafts" ekg-show-notes-in-drafts)
           "Find Tags"
           ("tt" "Tag" ekg-show-notes-with-tag)
           ("taa" "All Tags" ekg-show-notes-with-all-tags)
           ("ta?" "Any Tag" ekg-show-notes-with-any-tags)
           ]
          ["Capture"
           ("cc" "New Note" ekg-capture)
           ("cu" "...from URL" ekg-capture-url)
           ("cb" "...from current buffer" ekg-capture-file)
           ]
          ["Query" :if (lambda () (or (featurep 'ekg-llm) (featurep 'ekg-embedding)))
           ("qt" "for terms" ekg-embedding-search :if (lambda () (featurep 'ekg-embedding)))
           ("qb" "similar to current buffer" ekg-embedding-show-similar-to-current-buffer :if (lambda () (featurep 'ekg-embedding)))
           ("qR" "Regenerate embeddings" ekg-embedding-generate-all :if (lambda () (featurep 'ekg-embedding)))
           "AI"
           ("aq" "AI query, all notes" ekg-llm-query-with-notes :if (lambda () (featurep 'ekg-llm)))
           ]
          ["Misc"
           ("gr" "Global rename tag" ekg-global-rename-tag)
           ("e" "This note ..." ekg-notes-dispatch :if-mode ekg-notes-mode)
           ("Q" "Quit this menu" transient-quit-one)
           ]
          ])

       (global-set-key (kbd "<f6>") 'ekg-dispatch)
       (global-set-key (kbd "C-c e") 'ekg-dispatch)

       (transient-define-prefix ekg-notes-dispatch ()
         "Notes buffer Transient menu for EKG (Emacs Knowledge Graph)"
         [["Show Notes"
           ("sa" "with any of this note's tags" ekg-notes-any-note-tags)
           ("sA" "with any of these notes' tags" ekg-notes-any-tags)
           ("st" "select tag" ekg-notes-tag)
           ("ss" "search for similar" ekg-embedding-show-similar :if (lambda () (featurep 'ekg-embedding)))
           ]
          ["AI"
           ("aa" "AI send & append" ekg-llm-send-and-append-note :if (lambda () (featurep 'ekg-llm)))
           ("ar" "AI send & replace" ekg-llm-send-and-replace-note :if (lambda () (featurep 'ekg-llm)))
           ]
          ["Manage"
           ("c" "create" ekg-notes-create)
           ("d" "delete" ekg-notes-delete)
           ("g" "refresh" ekg-notes-refresh)
           ("k" "kill (hide) note" ekg-notes-kill)
           ("o" "open/edit" ekg-notes-open)
           ("m" "Change mode of current note" ekg-change-mode)
           ]
          ["Browse"
           ("b" "browse resource" ekg-notes-browse)
           ("u" "Browse to URL" ekg-browse-url)
           ("B" "select & browse" ekg-notes-select-and-browse-url)
           ]
          ["Global"
           ("g" "global ekg commands..." ekg-dispatch)
           ("q" "quit this menu" transient-quit-one)
           ("Q" "quit EKG" kill-buffer-and-window)
           ]
          ])
       (define-key ekg-notes-mode-map (kbd "e") 'ekg-notes-dispatch)
       (define-key ekg-notes-mode-map (kbd "?") 'ekg-notes-dispatch) ; help when I'm confused
       (define-key ekg-notes-mode-map (kbd "q") 'kill-buffer-and-window) ; I prefer this
       )

;;; get secrets stored in ~/.authinfo
(use-package auth-source
  :ensure nil)

;;; One machine (WSL2 Ubuntu) can't find the 'llm package, which causes this to fail.
;;; So suppress errors here.
(condition-case nil
    (use-package ekg
      :ensure (:host github
                     :repo "ahyatt/ekg"
                     ;; :branch "inline-tags"
                     ;; :fork (:host github
                     ;;              :repo "garyo/emacs-ekg"
                     ;;              :branch "garyo/updates")
                     )
      :config
      (require 'ekg-logseq)
      (require 'ekg-org-roam)
      ;; for LLM (AI) search, ekg uses the llm module.
      ;; To set that up, have to also require the llm module I'm using.
      (require 'ekg-embedding)
      (require 'ekg-llm)
      (require 'llm-openai)          ; this comes with the ekg package
      ;; Get my secret OpenAPI key from ~/.authinfo, store into LLM
      ;; See auth-source and ekg docs
      (if (featurep 'llm-openai)
	  (let* ((authval (auth-source-search :name "openai"
                                              :require '(:secret)))
		 (rawkey (plist-get (car authval) :secret))
		 (key (if (functionp rawkey)
			  (funcall rawkey)
			rawkey)))
            (message "Got auth key %s" key)
            (let ((my-provider (make-llm-openai :key key)))
              (setq ekg-llm-provider my-provider
                    ekg-embedding-provider my-provider)))
	)
      ;; After both transient and ekg are loaded, set up my transient menus
      (with-eval-after-load 'transient
        (with-eval-after-load 'ekg
          (setup-ekg-transients)))

      (defun get-ekg-body-tags (note)
	"Get #tags from body of EKG note"
	(let* ((string (ekg-note-text note))
               (regexp "#\\([-_.a-zA-Z0-9]+\\)")
               matches
               (newtags (save-match-data
			  (let ((pos 0)
				matches)
                            (while (string-match regexp string pos)
                              (push (match-string 1 string) matches)
                              (setq pos (match-end 0)))
                            matches))))
	  (seq-uniq (append newtags (ekg-note-tags note)))))

      (defun my-ekg-note-pre-save-hook (note)
	"Apply #tags found in body to the note's tags"
	(let ((tags (get-ekg-body-tags note)))
	  (message "Setting tags to %s" tags)
	  ;; Workaround: the setf macro below doesn't work properly;
	  ;; it macroexpands to a call to a function named
	  ;; "(setf ekg-note-tags)" including the parens and spaces!
	  ;; Just call aset to set the slot instead.
	  ;; See https://emacs.stackexchange.com/questions/79007
	  ;; (setf (ekg-note-tags note) tags)
	  (aset note (cl-struct-slot-offset 'ekg-note 'tags) tags)
	  (ekg--normalize-note note)
	  ))

      ;; Allow a note to have tags in the body, by scanning the body before saving and adding any tags to the note's tags.
      (add-hook 'ekg-note-pre-save-hook 'my-ekg-note-pre-save-hook)

      )
  (error nil)
  )


(provide 'init-ekg)
