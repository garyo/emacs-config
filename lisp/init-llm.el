;;; init-llm.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Initialize LLMs

(use-package auth-source
  :ensure nil)

(defun get-openai-key ()
  (let* ((authval (auth-source-search :name "openai"
                                      :require '(:secret)))
	 (rawkey (plist-get (car authval) :secret))
	 (key (if (functionp rawkey)
		  (funcall rawkey)
		rawkey)))
    key
    ))

(use-package gptel
  :config
  (setq gptel-api-key 'get-openai-key
        gptel-model "gpt-4o")
  )

(provide 'init-llm)
