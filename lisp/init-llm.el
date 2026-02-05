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

;; gptel: Emacs LLM integration
;; See https://github.com/karthink/gptel?tab=readme-ov-file
;; OpenAI ChatGPT is enabled by default. Add my preferred models:
;; Note: auth keys are in ~/.authinfo
;;   machine api.openai.com login apikey password ***
;;   machine api.anthropic.com login apikey password ***
(use-package gptel
  :config
  (gptel-make-anthropic "Claude" :stream t :key gptel-api-key)
  ;; Use this for gptel <-> mcp tool use:
  (require 'gptel-integrations)
  )

;; Model Context Protocol hub, tools for AI agents
;; (not yet on elpa/melpa)
(use-package mcp
  :ensure (:host github :repo "lizqwerscott/mcp.el")
  :config
  (setq mcp-hub-servers
        `(("filesystem" . (:command "npx"
                                    :args ("-y" "@modelcontextprotocol/server-filesystem"
                                           "/Users/garyo/")))
          ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
          ("ripgrep" . (:command "npx" :args ("-y" "mcp-ripgrep@latest")))
          ("ticktick" . (:command "/Users/garyo/.cargo/bin/uv"
                                  :args ("run" "--directory" "/Users/garyo/src/ai-tools/ticktick-mcp"
                                         "-m" "ticktick_mcp.cli" "run")))
          ("shell" . (:command "/Users/garyo/.cargo/bin/uvx"
                               :args ("mcp-shell-server")
                               :env (:ALLOW_COMMANDS
                                     ,(concat "ls,cat,pwd,grep,wc,touch,find,cp,mv,echo,"
                                              "emacs,emacsclient,bun,npx,node"))))
          ))
  )

(provide 'init-llm)
