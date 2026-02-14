(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'claude-opus-4.6
        gptel-backend (gptel-make-gh-copilot "GithubCopilot")))
(use-package mcp
  :ensure t
  :config
  (setq mcp-hub-servers
        '(("filesystem" . (:command "npx"
                           :args ("-y" "@modelcontextprotocol/server-filesystem")
                           :roots ("~/src")))
	  ("private-journal" . (:command "npx"
					 :args ("github:obra/private-journal-mcp")))
          ("fetch"      . (:command "uvx"
                           :args ("mcp-server-fetch"))))))
