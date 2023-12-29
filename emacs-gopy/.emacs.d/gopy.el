(use-package go-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (gofmt-command "gofumpt"))

(use-package lsp-mode
  :ensure t
  :commands lsp-register-client
  :config
  (lsp-register-custom-settings
   '(("gopls.staticcheck" t t))))

(use-package go-dlv :ensure t)
(use-package company-go :ensure t)
(use-package dap-go :ensure t)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-go)
;; (use-package dap-python)

(use-package lsp-pyright
  :ensure t)

(use-package python-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'lsp))
