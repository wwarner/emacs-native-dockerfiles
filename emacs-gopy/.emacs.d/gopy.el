(use-package go-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook #'lsp-deferred)
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
 (gofmt-command "gofumpt")
 (lsp-go-gopls-server-path "/go/bin/gopls"))
(add-hook 'before-save-hook 'gofmt-before-save)

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
  :ensure t
  :init
  (add-hook 'python-mode-hook #'lsp-deferred))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))
