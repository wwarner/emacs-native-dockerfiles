;; Copyright 2024 William Warner
;; SPDX-License-Identifier: GPL-3.0-only

(use-package go-mode
  :ensure t
  :commands lsp-register-client
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook 'lsp)
  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq gofmt-command "gofumpt")
  (lsp-register-custom-settings
   '(("gopls.staticcheck" t t))))
(use-package gotest :ensure t)
(use-package go-dlv :ensure t)
(use-package company-go :ensure t)

(use-package lsp-pyright :ensure t)
(use-package python-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'lsp))
