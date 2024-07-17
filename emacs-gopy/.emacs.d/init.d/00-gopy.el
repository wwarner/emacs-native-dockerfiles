;; Copyright 2024 William Warner
;; SPDX-License-Identifier: GPL-3.0-only

(use-package go-mode
  :ensure t
  :init
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'eglot-ensure)
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
