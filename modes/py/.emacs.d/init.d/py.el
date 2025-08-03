;; Copyright 2024 William Warner
;; SPDX-License-Identifier: GPL-3.0-only

(use-package go-mode
  :ensure t
  :init
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (setq gofmt-command "gofumpt"))
(use-package gotest :ensure t)
(use-package go-dlv :ensure t)
(use-package company-go :ensure t)
(with-eval-after-load 'dape
  (add-to-list 'dape-configs
               `(dlv
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1:55878")
                 host "127.0.0.1"
                 port 55878
                 :type "debug"
                 :request "launch"
		 :cwd "."
		 :program ".")))

(use-package lsp-pyright :ensure t)
(use-package python-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'lsp))
