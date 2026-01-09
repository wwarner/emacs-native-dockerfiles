;; Copyright 2024 William Warner
;; SPDX-License-Identifier: GPL-3.0-only

(use-package lsp-pyright :ensure t)
(use-package python-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'lsp))
