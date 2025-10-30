;; Copyright 2024 William Warner
;; SPDX-License-Identifier: GPL-3.0-only

(use-package sqlformat :ensure t)
(setq sqlformat-command 'sqlfmt)
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
