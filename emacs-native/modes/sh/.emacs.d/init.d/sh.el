;; Copyright 2024 William Warner
;; SPDX-License-Identifier: GPL-3.0-only

(require 'eglot)
(add-hook 'sh-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs
             '(sh-mode . ("bash-language-server" "start")))
