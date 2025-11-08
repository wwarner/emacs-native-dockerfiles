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

;; cribbed from Torstein Krause Johansen
;; https://gitlab.com/skybert/my-little-friends/-/blob/9041b0867353d5bcc10df6c622827e35390ecb0d/emacs/.emacs#L865-943
;; Great video https://www.youtube.com/watch?v=p_xX_vX8M7g
(use-package dape
  :ensure t
  :init
  (setq
   ;; Performance tweaks
   gc-cons-threshold 80000000

   dape-buffer-window-arrangement 'right
   ;; inlay-hints proved unusable for me
   dape-inlay-hints nil

   dape-configs
   '(
     (go-test
      modes (go-mode go-ts-mode)
      command "dlv"
      command-args ("dap" "--listen" "127.0.0.1:55878")
      command-cwd default-directory
      host "127.0.0.1"
      port 55878
      :name "Run the go tests in cwd"
      :request "launch"
      :mode "test"
      :type "go"
      :program "."
      )

     (go-run-main
      modes (go-mode go-ts-mode)
      command "dlv"
      command-args ("dap" "--listen" "127.0.0.1:55878")
      command-cwd (project-root (project-current))
      host "127.0.0.1"
      port 55878
      :name "Run main.go with --localtest"
      :request "launch"
      :mode "debug"
      :type "go"
      :program "main.go"
      :args ["-localtest"]
      )
     ))

  (global-set-key [f7] 'dape-step-in)
  (global-set-key [f8] 'dape-next)
  (global-set-key [f9] 'dape-continue))
