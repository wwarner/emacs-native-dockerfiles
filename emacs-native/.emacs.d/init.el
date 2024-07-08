;; Copyright 2024 William Warner
;; SPDX-License-Identifier: GPL-3.0-only

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Somehow this key binding is getting dropped in my build. It's
;; present in emacs-plus and others
(global-set-key (kbd "M-%") 'query-replace-regexp)

;; I rely on these because I like to work on a laptop screen, so I
;; need an efficient way to cycle through buffers
(global-set-key (kbd "M-P") 'previous-buffer)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-K") 'kill-this-buffer)
(global-set-key (kbd "M-0") 'delete-window)

;; Scroll up and down without moving the cursor
(setq scroll-preserve-screen-position 1)
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

(setq native-comp-jit-compilation t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tooltip-mode nil)
 '(tsc-dyn-get-from '(:compilation))
 '(use-package-compute-statistics t)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((use-package) (use-package) (comp))))
(menu-bar-mode -1)
(savehist-mode t)
(delete-selection-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; these packages need no additional configuration
(use-package iedit)
(use-package magit)
(use-package fzf)
(use-package solarized-theme)
(use-package zenburn-theme)
(use-package dockerfile-mode)
(use-package json-mode)
(use-package company)
(use-package uuidgen)
(use-package rainbow-delimiters)
(use-package soft-charcoal-theme)

;; clipetty copies to the system paste buffer
(use-package clipetty
  :config (global-set-key (kbd "M-w") 'clipetty-kill-ring-save))
(use-package lsp-mode
  :init (setq lsp-modeline-diagnostics-enable t))

;; brings up file navigation at startup
(use-package treemacs
  :config
  (treemacs-do-add-project-to-workspace "/root" "/root")
  (add-hook 'emacs-startup-hook 'treemacs))

(use-package vterm
  :init (setq vterm-always-compile-module t)
  (add-hook 'vterm-mode-hook (lambda () (setq show-trailing-whitespace nil))))
(use-package multi-vterm)

(use-package ef-themes
  :config (load-theme 'ef-winter t))

(use-package flycheck
  :config (flycheck-display-errors-delay 0.2))
(use-package lsp-ui
  :config
  (setq lsp-diagnostics-provider nil)
  :after flycheck)
(use-package lsp-treemacs
  :after lsp-mode
  :config  (setq lsp-treemacs-error-list-expand-depth 5)
  :commands lsp-treemacs-errors-list)

(add-hook 'prog-mode-hook
	  (lambda ()
	    (display-line-numbers-mode)
	    (local-set-key (kbd "C-c C-c") #'comment-region)
	    (local-set-key (kbd "C-c C-v") #'uncomment-region)
	    (hs-minor-mode)
	    (local-set-key (kbd "C-c <down>") #'hs-toggle-hiding)))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package rg
  :straight (rg :type git :host github :repo "dajva/rg.el")
  :config
  (when (file-exists-p "~/.rgignore") (setq rg-command-line-flags '("--ignore-file ~/.rgignore")))
  (setq rg-custom-type-aliases nil
	rg-hide-command nil
	rg-ignore-ripgreprc nil)
  (autoload 'wgrep-rg-setup "wgrep-rg")
  (add-hook 'rg-mode-hook 'wgrep-rg-setup)
  ;; gotta be able to cycle through buffers even in rg mode
  (add-hook 'rg-mode-hook (lambda () (local-unset-key (kbd "M-o"))))
  (add-hook 'rg-mode-hook (lambda () (local-unset-key (kbd "M-P"))))
  (add-hook 'rg-mode-hook (lambda () (local-unset-key (kbd "M-N")))))

;; derived images can put their elisp in this init directory and it
;; will be picked up in alphabetical order
(mapc 'load (file-expand-wildcards "~/.emacs.d/init.d/*.el"))

