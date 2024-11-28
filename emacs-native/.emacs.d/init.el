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
(global-set-key (kbd "<M-up>") (kbd "C-u 1 C-v"))
(global-set-key (kbd "<M-down>") (kbd "C-u 1 M-v"))

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
(use-package dockerfile-mode)
(use-package ef-themes)
(use-package iedit)
(use-package indent-tools)
(use-package json-mode)
(use-package magit)
(use-package rainbow-delimiters)
(use-package solarized-theme)
(use-package uuidgen)
(use-package zenburn-theme)

(use-package company
  :config (global-company-mode 1))

(use-package fzf
  :defer t
  :config
  (setq
   fzf/args "-x --print-query --margin=1,0 --no-hscroll"
   fzf/executable "fzf"
   fzf/git-grep-args "-i --line-number %s"
   fzf/grep-command "rg --no-heading -nH"
   fzf/position-bottom t
   fzf/window-height 15))

;; clipetty copies to the system paste buffer
(use-package clipetty
  :config (global-set-key (kbd "M-w") 'clipetty-kill-ring-save))

;; brings up file navigation at startup
(use-package treemacs
  :config
  (treemacs-do-add-project-to-workspace "/root" "/root")
  (add-hook 'emacs-startup-hook 'treemacs))

(use-package vterm
  :init (setq vterm-always-compile-module t)
  (add-hook 'vterm-mode-hook (lambda () (setq show-trailing-whitespace nil))))
(use-package multi-vterm)

;; also like theme ef-dark
(use-package soft-charcoal-theme
  :config (load-theme 'soft-charcoal t)
          (set-face-background 'mode-line "#555555"))

(add-hook 'yaml-mode-hook (lambda ()
	    (indent-tools-minor-mode)
	    (display-line-numbers-mode)))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (eglot-ensure)
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

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '(
	  (bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (dot        . ("https://github.com/rydesun/tree-sitter-dot"))
          (doxygen    . ("https://github.com/tree-sitter-grammars/tree-sitter-doxygen"))
          (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (gitcommit  . ("https://github.com/gbprod/tree-sitter-gitcommit"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (gosum      . ("https://github.com/amaanq/tree-sitter-go-sum"))
          (gowork     . ("https://github.com/omertuc/tree-sitter-go-work"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (http       . ("https://github.com/rest-nvim/tree-sitter-http"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua        . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
          (make       . ("https://github.com/tree-sitter-grammars/tree-sitter-make"))
          (markdown   . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"))
          (proto      . ("https://github.com/treywood/tree-sitter-proto"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql        . ("https://github.com/derekstride/tree-sitter-sql"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (vue        . ("https://github.com/tree-sitter-grammars/tree-sitter-vue"))
          (yaml       . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
	  )
	)
  )

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

(use-package dape
  :ensure (dape :type git :host github :repo "svaante/dape")
  :disabled t)

;; derived images can put their elisp in this init directory and it
;; will be picked up in alphabetical order
(mapc 'load (file-expand-wildcards "~/.emacs.d/init.d/*.el"))

