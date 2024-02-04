(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(global-set-key (kbd "M-P") 'previous-buffer)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-K") 'kill-this-buffer)
(global-set-key (kbd "M-0") 'delete-window)

(setq scroll-preserve-screen-position 1)
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(global-set-key (kbd "M-Z") 'fzf)
(setq switch-to-prev-buffer-skip
       (lambda (win buf bok)
     (let ( (b (buffer-name buf)) )
       (or (member b '("*Messages*"
               "*Apropos*"
               "*Warnings*"
               "*lsp-log*"
               "*Completions*"
               "*Backtrace*"))
           (string-match-p "^magit-process" b)
           (string-match-p "^\*nrepl-server " b)))))
(setq switch-to-next-buffer-skip switch-to-prev-buffer-skip)
(setq native-comp-deferred-compilation t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-reuse-buffers t)
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(inhibit-startup-screen t)
 '(markdown-command "markdown")
 '(package-selected-packages
   '(k8s-mode origami w3m folding
		 clipetty tramp-container ag use-package
		 project prettier prettier-js dockerfile-mode
		 glsl-mode docker docker-compose-mode
		 company string-inflection projectile json-mode
		 uuidgen tide rainbow-delimiters iedit))
 '(safe-local-variable-values
   '((project-root . "~/mm/github.com/MediaMath")))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(shr-inhibit-images nil)
 '(tooltip-mode nil)
 '(typescript-indent-level 2)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((use-package) (comp)))
 ;; '(use-package-compute-statistics t)
 )
(menu-bar-mode -1)
(savehist-mode t)
(delete-selection-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Basic things common to all programming modes
;;   show line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;   comment and uncooment shortcuts
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c C-c")
 'comment-region)))
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c C-v")
						    'uncomment-region)))
;;   turn on iedit at the cursor
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c ;")
						    'iedit-mode)))
;;   toggle visibility of the current code block
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c <down>") 'hs-toggle-hiding)))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; use clipetty mode to copy to paste buffer!
(global-set-key (kbd "M-w") 'clipetty-kill-ring-save)

(use-package magit :ensure t)
(use-package treemacs
  :ensure t
  :config (treemacs-do-add-project-to-workspace "/root" "/root"))
(use-package vterm
  :ensure t
  :init (setq vterm-always-compile-module t)
  (add-hook 'vterm-mode-hook (lambda () (setq show-trailing-whitespace nil))))

(use-package fzf :ensure t)

(use-package solarized-theme :ensure t)
(use-package zenburn-theme :ensure t)
(use-package soft-charcoal-theme :ensure t)
(load-theme 'soft-charcoal t)

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

(use-package yaml-pro
  :ensure t
  :straight (yaml-pro :type git :host github :repo "zkry/yaml-pro")
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-pro-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-pro-mode))
  (add-hook 'yaml-pro-mode-hook #'display-line-numbers-mode)
  (add-hook 'yaml-pro-mode-hook (lambda () (local-set-key (kbd "C-c C-c")
							  'comment-region)))
  (add-hook 'yaml-pro-mode-hook (lambda () (local-set-key (kbd "C-c C-v")
							  'uncomment-region)))
  (add-hook 'yaml-pro-mode-hook (lambda () (setq indent-tabs-mode nil))))

(use-package rg
  :ensure t
  :straight (rg :type git :host github :repo "dajva/rg.el")
  :config
  (when (file-exists-p "~/.rgignore") (setq rg-command-line-flags '("--ignore-file ~/.rgignore")))
  (setq rg-custom-type-aliases nil
	rg-hide-command nil
	rg-ignore-ripgreprc nil)
  ;; gotta be able to cycle through buffers even in rg mode
  (add-hook 'rg-mode-hook (lambda () (local-unset-key (kbd "M-o"))))
  (add-hook 'rg-mode-hook (lambda () (local-unset-key (kbd "M-P"))))
  (add-hook 'rg-mode-hook (lambda () (local-unset-key (kbd "M-N"))))
  (autoload 'wgrep-rg-setup "wgrep-rg")
  (add-hook 'rg-mode-hook 'wgrep-rg-setup))

(use-package tree-sitter
  :ensure t
  :config (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (mapc #'treesit-install-language-grammar
	(cl-remove-if '(lambda (v) (file-exists-p (concat "~/.emacs.d/tree-sitter/libtree-sitter-" (symbol-name v) ".so")))
		      (mapcar #'car treesit-language-source-alist))))
;; (use-package ts-fold
;;   :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(use-package lsp-mode :ensure t)

;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; derived images can put their elisp in this init directory and it
;; will be picked up in alphabetical order
(mapc 'load (file-expand-wildcards "~/.emacs.d/init.d/*.el"))

(add-hook 'emacs-startup-hook 'treemacs)
