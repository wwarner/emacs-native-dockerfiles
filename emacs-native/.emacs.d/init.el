(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(global-set-key (kbd "M-P") 'previous-buffer)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-K") 'kill-this-buffer)
(global-set-key (kbd "M-0") 'delete-window)
(global-unset-key (kbd "C-;"))

(setq scroll-preserve-screen-position 1)
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(global-set-key (kbd "M-Z") 'fzf)
(add-hook 'rg-mode-hook (lambda () (local-unset-key (kbd "M-o"))))
(add-hook 'rg-mode-hook (lambda () (local-unset-key (kbd "M-P"))))
(add-hook 'rg-mode-hook (lambda () (local-unset-key (kbd "M-N"))))
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
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088"
     "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c"
     "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f"
     "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7"
     "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077"
     "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e"
     "1a10896643cce14633f9e2b9f3727761cc528ee7bbbe7e8efeb442e067da1a96"
     "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773"
     default))
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(inhibit-startup-screen t)
 '(markdown-command "markdown")
 '(package-selected-packages
   '(tree-sitter tree-sitter-langs k8s-mode origami w3m vterm folding
		 kubernetes clipetty tramp-container ag use-package
		 project prettier prettier-js dockerfile-mode
		 glsl-mode docker dap-mode docker-compose-mode lsp-mode fzf
		 company string-inflection projectile json-mode lsp-ui
		 uuidgen tide deadgrep rainbow-delimiters
		 zenburn-theme solarized-theme magit iedit))
 '(rg-command-line-flags '("--ignore-file ~/.rgignore"))
 '(rg-custom-type-aliases nil)
 '(rg-hide-command nil)
 '(rg-ignore-ripgreprc nil)
 '(safe-local-variable-values
   '((project-root . "~/mm/github.com/MediaMath")))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(shr-inhibit-images nil)
 '(tooltip-mode nil)
 '(typescript-indent-level 2)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((use-package) (comp))))
(menu-bar-mode -1)
(savehist-mode t)
(delete-selection-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;(load-theme 'solarized-zenburn t)
;;(load-theme 'solarized-dark-high-contrast t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'yaml-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c C-c")
 'comment-region)))
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c C-v")
 'uncomment-region)))
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c ;")
 'iedit-mode)))
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c <down>") 'hs-toggle-hiding)))

(use-package lsp-mode
  :ensure t
  :commands lsp-register-client)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; use clipetty mode to copy to paste buffer!
(global-set-key (kbd "M-w") 'clipetty-kill-ring-save)

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
  :straight (yaml-pro :type git :host github :repo "zkry/yaml-pro"))

(use-package rg
  :straight (rg :type git :host github :repo "dajva/rg.el"))
(autoload 'wgrep-rg-setup "wgrep-rg")
(add-hook 'rg-mode-hook 'wgrep-rg-setup)

;;;;
;; treesitter
;;;;
;; cribbing from
;; https://github.com/nattakit-h/erica/blob/76d92526e9324407a96721fd7ff514abf117ac9f/init.el#L245-L286
(defconst data-directory (expand-file-name "data" user-emacs-directory))
(use-package treesit
  :ensure nil
  :commands (treesit-install-language-grammar treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
	'((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
	  (c . ("https://github.com/tree-sitter/tree-sitter-c"))
	  (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	  (css . ("https://github.com/tree-sitter/tree-sitter-css"))
	  (go . ("https://github.com/tree-sitter/tree-sitter-go"))
	  (html . ("https://github.com/tree-sitter/tree-sitter-html"))
	  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	  (json . ("https://github.com/tree-sitter/tree-sitter-json"))
	  (lua . ("https://github.com/tjdevries/tree-sitter-lua"))
	  (make . ("https://github.com/alemuller/tree-sitter-make"))
	  (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
	  (python . ("https://github.com/tree-sitter/tree-sitter-python"))
	  (php . ("https://github.com/tree-sitter/tree-sitter-php"))
	  (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
	  (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
	  (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
	  (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
	  (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
	  (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
	  (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  (setq treesit-extra-load-path (list (expand-file-name "treesit" data-directory)))
  (setq major-mode-remap-alist '((c-mode . c-ts-mode) (c++-mode . c++-ts-mode)))
  :config
  (defun treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	(treesit-install-language-grammar lang)
	(message "`%s' parser was installed." lang)
	(sit-for 0.75))))
  (advice-add
   'treesit--install-language-grammar-1
   :around
   (lambda (old-function out-dir &rest arguments)
     (apply old-function (car treesit-extra-load-path) arguments))))
;; end cribbing

;; yaml-pro-mode allows for navigation and folding of yaml.
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-pro-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-pro-mode))
(add-hook 'yaml-pro-mode-hook #'display-line-numbers-mode)
(add-hook 'yaml-pro-mode-hook (lambda () (local-set-key (kbd "C-c C-c")
'comment-region)))
(add-hook 'yaml-pro-mode-hook (lambda () (local-set-key (kbd "C-c C-v")
							'uncomment-region)))
(add-hook 'yaml-pro-mode-hook (lambda () (setq indent-tabs-mode nil)))

(defun fetch-helm-resources ()
  "Calls benthos/scripts/resources.sh with highlighted region, returns the helm_upgrade command that will match what is currently running in prod"
  (interactive)
  (let ((resources
	(shell-command-to-string
	 (concat
	  (concat project-root "/scripts/kube/resources.sh")
	  " "
	  (buffer-substring (region-beginning) (region-end))
	  ))))
    (message resources)
    (beginning-of-line)
    (insert (concat "# " resources))))

;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(add-hook 'emacs-startup-hook 'treemacs)
