;; Copyright 2024 William Warner
;; SPDX-License-Identifier: GPL-3.0-only

(dolist (lang '(javascript json typescript tsx))
  (when (or (not (treesit-language-available-p lang))
            noninteractive)
    (treesit-install-language-grammar lang)))

;; Use built-in js-ts-mode for JavaScript files
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; Prettier integration for formatting
(use-package prettier-js
  :ensure t
  :hook ((js-ts-mode . prettier-js-mode)
         (typescript-ts-mode . prettier-js-mode)
         (tsx-ts-mode . prettier-js-mode)
	 (json-ts-mode . prettier-js-mode))
  :config
  (setq prettier-js-command "prettier"
        prettier-js-args '("--single-quote" "--trailing-comma" "es5")))

;; eglot is already configured in prog-mode-hook in init.el
;; typescript-language-server will be automatically used for these modes

