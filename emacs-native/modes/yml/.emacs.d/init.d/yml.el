;; Copyright 2024 William Warner
;; SPDX-License-Identifier: GPL-3.0-only

;; This yaml configuration is cobbled together from `yaml-ts-mode`,
;; `indent-tools-minor-mode`, redhat's `yaml-language-server` and
;; `helm-ls`. In particular, `helm-ls` knows the keywords and syntax
;; of kubernetes resource files, so we can know when our structure is
;; illegal, and (sometimes) navigate to to definitions and references.
;; `yaml-ts-mode` is loaded automatically for .yaml files. `Helm-mode`
;; must be invoked manually with `M-x helm-mode`.

(when (or (not (treesit-language-available-p 'yaml))
          noninteractive)
  (treesit-install-language-grammar 'yaml))

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-hook 'yaml-ts-mode-hook
	  (lambda ()
	    (indent-tools-minor-mode)
	    (display-line-numbers-mode)
	    (local-set-key (kbd "C-c <down>") 'treesit-fold-toggle)))

(define-derived-mode helm-mode yaml-ts-mode "helm"
  "Major mode for editing kubernetes helm templates")

(require 'eglot)
(add-hook 'helm-mode-hook
	  (lambda ()
	    (eglot-ensure)
	    (add-to-list 'eglot-server-programs '(helm-mode "helm_ls" "serve"))
	    (setq-default eglot-workspace-configuration
                `(:helm-ls (:logLevel "debug"
                           :valuesFiles (:mainValuesFile "values.yaml"
                                        :lintOverlayValuesFile "values.lint.yaml"
                                        :additionalValuesFilesGlobPattern "values*.yaml")
                           :helmLint (:enabled t
                                     :ignoredMessages [])
                           :yamlls (:enabled t
                                    :enabledForFilesGlob "*.{yaml,yml}"
                                    :diagnosticsLimit 50
                                    :showDiagnosticsDirectly t
                                    :path "yaml-language-server"
                                    :initTimeoutSeconds 3
                                    :config (:schemas (:kubernetes "templates/**")
                                             :completion t
                                             :hover t)))))))
