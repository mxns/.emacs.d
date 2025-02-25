;;; stuff.el --- mxns config

;;; Commentary:
;;; stuff about stuff

;;; Code:

(require 'package)
(require 'use-package)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(setq confirm-kill-emacs 'y-or-n-p)
(setq ns-right-option-modifier 'option)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))
(setq use-package-always-ensure t)
(setq read-file-name-completion-ignore-case t)

;;; tree-sitter
(setq treesit-language-source-alist
      '((yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
	(json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
	(ruby . ("https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1"))
	(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
	(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
	(java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.23.5"))))
(treesit-install-language-grammar 'yaml)
(treesit-install-language-grammar 'json)
(treesit-install-language-grammar 'java)
(treesit-install-language-grammar 'tsx)
(treesit-install-language-grammar 'typescript)

(load "~/.emacs.d/scrolling")

(use-package undo-tree
  :bind ("C-c u" . undo-tree-visualize)
  :hook (prog-mode . undo-tree-mode))

(use-package vertico
  ;;:custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :config (vertico-mode))

(use-package savehist
  :config (savehist-mode))

(use-package rg
  :ensure-system-package rg
  :hook (grep-mode . (lambda () (toggle-truncate-lines 1))))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package projectile
  :init (setq projectile-project-search-path '("~/devel/fortifiedid/"))
  :config (projectile-mode)
  :config (setq projectile-project-search-path '("~/devel/fortifiedid/"))
  :config (projectile-discover-projects-in-search-path))

(use-package which-key
  :config (which-key-mode))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package treemacs
  :bind ("C-c t" . treemacs))

(use-package company)

(use-package hs-minor-mode
  :ensure nil
  :bind (:map hs-minor-mode-map ("C-c v" . hs-toggle-hiding)))

(use-package json-ts-mode
  :mode "\\.json\\'"
  :hook (json-ts-mode . hs-minor-mode))

(use-package yaml-ts-mode
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'")

(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :hook (typescript-ts-mode . lsp-mode)
  :hook (tsx-ts-mode . lsp-mode)
  :config (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui)

;;; https://repo.eclipse.org/content/repositories/jdtls-releases/org/eclipse/jdt/ls/org.eclipse.jdt.ls.core/
(use-package lsp-java
  :init (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.44.0/jdt-language-server-1.44.0-20250122155241.tar.gz")
  :init (setq lsp-java-java-path "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/bin/java")
  :init (setenv "JAVA_HOME"  "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/")
  :config (add-hook 'java-ts-mode-hook 'lsp))

;;; stuff.el ends here


