;;; init.el --- mxns config

;;; Commentary:
;;; My configuration

;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(require 'package)
(require 'use-package)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/local/")

(setq confirm-kill-emacs 'y-or-n-p)
(setq ns-right-option-modifier 'option)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))
(setq use-package-always-ensure t)
(setq read-file-name-completion-ignore-case t)

;;; init and refresh packages
;;; (package-initialize)
;;; (package-refresh-contents)

(load "~/.emacs.d/scrolling")

;; (use-package prog-mode
;;   :ensure nil
;;   :hook (electric-pair-mode . prog-mode))

(use-package undo-tree
  :bind ("C-c u" . undo-tree-visualize)
  :hook (prog-mode . undo-tree-mode))

(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :config (vertico-mode))

(use-package savehist
  :config (savehist-mode))

(use-package rg
  :ensure-system-package rg
  :hook (grep-mode . (lambda () (toggle-truncate-lines 1))))

(use-package consult
  :ensure-system-package fd
  :bind ("C-c f" . consult-fd)
  :bind ("C-c g" . consult-ripgrep)
  :init  (setq xref-show-xrefs-function #'consult-xref
               xref-show-definitions-function #'consult-xref)
  :config (require 'consult-xref)
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package projectile
  :init (setq projectile-project-search-path '("~/devel/fortifiedid/"))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode)
  :config (setq projectile-project-search-path '("~/devel/fortifiedid/"))
  :config (projectile-discover-projects-in-search-path))

(use-package which-key
  :config (which-key-mode))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package magit
  :bind ("C-c m" . magit-status))

(use-package treemacs
  :custom (lsp-treemacs-theme "Iconless")
  :bind ("C-c t" . treemacs))


(use-package company
  :bind (("M-TAB" . company-complete)))

(use-package hs-minor-mode
  :ensure nil
  :bind (:map hs-minor-mode-map ("C-c v" . hs-toggle-hiding)))

(use-package bash-ts-mode
  :ensure nil
  :mode "\\.sh\\'")

(use-package json-ts-mode
  :mode "\\.json\\'"
  :hook (json-ts-mode . hs-minor-mode))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'"
  :hook (yaml-mode . undo-tree-mode))

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
;;;  :mode "\\.java\\'"
  :init (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.44.0/jdt-language-server-1.44.0-20250122155241.tar.gz")
  :init (setq lsp-java-java-path "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/bin/java")
  :init (setenv "JAVA_HOME"  "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/")
  :config (add-hook 'java-ts-mode-hook 'lsp))

;;; init.el ends here
