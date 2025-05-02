;;; init.el --- mxns config -*- lexical-binding: t; -*-

;;; Commentary:
;;; My configuration

;;; Code:

(require 'package)
(require 'use-package)
(require 'xref)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/local/")

(package-initialize)
;;; (package-refresh-contents)

;;; (setenv "LSP_USE_PLISTS" "true")

;;; Make the byte-compiler happy and get rid of warnings.
(defvar origami-mode-map)
(defvar hs-minor-mode-map)
(defvar xref-show-xrefs-function)
(defvar xref-show-definitions-function)
(defvar ns-right-option-modifier)
(defvar match-paren--idle-timer nil)
(defvar match-paren--delay 0.5)
(declare-function lsp-java-type-hierarchy "lsp-java" ())

(setq confirm-kill-emacs 'y-or-n-p)
(setq ns-right-option-modifier 'option)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))
(setq use-package-always-ensure t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default indent-tabs-mode nil)
;; (setq show-paren-style 'parenthesis
;;       show-paren-when-point-inside-paren t
;;       show-paren-when-point-in-periphery t
;;       show-paren-context-when-offscreen t
;;       show-paren-delay 2)
(show-paren-mode 1)
(setq match-paren--idle-timer
      (run-with-idle-timer match-paren--delay t #'blink-matching-open))

;;;; per https://github.com/emacs-lsp/lsp-mode#performance
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold 200000000)

(load "~/.emacs.d/friendly")
(load "~/.emacs.d/scrolling")
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(defvar my-prefix-map (make-sparse-keymap)
  "My custom keymap for prefixed commands.")
(define-key my-prefix-map (kbd "r") #'xref-find-references)
(define-key global-map (kbd "C-c a") my-prefix-map)

(use-package undo-tree
  :bind
  ("C-c u" . undo-tree-visualize)
  :hook
  (prog-mode . undo-tree-mode))

(use-package conf-space-mode
  :ensure nil
  :hook
  (conf-space-mode . undo-tree-mode))

(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :config
  (vertico-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless)))

(use-package savehist
  :config (savehist-mode))

(use-package rg
  :ensure-system-package rg
  :hook
  (grep-mode . (lambda () (toggle-truncate-lines 1))))

(use-package consult
  :ensure-system-package fd
  :bind
  ("C-c f" . consult-fd)
  ("C-c g" . consult-ripgrep)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config (require 'consult-xref)
  :hook (completion-list-mode . consult-preview-at-point-mode))

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
  :init
  (setq projectile-project-search-path '("~/devel/fortifiedid/"))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :hook
  (projectile-after-switch-project . (lambda () (save-selected-window (treemacs-add-and-display-current-project-exclusively))))
  :config
  (projectile-mode)
  (projectile-discover-projects-in-search-path))

(use-package which-key
  :config
  (which-key-mode))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package magit
  :bind ("C-c m" . magit-status))

(use-package treemacs
  :custom (lsp-treemacs-theme "Iconless")
  :bind ("C-c t" . treemacs))

(use-package company
  :bind (("M-TAB" . company-complete)))

(use-package hs-minor-mode
  :ensure nil
  :bind
  (:map hs-minor-mode-map ("C-c v" . hs-toggle-hiding)))

(use-package origami-mode
  :ensure nil
  :bind
  (:map origami-mode-map ("C-c v" . origami-toggle-node))
  (:map origami-mode-map ("C-c C-v" . origami-toggle-node)))

(use-package terraform-mode
  :mode
  "\\.tf\\'")

(use-package bash-ts-mode
  :ensure nil
  :mode
  "\\.sh\\'")

(use-package json-ts-mode
  :mode
  "\\.json\\'"
  :hook
  (json-ts-mode . hs-minor-mode))

(use-package yaml-mode
  :mode
  "\\.yml\\'"
  "\\.yaml\\'"
  :hook (yaml-mode . undo-tree-mode))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'")

;;; thanks to https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :capf)       ; Using CAPF as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover nil)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter
  
  :init
  (setq lsp-use-plists nil)

  :config
  (define-key my-prefix-map (kbd "c") #'lsp-treemacs-call-hierarchy)
  (define-key my-prefix-map (kbd "o") #'helm-lsp-workspace-symbol))

(use-package lsp-ui)

(use-package apheleia
  :ensure apheleia
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))

;;; https://repo.eclipse.org/content/repositories/jdtls-releases/org/eclipse/jdt/ls/org.eclipse.jdt.ls.core/
(use-package lsp-java
;;;  :mode "\\.java\\'"
  :init
  (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/1.46.1/jdt-language-server-1.46.1-202504011455.tar.gz")
  (setq lsp-java-java-path "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/bin/java")
  (setenv "JAVA_HOME"  "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/")
  :config
  (add-hook 'java-ts-mode-hook 'lsp)
  (define-key my-prefix-map (kbd "t") #'lsp-java-type-hierarchy))

;;; init.el ends here
