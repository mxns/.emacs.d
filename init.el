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
(declare-function lsp-find-definition "lsp" ())
(declare-function lsp-find-references "lsp" ())
(declare-function lsp-rename "lsp" ())
(declare-function lsp-execute-code-action "lsp" ())
(declare-function treemacs-remove-project-from-workspace "treemacs" ())

(setq confirm-kill-emacs 'y-or-n-p)
(setq ns-right-option-modifier 'option)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))
(setq use-package-always-ensure t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default indent-tabs-mode nil)

;; (show-paren-mode 1)
;; (setq match-paren--idle-timer
;;       (run-with-idle-timer match-paren--delay t #'blink-matching-open))

;;;; per https://github.com/emacs-lsp/lsp-mode#performance
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold 200000000)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))
(load "~/.emacs.d/friendly")
(load "~/.emacs.d/scrolling")

(defvar my-lsp-java-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c g") #'lsp-find-definition)
    (define-key map (kbd "C-c c f") #'lsp-find-references)
    (define-key map (kbd "C-c c r") #'lsp-rename)
    (define-key map (kbd "C-c c x") #'lsp-execute-code-action)
    (define-key map (kbd "C-c c t") #'lsp-java-type-hierarchy)
    (define-key map (kbd "C-c c c") #'lsp-treemacs-call-hierarchy)
    (define-key map (kbd "C-c c e") #'lsp-treemacs-errors-list)
    (define-key map (kbd "C-c c o") #'helm-lsp-workspace-symbol)
    map)
  "Keymap for `my-lsp-java-mode'.")

(define-minor-mode my-lsp-java-mode
  "Minor mode to add Java-specific keybindings."
  :lighter " MyJava"
  :keymap my-lsp-java-mode-map)

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
  :config
  (require 'consult-xref)
  :hook
  (completion-list-mode . consult-preview-at-point-mode))

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

(use-package which-key
  :config
  (which-key-mode))

(use-package company
  :bind (("M-TAB" . company-complete)))

(use-package hs-minor-mode
  :ensure nil
  :bind
  (:map hs-minor-mode-map ("C-c v" . hs-toggle-hiding)))

(use-package origami-mode
  :ensure nil
  :bind
  (:map origami-mode-map ("C-c v" . origami-toggle-node)))

(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :config
  (defun mxns/on-project-switch (&rest _)
    "Function to run after switching projects."
    (save-selected-window (treemacs-add-and-display-current-project)))

  (defun mxns/on-project-kill (&rest _)
    "Function to run after killing projects."
    (treemacs-remove-project-from-workspace))

  (defun mxns/switch-to-first-project-buffer (project-path)
    "Switch to the first buffer belonging to PROJECT-PATH from file-name-history.
Uses file-name-history to find the most recently used file in the project."
    (interactive
     (list (completing-read "Project: "
                            (project-known-project-roots)
                            nil t)))
    
    (let* ((expanded-project-path (expand-file-name project-path))
           ;; Filter file-name-history for the specified project
           (project-files (cl-remove-if-not
                           (lambda (item)
                             (and (stringp item)
                                  (string-prefix-p expanded-project-path
                                                   (expand-file-name item))))
                           file-name-history))
           ;; Get first (most recent) item
           (first-file (car project-files)))
      
      (if first-file
          (let ((existing-buffer (find-buffer-visiting (expand-file-name first-file))))
            (if existing-buffer
                (progn
                  (switch-to-buffer existing-buffer)
                  (message "Switched to buffer: %s" (buffer-name existing-buffer)))
              (find-file (expand-file-name first-file))
              (message "Opened file: %s" first-file)
              (mxns/on-project-switch)))
        
        (message "No files found for project %s in history" project-path))))

  (global-set-key (kbd "C-c b") 'project-switch-to-buffer)
  (global-set-key (kbd "C-c q") 'mxns/switch-to-first-project-buffer)
  (advice-add 'project-switch-project :after #'mxns/on-project-switch)
  (advice-add 'project-kill-buffers :after #'mxns/on-project-kill))

(use-package magit
  :bind ("C-c m" . magit-status))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package terraform-mode
  :mode
  "\\.tf\\'")

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package yaml-mode
  :mode
  "\\.yml\\'"
  "\\.yaml\\'"
  :hook (yaml-mode . undo-tree-mode))

(use-package nxml-mode
  :ensure nil
  :hook (nxml-mode . undo-tree-mode))

;; (use-package bash-ts-mode
;;   :ensure nil
;;   :mode
;;   "\\.sh\\'")

;; (use-package json-ts-mode
;;   :mode
;;   "\\.json\\'"
;;   :hook
;;   (json-ts-mode . hs-minor-mode)
;;   (json-ts-mode . electric-pair-mode))

;; (use-package typescript-ts-mode
;;   :mode
;;   "\\.ts\\'"
;;   "\\.tsx\\'"
;;   :hook (typescript-s-mode . electric-pair-mode))

;; (use-package java-ts-mode
;;   :mode "\\.java\\'"
;;   :hook (java-ts-mode . electric-pair-mode))

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
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("java" "-jar" "/Users/mxns/java/google/google-java-format-1.26.0-all-deps.jar" "-"))
  (setf (alist-get 'java-ts-mode apheleia-mode-alist)
        'google-java-format)
  (apheleia-global-mode +1))

;;; thanks to https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . (lambda ()
                       (when (derived-mode-p 'java-mode)
                         (my-lsp-java-mode 1))))
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode
           java-ts-mode
           python-ts-mode) . lsp-deferred))
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
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure nil)     ; Debug support (causes error if X is not available)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-doc-enable nil)                ; causes error if X is not available
  (lsp-ui-sideline-show-hover t)      ; Sideline used only for diagnostics
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
  (lsp-ui-doc-use-childframe nil)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter
  
  :init
  (setq lsp-use-plists nil))

(use-package lsp-treemacs
  :custom (lsp-treemacs-theme "Iconless")
  :bind ("C-c t" . treemacs))

(use-package helm-lsp)

(use-package lsp-ui)

;;; https://download.eclipse.org/jdtls/milestones/
(use-package lsp-java
  :init
  (setq lsp-java-jdt-download-url
        "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.46.1/jdt-language-server-1.46.1-202504011455.tar.gz")
  (setq lsp-java-java-path
        "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/bin/java")
  (setenv "JAVA_HOME"
          "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/"))

;;; init.el ends here
