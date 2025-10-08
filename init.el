;;; init.el --- mxns config -*- lexical-binding: t; -*-

;;; Commentary:
;;; My configuration

;;; Code:

(require 'package)
(require 'use-package)
(require 'xref)
(require 'recentf)
(recentf-mode 1)

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
(defvar consult-fd-args)
(defvar mxns/window-zoom-p nil "Track window zoom state.")

(declare-function lsp-java-type-hierarchy "lsp-java" ())
(declare-function lsp-find-definition "lsp" ())
(declare-function lsp-find-references "lsp" ())
(declare-function lsp-rename "lsp" ())
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
(setq suggest-key-bindings nil)
(setq delete-by-moving-to-trash t)

;; (show-paren-mode 1)
;; (setq match-paren--idle-timer
;;       (run-with-idle-timer match-paren--delay t #'blink-matching-open))

;;;; per https://github.com/emacs-lsp/lsp-mode#performance
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold 200000000)
(setq garbage-collection-messages nil)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))
(load "~/.emacs.d/scrolling")
(xterm-mouse-mode 1)
(mouse-wheel-mode 1)

(when (>= emacs-major-version 28)
  (setq lock-file-name-transforms
        '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t))))
(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
(setq backup-directory-alist
      '((".*" . "~/.emacs.d/aux/")))

(add-hook 'occur-hook
          (lambda ()
            (switch-to-buffer-other-window "*Occur*")))

(defun mxns/consult-fd-hidden ()
  "Find files in project, including hidden files."
  (interactive)
  (let ((consult-fd-args "fd --hidden"))
    (consult-fd)))


(defun mxns/switch-to-first-project-buffer (project-path)
  "Switch to the most recently used buffer belonging to PROJECT-PATH.
If no buffer is found, fallback to opening the most recently used file
in the project using `recentf`."
  (interactive
   (list (completing-read "Switch to project: "
                          (project-known-project-roots)
                          nil t)))
  (let* ((expanded-project-path (expand-file-name project-path))
         ;; Buffers in most-recently-used order
         (project-buffers (seq-filter
                           (lambda (buf)
                             (let ((file (buffer-file-name buf)))
                               (and file
                                    (string-prefix-p expanded-project-path
                                                     (expand-file-name file)))))
                           (buffer-list)))
         (most-recent-buffer (car project-buffers)))
    (cond
     (most-recent-buffer
      (switch-to-buffer most-recent-buffer)
      (message "Switched to buffer: %s" (buffer-name most-recent-buffer)))
     (t
      (let* ((recent-files-in-project
              (seq-filter
               (lambda (file)
                 (string-prefix-p expanded-project-path
                                  (expand-file-name file)))
               recentf-list))
             (most-recent-file (car recent-files-in-project)))
        
        (if most-recent-file
            (progn
              (find-file most-recent-file)
              (message "Opened recent file: %s" most-recent-file))
          (project-switch-project project-path)))))))


(defun mxns/treemacs-toggle-lsp-symbols ()
  "Toggle LSP Symbols window without changing focus."
  (interactive)
  (let ((current-window (selected-window)))
    (if (get-buffer-window "*Lsp Symbols List*")
        ;; If symbols window is visible, close it
        (when-let ((window (get-buffer-window "*Lsp Symbols List*")))
          (delete-window window))
      ;; If symbols window is not visible, open it and return focus
      (progn
        (lsp-treemacs-symbols)
        (select-window current-window)))))


(defun mxns/toggle-window-zoom (&optional arg)
  "Toggle window zoom state.
With universal argument ARG, use current configuration."
  (interactive)
  (if arg
      (progn
        (window-configuration-to-register ?z)
        (delete-other-windows)
        (setq mxns/window-zoom-p t))
    (if mxns/window-zoom-p
        (condition-case err
            (progn
              (jump-to-register ?z)
              (setq mxns/window-zoom-p nil))
          (error
           (setq mxns/window-zoom-p nil)
           (message "Error while de-zooming window: %s" (error-message-string err))))
      (progn
        (window-configuration-to-register ?z)
        (delete-other-windows)
        (setq mxns/window-zoom-p t)))))

(define-advice delete-other-windows (:after (&rest _) reset-maximized)
  "Reset the window zoom state toggle."
  (setq mxns/window-zoom-p nil))

(global-set-key (kbd "C-c z") 'mxns/toggle-window-zoom)


;; (use-package ranger)


;; (use-package transpose-frame
;;   :ensure t
;;   :bind ("C-x 4 t" . transpose-frame))


(use-package xref
  :bind (("C-c <left>"  . xref-go-back)
         ("C-c <right>" . xref-go-forward)))


(use-package display-line-numbers
  :hook
  (nxml-mode . display-line-numbers-mode)
  (prog-mode . display-line-numbers-mode))


;; vundo and undo-tree are mutally exclusive
(use-package vundo
  :bind
  ("C-c u" . vundo))

(use-package undo-fu-session
  :ensure t
  :functions
  global-undo-fu-session-mode
  :config
  ;; Store undo session files in ~/.emacs.d/aux
  (setq undo-fu-session-directory (expand-file-name "aux" user-emacs-directory))
  
  ;; Exclude sensitive files
  (setq undo-fu-session-incompatible-files
        (list (concat "^" (expand-file-name "~/.secrets/"))))
  
  ;; Only enable for specific major modes
  (setq undo-fu-session-mode-hook-allow-list
        '(text-mode-hook
          prog-mode-hook
          conf-mode-hook))
  
  ;; Enable global mode
  (global-undo-fu-session-mode 1))

;; vundo and undo-tree are mutally exclusive
;; (use-package undo-tree
;;   :hook
;;   (prog-mode . undo-tree-mode)
;;   (conf-space-mode . undo-tree-mode)
;;   (yaml-mode . undo-tree-mode)
;;   (nxml-mode . undo-tree-mode)
;;   :bind
;;   ("C-c u" . undo-tree-visualize)
;;   :config
;;   (setq undo-tree-enable-undo-in-region t
;;         undo-tree-auto-save-history t
;;         undo-tree-history-directory-alist '((".*" . "~/.emacs.d/aux/"))
;;         undo-tree-visualizer-timestamps t
;;         undo-tree-visualizer-diff t)
;;   (make-directory "~/.emacs.d/aux/" t))


(use-package goggles
  :hook ((prog-mode text-mode conf-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing


;; (use-package conf-space-mode
;;   :ensure nil)


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
  ("C-c F" . #'mxns/consult-fd-hidden)
  ("C-c g" . consult-ripgrep)
  ("C-c r" . consult-buffer)
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


;; https://protesilaos.com/emacs/dotemacs#h:61863da4-8739-42ae-a30f-6e9d686e1995
(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-e C-c" . embark-collect)
         ("C-e C-e" . embark-export)))


(use-package embark-consult
  :ensure t)


;; https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))


(use-package hs-minor-mode
  :ensure nil
  :bind
  (("C-c v" . hs-toggle-hiding)))


(use-package origami-mode
  :ensure nil
  :bind
  (:map origami-mode-map ("C-c v" . origami-toggle-node)))


(use-package project
  :ensure nil
  :bind (("C-c b" . project-switch-to-buffer)
         ("C-c q" . mxns/switch-to-first-project-buffer))
  :bind-keymap ("C-c p" . project-prefix-map))


(use-package treemacs
  :functions
  mxns/treemacs-on-project-switch
  mxns/treemacs-on-project-kill
  mxns/switch-to-first-project-buffer
  lsp-workspace-folders-remove
  treemacs-do-remove-project-from-workspace

  :custom
  (treemacs-git-mode -1)
  
  :bind
  ("C-c t" . mxns/treemacs-toggle-preserve-window)
  ("C-c s" . mxns/treemacs-toggle-lsp-symbols)

  :config
  (defun mxns/treemacs-on-project-switch (&rest _)
    "Function to run when switching projects."
    (save-selected-window (treemacs-add-and-display-current-project)))

  (defun mxns/treemacs-on-project-kill (&rest _)
    "Remove project from Treemacs workspace."
    (when (fboundp 'treemacs-remove-project-from-workspace)
      (condition-case err
          (progn
            (treemacs-do-remove-project-from-workspace (project-root (project-current)))
            (lsp-workspace-folders-remove (project-root (project-current))))
        (error (message "Treemacs cleanup failed: %s" err)))))
  
  (defun mxns/treemacs-toggle-preserve-window ()
    "Toggle Treemacs without changing the selected window."
    (interactive)
    (save-selected-window
      (treemacs)))

  (advice-add 'mxns/switch-to-first-project-buffer :after #'mxns/treemacs-on-project-switch)
  (advice-add 'project-kill-buffers :before #'mxns/treemacs-on-project-kill))


(use-package magit
  :bind ("C-c m" . magit-status))


(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


(use-package yasnippet
  :config
  (yas-global-mode))


(use-package terraform-mode
  :mode
  "\\.tf\\'")


(use-package treesit-auto
  :functions
  treesit-auto-add-to-auto-mode-alist
  global-treesit-auto-mode
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(use-package yaml-mode
  :mode
  "\\.yml\\'"
  "\\.yaml\\'")


(use-package nxml-mode
  :ensure nil
  :init
  (setq nxml-child-indent 4))


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


(use-package typescript-ts-mode
  :hook (typescript-ts-mode . electric-pair-mode))


(use-package java-ts-mode
  :ensure nil
  :mode "\\.java\\'"
  :hook (java-ts-mode . electric-pair-mode))


(use-package apheleia
  :ensure apheleia
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config

  ;; Add commands to apheleia formatters
  (setf (alist-get 'prettier-js apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'prettier-java apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))

  ;; Map modes to formatters
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier-js
        (alist-get 'tsx-ts-mode        apheleia-mode-alist) 'prettier-js)
  (setf (alist-get 'js-ts-mode         apheleia-mode-alist) 'prettier-js)
  (setf (alist-get 'json-ts-mode       apheleia-mode-alist) 'prettier-js)
  (setf (alist-get 'java-ts-mode       apheleia-mode-alist) 'prettier-java)

  ;; Format on save is annoying, use apheleia-format-buffer manually instead
  (apheleia-global-mode -1))


;;; thanks to https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
(use-package lsp-mode
  :diminish "LSP"

  :ensure t

  :functions
  lsp-workspaces
  mxns/cleanup-lsp-on-project-kill

  :init
  (setq lsp-use-plists nil)

  :bind
  ("M-RET" . lsp-execute-code-action)

  :config
  (defun mxns/cleanup-lsp-on-project-kill (&rest _)
    "Remove LSP workspace folder when killing project buffers."
    (when (and (fboundp 'lsp-workspace-folders-remove)
               (lsp-workspaces))
      (let ((current-project (project-current)))
        (when current-project
          (let ((project-root (project-root current-project)))
            (condition-case err
                (lsp-workspace-folders-remove project-root)
              (error (message "Failed to remove LSP workspace folder: %s" err))))))))

  ;;(advice-add 'project-kill-buffers :after #'mxns/lsp-on-project-kill)
  
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode
           json-ts-mode
           bash-ts-mode
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
  (lsp-ui-sideline-show-code-actions t)
  (lsp-auto-execute-action nil)
  
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
  (lsp-lens-enable t)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter
  )


(use-package lsp-treemacs
  :custom (lsp-treemacs-theme "Iconless"))


(use-package helm-lsp)


(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-flycheck-list-position 'bottom)
  :bind
  (:map lsp-ui-flycheck-list-mode-map
        ("RET" . lsp-ui-flycheck-list--visit)
        ("M-RET" . lsp-ui-flycheck-list--view)
        ("n" . next-line)
        ("p" . previous-line)
        ))

;;; https://download.eclipse.org/jdtls/milestones/
(use-package lsp-java
  :init
  (setq lsp-java-jdt-download-url
        "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.49.0/jdt-language-server-1.49.0-202507311558.tar.gz")
  (setq lsp-java-java-path
        "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/bin/java")
  (setenv "JAVA_HOME"
          "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/"))


(defcustom lsp-ui-sideline-cycle-start-state 0
  "Starting state for `lsp-ui-sideline-cycle-toggle'.
0: hover off, code-actions off
1: hover on, code-actions off
2: hover off, code-actions on
3: hover on, code-actions on"
  :type '(choice (const :tag "Both off" 0)
                 (const :tag "Hover only" 1)
                 (const :tag "Code actions only" 2)
                 (const :tag "Both on" 3))
  :group 'lsp-ui-sideline)

(defvar lsp-ui-sideline-cycle-state 3
  "Current state in the sideline cycle.
Initialized from `lsp-ui-sideline-cycle-start-state'.")

(defun lsp-ui-sideline-cycle-toggle ()
  "Cycle through LSP UI sideline display states.
State 0: hover off, code-actions off
State 1: hover on, code-actions off
State 2: hover off, code-actions on
State 3: hover on, code-actions on"
  (interactive)
  ;; Initialize state if nil
  (unless lsp-ui-sideline-cycle-state
    (setq lsp-ui-sideline-cycle-state lsp-ui-sideline-cycle-start-state))
  
  ;; Advance to next state
  (setq lsp-ui-sideline-cycle-state
        (mod (1+ lsp-ui-sideline-cycle-state) 4))
  
  ;; Apply settings based on state
  (pcase lsp-ui-sideline-cycle-state
    (0 (setq lsp-ui-sideline-show-hover nil
             lsp-ui-sideline-show-code-actions nil)
       (message "LSP UI Sideline: Both off"))
    (1 (setq lsp-ui-sideline-show-hover t
             lsp-ui-sideline-show-code-actions nil)
       (message "LSP UI Sideline: Hover only"))
    (2 (setq lsp-ui-sideline-show-hover nil
             lsp-ui-sideline-show-code-actions t)
       (message "LSP UI Sideline: Code actions only"))
    (3 (setq lsp-ui-sideline-show-hover t
             lsp-ui-sideline-show-code-actions t)
       (message "LSP UI Sideline: Both on")))
  
  ;; Refresh if lsp-ui-mode is active
  (when (bound-and-true-p lsp-ui-mode)
    (lsp-ui-sideline--run)))

(global-set-key (kbd "C-c l t") 'lsp-ui-sideline-cycle-toggle)

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
