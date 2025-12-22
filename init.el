;;; init.el --- mxns config -*- lexical-binding: t; -*-

;;; Commentary:
;;; My configuration

;;; Code:

(require 'package)
(require 'use-package)
(require 'xref)
(require 'recentf)
(require 'ansi-color)

(recentf-mode 1)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/local/")

(package-initialize)
;;; (package-refresh-contents)

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
(defvar undo-fu-session-mode-hook-allow-list)

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
(load "~/.emacs.d/init-nav")
(load "~/.emacs.d/init-project")
(load "~/.emacs.d/init-sql-client")
(load "~/.emacs.d/init-eglot")
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


(defun mxns/ansi-colorize-buffer ()
  "Interpret ANSI escape sequences in the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (with-silent-modifications
        (ansi-color-apply-on-region (point-min) (point-max))))))


;; (use-package ranger)


;; (use-package transpose-frame
;;   :ensure t
;;   :bind ("C-x 4 t" . transpose-frame))


(use-package delight
  :ensure t
  :config
  (delight '((eldoc-mode nil "eldoc")
             (mxns/nav-mode nil "nav"))))


(use-package xref
  :bind (("C-c <left>"  . xref-go-back)
         ("C-c <right>" . xref-go-forward)
         ("C-c b"  . xref-go-back)
         ("C-c f" . xref-go-forward)))


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
  (undo-fu-session-global-mode 1))

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
  :delight
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


;; https://protesilaos.com/emacs/dotemacs#h:61863da4-8739-42ae-a30f-6e9d686e1995
(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-e C-c" . embark-collect)
         ("C-e C-e" . embark-export)))


(use-package embark-consult
  :ensure t)


(use-package consult
  :ensure-system-package fd
  :bind
  (("C-c r" . consult-buffer)
   ("C-<tab>" . consult-buffer)
   :map vertico-map
   ("C-<tab>". vertico-next))
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
  :delight
  :config
  (which-key-mode 1)
  )


(use-package company
  :delight
  :bind (("M-TAB" . company-complete))
  :init
  (global-company-mode))


;; https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))


(use-package vimish-fold
  :ensure t
  :config
  (vimish-fold-global-mode 1)
  :bind (("C-c v f" . vimish-fold)
         ("C-c v u" . vimish-fold-unfold)
         ("C-c v t" . vimish-fold-toggle)
         ("C-c v d" . vimish-fold-delete)
         ("C-c v D" . vimish-fold-delete-all)))


(use-package magit
  :bind ("C-c m" . magit-status))


(use-package yasnippet
  :delight yas-minor-mode
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
  :delight
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
  :delight
  :hook (typescript-ts-mode . electric-pair-mode))


(use-package java-ts-mode
  :delight
  :ensure nil
  :mode "\\.java\\'"
  :hook (java-ts-mode . electric-pair-mode))


(use-package apheleia
  :delight
  :ensure apheleia
  :delight
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


;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
