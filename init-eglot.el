;;; init-eglot.el --- mxns config -*- lexical-binding: t; -*-

;;; Commentary:
;;; My eglot configuration

;;; Code:

(use-package eglot
  :ensure t
  :hook
  ;; Enable for your languages - adjust as needed
  ((typescript-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (java-mode . eglot-ensure)
   (python-mode . eglot-ensure))

  :bind-keymap ("C-c l" . mxns/eglot-prefix-map)
  
  :config
  ;; Shutdown server when last buffer is killed
  (setq eglot-autoshutdown t)
  
  ;; Sync buffer changes immediately
  (setq eglot-send-changes-idle-time 0)
  
  ;; Don't log every event (improves performance)
  (setq eglot-events-buffer-config '(:size 0 :format full))
  
  ;; Show all available completions
  (setq eglot-ignored-server-capabilities '())
  
  ;; Use custom server commands if needed
  ;; (add-to-list 'eglot-server-programs
  ;;              '(java-mode . ("jdtls")))
  )

;; Enhance completion with corfu or company
;; (use-package corfu
;;   :ensure t
;;   :init (global-corfu-mode)
;;   :config
;;   (setq corfu-auto t
;;         corfu-auto-delay 0.1
;;         corfu-auto-prefix 2))

;; Better documentation popups
(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

;; Breadcrumb in headerline (optional but nice)
(use-package breadcrumb
  :ensure t
  :hook (eglot-managed-mode . breadcrumb-mode))

(defvar mxns/eglot-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'eglot-code-actions)
    (define-key map "r" 'eglot-rename)
    (define-key map "d" 'eldoc-doc-buffer)
    (define-key map "e" 'flymake-show-project-diagnostics)
    (define-key map "b" 'flymake-show-buffer-diagnostics)
    (define-key map "c" 'consult-flymake)
    (define-key map "n" 'flymake-goto-next-error)
    (define-key map "p" 'flymake-goto-prev-error)
    map)
  "Keymap for code commands.")

(which-key-add-keymap-based-replacements mxns/eglot-prefix-map
    "a" "Code Actions"
    "r" "Rename"
    "d" "Doc"
    "e" "Project diagnostics"
    "b" "Buffer diagnostics"
    "c" "Consult Flymake"
    "n" "Next Error"
    "p" "Prev Error"
    )


;;; init-eglot.el ends here
