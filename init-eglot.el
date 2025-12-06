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
  
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc-doc-buffer)  ; show documentation
              ("C-c l e" . flymake-show-project-diagnostics))
  
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


;;; init-eglot.el ends here
