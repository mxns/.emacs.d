;;; init-neotree.el --- mxns config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function neo-global--window-exists-p "neotree")

(use-package neotree
  :ensure t
  :bind
  (("C-c t" . neotree-project-root-toggle)
   ("C-c T" . neotree-project-collapse-others))
  :hook
  (neotree-mode . hl-line-mode)
  :custom-face
  (hl-line ((t (:inverse-video t))))
  :config
  (setq neo-show-hidden-files t)
  (setq neo-autorefresh t)
  (setq neo-theme 'arrow)
  (setq neo-smart-open t)
  (setq neo-window-width 30)

  (defun neotree-project-root ()
    "Open neotree at the project root and find current file."
    (interactive)
    (let* ((project-root (if-let ((project (project-current)))
                             (project-root project)
                           default-directory))
           (current-file (buffer-file-name)))
      (save-selected-window
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-root)
              (when current-file (neotree-find current-file)))
          (progn
            (neotree-show)
            (neotree-dir project-root)
            (when current-file
              (run-with-idle-timer 0.1 nil
                                   (lambda ()
                                     (save-selected-window
                                       (neotree-find current-file))))))))))
  
  (defun neotree-project-root-toggle ()
    "Toggle neotree at the project root and find current file."
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (neotree-project-root)))

  (defun neotree-project-collapse-others ()
    "Collapse all neotree nodes and show current file."
    (interactive)
    (when (neo-global--window-exists-p)
      (save-selected-window
        (neo-global--select-window)
        (neotree-collapse-all)))
    (neotree-project-root))
  
  (defun neotree-project-root-after-switch (&rest _args)
    "Open neotree at project root after switching projects."
    (when (and (project-current)
               (not current-prefix-arg))  ; skip if called with C-u
      (neotree-project-root)))
  
  (advice-add 'mxns/project-switch-project :after #'neotree-project-root-after-switch)
)

;;; init-neotree.el ends here
