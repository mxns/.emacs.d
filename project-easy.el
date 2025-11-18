;;; project-easy.el --- mxns config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar recentf-list)
(declare-function project-root "treemacs" (project))

(defun mxns/project-switch-project (project-path)
  "Switch to the most recently used buffer belonging to PROJECT-PATH.
If no buffer is found, fallback to opening the most recently used file
in the project using `recentf'.
If no recent file is found, fallback to user selection via
`project-switch-project'p."
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


(defun mxns/project-kill-project ()
  "Remove from Treemacs workspace, remove LSP workspace folders, and kill buffers."
  (interactive)
  (let ((root (project-root (project-current))))
    (progn
      (when (and
             (fboundp 'treemacs-do-remove-project-from-workspace)
             (y-or-n-p (format "Remove project %s from Treemacs workspace?" root)))
        (condition-case err
            (treemacs-do-remove-project-from-workspace root)
          (error (message "Failed to remove project %s from Treemacs workspace: %s" root err))))
      (when (and
             (fboundp 'lsp-workspace-folders-remove)
             (y-or-n-p (format "Remove LSP folders for project %s?" root)))
        (condition-case err
            (lsp-workspace-folders-remove root)
          (error (message "Failed to remove LSP folders for project %s: %s" root err))))
      (project-kill-buffers))))


(use-package project
  :ensure nil
  :bind-keymap
  ("C-c p" . mxns/project-prefix-map))


(use-package treemacs
  :functions
  mxns/treemacs-switch-project
  mxns/project-switch-project
  lsp-workspace-folders-remove
  treemacs-do-remove-project-from-workspace

  :init
  (setq project-kill-buffers-display-buffer-list t)

  :custom
  (treemacs-git-mode -1)
  
  :bind
  ("C-c s" . mxns/treemacs-toggle-lsp-symbols)
  ("C-c t" . mxns/treemacs-toggle-preserve-window)

  :config
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

  (defun mxns/treemacs-toggle-preserve-window ()
    "Toggle Treemacs without changing the selected window."
    (interactive)
    (save-selected-window
      (treemacs)))

  (defun mxns/treemacs-switch-project (&rest _)
    "Function to run when switching projects."
    (save-selected-window (treemacs-add-and-display-current-project)))

  (advice-add 'mxns/project-switch-project :after #'mxns/treemacs-switch-project))



(defun mxns/project-mode-line ()
  "Return project name for mode-line."
  (let ((project (project-current)))
    (when project
      (propertize
       (concat " ["
               (file-name-nondirectory
                (directory-file-name (project-root project)))
               "]")
       'face 'font-lock-keyword-face))))

;; Add to mode-line-misc-info
(add-to-list 'mode-line-misc-info '(:eval (mxns/project-mode-line)) t)


(defvar mxns/project-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'consult-fd)
    (define-key map "g" 'consult-ripgrep)
    (define-key map "a" 'mxns/project-switch-project)
    (define-key map "q" 'mxns/project-kill-project)
    (define-key map "r" 'project-query-replace-regexp)
    (define-key map "b" 'project-switch-to-buffer)
    (define-key map "p" 'project-switch-project)
    (define-key map "d" 'project-find-dir)
    (define-key map "D" 'project-dired)
    (define-key map "v" 'project-vc-dir)
    (define-key map "\C-b" 'project-list-buffers)
    ;; (define-key map "F" 'project-or-external-find-file)
    ;; (define-key map "k" 'project-kill-buffers)
    ;; (define-key map "G" 'project-or-external-find-regexp)
    map)
  "Keymap for project commands.")


;;; project-easy.el ends here
