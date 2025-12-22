;;; init-project.el --- mxns config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar recentf-list)
(declare-function neo-global--window-exists-p "neotree")
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


(defun mxns/kill-buffer-project-aware ()
  "Kill current buffer and switch to most recent buffer in same project.
If no project buffers remain, invoke `project-switch-project'."
  (interactive)
  (let* ((proj (project-current))
         (current-buf (current-buffer)))
    (message "[DEBUG] Current buffer: %s" (buffer-name current-buf))
    (if (not proj)
        ;; Not in a project, just kill normally
        (progn
          (message "[DEBUG] Not in a project, killing normally")
          (kill-buffer current-buf))
      ;; In a project - find other project buffers BEFORE killing
      (let* ((project-buffers (project-buffers proj))
             (other-project-buffers
              (seq-filter (lambda (buf)
                           (let ((name (buffer-name buf)))
                             (and (not (eq buf current-buf))
                                  (not (minibufferp buf))
                                  (buffer-live-p buf)
                                  ;; Exclude internal buffers (names starting with space)
                                  (not (string-prefix-p " " name))
                                  ;; Exclude special buffers (names starting with *)
                                  (not (string-prefix-p "*" name))
                                  (memq buf project-buffers))))
                         (buffer-list)))
             (target-buffer (car other-project-buffers)))
        (message "[DEBUG] Total project buffers: %d" (length project-buffers))
        (message "[DEBUG] Project buffer names: %s"
                 (mapconcat #'buffer-name project-buffers ", "))
        (message "[DEBUG] Filtered project buffers (excluding special): %d"
                 (length other-project-buffers))
        (message "[DEBUG] Filtered buffer names: %s"
                 (mapconcat #'buffer-name other-project-buffers ", "))
        (message "[DEBUG] Target buffer: %s"
                 (if target-buffer (buffer-name target-buffer) "nil"))
        (if target-buffer
            (progn
              ;; Switch first, then kill - avoids Emacs auto-switching to random buffer
              (message "[DEBUG] Switching to target buffer: %s" (buffer-name target-buffer))
              (switch-to-buffer target-buffer)
              (message "[DEBUG] After switch, current buffer: %s" (buffer-name (current-buffer)))
              (kill-buffer current-buf)
              (message "[DEBUG] After kill, current buffer: %s" (buffer-name (current-buffer))))
          ;; Last buffer in project - kill and let user decide next action
          (message "[DEBUG] No other project buffers, calling project-switch-project")
          (let ((project-root (project-root proj)))
            (kill-buffer current-buf)
            (vc-dir project-root)
            (message "[DEBUG] After kill, before project-switch-project, current buffer: %s"
                     (buffer-name (current-buffer)))
            (project-switch-project project-root)))))))


(defun mxns/tree-compile ()
  "Choose and run a compile command for current project."
  (interactive)
  (if (boundp 'mxns/tree-compile-commands)
      (let ((cmd (completing-read
                  "Compile command: "
                  mxns/tree-compile-commands
                  nil nil nil nil
                  (car mxns/tree-compile-commands))))
        (compile cmd))
    (call-interactively 'compile)))  ; Fallback to normal compile


;; Mark variable as safe when it's a list of strings
(put 'mxns/tree-compile-commands 'safe-local-variable
     (lambda (val)
       (and (listp val)
            (seq-every-p #'stringp val))))


(use-package project
  :ensure nil
  :bind-keymap
  ("C-c p" . mxns/project-prefix-map))


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
    (define-key map "c" 'mxns/tree-compile)
    (define-key map "q" 'mxns/project-kill-project)
    (define-key map "r" 'project-query-replace-regexp)
    (define-key map "b" 'project-switch-to-buffer)
    (define-key map "p" 'project-switch-project)
    (define-key map "d" 'project-find-dir)
    (define-key map "D" 'project-dired)
    (define-key map "v" 'project-vc-dir)
    (define-key map "\C-b" 'project-list-buffers)
    (define-key map "k" 'mxns/kill-buffer-project-aware)
    ;; (define-key map "F" 'project-or-external-find-file)
    ;; (define-key map "G" 'project-or-external-find-regexp)
    map)
  "Keymap for project commands.")

(which-key-add-keymap-based-replacements mxns/project-prefix-map
    "f" "Find file (fd)"
    "g" "Grep (rg)"
    "a" "Switch project"
    "c" "Compile"
    "q" "Kill project"
    "r" "Query replace regexp"
    "b" "Switch to buffer"
    "p" "Open project"
    "d" "Find directory"
    "D" "Open in Dired"
    "v" "VC directory"
    "C-b" "List buffers"
    "k" "Kill buffer (project)"
    )

;;; init-project.el ends here
