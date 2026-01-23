;;; prosecco.el --- minimal overlay on the `project' package -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar recentf-list)


(defun prosecco-mode-line ()
  "Return project name for mode-line."
  (let ((project (project-current)))
    (when project
      (propertize
       (concat " ["
               (file-name-nondirectory
                (directory-file-name (project-root project)))
               "]")
       'face 'font-lock-keyword-face))))
(add-to-list 'mode-line-misc-info '(:eval (prosecco-mode-line)) t)


(defun prosecco-switch-project (&optional project-path)
  "Switch to the most recently used buffer in the target project.
If PROJECT-PATH is not provided, uses `project-current-directory-override'
if set (when called via `project-switch-project'), otherwise prompts.

Falls back through: recent buffer → recent file → projectfind-file."
  (interactive
   (list (unless project-current-directory-override
           (project-prompt-project-dir))))
  (setq project-path (or project-current-directory-override
                         project-path))
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
          ;; No recent buffers or files - fallback to finding a file
          (let ((default-directory expanded-project-path))
            (project-find-file))))))))


(defun prosecco-kill-project (arg)
  "Kill the buffers belonging to the current project. Only the buffers that match a condition in
`project-kill-buffer-conditions' will be killed. With the prefix argument, kill the buffers belonging
to all other projects instead, using the same conditions."
  (interactive "P")
  (if arg
      (if-let ((current-proj (project-current)))
          (let* ((current-root (project-root current-proj))
                 (other-project-bufs
                  (seq-filter
                   (lambda (buf)
                     (when-let ((buf-proj (with-current-buffer buf
                                            (project-current))))
                       ;; Buffer belongs to a different project
                       (and (not (equal (project-root buf-proj) current-root))
                            ;; And it matches the kill conditions
                            (project--buffer-check buf project-kill-buffer-conditions))))
                   (buffer-list))))
            (if other-project-bufs
                (when (yes-or-no-p (format "Kill %d buffers from other projects? "
                                           (length other-project-bufs)))
                  (mapc #'kill-buffer other-project-bufs)
                  (message "Killed %d buffers from other projects" (length other-project-bufs)))
              (message "No buffers from other projects to kill")))
        (message "Not in a project"))
    (project-kill-buffers)))


(defun prosecco-kill-buffer (arg)
  "Kill buffer (with completion) and switch to most recent buffer in same project.
With prefix argument, kill all other project buffers instead.
If no project buffers remain, invoke `project-find-file'."
  (interactive "P")
  (let* ((proj (project-current)))
    (if (not proj)
        ;; Not in a project - simple kill
        (kill-buffer (if arg
                        (current-buffer)
                      (get-buffer (read-buffer "Kill buffer: " 
                                              (current-buffer) 
                                              t))))
      ;; In a project - compute filtered buffers ONCE at the start
      (let* ((project-buffers (project-buffers proj))
             (filtered-project-buffers
              (seq-filter (lambda (buf)
                            (project--buffer-check buf project-kill-buffer-conditions))
                          project-buffers))
             (buffer-to-kill
              (if arg
                  (current-buffer)
                ;; Use the already-filtered list for completion
                (get-buffer
                 (completing-read "Kill buffer: "
                                 (mapcar #'buffer-name filtered-project-buffers)
                                 nil t nil nil
                                 (buffer-name (current-buffer)))))))

        (if arg
            ;; Prefix arg: kill all other project buffers
            (let ((other-project-buffers
                   (seq-filter (lambda (buf)
                                (not (eq buf buffer-to-kill)))
                              filtered-project-buffers)))
              (when (yes-or-no-p (format "Kill %d other project buffers? "
                                        (length other-project-buffers)))
                (mapc #'kill-buffer other-project-buffers)))

          ;; No prefix arg: find a target buffer to switch to, then kill the current buffer
          (let* ((remaining-buffers
                  (seq-filter (lambda (buf)
                                (and (project--buffer-check buf project-kill-buffer-conditions)
                                     (buffer-file-name buf)
                                     (not (eq buf buffer-to-kill))))
                              (project-buffers proj)))
                 (target-buffer (car remaining-buffers)))

            (if target-buffer
                (progn (switch-to-buffer target-buffer)
                       (kill-buffer buffer-to-kill))
              ;; No buffers left in project
              (let ((project-root (project-root proj)))
                (project-find-file)
                (kill-buffer buffer-to-kill)))))))))


(defun prosecco-mode-line ()
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


;;; prosecco.el ends here
