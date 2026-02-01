;;; prosecco.el --- minimal overlay on the `project' package -*- lexical-binding: t; -*-

;; Author: Your Name <your@email.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, project
;; URL: https://github.com/username/prosecco

;;; Commentary:

;; Prosecco provides project-aware buffer management commands that integrate
;; with Emacs' built-in `project' package.  It offers smarter buffer switching,
;; killing, and window management that prefers buffers from the current project.
;;
;; Features:
;; - `prosecco-switch-to-buffer': Switch to project buffers or orphan buffers
;; - `prosecco-kill-buffer': Kill buffer with project-aware completion
;; - `prosecco-bury-buffer': Bury buffer, preferring project buffers next
;; - `prosecco-quit-window': Quit window with project-aware buffer selection
;; - `prosecco-switch-project': Switch to most recent buffer in a project
;; - `prosecco-kill-project': Kill project buffers (with prefix: other projects)
;; - Mode-line indicator showing current project name
;;
;; Enable `prosecco-mode' to remap standard buffer commands to their
;; prosecco equivalents.

;;; Code:

(require 'project)

(defvar recentf-list)
(defvar project-current-directory-override)
(defvar project-kill-buffer-conditions)
(defvar project-ignore-buffer-conditions)

(defgroup prosecco nil
  "Project-aware buffer management."
  :group 'project
  :prefix "prosecco-")

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

;;;###autoload
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

;;;###autoload
(defun prosecco-kill-project (arg)
  "Kill the buffers belonging to the current project.
Only the buffers that match a condition in
`project-kill-buffer-conditions' will be killed.  With the prefix
argument ARG, kill the buffers belonging to all other projects
instead, using the same conditions."
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

;;;###autoload
(defun prosecco-kill-buffer (arg)
  "Kill buffer (with completion) and switch to most recent buffer in same project.
With prefix argument ARG, kill all other project buffers instead.
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
              (let ((default-directory (project-root proj)))
                (project-find-file)
                (kill-buffer buffer-to-kill)))))))))

;;;###autoload
(defun prosecco-switch-to-buffer (buffer-or-name &optional _arg)
  "Switch to a buffer from current project or one not belonging to any project.
If not in a project, falls back to standard `switch-to-buffer'.
With prefix argument ARG, use standard `switch-to-buffer' unconditionally.
BUFFER-OR-NAME is the buffer to switch to."
  (interactive
   (if current-prefix-arg
       (list (read-buffer "Switch to buffer: " (other-buffer (current-buffer))) t)
     (list (prosecco--read-project-or-orphan-buffer) nil)))
  (switch-to-buffer buffer-or-name))


(defun prosecco--read-project-or-orphan-buffer ()
  "Read a buffer belonging to current project or no project at all.
If not in a project, falls back to standard `read-buffer'."
  (let ((pr (project-current)))
    (if (not pr)
        (read-buffer "Switch to buffer: " (other-buffer (current-buffer)) nil)
      (let* ((current-buffer (current-buffer))
             (other-buffer (other-buffer current-buffer))
             (other-name (buffer-name other-buffer))
             (project-bufs (project-buffers pr))
             (predicate
              (lambda (buffer)
                ;; BUFFER is (BUF-NAME . BUF-OBJ)
                (let ((buf (cdr buffer)))
                  (and (not (project--buffer-check buf project-ignore-buffer-conditions))
                       (or (memq buf project-bufs)
                           ;; Buffer belongs to no project
                           (null (with-current-buffer buf
                                   (project-current nil))))))))
             (buffer (read-buffer
                      "Switch to buffer: "
                      (when (funcall predicate (cons other-name other-buffer))
                        other-name)
                      nil
                      predicate)))
        (if (or (get-buffer buffer)
                (file-in-directory-p default-directory (project-root pr)))
            buffer
          (let ((default-directory (project-root pr)))
            (get-buffer-create buffer)))))))


(defun prosecco--next-project-or-orphan-buffer (exclude-buffer project)
  "Find next buffer to show, excluding EXCLUDE-BUFFER.
Prefers PROJECT buffers, then orphan buffers (no project).
Returns nil if no suitable buffer found."
  (let* ((project-bufs (project-buffers project))
         (valid-buf-p
          (lambda (buf)
            (and (not (eq buf exclude-buffer))
                 (not (project--buffer-check buf project-ignore-buffer-conditions)))))
         (next-project-buf
          (seq-find (lambda (buf)
                      (and (funcall valid-buf-p buf)
                           (memq buf project-bufs)))
                    (buffer-list)))
         (next-orphan-buf
          (seq-find (lambda (buf)
                      (and (funcall valid-buf-p buf)
                           (null (with-current-buffer buf (project-current nil)))))
                    (buffer-list))))
    (or next-project-buf next-orphan-buf)))

;;;###autoload
(defun prosecco-bury-buffer ()
  "Bury current buffer and switch to next project buffer, or orphan if none.
If not in a project, falls back to standard `bury-buffer'.
Project buffers are preferred; orphan buffers (those not belonging to any
project) are used as fallback.  Both respect `project-ignore-buffer-conditions'."
  (interactive)
  (let ((pr (project-current)))
    (if (not pr)
        (bury-buffer)
      (let* ((current (current-buffer))
             (next-buf (prosecco--next-project-or-orphan-buffer current pr)))
        (if next-buf
            (progn
              (switch-to-buffer next-buf)
              (bury-buffer current))
          (bury-buffer))))))

;;;###autoload
(defun prosecco-quit-window (&optional kill window)
  "Quit WINDOW, using prosecco buffer selection when in a project.
WINDOW defaults to the selected window.  With prefix argument KILL,
kill the buffer instead of burying it.

When in a project, prefers showing project buffers, then orphan buffers.
When not in a project, falls back to standard `quit-window'.
Dedicated windows are deleted when possible, preserving standard behavior."
  (interactive "P")
  (setq window (or window (selected-window)))
  (let* ((buffer (window-buffer window))
         (pr (with-current-buffer buffer (project-current))))
    (if (not pr)
        ;; Not in a project - standard behavior
        (quit-window kill window)
      ;; In a project - custom buffer selection
      (with-current-buffer buffer
        (run-hooks 'quit-window-hook))
      (let ((dedicated (window-dedicated-p window)))
        (cond
         ;; Try to delete dedicated windows (not side windows)
         ((and dedicated (not (eq dedicated 'side))
               (window--delete window 'dedicated kill))
          ;; Window was deleted, handle buffer
          (if kill
              (kill-buffer buffer)
            (bury-buffer buffer)))
         ;; Window survives - use our buffer selection
         (t
          (let ((next-buf (prosecco--next-project-or-orphan-buffer buffer pr)))
            (if next-buf
                (progn
                  (set-window-buffer window next-buf)
                  ;; Clear quit-restore since we're taking over
                  (set-window-parameter window 'quit-restore nil)
                  ;; Restore side dedication if needed
                  (when (eq dedicated 'side)
                    (set-window-dedicated-p window 'side))
                  (if kill
                      (kill-buffer buffer)
                    (bury-buffer buffer)))
              ;; No suitable buffer - fall back to standard
              (quit-restore-window window (if kill 'kill 'bury))))))))))

;;;###autoload
(define-minor-mode prosecco-mode
  "Minor mode for project-aware buffer management.
Remaps standard buffer commands to prosecco equivalents that prefer
project buffers, with orphan buffers as fallback.

When enabled, adds a mode-line indicator showing the current project name."
  :global t
  :group 'prosecco
  :keymap
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-x b" #'prosecco-switch-to-buffer)
    (keymap-set map "C-x k" #'prosecco-kill-buffer)
    (keymap-set map "<remap> <quit-window>" #'prosecco-quit-window)
    map)
  (if prosecco-mode
      (add-to-list 'mode-line-misc-info '(:eval (prosecco-mode-line)) t)
    (setq mode-line-misc-info
          (delete '(:eval (prosecco-mode-line)) mode-line-misc-info))))

(provide 'prosecco)
;;; prosecco.el ends here
