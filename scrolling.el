;;; -*- lexical-binding: t -*-

;;; stuff.el --- mxns config

;;; Commentary:
;;; stuff about stuff

;;; Code:

(require 'scroll-lock)

(defun do-while-preserving-screen-position (action &optional use-arg)
  "Return a function that perform ACTION while not preserving screen position.
If USE-ARG is provided and ARG is present, ACTION is called with ARG.
Otherwise, ACTION is called without arguments."
  (lambda (&optional arg)
    (interactive "p")
    (let ((current-setting (if scroll-preserve-screen-position 1 nil))
	  (current-column-position (max
				    (or (current-column) 0)
				    (or (car temporary-goal-column) 0))))
      (condition-case err
          (progn
            (setq scroll-preserve-screen-position 1)
            (let ((result (if (and arg use-arg)
			      (funcall action arg)
                            (funcall action))))
	      (setq scroll-preserve-screen-position current-setting)
	      result))
        (error
         (setq scroll-preserve-screen-position current-setting)
         (signal (car err) (cdr err)))))))

(defun do-while-not-preserving-screen-position (action &optional use-arg)
  "Return a function that perform ACTION while not preserving screen position.
If USE-ARG is provided and ARG is present, ACTION is called with ARG.
Otherwise, ACTION is called without arguments."
  (lambda (&optional arg)
    (interactive "p")
    (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
      (condition-case err
          (progn
            (setq scroll-preserve-screen-position nil)
            (let ((result (if (and arg use-arg)
			      (funcall action arg)
                            (funcall action))))
	      (setq scroll-preserve-screen-position current-setting)
	      result))
        (error
         (setq scroll-preserve-screen-position current-setting)
         (signal (car err) (cdr err)))))))


(global-set-key (kbd "M-n") (do-while-preserving-screen-position #'scroll-lock-next-line t))
(global-set-key (kbd "M-p") (do-while-preserving-screen-position #'scroll-lock-previous-line t))
(global-set-key (kbd "M-N") (do-while-not-preserving-screen-position #'scroll-lock-next-line))
(global-set-key (kbd "M-P") (do-while-not-preserving-screen-position #'scroll-lock-previous-line))
(global-set-key (kbd "C-v") (do-while-preserving-screen-position #'scroll-up-command))
(global-set-key (kbd "M-v") (do-while-preserving-screen-position #'scroll-down-command))

(defun my/horizontal-scroll-mode ()
  "Activate temporary horizontal scroll mode."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-f") (lambda (&optional arg) (interactive "p") (scroll-left (or arg 1))))
    (define-key map (kbd "M-f") (lambda () (interactive "p")  (scroll-left 4)))
    (define-key map (kbd "C-b") (lambda () (interactive "p") (scroll-left -1)))
    (define-key map (kbd "M-b") (lambda () (interactive "p") (scroll-left -4)))
    (define-key map (kbd "C-p") (do-while-preserving-screen-position (lambda () (scroll-lock-previous-line))))
    (define-key map (kbd "M-p") (do-while-preserving-screen-position (lambda () (scroll-lock-previous-line 4))))
    (define-key map (kbd "C-n") (do-while-preserving-screen-position (lambda () (scroll-lock-next-line))))
    (define-key map (kbd "M-n") (do-while-preserving-screen-position (lambda () (scroll-lock-next-line))))
    (define-key map (kbd "C-u") t)
    (set-transient-map map t)))


;; Bind the command to "C-c n"
(global-set-key (kbd "C-c C-n") #'my/horizontal-scroll-mode)
(global-set-key (kbd "C-c n") #'my/horizontal-scroll-mode)

;;; scrolling.el ends here
