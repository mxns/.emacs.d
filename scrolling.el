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
    (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
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
(global-set-key (kbd "M-P") (do-while-not-preserving-screen-position #'scroll-lock-next-line))
(global-set-key (kbd "M-N") (do-while-not-preserving-screen-position #'scroll-lock-previous-line))
(global-set-key (kbd "C-v") (do-while-preserving-screen-position #'scroll-up-command))
(global-set-key (kbd "M-v") (do-while-preserving-screen-position #'scroll-down-command))

(defun my/horizontal-scroll-mode ()
  "Activate temporary horizontal scroll mode."
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<right>") (lambda () (interactive) (scroll-left 1)))
     (define-key map (kbd "C-<right>") (lambda () (interactive) (scroll-left 4)))
     (define-key map (kbd "<left>") (lambda () (interactive) (scroll-left -1)))
     (define-key map (kbd "C-<left>") (lambda () (interactive) (scroll-left -4)))
     (define-key map (kbd "<up>") (do-while-preserving-screen-position #'scroll-lock-previous-line))
     (define-key map (kbd "<down>") (do-while-preserving-screen-position #'scroll-lock-next-line))
     map)
   t))

;; Bind the command to "C-c n"
(global-set-key (kbd "C-c <right>") #'my/horizontal-scroll-mode)
(global-set-key (kbd "C-c <left>") #'my/horizontal-scroll-mode)

;;; scrolling.el ends here
