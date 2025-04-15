;;; -*- lexical-binding: t -*-

;;; stuff.el --- mxns config

;;; Commentary:
;;; stuff about stuff

;;; Code:

(require 'scroll-lock)

(defun mxns/do-while-preserving-screen-position (action &optional use-arg)
  "Return a function that perform ACTION while preserving screen position.
If USE-ARG is provided and ARG is present, ACTION is called with ARG.
Otherwise, ACTION is called without arguments."
  (lambda (&optional arg)
    (interactive "p")
    (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
      (condition-case err
          (progn
	    (if (> (current-column) (or scroll-lock-temporary-goal-column 0)) (scroll-lock-update-goal-column))
            (setq scroll-preserve-screen-position 1)
            (let ((result (if (and arg use-arg)
			      (funcall action arg)
                            (funcall action))))
	      (setq scroll-preserve-screen-position current-setting)
	      result))
        (error
	 (set-goal-column t)
         (setq scroll-preserve-screen-position current-setting)
         (signal (car err) (cdr err)))))))

(defun mxns/do-while-not-preserving-screen-position (action &optional use-arg)
  "Return a function that perform ACTION while not preserving screen position.
If USE-ARG is provided and ARG is present, ACTION is called with ARG.
Otherwise, ACTION is called without arguments."
  (lambda (&optional arg)
    (interactive "p")
    (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
      (condition-case err
          (progn
	    (if (> (current-column) (or goal-column 0)) (set-goal-column nil))
            (setq scroll-preserve-screen-position nil)
            (let ((result (if (and arg use-arg)
			      (funcall action arg)
                            (funcall action))))
	      (setq scroll-preserve-screen-position current-setting)
	      result))
        (error
	 (set-goal-column t)
         (setq scroll-preserve-screen-position current-setting)
         (signal (car err) (cdr err)))))))

(defun mxns/scroll-lock-update-goal-column ()
  "Update `scroll-lock-temporary-goal-column' if necessary."
  (unless (memq last-command '(mxns/scroll-lock-next-line
			       mxns/scroll-lock-previous-line
			       scroll-lock-forward-paragraph
			       scroll-lock-backward-paragraph))
    (setq scroll-lock-temporary-goal-column (current-column))))

(defun mxns/scroll-lock-next-line (&optional arg)
  "Scroll up ARG lines keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (mxns/scroll-lock-update-goal-column)
  (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
    (setq scroll-preserve-screen-position 1)
    (if (pos-visible-in-window-p (point-max))
	(forward-line arg)
      (scroll-up arg))
    (setq scroll-preserve-screen-position current-setting))
  (scroll-lock-move-to-column scroll-lock-temporary-goal-column))

(defun mxns/scroll-lock-previous-line (&optional arg)
  "Scroll up ARG lines keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (mxns/scroll-lock-update-goal-column)
  (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
    (setq scroll-preserve-screen-position 1)
    (condition-case nil
	(scroll-down arg)
      (beginning-of-buffer (forward-line (- arg))))
    (setq scroll-preserve-screen-position current-setting))
  (scroll-lock-move-to-column scroll-lock-temporary-goal-column))


(global-set-key (kbd "M-n") #'mxns/scroll-lock-next-line)
(global-set-key (kbd "M-p") #'mxns/scroll-lock-previous-line)
(global-set-key (kbd "M-N") #'scroll-lock-next-line)
(global-set-key (kbd "M-P") #'scroll-lock-previous-line)
(global-set-key (kbd "C-v") (mxns/do-while-preserving-screen-position #'scroll-up-command))
(global-set-key (kbd "M-v") (mxns/do-while-preserving-screen-position #'scroll-down-command))

(defun mxns/horizontal-scroll-mode ()
  "Activate temporary horizontal scroll mode."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-f") (lambda (&optional arg) (interactive "p") (scroll-left (or arg 1))))
    (define-key map (kbd "M-f") (lambda () (interactive "p")  (scroll-left 4)))
    (define-key map (kbd "C-b") (lambda () (interactive "p") (scroll-left -1)))
    (define-key map (kbd "M-b") (lambda () (interactive "p") (scroll-left -4)))
    (define-key map (kbd "C-p") (mxns/do-while-preserving-screen-position (lambda () (scroll-lock-previous-line))))
    (define-key map (kbd "M-p") (mxns/do-while-preserving-screen-position (lambda () (scroll-lock-previous-line 4))))
    (define-key map (kbd "C-n") (mxns/do-while-preserving-screen-position (lambda () (scroll-lock-next-line))))
    (define-key map (kbd "M-n") (mxns/do-while-preserving-screen-position (lambda () (scroll-lock-next-line))))
    (define-key map (kbd "C-u") t)
    (set-transient-map map t)))


;; Bind the command to "C-c n"
(global-set-key (kbd "C-c C-n") #'mxns/horizontal-scroll-mode)
(global-set-key (kbd "C-c n") #'mxns/horizontal-scroll-mode)

;;; scrolling.el ends here
