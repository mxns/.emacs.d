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
  (interactive "P")
  (or arg (setq arg 1))
  (mxns/scroll-lock-update-goal-column)
  (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
    (condition-case err
        (progn
          (setq scroll-preserve-screen-position 1)
          (if (not (pos-visible-in-window-p (point-max)))
	      (scroll-up arg))
          (setq scroll-preserve-screen-position current-setting))
      (error
       (setq scroll-preserve-screen-position current-setting)
       (signal (car err) (cdr err)))))
  (scroll-lock-move-to-column scroll-lock-temporary-goal-column))

(defun mxns/scroll-lock-previous-line (&optional arg)
  "Scroll up ARG lines keeping point fixed."
  (interactive "P")
  (or arg (setq arg 1))
  (mxns/scroll-lock-update-goal-column)
  (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
    (condition-case err
        (progn
          (setq scroll-preserve-screen-position 1)
          (scroll-down arg)
          (setq scroll-preserve-screen-position current-setting))
      (error
       (setq scroll-preserve-screen-position current-setting)
       (signal (car err) (cdr err)))))
  (scroll-lock-move-to-column scroll-lock-temporary-goal-column))


;; (global-set-key (kbd "M-n") #'mxns/scroll-lock-next-line)
;; (global-set-key (kbd "M-p") #'mxns/scroll-lock-previous-line)
;; (global-set-key (kbd "M-N") #'scroll-lock-next-line)
;; (global-set-key (kbd "M-P") #'scroll-lock-previous-line)
(global-set-key (kbd "C-v") (mxns/do-while-preserving-screen-position #'scroll-up-command))
(global-set-key (kbd "M-v") (mxns/do-while-preserving-screen-position #'scroll-down-command))

(defun my-digit-argument-wrapper (digit)
  "Wrapper for digit-argument to be used in transient maps."
  (interactive "P")
  (universal-argument)
  (universal-argument-more digit))

(defvar mxns/movement-transient-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'mxns/scroll-lock-next-line)
    (define-key map (kbd "p") 'mxns/scroll-lock-previous-line)
    (define-key map (kbd "N") 'scroll-lock-next-line)
    (define-key map (kbd "P") 'scroll-lock-previous-line)
    (define-key map (kbd "f") 'forward-char)
    (define-key map (kbd "b") 'backward-char)
    (define-key map (kbd "C-f") 'forward-char)
    (define-key map (kbd "C-b") 'backward-char)
    (define-key map (kbd "C-n") 'next-line)
    (define-key map (kbd "C-p") 'previous-line)
    (define-key map (kbd "M-f") 'forward-word)
    (define-key map (kbd "M-b") 'backward-word)
    (define-key map (kbd "1") #'(lambda () (interactive) (my-digit-argument-wrapper 1)))
    (define-key map (kbd "2") #'(lambda () (interactive) (my-digit-argument-wrapper 2)))
    (define-key map (kbd "3") #'(lambda () (interactive) (my-digit-argument-wrapper 3)))
    (define-key map (kbd "4") #'(lambda () (interactive) (my-digit-argument-wrapper 4)))
    (define-key map (kbd "5") #'(lambda () (interactive) (my-digit-argument-wrapper 5)))
    (define-key map (kbd "6") #'(lambda () (interactive) (my-digit-argument-wrapper 6)))
    (define-key map (kbd "7") #'(lambda () (interactive) (my-digit-argument-wrapper 7)))
    (define-key map (kbd "8") #'(lambda () (interactive) (my-digit-argument-wrapper 8)))
    (define-key map (kbd "9") #'(lambda () (interactive) (my-digit-argument-wrapper 9)))
    (define-key map (kbd "0") #'(lambda () (interactive) (my-digit-argument-wrapper 0)))
    map)
  "Transient map for word, character, and line movement.")

(defun mxns/activate-movement-map ()
  "Activate movement map."
  (interactive)
  (mxns/scroll-lock-update-goal-column)
  (set-transient-map mxns/movement-transient-map t))

(global-set-key (kbd "C-c n") #'mxns/activate-movement-map)

;;; scrolling.el ends here
