;;; -*- lexical-binding: t -*-

(setq mxns/mouse 1)

(defun mxns/toggle-mouse ()
  (interactive)
  (unless (display-graphic-p)
    (if mxns/mouse
	(progn
	  (message "Enable mouse scroll")
	  (xterm-mouse-mode 1)
	  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
	  (global-set-key (kbd "<wheel-up>") 'scroll-down-line)
	  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
	  (global-set-key (kbd "<wheel-down>") 'scroll-up-line)
	  (global-set-key (kbd "<mouse-6>") (lambda () (interactive) (scroll-left -1)))
	  (global-set-key (kbd "<wheel-right>") (lambda () (interactive) (scroll-left -1)))
	  (global-set-key (kbd "<mouse-7>") (lambda () (interactive) (scroll-left 1)))
	  (global-set-key (kbd "<wheel-left>") (lambda () (interactive) (scroll-left 1)))
	  (setq mxns/mouse nil))
      (progn
	(message "Disable mouse scroll")
	(xterm-mouse-mode nil)
	(global-unset-key (kbd "<mouse-4>"))
	(global-unset-key (kbd "<mouse-5>"))
	(global-unset-key (kbd "<mouse-6>"))
	(global-unset-key (kbd "<mouse-7>"))
	(global-unset-key (kbd "<wheel-up>"))
	(global-unset-key (kbd "<wheel-down>"))
	(global-unset-key (kbd "<wheel-left>"))
	(global-unset-key (kbd "<wheel-right>"))
	(setq mxns/mouse 1)))))




