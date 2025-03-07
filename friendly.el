
(setq mxns/mouse 1)

(defun mxns/toggle-mouse ()
  (interactive)
  (unless (display-graphic-p)
    (if mxns/mouse
	(progn
	  (xterm-mouse-mode 1)
	  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
	  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
	  (global-set-key (kbd "<mouse-6>") (lambda () (interactive) (scroll-left -1)))
	  (global-set-key (kbd "<mouse-7>") (lambda () (interactive) (scroll-left 1)))
	  (setq mxns/mouse nil))
      (progn
	(xterm-mouse-mode nil)
	(global-unset-key (kbd "<mouse-4>"))
	(global-unset-key (kbd "<mouse-5>"))
	(global-unset-key (kbd "<mouse-6>"))
	(global-unset-key (kbd "<mouse-7>"))
	(setq mxns/mouse 1)))))




