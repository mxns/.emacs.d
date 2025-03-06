;;;; Mouse scrolling in terminal emacs
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  (global-set-key (kbd "<mouse-6>") (lambda () (interactive) (scroll-left -1)))
  (global-set-key (kbd "<mouse-7>") (lambda () (interactive) (scroll-left 1))))
