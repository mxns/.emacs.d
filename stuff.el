;;; stuff.el --- mxns config

;;; Commentary:
;;; stuff about stuff

;;; Code:

(require 'package)
(require 'use-package)
(require 'scroll-lock)

(defun fwd-scroll (amount)
  "Forward scroll AMOUNT lines."
  (let ((adjusted-amount
	 (min
	  amount
	  (- (line-number-at-pos (point-max)) (line-number-at-pos)))))
    (cond
     ((<= adjusted-amount 0)
      (message "End of buffer"))
     ((< adjusted-amount amount)
      (forward-line adjusted-amount))
     (t
      (scroll-lock-next-line amount)))))

(defun bwd-scroll (amount)
  "Backward scroll AMOUNT lines."
  (let ((adjusted-amount
	 (min
	  amount
	  (- (line-number-at-pos) 1))))
    (cond
     ((<= adjusted-amount 0)
      (message "Beginning of buffer"))
     ((= (line-number-at-pos (window-start)) 1)
      (forward-line (- 0 amount)))
     (t
      (scroll-lock-previous-line adjusted-amount)))))

(global-set-key (kbd "M-n") (lambda (amount)
			      (interactive "p")
			      (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
				(progn
				  (setq scroll-preserve-screen-position 1)
				  (scroll-lock-next-line amount)
				  (setq scroll-preserve-screen-position current-setting)))))

(global-set-key (kbd "M-p") (lambda (amount)
			      (interactive "p")
			      (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
				(progn
				  (setq scroll-preserve-screen-position 1)
				  (scroll-lock-previous-line amount)
				  (setq scroll-preserve-screen-position current-setting)))))

(global-set-key (kbd "M-N") (lambda (amount)
			      (interactive "p")
			      (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
				(progn
				  (setq scroll-preserve-screen-position nil)
				  (scroll-lock-next-line amount)
				  (setq scroll-preserve-screen-position current-setting)))))

(global-set-key (kbd "M-P") (lambda (amount)
			      (interactive "p")
			      (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
				(progn
				  (setq scroll-preserve-screen-position nil)
				  (scroll-lock-previous-line amount)
				  (setq scroll-preserve-screen-position current-setting)))))

(global-set-key (kbd "C-v")
                (lambda (amount)
                  (interactive "p")
                  (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
                    (condition-case err
                        (progn
                          (setq scroll-preserve-screen-position 1)
                          (scroll-up-command)
                          (setq scroll-preserve-screen-position current-setting))
                      (error
                       (setq scroll-preserve-screen-position current-setting)
                       (signal (car err) (cdr err)))))))

(global-set-key (kbd "M-v")
                (lambda (amount)
                  (interactive "p")
                  (let ((current-setting (if scroll-preserve-screen-position 1 nil)))
                    (condition-case err
                        (progn
                          (setq scroll-preserve-screen-position 1)
                          (scroll-down-command)
                          (setq scroll-preserve-screen-position current-setting))
                      (error
                       (setq scroll-preserve-screen-position current-setting)
                       (signal (car err) (cdr err)))))))

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(setq confirm-kill-emacs 'y-or-n-p)
(setq scroll-preserve-screen-position 1)
(setq ns-right-option-modifier 'option)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))
(setq use-package-always-ensure t)
(setq read-file-name-completion-ignore-case t)
;;;(setq js-indent-level 2)
(setq treesit-language-source-alist
      '((yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
	(json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
	(ruby . ("https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1"))
	(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
	(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
	(java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.23.5"))))
(treesit-install-language-grammar 'yaml)
(treesit-install-language-grammar 'json)
(treesit-install-language-grammar 'java)
(treesit-install-language-grammar 'tsx)
(treesit-install-language-grammar 'typescript)

(use-package undo-tree
  :hook (prog-mode . undo-tree-mode))
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :config (vertico-mode))
(use-package savehist
  :init (savehist-mode))
(use-package rg
  :ensure-system-package rg
  :hook (grep-mode . (lambda () (setq truncate-lines t))))
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode))
(use-package projectile
  :init (setq projectile-project-search-path '("~/devel/fortifiedid/"))
  :config (projectile-mode)
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :config (setq projectile-project-search-path '("~/devel/fortifiedid/"))
  :config (projectile-discover-projects-in-search-path))
(use-package which-key
  :ensure t
  :init (which-key-mode)) ;;; Try using C-c C-h instead of which-key
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(use-package magit)
(use-package treemacs)
(use-package company)
(use-package undo-tree)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c t") 'treemacs)

(add-hook 'json-ts-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-v") 'hs-toggle-hiding)))

(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(add-hook 'java-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'java-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'java-ts-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'java-ts-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'java-ts-mode-hook #'hs-minor-mode)
(add-hook 'java-ts-mode-hook #'electric-pair-mode)

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-hook 'json-ts-mode-hook #'hs-minor-mode)
(add-hook 'json-ts-mode-hook #'electric-pair-mode)
(add-hook 'json-ts-mode-hook (lambda () (setq indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;; (add-hook 'typescript-ts-mode-hook #'hs-minor-mode)
;; (add-hook 'typescript-ts-mode-hook #'electric-pair-mode)
;; (add-hook 'typescript-ts-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-hook 'tsx-ts-mode-hook #'hs-minor-mode)
;; (add-hook 'tsx-ts-mode-hook #'electric-pair-mode)
;; (add-hook 'tsx-ts-mode-hook (lambda () (setq indent-tabs-mode nil)))

(use-package typescript-ts-mode
  :mode ("\\.tsx\\'" . typescript-ts-mode)
  :hook (typescript-ts-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package tsx-ts-mode
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :hook (tsx-ts-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :hook (typescript-ts-mode . lsp-mode)
  :hook (tsx-ts-mode . lsp-mode)
  :config (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui)

;;; https://repo.eclipse.org/content/repositories/jdtls-releases/org/eclipse/jdt/ls/org.eclipse.jdt.ls.core/
(use-package lsp-java
  :init
  (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.44.0/jdt-language-server-1.44.0-20250122155241.tar.gz")
  (setq lsp-java-java-path "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/bin/java")
  (setenv "JAVA_HOME"  "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/")
  :config (add-hook 'java-ts-mode-hook 'lsp))

;;; stuff.el ends here


