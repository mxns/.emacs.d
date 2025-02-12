;;; stuff.el --- mxns config

;;; Commentary:
;;; stuff about stuff

;;; Code:

(require 'package)
(require 'use-package)
(require 'scroll-lock)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(defun my-scroll-lock-next-line (scroll-amount)
  "Enable `scroll-lock-mode`, scroll down SCROLL-AMOUNT lines, restore `scroll-lock-mode` to original value."
  (interactive "p")
  (let ((scroll-locked (if scroll-lock-mode 1 -1)))
    (scroll-lock-mode 1)
    (condition-case err
	(scroll-up-line scroll-amount)
      (error
       (progn
	 (scroll-lock-mode scroll-locked)
	 (signal (car err) (cdr err))))) ;;; (car err) is the error type (e.g., wrong-type-argument).
    (scroll-lock-mode scroll-locked)))

(defun my-scroll-lock-previous-line (scroll-amount)
  "Enable `scroll-lock-mode`, scroll up SCROLL-AMOUNT lines, restore `scroll-lock-mode` to original value."
  (interactive "p")
  (let ((scroll-locked (if scroll-lock-mode 1 -1)))
    (scroll-lock-mode 1)
    (condition-case err
	(scroll-down-line scroll-amount)
      (error
       (let ((err-type (car err)))
	 (cond
	  ((eq err-type 'beginning-of-buffer)
	   (progn
	     (scroll-lock-mode scroll-locked)
	     (forward-line -1)))
	  (t
	   (signal (car err) (cdr err)))))))
    (scroll-lock-mode scroll-locked)))

(setq confirm-kill-emacs 'y-or-n-p)
(savehist-mode 1)
(setq scroll-preserve-screen-position 1)
(setq ns-right-option-modifier 'option)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

(setq use-package-always-ensure t)

(setq treesit-language-source-alist
      '((yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
	(json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
	(ruby . ("https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1"))
	(java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.23.5"))))
(treesit-install-language-grammar 'yaml)
(treesit-install-language-grammar 'json)
(treesit-install-language-grammar 'java)

(use-package projectile
  :init (projectile-mode))
(setq projectile-project-search-path '("~/devel/fortifiedid/"))
(customize-set-value 'projectile-completion-system 'ido)
(projectile-discover-projects-in-search-path)
(setq read-file-name-completion-ignore-case t)

(use-package which-key
  :ensure t
  :init (which-key-mode)) ;;; Try using C-c C-h instead of which-key

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package magit)

(use-package treemacs)

;;;(setq js-indent-level 2)

(global-set-key (kbd "M-n") 'my-scroll-lock-next-line)
(global-set-key (kbd "M-p") 'my-scroll-lock-previous-line)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c t") 'treemacs)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;; (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
(add-hook 'json-ts-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-v") 'hs-toggle-hiding)))

(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(add-hook 'java-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'java-ts-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'java-ts-mode-hook #'hs-minor-mode)
(add-hook 'java-ts-mode-hook #'electric-pair-mode)

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-hook 'json-ts-mode-hook #'hs-minor-mode)
(add-hook 'json-ts-mode-hook #'electric-pair-mode)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

(setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.44.0/jdt-language-server-1.44.0-20250122155241.tar.gz")
(setq lsp-java-java-path "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/bin/java")
(setenv "JAVA_HOME"  "/Users/mxns/java/zulu23.32.11-ca-jdk23.0.2-macosx_aarch64/zulu-23.jdk/Contents/Home/")
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)))
(use-package company)
(use-package lsp-ui)
(use-package lsp-java
  :config (add-hook 'java-ts-mode-hook 'lsp))

;;; stuff.el ends here


