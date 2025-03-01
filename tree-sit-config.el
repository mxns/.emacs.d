;;; tree-sit-config.el --- mxns config

;;; Commentary:
;;; tree-sitter configuration and initialization

;;; Code:

;;; tree-sitter
(setq treesit-language-source-alist
      '((markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
	(json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
	(bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3"))
	(ruby . ("https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1"))
	(python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.23.6"))
	(html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.2"))
	(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1"))
	(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
	(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
	(java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.23.5"))))
(treesit-install-language-grammar 'markdown)
(treesit-install-language-grammar 'json)
(treesit-install-language-grammar 'bash)
(treesit-install-language-grammar 'python)
(treesit-install-language-grammar 'html)
(treesit-install-language-grammar 'javascript)
(treesit-install-language-grammar 'tsx)
(treesit-install-language-grammar 'typescript)
(treesit-install-language-grammar 'java)

;;; tree-sit-config.el ends here
