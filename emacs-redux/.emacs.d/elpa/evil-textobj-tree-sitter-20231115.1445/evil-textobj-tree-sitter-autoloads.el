;;; evil-textobj-tree-sitter-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from evil-textobj-tree-sitter-core.el

(autoload 'evil-textobj-tree-sitter-get-textobj "evil-textobj-tree-sitter-core" "\
Macro to create a textobj function from `GROUP'.
You can pass in multiple groups as a list and in that case as long as
any one of them is available, it will be picked.

You can optionally pass in a alist mapping `major-mode' to their
respective tree-sitter query in `QUERY' with named captures to use
that instead of the default query list.  If `QUERY' does not contain
current `major-mode', then the default queries are used.
Check the README file in the repo to see how to use it.

Check this url for builtin objects
https://github.com/nvim-treesitter/nvim-treesitter-textobjects#built-in-textobjects

(fn GROUP &optional QUERY)" nil t)
(function-put 'evil-textobj-tree-sitter-get-textobj 'lisp-indent-function 'defun)
(autoload 'evil-textobj-tree-sitter-goto-textobj "evil-textobj-tree-sitter-core" "\
Got to the start/end of the textobj of type `GROUP'.
By default it goes to the start of the textobj, but pass in `END' if
you want to go to the end of the textobj instead.  You can pass in
`PREVIOUS' if you want to search backwards.  Optionally pass in
`QUERY' if you want to define a custom query.

(fn GROUP &optional PREVIOUS END QUERY)")
(register-definition-prefixes "evil-textobj-tree-sitter-core" '("evil-textobj-tree-sitter-"))


;;; Generated autoloads from evil-textobj-tree-sitter-thing-at-point.el

(autoload 'function-at-point "evil-textobj-tree-sitter-thing-at-point" "\
Return the function at point, or nil if none is found.")
(autoload 'loop-at-point "evil-textobj-tree-sitter-thing-at-point" "\
Return the loop at point, or nil if none is found.")
(autoload 'conditional-at-point "evil-textobj-tree-sitter-thing-at-point" "\
Return the conditional at point, or nil if none is found.")
(autoload 'assignment-at-point "evil-textobj-tree-sitter-thing-at-point" "\
Return the assignment at point, or nil if none is found.")
(autoload 'class-at-point "evil-textobj-tree-sitter-thing-at-point" "\
Return the class at point, or nil if none is found.")
(autoload 'comment-at-point "evil-textobj-tree-sitter-thing-at-point" "\
Return the comment at point, or nil if none is found.")
(autoload 'parameter-at-point "evil-textobj-tree-sitter-thing-at-point" "\
Return the parameter at point, or nil if none is found.")
(register-definition-prefixes "evil-textobj-tree-sitter-thing-at-point" '("evil-textobj-tree-sitter--thing-at-point-bounds"))

;;; End of scraped data

(provide 'evil-textobj-tree-sitter-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; evil-textobj-tree-sitter-autoloads.el ends here
