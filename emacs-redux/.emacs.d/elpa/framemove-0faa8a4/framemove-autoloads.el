;;; framemove-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from framemove.el

(autoload 'fm-down-frame "framemove" nil t)
(autoload 'fm-up-frame "framemove" nil t)
(autoload 'fm-left-frame "framemove" nil t)
(autoload 'fm-right-frame "framemove" nil t)
(autoload 'framemove-default-keybindings "framemove" "\
Set up keybindings for `framemove'.
Keybindings are of the form MODIFIER-{left,right,up,down}.
Default MODIFIER is 'meta.

(fn &optional MODIFIER)" t)
(register-definition-prefixes "framemove" '("fm-" "framemove-hook-into-windmove"))

;;; End of scraped data

(provide 'framemove-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; framemove-autoloads.el ends here
