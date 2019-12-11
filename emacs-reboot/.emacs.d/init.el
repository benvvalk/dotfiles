;;----------------------------------------
;; UI settings
;;----------------------------------------

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;;----------------------------------------
;; bootstrap use-package
;;----------------------------------------

(add-to-list 'load-path "~/.emacs.d/packages/use-package")
(require 'use-package)

;;----------------------------------------
;; evil (vim emulation)
;;----------------------------------------

(use-package undo-tree
  :load-path "packages/undo-tree")

(use-package goto-chg
  :load-path "packages/goto-chg")

(use-package evil
  :requires (undo-tree goto-chg)
  :load-path "packages/evil"
  :config
  (evil-mode 1))

;;----------------------------------------
;; ivy
;;----------------------------------------

;; Note: The GitHub repo `abo-abo/swiper`
;; is `abo-abo/swiper` includes the
;; `ivy`, `counsel`, and `swiper` packages.

(use-package ivy
  :load-path "packages/swiper"
  :config
  (ivy-mode 1))
