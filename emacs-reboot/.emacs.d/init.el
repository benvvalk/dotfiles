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
;; general.el
;;----------------------------------------

(use-package general
  :load-path "packages/general")

;;----------------------------------------
;; ivy/counsel/swiper
;;----------------------------------------

;; Note: The GitHub repo `abo-abo/swiper`
;; provides the `ivy`, `counsel`, and
;; `swiper` packages.

(use-package ivy
  :load-path "packages/swiper"
  :config
  (ivy-mode 1))

(use-package counsel
  :requires (ivy)
  :general
  ('normal
   :prefix "SPC"
   "f f" 'counsel-find-file)
  :load-path "packages/swiper")

(use-package swiper
  :requires (ivy)
  :load-path "packages/swiper")

;;----------------------------------------
;; basic keybindings
;;----------------------------------------

(general-def 'normal
  :prefix "SPC"
  "f s" 'save-buffer
  "w v" 'split-window-right
  "w s" 'split-window-below)

