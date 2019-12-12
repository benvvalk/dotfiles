;;----------------------------------------
;; basic UI settings
;;----------------------------------------

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;;----------------------------------------
;; package.el
;;----------------------------------------

(require 'package)

;; Don't auto-load installed packages
;; after init.el finishes. Let `use-package`
;; handle loading of packages instead (see
;; below).
(setq package-enable-at-startup nil)

;; Uncomment the following line to work around
;; package signing problems

;;(setq package-check-signature nil)

(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;;----------------------------------------
;; use-package
;;----------------------------------------

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;----------------------------------------
;; ivy/counsel/swiper
;;
;; Note: ivy, counsel, and swiper are
;; all included in the `ivy` package.
;;----------------------------------------

(use-package ivy
  :config (ivy-mode 1))

(use-package counsel
  :requires ivy)

(use-package swiper
  :requires ivy)
