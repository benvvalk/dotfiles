;;----------------------------------------
;; basic UI settings
;;----------------------------------------

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;;----------------------------------------
;; fonts
;;----------------------------------------

;; Set the default font size.
;;
;; Height units are 1/10 pt. For example,
;; using `:height 140` sets font size to 14 pt.

(set-face-attribute 'default nil :height 140)

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
;; evil
;;----------------------------------------

(use-package evil
  :config (evil-mode 1))

(setq benv/evil-leader-key "SPC")

;;----------------------------------------
;; general.el
;;----------------------------------------

(use-package general)

;; Unbind "SPC" so I can use it as a prefix
;; key, without getting "Key sequence starts
;; with non-prefix key" error.

(general-define-key
 :states '(motion normal visual operator)
 :prefix benv/evil-leader-key
 "" nil)

;;----------------------------------------
;; ivy/counsel/swiper
;;
;; Note: ivy, counsel, and swiper are
;; all included in the `ivy` package.
;;----------------------------------------

(use-package ivy
  :config (ivy-mode 1)
  :general
  ('motion
   :prefix benv/evil-leader-key
   "b b" 'ivy-switch-buffer
   "b d" 'kill-this-buffer))

(use-package counsel
  :requires ivy
  :general
  ('motion
   "M-x" 'counsel-M-x)
  ('motion
   :prefix benv/evil-leader-key
   "f f" 'counsel-find-file))

(use-package swiper
  :requires ivy)

;;----------------------------------------
;; magit
;;----------------------------------------

(use-package magit
  :general
  ('motion
   :prefix benv/evil-leader-key
   "g s" 'magit-status))

;;----------------------------------------
;; projectile
;;----------------------------------------

(use-package projectile)

(use-package counsel-projectile
  :general
  ('motion
   :prefix benv/evil-leader-key
   "p p" 'counsel-projectile))

;;----------------------------------------
;; basic keybindings
;;----------------------------------------

(general-def 'motion
  :prefix benv/evil-leader-key
  "f s" 'save-buffer
  "q q" 'save-buffers-kill-terminal
  "w m" 'delete-other-windows
  "w s" 'split-window-below
  "w v" 'split-window-right)

;;----------------------------------------
;; themes
;;----------------------------------------

(load-theme 'leuven)