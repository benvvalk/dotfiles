;;----------------------------------------
;; basic UI settings
;;----------------------------------------

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; inhibit default Emacs startup message
;; with Emacs tutorial, GNU project info, etc.

(setq inhibit-startup-message t)

;----------------------------------------
;; fonts
;;----------------------------------------

;; Set the default font size.
;;
;; Height units are 1/10 pt. For example,
;; using `:height 140` sets font size to 14 pt.

(set-face-attribute 'default nil :height 140)

;----------------------------------------
;; indentation
;;----------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;----------------------------------------
;; package.el
;;----------------------------------------

(require 'package)

;; Don't auto-load all locally-installed packages
;; after init.el finishes. Let `use-package`
;; perform on-demand loading of packages instead
;; (i.e. "autoloading").
(setq package-enable-at-startup nil)

;; Uncomment the following line to work around
;; package signing problems

;;(setq package-check-signature nil)

(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
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

(setq benv/evil-leader-key "SPC")
(setq benv/major-mode-leader-key ",")

(use-package evil
  :config
  (evil-mode 1)
  (setq evil-move-cursor-back nil))

;;----------------------------------------
;; general.el
;;----------------------------------------

(use-package general)

;; Unbind "," and "SPC" so I can use them as prefix
;; keys, without getting "Key sequence starts
;; with non-prefix key" error.

(general-define-key
 :states '(motion normal visual operator)
 benv/evil-leader-key nil
 benv/major-mode-leader-key nil)

;;----------------------------------------
;; basic keybindings
;;----------------------------------------

(general-def 'motion
  :prefix benv/evil-leader-key
  "f s" 'save-buffer
  "q q" 'save-buffers-kill-terminal
  "t l" 'visual-line-mode
  "t w" 'whitespace-mode
  "u"   'universal-argument
  "w m" 'delete-other-windows
  "w s" 'split-window-below
  "w v" 'split-window-right)

;; Restore standard vim mapping
;; for C-u -> PageUp. In emacs, C-u
;; is normally mapped to `universal-argument`
;; I use SPC-u for that instead (see above
;; mapping).

(general-def 'motion
  "C-u" 'evil-scroll-up)

;;----------------------------------------
;; ivy/counsel/swiper
;;
;; Note: ivy, counsel, and swiper are
;; all included in the `ivy` package.
;;----------------------------------------

(use-package ivy
  :defer nil
  :config (ivy-mode 1)
  :general
  ('motion
   :prefix benv/evil-leader-key
   "b b" 'ivy-switch-buffer
   "b d" 'kill-this-buffer))

(use-package counsel
  :general
  ('motion
   "M-x" 'counsel-M-x)
  ('motion
   :prefix benv/evil-leader-key
   "f f" 'counsel-find-file
   "f r" 'counsel-recentf))

(use-package swiper
  :general
  ('motion
   :prefix benv/evil-leader-key
   "s s" 'swiper))

;;----------------------------------------
;; org-mode
;;----------------------------------------

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :general
  ('motion
   "TAB" 'org-cycle)
  ('motion
   :prefix benv/major-mode-leader-key
   "h i" 'org-insert-heading-after-current
   "I"   'org-clock-in
   "O"   'org-clock-out))

;;----------------------------------------
;; magit
;;----------------------------------------

(use-package magit
  :general
    ;; Unset any bindings for my evil leader
    ;; key (currently "SPC") in magit, so that
    ;; my evil leader key behaves normally
    ;; in magit windows.
    (:keymaps '(magit-status-mode-map
                magit-log-mode-map
                magit-diff-mode-map)
                "SPC" nil)
    ('motion
    :prefix benv/evil-leader-key
    "g s" 'magit-status)
  :config
    ;; display magit status buffer in currently
    ;; selected window (not the "other" window)
    (setq magit-display-buffer-function
            'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after (evil magit))

;;----------------------------------------
;; projectile
;;----------------------------------------

(use-package projectile
  :config (projectile-mode 1))

(use-package counsel-projectile
  :general
  ('motion
   :prefix benv/evil-leader-key
   "p p" 'counsel-projectile-switch-project
   "p f" 'counsel-projectile-find-file))

;;----------------------------------------
;; winum
;;----------------------------------------

(use-package winum
  :defer nil
  :config
  (winum-mode)
  :general
  ('motion
   :prefix benv/evil-leader-key
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4))

;;----------------------------------------
;; buffer switching
;;----------------------------------------

;; See https://www.emacswiki.org/emacs/SwitchingBuffers#toc5
;; for further info.

(defun benv/switch-to-previous-buffer ()
  "Switch to most recent non-visible buffer."
  (interactive)
  (switch-to-buffer
   (other-buffer (current-buffer))))

(general-def 'motion
  :prefix benv/evil-leader-key
  "TAB" 'benv/switch-to-previous-buffer)

;;----------------------------------------
;; javascript
;;----------------------------------------

;; Unity-specific extensions for javascript
;; source files

(add-to-list 'auto-mode-alist '("\\.jspre\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jslib\\'" . js-mode))

;;----------------------------------------
;; themes
;;----------------------------------------

(load-theme 'leuven)
