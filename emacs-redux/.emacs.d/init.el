;;----------------------------------------
;; basic UI settings
;;----------------------------------------

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; disable annoying "ding" sound on Windows
;; when hitting top/bottom of buffer

(setq ring-bell-function 'ignore)

;; inhibit default Emacs startup message
;; with Emacs tutorial, GNU project info, etc.

(setq inhibit-startup-message t)

;; Change all confirmation prompts to require
;; only a single character ('y' or 'n'), rather
;; than typing out the entire word ("yes" or "no").
;; I find the full-word prompts quite annoying,
;; even if they are a bit safer.

(defalias 'yes-or-no-p 'y-or-n-p)

;----------------------------------------
;; file backups / auto-revert
;;----------------------------------------

(setq
    backup-directory-alist '(("." . "~/.emacs.d/backups"))
    backup-by-copying t
    delete-old-versions t
    kept-new-versions 50
    kept-old-versions 5
    version-control t)

;; "Auto-revert" means that whenever a file is changed
;; outside of emacs, any buffers visiting that file
;; should be updated (reverted) to match the file on
;; disk.

(global-auto-revert-mode)

;----------------------------------------
;; fonts
;;----------------------------------------

;; Set the default font size.
;;
;; Height units are 1/10 pt. For example,
;; using `:height 140` sets font size to 14 pt.

(set-face-attribute 'default nil :height 130)

;;----------------------------------------
;; indentation
;;----------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;----------------------------------------
;; long lines
;;----------------------------------------

(setq-default
 truncate-lines t
 truncate-partial-width-windows t)

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
;; savehist-mode
;;----------------------------------------

;; save minibuffer command history between sessions
(use-package savehist
  :config
  (savehist-mode))

;;----------------------------------------
;; evil
;;----------------------------------------

(setq benv/evil-leader-key "SPC")
(setq benv/major-mode-leader-key ",")
(setq benv/evil-insert-mode-leader-key "C-SPC")

(use-package evil
  :init
  ;; evil-collection requires the following settings before
  ;; loading evil and evil-collection. (See evil-collection
  ;; README for further details.)
  (setq evil-want-keybinding nil
        evil-want-integration t)
  :config
  (evil-mode 1)
  (setq evil-move-cursor-back nil))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-key-blacklist
        (list benv/evil-leader-key
              benv/major-mode-leader-key
              benv/evil-insert-mode-leader-key))
  (evil-collection-init))

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
;; file management
;;----------------------------------------

;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun benv/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "deleted file %s" filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

;; based on https://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun benv/rename-file-and-buffer ()
  "Renames the current buffer and underlying file."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-filename (read-file-name default-directory)))
        (progn
          (rename-file filename new-filename 1)
          (set-visited-file-name new-filename t t))))))

(defun benv/yank-filename ()
  "Yank absolute file path for current buffer
and echo it in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

;;----------------------------------------
;; basic keybindings
;;----------------------------------------

(general-def
  :states '(motion insert emacs)
  :prefix benv/evil-leader-key
  :non-normal-prefix benv/evil-insert-mode-leader-key
  "e b" 'eval-buffer
  "f s" 'save-buffer
  "f D" 'benv/delete-file-and-buffer
  "f R" 'benv/rename-file-and-buffer
  "q q" 'save-buffers-kill-terminal
  "h m" 'woman
  "t l" 'visual-line-mode
  "t w" 'whitespace-mode
  "u"   'universal-argument
  "w m" 'delete-other-windows
  "w s" 'split-window-below
  "w v" 'split-window-right
  "y f" 'benv/yank-filename)

;; restore standard vim mappings

(general-def 'motion
  "C-i" 'evil-jump-forward
  "C-u" 'evil-scroll-up)

;;----------------------------------------
;; piglet.el
;;----------------------------------------

(load-file "~/.emacs.d/site-lisp/piglet/piglet.el")

;;----------------------------------------
;; compilation mode
;;----------------------------------------

(use-package compilation-mode
  :general
  ;; Unset default "SPC" binding
  ;; so that my evil leader key
  ;; behaves normally in dired
  (:keymaps 'compilation-mode-map "SPC" nil)
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "c c" 'compile))

;;----------------------------------------
;; ediff
;;----------------------------------------

(use-package ediff
  :config
  ;; Override default behaviour of opening ediff control window
  ;; in a new frame. (Show it in a new window along the bottom
  ;; of the current frame instead.)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;----------------------------------------
;; ivy/counsel/swiper
;;
;; Note: ivy, counsel, and swiper are
;; all included in the `ivy` package.
;;----------------------------------------

(use-package ivy
  :defer nil
  :config
  (ivy-mode 1)
  ;; remove annoying "^" that is inserted at
  ;; the beginning of ivy input by default
  (setq ivy-initial-inputs-alist nil)
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "b b" 'ivy-switch-buffer
   "b d" 'kill-this-buffer
   "b r" 'rename-buffer
   "b R" 'rename-buffer
   "i r" 'ivy-resume)
  (:keymaps 'ivy-minibuffer-map
            "C-o" 'ivy-call
            "<C-return>" 'ivy-immediate-done))

(use-package counsel
  :general
  ('motion
   "M-x" 'counsel-M-x)
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "c r" 'counsel-rg
   "f f" 'counsel-find-file
   "f r" 'counsel-recentf
   "p r" 'counsel-projectile-rg))

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
  :hook (org-mode . visual-line-mode)
  :config
  ;; Set foreground/background colors for all headings to black/gray.
  ;; By default, the "leuven" theme sets each heading level to very
  ;; different colors, resulting in an ugly rainbow effect that I find
  ;; very distracting.
  (dolist (face '(org-level-1 org-level-2 org-level-3
                  org-level-4 org-level-5 org-level-6
                  org-level-7 org-level-8))
    (set-face-attribute face nil
                        :height 1.0
                        :foreground "#3C3C3C" :background "#F0F0F0"
                        :underline nil :overline nil))
  ;; Change "TODO" foreground/background colors to green/gray,
  ;; which I find more soothing than the "leuven" theme's default
  ;; red/pink colors.
  (set-face-attribute 'org-todo nil
                        :background "#F0F0F0" ;"SeaGreen1"
                        :foreground "SeaGreen4"
                        :box '(:line-width 1 :color "SeaGreen4"))
  ;; align body text with parent org heading/bullet
  (setq org-startup-indented t)
  ;; when following links (org-open-at-point), open the
  ;; link in the same window (not the "other window")
  (setq org-link-frame-setup '((file . find-file)))
  ;; pressing Return key on a hyperlink will open it
  (setq org-return-follows-link t)
  ;; auto-expand all bullets when opening a file
  (setq org-startup-folded nil)
  (setq org-startup-with-inline-images t)
  (setq org-agenda-files '("~/Sync/notes/personal-todo.org"
                           "~/Sync/notes/20200711223732_work_todo.org"))
  ;; increase size of images used to preview LaTeX fragments
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.75))
  ;; preview all latex fragments in an .org file by default
  (setq org-startup-with-latex-preview t)
  (setq org-capture-templates
        '(("i" "inbox" entry (file "~/Sync/notes/20210325174000_inbox.org")
           "* TODO %?\n%i" :prepend t)
          ("w" "workflow" entry (file "~/Sync/notes/20210325103700_workflow_todo.org")
           "* TODO %?\n%i" :prepend t)))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (use-package orgit)
  :general
  ('motion
   :prefix benv/evil-leader-key
   "o a" 'org-agenda-list)
  ('motion
   "C-c c" 'org-capture)
  ('motion org-mode-map
   "TAB" 'org-cycle
   "RET" 'org-open-at-point)
  ('insert org-mode-map
   "C-c p" 'org-cliplink)
  ('motion org-mode-map
   :prefix benv/major-mode-leader-key
   "o r" 'org-attach-reveal-in-emacs
   "t i" 'org-toggle-inline-images
   "t l" 'org-toggle-link-display
   "t L" 'org-latex-preview
   "h i" 'org-insert-heading-after-current
   "I"   'org-clock-in
   "O"   'org-clock-out
   "p"   'org-cliplink))

;;----------------------------------------
;; org-roam
;;----------------------------------------

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :general
  ('motion
   :prefix benv/major-mode-leader-key
   "r b" 'org-roam
   "r i" 'org-roam-insert
   "r f" 'org-roam-find-file)
  ('insert
   "C-c r i" 'org-roam-insert)
  :config
  (setq org-roam-directory "~/Sync/notes")
  (org-roam-mode))

;;----------------------------------------
;; markdown-mode
;;----------------------------------------

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;;----------------------------------------
;; ace-link
;;----------------------------------------

(use-package ace-link
  :general
  ('motion org-mode-map
   :prefix benv/major-mode-leader-key
   "l" 'ace-link-org))

;;----------------------------------------
;; dired
;;----------------------------------------

(use-package dired
  :general
  ;; Unset default "SPC" binding
  ;; so that my evil leader keys
  ;; behaves normally in dired
  (:keymaps 'dired-mode-map
        benv/evil-leader-key nil
        benv/evil-insert-mode-leader-key nil)
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "d d" 'dired-jump
   "d e" '(lambda () (interactive) (dired "~/.emacs.d/"))
   "d h" '(lambda () (interactive) (dired "~"))
   "d w c" '(lambda () (interactive) (dired "/mnt/c/"))
   "d w d" '(lambda () (interactive) (dired "/mnt/d/"))
   "d w h" '(lambda () (interactive) (dired "/mnt/c/Users/Ben"))
   "d w t" '(lambda () (interactive) (dired "/mnt/d/tmp"))
   "c r" 'recompile)
  :config
  (setq dired-dwim-target t)
  ;; Confirm/cancel by pressing single 'y'/'n' key.
  ;; (The default is to spell out "yes" or "no").
  (setq dired-deletion-confirmer #'y-or-n-p)
  ;; Don't prompt when recursively deleting directories. (Just do it.)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-hal --group-directories-first")
  ;; general.el has a bug where it clobbers
  ;; the user's custom hjkl bindings
  ;; for a mode by running `evil-add-hjkl-bindings`.
  ;; The workaround is make the bindings
  ;; after dired-mode is loaded, as I am doing here.
  ;; See https://github.com/noctuid/general.el/issues/89.
  (general-def 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

;; Use hl-line-mode to highlight the current
;; file/directory line in dired
(use-package hl-line-mode
  :hook (dired-mode . hl-line-mode)
  :init
  ;; We use advice here because the `hl-line` face
  ;; is not defined until hl-line-mode is loaded
  (defadvice hl-line-mode (after benv/advise-hl-line-mode)
    (set-face-background 'hl-line "green")))

;;----------------------------------------
;; magit
;;----------------------------------------

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

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
    (:states '(motion insert emacs)
     :prefix benv/evil-leader-key
     :non-normal-prefix benv/evil-insert-mode-leader-key
     "g s" 'magit-status)
  :config
    ;; display magit status buffer in currently
    ;; selected window (not the "other" window)
    (setq magit-display-buffer-function
          'magit-display-buffer-same-window-except-diff-v1)
    (setq magit-log-margin '(nil age magit-log-margin-width t 18))
    ;; hide windows line endings ("^M") in magit status buffer
    (add-hook 'magit-status-mode-hook 'remove-dos-eol)
    (add-hook 'magit-diff-mode-hook 'remove-dos-eol)
    (use-package orgit))

;;----------------------------------------
;; projectile
;;----------------------------------------

(use-package projectile
  :config (projectile-mode 1))

(use-package counsel-projectile
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "p p" 'counsel-projectile-switch-project
   "p f" 'counsel-projectile-find-file))

;;----------------------------------------
;; winner
;;----------------------------------------

(winner-mode)

(general-def
  :states '(motion insert emacs)
  :prefix benv/evil-leader-key
  :non-normal-prefix benv/evil-insert-mode-leader-key
  "w u" 'winner-undo
  "w U" 'winner-redo
  "w r" 'winner-redo)

;;----------------------------------------
;; avy
;;----------------------------------------

(use-package avy
  :general
  (:states '(motion normal)
   "s" 'avy-goto-char-timer))

;;----------------------------------------
;; winum
;;----------------------------------------

(use-package winum
  :defer nil
  :config
  (winum-mode)
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
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

(general-def
  :states '(motion insert emacs)
  :prefix benv/evil-leader-key
  :non-normal-prefix benv/evil-insert-mode-leader-key
  "TAB" 'benv/switch-to-previous-buffer)

;;----------------------------------------
;; javascript
;;----------------------------------------

;; Unity-specific extensions for javascript
;; source files

(add-to-list 'auto-mode-alist '("\\.jspre\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jslib\\'" . js-mode))

;;----------------------------------------
;; C#
;;----------------------------------------

(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode))

(use-package omnisharp
  :hook (csharp-mode . omnisharp-mode)
  :general
  ('motion
   :prefix benv/major-mode-leader-key
   "g" 'omnisharp-go-to-definition
   "G" 'omnisharp-go-to-definition-other-window
   "u" 'omnisharp-find-usages))

(use-package company
  :hook (csharp-mode . company-mode)
  :config
  (add-to-list 'company-backends 'company-omnisharp))

;;----------------------------------------
;; calfw
;;----------------------------------------

(use-package calfw)

(use-package calfw-org
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "o c" 'cfw:open-org-calendar))

;;----------------------------------------
;; elfeed
;;----------------------------------------

(use-package elfeed
  :commands (elfeed)
  :general
  ('motion elfeed-search-mode-map
   "RET" 'elfeed-search-show-entry
   "C-l" 'elfeed-search-update--force
   "b" 'elfeed-search-browse-url
   "G" 'elfeed-search-fetch)
  ('normal elfeed-search-mode-map
   "q" 'elfeed-search-quit-window
   "r" 'elfeed-search-untag-all-unread
   "u" 'elfeed-search-tag-all-unread)
  ('normal elfeed-show-mode-map
   "q" 'elfeed-kill-buffer)
  :config
  (setq elfeed-feeds
        '("http://www.reddit.com/r/emacs/.rss"
          "https://forum.unity3d.com/forums/-/index.rss"
          "https://news.ycombinator.com/rss"
          "https://forum.unity3d.com/forums/10/index.rss")))

;;----------------------------------------
;; web browser
;;----------------------------------------

(setq browse-url-generic-program
    (or (executable-find "firefox.exe")
        (executable-find "firefox")))

(setq browse-url-browser-function 'browse-url-generic)

;;----------------------------------------
;; autoit-mode
;;
;; https://www.autoitscript.com/forum/topic/10818-emacs-major-mode-for-autoit-v3/
;;----------------------------------------

(use-package autoit
  :load-path "~/.emacs.d/local"
  :mode ("\\.au3\\'" . autoit-mode))

;;----------------------------------------
;; org-attach-screenshot
;;----------------------------------------

(defun benv/org-attach-screenshot ()
  "Call org-attach-screenshot with a prefix argument,
so that the emacs doesn't get hidden prior
to running the screenshot command."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-attach-screenshot)))

(use-package org-attach-screenshot
  :load-path "~/.emacs.d/site-lisp/org-screenshot"
  :general
  ('motion org-mode-map
    :prefix benv/major-mode-leader-key
    "s" 'benv/org-attach-screenshot)
  ('insert org-mode-map
    "C-c s" 'benv/org-attach-screenshot)
  :config
  (setq org-attach-screenshot-command-line
        "cliprect-wsl %f"))

;;----------------------------------------
;; shell mode
;;----------------------------------------

(use-package shell
  :general
  ;; make C-d do PageDown (like vim) instead of delete character
  ('normal shell-mode-map
    "C-d" 'evil-scroll-down)
  :config
  ;; Open shell-mode in the currently selected window.
  ;; (By default emacs uses `pop-to-buffer` to select
  ;; the buffer for shell-mode.)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*shell*") display-buffer-same-window)))

;;----------------------------------------
;; vterm
;;----------------------------------------

(use-package vterm
  :general
  ('motion
   :prefix benv/evil-leader-key
   "$" 'vterm))

;;----------------------------------------
;; emacs-lisp-mode
;;----------------------------------------

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :general
  (:keymaps 'lispy-mode-map
   :states '(motion insert emacs)
   :prefix "C-c"
   "f" 'lispy-ace-paren
   "h" 'lispy-left
   "l" 'lispy-right)
  :config
  ;; Add some custom lispy key bindings, beyond the
  ;; ones provided by evil-collection in
  ;; ~/.emacs.d/elpa/evil-collection-20210401.1012/modes/lispy/evil-collection-lispy.el
  (let ((map evil-collection-lispy-mode-map-special))
    (lispy-define-key map "M-(" 'lispy-wrap-round)))

(use-package show-paren-mode
  :hook (emacs-lisp-mode . show-paren-mode))

;;----------------------------------------
;; Windows clipboard integration
;;----------------------------------------

(defun yank-region-to-windows-clipboard (start end)
  (interactive "r")
  (shell-command-on-region start end "clip.exe"))

(general-def 'motion
  :prefix benv/evil-leader-key
  "y w" 'yank-region-to-windows-clipboard)

;;----------------------------------------
;; themes
;;----------------------------------------

(load-theme 'leuven)

;;----------------------------------------
;; customization system
;;----------------------------------------

;; By default, Emacs appends code generated by
;; the customization system to the end of the user's
;; `init.el` (i.e. this file), which I find highly
;; messy and annoying. Tell emacs to write the
;; customization code to its own file instead.

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;----------------------------------------
;; Print emacs startup time.
;;
;; (Overrides default minibuffer message
;; immediately after emacs startup.)
;;----------------------------------------

(defun display-startup-echo-area-message ()
  (message "emacs startup time: %s" (emacs-init-time)))
