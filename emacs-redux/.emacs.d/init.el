;;; -*- lexical-binding: t -*-

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

(let* ((hostname (system-name))
       (font-size (cond
                   ((string= hostname "framework") 110)
                   (t 130))))
  (set-face-attribute 'default nil :height font-size))

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
;; general.el
;;----------------------------------------

(use-package general)

;;----------------------------------------
;; evil
;;----------------------------------------

(setq benv/evil-leader-key "SPC")
(setq benv/major-mode-leader-key ",")
(setq benv/evil-insert-mode-leader-key "C-SPC")

(use-package evil
  :defer nil
  :init
  ;; evil-collection requires the following settings before
  ;; loading evil and evil-collection. (See evil-collection
  ;; README for further details.)
  (setq evil-want-keybinding nil
        evil-want-integration t)
  :custom
  ;; Setting `evil-move-beyond-eol` to `t` makes it possible to move
  ;; the cursor one position past the last character in a line. This
  ;; can be thought of as positioning the character on the newline
  ;; character.
  ;;
  ;; The default value of `evil-move-beyond-eol` is nil, but this
  ;; setting prevents some of emacs' built-in elisp functions from
  ;; working as in a consistent manner, and was making some of my own
  ;; lisp-parsing code (for lisp-tree-mode) way more complicated than
  ;; it needed to be. In particular, the intended behaviour of emacs'
  ;; `forward-sexp` function is to always move to the first character
  ;; *after* the current/next sexp. But if `evil-move-beyond-eol` is
  ;; set to nil, then `forward-sexp` can get at the last character of
  ;; a line (e.g. ")"), rather than moving to the newline character
  ;; after it.  And that means that I need to handle sexp's that end
  ;; at line endings as a special case in my code, which I don't want
  ;; to do.
  ;;
  ;; For further explanation/discussion of this issue, see [1].
  ;;
  ;; [1]: https://github.com/syl20bnr/spacemacs/issues/2525
  (evil-move-beyond-eol t)
  :general
  ;; Unbind "," and "SPC" so I can use them as prefix
  ;; keys, without getting "Key sequence starts
  ;; with non-prefix key" error.
  (:states '(motion normal visual operator)
   benv/evil-leader-key nil
   benv/major-mode-leader-key nil)
  ;; Make standard readline/emacs keybinds available
  ;; in evil insert mode.
  (:states 'insert
           "C-a" #'beginning-of-line
           "M-b" #'backward-word
           "M-f" #'forward-word
           "C-k" #'kill-line
           "M-d" #'kill-word
           "C-e" #'evil-scroll-line-down
           "C-y" #'evil-scroll-line-up)
  :config
  (evil-mode 1)
  (setq evil-move-cursor-back nil)
  (evil-select-search-module 'evil-search-module 'evil-search))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :defer nil
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (setq evil-collection-key-blacklist
        (list benv/evil-leader-key
              benv/major-mode-leader-key
              benv/evil-insert-mode-leader-key))
  (evil-collection-init))

;;----------------------------------------
;; minibuffer settings
;;----------------------------------------

(use-package emacs
  :defer nil
  :init
  ;; Create auto-save files (e.g. '#myfile.txt#')
  ;; in a separate directory.
  (make-directory "~/.emacs.d/auto-save" t)
  (setq auto-save-file-name-transforms
        `((".*" "~/.emacs.d/auto-save/" t)))
  ;; Enable recursive minibuffers. Among other uses
  ;; cases, this allows you to run a command with M-x
  ;; while you are editing text in the minibuffer.
  (setq enable-recursive-minibuffers t)
  ;; Display current depth of minibuffer recursion
  ;; as a number surrounded by square brackets,
  ;; e.g. `[2]`.
  (minibuffer-depth-indicate-mode)
  :general
  ;; Unbind the Escape key in the minibuffer.
  ;;
  ;; When using `evil-collection-setup-minibuffer`
  ;; to enable evil keybindings in the minibuffer,
  ;; the default behaviour of the Escape key is
  ;; to quit the minibuffer. This is annoying
  ;; because I often do complex editing of shell
  ;; commands in the minibuffer, and accidentally
  ;; hitting the Escape key in normal mode causes
  ;; me to prematurely exit the minibuffer and lose
  ;; whatever text/command I was writing.
  (:states '(motion normal emacs)
   :keymaps '(minibuffer-local-map minibuffer-inactive-mode-map)
   "<escape>" nil))

;;----------------------------------------
;; proced: interactive equivalent of 'ps' command
;;----------------------------------------

(use-package proced
  :init
  (defun benv/proced-mode-setup ()
    ;; Turn on auto-refresh processes list.
    (setq proced-auto-update-flag t)
    ;; Refresh process list every 1 seconds.
    ;; Note: If try to set this smaller than 1
    ;; (e.g. 0.5), it doesn't seem to have any
    ;; practical effect.
    (setq proced-auto-update-interval 1))
  (add-hook 'proced-mode-hook #'benv/proced-mode-setup))

;;----------------------------------------
;; sudo-edit
;;
;; Edit a file with sudo permissions.
;;----------------------------------------

(use-package sudo-edit
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "t s" 'sudo-edit))

;;----------------------------------------
;; face-remap (built-in package)
;;----------------------------------------

(use-package face-remap
  :config
  ;; adjusts text-scaling increment for each keypress
  (setq text-scale-mode-step 1.1)
  :general
  ;; use the keybinds for zoom in/out as web browsers
  (:states '(motion insert emacs)
   "C-=" 'text-scale-adjust
   "C-+" 'text-scale-adjust
   "C--" 'text-scale-adjust))

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
  "b b" 'switch-to-buffer
  "b d" 'kill-this-buffer
  "b r" 'rename-buffer
  "b R" 'rename-buffer
  "e b" 'eval-buffer
  "f f" 'find-file
  "f s" 'save-buffer
  "f C" 'write-file ; behaves like "File -> Save As"
  "f D" 'benv/delete-file-and-buffer
  "f R" 'benv/rename-file-and-buffer
  "q q" 'save-buffers-kill-terminal
  "h m" 'woman
  "t d" 'toggle-debug-on-error
  "t l" 'visual-line-mode
  "u"   'universal-argument)

;; restore standard vim mappings

(general-def 'motion
  "C-i" 'evil-jump-forward
  "C-u" 'evil-scroll-up)

;;----------------------------------------
;; whitespace-mode
;;----------------------------------------

(use-package whitespace
  :custom
  ;; Same as default value, but with `lines` removed to
  ;; disable yellow highlighting of long lines.
  (whitespace-style '(face
                      tabs
                      spaces
                      trailing
                      space-before-tab
                      newline
                      indentation
                      empty
                      space-after-tab
                      space-mark
                      tab-mark
                      newline-mark))
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "t w" 'whitespace-mode))

;;----------------------------------------
;; hippie-expand ("M-/")
;;----------------------------------------

;; By default "M-/" is bound to `dabbrev-expand`,
;; which completes a word based on other
;; words found in the current buffer.
;; `hippie-expand` is similar to `dabbrev-expand`
;; but it is smarter and more configurable [1].
;;
;; The "M-=" binding is a no-op that can be used to
;; concatenate multiple completions (e.g.
;; completing consecutive components of a file path).
;; Since repeatedly pressing "M-/" cycles through
;; the completion candidates, calling `ignore` provides a
;; way to "accept" the current completion and start
;; a new one. (I chose "M-=" because `=` is next
;; `/` in the Dvorak keyboard layout.)
;;
;; [1]: https://www.masteringemacs.org/article/text-expansion-hippie-expand

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-=") 'ignore)

;;----------------------------------------
;; vertico
;;----------------------------------------

(use-package vertico
  :defer nil
  :config
  (vertico-mode)
  :general
  (:states '(motion insert emacs)
   :keymaps 'vertico-map
   "M-<" 'vertico-first
   "M->" 'vertico-last
   "C-n" 'vertico-next
   "C-p" 'vertico-previous
   "C-v" 'vertico-scroll-up
   "M-v" 'vertico-scroll-down))

;;----------------------------------------
;; consult
;;----------------------------------------

(use-package consult
  :config
  (defun benv/grep-notes ()
    "Run grep on my notes."
    (interactive)
    (consult-ripgrep "~/Sync/notes"))
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "c r" 'consult-ripgrep
   "r g" 'benv/grep-notes
   "m m" 'consult-man
   "s s" 'consult-line))

;;----------------------------------------
;; embark
;;----------------------------------------

(use-package embark

  :defer nil

  :general

  (:states '(motion insert emacs)
   "C-," 'embark-act         ;; pick some comfortable binding
   "C-;" 'embark-dwim        ;; good alternative: M-.
   "C-h B" 'embark-bindings) ;; alternative for `describe-bindings'

  (:states '(motion insert emacs)
   :keymaps 'minibuffer-local-map
   "M-," 'embark-become)

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :custom

  ;; The list below is the default value for `embark-indicators`,
  ;; but with the `embark-mixed-indicator` element removed
  ;; and `embark--vertico-indicator`/`embark-minimal-indicator`
  ;; added.
  ;;
  ;; `embark-mixed-indicator` causes Embark to automatically
  ;; open a help window with available keybindings when there
  ;; no key press for a short period of time. (The behaviour is
  ;; similar to `which-key`). I don't like that behaviour
  ;; because the embark help window is huge and obsures a
  ;; lot of the emacs frame.
  ;;
  ;; Instead of the automatic help window, I prefer to press
  ;; C-h when I can't remember what key to press, which
  ;; opens a completion list of the available embark actions.

  (embark-indicators '(embark--vertico-indicator
                       embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;----------------------------------------
;; orderless
;;----------------------------------------

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;----------------------------------------
;; marginalia
;;----------------------------------------

(use-package marginalia
  :defer nil
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;;----------------------------------------
;; window (built-in emacs package)
;;----------------------------------------

(use-package window
  :init
  (setq benv/window-resize-step-horizontal 4)
  (setq benv/window-resize-step-vertical 2)
  (defun benv/shrink-window ()
    (interactive)
    (shrink-window benv/window-resize-step-vertical))
  (defun benv/enlarge-window ()
    (interactive)
    (enlarge-window benv/window-resize-step-vertical))
  (defun benv/shrink-window-horizontally ()
    (interactive)
    (shrink-window-horizontally benv/window-resize-step-horizontal))
  (defun benv/enlarge-window-horizontally ()
    (interactive)
    (enlarge-window-horizontally benv/window-resize-step-horizontal))

  (defun benv/mirror-window (dir)
    "Make the neighbour window in direction DIR a mirror of
the current window. In other words, make the selected buffer,
cursor position, and window scroll position of the neighbour
window identical to the current window.

If a neighbour window does not already exist in direction DIR,
this function will split the current window."
    (unless (window-in-direction dir)
      (split-window nil nil dir))
    (let ((buffer (current-buffer))
          (start (window-start))
          (window (window-in-direction dir)))
      (save-selected-window (select-window window)
                            (set-window-start (selected-window) start)
                            (switch-to-buffer buffer))))

  (defun benv/switch-to-buffer-in-dir (dir)
    "Select a buffer interactively and open it in the
neighbour window in direction DIR, without changing
the currently selected window.

If a neighbour window does not already exist in direction DIR,
this function will create one by splitting the current
window."
    (when-let ((buffer (read-buffer "buffer: ")))
      (unless (window-in-direction dir)
        (split-window nil nil dir))
      (save-selected-window (select-window (window-in-direction dir))
                            (switch-to-buffer buffer))))

  (defun benv/switch-to-buffer-in-dir-and-focus (dir)
    "Select a buffer interactively, then select the
neighbour window in direction DIR and open the buffer
there.

If a neighbour window does not already exist in direction DIR,
this function will create one by splitting the current
window."
    (when-let ((buffer (read-buffer "buffer: ")))
      (unless (window-in-direction dir)
        (split-window nil nil dir))
      (select-window (window-in-direction dir))
      (switch-to-buffer buffer)))

  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "b h" (lambda () (interactive) (benv/switch-to-buffer-in-dir 'left))
   "b j" (lambda () (interactive) (benv/switch-to-buffer-in-dir 'down))
   "b k" (lambda () (interactive) (benv/switch-to-buffer-in-dir 'up))
   "b l" (lambda () (interactive) (benv/switch-to-buffer-in-dir 'right))
   "b H" (lambda () (interactive) (benv/switch-to-buffer-in-dir-and-focus 'left))
   "b J" (lambda () (interactive) (benv/switch-to-buffer-in-dir-and-focus 'down))
   "b K" (lambda () (interactive) (benv/switch-to-buffer-in-dir-and-focus 'up))
   "b L" (lambda () (interactive) (benv/switch-to-buffer-in-dir-and-focus 'right))
   "w m h" '(lambda () (interactive) (benv/mirror-window 'left))
   "w m l" '(lambda () (interactive) (benv/mirror-window 'right))
   "w M" 'delete-other-windows
   "w o" 'delete-other-windows
   "w s" 'split-window-below
   "w v" 'split-window-right
   "y f" 'benv/yank-filename)
  (:states '(motion normal insert emacs)
   "M-J" 'benv/enlarge-window
   "M-K" 'benv/shrink-window
   "M-H" 'benv/shrink-window-horizontally
   "M-L" 'benv/enlarge-window-horizontally))

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

  ;; Disable confirmation prompt to quit ediff session.
  ;; See: https://emacs.stackexchange.com/a/24602

  (defun disable-y-or-n-p (orig-fun &rest args)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
      (apply orig-fun args)))

  (advice-add 'ediff-quit :around #'disable-y-or-n-p)

  ;; Override default behaviour of opening ediff control window
  ;; in a new frame. (Show it in a new window along the bottom
  ;; of the current frame instead.)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; open diffs in a vertical split by default
  (setq ediff-split-window-function 'split-window-horizontally))

;;----------------------------------------
;; info-mode
;;----------------------------------------

(use-package info
  :general
  ;; Unbind "SPC" so that evil leader key works in *info* buffers
  (:keymaps 'Info-mode-map "SPC" nil))

;;----------------------------------------
;; org-mode
;;----------------------------------------

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . visual-line-mode)
  :custom
  (org-confirm-babel-evaluate . nil)
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
  ;; Prevents org-mode from inserting unwanted indentation every time
  ;; I insert a line with 'o' or 'O' in evil-mode.
  ;; See: https://github.com/syl20bnr/spacemacs/issues/13255
  (setq org-src-preserve-indentation t)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (use-package orgit)
  ;; org-babel stuff
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))
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
   "T i c" 'org-table-insert-column
   "T d c" 'org-table-delete-column
   "h i" 'org-insert-heading-after-current
   "I"   'org-clock-in
   "O"   'org-clock-out
   "p"   'org-cliplink))

;;----------------------------------------
;; org-roam
;;----------------------------------------

(when (file-directory-p "~/Sync/notes")
  (use-package org-roam
    :hook (after-init . org-roam-mode)
    :general
    ('motion org-mode-map
             :prefix benv/major-mode-leader-key
             "r b" 'org-roam
             "r i" 'org-roam-insert)
    ('motion
     :prefix benv/evil-leader-key
     "r f" 'org-roam-find-file
     "r g" 'benv/grep-notes)
    ('insert
     "C-c r i" 'org-roam-insert)
    :config
    (setq org-roam-directory "~/Sync/notes")
    (org-roam-mode)))

;;----------------------------------------
;; markdown-mode
;;----------------------------------------

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;;----------------------------------------
;; ace-link
;;----------------------------------------

(use-package ace-link

  :commands (ace-link
             ace-link--org-collect
             ace-link--org-action)

  :init

  (defun benv/ace-link-org-winum (n focus)
    "Open a visible link in an `org-mode' buffer in window N.
If focus is non-nil, switch focus to window N. Otherwise,
focus remains in the current window.

This function is a modified version of `ace-link-org`
from ace-link.el."
    (interactive)
    (require 'org)
    (let* ((pt (avy-with ace-link-org
                 (avy-process
                  (mapcar #'cdr (ace-link--org-collect))
                  (avy--style-fn avy-style))))
           (func (lambda () (interactive) (ace-link--org-action pt))))
      (benv/call-function-in-window-n func n focus)))

  (defun benv/create-ace-link-winum-keybinds ()
    (dolist (tuple '((1 . "!")
                     (2 . "@")
                     (3 . "#")
                     (4 . "$")
                     (5 . "%")
                     (6 . "^")
                     (7 . "&")
                     (8 . "*")
                     (9 . "(")))
      (general-def
        :states '(motion insert emacs)
        :prefix benv/evil-leader-key
        :non-normal-prefix benv/evil-insert-mode-leader-key
        "l ." #'ace-link)
      (let ((n (car tuple))
            (sym (cdr tuple)))
        (general-def
          :states '(motion insert emacs)
          :prefix benv/evil-leader-key
          :non-normal-prefix benv/evil-insert-mode-leader-key
          (format "l %s" n) (lambda () (interactive) (benv/ace-link-org-winum n nil))
          (format "l %s" sym) (lambda () (interactive) (benv/ace-link-org-winum n t))))))

  (benv/create-ace-link-winum-keybinds)

  :general
  ('motion org-mode-map
   :prefix benv/major-mode-leader-key
   "l" 'ace-link-org))


;;----------------------------------------
;; recentf
;;----------------------------------------

(use-package recentf
  :init
  (defun benv/recentf ()
    "Present a list of recently opened files."
    (interactive)
    (let ((file (completing-read "Open File: " recentf-list)))
      (find-file file)))
  :config
  (recentf-mode)
  ;; Default is 20.
  (setq recentf-max-saved-items 500)
  ;; Save the recent files list to disk every
  ;; 5 minutes. (The default behaviour only saves
  ;; it when exiting emacs.)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "f r" 'benv/recentf))

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
   "d /" '(lambda () (interactive) (dired "/"))
   "d e" '(lambda () (interactive) (dired "~/.emacs.d/"))
   "d h" '(lambda () (interactive) (dired "~"))
   "d r" 'benv/recentd
   "d w c" '(lambda () (interactive) (dired "/mnt/c/"))
   "d w d" '(lambda () (interactive) (dired "/mnt/d/"))
   "d w h" '(lambda () (interactive) (dired "/mnt/c/Users/Ben"))
   "d w t" '(lambda () (interactive) (dired "/mnt/d/tmp"))
   "e ." '(lambda () (interactive) (benv/open-windows-explorer default-directory))
   "e r" 'benv/windows-explorer-recentd)
  :config

  ;; Add directories to recentf list (recently open files history),
  ;; so that we can quickly open them in dired via recentf.

  (recentf-mode)

  (defun benv/recentf-add-dired ()
    "If the current buffer is a dired buffer, add the directory to the
recentf list (recently opened files history). This allows me to quickly jump
to a recently/frequently accessed directory in dired via recentf."
    (and (derived-mode-p 'dired-mode) default-directory
         (recentf-add-file default-directory))
    ;; Must return nil because it is run from `write-file-functions'.
    nil)

  (add-hook 'dired-after-readin-hook 'benv/recentf-add-dired)

  (defun benv/recentd-list ()
    "Return a dedup'ed list of directories recently visited in dired.
The directory list is extracted from `recentf-list`."
    (delete-dups
     (seq-filter #'file-directory-p recentf-list)))

  (defun benv/recentd ()
    "Present a list of recently used directories and open the selected one in dired"
    (interactive)
    (let ((recent-dirs
           (benv/recentd-list)))
      (let ((dir (completing-read "Directory: " recent-dirs)))
        (dired dir))))

  ;; I copied this code from:
  ;; https://emacs.stackexchange.com/questions/64588/how-do-i-get-all-marked-files-from-all-dired-buffers
  (defun benv/dired-get-marked-files-all-buffers ()
    "Return a list of marked files from all Dired buffers."
    (let ((files  ())
          (here   ()))
      (dolist (buf  (mapcar #'cdr dired-buffers))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq here  (dired-get-marked-files nil nil nil t)))
          (when (or (null (cdr here))  (eq t (car here)))
            (setq here  (cdr here)))
          (setq files  (nconc here files))))
      (setq files  (delete-dups files))))
  ;; Do a recursive diff on two directories.
  (defun benv/dired-ztree-diff-marked-files ()
    (interactive)
    (let* ((marked-files (benv/dired-get-marked-files-all-buffers))
           (dir1 (pop marked-files))
           (dir2 (pop marked-files)))
      (when (and dir1 dir2)
        (ztree-diff dir1 dir2))))

  (defun benv/open-windows-explorer (dir)
    "Open Windows Explorer (file manager) in directory DIR.

Note: This command works as desired, but the shell command always
returns exit status 1, for reasons I don't understand.
"
    (let ((process-connection-type nil)
          (default-directory dir))
      (benv/async-shell-command-silent "explorer.exe .")))

  (defun benv/windows-explorer-recentd ()
    "Open a recently visited directory in Windows Explorer."
    (interactive)
    (let ((dir (completing-read "dir: " (benv/recentd-list))))
      (benv/open-windows-explorer dir)))

  ;; When two dired buffers are visible, use the "other" dired buffer
  ;; as the default target for file operations (copy, move, etc.)
  (setq dired-dwim-target t)
  ;; Confirm/cancel by pressing single 'y'/'n' key.
  ;; (The default is to spell out "yes" or "no").
  (setq dired-deletion-confirmer #'y-or-n-p)
  ;; Don't prompt when recursively deleting directories. (Just do it.)
  (setq dired-recursive-deletes 'always)
  ;; Don't prompt when recursively copying directories. (Just do it.)
  (setq dired-recursive-copies 'always)
  (setq dired-listing-switches "-hAl --group-directories-first")
  ;; general.el has a bug where it clobbers
  ;; the user's custom hjkl bindings
  ;; for a mode by running `evil-add-hjkl-bindings`.
  ;; The workaround is make the bindings
  ;; after dired-mode is loaded, as I am doing here.
  ;; See https://github.com/noctuid/general.el/issues/89.
  (general-def 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

;; Press "f" in a dired buffer to trigger fast directory
;; filtering/navigation using `dired-narrow`. Currently I
;; am using my own patched copy of `dired-narrow.el` which
;; contains a couple of minor bug fixes (see
;; ~/.emacs.d/site-lisp/dired-narrow/README.md).
(use-package dired-narrow
  :load-path "~/.emacs.d/site-lisp/dired-narrow"
  :init
  (defun benv/dired-narrow-recurse ()
    (let ((filename (dired-utils-get-filename)))
      (revert-buffer)
      (dired-find-file)
      (when (file-directory-p filename)
        (dired-narrow))))
  (defun benv/dired-narrow-goto-first-file ()
    (interactive)
    (with-current-buffer
        dired-narrow-buffer
        (while (dired-hacks-previous-file))))
  (defun benv/dired-narrow-goto-last-file ()
    (interactive)
    (with-current-buffer dired-narrow-buffer
      (forward-line -1)
      (while (dired-hacks-next-file))
      (dired-hacks-previous-file)))
  :general
  (:keymaps 'dired-mode-map
   :states 'normal
   "f" 'dired-narrow)
  (:keymaps 'dired-narrow-map
   :states '(motion insert emacs)
   "M-<" 'benv/dired-narrow-goto-first-file
   "M->" 'benv/dired-narrow-goto-last-file
   "C-n" 'dired-narrow-next-file
   "C-p" 'dired-narrow-previous-file)
  :config
  (setq dired-narrow-exit-action 'benv/dired-narrow-recurse)
  (setq dired-narrow-exit-when-one-left nil))

;; dired-subtree: expand directories in dired using TAB.
(use-package dired-subtree
  :config
  ;; Fix bug where dired subtrees appear as black blobs:
  ;; https://github.com/fniessen/emacs-leuven-theme/issues/64
  (setq dired-subtree-use-backgrounds nil)
  :general
  (:keymaps 'dired-mode-map
   :states 'normal
   "TAB" 'dired-subtree-toggle
   ; Note: "<backtab>" represents Shift+Tab.
   "<backtab>" 'dired-subtree-remove
   "<C-tab>" 'dired-subtree-cycle))

;; Use hl-line-mode to highlight the current
;; file/directory line in dired
(use-package hl-line
  :demand t
  :config
  (global-hl-line-mode +1)
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "t h" 'global-hl-line-mode))

;;----------------------------------------
;; magit
;;----------------------------------------

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(use-package magit
  :config
  ;; Hack `magit-copy-section-value` with an advice, so that copying
  ;; a commit in a magit buffer (keybinding `y s`) copies the
  ;; abbreviated 7-character commit ID rather than the full ID.
  (defun benv/copy-section-value-abbrev ()
    (let ((result (substring (car kill-ring-yank-pointer) 0 7)))
      (kill-new result)
      (message result)))
  (advice-add 'magit-copy-section-value :after #'benv/copy-section-value-abbrev)
  :general
    ;; Unset any bindings for my evil leader
    ;; key (currently "SPC") in magit, so that
    ;; my evil leader key behaves normally
    ;; in magit windows.
    (:keymaps '(magit-status-mode-map
                magit-log-mode-map
                magit-diff-mode-map
                magit-refs-mode-map)
                "SPC" nil)
    (:states '(motion insert emacs)
     :prefix benv/evil-leader-key
     :non-normal-prefix benv/evil-insert-mode-leader-key
     "g s" 'magit-status
     "g l" 'benv/magit-log-head
     "g L" 'magit-log-all)
  :config
    (defun benv/magit-log-head ()
      (interactive)
      (magit-log-head))
    ;; display magit status buffer in currently
    ;; selected window (not the "other" window)
    (setq magit-display-buffer-function
          'magit-display-buffer-same-window-except-diff-v1)
    (setq magit-log-margin '(nil age magit-log-margin-width t 18))
    (setq magit-diff-refine-hunk t)
    ;; hide windows line endings ("^M") in magit status buffer
    (add-hook 'magit-status-mode-hook 'remove-dos-eol)
    (add-hook 'magit-diff-mode-hook 'remove-dos-eol)
    (use-package orgit))

;;----------------------------------------
;; projectile
;;----------------------------------------

(use-package projectile
  :custom
  ;; use the emacs' default completion system (i.e. completing-read),
  ;; so that projectile completions are handled by vertico.
  (projectile-completion-system 'default)
  :config
  (defun benv/projectile-dired ()
    "Jump to root directory of a project in dired."
    (interactive)
    (let ((dir (completing-read
                "project: "
                projectile-known-projects)))
      (dired dir)))
  (defun benv/projectile-magit-status ()
    "Jump to magit status buffer for a projectile project."
    (interactive)
    (let ((project-dir (completing-read
                        "magit status: "
                        projectile-known-projects)))
      (magit-status project-dir)))
  (defun benv/projectile-magit-log-current ()
    "Jump to magit log buffer for a projectile project."
    (interactive)
    (let ((default-directory (completing-read
                              "magit log all: "
                              projectile-known-projects)))
      (magit-log-head)))
  (defun benv/projectile-magit-log-all ()
    "Jump to magit log buffer for a projectile project."
    (interactive)
    (let ((default-directory (completing-read
                              "magit log all: "
                              projectile-known-projects)))
      (magit-log-all)))

  (defun benv/projectile-ripgrep ()
    "Run consult-ripgrep on a projectile project."
    (interactive)
    (let ((default-directory (completing-read
                              "ripgrep on project: "
                              projectile-known-projects)))
      (consult-ripgrep)))

  (defun benv/projectile-unity ()
    "Launch Unity from the root directory of a recently opened project.
Determine the correct Unity executable for the project by using `unity-which`."
    (interactive)
    (let ((default-directory (completing-read
                               "project: "
                               projectile-known-projects))
          (process-connection-type nil))
      (async-shell-command
        (format "\"$(unity-which)\" -logFile - -projectPath \"$(syspath %s)\""
                default-directory))))

  (projectile-mode 1)
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "p d" 'benv/projectile-dired
   "p p" 'projectile-switch-project
   "p r" 'benv/projectile-ripgrep
   "p f" 'projectile-find-file
   "p u" 'benv/projectile-unity
   "p g s" 'benv/projectile-magit-status
   "p g l" 'benv/projectile-magit-log-current
   "p g L" 'benv/projectile-magit-log-all))

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
  :init

  (defun benv/call-function-same-window (func)
    "Call function FUNC and display the resulting buffer
(if any) in the current window."
    (let ((display-buffer-alist '((".*" display-buffer-same-window))))
      (call-interactively func)))

  (defun benv/select-window-n-and-call-function (func n)
    "Change focus to window N (as numbered by winum-mode),
d invoke the given function FUNC."
    (interactive)
    (let ((calling-buffer (buffer-name))
          (calling-point (point)))
      (winum-select-window-by-number n)
      ;; force function to show buffers in the currently selected window
      (let ((display-buffer-alist '((".*" display-buffer-same-window))))
        ;; Switch to the same buffer and cursor position ("point")
        ;; as the calling window, so that the function behaves
        ;; exactly as if it was invoked from the calling window.
        ;;
        ;; Many commands behave differently based on
        ;; the current value of `default-directory`
        ;; (e.g. `dired`), `buffer-file-name` (e.g. `dired-jump`),
        ;; `point` (e.g. `find-function-at-point`).
        (switch-to-buffer calling-buffer)
        (goto-char calling-point)
        (call-interactively func))))

  (defun benv/call-function-in-window-n (func n focus)
    "Invoke the given function FUNC in target window N, as numbered by winum-mode.
If FOCUS is not nil, change input focus to target window N. Otherwise, keep
the focus in the current window."
    (interactive)
    (if focus (benv/select-window-n-and-call-function func n)
      (save-selected-window
        (benv/select-window-n-and-call-function func n))))

  (defun benv/create-winum-keybinds (prefix-keys func)
    "Create key bindings that call function FUNC in windows 1-9, as
identified by winum-mode.  The generated keybindings consist of the
evil leader key (SPC), followed by PREFIX-KEYS, followed by a single
digit (1-9) or number key symbol (!,@,#,$,^,&,*,().

For example, if PREFIX-KEYS is \"b\" and FUNC is switch-to-buffer, the
functions will generate bindings for \"SPC b 1\" and \"SPC b !\" that
call switch-to-buffer in window 1.

The difference between the number and symbol key bindings is
the focused window after the FUNC has completed. The number bindings
keep the focus in the current window, whereas the symbol key bindings
will change the focus to the target window."
    (dolist (tuple '((1 . "!")
                     (2 . "@")
                     (3 . "#")
                     (4 . "$")
                     (5 . "%")
                     (6 . "^")
                     (7 . "&")
                     (8 . "*")
                     (9 . "(")))
      (general-def
        :states '(motion insert emacs)
        :prefix benv/evil-leader-key
        :non-normal-prefix benv/evil-insert-mode-leader-key
        (format "%s ." prefix-keys) (lambda () (interactive) (benv/call-function-same-window func)))
      (let ((n (car tuple))
            (sym (cdr tuple)))
        (general-def
          :states '(motion insert emacs)
          :prefix benv/evil-leader-key
          :non-normal-prefix benv/evil-insert-mode-leader-key
          (format "%s %s" prefix-keys n) (lambda () (interactive) (benv/call-function-in-window-n func n nil))
          (format "%s %s" prefix-keys sym) (lambda () (interactive) (benv/call-function-in-window-n func n t))))))

  (benv/create-winum-keybinds "b" #'switch-to-buffer)
  (benv/create-winum-keybinds "d" #'dired-jump)
  (benv/create-winum-keybinds "h f" #'describe-function)
  (benv/create-winum-keybinds "h v" #'describe-variable)
  (benv/create-winum-keybinds "m" #'woman)
  (benv/create-winum-keybinds "w d" #'delete-window)
  (benv/create-winum-keybinds "P" #'proced)

  :config
  ;; enable winum-mode globally
  (winum-mode)

  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "0" 'winum-select-window-0
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4
   "5" 'winum-select-window-5
   "6" 'winum-select-window-6
   "7" 'winum-select-window-7
   "8" 'winum-select-window-8
   "9" 'winum-select-window-9))

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
;; C/C++
;;----------------------------------------

(use-package cc-mode
  :init
  (setq c-default-style "stroustrup"))

(use-package lsp
  :hook (c-mode . lsp-mode)
  :config
  (setq lsp-enable-snippet nil)
  (setq lsp-completion-provider :none))

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
;; shell-command / async-shell-command
;;----------------------------------------

(use-package simple
  :defer nil
  :commands (shell-command async-shell-command)
  :init
  (defun set-exec-path-from-shell-path ()
    "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell.

Source: https://www.emacswiki.org/emacs/ExecPath"
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string
                            "[ \t\n]*$" ""
                            (shell-command-to-string
                             "$SHELL --login -c 'echo $PATH' 2>/dev/null"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (defun benv/shell-command-history ()
    "Search shell-command-history and insert
result into current buffer (e.g. minibuffer)."
    (interactive)
    (let* ((vertico-sort-function nil)
           (command (completing-read "cmd: " shell-command-history)))
      (insert command)))

  (defun benv/async-shell-command-silent (cmd)
    "Works just like `async-shell-command`, but does not automatically
display a buffer with the STDOUT/STDERR from the command."
    (let (;; Do not automatically display the buffer with the
          ;; shell command output. Under normal circumstances,
          ;; `explorer.exe .` doesn't print anything to the console
          ;; anyway.
          (display-buffer-alist
           (list (cons "\\*Async Shell Command\\*.*"
                       (cons #'display-buffer-no-window nil)))))
      (async-shell-command cmd)))

  ;; Make sure command-line tools like `git log`
  ;; don't try to use a pager (e.g. `less`), since
  ;; emacs shell buffers can't handle interactive
  ;; terminal programs and the resulting output gets
  ;; garbled.
  (setenv "PAGER" "cat")

  :general
  (:keymaps 'minibuffer-local-shell-command-map
   :states '(motion insert emacs)
   "C-a" 'move-beginning-of-line
   "C-e" 'move-end-of-line
   "C-k" 'kill-line
   "C-n" 'next-history-element
   "C-p" 'previous-history-element
   "C-r" 'benv/shell-command-history)
  :config
  (set-exec-path-from-shell-path))

;;----------------------------------------
;; shelldon
;; https://github.com/Overdr0ne/shelldon
;;----------------------------------------

(use-package shelldon

  :demand t
  :load-path "~/.emacs.d/site-lisp/shelldon"

  :commands (shelldon
             shelldon-send-line-at-point
             shelldon-send-region
             shelldon-output-history)

  :init

  ;; Toggle whether emacs communicates with subprocesses (shell commands)
  ;; using a pty or a simple pipe.
  ;;
  ;; I often need to toggle this under WSL1, because Windows EXEs that
  ;; use the console (e.g. adb.exe) will hang when emacs attempt
  ;; communicate with them over a pty [1]. On the other hand, regular
  ;; linux shell commands that use password prompts or other types of
  ;; interactive input will fail unless emacs uses a pty (i.e.
  ;; process-connection-type is t).
  ;;
  ;; [1]: https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Subprocess-hang.html
  (defun benv/toggle-pty-for-shell-commands ()
    (interactive)
    (if process-connection-type
        (setq process-connection-type nil)
      (setq process-connection-type t))
    (if process-connection-type
        (message "pty for shell commands enabled")
      (message "pty for shell commands disabled")))

  ;; Quick-and-dirty function to check if the command (process)
  ;; for the current buffer is still running.
  (defun benv/print-buffer-process-state ()
    (interactive)
    (if (get-buffer-process (buffer-name))
        (message "buffer process is RUNNING")
      (message "buffer process is nil")))

  (defun benv/shelldon-output-history ()
    "Run `shelldon-output-history` with `vertico-sort-function`
set to nil, so that the command history is display in the
exact order provided by shelldon, i.e. in descending order of
recency."
    (interactive)
    (let ((vertico-sort-function nil))
      (shelldon-output-history)))

  ;; Create keybinds to run commands in specific windows,
  ;; as identified by winum.

  (benv/create-winum-keybinds "x" #'shelldon)
  (benv/create-winum-keybinds "x l" #'shelldon-send-line-at-point)
  (benv/create-winum-keybinds "x r" #'shelldon-send-region)
  (benv/create-winum-keybinds "x h" #'benv/shelldon-output-history)

  :general

  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "b p" 'benv/print-buffer-process-state
   "t p" 'benv/toggle-pty-for-shell-commands)
  :init
  (evil-set-initial-state 'shell-mode 'normal))

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
  ;; Disable lispy default keybind M-J -> lispy-join,
  ;; because conflicts with my window-resizing keybinds
  ;; (M-H,M-J,M-K,M-L).
  (general-def
   :keymaps 'lispy-mode-map
   :states '(motion normal emacs)
   "M-J" nil)
  ;; Add some custom lispy key bindings, beyond the
  ;; ones provided by evil-collection in
  ;; ~/.emacs.d/elpa/evil-collection-20210401.1012/modes/lispy/evil-collection-lispy.el
  (let ((map evil-collection-lispy-mode-map-special))
    (lispy-define-key map "M-(" 'lispy-wrap-round)))

(use-package show-paren-mode
  :hook (emacs-lisp-mode . show-paren-mode))

;;----------------------------------------
;; ledger-mode
;;----------------------------------------

(use-package ledger-mode
  :mode ("\\.ledger'" . ledger-mode))

;;----------------------------------------
;; ztree
;;----------------------------------------

(use-package ztree
  :commands (ztree-diff ztree-dir))

;;----------------------------------------
;; email (mu4e)
;;
;; Setup in this section is based on:
;; https://www.djcbsoftware.nl/code/mu/mu4e/Gmail-configuration.html
;;----------------------------------------

;; Handy interactive interface to the `pass` program (a la Magit).
;;
;; Note: This package is completely optional, and the rest
;; of my email/password configuration (below) would not be
;; affected by removing/disabling it.
(use-package pass)

;; Tell emacs to retrieve all passwords (e.g. SMTP password
;; when sending email) using the `pass` command, using the
;; host name as argument.
;;
;; Source: https://www.reddit.com/r/emacs/comments/o4g7dv/comment/h2ilq3n/
(use-package auth-source-pass
  :init
  (auth-source-pass-enable))

(use-package smtpmail
  :config
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-debug-info t
        smtpmail-debug-verb t))

(use-package mu4e
  :load-path "~/share/emacs/site-lisp/mu4e"
  :general
  ('motion mu4e-view-mode-map
   :prefix benv/major-mode-leader-key
   "a" 'mu4e-view-mime-part-action)
  (:states '(motion normal emacs)
   :keymaps '(mu4e-main-mode-map mu4e-view-mode-map mu4e-headers-mode-map)
   ";" 'benv/mu4e-switch-context)
  :commands (mu4e)
  :config
  ;; Workaround: For some reason, the mu4e status buffer doesn't get
  ;; updated after I switch mu4e contexts (i.e. switch the active email
  ;; account). To workaround this problem, I just quit mu4e and
  ;; restart it after switching contexts.
  (defun benv/mu4e-switch-context ()
    (interactive)
    (mu4e-context-switch)
    (mu4e-quit)
    (mu4e))
  ;; select package for composing and sending emails
  (setq mail-user-agent 'mu4e-user-agent)
  ;; automatically run `mu4e-get-mail-command` every 10 minutes
  (setq mu4e-update-interval 600)
  ;; hack to avoid UID errors when using `mbsync`
  (setq mu4e-change-filenames-when-moving t)
  ;; don't save sent messages to Sent folder, Fastmail/Gmail does this for us
  (setq mu4e-sent-messages-behaviour 'delete)

  ;; Fix mu4e's non-standard/troublesome behaviour when
  ;; deleting messages.
  ;;
  ;; When executing delete marks, mu4e's default behaviour is to
  ;; perform two actions: (1) move the message to the trash folder,
  ;; and (2) set the IMAP deleted/trashed flag on the message. But it
  ;; should only do (1). Setting the deleted/trash flag causes
  ;; `mbsync` to follow up with an EXPUNGE command,
  ;; which then causes gmail/fastmail to immediately (and
  ;; permanently) delete the message from the trash folder.
  ;; See [1] for a more detailed discussion/description of the problem.
  ;;
  ;; Immediately deleting messages from the trash folder defeats
  ;; the whole purpose of having a trash folder. I want to
  ;; my deleted messages to stay in the trash for reasonable
  ;; length of time, so that I can recover any messages that
  ;; I deleted accidentally.
  ;;
  ;; In order to achieve that behaviour, we just have
  ;; to make sure that mu4e doesn't set the deleted/trashed
  ;; flag when moving messages to the trash folder, which is
  ;; exactly what the code below does. Btw, the code below
  ;; is based on [2] and [3], with some minor updates due to
  ;; changes in mu4e's function/variable names.
  ;;
  ;; [1]: https://github.com/djcb/mu/issues/1136
  ;; [2]: https://github.com/djcb/mu/issues/1136#issuecomment-486177435
  ;; [3]: https://github.com/djcb/mu/issues/1136#issuecomment-1066303788

  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg)
                            (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                        ;; Here's the main difference to the regular trash mark,
                        ;; no +T before -N so the message is not marked as
                        ;; IMAP-deleted:
                        (mu4e~proc-move docid (mu4e-get-trash-folder msg) "-N"))))

  ;; Settings when composing replies and new messages.

  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              ;; when typing text, don't automatically insert newlines at column 80
              (auto-fill-mode -1)
              ;; turn on word wrap
              (visual-line-mode)))

  ;; don't show confirmation prompt when quitting mu4e
  (setq mu4e-confirm-quit nil)
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "fastmail"
            :vars '(
                    ;; SMTP settings
                    (smtpmail-default-smtp-server . "smtp.fastmail.com")
                    (smtpmail-smtp-server  . "smtp.fastmail.com")
                    (smtpmail-smtp-user . "awesomesaucelabs@fastmail.com")
                    (smtpmail-stream-type . ssl)
                    (smtpmail-smtp-service . 465)

                    ;; mu4e folder settings
                    (mu4e-drafts-folder . "/Drafts")
                    (mu4e-sent-folder . "/Sent")
                    (mu4e-trash-folder . "/Trash")
                    (mu4e-maildir-shortcuts .
                          ((:maildir "/INBOX"  :key ?i)
                           (:maildir "/Sent"   :key ?s)
                           (:maildir "/Trash"  :key ?t)
                           (:maildir "/Drafts" :key ?d)))

                    (mu4e-bookmarks .
                          ((:name  "Inbox (last 7 days)"
                                   :query "maildir:/INBOX date:1w.."
                                   :key ?i)))

                    ;; Note: I override the default mu database location here
                    ;; ("~/.mu") because I have multiple email accounts with separate
                    ;; Maildirs, and `mu` requires a separate database for each.
                    (mu4e-mu-home . "~/.mu/fastmail")
                    (mu4e-maildir . "~/Maildir/fastmail")

                    ;; shell command to retrieve new mail
                    (mu4e-get-mail-command . "mbsync -V fastmail")

                    ;; reply-to address
                    (mu4e-reply-to-address . "awesomesaucelabs@fastmail.com")
                    (user-mail-address . "awesomesaucelabs@fastmail.com")
                    (user-full-name . "Ben Vandervalk")))
          ,(make-mu4e-context
            :name "awesomesaucelabs@gmail.com"
            :vars '(
                    ;; SMTP settings
                    (smtpmail-default-smtp-server . "smtp.gmail.com")
                    (smtpmail-smtp-server  . "smtp.gmail.com")
                    (smtpmail-smtp-user . "awesomesaucelabs@gmail.com")
                    (smtpmail-stream-type . ssl)
                    (smtpmail-smtp-service . 465)

                    ;; mu4e folder settings
                    (mu4e-drafts-folder . "/[Gmail]/Drafts")
                    (mu4e-sent-folder . "/[Gmail]/Sent Mail")
                    (mu4e-trash-folder . "/[Gmail]/Trash")
                    (mu4e-maildir-shortcuts .
                          ((:maildir "/INBOX"      :key ?i)
                           (:maildir "/[Gmail]/Sent Mail"  :key ?s)
                           (:maildir "/[Gmail]/Trash"      :key ?t)
                           (:maildir "/[Gmail]/Drafts"     :key ?d)))

                    (mu4e-bookmarks .
                          ((:name  "Inbox (last 7 days)"
                                   :query "maildir:/INBOX date:1w.."
                                   :key ?i)))

                    ;; Note: I override the default mu database location here
                    ;; ("~/.mu") because I have multiple email accounts with separate
                    ;; Maildirs, and `mu` requires a separate database for each.
                    (mu4e-mu-home . "~/.mu/awesomesaucelabs")
                    (mu4e-maildir . "~/Maildir/awesomesaucelabs")

                    ;; shell command to retrieve new mail
                    (mu4e-get-mail-command . "mbsync -V awesomesaucelabs")

                    ;; reply-to address
                    (mu4e-reply-to-address . "awesomesaucelabs@gmail.com")
                    (user-mail-address . "awesomesaucelabs@gmail.com")
                    (user-full-name . "Ben Vandervalk")))
          ,(make-mu4e-context
            :name "ben.vvalk@gmail.com"
            :vars '(
                    ;; SMTP settings
                    (smtpmail-default-smtp-server . "smtp.gmail.com")
                    (smtpmail-smtp-server  . "smtp.gmail.com")
                    (smtpmail-smtp-user . "ben.vvalk@gmail.com")
                    (smtpmail-stream-type . ssl)
                    (smtpmail-smtp-service . 465)

                    ;; mu4e folder settings
                    (mu4e-drafts-folder . "/[Gmail]/Drafts")
                    (mu4e-sent-folder . "/[Gmail]/Sent Mail")
                    (mu4e-trash-folder . "/[Gmail]/Trash")
                    (mu4e-maildir-shortcuts .
                          ((:maildir "/INBOX"      :key ?i)
                           (:maildir "/[Gmail]/Sent Mail"  :key ?s)
                           (:maildir "/[Gmail]/Trash"      :key ?t)
                           (:maildir "/[Gmail]/Drafts"     :key ?d)))

                    (mu4e-bookmarks .
                          ((:name  "Inbox (last 7 days)"
                                   :query "maildir:/INBOX date:1w.."
                                   :key ?i)))

                    ;; Note: I override the default mu database location here
                    ;; ("~/.mu") because I have multiple email accounts with separate
                    ;; Maildirs, and `mu` requires a separate database for each.
                    (mu4e-mu-home . "~/.mu/ben.vvalk")
                    (mu4e-maildir . "~/Maildir/ben.vvalk")

                    ;; shell command to retrieve new mail
                    (mu4e-get-mail-command . "mbsync -V ben.vvalk")

                    ;; reply-to address
                    (mu4e-reply-to-address . "ben.vvalk@gmail.com")
                    (user-mail-address . "ben.vvalk@gmail.com")
                    (user-full-name . "Ben Vandervalk"))))))

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
