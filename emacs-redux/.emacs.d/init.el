;;; -*- lexical-binding: t -*-

;;----------------------------------------
;; basic UI settings
;;----------------------------------------

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Fix jerky scrolling when scrolling with j/k.
;; Source: https://stackoverflow.com/a/1128948

(setq scroll-step            1
      scroll-conservatively  10000)

;; Fix jerky scrolling when scrolling with mouse wheel.
;; Source: https://stackoverflow.com/a/445881

(setq mouse-wheel-scroll-amount '(2 ((shift) . 2) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

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
                   ((string-match-p (regexp-quote "Mac") hostname) 170)
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
        ("nongnu" . "https://elpa.nongnu.org/nongnu/") ;; for `eat' package (`claude-code.el' dependency)
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
  (setq history-length 5000) ; default is 100, which is not enough!
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
  ;; set to nil, then `forward-sexp` can only get to the last character of
  ;; a line (e.g. ")"), rather than moving to the newline character
  ;; after it.  And that means that I need to handle sexp's that end
  ;; at line endings as a special case in my code, which I don't want
  ;; to do.
  ;;
  ;; For further explanation/discussion of this issue, see:
  ;; https://github.com/syl20bnr/spacemacs/issues/2525
  (evil-move-beyond-eol t)
  (evil-undo-system 'undo-redo)
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
  ;; For some reason, evil's default window swapping functions
  ;; "rebalance" the window sizes after swapping the target windows.
  ;; That doesn't work well for my usage of EXWM, because the windows
  ;; end up spanning my two monitors, as I noted with my
  ;; `evil-auto-balance-windows' setting below.
  (:states '(motion normal)
           "RET" nil
           "C-w H" #'windmove-swap-states-left
           "C-w J" #'windmove-swap-states-down
           "C-w K" #'windmove-swap-states-up
           "C-w L" #'windmove-swap-states-right)

  :config
  (evil-mode 1)
  (setq evil-move-cursor-back nil)

  ;; When splitting windows, don't adjust the window sizes to make
  ;; them all the same height/width. Just divide the current window in
  ;; half.
  ;;
  ;; This setting is important for me to be productive in EXWM,
  ;; because I use one large emacs frame that is split across two
  ;; monitors. If `evil-auto-balance-windows` is set true, the windows
  ;; get resized such that they straddle the gaps between my two
  ;; monitors, which is very distracting.
  ;;
  ;; The reason that I only use a single emacs frame is that EXWM has
  ;; an annoying design flaw, where X11 apps in other
  ;; frames/workspaces don't appear in the Emacs buffer list. By only
  ;; using a single emacs frame, I completely avoid that annoyance
  ;; and the overall experience is much better.
  (setq evil-auto-balance-windows nil)

  ;; Make window movement work across frames, as per:
  ;; https://github.com/doomemacs/doomemacs/issues/2577
  (use-package framemove
   :config
   (setq framemove-hook-into-windmove t))

  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Customize evil search highlighting behaviour.
  ;;
  ;; (1) Automatically disable search highlighting when
  ;; Enter key is pressed (to select the current match).
  ;; (2) Add advice to make ctrl-g (keyboard-quit) clear evil search
  ;; highlighting. (I also tried this using `:after` instead of
  ;; `:before`, but it didn't work for some reason.)
  ;;
  ;; Note: (2) is needed because search highlighting is reactivated
  ;; when pressing `n` or `N` to jump to the next/previous match
  ;; of the most recent interactive search.

  (setq evil-ex-search-persistent-highlight nil)

  (defun benv/keyboard-quit-advice (&rest args)
  	(when (and (boundp 'evil-mode) (eq evil-state 'normal))
  	  (evil-ex-nohighlight)))

  (advice-add 'keyboard-quit :before #'benv/keyboard-quit-advice))

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

(use-package evil-lion :config (evil-lion-mode))

;; Adds "il"/"al" text objects for current line.
(use-package evil-textobj-line)

;; Adds "ie"/"ae" text objects for entire buffer.
(use-package evil-textobj-entire
  :config
  ;; Note: The default value of `evil-textobj-entire-key' is "e".
  ;; As a result, the buffer text object gets bound to `ie' and `ae'.
  (define-key evil-outer-text-objects-map evil-textobj-entire-key 'evil-entire-entire-buffer)
  (define-key evil-inner-text-objects-map evil-textobj-entire-key 'evil-entire-entire-buffer))

;; Support for editing with multiple cursors.
;;
;; I also tried `evil-mc`, but for some reason it was insanely bugged
;; when I tried it with my emacs configuration on macOS (perhaps an
;; unexpected interaction with another emacs package). For example,
;; when I created multiple cursors and started typing characters in
;; Insert mode, only a subset of the characters were replicated at the
;; other cursor locations! No idea what's going on there, and I didn't
;; attempt to debug it.
;;
;; `evil-multiedit` (below) worked out-of-the-box for me, but the
;; functionality is much more basic than `evil-mc`. For example,
;; `evil-mc` allows you to manually place the cursors wherever you
;; want, whereas `evil-multiedit` can only place cursors by matching
;; the region or the word under the cursor.

(use-package evil-multiedit
  :config
  (setq evil-multiedit-follow-matches t)
  ;; Enable Ex command `:iedit REGEX' that allows you to invoke
  ;; evil-multiedit with a regular expression
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  :general
  (:states '(normal visual multiedit)
   "C-M-<down>"  'evil-multiedit-match-and-next
   "C-M-<up>"    'evil-multiedit-match-and-prev
   "RET" 		 nil))

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
  (setq text-scale-mode-step 1.05)
  :general
  ;; use the keybinds for zoom in/out as web browsers
  (:states '(motion insert emacs)
   "C-=" 'text-scale-adjust
   "C-+" 'text-scale-adjust
   "C--" 'text-scale-adjust))

;;----------------------------------------
;; file management and paths
;;----------------------------------------

(defun benv/windows-path-to-wsl-path (path)
  "Convert a Windows file path to an equivalent WSL file path."
  ;; replace backslashes with forward slashes
  (setq path (replace-regexp-in-string "\\\\" "/" path))
  (if (string-match "^\\(.*\\)\\([a-zA-Z]\\):\\(.*\\)" path)
      (let* ((uri-scheme (or (match-string 1 path) ""))
             (drive-letter (downcase (match-string 2 path)))
             (file-path (match-string 3 path))
             (wsl-path (concat "/mnt/" drive-letter file-path)))
        (if (string= "" uri-scheme)
            wsl-path
          (concat uri-scheme "mnt/" drive-letter file-path)))
    path))

;; Test: Example path taken from a compilation buffer for Windows
;; CMake build.

(benv/windows-path-to-wsl-path
 "D:\\git\\awesomesauce\\rabbit-plugin\\src\\PluginAPI.cpp")

(defun benv/wsl-path-to-windows-path (path)
  "Convert a WSL file path to an equivalent Windows file path."
  (if (string-match "^\\(.*\\)/mnt/\\([a-z]\\)\\(.*\\)" path)
      (let* ((uri-scheme (match-string 1 path))
             (drive-letter (upcase (match-string 2 path)))
             (file-path (match-string 3 path))
             (file-path-with-backslashes (replace-regexp-in-string "/" "\\\\" file-path))
             (windows-path (concat drive-letter ":" file-path-with-backslashes)))
        (if (string= "" uri-scheme)
            windows-path
          (concat uri-scheme "/" drive-letter "%3a" file-path)))
    path))

(defun benv/yank-wsl-path ()
  (interactive)
  (insert (benv/windows-path-to-wsl-path (current-kill 0))))

(defun benv/yank-windows-path ()
  (interactive)
  (insert (benv/wsl-path-to-windows-path (current-kill 0))))

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

(defun benv/yank-directory ()
  "Yank absolute file path for current buffer
and echo it in the minibuffer."
  (interactive)
  (kill-new default-directory)
  (message default-directory))

(use-package ffap
  :init
  ;; Makes `ffap-find-file-at-point' recognize Windows-style file
  ;; paths and automatically translate them Linux-style WSL paths.
  ;; In `evil-mode', this makes "g f" able to navigate to the Windows
  ;; file path under the cursor.
  ;;
  ;; Note: We need two advices here because `ffap-find-file-at-point'
  ;; parses the string at point with a different function, depending
  ;; on whether the string at point is quoted or unquoted.
  ;; `ffap-string-at-point' is used when the string is quoted and
  ;; `ffap-guesser' is used when the string is unquoted.
  (advice-add 'ffap-guesser :filter-return #'benv/windows-path-to-wsl-path)
  (advice-add 'ffap-string-at-point :filter-return #'benv/windows-path-to-wsl-path))

;;----------------------------------------
;; direnv
;;----------------------------------------

(use-package direnv
	:config
	(direnv-mode))

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
  "e r" 'eval-region
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

;; Note: The <next>/<prior> lines remap the PageDown/PageUp keys
;; to half-screen jumps, as is done by the default C-d/C-u bindings
;; in evil/vim.
(general-def
  :states '(motion normal insert emacs)
  "M-q" 'quit-window
  "<next>" 'evil-scroll-down
  "<prior>" 'evil-scroll-up)

;; restore standard vim mappings

(general-def 'motion
  "C-i" 'evil-jump-forward
  "C-u" 'evil-scroll-up)

;;----------------------------------------
;; whitespace-related
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
  (mode-require-final-newline nil)
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "t w" 'whitespace-mode))

;; Automatically delete trailing whitespace when saving a file, but
;; only on lines that were modified. This lets me automatically clean
;; up trailing whitespaces in my own work, while leaving other people's
;; code alone (e.g. Colin's code). Cleaning up whitespace errors
;; in other people's code is a bad idea, because it leads to unexpected
;; version control conflicts.

(use-package ws-butler
	:hook prog-mode)

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

  ;; Some extra code to make the vertico line-wrapping behaviour
  ;; configurable on a per-command basis.
  ;;
  ;; Based on: https://github.com/minad/vertico/issues/257#issuecomment-1194441344

  (defvar benv/vertico-truncate-lines t
    "Determines the value of `truncate-lines' in the minibuffer.

I use this variable to enable/disable wrapping of vertico completion
candidates across multiple lines. For example, I set
`benv/vertico-truncate-lines' to `nil' for the
`benv/shelldon-output-history' command because I often use very long
shell commands, and I often need to see full command to select the
right one.  On the other hand, the majority of completion lists become
very hard to read if the candidates are wrapped across multiple lines
(e.g. `describe-variable'), and so I set `benv/vertico-truncate-lines'
to `t' by default.

Note: This variable needs to be declared with `defvar', rather than
just set with `setq', so that it has dynamic binding behaviour rather
than lexical binding behaviour. For example, with lexical binding the
assignment of `nil' to `benv/vertico-truncate-lines' in
`benv/shelldon-output-history' has no effect on the value seen by the
advice function below (`benv/vertico-truncate-lines'). This caused me
some confusion for a while.")

  (advice-add #'vertico--resize-window :after #'benv/vertico-set-truncate)

  (defun benv/vertico-set-truncate (&rest _)
    (setq-local truncate-lines benv/vertico-truncate-lines))

  ;; Enable vertico for completions.
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

(use-package vertico-posframe
  :config

  ;; Set the frame to a fixed width (in characters).
  ;;
  ;; The default behaviour is to resize the width to
  ;; fit the content. However, that often causes the frame
  ;; to resize while scrolling through completion list,
  ;; which is really disconcerting.
  (setq vertico-posframe-width 90)

  (setq vertico-posframe-parameters
        '((left-fringe . 24)
          (right-fringe . 24)))

  (vertico-posframe-mode 1))

;;----------------------------------------
;; consult
;;----------------------------------------

(use-package consult
  :config
  (defun benv/grep-notes ()
    "Run grep on my notes."
    (interactive)
    (consult-ripgrep "~/Sync/notes"))
  ;; Disable "live previews" when selecting a new buffers.
  ;; I find it very distracting.
  (consult-customize consult-buffer :preview-key "M-.")
  :bind
  ("s-b" . consult-buffer)
  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "c r" 'consult-ripgrep
   "n g" 'benv/grep-notes
   "m m" 'consult-man
   "s s" 'consult-line))

;; The `consult-dir' package provides an extremely useful function
;; called `consult-dir', which behaves differently depending on
;; whether it is called from a regular buffer or from the minibuffer.
;;
;; In a regular buffer, `consult-dir' prompts for a recently visited
;; directory and then opens a `find-file' under that directory to open
;; a file.
;;
;; In the minibuffer, `consult-dir' prompts for a recently visited
;; directory and then replaces the currently minibuffer text with that
;; directory. This is extremely handy for quickly filling out
;; directory prompts (e.g. the destination directory of a dired copy
;; command).
(use-package consult-dir
  :config
  ;; Get root directories for projects using projectile
  ;; (as opposed to project.el, which is the default).
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs)
  ;; When selecting a directory for insertion into the minibuffer,
  ;; replace the existing text rather than "shadowing" it (the
  ;; default). Shadowing keeps the existing minibuffer text but
  ;; inactivates it and shows it in a light grey color. I find this
  ;; very ugly and awkward, and prefer to just replace the
  ;; existing text.
  (setq consult-dir-shadow-filenames nil)
  :general
  (:states '(emacs insert)
   :keymaps 'minibuffer-local-map
   "C-x C-d" #'consult-dir
   "C-x C-j" #'consult-dir-jump-file)
  (:states '(motion normal emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "c d" #'consult-dir))

;; Package that defines many additional `consult` commands
;; for searching Google, Wikipedia, etc.
(use-package consult-omni
  :config
  (use-package consult-omni-sources
    :load-path "~/.emacs.d/elpa/consult-omni/sources"
    :config
    (setq consult-omni-sources-modules-to-load '(consult-omni-google consult-omni-wikipedia))
    (setq consult-omni-dynamic-refresh-delay 5.0)
    (setq consult-omni-google-customsearch-key
          (lambda ()
            (auth-source-pass-get 'secret "developers.google.com/api-key")))
    (setq consult-omni-google-customsearch-cx
          (lambda ()
            (auth-source-pass-get 'secret "developers.google.com/cx-number")))
    (consult-omni-sources-load-modules)))

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
;; window.el (built-in)
;;----------------------------------------

(use-package emacs
  :demand t
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

  (defmacro benv/in-neighbor-window (dir body)
    `(let ((exists (window-in-direction ,dir))
           (current-window (selected-window)))
       ;; If a neighbor window in direction DIR does
       ;; not already exist, create one by splitting
       ;; the current window.
       (unless exists
         (split-window nil nil ,dir))
       ;; Switch focus to the neighbor window in direction DIR.
       (select-window (window-in-direction ,dir))
       ;; Execute BODY in neighbor window.
       ,body
       ;; If we created a new neighbor window,
       ;; configure the window state such that
       ;; calling `quit-window` will close the window and
       ;; return focus to the previously-focused window (i.e.
       ;; `current-window`, the window that we split to
       ;; create the neighbor window.)
       (unless exists
         ;; Clear the buffer history for the newly created neighbor window.
         ;; Otherwise, the buffer for the previously selected window
         ;; (i.e. the window that we split to create the neighbor
         ;; window) will be in the buffer history, and this will prevent
         ;; emacs from deleting the window when `quit-window` is called.
         (set-window-prev-buffers (selected-window) nil)
         ;; Set up the `quit-restore` window parameter, which tells
         ;; emacs how to behave when `quit-window` is called. In our
         ;; case we want emacs to delete the neighbor window and
         ;; set the focus back to the previously selected window.
         (set-window-parameter (selected-window)
                               'quit-restore
                               (list
                                'window
                                'window
                                current-window
                                (current-buffer))))))

  (defmacro benv/with-neighbor-window (dir body)
    "Temporarily select neighbor window in direction DIR
and evaluate BODY. Possible values of dir are 'left',
'right', 'up', 'down'.

If there is no neigbor window in direction DIR, create
it by splitting the current window."
    `(progn
       (unless (window-in-direction ,dir)
         (split-window nil nil ,dir))
       (save-selected-window
         (select-window (window-in-direction ,dir))
         ,body)))

  (defun benv/mirror-window (dir)
    "Make the neighbour window in direction DIR a mirror of
the current window. In other words, make the selected buffer,
cursor position, and window scroll position of the neighbour
window identical to the current window.

If a neighbour window does not already exist in direction DIR,
this function will split the current window."
    (let ((buffer (current-buffer))
          (start (window-start)))
      (benv/with-neighbor-window
       dir
       (progn (switch-to-buffer buffer)
              (set-window-start (selected-window) start)))))

  (defun benv/mirror-window-in-dir-and-focus (dir)
    (let ((buffer (current-buffer))
          (start (window-start)))
      (benv/in-neighbor-window
       dir
       (progn (switch-to-buffer buffer)
              (set-window-start (selected-window) start)))))

  (defun benv/switch-to-buffer-in-dir (dir)
    "Select a buffer interactively and open it in the
neighbour window in direction DIR, without changing
the currently selected window.

If a neighbour window does not already exist in direction DIR,
this function will create one by splitting the current
window."
    (when-let ((buffer (read-buffer "buffer: ")))
      (benv/with-neighbor-window dir (switch-to-buffer buffer))))

  (defun benv/switch-to-buffer-in-dir-and-focus (dir)
    "Select a buffer interactively, then select the
neighbour window in direction DIR and open the buffer
there.

If a neighbour window does not already exist in direction DIR,
this function will create one by splitting the current
window."
    (when-let ((buffer (read-buffer "buffer: ")))
      (benv/in-neighbor-window dir (switch-to-buffer buffer))))

  (defun benv/evil-goto-mark-in-dir (dir)
    "Read a single character for an evil mark, then
jump to that mark in the neighbour window in direction DIR,
without changing the current window focus.

If a neighbour window does not already exist in direction DIR,
this function will create one by splitting the current
window."
    (let ((char (read-char)))
      (if (evil-get-marker char)
          (benv/with-neighbor-window dir (evil-goto-mark char))
        (message "No mark set for %c" char))))

  (defun benv/evil-goto-mark-in-dir-and-focus (dir)
    "Read a single character for an evil mark, then switch
focus to the neighbour window in direction DIR and jump to
the selected mark.

If a neighbour window does not already exist in direction DIR,
this function will create one by splitting the current
window."
    (let ((char (read-char)))
      (if (evil-get-marker char)
          (benv/in-neighbor-window dir (evil-goto-mark char))
        (message "No mark set for %c" char))))

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
   "f y" 'benv/yank-filename
   "d y" 'benv/yank-directory
   "w h" (lambda () (interactive) (benv/mirror-window 'left))
   "w j" (lambda () (interactive) (benv/mirror-window 'down))
   "w k" (lambda () (interactive) (benv/mirror-window 'up))
   "w l" (lambda () (interactive) (benv/mirror-window 'right))
   "w H" (lambda () (interactive) (benv/mirror-window-in-dir-and-focus 'left))
   "w J" (lambda () (interactive) (benv/mirror-window-in-dir-and-focus 'down))
   "w K" (lambda () (interactive) (benv/mirror-window-in-dir-and-focus 'up))
   "w L" (lambda () (interactive) (benv/mirror-window-in-dir-and-focus 'right))
   "' h" (lambda () (interactive) (benv/evil-goto-mark-in-dir 'left))
   "' j" (lambda () (interactive) (benv/evil-goto-mark-in-dir 'down))
   "' k" (lambda () (interactive) (benv/evil-goto-mark-in-dir 'up))
   "' l" (lambda () (interactive) (benv/evil-goto-mark-in-dir 'right))
   "' H" (lambda () (interactive) (benv/evil-goto-mark-in-dir-and-focus 'left))
   "' J" (lambda () (interactive) (benv/evil-goto-mark-in-dir-and-focus 'down))
   "' K" (lambda () (interactive) (benv/evil-goto-mark-in-dir-and-focus 'up))
   "' L" (lambda () (interactive) (benv/evil-goto-mark-in-dir-and-focus 'right))
   "w M" 'delete-other-windows
   "w o" 'delete-other-windows
   "w s" 'split-window-below
   "w v" 'split-window-right
   "y f" 'benv/yank-filename
   "y d" 'benv/yank-directory
   )
  :bind
  ("M-J" . benv/enlarge-window)
  ("M-K" . benv/shrink-window)
  ("M-H" . benv/shrink-window-horizontally)
  ("M-L" . benv/enlarge-window-horizontally)
  ("s-j" . evil-window-left)
  ("s-k" . evil-window-down)
  ("s-l" . evil-window-up)
  ("s-;" . evil-window-right)
  ("s-J" . windmove-swap-states-left)
  ("s-K" . windmove-swap-states-down)
  ("s-L" . windmove-swap-states-up)
  ("s-:" . windmove-swap-states-right)
  ("s-u" . winner-undo)
  ("s-U" . winner-redo)
  ("s-d" . delete-window)
  ("s-o" . delete-other-windows)
  ("s-v" . evil-window-vsplit)
  ("s-s" . evil-window-split))

(use-package general
  :ensure t
  :config
  ;; Store buffers in a vector
  (defvar my-buffer-shortcuts (make-vector 10 nil)
    "Vector storing buffer assignments for quick access.")

  ;; Assign current buffer to a number
  (defun my-assign-buffer-to-number (num)
    "Assign current buffer to key NUM (0-9)."
    (interactive "nAssign to number (0-9): ")
    (when (and (>= num 0) (< num 10))
      (aset my-buffer-shortcuts num (current-buffer))
      (message "Buffer assigned to %d" num)))

  ;; Switch to a buffer assigned to a number
  (defun my-switch-to-buffer-number (num)
    "Switch to buffer assigned to key NUM (0-9)."
    (interactive "nSwitch to number (0-9): ")
    (let ((buf (and (>= num 0) (< num 10) (aref my-buffer-shortcuts num))))
      (if buf
          (switch-to-buffer buf)
        (message "No buffer assigned to %d" num))))

  ;; Set up keybindings for all three states
  (general-define-key
   :states '(motion insert emacs)
   :keymaps 'override

   ;; Assign buffer to number keys (C-s-0 through C-s-9)
   "C-s-0" (lambda () (interactive) (my-assign-buffer-to-number 0))
   "C-s-1" (lambda () (interactive) (my-assign-buffer-to-number 1))
   "C-s-2" (lambda () (interactive) (my-assign-buffer-to-number 2))
   "C-s-3" (lambda () (interactive) (my-assign-buffer-to-number 3))
   "C-s-4" (lambda () (interactive) (my-assign-buffer-to-number 4))
   "C-s-5" (lambda () (interactive) (my-assign-buffer-to-number 5))
   "C-s-6" (lambda () (interactive) (my-assign-buffer-to-number 6))
   "C-s-7" (lambda () (interactive) (my-assign-buffer-to-number 7))
   "C-s-8" (lambda () (interactive) (my-assign-buffer-to-number 8))
   "C-s-9" (lambda () (interactive) (my-assign-buffer-to-number 9))

   ;; Switch to buffer by number (s-0 through s-9)
   "s-0" (lambda () (interactive) (my-switch-to-buffer-number 0))
   "s-1" (lambda () (interactive) (my-switch-to-buffer-number 1))
   "s-2" (lambda () (interactive) (my-switch-to-buffer-number 2))
   "s-3" (lambda () (interactive) (my-switch-to-buffer-number 3))
   "s-4" (lambda () (interactive) (my-switch-to-buffer-number 4))
   "s-5" (lambda () (interactive) (my-switch-to-buffer-number 5))
   "s-6" (lambda () (interactive) (my-switch-to-buffer-number 6))
   "s-7" (lambda () (interactive) (my-switch-to-buffer-number 7))
   "s-8" (lambda () (interactive) (my-switch-to-buffer-number 8))
   "s-9" (lambda () (interactive) (my-switch-to-buffer-number 9))))

;;----------------------------------------
;; which-window.el: prompt user for target
;; window when emacs wants to display a
;; buffer (my own package)
;;----------------------------------------

(use-package which-window
  :disabled
  :load-path "~/.emacs.d/site-lisp/which-window"
  :config
  (which-window-mode 1))

;;----------------------------------------
;; compilation mode
;;----------------------------------------

(defun benv/compilation-find-file-advice (orig-fun marker filename directory &rest formats)
  "Advice to make built-in `compilation-find-file' function translate
Windows-style file paths to Linux-style WSL file paths, when running
Emacs under WSL.

I need this because I often run Windows builds (e.g. `cmake.exe') from
an Emacs instance that is running under WSL. Unless I add my own
path translation, clicking on file paths in a compilation buffer will
not automatically jump to the associated location in the source code
file."
  (setq filename (benv/windows-path-to-wsl-path filename))
  (apply orig-fun marker filename directory formats))

(use-package compilation-mode

  :init

  (defun benv/compilation-mode-setup ()
    ;; Turn on "word wrap" by default, so that I can
    ;; see the full text of long commands and error
    ;; messages.
    (visual-line-mode)
    ;; Automatically scroll to the bottom of the buffer
    ;; as new output appears. In other words, follow the
    ;; output like the Unix `tail` command.
    (setq compilation-scroll-output t)
    ;; By default, emacs automatically collapses long lines and shows
    ;; an ellipsis button/link ("[...]")  to expand them. Disable
    ;; that, so that I can actually see the damn output without
    ;; clicking a button.
    (setq compilation-max-output-line-length nil))

  (add-hook #'compilation-mode-hook #'benv/compilation-mode-setup)

  ;; Enable ANSI color processing in compilation buffers
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

  ;; Advice to make Windows-style file paths clickable in
  ;; `compilation-mode' buffers.
  (advice-add 'compilation-find-file :around #'benv/compilation-find-file-advice)

  :general

  ;; Unset default "SPC" binding
  ;; so that my evil leader key
  ;; behaves normally in dired
  (:keymaps 'compilation-mode-map "SPC" nil)
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "c c" 'compile)
  (:states '(motion emacs)
   :keymaps 'compilation-mode-map
   "RET" 'compilation-display-error
   "C-RET" 'compile-goto-error)

  :bind
  ("s-c" . project-compile))

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
  ;; Enable org habit-tracker module:
  ;; https://orgmode.org/manual/Tracking-your-habits.html
  ;;
  ;; Always show habit consistency graph in org-agenda,
  ;; but only for the current day.
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-all-today t)
  (setq org-habit-show-habits-only-for-today t)
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
  ;; Display inline images in org buffers by default.
  (setq org-startup-with-inline-images t)
  ;; Displaying large images in org-mode buffers makes scrolling
  ;; really jumpy and disorienting. This setting tells org-mode to
  ;; scale the image size up or down as needed, so that the resulting
  ;; width is 400 pixels.
  (setq org-image-actual-width 400)
  (setq org-agenda-files '("~/Sync/notes/habits.org"))
  ;; increase size of images used to preview LaTeX fragments
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.75))
  ;; preview all latex fragments in an .org file by default
  (setq org-startup-with-latex-preview t)
  (setq org-capture-templates
        '(("i" "inbox" entry (file "~/Sync/notes/20210325174000_inbox.org")
           "* TODO %?\n%i" :prepend t)
          ("b" "bugpile todo" entry (file "/home/benv/Sync/notes/20250220160301-bugpile_todo.org")
           "* TODO %?\n%i" :prepend t)
          ("r" "rabbit todo" entry (file "/home/benv/Sync/notes/20220808131306-rabbit_todo.org")
           "* TODO %?\n%i" :prepend t)
          ("w" "workflow" entry (file "~/Sync/notes/20210325103700_workflow_todo.org")
           "* TODO %?\n%i" :prepend t)))
  ;; Prevents org-mode from inserting unwanted indentation every time
  ;; I insert a line with 'o' or 'O' in evil-mode.
  ;; See: https://github.com/syl20bnr/spacemacs/issues/13255
  (setq org-src-preserve-indentation t)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  ;; plantuml
  (setq org-plantuml-exec-mode 'plantuml)
  ;; org-babel stuff
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (plantuml . t)
     (shell . t)))

  (defun benv/org-attach-screenshot-and-insert-link ()
    (interactive)
    (unless (derived-mode-p 'org-mode)
      (user-error "this command only works in org-mode"))
    (when-let* ((dest-dir (org-attach-dir t))
                (windows-tmp-dir "/mnt/c/tmp")
                (basename (read-string "screenshot filename: "))
                (windows-tmp-path (format "%s/%s" windows-tmp-dir basename))
                (dest-path (format "%s/%s" dest-dir basename)))
      (make-directory windows-tmp-dir t)
      (shell-command
       (format "i_view64.exe /capture=4 /convert='c:\\tmp\\%s'" basename))
      (copy-file windows-tmp-path dest-path)
      (insert (format "[[file:%s]]" dest-path)))
      (org-redisplay-inline-images))

  (defun benv/org-open-attach-directory ()
    "Open the org-attach directory for the current org heading in
dired, in the current window. Create a new attachment directory if it
doesn't already exist."
    (interactive)
    (when-let* ((attach-dir (org-attach-dir t)))
      (dired attach-dir)))

  (defun benv/org-open-attach-directory-in-dir (dir)
    "Open the org-attach directory for the current org heading in
dired, in a neighbor window. DIR indicates the direction of the target
neighbor window.

If the current org heading does not have an attachment directory, a
new directory will be created. If the target neighbor window doesn't
exist, the current window will be split to create it."
    (when-let* ((attach-dir (org-attach-dir t)))
      (benv/in-neighbor-window dir (dired attach-dir))))

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
   "a ." 'benv/org-open-attach-directory
   "a h" (lambda () (interactive) (benv/org-open-attach-directory-in-dir 'left))
   "a j" (lambda () (interactive) (benv/org-open-attach-directory-in-dir 'down))
   "a k" (lambda () (interactive) (benv/org-open-attach-directory-in-dir 'up))
   "a l" (lambda () (interactive) (benv/org-open-attach-directory-in-dir 'right))
   "t i" 'org-toggle-inline-images
   "t l" 'org-toggle-link-display
   "t L" 'org-latex-preview
   "T i c" 'org-table-insert-column
   "T d c" 'org-table-delete-column
   "h i" 'org-insert-heading-after-current
   "I"   'org-clock-in
   "O"   'org-clock-out
   "p"   'org-cliplink))

(use-package orgit
  :disabled
  :config
  (defun benv/replace-orgit-rev-link-description (&rest args)
    "Replace the description of the most recently stored org link, if the
link is of type `orgit-rev'."
    (when-let* ((link-desc-pair (car org-stored-links))
                (link (car link-desc-pair))
                (link-parts (split-string link ":"))
                (link-type (nth 0 link-parts))
                (git-repo-path (expand-file-name (nth 1 link-parts)))
                (git-commit-hash (nth 3 link-parts)))
      (when (string= link-type "orgit-rev")
        (let ((new-desc (shell-command-to-string
                         (format "git --git-dir='%s/.git' log -n1 --format='format:%%h %%s' %s"
                                 git-repo-path
                                 git-commit-hash))))
          (setf (cadar org-stored-links) new-desc)))))

  ;; Use advice to generate nicer default link descriptions for
  ;; `orgit-rev' links.  The default link description consists of
  ;; the repo path and the commit hash. My custom description is the
  ;; commit hash and the subject line of the commit message, which is
  ;; much more informative.
  (advice-add 'org-store-link :after #'benv/replace-orgit-rev-link-description)
  (advice-add 'orgit-store-link :after #'benv/replace-orgit-rev-link-description))

;;----------------------------------------
;; org-roam
;;----------------------------------------

(when (file-directory-p "~/Sync/notes")
  (use-package org-roam
    :load-path "~/.emacs.d/site-lisp/org-roam"
    :hook (after-init . org-roam-mode)
    :config

    (defun benv/completing-read-notes ()
      "Select a note by its title using `completing-read`, then
return the file path for the note."
      (when-let* ((completions (org-roam--get-title-path-completions))
                  (title-with-tags (org-roam-completion--completing-read "Note: " completions))
                  (res (cdr (assoc title-with-tags completions)))
                  (file-path (plist-get res :path)))
        file-path))

    (defun benv/switch-to-note-in-dir (dir)
      "Select a note by its title using `completing-read`, then
open the note in the neighbor window in direction DIR ('left',
'right', 'down', 'up'), without changing the currently selected
window.

If a neighbor window doesn't already exist in direction DIR,
create it by splitting the current window."
      (when-let ((note (benv/completing-read-notes)))
        (benv/with-neighbor-window dir (find-file note))))

    (defun benv/switch-to-note-in-dir-and-focus (dir)
      "Select a note by its title using `completing-read`,
switch focus to the neighbour window in direction DIR
('left', 'right', 'down', or 'up'), and open the note.

If a neighbor window doesn't already exist in direction DIR,
create it by splitting the current window."
      (when-let ((note (benv/completing-read-notes)))
        (benv/in-neighbor-window dir (find-file note))))

    :general
    ('motion org-mode-map
             :prefix benv/major-mode-leader-key
             "r b" 'org-roam
             "r i" 'org-roam-insert)
    ('motion
     :prefix benv/evil-leader-key
     "n h" (lambda () (interactive) (benv/switch-to-note-in-dir 'left))
     "n j" (lambda () (interactive) (benv/switch-to-note-in-dir 'down))
     "n k" (lambda () (interactive) (benv/switch-to-note-in-dir 'up))
     "n l" (lambda () (interactive) (benv/switch-to-note-in-dir 'right))
     "n H" (lambda () (interactive) (benv/switch-to-note-in-dir-and-focus 'left))
     "n J" (lambda () (interactive) (benv/switch-to-note-in-dir-and-focus 'down))
     "n K" (lambda () (interactive) (benv/switch-to-note-in-dir-and-focus 'up))
     "n L" (lambda () (interactive) (benv/switch-to-note-in-dir-and-focus 'right))
     "n ." 'org-roam-find-file
     "r f" 'org-roam-find-file
     "r g" 'benv/grep-notes)
    (:states '(motion normal insert emacs)
     "s-n" 'org-roam-find-file
     "C-c r i" 'org-roam-insert)
    :config
    (setq org-roam-directory "~/Sync/notes")
    (org-roam-mode)))

;;----------------------------------------
;; markdown-mode
;;----------------------------------------

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config (setq markdown-fontify-code-blocks-natively t)
  :hook (markdown-mode . visual-line-mode))

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

  (defun benv/recentf-in-dir (dir)
    "Select a file through recentf and open it in the
neighbour window in direction DIR, without changing
the currently selected window.

If a neighbour window does not already exist in direction DIR,
this function will create one by splitting the current
window."
    (when-let ((file (completing-read "recent file: " recentf-list)))
      (unless (window-in-direction dir)
        (split-window nil nil dir))
      (save-selected-window (select-window (window-in-direction dir))
                            (find-file file))))

  (defun benv/recentf-in-dir-and-focus (dir)
    "Select a file through recentf, then select the
neighbour window in direction DIR and open it.

If a neighbour window does not already exist in direction DIR,
this function will create one by splitting the current
window."
    (when-let ((file (completing-read "recent file: " recentf-list)))
      (unless (window-in-direction dir)
        (split-window nil nil dir))
      (select-window (window-in-direction dir))
      (find-file file)))

  :general
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "f r h" (lambda () (interactive) (benv/recentf-in-dir 'left))
   "f r j" (lambda () (interactive) (benv/recentf-in-dir 'down))
   "f r k" (lambda () (interactive) (benv/recentf-in-dir 'up))
   "f r l" (lambda () (interactive) (benv/recentf-in-dir 'right))
   "f r H" (lambda () (interactive) (benv/recentf-in-dir-and-focus 'left))
   "f r J" (lambda () (interactive) (benv/recentf-in-dir-and-focus 'down))
   "f r K" (lambda () (interactive) (benv/recentf-in-dir-and-focus 'up))
   "f r L" (lambda () (interactive) (benv/recentf-in-dir-and-focus 'right))
   "f r ." 'benv/recentf))

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
   "d r ." 'benv/recentd
   "d r h" (lambda () (interactive) (benv/recentd-in-neighbor-window 'left))
   "d r j" (lambda () (interactive) (benv/recentd-in-neighbor-window 'down))
   "d r k" (lambda () (interactive) (benv/recentd-in-neighbor-window 'up))
   "d r l" (lambda () (interactive) (benv/recentd-in-neighbor-window 'right))
   "d r H" (lambda () (interactive) (benv/recentd-in-neighbor-window-and-focus 'left))
   "d r J" (lambda () (interactive) (benv/recentd-in-neighbor-window-and-focus 'down))
   "d r K" (lambda () (interactive) (benv/recentd-in-neighbor-window-and-focus 'up))
   "d r L" (lambda () (interactive) (benv/recentd-in-neighbor-window-and-focus 'right))
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

  (defun benv/recentd-in-neighbor-window (direction)
    (when-let ((recentd (completing-read "Directory: " (benv/recentd-list))))
      (benv/with-neighbor-window direction (dired recentd))))

  (defun benv/recentd-in-neighbor-window-and-focus (direction)
    (when-let ((recentd (completing-read "Directory: " (benv/recentd-list))))
      (benv/in-neighbor-window direction (dired recentd))))

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

(use-package find-dired
  :config
  (setq find-name-arg "-iname"))

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

(use-package magit-section
  :general
  (:keymaps 'magit-section-mode-map
   :states '(motion insert emacs)
           ;; Make sure that evil doesn't interfere with using
           ;; TAB key to toggle sections in `magit-section-mode'.
           ;; While developing my `difftool.el', I observed that
           ;; tab was being mapped to `evil-jump-forward'.
           "TAB" #'magit-section-toggle))

(use-package magit

  :custom
  (magit-copy-revision-abbreviated t)

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
    (:states '(motion insert emacs)
     "s-m" 'magit-status)
  :config
    (defun benv/magit-log-head ()
      (interactive)
      (magit-log-head))
    ;; display magit status buffer in currently
    ;; selected window (not the "other" window)
    (setq magit-display-buffer-function
          'magit-display-buffer-same-window-except-diff-v1)
    (setq magit-log-margin '(nil "%b-%d-%y" magit-log-margin-width t 18))
    (setq magit-diff-refine-hunk t)
    ;; hide windows line endings ("^M") in magit status buffer
    (add-hook 'magit-status-mode-hook 'remove-dos-eol)
    (add-hook 'magit-diff-mode-hook 'remove-dos-eol)
    (use-package orgit))

(use-package forge
  :after magit)

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
   "p g L" 'benv/projectile-magit-log-all)
  (:states '(motion insert emacs)
   "s-M" 'benv/projectile-magit-status))

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

(use-package avy)

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
  (switch-to-buffer nil))

(general-def
  :states '(motion insert emacs)
  :prefix benv/evil-leader-key
  :non-normal-prefix benv/evil-insert-mode-leader-key
  "TAB" 'benv/switch-to-previous-buffer)

(general-def
  :states '(motion normal insert emacs)
  "s-<tab>" 'benv/switch-to-previous-buffer)

;;----------------------------------------
;; javascript
;;----------------------------------------

;; Unity-specific extensions for javascript
;; source files

(use-package js-mode
  :mode (("\\.jspre\\'" . js-mode)
         ("\\.jslib\\'" . js-mode))
  :custom
  (js-indent-level 2))

;;----------------------------------------
;; python
;;----------------------------------------

(use-package dape
	:config
    (add-to-list 'dape-configs
                 `(debugpy-remote-attach
                   modes (python-mode python-ts-mode)
                   host (lambda () (read-string "Host: " "localhost"))
                   port (lambda () (read-number "Port: "))
                   :request "attach"
                   :type "python"
                   :justMyCode nil
                   :showReturnValue t)))

;;----------------------------------------
;; dtrt: automatically detect indentation settings from source file
;; (indent offset and indent-tabs-mode)
;;----------------------------------------

(use-package dtrt-indent
  :demand t
  :config
  (dtrt-indent-global-mode))

;;----------------------------------------
;; C#
;;----------------------------------------

(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-mode)
  :init
  ;; Tell `csharp-mode` not to automatically add a newline to the end
  ;; of my source files. It's not necessary and it keeps cluttering up
  ;; my git unstaged changes.
  (defun benv/csharp-mode-hook ()
    (setq require-final-newline nil))
  (add-hook 'csharp-mode-hook #'benv/csharp-mode-hook))

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
  ;; Set default indentation style for C/C++ code.
  ;; "stroustrup" mostly works for me, although I
  ;; did not spend much time trying the different
  ;; built-in options.
  (setq c-default-style "stroustrup")

  ;; Tell `c-mode`/`c++-mode` not to automatically add a newline to
  ;; the end of my source files. It's not necessary and it keeps
  ;; cluttering up my git unstaged changes.
  (defun benv/cc-mode-hook ()
    (setq require-final-newline nil))
  (add-hook 'c-mode-common-hook #'benv/cc-mode-hook))

(use-package eglot
  :hook (eglot-mode . company-mode)
  :config
  ;; Tell eldoc not to resize the minibuffer area.
  ;;
  ;; Eldoc automatically shows the documentation for the
  ;; method/variable under the cursor, which is very helpful. However,
  ;; if the documentation has multiple lines, it automatically resizes
  ;; the minibuffer area, which is very distracting.
  (setq eldoc-echo-area-use-multiline-p nil)

  (add-to-list 'eglot-server-programs '(c++-mode "clangd.exe"))
  (add-to-list 'eglot-server-programs '(c-mode "clangd.exe"))

  (defun benv/wsl-file-uri-to-windows (file-uri)
    (if (string-match (rx "file:///mnt/"
                          (group (char "a-z"))
                          (group (zero-or-more "/" (regex ".*"))))
                      file-uri)
        (let ((drive-letter (match-string 1 file-uri))
              (rest (match-string 2 file-uri)))
          (concat "file:///" drive-letter ":" rest))
      file-uri))

  ;; tests
  (benv/wsl-file-uri-to-windows "file:///mnt/d/git/awesomesauce/rabbit/")
  (benv/wsl-file-uri-to-windows "file:///mnt/d")

  (defun benv/eglot--path-to-uri-advice (orig-fun path)
    (let ((orig-uri (funcall orig-fun path)))
      (benv/wsl-file-uri-to-windows orig-uri)))

  (advice-add 'eglot--path-to-uri :around #'benv/eglot--path-to-uri-advice)

  (defun benv/windows-file-uri-to-wsl (file-uri)
    (if (string-match (rx "file:///"
                          (group (char "a-zA-Z"))
                          ":"
                          (group (regex ".*")))
                      file-uri)
        (let* ((drive-letter (downcase (match-string 1 file-uri)))
               (rest (match-string 2 file-uri))
               (rest-with-forward-slashes (replace-regexp-in-string (regexp-quote "\\") "/" rest)))
          (concat "file:///mnt/" drive-letter rest-with-forward-slashes))
      file-uri))

  ;; tests
  (benv/windows-file-uri-to-wsl "file:///d:\\git\\awesomesauce\\rabbit\\Assets\\Rabbit\\Scripts\\UnityWebRequestUtil.cs")

  (defun benv/eglot--uri-to-path-advice (orig-fun uri)
    (let ((wsl-uri (benv/windows-file-uri-to-wsl uri)))
      (funcall orig-fun wsl-uri)))

  (benv/eglot--uri-to-path-advice #'eglot--uri-to-path "file:///d:\\git\\awesomesauce\\rabbit\\Assets\\Rabbit\\Scripts\\UnityWebRequestUtil.cs")

  (advice-add 'eglot--uri-to-path :around #'benv/eglot--uri-to-path-advice))

(use-package xref
  :init
  (add-to-list 'display-buffer-alist
               '("\\*xref\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.3))))

(use-package flymake
  :init
  (add-to-list 'display-buffer-alist
               '("\\*Flymake"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.3))))

(use-package cmake-mode
  :config
  (setq cmake-tab-width 4))

;;----------------------------------------
;; gptel (integration for OpenAI and
;; other LLMs)
;;----------------------------------------

(use-package gptel
  :config
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda () (auth-source-pass-get 'secret "console.anthropic.com/awesomesaucelabs@gmail.com/api-key")))
  :general
  (:states '(motion normal insert emacs)
   "M-g" #'gptel-menu))

;;----------------------------------------
;; agent-shell (ACP client for emacs)
;;----------------------------------------

(use-package agent-shell
  :config
  ;; Use a minimal style for the banner line that appears at the top
  ;; of `agent-shell' buffers.
  ;;
  ;; The default style is `graphical', but that renders very weirdly
  ;; on my Framework laptop -- the Anthropic logo and the font are
  ;; both huge, which makes the banner take up about 15% of my screen
  ;; height.
  (setq agent-shell-header-style 'text)

  ;; Disable the welcome message with the huge "Claude Code" logo
  ;; ASCII art.
  (setq agent-shell-show-welcome-message nil)

  ;; Don't show AI company logos in the completion list, when starting
  ;; a new agent shell with `M-x agent-shell'. The logo seem to cause
  ;; lag on my Framework laptop.
  ;;
  ;; There is a bug that causes "nilnil" to be prepended to each
  ;; completion candidate, but it's still better than the lag.
  (setq agent-shell-show-config-icons nil)

  ;; Configure authentication for Claude Code.
  (setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication
       :api-key (lambda () (auth-source-pass-get 'secret "console.anthropic.com/awesomesaucelabs@gmail.com/api-key")))))

;;----------------------------------------
;; Claude Code integration
;;----------------------------------------

(use-package claude-code
  :load-path "~/.emacs.d/site-lisp/claude-code/"
  :config
  ;; Note: `claude-code.el' requires the `eat'
  ;; terminal emulator to function.
  (use-package eat)
  (claude-code-mode))

;;----------------------------------------
;; treesit
;;
;; The built-in tree sitter package that
;; comes with emacs 29 and newer.
;;
;; I used the following article as a
;; reference for setting it up:
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;;----------------------------------------

(use-package treesit
  :config
  ;; How to add treesitter support for a new language:
  ;;
  ;; (1) Add the git repo for the grammar to
  ;; `treesit-language-source-alist' (below).
  ;; (2) Compile and install the shared library for the grammar, by
  ;; invoking `treesit-install-language-grammar' and selecting the
  ;; target language (e.g. "cpp").
  ;;
  ;; In order to setup emacs on a new machine, I will need to rerun
  ;; step (2) for each language.
  ;;
  ;; For detailed instructions, see:
  ;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
  (setq treesit-language-source-alist
        '((c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (diff . ("https://github.com/the-mikedavis/tree-sitter-diff.git")))))

(use-package evil-textobj-tree-sitter
  :config
  (define-key evil-outer-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.inner")))

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
      (or (executable-find "/Applications/Firefox.app/Contents/MacOS/firefox")
          (executable-find "vieb.exe")
          (executable-find "firefox.exe")
          (executable-find "firefox")))

(setq browse-url-browser-function 'browse-url-generic)

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
    (let* (;; Set `benv/vertico-truncate-lines' to `nil' to show
           ;; the full string for each candidate shell command, wrapping
           ;; across multiple lines if necessary. This variable
           ;; works in conjunction with an advice defined in the
           ;; `use-package' declaration for `vertico' (above).
           (benv/vertico-truncate-lines nil)
           (vertico-sort-function nil)
           (command (completing-read "cmd: " shell-command-history)))
      (insert command)))

  (defun benv/filter-region-or-buffer-with-shell-command (shell-command)
    "Run a shell command, feeding the selected region to standard input (STDIN).
If no region is currently selected, send the entire contents of the current buffer
to STDIN instead.

Display the output in a new buffer, in the current window."
    (interactive
     (list (read-shell-command "Shell command: ")))
    (let (;; Don't allow `shell-command-on-region' to grow the
          ;; size of the minibuffer in order to show the output.
          (resize-mini-windows nil)
          ;; Display the output buffer in the current window.
          ;;
          ;; `shell-command-on-region' calls `display-buffer' on the
          ;; output buffer, but the default behaviour is pop up the buffer
          ;; in a new window. IMO, replacing the buffer in the current
          ;; window is a much better workflow.
          (display-buffer-alist '((".*" display-buffer-same-window)))
          (buffer (current-buffer))
          (output-buffer (generate-new-buffer "*filter-buffer*"))
          (start (if (use-region-p) (region-beginning) (buffer-end 0)))
          (end (if (use-region-p) (region-end) (buffer-end 1))))
      (shell-command-on-region start end shell-command output-buffer)
      ;; Always switch to `output-buffer' after running the command.
      ;; By default `shell-command-on-region' will only display the
      ;; output buffer if the output was too large to display in the
      ;; minibuffer.
      (switch-to-buffer output-buffer)
      ;; Store the command that was used to generate the output buffer
      ;; in a local variable, and also the buffer that was used as
      ;; input for the command. This should allow us to do some useful
      ;; history navigation.
      (setq-local input-buffer buffer)
      (setq-local buffer-command shell-command)))

  (defun benv/get-ancestor-buffers-and-commands ()
    "In a buffer that was created using
`benv/filter-region-or-buffer-with-shell-command', return an alist of ancestor
buffer/command pairs that represent the pipeline that led to the
generation of the current buffer.

This allows us to easily jump backwards to a previous step in the
pipeline (i.e. undo)."
    (let ((alist nil))
      (while (local-variable-p 'input-buffer)
        (push (cons input-buffer buffer-command) alist)
        (set-buffer input-buffer))
      alist))

  (defun benv/shell-command-on-buffer (shell-command)
    "Run a shell command, feeding the current buffer as standard input (STDIN)."
    (interactive
     (list (read-shell-command "Shell command: ")))
    (shell-command-on-region (buffer-end 0) (buffer-end 1) shell-command))

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
  (:states '(motion insert emacs)
   :prefix benv/evil-leader-key
   :non-normal-prefix benv/evil-insert-mode-leader-key
   "!" #'benv/shell-command-on-buffer
   "|" #'benv/filter-region-or-buffer-with-shell-command)
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

  ;; Set the default `PATH' for Windows shells (`cmd.exe').
  ;;
  ;; For whatever reason, in the Windows version of Emacs, the
  ;; `cmd.exe' shells started by commands like `shell' and
  ;; `shell-command' don't initialize the `PATH' environment
  ;; variable. I'm not sure why it doesn't use the value of `PATH'
  ;; from the Environment Variables dialog in the Control Panel.

  (when (eq system-type 'windows-nt)
    (add-to-list 'process-environment
                 (format "PATH=%s"
                         (string-join
                          '(
                            "C:\\Windows\\System32"
                            "C:\\Windows\\System32\\OpenSSH"
                            "C:\\Users\\Ben\\scoop\\shims"
                            "C:\\Users\\Ben\\scoop\\apps\\llvm\\16.0.6\\bin"
                            "C:\\Windows\\System32\\WindowsPowerShell\\v1.0")
                          ";"))))

  ;; Custom defaults for shelldon-mode buffers.
  ;;
  ;; (1) Enable word wrap by default. This way I can always see the
  ;; full text of error messages, compile commands, etc., without
  ;; having to manually enable `visual-line-mode' first.
  ;;
  ;; (2) Make the buffer editable by default. This makes it possible
  ;; to answer confirmation prompts and other types of prompts
  ;; without having to manually disable `read-only-mode' first
  ;; with `C-x C-q'.

  (defun benv/shelldon-mode-setup ()
    (visual-line-mode)
    (read-only-mode 0))

  (add-hook #'shelldon-mode-hook #'benv/shelldon-mode-setup)

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

  ;; If we are running emacs under WSL1, default to using a simple
  ;; pipe to communicate with subprocesses rather than a pty. This
  ;; prevents Windows executables from hanging when I launch them from
  ;; emacs running under WSL. For further explanation, see my notes about
  ;; `benv/toggle-pty-for-shell-commands' above.
  (when (string-match "-[Mm]icrosoft" operating-system-release)
    (setq process-connection-type nil))

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
    (let (;; Set `benv/vertico-truncate-lines' to `nil' to show
          ;; the full string for each candidate shell command, wrapping
          ;; across multiple lines if necessary. This variable
          ;; works in conjunction with an advice defined in the
          ;; `use-package' declaration for `vertico' (above).
          (benv/vertico-truncate-lines nil)
          (vertico-sort-function nil))
      (shelldon-output-history)))

  (defun benv/shelldon-from-history ()
    "Select a command from shell command history and use it prefill a shelldon prompt."
    (interactive)
    (let* ((benv/vertico-truncate-lines nil)
           (vertico-sort-function nil)
           (command (completing-read "Select command: " shell-command-history)))
      ;; Call shelldon interactively, then insert the selected command
      (minibuffer-with-setup-hook
          (lambda ()
            ;; This function runs when the minibuffer is set up
            (insert command)
            ;; Move cursor to end of inserted text
            (goto-char (point-max)))
        (call-interactively #'shelldon))))

  (defun benv/shelldon-run-line-in-neighbor-window (dir)
    (interactive)
    (when-let ((line (thing-at-point 'line t)))
      (message "running line: %s" line)
      (benv/with-neighbor-window dir (shelldon-async-command line))))

  (defun benv/shelldon-run-line-in-neighbor-window-and-focus (dir)
    (interactive)
    (when-let ((line (thing-at-point 'line t)))
      (benv/in-neighbor-window dir (shelldon-async-command line))))

  (defun benv/shelldon-run-region-in-neighbor-window (dir)
    (interactive)
    (benv/with-neighbor-window dir (shelldon-send-region)))

  (defun benv/shelldon-run-region-in-neighbor-window-and-focus (dir)
    (interactive)
    (benv/in-neighbor-window dir (shelldon-send-region)))

  ;; Note: This function is a modified version of
  ;; `shelldon-output-history` from `shelldon.el`.
  (defun benv/shelldon-buffer-in-neighbor-window (dir)
    (interactive)
    (when-let ((buffer (cdr (assoc (completing-read shelldon-prompt-str shelldon--hist) shelldon--hist))))
		(benv/with-neighbor-window dir (switch-to-buffer buffer))))

  ;; Note: This function is a modified version of
  ;; `shelldon-output-history` from `shelldon.el`.
  (defun benv/shelldon-buffer-in-neighbor-window-and-focus (dir)
    (interactive)
    (when-let ((buffer (cdr (assoc (completing-read shelldon-prompt-str shelldon--hist) shelldon--hist))))
		(benv/in-neighbor-window dir (switch-to-buffer buffer))))

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
   "t p" 'benv/toggle-pty-for-shell-commands
   "' h" (lambda () (interactive) (benv/evil-goto-mark-in-dir 'left))
   "x j" (lambda () (interactive) (benv/evil-goto-mark-in-dir 'down))
   "x l h" (lambda () (interactive) (benv/shelldon-run-line-in-neighbor-window 'left))
   "x l j" (lambda () (interactive) (benv/shelldon-run-line-in-neighbor-window 'down))
   "x l k" (lambda () (interactive) (benv/shelldon-run-line-in-neighbor-window 'up))
   "x l l" (lambda () (interactive) (benv/shelldon-run-line-in-neighbor-window 'right))
   "x l H" (lambda () (interactive) (benv/shelldon-run-line-in-neighbor-window-and-focus 'left))
   "x l J" (lambda () (interactive) (benv/shelldon-run-line-in-neighbor-window-and-focus 'down))
   "x l K" (lambda () (interactive) (benv/shelldon-run-line-in-neighbor-window-and-focus 'up))
   "x l L" (lambda () (interactive) (benv/shelldon-run-line-in-neighbor-window-and-focus 'right))
   "x r h" (lambda () (interactive) (benv/shelldon-run-region-in-neighbor-window 'left))
   "x r j" (lambda () (interactive) (benv/shelldon-run-region-in-neighbor-window 'down))
   "x r k" (lambda () (interactive) (benv/shelldon-run-region-in-neighbor-window 'up))
   "x r l" (lambda () (interactive) (benv/shelldon-run-region-in-neighbor-window 'right))
   "x r H" (lambda () (interactive) (benv/shelldon-run-region-in-neighbor-window-and-focus 'left))
   "x r J" (lambda () (interactive) (benv/shelldon-run-region-in-neighbor-window-and-focus 'down))
   "x r K" (lambda () (interactive) (benv/shelldon-run-region-in-neighbor-window-and-focus 'up))
   "x r L" (lambda () (interactive) (benv/shelldon-run-region-in-neighbor-window-and-focus 'right))
   "x h h" (lambda () (interactive) (benv/shelldon-buffer-in-neighbor-window 'left))
   "x h j" (lambda () (interactive) (benv/shelldon-buffer-in-neighbor-window 'down))
   "x h k" (lambda () (interactive) (benv/shelldon-buffer-in-neighbor-window 'up))
   "x h l" (lambda () (interactive) (benv/shelldon-buffer-in-neighbor-window 'right))
   "x h H" (lambda () (interactive) (benv/shelldon-buffer-in-neighbor-window-and-focus 'left))
   "x h J" (lambda () (interactive) (benv/shelldon-buffer-in-neighbor-window-and-focus 'down))
   "x h K" (lambda () (interactive) (benv/shelldon-buffer-in-neighbor-window-and-focus 'up))
   "x h L" (lambda () (interactive) (benv/shelldon-buffer-in-neighbor-window-and-focus 'right)))
  (:states '(motion emacs)
   "s-r" #'benv/shelldon-from-history
   "s-x" #'shelldon)
  :init
  (evil-set-initial-state 'shell-mode 'normal))

;;----------------------------------------
;; grep/wgrep/ripgrep/rg
;;----------------------------------------

(use-package grep
  :general
  (:keymaps 'grep-mode-map "SPC" nil))

(use-package rg
  :general
  (:keymaps 'rg-mode-map "SPC" nil))

(use-package wgrep)

;;----------------------------------------
;; vterm
;;----------------------------------------

(use-package vterm
  :general
  ('motion
   :prefix benv/evil-leader-key
   "$" 'vterm))

;;----------------------------------------
;; guile scheme
;;----------------------------------------

(use-package geiser-guile
  :config
  (add-to-list 'geiser-guile-load-path "~/.config/guix")
  (add-to-list 'geiser-guile-load-path "~/git/guix")
  (use-package paredit))

;;----------------------------------------
;; emacs-lisp-mode
;;----------------------------------------

(use-package elisp-mode
  :config
  (electric-pair-mode)
  (defun benv/pp (sexp)
    "Pretty print SEXP in a popup buffer.

I wrote function this because evaluating `(pp var)' with `M-:`
(`eval') often results in truncated output in the \"*Messages*\"
buffer, when examining large/complex expressions.

In addition, it's more convenient for the result to pop up
immediately in an isolated buffer, rather than having
to switch to the \"*Messages*\" buffer and then search through
the messages for the relevant output."
    (let ((buf (get-buffer-create "*PP Output*")))
      (with-current-buffer buf
        (erase-buffer)
        (pp sexp buf)
        (emacs-lisp-mode))
      (pop-to-buffer buf))))

(use-package lispy
  :hook (elisp-mode . lispy-mode)
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
  ;; Use spaces for indentation rather than tabs.
  ;;
  ;; I'm don't have any personal preference, but ledger-mode's built-in
  ;; indentation functions seem to assume that spaces are being used.
  :hook (ledger-mode . (lambda () (setq indent-tabs-mode nil)))
  :mode ("\\.ledger'" . ledger-mode))

;;----------------------------------------
;; ztree
;;----------------------------------------

(use-package ztree
  :commands (ztree-diff ztree-dir))

;;----------------------------------------
;; html editing
;;----------------------------------------

;; `web-mode' handles syntax highlighting of mixed HTML, CSS, and
;; Javascript much better than `mhtml-mode' (Emacs' default mode
;; for HTML files).
(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  ;; Note: For some reason `auto-revert-mode' is disabled by default
  ;; in `web-mode', even though I have turned on `global-auto-revert'
  ;; mode above.
  :hook (web-mode . (lambda () (auto-revert-mode))))

;;----------------------------------------
;; email (mu4e)
;;
;; Setup in this section is based on:
;; https://www.djcbsoftware.nl/code/mu/mu4e/Gmail-configuration.html
;;----------------------------------------

(use-package epg-config
  :config
  (setq epg-pinentry-mode 'loopback))

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
  (auth-source-pass-enable)
  :config
  ;; Change the default length of randomly-generated passwords from 25 -> 10.
  (setq password-store-password-length 10))

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
  ;; Don't automatically fetch and index new mail at regular
  ;; intervals.  Fetch mail manually with `C-c C-u'
  ;; (`mu4e-get-mail-command') instead.
  ;;
  ;; Automatically fetching mail would be nice but there are
  ;; two annoying problems with it:
  ;; (1) It freezes the mu4e UI until the fetching/indexing has
  ;; completed, which can take up to 30 seconds.
  ;; (2) Every time mu4e fetches mail it brings up a prompt for my
  ;; `pass`/`gpg` password, which is highly annoying.
  (setq mu4e-update-interval nil)
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
;; Firefox intergrations.
;;----------------------------------------

(defun benv/find-firefox-places-db (firefox-dir)
  "Find places.sqlite in any profile directory under FIREFOX-DIR."
  (let ((places-db nil))
    (dolist (file (directory-files-recursively firefox-dir "places\\.sqlite$" t))
      (when (and (file-readable-p file)
                 (not places-db))
        (setq places-db file)))
    places-db))

(defun benv/firefox-visit-history-url ()
  "Select URL from Firefox history sorted by frecency score, or enter search terms."
  (interactive)

  (unless (executable-find "sqlite3")
    (user-error "Cannot find sqlite3 executable on PATH"))

  (let* ((firefox-dir "~/.mozilla/firefox")
         (places-db (benv/find-firefox-places-db firefox-dir))
         (temp-db "/tmp/firefox-places-temp.sqlite")
         (urls nil))

    (unless places-db
      (user-error "Cannot find Firefox places.sqlite database"))

    ;; Copy DB since Firefox might have it locked
    (copy-file places-db temp-db t)

    ;; Get URLs sorted by frecency
    (with-temp-buffer
      (call-process "sqlite3" nil t nil
                    temp-db
                    "SELECT url, title, frecency FROM moz_places
                     WHERE url LIKE 'http%'
                     ORDER BY frecency DESC LIMIT 500;")
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "\\(.*\\)|\\(.*\\)|\\([0-9]+\\)")
          (let* ((url (match-string 1))
                 (title (match-string 2))
                 (frecency (match-string 3))
                 (display-title (if (string-empty-p title)
                                    "*no title*"
                                  title)))
            (push (cons (format "%s | %s | frecency:%s"
                                url
                                display-title
                                frecency)
                        url)
                  urls)))
        (forward-line 1)))

    ;; Clean up temp DB
    (delete-file temp-db)

    ;; Let user select URL from history or enter custom input
    (let* ((vertico-sort-function nil)
           (selected (completing-read "Firefox history or enter search terms: "
                                      (reverse urls)
                                      nil nil)))
      (when selected
        (if (assoc selected urls)
            ;; User selected from history
            (browse-url-firefox (cdr (assoc selected urls)) t)
          ;; User entered custom input - treat as search terms
          (browse-url-firefox (concat "https://www.google.com/search?q="
                                      (url-hexify-string selected))
                              t))))))

(defun benv/google-search-new-window (query)
  "Perform a Google search in a new Firefox window for QUERY.

If any text is currently selected, use that as the default value
for QUERY."
  (interactive
   (list (if (use-region-p)
             (read-string "Google search: " (buffer-substring-no-properties (region-beginning) (region-end)))
           (read-string "Google search: "))))
  (let ((search-url (format "https://www.google.com/search?q=%s"
                            (url-hexify-string query))))
    (browse-url-firefox search-url t)))

(general-def
  :states '(motion insert emacs)
  "s-g"   'benv/google-search-new-window
  "s-w"   'benv/firefox-visit-history-url)

;;----------------------------------------
;; Emacs profiler (built-in)
;;----------------------------------------

(use-package profiler
  :bind
  ("s-p" . profiler-start)
  ("s-P" . profiler-stop))

;;----------------------------------------
;; EXWM
;;----------------------------------------

(use-package exwm

  ;; Only load EXWM if the `EXWM` environment variable is defined.
  ;; I usually do from my `.xsession` file, with a line like
  ;; `export EXWM=1`.
  :if (getenv "EXWM")

  :config

  ;; This fixes a strange focus problem in EXWM, where the cursor
  ;; remains hollow after selecting a window.
  ;;
  ;; I found the solution at:
  ;; https://github.com/ch11ng/exwm/issues/889#issuecomment-1874977844
  (setq x-no-window-manager t)

  ;; Machine-specific screen/workpace configuration.
  (let ((hostname (system-name)))
    (cond ((equal hostname "guix")
           ;; My Guix desktop machine with 2 monitors.
           ;; The code below was copied and modified from:
           ;; https://github.com/emacs-exwm/exwm/wiki#randr-multi-screen
           (setq exwm-workspace-number 2)
           (setq exwm-randr-workspace-monitor-plist
                 '(0 "DVI-I-1" 1 "DP-3"))
           (add-hook 'exwm-randr-screen-change-hook
                     (lambda ()
                       (start-process-shell-command
                        "xrandr" nil "xrandr --output DVI-I-1 --left-of DP-3 --auto")))
           (exwm-randr-mode 1))
          (t
           ;; Assume one screen and workspace by default.
           (setq exwm-workspace-number 1))))

  ;; Key prefixes that are always passed through to Emacs.
  (setq exwm-input-prefix-keys
        '(?\C-h ;; `describe-key`, `describe-variable`, etc.
          ?\C-w ;; evil window commands (note: masks `C-w` to close tab in Firefox)
          ?\C-x ;; emacs window commands (e.g. `C-x 2` for `split-window-below`)
          ?\M-! ;; `shell-command`
          ?\M-& ;; `async-shell-command`
          ?\M-: ;; `eval-expression`
          ?\M-q ;; quit-window
          ?\M-x ;; `execute-extended-command`
          ?\s-c ;; `claude-code-command-map`
          ?\s-g ;; benv/google-search-new-window
          ?\s-j ;; evil-window-left
          ?\s-k ;; evil-window-down
          ?\s-l ;; evil-window-up
          ?\s-; ;; evil-window-right
          ?\s-J ;; windmove-swap-states-left
          ?\s-K ;; windmove-swap-states-down
          ?\s-L ;; windmove-swap-states-up
          ?\s-: ;; windmove-swap-states-right
          ?\s-u ;; winner-undo
          ?\s-U ;; winner-redo
          ?\s-d ;; delete-window
          ?\s-o ;; delete-other-windows
          ?\s-v ;; evil-window-vsplit
          ?\s-s ;; evil-window-split
          ?\s-r ;; benv/shelldon-from-history
          ?\s-b ;; consult-buffer
          ?\s-n ;; org-roam-find-file
          ?\s-w ;; benv/firefox-visit-history-url
          ?\s-x ;; shelldon (run shell command)
          ?\s-m ;; magit-status
          ?\s-M ;; benv/projectile-magit-status
          ?\s-0 ;; switch to buffer 0
          ?\s-1 ;; switch to buffer 1
          ?\s-2 ;; switch to buffer 2
          ?\s-3 ;; switch to buffer 3
          ?\s-4 ;; switch to buffer 4
          ?\s-5 ;; switch to buffer 5
          ?\s-6 ;; switch to buffer 6
          ?\s-7 ;; switch to buffer 7
          ?\s-8 ;; switch to buffer 8
          ?\s-9 ;; switch to buffer 9
          ?\s-\C-0 ;; assign current buffer to Win+0
          ?\s-\C-1 ;; assign current buffer to Win+1
          ?\s-\C-2 ;; assign current buffer to Win+2
          ?\s-\C-3 ;; assign current buffer to Win+3
          ?\s-\C-4 ;; assign current buffer to Win+4
          ?\s-\C-5 ;; assign current buffer to Win+5
          ?\s-\C-6 ;; assign current buffer to Win+6
          ?\s-\C-7 ;; assign current buffer to Win+7
          ?\s-\C-8 ;; assign current buffer to Win+8
          ?\s-\C-9 ;; assign current buffer to Win+9
          ))

  ;; The above `exwm-input-prefix-keys'
  ;; can still be passed through to the underlying X11 program,
  ;; if one presses `C-q` first.
  (define-key exwm-mode-map
              [?\C-q] 'exwm-input-send-next-key)

  ;; Couldn't get this keybinding to work with `exwm-input-prefix-keys'
  ;; above, but adding to `exwm-mode-map' instead seems to work.
  (define-key exwm-mode-map
              (kbd "s-<tab>") 'benv/switch-to-previous-buffer)

  (setq exwm-input-global-keys
        ;; Reset EXWM input mode to "line mode". In other words,
        ;; escape "char mode" where all keys get sent to the
        ;; X application.
        ;;
        ;; Note: Fullscreening an automatically puts it into
        ;; "char mode".
        '(([?\s-r] . exwm-reset)))

  (defun exwm-rename-buffer ()
    (interactive)
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ":"
             (if (<= (length exwm-title) 50) exwm-title
               (concat (substring exwm-title 0 49) "...")))))

  ;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
  (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

  (exwm-enable))

;;----------------------------------------
;; Dictionary setup, by following the instructions from:
;; https://mbork.pl/2017-01-14_I'm_now_using_the_right_dictionary
;;----------------------------------------

(use-package sdcv-mode
  :load-path "~/.emacs.d/site-lisp/sdcv-mode")

;;----------------------------------------
;; nix-mode
;;----------------------------------------

(use-package nix-mode)

;;----------------------------------------
;; pgmacs: postgres client
;;----------------------------------------

(use-package pgmacs
  :config
  (defun benv/pgmacs (dbname)
    "Connect to local PostgreSQL database DBNAME using pgmacs.
  Automatically detects the Unix socket path and current user.

I wrote this wrapper method because pg.el/pgmacs isn't very smart
about figuring out of correct connection string. It seems that you
need to explicitly tell it the location of the Unix socket file."
    (interactive "sDatabase name: ")
    (let* ((user (or (getenv "USER") (user-login-name)))
           ;; Get the socket directory from psql
           (socket-dir-output (shell-command-to-string
                              (format "psql -t -A -c 'SHOW unix_socket_directories;' 'postgresql:///%s' 2>/dev/null || echo '/var/run/postgresql'" dbname)))
           (socket-dir (string-trim socket-dir-output))
           ;; Get the port from psql
           (port-output (shell-command-to-string
                        (format "psql -t -A -c 'SHOW port;' 'postgresql:///%s' 2>/dev/null || echo '5432'" dbname)))
           (port (string-trim port-output))
           ;; Construct the full socket path
           (socket-path (format "%s/.s.PGSQL.%s" socket-dir port))
           ;; URL-encode the socket path
           (encoded-socket-path (url-hexify-string socket-path))
           ;; Construct the full URI
           (uri (format "postgresql://%s@%s/%s" user encoded-socket-path dbname)))
      (message "Connecting to: %s" uri)
      (pgmacs-open-uri uri))))

;;----------------------------------------
;; Start emacs server and set EDITOR=emacsclient.
;;
;; I added this so that `jj describe` would show a new buffer in my
;; running emacs instance, for writing a commit message.
;;----------------------------------------

(server-start)
(setenv "EDITOR" "emacsclient")

;;----------------------------------------
;; Print emacs startup time.
;;
;; (Overrides default minibuffer message
;; immediately after emacs startup.)
;;----------------------------------------

(defun display-startup-echo-area-message ()
  (message "emacs startup time: %s" (emacs-init-time)))
