;; which-window.el: Intercept calls to `display-buffer' and show a
;; prompt to select the target window.

(define-minor-mode which-window-mode
  "Toggle Which Window mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When is Which Window mode is enabled, emacs will prompt the user
for a target window, every time it wants to display a buffer."
  :global t       ;; enable this minor mode in all buffers
  :init-value nil ;; initial value of `which-window-mode' variable
  :lighter ""     ;; mode line indicator
  (if which-window-mode
      (advice-add #'display-buffer :around #'which-window--display-buffer-advice)
    (advice-remove #'display-buffer #'which-window--display-buffer-advice)))

(defvar which-window--buffer-blacklist
  '("transient" "debug" "trace")
  "A list of regular expressions for buffer names that which-window
should *not* handle. Buffer names that match this list will be
displayed with `display-buffer', as per default behaviour.")

(defvar which-window--side-window-percent-size 40
  "The percent size of a side window, relative to the
total height/width of the frame. This only variable
only affects the size of windows that are created by
which-window mode; when the which-window reuses an
existing side window, its size is left unchanged.")

(defun which-window--string-match-list-p (regex list)
  "Return non-nil if REGEX exactly matches one or more strings in LIST.
Return nil otherwise."
  (catch 'matched
    (dolist (item list)
      (when (string-match-p item regex)
        (throw 'matched t)))
    nil))

(defun which-window--display-buffer-in-neighbor-window (bufname dir &optional focus)
  "Display the buffer with name BUFNAME in a neighbor window next to
the currently selected window, in direction DIR. If a neighbor window
doesn't already exist in direction DIR, create it by splitting the
currently selected window.

If the optional FOCUS parameter is non-nil, switch input focus to the
neighbor window.

Return the neighbor window."
  (let ((neighbor-exists (window-in-direction dir)))
    ;; If a neighbor window in direction DIR does
    ;; not already exist, create one by splitting
    ;; the current window.
    (unless neighbor-exists
      (split-window nil nil dir))
    (let ((neighbor-window (window-in-direction dir)))
        (set-window-buffer neighbor-window bufname)
        ;; If we created a new neighbor window by splitting the
        ;; currently selected window, configure the `quit-restore'
        ;; window parameter, so that calling `quit-window' will close
        ;; the window and return focus to the previously selected
        ;; window.
        ;;
        ;; Background: By convention, most read-only buffers
        ;; (e.g. `help-mode' buffers) bind the 'q' key to the
        ;; `quit-window' function. The purpose of `quit-window' is to:
        ;; (1) hide/close the current buffer, and (2) restore emacs'
        ;; window configuration to its previous state before
        ;; displaying the buffer. For further explanation, see:
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Quitting-Windows.html
        (unless neighbor-exists
          ;; Clear the buffer history for the newly created neighbor window.
          ;; Otherwise, the buffer for the previously selected window
          ;; (i.e. the window that we split to create the neighbor
          ;; window) will be in the buffer history, and this will prevent
          ;; emacs from deleting the window when `quit-window` is called.
          (set-window-prev-buffers neighbor-window nil)
          ;; Set the `quit-restore' window parameter, which tells
          ;; emacs what to do when `quit-window` is called. In our
          ;; case we want emacs to delete the neighbor window and
          ;; set the focus back to the previously selected window.
          ;;
          ;; Note: The semantics of the `quit-restore' parameter (a
          ;; 4-element list) are a bit tricky/non-obvious. For
          ;; a detailed explanation, see:
          ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Quitting-Windows.html
          (set-window-parameter neighbor-window
                                'quit-restore
                                (list
                                 'window
                                 'window
                                 (selected-window)
                                 (get-buffer bufname))))
        ;; Switch focus to the neighbor window if the FOCUS
        ;; parameter to this function was non-nil.
        (when focus (select-window neighbor-window))
        neighbor-window)))

(defun which-window--display-buffer-in-bottom-window (bufname &optional focus)
  "Display the buffer with name BUFNAME in a window along the bottom
of the frame. If no such window exists, create one by splitting the
top-level window. The size of a newly created bottom window is
determined by `which-window--side-window-percent-size'.

If the optional FOCUS parameter is non-nil, switch input focus to the
bottom window.

Return the bottom window."
  (let* ((root-window (frame-root-window))
         (root-window-height (window-size root-window))
         ;; `window-top-child' will return `nil' unless the root node
         ;; of the window layout tree represents a vertical split.
         (bottom-window-exists (window-top-child root-window))
         (bottom-window-fraction (/ (float which-window--side-window-percent-size) 100))
         (bottom-window-height (round (* bottom-window-fraction root-window-height))))
    (message "root-window-height: %d, bottom-window-height: %d" root-window-height bottom-window-height)
    (unless bottom-window-exists
      (split-window root-window (- bottom-window-height)))
    (let* ((root-window (frame-root-window))
           (bottom-window (window-top-child root-window)))
      ;; traverse siblings until we get to the bottom-most child window
      (while (window-next-sibling bottom-window)
        (setq bottom-window (window-next-sibling bottom-window)))
      ;; display the given buffer in the bottom-most window
      (set-window-buffer bottom-window bufname)
      ;; If we created the bottom window by splitting the
      ;; top-level window, configure the `quit-restore'
      ;; window parameter, so that calling `quit-window' will close
      ;; the window and return focus to the previously selected
      ;; window.
      ;;
      ;; Background: By convention, most read-only buffers
      ;; (e.g. `help-mode' buffers) bind the 'q' key to the
      ;; `quit-window' function. The purpose of `quit-window' is to:
      ;; (1) hide/close the current buffer, and (2) restore emacs'
      ;; window configuration to its previous state before
      ;; displaying the buffer. For further explanation, see:
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Quitting-Windows.html
      (unless bottom-window-exists
        ;; Clear the buffer history for the newly created neighbor window.
        ;; Otherwise, the buffer for the previously selected window
        ;; (i.e. the window that we split to create the neighbor
        ;; window) will be in the buffer history, and this will prevent
        ;; emacs from deleting the window when `quit-window` is called.
        (set-window-prev-buffers bottom-window nil)
        ;; Set the `quit-restore' window parameter, which tells
        ;; emacs what to do when `quit-window` is called. In our
        ;; case we want emacs to delete the bottom window and
        ;; set the focus back to the previously selected window.
        ;;
        ;; Note: The semantics of the `quit-restore' parameter (a
        ;; 4-element list) are a bit tricky/non-obvious. For
        ;; a detailed explanation, see:
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Quitting-Windows.html
        (set-window-parameter bottom-window
                              'quit-restore
                              (list
                               'window
                               'window
                               (selected-window)
                               (get-buffer bufname))))
      ;; Switch focus to the bottom window if the FOCUS
      ;; parameter to this function was non-nil.
      (when focus (select-window bottom-window))
      bottom-window)))

(defun which-window--display-buffer-full-frame (bufname)
  "Display buffer with name BUFNAME in a window that occupies the
entire frame, by displaying the buffer in currently selected window
and then deleting all other windows in the same frame."
  (let ((current-window (selected-window)))
    (message "display buffer %s in full frame window; current window is %s" bufname current-window)
    (delete-other-windows)
    (set-window-buffer current-window bufname)
    current-window))

;; Magit compatibility fix:
;;
;; Most packages seem to be well-behaved if I suppress display of the
;; buffer by returning `nil' from `display-buffer'. (The return value
;; of `display-buffer' is the window where the buffer was displayed.)
;;
;; However, `magit-status' throws an error if I return `nil' from
;; `display-buffer', since its default behaviour is to call
;; `select-window' on the returned value of `display-buffer'. This can
;; be fixed by setting `magit-display-buffer-noselect' to `t'.

(setq magit-display-buffer-noselect t)

;; Note: I set global variables to pass arguments to
;; `which-window--transient'. I could also use the `transient's
;; "scope" mechanism to pass the arguments (see the
;; `transient-showcase' README for an example), but I feel that
;; using global variables is simpler/clearer.

(setq which-window--bufname nil)

(transient-define-prefix which-window--transient ()
  [:description
   (lambda ()
     (format "Display \"%s\" buffer in which window?" which-window--bufname))
   [("h" "left & focus"
    (lambda () (interactive)
      (which-window--display-buffer-in-neighbor-window
       which-window--bufname 'left t)))
   ("j" "down & focus"
    (lambda () (interactive)
      (which-window--display-buffer-in-neighbor-window
       which-window--bufname 'down t)))
   ("k" "up & focus"
    (lambda () (interactive)
      (which-window--display-buffer-in-neighbor-window
       which-window--bufname 'up t)))
   ("l" "right & focus"
    (lambda () (interactive)
      (which-window--display-buffer-in-neighbor-window
       which-window--bufname 'right t)))]
   [("H" "left"
    (lambda () (interactive)
      (which-window--display-buffer-in-neighbor-window
       which-window--bufname 'left)))
   ("J" "down"
    (lambda () (interactive)
      (which-window--display-buffer-in-neighbor-window
       which-window--bufname 'down)))
   ("K" "up"
    (lambda () (interactive)
      (which-window--display-buffer-in-neighbor-window
       which-window--bufname 'up)))
   ("L" "right"
    (lambda () (interactive)
      (which-window--display-buffer-in-neighbor-window
       which-window--bufname 'right)))]
   [("b" "bottom & focus"
     (lambda () (interactive)
       (which-window--display-buffer-in-bottom-window
       which-window--bufname t)))]
   [("B" "bottom"
     (lambda () (interactive)
       (which-window--display-buffer-in-bottom-window
       which-window--bufname)))]
   [("." "current window"
     (lambda () (interactive)
       (set-window-buffer (selected-window) which-window--bufname)))
    ("f" "full frame"
     (lambda () (interactive)
       (which-window--display-buffer-full-frame
        which-window--bufname)))]])

(defun which-window--display-buffer-advice (orig-fun &rest args)
  (message "display-buffer called with args %S" args)
  (let ((bufname (buffer-name (car args))))
    (message "buffer name: %s" bufname)
    ;; Note: If we are displaying the `which-window--transient' popup,
    ;; use `display-buffer' to display it, to avoid infinite recursion.
    ;; Otherwise, interactively select the target window and display
    ;; the buffer via `which-window--transient'.
    (if (which-window--string-match-list-p bufname which-window--buffer-blacklist)
        (let ((result (apply orig-fun args)))
          (message "display-buffer returned %S" result)
          result)
      ;; Set arguments to pass to `transient' popup.
      (setq which-window--bufname bufname)
      ;; Open the transient popup.
      ;; Note: This function call returns immediately; it
      ;; does not wait for user input!
      (which-window--transient)
      ;; The return value from `display-buffer' is the window where
      ;; the buffer was displayed. We need to return `nil' here
      ;; because we are intentionally suppressing display of the
      ;; buffer during this `display-buffer' call. Instead we will
      ;; wait until the user selects a target window in the transient
      ;; popup (which happens later, outside of this function), and
      ;; then display the buffer using `set-window-buffer', which is a
      ;; more direct/low-level function than `display-buffer'.
      nil)))
