(define-module (benv exwm)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (ice-9 format))

;; Copied and modified from `exwm` package in `~/git/guix/gnu/packages/emacs-xyz.scm`.
(define-public benv-exwm
  (package
    (name "benv-exwm")
    (version "0.32")
    (synopsis "Emacs X window manager")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/"
                           "exwm-" version ".tar"))
       (sha256
        (base32 "0k3c7grgkkpgd0r8b9vsqa5ywhb4vwxr3wfjyfxw8xy0yq7y0jvn"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-xelb))
    (inputs
     (list xhost dbus))
    ;; The following functions and variables needed by emacs-exwm are
    ;; not included in emacs-minimal:
    ;; scroll-bar-mode, fringe-mode
    ;; x-display-pixel-width, x-display-pixel-height
    (arguments
     (list
      #:emacs emacs
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'install-xsession
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((xsessions (string-append #$output "/share/xsessions"))
                     (bin (string-append #$output "/bin"))
                     (exwm-executable (string-append bin "/exwm")))
                ;; Add a .desktop file to xsessions
                (mkdir-p xsessions)
                (mkdir-p bin)
                (make-desktop-entry-file
                 (string-append xsessions "/exwm.desktop")
                 #:name #$name
                 #:comment #$synopsis
                 #:exec exwm-executable
                 #:try-exec exwm-executable)
                ;; Add a shell wrapper to bin
                (with-output-to-file exwm-executable
                  (lambda _
                    (format #t "#!~a ~@
                     ~a +SI:localuser:$USER ~@
                     exec ~a --exit-with-session ~a --maximized --debug-init \"$@\" ~%"
                            (search-input-file inputs "/bin/sh")
                            (search-input-file inputs "/bin/xhost")
                            (search-input-file inputs "/bin/dbus-launch")
                            (search-input-file inputs "/bin/emacs"))))
                (chmod exwm-executable #o555)))))))
    (home-page "https://github.com/ch11ng/exwm")
    (description
     "EXWM is a full-featured tiling X window manager for Emacs built on top
of XELB.")
    (license license:gpl3+)))
