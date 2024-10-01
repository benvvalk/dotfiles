;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (gnu home services shells)
             (gnu home services syncthing)
             (guix channels)
             (guix gexp)
             (guix inferior)
             (srfi srfi-1))

;; Note: Guix "inferiors" are used to install a
;; package using a specific commit of the
;; Guix packages repo [1]. In this case, I am using
;; it to install emacs-29.1, because that's the
;; version that I know works with the emacs configuration
;; from my `dotfiles` repo.
;;
;; [1]: https://guix.gnu.org/manual/en/html_node/Inferiors.html

(define emacs-29.1-inferior
  (inferior-for-channels
   (list (channel
          (name 'guix)
          (url "https://git.savannah.gnu.org/git/guix.git")
          (commit "c7f937cf")))))


(home-environment

  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.

 (packages

  (cons*

   ;; Install Emacs 29.1, the specific version of Emacs that I know
   ;; works well with my `init.el`.

   (first (lookup-inferior-packages emacs-29.1-inferior "emacs-pgtk-xwidgets"))

   ;; Other packages, for which installed versions don't really matter.
   ;; Note: Ideally I would like to nail down the exact version
   ;; of every package, but for now this is good enough.

   (specifications->packages
    (list "guix"
          "coreutils"
          "git"
          "openssh"
          "recutils"
          "ripgrep"
          "which"))))

 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.

 (services

  (list

   ;; Bash configuration (.bash_profile / .bashrc)

   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)))

   ;; Syncthing configuration.
   ;;
   ;; This is just the default configuration, which installs/runs
   ;; the Syncthing daemon with default parameters.
   ;;
   ;; I don't know if it's possible to
   ;; configure Syncthing to automatically connect to specific machines
   ;; (that would be really cool), but I couldn't figure out how to
   ;; to do that. I guess I will just configure it manually through
   ;; the web UI.

   (service home-syncthing-service-type))))
