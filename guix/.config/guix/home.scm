;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu packages databases)
             (gnu packages gnupg)
             (gnu packages gnuzilla)
             (gnu packages package-management)
             (gnu packages password-utils)
             (gnu packages rust-apps)
             (gnu packages ssh)
             (gnu packages version-control)
             (gnu services)
             (gnu system)
             (gnu home services gnupg)
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

  (cons

   ;; Emacs 29.1, the version of Emacs that I know works with my `init.el`.

   (first (lookup-inferior-packages emacs-29.1-inferior "emacs-pgtk-xwidgets"))

   (append

    ;; Other packages that I use daily.
    ;;
    ;; It's probably fine to use rolling versions of these packages,
    ;; rather than pinning them to a specific version, like I do with
    ;; Emacs above.
    (list git
          gnupg
          icecat
          openssh
          password-store
          pinentry-emacs
          recutils
          ripgrep
          stow))))

 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.

 (services

  (list

   ;; Bash configuration (.bash_profile / .bashrc)

   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bash-profile (list (plain-file "bash-profile" "\

# Symlink shared `.gnupg` and `.password-store` directories from
# Syncthing to my home directory.
#
# Note: On a new machine, the source directories under `~/Sync`
# (e.g. `~/Sync/.gnupg`) will not exist until I've done the initial
# setup of Syncthing in the web UI, in order to connect this machine to
# my other computers. I don't think it's practical to automate that
# step, and it only has to be done once anyway.

if [ ! -e .gnupg -a -d Sync/.gnupg ]; then ln -s Sync/.gnupg .; fi
if [ ! -e .password-store -a -d Sync/.password-store ]; then ln -s Sync/.gnupg .; fi

# Symlink my dotfiles into my home directory.
#
# Notes:
#
# * I prefer to just symlink my dotfiles into my home directory using `stow`,
# rather than using Guix's built-in `home-dotfiles-service-type`, because
# the latter copies the dotfiles into `/gnu/store` and makes them read-only.
# This would prevent me from editing my dotfiles in place, which would seriously
# hamper my fun/productivity. For example, I would need to run
# `guix home reconfigure` every time I made a change to my `init.el`,
# which would be extremely slow and awkward! On top of that, I don't
# see much value in storing all the historical versions of my dotfiles in
# the Guix store. It makes a lot more sense to use `git` for that.
#
# * On a new machine, the source directories under `~/dotfiles`
# (e.g. `~/dotfiles/emacs-redux`) won't exist until I've manually `git clone`d
# my `dotfiles` repo from GitHub. I could potentially automate that
# step if I shared `ssh` keys between my machines using Syncthing, because
# then I wouldn't have to manually add a new `ssh` key on GitHub. But I'm not
# sure that's a good idea security-wise.
#
# * It does no harm to repeatedly run `stow` on a directory
# from my `~/dotfiles` directory. The operation is idempotent,
# so I don't need check if the target files/directories already exist
# (e.g. `~/.emacs.d`).

if [ -d dotfiles/emacs-redux ]; then stow --dir=dotfiles emacs-redux; fi

")))))

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
