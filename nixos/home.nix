{ config, pkgs, pkgs-emacs, ... }: {

    home = {
        username = "benv";
        homeDirectory = "/home/benv";
        packages = [
            pkgs-emacs.emacs
        ];
        stateVersion = "24.05";
    };

    programs.bash = {
        enable = true;
        initExtra = ''
            # Symlink shared `.gnupg` and `.password-store` directories from
            # Syncthing to my home directory.
            #
            # Note: On a new machine, the source directories under `~/Sync`
            # (e.g. `~/Sync/.gnupg`) will not exist until I've done the initial
            # setup of Syncthing in the web UI, in order to connect this machine to
            # my other computers. I don't think it's practical to automate that
            # step, and it only has to be done once anyway.

            if [ ! -e .ssh -a -d Sync/.ssh ]; then ln -s Sync/.ssh .; fi
            if [ ! -e .gnupg -a -d Sync/.gnupg ]; then ln -s Sync/.gnupg .; fi
            if [ ! -e .password-store -a -d Sync/.password-store ]; then ln -s Sync/.password-store .; fi

            # Symlink my dotfiles into my home directory.
            #
            # Note: It does no harm to repeatedly run `stow` on a directory
            # from my `~/dotfiles` directory. The operation is idempotent,
            # so I don't need check if the target files/directories already exist
            # (e.g. `~/.emacs.d`).

            if [ ! -e .emacs.d -a -d dotfiles/emacs-redux ]; then stow --dir=dotfiles emacs-redux; fi
        '';
    };
}
