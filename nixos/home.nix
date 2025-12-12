{ config, pkgs, inputs, system, ... }: {

    home = {
        username = "benv";
        homeDirectory = "/home/benv";
        packages = with pkgs; [
            alsa-utils # for `alsamixer`
            direnv # for emacs-direnv
            inputs.nixpkgs-emacs.legacyPackages.${system}.emacs
            filezilla # graphical FTP client
            gcc # `org-roam` needs this to auto-compile its own `sqlite` binary
            gnome-terminal
            ripgrep # for `M-x rg` in emacs
            sqlite # for `benv/firefox-visit-history-url` in emacs
            vlc # video player
        ];
        stateVersion = "25.05";
    };

    # Let home-manager install/update its own binary.
    programs.home-manager.enable = true;

    programs.bash = {
        enable = true;
        initExtra = ''
            export EDITOR=vim

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

    # Allow home-manager to clobber existing config file (if any).
    #
    # This means that all Plover settings are lost when I run
    # `home-manager switch`. In particular, I need to manually
    # reconfigure the Plover machine settings (keyboard device file)
    # and the dictionary list in the Plover GUI. (Note: I recorded my
    # dictionary list below, so that I can easily restore it.)
    #
    # NOTE: I've noticed that my settings are preserved if the Plover
    # GUI is open when I run `home-manager switch`, probably because
    # the GUI automatically saves the working config to disk.
    #
    # IDEA: I could also pass the `-b $FILE_EXTENSION` option to
    # `home-manager switch`, but I would need to embed the date or
    # some other unique identifier in `$FILE_EXTENSION`, otherwise the
    # `home-manager switch` will fail on the second run because the
    # backup file already exists. Also, I would need replace the new
    # Nix-generated config file with the backup file in order to
    # restore my settings (perhaps with a custom script).

    home.file.".mozilla/firefox/profiles.ini".force = true;

    programs.firefox =  {
        enable = true;
        profiles.benv = {
          extensions.packages = with inputs.firefox-addons.packages.${system}; [
              tridactyl
              ublock-origin
          ];
        };
    };

    # Plover configuration.
    #
    # Note: `plover-flake` doesn't seem to have any options for
    # automatically configuring the dictionary list, so I still need
    # to do that manually in the Plover GUI.
    #
    # Here is my dictionary list:
    #
    # ~/dotfiles/plover/ben-fingerspelling.py
    # ~/dotfiles/plover/attached-numbers.py
    # ~/dotfiles/plover/emily-modifiers.py
    # ~/dotfiles/plover/emily-symbols.py
    # ~/dotfiles/plover/user.json
    # ~/.config/plover/commands.json (e.g. TKUPT -> update dictionary)
    # ~/.config/plover/main.json

    # Allow home-manager to clobber existing file (if any).
    home.file.".config/plover/plover.cfg".force = true;

    imports = [
        inputs.plover-flake.homeManagerModules.plover
    ];

    programs.plover = {
      enable = true;
      package = inputs.plover-flake.packages.${system}.plover.withPlugins (
        ps: with ps; [
            plover-python-dictionary
        ]
      );
      settings = {
          "Machine Configuration" = {
            machine_type = "Gemini PR";
            auto_start = true;
          };
      };
    };

    programs.ssh = {
      enable = true;
      matchBlocks = {
        "github.com" = {
          user = "git";
          identityFile = "~/.ssh/benvvalk_ed25519";
        };
      };
    };

    # Automatically start Syncthing as a user-level systemd service.
    #
    # Note: I need to do the initial setup of Syncthing manually:
    #
    # (1) On my Mac M1 Mini, share the "~/Sync" folder to this machine.
    # (2) On this machine, approve connection with Mac M1 Mini and accept
    #     its offer to share the "~/Sync" folder.
    # (3) On this machine, go to the web UI (localhost:8384) and
    #     set the GUI user/password. Use the user/password from
    #     my password store so that I won't forget it.
    #
    # Previously, I tried making a very fancy/automated Syncthing setup as a
    # system-level service in `configuration.nix`, but I found that it was
    # lacking some important options, and the extra complexity really wasn't
    # worth the benefits. For example, it doesn't seem possible to safely set
    # the GUI user/password via `sops-nix`, because it can only read the
    # user/password attributes from a string rather than a file.
    #
    # My abandoned attempt to make an automated Syncthing setup is on the
    # `nixos/syncthing-as-system-service` branch of my `dotfiles` repo.
    services.syncthing = {
       enable = true;
       # Don't create an empty `~/Sync` folder by default. When initially
       # syncing folders from my Mac M1 Mini, it feels safer and clearer
       # if there isn't already an (empty) `~/Sync` folder on this machine.
       extraOptions = ["--no-default-folder"];
    };
}
