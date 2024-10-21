{ config, pkgs, ... }: {

    home = {
        username = "benv";
        homeDirectory = "/home/benv";
        stateVersion = "24.05";
    };

    programs.bash = {
        enable = true;
    };

    # Automatically starts user-level systemd service for Syncthing.
    #
    # Initial set-up to connect to other machines and share folders needs to be
    # done manually through the web UI.
    services.syncthing.enable = true;
}
