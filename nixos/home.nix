{ config, pkgs, ... }: {

    home = {
        username = "benv";
        homeDirectory = "/home/benv";
        stateVersion = "24.05";
    };

    programs.bash = {
        enable = true;
        shellAliases = {
            rebuild = "sudo nixos-rebuild switch";
        };
    };
}
