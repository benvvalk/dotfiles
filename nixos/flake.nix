# Note: I wrote this `flake.nix` by following the example
# in "Full NixOS Guide: Everything You Need to Know in One Place!"
# https://www.youtube.com/watch?v=nLwbNhSxLd4

{
    description = "Ben's system configuration";
   
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";

        # The `nixpkgs` commit where the `emacs` package provides emacs-29.1.
        # I want to stick with emacs-29.1 because it is the version
        # that I know works well with my `init.el`.
        # 
        # Here I am using the technique for pinning Nix package
        # versions that is described at timestamp 00:20:15 of the
        # following video:
        #
        # "Getting Started with Nix Home Manager",
        # https://www.youtube.com/watch?v=cZDiqGWPHKI
        nixpkgs-emacs.url = "github:nixos/nixpkgs/160b762eda6d139ac10ae081f8f78d640dd523eb";

        home-manager = {
            url = "github:nix-community/home-manager/release-25.05";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { nixpkgs, nixpkgs-emacs, home-manager, ... }:
        let
            system = "x86_64-linux";
        in
        {
            nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
                inherit system;
                modules = [ ./configuration.nix ];
            };

            homeConfigurations.benv = home-manager.lib.homeManagerConfiguration {
                pkgs = nixpkgs.legacyPackages.${system};
                modules = [ ./home.nix ];
                extraSpecialArgs = {
                    pkgs-emacs = nixpkgs-emacs.legacyPackages.${system};
                };
            };
        };
}
