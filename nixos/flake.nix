# Note: I wrote this `flake.nix` by following the example
# in "Full NixOS Guide: Everything You Need to Know in One Place!"
# https://www.youtube.com/watch?v=nLwbNhSxLd4

{
    description = "Ben's system configuration";
   
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";

        # Tutorial: https://www.youtube.com/watch?v=GaM_paeX7TI
        firefox-addons = {
            url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        # The `nixpkgs` commit where the `emacs` package provides emacs-29.4.
        # I want to stick with emacs-29.4 because it is the version
        # that I know works well with my `init.el`.
        # 
        # Here I am using the technique for pinning Nix package
        # versions that is described at timestamp 00:20:15 of the
        # following video:
        #
        # "Getting Started with Nix Home Manager",
        # https://www.youtube.com/watch?v=cZDiqGWPHKI
        nixpkgs-emacs.url = "github:nixos/nixpkgs/b58e19b11fe72175fd7a9e014a4786a91e99da5f";

        # https://github.com/opensteno/plover-flake
        plover-flake.url = "github:openstenoproject/plover-flake";

        # The video I followed for initial setup of `home-manager`:
        # https://www.youtube.com/watch?v=FcC2dzecovw
        home-manager = {
            url = "github:nix-community/home-manager/release-25.05";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { nixpkgs, home-manager, ... }@inputs:
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
                extraSpecialArgs = { inherit inputs system; };
                modules = [ ./home.nix ];
            };
        };
}
