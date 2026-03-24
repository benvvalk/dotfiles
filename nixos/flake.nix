# Note: I wrote this `flake.nix` by following the example
# in "Full NixOS Guide: Everything You Need to Know in One Place!"
# https://www.youtube.com/watch?v=nLwbNhSxLd4

{
    description = "Ben's system configuration";
   
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";

        # Tutorial: https://www.youtube.com/watch?v=GaM_paeX7TI
        firefox-addons = {
            url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        # The `nixpkgs` commit that provides emacs-30.2. Upgrading to
        # a new Emacs version tends to also require updating packages
        # in my `init.el` to solve compatibility problems. So I like
        # to pin my Emacs version, and only upgrade when I explicitly
        # choose to do so.
        #
        # Here I am using the technique for pinning Nix package
        # versions that is described at timestamp 00:20:12 of the
        # following video:
        #
        # "Getting Started with Nix Home Manager",
        # https://youtu.be/cZDiqGWPHKI?t=1212
        nixpkgs-emacs.url = "github:nixos/nixpkgs/0c39f3b5a9a234421d4ad43ab9c7cf64840172d0";

        # https://github.com/opensteno/plover-flake
        plover-flake.url = "github:openstenoproject/plover-flake";

        # The video I followed for initial setup of `home-manager`:
        # https://www.youtube.com/watch?v=FcC2dzecovw
        home-manager = {
            url = "github:nix-community/home-manager/release-25.11";
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
