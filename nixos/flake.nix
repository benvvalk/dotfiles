# Note: I wrote this `flake.nix` by following the example
# in "Full NixOS Guide: Everything You Need to Know in One Place!"
# https://www.youtube.com/watch?v=nLwbNhSxLd4

{
    description = "Ben's system configuration";
   
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";

        home-manager = {
            url = "github:nix-community/home-manager/release-24.05";
            inputs.nixpkgs.follows = "nixpkgs";
    };

    outputs = { nixpkgs, home-manager, ... }:
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
            };
        };
}
