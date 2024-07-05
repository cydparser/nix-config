{
  description = "Nix configurations";

  inputs = {
    dotfiles = {
      url = "github:cydparser/dotfiles";
      inputs = {
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";


    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    dotfiles,
    flake-utils,
    home-manager,
    nixpkgs,
    ...
  }:
    {
      nixosConfigurations = let
        importNixpkgs = system: args:
          import nixpkgs (nixpkgs.lib.attrsets.recursiveUpdate {
              inherit system;

              config = {
                allowUnfree = true;
              };

              overlays = [
                dotfiles.overlays.default
                (_final: _prev: {
                  # nixd = inputs.nixd.packages.${system}.nixd;
                })
              ];
            }
            args);
      in {
        tpad = let
          system = flake-utils.lib.system.x86_64-linux;

          pkgs = importNixpkgs system {
            config = {
              pulseaudio = true;
            };
          };
        in
          nixpkgs.lib.nixosSystem {
            inherit system;

            modules = [
              {nixpkgs.pkgs = pkgs;}
              home-manager.nixosModules.home-manager
              dotfiles.nixosModules.tpad
              self.nixosModules.configuration
              self.nixosModules.tpad
            ];
          };
      };

      nixosModules = {
        configuration = ./configuration.nix;
        tpad = ./host/tpad.nix;
      };
    }
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {};
    in {
      formatter = pkgs.alejandra;
    });
}
