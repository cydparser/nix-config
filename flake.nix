{
  description = "nix-config";

  nixConfig = {
    extra-substituters = [ "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";
  };

  outputs =
    inputs@{
      self,
      flake-utils,
      home-manager,
      nixos-wsl,
      nixpkgs,
      ...
    }:
    let
      importNixpkgs =
        system:
        import nixpkgs {
          inherit nixpkgs system;

          overlays = [
            inputs.emacs-overlay.overlays.package
            overlay
          ];
        };

      overlay = final: prev: {
        merriam-webster-1913 = final.callPackage nix/merriam-webster-1913.nix { };

        sdcv = self.symlinkJoin {
          name = "sdcv";
          paths = [ prev.sdcv ];
          buildInputs = [ final.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/sdcv \
              --set STARDICT_DATA_DIR "${final.merriam-webster-1913}"
          '';
        };
      };

      common = {
        config.nix-config = rec {
          user = "cyd";

          home-manager = {
            users.${user} =
              { ... }:
              {
                config = {
                  nix-config = {
                    git = {
                      user = {
                        name = "cydparser";
                        email = "cydparser@gmail.com";
                      };
                    };
                    home.stateVersion = "24.11";
                  };
                };
              };
          };
        };
      };
    in
    {
      overlays = {
        default = overlay;
      };

      # nixosModules = {
      #   # TODO
      # };

      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {
            flake-inputs = {
              inherit home-manager;
              nix-index-database = inputs.nix-index-database;
            };
          };
          modules = [
            modules/base.nix
            common
            modules/home-manager.nix
            nixos-wsl.nixosModules.default
            hosts/wsl.nix
          ];
        };
      };

    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = importNixpkgs system;
      in
      {
        devShells = {
          default = with pkgs; [
            home-manager.${system}.packages.home-manager
            just
            nixfmt-rfc-style
          ];
        };

        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}
