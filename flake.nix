{
  description = "nix-config";

  nixConfig = {
    extra-substituters = [ "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    determinate = {
      url = "https://flakehub.com/f/DeterminateSystems/determinate/3";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager-darwin = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };

    iterm2-color-schemes = {
      url = "github:mbadolato/iTerm2-Color-Schemes";
      flake = false;
    };

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-25.05";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-25.05-darwin";

    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    systems.url = "github:nix-systems/default";
  };

  outputs =
    inputs@{
      self,
      flake-utils,
      nixpkgs,
      ...
    }:
    let
      systems = if builtins ? currentSystem then [ builtins.currentSystem ] else import inputs.systems;

      lib = nixpkgs.lib;

      forEachSystem = lib.genAttrs systems;

      importNixpkgs =
        {
          system,
          nixpkgs ? inputs.nixpkgs,
        }:
        import nixpkgs {
          inherit system;

          overlays = [
            inputs.emacs-overlay.overlays.package
            overlay
          ];

          config = {
            allowUnfree = true;
          };
        };

      overlay =
        final: prev:
        {
          merriam-webster-1913 = final.callPackage nix/merriam-webster-1913.nix { };

          sdcv = final.symlinkJoin {
            name = "sdcv";
            paths = [ prev.sdcv ];
            buildInputs = [ final.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/sdcv \
                --set STARDICT_DATA_DIR "${final.merriam-webster-1913}"
            '';
          };
        }
        // prev.lib.attrsets.genAttrs [
          "cabal-gild"
          "eventlog2html"
          "ghc-events"
          "lentil"
        ] (name: final.haskell.lib.justStaticExecutables final.haskellPackages.${name});
    in
    {
      overlays = {
        default = overlay;
      };

      darwinModules = {
        default = modules/darwin/default.nix;
      };

      nixosModules = {
        base = modules/base.nix;
        home-manager = modules/home-manager.nix;
        linux-minimal = modules/linux/minimal.nix;
        linux = modules/linux/default.nix;
      };

      darwinConfigurations = {
        "moon" = inputs.nix-darwin.lib.darwinSystem {
          pkgs = importNixpkgs {
            system = flake-utils.lib.system.aarch64-darwin;
            nixpkgs = inputs.nixpkgs-darwin;
          };

          specialArgs = {
            flake-inputs = {
              inherit (inputs) iterm2-color-schemes nix-index-database;
              home-manager = inputs.home-manager-darwin;
            };
          };

          system.configurationRevision = self.rev or self.dirtyRev or null;

          modules = [
            inputs.determinate.darwinModules.default
            self.darwinModules.default
            hosts/moon.nix
          ];
        };
      };

      nixosConfigurations = {
        vbox = lib.nixosSystem {
          pkgs = importNixpkgs { system = flake-utils.lib.system.x86_64-linux; };

          specialArgs = {
            flake-inputs = {
              inherit (inputs) home-manager iterm2-color-schemes nix-index-database;
            };
          };

          modules = [
            self.nixosModules.linux
            hosts/vbox.nix
          ];
        };

        wsl = lib.nixosSystem {
          pkgs = importNixpkgs { system = flake-utils.lib.system.x86_64-linux; };

          specialArgs = {
            flake-inputs = {
              inherit (inputs)
                home-manager
                iterm2-color-schemes
                nixos-wsl
                nix-index-database
                ;
            };
          };

          modules = [
            self.nixosModules.linux-minimal
            hosts/wsl.nix
          ];
        };
      };

      packages = forEachSystem (system: {
        devenv-up = self.devShells.${system}.default.config.procfileScript;

        devenv-test = self.devShells.${system}.default.config.test;
      });

      devShells = forEachSystem (
        system:
        let
          pkgs = importNixpkgs { inherit system; };
        in
        {
          default = inputs.devenv.lib.mkShell {
            inherit inputs pkgs;

            modules = [
              {
                git-hooks.hooks = {
                  nixfmt-rfc-style = {
                    enable = true;
                  };
                };

                languages = {
                  nix = {
                    enable = true;
                    lsp.package = pkgs.nixd;
                  };
                };

                packages = [ ];
              }
            ];
          };
        }
      );
    };
}
