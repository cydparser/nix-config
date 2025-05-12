{
  description = "nix-config";

  nixConfig = {
    extra-substituters = [ "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
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

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

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
      home-manager,
      nixos-wsl,
      nixpkgs,
      ...
    }:
    let
      systems = if builtins ? currentSystem then [ builtins.currentSystem ] else import inputs.systems;

      lib = nixpkgs.lib;

      forEachSystem = lib.genAttrs systems;

      importNixpkgs =
        system:
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
        default = modules/default.nix;
        base = modules/base.nix;
        home-manager = modules/home-manager.nix;
        linux = modules/linux/default.nix;
      };

      darwinConfigurations = {
        "moon" = inputs.nix-darwin.lib.darwinSystem {
          pkgs = importNixpkgs flake-utils.lib.system.aarch64-darwin;

          specialArgs = {
            flake-inputs = lib.attrsets.getAttrs [ "home-manager" "nix-index-database" ] inputs;
          };

          system.configurationRevision = self.rev or self.dirtyRev or null;

          modules = [
            self.nixosModules.default
            self.darwinModules.default
            hosts/moon.nix
          ];
        };
      };

      nixosConfigurations = {
        vbox = nixpkgs.lib.nixosSystem {
          pkgs = importNixpkgs flake-utils.lib.system.x86_64-linux;

          specialArgs = {
            flake-inputs = {
              inherit (inputs) home-manager nix-index-database;
            };
          };

          modules = [
            self.nixosModules.default
            self.nixosModules.linux
            hosts/vbox.nix
          ];
        };

        wsl = nixpkgs.lib.nixosSystem {
          pkgs = importNixpkgs flake-utils.lib.system.x86_64-linux;

          specialArgs = {
            flake-inputs = {
              inherit (inputs) home-manager nixos-wsl nix-index-database;
            };
          };

          modules = [
            self.nixosModules.default
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
          pkgs = importNixpkgs system;
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
