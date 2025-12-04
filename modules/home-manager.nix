{
  config,
  flake-inputs,
  lib,
  pkgs,
  ...
}:
{
  options.nix-config.home-manager =
    let
      inherit (lib) mkOption types;
    in
    {
      stateVersion = mkOption {
        type = types.str;
      };
    };

  config =
    let
      cfg = config.nix-config;

      inherit (cfg) username;
    in
    {
      home-manager = {
        verbose = true;

        extraSpecialArgs = {
          inherit (flake-inputs) iterm2-color-schemes;
        };

        # Install packages to /etc/profiles; needed to run `nixos-rebuild build-vm`.
        useUserPackages = true;

        # Use system-level Nixpkgs.
        useGlobalPkgs = true;

        users.${username} =
          { ... }:
          {
            imports = [
              flake-inputs.nix-index-database.homeModules.nix-index
              { programs.nix-index-database.comma.enable = true; }
              ../home/default.nix
            ];

            home = {
              stateVersion = cfg.home-manager.stateVersion;
            };
          };
      };
    };
}
