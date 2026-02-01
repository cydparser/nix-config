{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.nix-config;
in
{
  options.nix-config =
    let
      inherit (lib) mkOption types;
    in
    {
      username = mkOption {
        type = types.str;
        description = "Username of primary user";
      };
    };

  config = {
    nix = {
      package = pkgs.nixVersions.latest;

      settings = {
        experimental-features = [
          "nix-command"
          "flakes"
        ];

        keep-derivations = true;
        keep-outputs = true;

        # Remove warning about non-existent channel.
        nix-path = "nixpkgs=flake:nixpkgs";

        trusted-users = [ "root" cfg.username ];

        upgrade-nix-store-path-url = "https://install.determinate.systems/nix-upgrade/stable/universal";
      };
    };
  };
}
