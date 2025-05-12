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

      stateVersion = mkOption {
        type = if pkgs.stdenv.isLinux then types.str else types.int;
        description = "See system.stateVersion";
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

        trusted-users = [ cfg.username ];
      };
    };

    system.stateVersion = cfg.stateVersion;
  };
}
