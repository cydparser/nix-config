{
  config,
  lib,
  pkgs,
  ...
}:
{

  options.nix-config =
    let
      inherit (lib) mkOption types;
    in
    {
      user = mkOption {
        type = types.str;
        description = "Username of primary user";
      };
    };

  nix = {
    package = pkgs.nixVersions.latest;

    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];

      keep-derivations = true;
      keep-outputs = true;

      trusted-users = [ config.nix-config.user ];
    };
  };

  nixpkgs.config.allowUnfree = true;

}
