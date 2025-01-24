{
  lib,
  pkgs,
  ...
}:
{
  imports =
    [
      ./base.nix
      ./home-manager.nix
    ]
    ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [
      linux/default.nix
    ];
}
