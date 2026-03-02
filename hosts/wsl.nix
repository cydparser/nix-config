{
  config,
  flake-inputs,
  pkgs,
  ...
}:
let
  cfg = config.nix-config;
in
{
  imports = [
    flake-inputs.nixos-wsl.nixosModules.default
    ../users/cyd.nix
  ];

  networking.hostName = "wsl";

  nix-config = {
    home-manager.stateVersion = "24.05";
  };

  programs.nix-ld = {
    enable = true;
  };

  system.stateVersion = "25.11";

  wsl = {
    enable = true;
    defaultUser = cfg.username;
  };
}
