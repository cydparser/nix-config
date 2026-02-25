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

  boot.initrd.systemd.enable = true;

  networking.hostName = "wsl";

  nix-config = {
    home-manager.stateVersion = "24.05";
  };

  programs.nix-ld = {
    enable = true;
  };

  services.userborn.enable = true;

  system.etc.overlay.enable = true;
  system.stateVersion = "24.05";

  wsl = {
    enable = true;
    defaultUser = cfg.username;
  };
}
