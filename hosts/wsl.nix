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

  fileSystems."/mnt/x" = {
    device = "/dev/disk/by-uuid/02f1f1c0-ed69-43c6-89fd-560fd26be8b8";
    fsType = "ext4";
  };

  networking.hostName = "wsl";

  nix-config = {
    home-manager.stateVersion = "24.05";
  };

  programs.nix-ld = {
    enable = true;
    package = pkgs.nix-ld-rs;
  };

  system.stateVersion = "24.05";

  wsl = {
    enable = true;
    defaultUser = cfg.username;
  };
}
