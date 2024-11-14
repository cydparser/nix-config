{ pkgs, config, ... }:
let
  cfg = config.nix-config;
in
{
  imports = [
    ../users/cyd.nix
  ];

  fileSystems."/mnt/x" = {
    device = "/dev/disk/by-uuid/02f1f1c0-ed69-43c6-89fd-560fd26be8b8";
    fsType = "ext4";
  };

  networking.hostName = "wsl";

  nix-config = {
    stateVersion = "24.05";
  };

  programs.nix-ld = {
    enable = true;
    package = pkgs.nix-ld-rs;
  };

  wsl = {
    enable = true;
    defaultUser = cfg.username;
  };
}
