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
    stateVersion = "24.05";
  };

  programs.nix-ld = {
    enable = true;
    package = pkgs.nix-ld-rs;
  };

  # TODO: Remove after fix: https://github.com/nix-community/NixOS-WSL/issues/650
  # Copied from https://github.com/nix-community/NixOS-WSL/blob/10154ce2b21236653e40b87c45079792ae8ca950/modules/wsl-distro.nix
  # Prevent systemd from mounting a tmpfs over the runtime dir (and thus hiding the wayland socket)
  systemd.services."user-runtime-dir@" = {
    overrideStrategy = "asDropin";
    serviceConfig.ExecStart =
      let
        wrapped = pkgs.writeShellScript "user-runtime-dir-start-wrapped" ''
          if [ -d "/run/user/$1" ]; then
            exit 0
          else
            ${config.systemd.package}/lib/systemd/systemd-user-runtime-dir start "$1"
          fi
        '';
      in
      [
        "" # unset old value
        "${wrapped} %i"
      ];
  };

  wsl = {
    enable = true;
    defaultUser = cfg.username;
  };
}
