{ config, pkgs, ... } : {

  users.mutableUsers = true;

  users.extraUsers.cyd = {
    name = "cyd";
    isNormalUser = true;
    extraGroups = [
      "docker"
      "vboxsf"
      "wheel"
    ];
    shell = "/run/current-system/sw/bin/zsh";

    # Place secrets data in local.nix:
    #   hashedPassword = ""; # mkpasswd -m sha-512
    #   openssh.authorizedKeys.keys = [];
  };

}
