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
  };

}
