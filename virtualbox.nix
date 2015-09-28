{ config, pkgs, ... } : {

  fileSystems."/vbox" = {
    fsType = "vboxsf";
    device = "Downloads";
    options = "rw,dmask=022";
  };

  virtualisation.virtualbox.guest.enable = true;

}
