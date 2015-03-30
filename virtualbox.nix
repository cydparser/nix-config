{ config, pkgs, ... } : {

  fileSystems."/vbox" = {
    fsType = "vboxsf";
    device = "Downloads";
    options = "rw,dmask=022";
  };

  services.virtualboxGuest.enable = true;

}
