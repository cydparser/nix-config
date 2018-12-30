{ config, pkgs, ... }: {

  boot = {
    loader.grub = {
      device = "/dev/sda";
      enable = true;
      version = 2;
    };
  };

  environment.etc = {
    "modprobe.d/hid_apple.conf".text =
      ''
        options hid_apple fnmode=2
        options hid_apple swap_opt_cmd=1
      '';
  };

  networking = {
    hostName = "vbox";
  };

  services = {
    xserver = {
      xkbVariant = "dvorak,"; # TODO
    };
  };

}
