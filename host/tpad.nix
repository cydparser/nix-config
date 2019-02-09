{ _config, pkgs, ... }: {

  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    lm_sensors
    s-tui
  ];

  fonts = {
    fontconfig = {
      antialias = true;
      dpi = 153; # = (+ 102 (/ 102 2))
      hinting.enable = false;
      subpixel.lcdfilter = "none";
    };
  };

  i18n = {
    consoleFont = "ter-232n";
  };

  networking = {
    hostName = "tpad";
    wireless.enable = true;
  };

  security.pam.enableEcryptfs = true;

  services = {
    xserver = {
      dpi = 153;
      videoDrivers = [ "nvidia" ];
      xkbVariant = ",dvorak"; # TODO
    };
  };

}
