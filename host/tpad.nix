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

  i18n = {
    consoleFont = "ter-232n";
  };

  networking = {
    hostName = "tpad";
    wireless.enable = true;
  };

  security.pam.enableEcryptfs = true;

  services = {
    clamav = {
      daemon.enable = true;
    };

    xserver = {
      videoDrivers = [ "nvidia" ];
      xkbVariant = ",dvorak"; # TODO
    };
  };

}
