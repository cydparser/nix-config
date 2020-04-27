let
  dpi = 153;
in { pkgs, ... }: {

  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

  console = {
    font = "ter-232n";
  };

  environment.systemPackages = with pkgs; [
    brightnessctl
    docker-compose
    jetbrains.idea-community
    lm_sensors
    s-tui
    slack
    zoom-us
  ];

  fonts = {
    fontconfig = {
      dpi = dpi;
    };
  };

  networking = {
    hostName = "tpad";
    wireless.enable = true;
  };

  security.pam.enableEcryptfs = true;

  services = {
    clamav = {
      daemon.enable = true;
      updater.enable = true;
    };

    xserver = {
      dpi = dpi;
      videoDrivers = [ "nvidia" ];
      xkbVariant = "dvorak,";

      inputClassSections = [
        ''
          Identifier       "All pointers"
          Driver           "libinput"
          MatchIsPointer   "on"
          MatchDevicePath  "/dev/input/event*"
          Option           "Accel Speed"      "1.0"
          Option           "NaturalScrolling" "on"
        ''
      ];

      libinput = {
        enable = true;
        accelSpeed = "1.0";
        disableWhileTyping = true;
        naturalScrolling = true;
      };

    };
  };

}
