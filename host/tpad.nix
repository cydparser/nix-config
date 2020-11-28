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
    androidStudioPackages.dev
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

  hardware = {
    bluetooth.enable = true;
  };

  networking = {
    hostName = "tpad";
    wireless.enable = true;
  };

  security.pam.enableEcryptfs = true;

  services = {
    blueman.enable = true;

    clamav = {
      daemon.enable = true;
      updater.enable = true;
    };

    xserver = {
      dpi = dpi;
      videoDrivers = [ "nvidia" ];
      xkbVariant = "dvorak,";


      libinput = {
        enable = true;
        accelSpeed = "1.0";
        disableWhileTyping = true;
        naturalScrolling = true;
      };

    };
  };

  virtualisation.libvirtd = {
    enable = true;
    onBoot = "ignore";
    onShutdown = "suspend";
    qemuPackage = pkgs.qemu_kvm;
  };

}
