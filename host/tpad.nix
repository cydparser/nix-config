let
  dpi = 153;
in { pkgs, ... }: {

  boot = {
    blacklistedKernelModules = [
      # Webcam video
      "uvcvideo"
    ];

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
    lm_sensors
    pavucontrol
  ];

  fonts = {
    fontconfig = {
      dpi = dpi;
    };
  };

  hardware = {
    bluetooth = {
      # enable = true;
      package = pkgs.bluezFull;

      settings = {
        General = {
          # Enable A2DP sink
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };

    pulseaudio = {
      extraModules = [ pkgs.pulseaudio-modules-bt ];
      package = pkgs.pulseaudioFull;

      extraConfig = "
        load-module module-switch-on-connect
      ";
    };
  };

  networking = {
    hostName = "tpad";
    wireless.enable = true;
  };

  security.pam.enableEcryptfs = true;

  services = {
    # blueman.enable = true;

    clamav = {
      daemon.enable = true;
      updater.enable = true;
    };

    xserver = {
      dpi = dpi;
      videoDrivers = [ "nvidia" ];
      xkbVariant = "dvorak,";

      desktopManager = {
        enlightenment.enable = true;
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
