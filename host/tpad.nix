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
      package = pkgs.pulseaudioFull;

      extraConfig = "
        load-module module-switch-on-connect
      ";
    };
  };

  networking = {
    hostName = "tpad";
    wireless.enable = true;
    wireless.interfaces = [ "wlp4s0" ];
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

  system = {
    stateVersion = "21.11";
  };

  virtualisation.libvirtd = {
    enable = true;
    onBoot = "ignore";
    onShutdown = "suspend";
    qemu.package = pkgs.qemu_kvm;
  };

}
