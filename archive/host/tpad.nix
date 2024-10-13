let
  dpi = 153;
in
  {pkgs, ...}: {
    boot = {
      blacklistedKernelModules = [
        # Webcam video
        "uvcvideo"
      ];

      kernelParams = ["mem_sleep_default=deep"];

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
      lm_sensors
      ntfs3g
      pavucontrol
    ];

    hardware = {
      bluetooth = {
        enable = false;
        package = pkgs.bluezFull;

        settings = {
          General = {
            # Enable A2DP sink
            Enable = "Source,Sink,Media,Socket";
          };
        };
      };
      nvidia-container-toolkit.enable = true;

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
      wireless.interfaces = ["wlp4s0"];
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
        videoDrivers = ["nvidia"];
        xkb.variant = "dvorak,";

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
