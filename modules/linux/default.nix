{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./minimal.nix
    ./desktop.nix
  ];

  options.nix-config =
    let
      inherit (lib) types;
    in
    {
      audio.enable = lib.mkOption {
        type = types.bool;
        default = true;
      };
    };

  config =
    let
      cfg = config.nix-config;
    in
    {
      boot = {
        kernel.sysctl = {
          # Copied from virtualisation.lxd.recommendedSysctlSettings
          "fs.inotify.max_queued_events" = 1048576;
          "fs.inotify.max_user_instances" = 1048576;
          "fs.inotify.max_user_watches" = 1048576;
          "vm.max_map_count" = 262144;
          "kernel.dmesg_restrict" = 1;
          "net.ipv4.neigh.default.gc_thresh3" = 8192;
          "net.ipv6.neigh.default.gc_thresh3" = 8192;
          "kernel.keys.maxkeys" = 2000;

          "kernel.sysrq" = builtins.foldl' builtins.bitOr 0 [
            2 # console logging
            8 # dumps
            16 # sync
            128 # reboot/poweroff
          ];
        };

        tmp = {
          cleanOnBoot = true;
        };
      };

      console = {
        earlySetup = true;
        keyMap = "dvorak";
      };

      fileSystems."/".options = [ "noatime" ];

      fonts = {
        fontconfig = {
          allowBitmaps = false;
          subpixel.rgba = "none";
        };
      };

      hardware = {
        graphics.enable32Bit = true;
      };

      networking.firewall.allowPing = false;

      programs.ssh.startAgent = true;

      security = {
        pam = {
          loginLimits = [
            {
              domain = "*";
              type = "-";
              item = "nofile";
              value = "524288";
            }
          ];
        };

        rtkit.enable = cfg.audio.enable;
      };

      services = {
        acpid.enable = true;

        dictd = {
          enable = true;
          DBs = with pkgs.dictdDBs; [
            wiktionary
            wordnet
          ];
        };

        libinput = {
          enable = true;

          mouse = {
            naturalScrolling = true;
          };

          touchpad = {
            accelSpeed = "1.0";
            disableWhileTyping = true;
            naturalScrolling = true;
          };
        };

        ntp.enable = true;

        pipewire = {
          enable = cfg.audio.enable;
          alsa.enable = true;
          alsa.support32Bit = true;
          pulse.enable = true;
        };
      };
    };
}
