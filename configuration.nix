{ pkgs, ... }: {

  imports = [
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/secret.nix
    ./host.nix # Symlink to host/NAME.nix
  ];

  boot = {
    cleanTmpDir = true;
    supportedFilesystems = [ "ecryptfs" ];
    tmpOnTmpfs = true;

    # Copied from virtualisation.lxd.recommendedSysctlSettings
    kernel.sysctl = {
      "fs.inotify.max_queued_events" = 1048576;
      "fs.inotify.max_user_instances" = 1048576;
      "fs.inotify.max_user_watches" = 1048576;
      "vm.max_map_count" = 262144;
      "kernel.dmesg_restrict" = 1;
      "net.ipv4.neigh.default.gc_thresh3" = 8192;
      "net.ipv6.neigh.default.gc_thresh3" = 8192;
      "kernel.keys.maxkeys" = 2000;
    };
  };

  console = {
    earlySetup = true;

    packages = with pkgs.kbdKeymaps; [
      dvp
      neo
      pkgs.terminus_font
    ];

    useXkbConfig = true;
  };

  environment.systemPackages = with pkgs; [
    cachix
    ctags
    diffutils
    direnv
    dmenu
    dnsutils
    ecryptfs
    ecryptfs-helper
    emacs
    file
    git
    git-lfs
    gitAndTools.delta
    gnupg
    htop
    jq
    lightdm
    lld
    lsof
    nix-index
    nix-prefetch-git
    nix-prefetch-github
    ripgrep
    scrot
    s-tui
    termite
    tree
    unzip
    upx
    usbutils
    vim
    xorg.xev
    xorg.xmessage
    xsel
    zsh
  ];

  fileSystems."/".options = [ "noatime" ];

  fonts = {
    enableDefaultFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;

    fontconfig = {
      allowBitmaps = false;
      subpixel.rgba = "none";
    };

    fonts = with pkgs; [
      cascadia-code
      inconsolata
    ];
  };

  hardware = {
    opengl.driSupport32Bit = true;

    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      support32Bit = true;
    };
  };

  networking.firewall.allowPing = false;

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    package = pkgs.nixFlakes;

    trustedUsers = [
      "cyd"
      "root"
    ];
  };

  nixpkgs.config = {
    allowUnfree = true;
    pulseaudio = true;
  };

  programs = {
    bash = {
      enableCompletion = true;
    };

    # TODO: gpg agent
    ssh.startAgent = true;
    tmux.enable =  true;

    zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
    };
  };

  security = {
    pam = {
      loginLimits = [
        { domain = "*"; type = "-"; item = "nofile"; value = "65535"; }
      ];
    };
  };

  services = {
    fstrim.enable = true;
    lorri.enable = true;
    ntp.enable = true;
    physlock.enable = true;

    xserver = {
      autorun = true;
      enable = true;
      exportConfiguration = true;

      autoRepeatDelay = 325;
      autoRepeatInterval = 45;
      layout = "us,us";
      # NB: Pressing both shifts is not the same as `setxkbmap us`.
      xkbOptions = "caps:ctrl_modifier,ctrl:ralt_rctrl,grp:shifts_toggle";

      desktopManager = {
        xterm.enable = false;
      };

      displayManager = {
        defaultSession = "none+xmonad";

        lightdm = {
          enable = true;
        };
      };

      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
      };
    };
  };

  system = {
    stateVersion = "20.09";
  };

  time.timeZone = "US/Pacific";

  users = {
    defaultUserShell = pkgs.zsh;

    extraUsers = {
      cyd = {
        name = "cyd";
        isNormalUser = true;
        extraGroups = [
          "docker"
          "libvirtd"
          "vboxsf"
          "wheel"
        ];

        uid = 1000;
      };
    };

    mutableUsers = true;
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
    storageDriver = "devicemapper";
  };

}
