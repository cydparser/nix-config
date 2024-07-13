{pkgs, ...}: {
  boot = {
    supportedFilesystems = ["ecryptfs"];

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
      useTmpfs = true;
    };
  };

  console = {
    earlySetup = true;

    packages = [
      pkgs.terminus_font
    ];

    useXkbConfig = true;
  };

  environment.systemPackages = with pkgs; [
    dmenu
    dnsutils
    ecryptfs
    file
    git
    gnupg
    gnutls
    groff
    lsof
    openssl
    seahorse # gnome-keyring GUI
    scrot
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

  environment.variables = {
    EDITOR = "vim";
    VISUAL = "vim";
  };

  fileSystems."/".options = ["noatime"];

  fonts = {
    enableDefaultPackages = true;
    fontDir.enable = true;
    enableGhostscriptFonts = true;

    fontconfig = {
      allowBitmaps = false;
      subpixel.rgba = "none";
    };

    packages = with pkgs; [
      inconsolata
    ];
  };

  hardware = {
    graphics.enable32Bit = true;

    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };

  networking.firewall.allowPing = false;

  nix = {
    package = pkgs.nixVersions.latest;

    settings = {
      auto-optimise-store = true;

      experimental-features = [
        "nix-command"
        "flakes"
      ];

      keep-derivations = true;
      keep-outputs = true;

      min-free = 2048 * 1024 * 1024;
      max-free = 8192 * 1024 * 1024;

      trusted-users = [
        "cyd"
        "root"
      ];
    };
  };

  programs = {
    nano.enable = false;
    vim.defaultEditor = true;
    # TODO: gpg agent
    ssh.startAgent = true;
    tmux.enable = true;

    zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
    };
  };

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
  };

  services = {
    dictd = {
      enable = true;
      DBs = with pkgs.dictdDBs; [wiktionary wordnet];
    };

    displayManager.defaultSession = "none+xmonad";
    fstrim.enable = true;
    gnome.gnome-keyring.enable = true;

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
    physlock.enable = true;

    xserver = {
      autorun = true;
      enable = true;
      exportConfiguration = true;

      autoRepeatDelay = 325;
      autoRepeatInterval = 45;
      xkb.layout = "us,us";
      # NB: Pressing both shifts is not the same as `setxkbmap us`.
      xkb.options = "caps:ctrl_modifier,ctrl:ralt_rctrl,grp:shifts_toggle";

      desktopManager = {
        xterm.enable = false;
      };

      displayManager = {
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

  time.timeZone = "US/Pacific";

  users = {
    defaultUserShell = pkgs.zsh;

    extraUsers = {
      cyd = {
        name = "cyd";
        isNormalUser = true;
        extraGroups = [
          "audio"
          "cdrom"
          "clamav"
          "docker"
          "floppy"
          "input"
          "libvirtd"
          "lp"
          "systemd-journal"
          "vboxsf"
          "video"
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
    autoPrune = {
      enable = true;
    };
  };
}
