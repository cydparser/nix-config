{pkgs, ...}: {
  imports = [
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/secret.nix
    ./host.nix # Symlink to host/NAME.nix
  ];

  boot = {
    cleanTmpDir = true;
    supportedFilesystems = ["ecryptfs"];
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
      pkgs.terminus_font
    ];

    useXkbConfig = true;
  };

  environment.systemPackages = with pkgs; [
    dmenu
    dnsutils
    ecryptfs
    ecryptfs-helper
    file
    git
    gnome.seahorse # gnome-keyring GUI
    gnupg
    gnutls
    groff
    htop
    lightdm
    lsof
    nix-index
    nushell
    openssl
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

  environment.variables = {
    EDITOR = "vim";
    VISUAL = "vim";
  };

  fileSystems."/".options = ["noatime"];

  fonts = {
    enableDefaultFonts = true;
    fontDir.enable = true;
    enableGhostscriptFonts = true;

    fontconfig = {
      allowBitmaps = false;
      subpixel.rgba = "none";
    };

    fonts = with pkgs; [
      inconsolata
    ];
  };

  hardware = {
    opengl.driSupport32Bit = true;

    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };

  networking.firewall.allowPing = false;

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    settings = {
      auto-optimise-store = true;

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

  nixpkgs.config = {
    allowUnfree = true;
    pulseaudio = true;
  };

  programs = {
    bash = {
      enableCompletion = true;
    };

    nix-ld.enable = true;

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

    fstrim.enable = true;
    gnome.gnome-keyring.enable = true;
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
    storageDriver = "devicemapper";
  };
}
