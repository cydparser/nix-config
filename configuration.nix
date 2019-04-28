{ pkgs, ... }: {

  imports = [
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/secret.nix
    ./host.nix # Symlink to host/NAME.nix
  ];

  boot = {
    cleanTmpDir = true;
    earlyVconsoleSetup = true;
    supportedFilesystems = [ "ecryptfs" ];
    tmpOnTmpfs = true;
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
    gnupg
    gnutls
    htop
    jq
    lightdm
    lld
    lsof
    nix-index
    nix-prefetch-git
    nix-prefetch-github
    openssl
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
    enableCoreFonts = true;
    enableDefaultFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;

    fontconfig = {
      allowBitmaps = false;
      subpixel.lcdfilter = "none";
      subpixel.rgba = "none";
    };

    fonts = with pkgs; [
      corefonts
      hasklig
      inconsolata
      source-code-pro
      symbola
      ubuntu_font_family
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

  i18n = {
    consolePackages = with pkgs.kbdKeymaps; [
      dvp
      neo
      pkgs.terminus_font
    ];

    consoleUseXkbConfig = true;
  };

  networking.firewall.allowPing = false;

  nix = {
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
    ntp.enable = true;
    physlock.enable = true;

    xserver = {
      autorun = true;
      enable = true;
      exportConfiguration = true;

      autoRepeatDelay = 325;
      autoRepeatInterval = 45;
      layout = "us,us";
      xkbOptions = "caps:ctrl_modifier,ctrl:ralt_rctrl,grp:shifts_toggle";

      desktopManager = {
        default = "none";
        xterm.enable = false;
        xfce.enable = true;
      };

      displayManager = {
        lightdm = {
          enable = true;
        };
      };

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
        naturalScrolling = true;
      };

      multitouch = {
        enable = true;
        ignorePalm = true;
        invertScroll = true;
      };

      windowManager = {
        default = "xmonad";

        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
      };
    };
  };

  system = {
    stateVersion = "19.03";
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
