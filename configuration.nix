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
      corefonts
      hasklig
      inconsolata
      lmodern
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
    stateVersion = "20.03";
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
