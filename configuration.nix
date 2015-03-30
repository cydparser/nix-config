{ config, pkgs, ... } : {

  imports = [
    ./docker.nix
    ./hardware-configuration.nix
    ./virtualbox.nix
    ./users.nix
    ./local-users.nix
  ];

  boot = {
    cleanTmpDir = true;

    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };
  };

  networking.hostName = "nixos";
  networking.hostId = "071abf15";

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      inconsolata
      ubuntu_font_family
    ];
  };

  programs = {
    ssh.startAgent = false;
  };

  i18n = {
    consoleFont = "inconsolata";
  };

  environment.systemPackages = with pkgs; [
    curl
    firefox
    git
    thunderbird
    tmux
    vim
    wget
    zsh
  ];

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  services = {
    ntp.enable = true;

    openssh.enable = true;

    printing.enable = true;

    xserver = {
      enable = true;
      autorun = true;
      layout = "us";
      startGnuPGAgent = true;

      desktopManager = {
        default = "none";
        xterm.enable = false;
        kde4.enable = false;
      };

      displayManager = {
        slim = {
          enable = true;
          defaultUser = "cyd";
          autoLogin = false;
        };
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

  time.timeZone = "America/Pacific";

}
