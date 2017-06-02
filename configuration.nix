{ config, pkgs, ... }: {

  imports = (builtins.filter builtins.pathExists [
    ./hardware-configuration.nix
    ./local.nix
    ./users.nix
  ]);

  boot = {
    cleanTmpDir = true;

    loader.grub = {
      device = "/dev/sda";
      enable = true;
      version = 2;
    };
  };

  environment.systemPackages =
    let
      nu = import <nu> {};
    in
      (with pkgs; [
        clang
        cppcheck
        csslint
        ctags
        diffutils
        dmenu
        emacs
        git
        gnupg
        gnutls
        graphviz
        hunspell
        hunspellDicts.en-us
        jq
        mkpasswd
        nix-prefetch-git
        nix-repl
        nix-zsh-completions
        nixops
        nixpkgs-lint
        openssl
        pinentry
        python35Packages.pylint
        ripgrep
        ruby
        sdcv
        termite
        upx
        vim
        z3
        zsh
      ]) ++ (with nu.pkgs; [
        docker
        firefox
        torbrowser
      ]) ++ (with nu.haskellPackages; [
        ShellCheck
        apply-refact
        bench
        cabal-install
        cabal2nix
        codex
        ghc
        hasktags
        hindent
        hlint
        hoogle
        hpack
        hspec-discover
        pandoc
        shake
        stack
        threadscope
      ]);

  environment.variables = {
    NO_AT_BRIDGE = "1";
  };

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;

    fontconfig = {
      defaultFonts = {
        monospace = [ "inconsolata" ];
      };
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
    opengl.driSupport32Bit  = true;

    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      support32Bit = true;
    };
  };

  nixpkgs.config = {
    allowUnfree = true;

    firefox = {
      enableAdobeFlash = true;
      enableAdobeFlashDRM = true;
      enableDjvu = true;
      enableGoogleTalkPlugin = true;
      icedtea = true;
    };
    pulseaudio = true;
  };

  programs = {
    bash = {
      enableCompletion = true;
    };
    ssh.startAgent = true;

    tmux.enable =  true;

    zsh = {
      enable = true;
      enableSyntaxHighlighting = true;
    };
  };

  services = {
    ntp.enable = true;
    openssh.enable = false;
    printing.enable = true;

    xserver = {
      autorun = true;
      enable = true;
      layout = "us,us";
      xkbVariant = "dvorak,";
      xkbOptions = "grp:shifts_toggle";

      desktopManager = {
        default = "none";
        xterm.enable = false;
      };
      displayManager = {
        sessionCommands = ''
          xset r rate 300 50
        '';
        slim = {
          autoLogin = false;
          enable = true;
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

  system = {
    # autoUpgrade.enable = true;
    stateVersion = "17.03";
  };

  time.timeZone = "US/Pacific";

  virtualisation.docker = {
    enable = true;
    storageDriver = "devicemapper";
  };

}
