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
      nu = import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) {};
    in
      (with pkgs; [
        aspell
        cppcheck
        csslint
        ctags
        dmenu
        gnutls
        graphviz
        jq
        mkpasswd
        nix-repl
        nixops
        nixpkgs-lint
        openssl
        pinentry
        pylint
        ruby
        termite
        tmux
        vim
        w3m
        z3
        zsh
      ]) ++ (with nu.pkgs; [
        blender
        clang
        docker
        emacs
        firefox
        gimp
        git
        inkscape
        nodejs
        thunderbird
        torbrowser
      ]) ++ (with nu.haskellPackages; [
        ShellCheck
        apply-refact
        cabal-install
        cabal2nix
        criterion
        ghc
        ghc-mod
        hakyll
        hasktags
        hindent
        hlint
        hoogle
        hpack
        hpc_0_6_0_2
        hspec-discover
        json-autotype
        lentil
        liquidhaskell
        pandoc
        shake
        stack
        structured-haskell-mode
        stylish-haskell
        xmobar
      ]);

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
      inconsolata
      source-code-pro
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
    ssh.startAgent = false;
  };

  services = {
    ntp.enable = true;
    openssh.enable = true;
    printing.enable = true;

    xserver = {
      autorun = true;
      enable = true;
      layout = "us";
      startGnuPGAgent = true;

      desktopManager = {
        default = "none";
        kde4.enable = false;
        xterm.enable = false;
      };
      displayManager = {
        slim = {
          autoLogin = false;
          defaultUser = "cyd";
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
    stateVersion = "15.09";
  };

  virtualisation.docker = {
    enable = true;
    storageDriver = "devicemapper";
  };

}
