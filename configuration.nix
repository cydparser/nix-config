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
        tmux
        w3m
        z3
        zsh
      ]) ++ (with nu.pkgs; [
        blender
        docker
        emacs
        firefox
        gimp
        git
        inkscape
        thunderbird
      ]) ++ (with nu.haskellPackages; [
        ShellCheck
        apply-refact
        cabal-install
        cabal2nix
        criterion
        ghc
        hakyll
        hasktags
        hindent
        hlint
        hoogle
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

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
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
