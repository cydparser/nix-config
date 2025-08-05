{ config, lib, ... }:
{
  imports = [
    ./core.nix
    ./dev.nix
    ./emacs.nix
    ./fonts.nix
    ./git.nix
    ./haskell.nix
    ./jujutsu.nix
    ./nix.nix
    ./nushell.nix
    ./rust.nix
    ./shell.nix
    ./terminal.nix
    ./vscode.nix
    ./xmonad.nix
  ];

  options.nix-config =
    let
      inherit (lib) mkOption types;
    in
    {
      src = mkOption {
        type = types.str;
        description = "Source code location";
        default = "${config.home.homeDirectory}/src/nix-config";
      };
    };

  config = {
    programs = {
      home-manager.enable = true;
    };

    xdg.enable = true;
  };
}
