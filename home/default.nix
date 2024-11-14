{ ... }:
{
  imports = [
    ./core.nix
    ./emacs.nix
    ./fonts.nix
    ./git.nix
    ./jujutsu.nix
    ./nix.nix
    ./nushell.nix
    ./typst.nix
    ./vscode.nix
  ];

  config = {
    programs = {
      home-manager.enable = true;
    };

    xdg.enable = true;
  };
}
