{
  imports = [
    ./bash.nix
    ./core.nix
    ./dev.nix
    ./emacs.nix
    ./fonts.nix
    ./git.nix
    ./haskell.nix
    ./jujutsu.nix
    ./nix.nix
    ./nushell.nix
    ./vscode.nix
  ];

  config = {
    programs = {
      home-manager.enable = true;
    };

    xdg.enable = true;
  };
}
