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

  config = {
    programs = {
      home-manager.enable = true;
    };

    xdg.enable = true;
  };
}
