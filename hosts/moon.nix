{ config, ... }:
{
  imports = [
    ../users/cyd.nix
  ];

  config =
    let
      cfg = config.nix-config;
    in
    {
      networking.hostName = "moon";

      home-manager.users.${cfg.username} = {
        nix-config.vscode.enable = false;
      };

      nix-config = {
        home-manager = {
          stateVersion = "25.05";
        };
      };

      system.stateVersion = 6;
    };
}
