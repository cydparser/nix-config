inputs@{
  config,
  lib,
  osConfig ? { },
  pkgs,
  ...
}:
let
  utils = import ./utils.nix inputs;
in
{
  options.nix-config.vscode = {
    enable = utils.mkEnable "vscode";

    server = lib.mkEnableOption "vscode-server" // {
      default = utils.isWsl;
    };
  };

  config =
    let
      inherit (lib) mkIf modules;
      cfg = config.nix-config.vscode;
    in
    mkIf cfg.enable (
      modules.mkMerge [
        (mkIf (!cfg.server) { home.packages = with pkgs; [ vscode ]; })
        (mkIf cfg.server {
          home.packages = with pkgs; [ wget ];

          # TODO: nix-ld
        })
      ]
    );
}
