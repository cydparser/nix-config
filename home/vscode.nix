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
  options.nix-config.vscode =
    let
      inherit (lib) mkEnableOption;
    in
    {
      enable = utils.mkEnable "vscode";

      server = mkEnableOption "vscode-server" // {
        default = utils.isWsl osConfig;
      };
    };

  config =
    let
      inherit (lib) mkIf modules;
      cfg = config.nix-config.vscode;
    in
    mkIf cfg.enable (
      modules.mkMerge [
        { home.packages = with pkgs; [ vscode ]; }
        (mkIf cfg.server {
          home.packages = with pkgs; [ wget ];

          # TODO: nix-ld
        })
      ]
    );
}
