{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.nix-config.dev.nix;
in
{
  options.nix-config.dev.nix = {
    enable = lib.mkEnableOption "nix" // {
      default = true;
    };
  };

  config = lib.modules.mkMerge [
    {
      home.packages = with pkgs; [
        nix-output-monitor
      ];
    }
    (lib.mkIf cfg.enable {
      home.packages = with pkgs; [
        cachix
        devenv
        nix-prefetch-git
        nixd
        nixfmt
        npins
        nurl
        statix
        vulnix
      ];
    })
  ];
}
