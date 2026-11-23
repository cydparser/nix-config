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
    (lib.mkIf cfg.enable {
      home.packages = with pkgs; [
        cachix
        dix
        nix-prefetch-git
        nix-tree
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
