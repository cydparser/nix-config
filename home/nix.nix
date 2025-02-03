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

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      cachix
      devenv
      nil
      nix-output-monitor
      nix-prefetch-git
      nixfmt-rfc-style
      npins
      nurl
      statix
    ];
  };
}
