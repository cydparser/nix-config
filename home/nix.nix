{
  lib,
  pkgs,
  ...
}:
{

  options.nix-config.dev.nix = {
    enable = lib.mkEnableOption "nix" // {
      default = true;
    };
  };

  config = {
    home.packages = with pkgs; [
      cachix
      nix-output-monitor
      nix-prefetch-git
      nurl
      nil
      nickel
      nixfmt-rfc-style
      statix
    ];
  };

}
