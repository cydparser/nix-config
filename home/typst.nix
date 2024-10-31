{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.nix-config.typst = {
    enable = lib.mkEnableOption "typst" // {
      default = true;
    };
  };

  config =
    let
      cfg = config.nix-config.typst;
    in
    lib.mkIf cfg.enable {
      home.packages = with pkgs; [
        tinymist
        typst
        typstyle
      ];
    };
}
