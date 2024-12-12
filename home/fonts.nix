inputs@{
  config,
  lib,
  pkgs,
  ...
}:
let
  utils = import ./utils.nix inputs;
in
{
  options.nix-config.fonts = {
    enable = utils.mkEnable "fonts";
  };

  config =
    let
      inherit (lib) mkIf;
      cfg = config.nix-config.fonts;
    in
    mkIf cfg.enable {
      fonts.fontconfig.enable = true;

      home.packages = with pkgs; [
        symbola
        nerd-fonts.caskaydia-cove
        nerd-fonts.hasklug
        nerd-fonts.inconsolata
        nerd-fonts.iosevka-term
      ];
    };
}
