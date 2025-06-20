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
        merriweather
        nerd-fonts.fira-code
        nerd-fonts.hasklug
        nerd-fonts.iosevka-term
        nerd-fonts.iosevka
        noto-fonts
      ];
    };
}
