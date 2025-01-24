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
  options.nix-config.emacs = {
    enable = utils.mkEnable "Emacs";
  };

  config =
    let
      inherit (lib) mkIf;
      cfg = config.nix-config.emacs;
    in
    mkIf cfg.enable {
      programs = {
        emacs = {
          enable = true;
          # TODO: defaultEditor = true;
          package = if utils.isWayland osConfig then pkgs.emacs29-pgtk else pkgs.emacs29;

          extraPackages =
            ps: with ps; [
              pdf-tools
              treesit-grammars.with-all-grammars
              vterm
            ];
        };
      };

      home.file = {
        "Library/Spelling".source = "${pkgs.hunspellDicts.en-us}/share/hunspell";
      };

      home.packages = with pkgs; [
        cask
        espeak
        hunspell
        sdcv
        wordnet
      ];
    };
}
