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
          package = if utils.isWayland osConfig then pkgs.emacs30-pgtk else pkgs.emacs30;

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

      xdg.configFile = {
        "hunspell/en_US".source =
          config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/src/nix-config/home/config/hunspell/en_US";
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
