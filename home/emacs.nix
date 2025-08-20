inputs@{
  config,
  lib,
  pkgs,
  ...
}:
let
  utils = import ./utils.nix inputs;

  emacs-darwin =
    let
      homebrew-emacs-plus = pkgs.fetchgit {
        url = "https://github.com/d12frosted/homebrew-emacs-plus.git";
        rev = "3e95d573d5f13aba7808193b66312b38a7c66851";
        outputHash = "1dcn3z5swbymbqxyn0b5g0l4ydhqvsdm83qayixqkrgl3fgn5shi";
        outputHashAlgo = "sha256";
      };

      emacs = pkgs.emacs-pgtk;

      patchesDir = "${homebrew-emacs-plus}/patches/emacs-${builtins.head (builtins.splitVersion emacs.version)}";
    in
    emacs.overrideAttrs (old: {
      patches =
        (old.patches or [ ])
        ++ lib.attrsets.mapAttrsToList (k: _: "${patchesDir}/${k}") (
          lib.filterAttrs (k: v: lib.hasSuffix ".patch" k) (builtins.readDir patchesDir)
        );
    });
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

          package =
            if pkgs.stdenv.isDarwin then
              emacs-darwin
            else if utils.isWayland then
              pkgs.emacs-pgtk
            else
              pkgs.emacs;

          extraPackages =
            ps: with ps; [
              pdf-tools
              treesit-grammars.with-all-grammars
              vterm
            ];
        };
      };

      xdg.configFile = {
        "hunspell/en_US".source =
          config.lib.file.mkOutOfStoreSymlink "${config.nix-config.src}/home/config/hunspell/en_US";
      };

      home.packages =
        with pkgs;
        [
          cask
          espeak
          (hunspell.withDicts (d: [ d.en-us ]))
          sdcv
          wordnet
        ]
        ++ lib.optionals pkgs.stdenv.isDarwin [
          # `insert-directory-program` needs `gls`.
          coreutils-prefixed
        ];

      home.sessionVariables = {
        EDITOR = "${config.programs.emacs.package}/bin/emacsclient";
      };
    };
}
