inputs@{
  config,
  lib,
  pkgs,
  pkgs-unstable,
  ...
}:
let
  utils = import ./utils.nix inputs;

  emacs-darwin =
    let
      homebrew-emacs-plus = pkgs.fetchgit {
        url = "https://github.com/d12frosted/homebrew-emacs-plus.git";
        rev = "321b91cd313cd417a3605bbd99681e9a37616744";
        outputHash = "1phfkrcwhkn3lbxbl0w36lihyqv4rb18ky519cgarxkqgqpc41sm";
        outputHashAlgo = "sha256";
      };

      emacs = pkgs-unstable.emacs-pgtk;

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
              jinx
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
