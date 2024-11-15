{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.nix-config.dev.haskell =
    let
      inherit (lib) mkOption types;
    in
    {
      enable = lib.mkEnableOption "haskell" // {
        default = true;
      };

      ghc.version = mkOption {
        type = types.str;
        default = "9.10";
      };
    };

  config =
    let
      inherit (lib) mkIf modules;

      cfg = config.nix-config.dev.haskell;

      ghcVersionNix = lib.strings.replaceStrings [ "." ] [ "" ] cfg.ghc.version;

      ghc-with-packages = (
        pkgs.haskell.packages."ghc${ghcVersionNix}".ghcWithPackages (
          ps: with ps; [
            pretty-simple
            witch
            zlib
          ]
        )
      );

      hls = pkgs.haskell-language-server.override {
        supportedGhcVersions = [ ghcVersionNix ];
      };
    in
    modules.mkMerge [
      (mkIf cfg.enable ({
        home.packages = with pkgs; [
          # cabal-add (broken)
          cabal-gild
          cabal-install
          cabal2nix
          eventlog2html
          ghc-with-packages
          ghc-events
          hls
        ];

        home.file = {
          ".ghci".source = ./ghci;

          ".haskeline".source = ./haskeline;

          ".hlint.yaml".source = ./hlint.yaml;
        };

        xdg.configFile = {
          "brittany/config.yaml".source = config/brittany/config.yaml;

          "fourmolu.yaml".source = config/fourmolu.yaml;

          "stylish-haskell/config.yaml".source = config/stylish-haskell/config.yaml;
        };
      }))
    ];
}
