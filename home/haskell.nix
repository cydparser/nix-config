{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.nix-config.dev.haskell =
    let
      inherit (lib) mkOption options types;
    in
    {
      enable = lib.mkEnableOption "haskell" // {
        default = true;
      };

      ghc = {
        enable = options.mkEnable "ghc";

        version = mkOption {
          type = types.str;
          default = "9.10";
        };
      };

      tools.enable = lib.mkEnableOption "tools";
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
    mkIf cfg.enable (
      modules.mkMerge [
        (mkIf cfg.tools.enable ({
          home.packages =
            with pkgs;
            [
              # cabal-add (broken)
              cabal-gild
              cabal2nix
              eventlog2html
              ghc-events
            ]
            ++ lib.options cfg.ghc.enable [
              cabal-install
              ghc-with-packages
              hls
            ];
        }))
        {
          home.file = {
            ".ghci".source = ./ghci;

            ".haskeline".source = ./haskeline;

            ".hlint.yaml".source = ./hlint.yaml;
          };

          home.sessionVariables = {
            CABAL_DIR = "$HOME/.cabal";
          };

          xdg.configFile = {
            "brittany/config.yaml".source = config/brittany/config.yaml;

            "fourmolu.yaml".source = config/fourmolu.yaml;

            "stylish-haskell/config.yaml".source = config/stylish-haskell/config.yaml;
          };
        }
        (mkIf pkgs.stdenv.isDarwin (
          let
            init-ghcup = ''
              [ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"
            '';
          in
          {
            programs.bash.profileExtra = init-ghcup;
            programs.zsh.envExtra = init-ghcup;
          }
        ))
      ]
    );
}
