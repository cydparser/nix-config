inputs@{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.nix-config.dev;

  utils = import ./utils.nix inputs;
in
{
  options.nix-config.dev =
    lib.attrsets.genAttrs
      [
        "sh"
        "toml"
        "yaml"
      ]
      (name: {
        enable = utils.mkEnable name;
      });

  config =
    let
      inherit (lib) mkIf modules;
    in
    modules.mkMerge [
      {
        home.packages = with pkgs; [
          lentil
          tokei
        ];
      }
      (mkIf cfg.sh.enable {
        home.packages = with pkgs; [
          shellcheck
          shfmt
        ];

        xdg.configFile = {
          "shellcheckrc".source = config/shellcheckrc;
        };
      })
      (mkIf cfg.toml.enable {
        home.packages = with pkgs; [
          taplo-lsp
        ];
      })
      (mkIf cfg.yaml.enable {
        home.packages = with pkgs; [
          yaml-language-server
          yamlfmt
        ];

        xdg.configFile = {
          "yamlfmt/.yamlfmt".source = config/yamlfmt/.yamlfmt;
        };
      })
    ];
}
