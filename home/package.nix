{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) types;

  isDarwin = pkgs.stdenv.isDarwin;
in
{
  options.nix-config = {
    homebrew = {
      enable = lib.mkEnableOption "homebrew" // {
        default = isDarwin;
      };

      opt-out = lib.mkOption {
        type = types.bool;
        default = false;
      };

      prefix = lib.mkOption {
        type = types.uniq types.str;
        default = if isDarwin then "/opt/homebrew" else "/home/linuxbrew/.linuxbrew";
      };
    };
  };

  config =
    let
      cfg = config.nix-config;

      init-homebrew = ''
        eval "$(${cfg.homebrew.prefix}/bin/brew shellenv)"
      '';
    in
    lib.mkIf cfg.homebrew.enable {
      home.sessionVariables = {
        HOMEBREW_ANALYTICS_DEBUG = "1";
        HOMEBREW_NO_ANALYTICS = lib.optionalString (!cfg.homebrew.opt-out) "1";
      };

      programs = {
        bash.initExtra = init-homebrew;
        zsh.initContent = init-homebrew;
      };
    };
}
