{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.nix-config.dev.rust =
    let
      inherit (lib) mkOption options types;
    in
    {
      enable = lib.mkEnableOption "rust" // {
        default = true;
      };
    };

  config =
    let
      inherit (lib) mkIf modules;

      cfg = config.nix-config.dev.rust;
    in
    mkIf cfg.enable (
      modules.mkMerge [
        {
          home.packages = with pkgs; [
            cargo-audit
            cargo-binutils
            cargo-deny
            cargo-edit
            cargo-expand
            cargo-nextest
            # cargo-semver-checks
          ];
        }
        (mkIf pkgs.stdenv.isDarwin (
          let
            init-rustup = ''
              [ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
            '';
          in
          {
            programs.bash.profileExtra = init-rustup;
            programs.zsh.envExtra = init-rustup;
          }
        ))
      ]
    );
}
