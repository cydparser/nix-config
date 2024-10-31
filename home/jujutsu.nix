{
  config,
  lib,
  ...
}:
{
  options.nix-config.jujutsu = {
    enable = lib.mkEnableOption "jujutsu" // {
      default = true;
    };
  };

  config =
    let
      inherit (lib) mkIf;
      cfg = config.nix-config.jujutsu;
      git = config.nix-config.git;
    in
    mkIf cfg.enable {
      programs.jujutsu = {
        enable = cfg.enable;

        settings = {
          user = {
            name = git.user.name;
            email = git.user.email;
          };
        };
      };
    };
}
