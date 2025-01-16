{
  config,
  lib,
  ...
}:
{
  options.nix-config.xmonad = {
    enable = lib.mkEnableOption "xmonad";
  };

  config =
    let
      cfg = config.nix-config.xmonad;
    in
    lib.mkIf cfg.enable {
      xsession.windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = config/xmonad/xmonad.hs;
      };
    };
}
