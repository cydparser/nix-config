{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.nix-config.desktop =
    let
      inherit (lib) types;
    in
    {
      windowManager = {
        type = types.nullOr (types.enum [ "xmonad" ]);
        default = "xmonad";
      };
    };

  config =
    let
      inherit (lib) modules;

      cfg = config.nix-config.desktop;

      enableXserver = cfg.windowManager == "xmonad";
    in
    modules.mkMerge [
      {
        services = {
          xserver = {
            enable = enableXserver;
            autoRepeatDelay = 325;
            autoRepeatInterval = 45;

            xkb = {
              layout = "us,us";
              variant = "dvorak,";
              # NB: Pressing Win+space is not the same as `setxkbmap us`.
              options = "caps:ctrl_modifier,ctrl:ralt_rctrl,grp:win_space_toggle";
            };
          };
        };
      }
      (modules.mkIf (cfg.windowManager == "xmonad") {
        environment.systemPackages = with pkgs; [
          dmenu
        ];

        services = {
          displayManager.defaultSession = "none+xmonad";

          xserver = {
            displayManager.lightdm.enable = true;

            windowManager = {
              xmonad = {
                enable = true;
                enableContribAndExtras = true;
              };
            };
          };
        };
      })
    ];
}
